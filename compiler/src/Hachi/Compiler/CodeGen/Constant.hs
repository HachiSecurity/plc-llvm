{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module contains code generation functions for constants.
module Hachi.Compiler.CodeGen.Constant (
    generateConstantGlobals,
    forceErrRef,

    CompileConstant,
    compileConst,
    compileConstDynamic,
    loadConstVal
) where

-------------------------------------------------------------------------------

import Control.Monad

import qualified Data.ByteString as BS
import Data.Proxy
import qualified Data.Text as T

import LLVM.AST as LLVM
import LLVM.AST.Constant
import LLVM.AST.IntegerPredicate as LLVM
import LLVM.IRBuilder as IR

import PlutusCore as PLC
import PlutusCore.Data as PLC

import Hachi.Compiler.CodeGen.Closure
import Hachi.Compiler.CodeGen.Common
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types
import Hachi.Compiler.CodeGen.Externals

-------------------------------------------------------------------------------

-- | `generateConstantGlobals` is a computation which emits global definitions
-- related to constants.
generateConstantGlobals :: MonadCodeGen m => m ()
generateConstantGlobals = void $ do
    void $ global "returnRegister" (ptrOf VoidType) $ Null (ptrOf VoidType)

    runIRBuilderT emptyIRBuilder $ do
        void $ globalStringPtr "%d\n" "i64FormatStr"
        void $ globalStringPtr "%s\n" "strFormatStr"
        void $ globalStringPtr "True" "trueStr"
        void $ globalStringPtr "False" "falseStr"

        void $ globalStringPtr "Attempted to instantiate a non-polymorphic term.\n" "forceErr"

returnRef :: Operand
returnRef = ConstantOperand
          $ GlobalReference (ptrOf $ ptrOf VoidType) "returnRegister"

i64FormatRef :: Operand
i64FormatRef = ConstantOperand $ asStringPtr $
    GlobalReference (stringPtr "%d\n") "i64FormatStr"

strFormatRef :: Operand
strFormatRef = ConstantOperand $ asStringPtr $
    GlobalReference (stringPtr "%s\n") "strFormatStr"

trueRef :: Operand
trueRef = ConstantOperand $ asStringPtr $
    GlobalReference (stringPtr "True") "trueStr"

falseRef :: Operand
falseRef = ConstantOperand $ asStringPtr $
    GlobalReference (stringPtr "False") "falseStr"

forceErrRef :: Operand
forceErrRef = ConstantOperand $ asStringPtr $
    GlobalReference (stringPtr "Attempted to instantiate a non-polymorphic term.\n") "forceErr"

-------------------------------------------------------------------------------

class CompileConstant a where
    -- | `compileConstant` @name val@ generates code which initialises the
    -- constant given by @val@. In simple cases, this may just return a
    -- constant expression, while in other cases we may generate a global
    -- and return a pointer to it.
    compileConstant :: MonadCodeGen m => String -> a -> m Constant

    -- | `compileLoadConstant` @closurePtr@ loads the constant from a closure
    -- given by @closurePtr@. This function must be used with
    -- `XTypeApplications` to specify which implementation to use.
    compileLoadConstant
        :: (MonadCodeGen m, MonadIRBuilder m)
        => ClosurePtr -> m Operand

    -- | `compilePrintBody` @name closurePtr@ generates code which
    -- pretty-prints the value of the constant stored in @closurePtr@.
    compilePrintBody
        :: (MonadCodeGen m, MonadIRBuilder m)
        => String -> ClosurePtr -> m ()

ccProxy :: Proxy CompileConstant
ccProxy = Proxy

instance CompileConstant Integer where
    -- we can just stick the integer value directly into the closure
    compileConstant _ val = pure $ Int 64 val

    -- retrieve the integer value from the closure directly
    compileLoadConstant = loadFromClosure (ClosureFreeVar 0) (Just $ ptrOf i64)

    compilePrintBody _ this = do
        -- retrieve the constant from the closure
        v <- compileLoadConstant @Integer this

        -- use printf to pretty-print it
        void $ printf i64FormatRef [v]

instance CompileConstant BS.ByteString where

instance CompileConstant T.Text where

instance CompileConstant () where

instance CompileConstant Bool where
    -- we can just stick the boolean value directly into the closure
    compileConstant _ val =
        pure $ Int 1 $ toInteger $ fromEnum val

    -- retrieve the boolean value from the closure directly
    compileLoadConstant = loadFromClosure (ClosureFreeVar 0) (Just $ ptrOf i1)

    compilePrintBody _ this = do
        -- load the bit value from the closure
        v <- compileLoadConstant @Bool this

        -- check whether it is 0, which we treat as False, while any other
        -- value should be treated as True; depending on this test,
        -- select the correct string pointer for pretty-printing
        r <- icmp LLVM.EQ v (ConstantOperand $ Int 1 0)
        bstr <- select r falseRef trueRef

        void $ printf strFormatRef [bstr]

instance CompileConstant PLC.Data where

instance (CompileConstant a, CompileConstant b) => CompileConstant (a,b) where

instance CompileConstant a => CompileConstant [a] where

-------------------------------------------------------------------------------

-- | `compileConstEntry` @name builder@ generates an entry function for the
-- constant named @name@ where the body which loads the constant from the
-- closure is generated by @builder@.
compileConstEntry
    :: MonadCodeGen m
    => String -> (ClosurePtr -> IRBuilderT m Operand) -> m Constant
compileConstEntry name loadFn = do
    let llvmName = mkName name
    void $ IR.function llvmName [(closureTyPtr, "this")] closureTyPtr $
        \[this] -> do
            compileTrace name

            v <- loadFn $ MkClosurePtr this
            store returnRef 0 v

            ret this

    pure $ GlobalReference clsEntryTy llvmName

-- | `compileConst` @value@ generates code for a constant whose @value@ is
-- provided as the first argument.
compileConst :: MonadCodeGen m => Some (ValueOf DefaultUni) -> m Operand
compileConst (Some (ValueOf tag (x :: a))) = do
    name <- mkFresh "con"

    ptr <- bring ccProxy tag (compileConstant name x)

    let entryName = name <> "_entry"

    codePtr <- compileConstEntry entryName (bring ccProxy tag $ compileLoadConstant @a)

    -- 2. generate print code
    let builder = bring ccProxy tag (compilePrintBody @a name)
    print_fun <- compileConstPrint name builder

    -- 3. generate a static closure: this should be sufficient since constants
    -- hopefully do not contain any free variables
    ConstantOperand <$> compileClosure name codePtr print_fun [ptr]

-- | `compileConstDynamic` @value@ generates code which allocates a dynamic
-- closure for the value given by @value@. Note that this function must be
-- used with `XTypeApplications` in order to select a type for the type
-- variable @a@.
compileConstDynamic
    :: forall a m. (MonadCodeGen m, MonadIRBuilder m, CompileConstant a)
    => Operand
    -> m Operand
compileConstDynamic val = do
    name <- mkFresh "con"

    -- 1. generate entry code
    let entryName = name <> "_entry"
    codePtr <- compileConstEntry entryName $ compileLoadConstant @a

    -- 2. generate print code
    printPtr <- compileConstPrint name $ compilePrintBody @a name

    -- 3. generate a dynamic closure
    allocateClosure False codePtr printPtr [val]

-- | `compileConstPrint` @name builder@ compiles a print function for a
-- constant which loads the constant value from the closure using @builder@.
compileConstPrint
    :: MonadCodeGen m
    => String -> (ClosurePtr -> IRBuilderT m ()) -> m Constant
compileConstPrint name bodyBuilder = do
    let printName = mkName $ name <> "_print"

    _ <- IR.function printName [(closureTyPtr, "this")] VoidType $
        \[this] -> do
            compileTrace (name <> "_print")

            bodyBuilder $ MkClosurePtr this

            retVoid

    pure $ GlobalReference printFnTy printName

-- | `loadConstVal` @type@ loads the value of the result register (which
-- constant closures write to when entered) assuming that it is of the
-- type given by @type@.
loadConstVal
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => LLVM.Type -> m Operand
loadConstVal ty = do
    -- the result register stores a pointer to some value, so we first need
    -- to load that pointer from the result register
    ptr <- load returnRef 0

    -- we then cast it to a pointer of the appropriate type we want and load
    -- the corresponding value
    bitcast ptr ty

-------------------------------------------------------------------------------
