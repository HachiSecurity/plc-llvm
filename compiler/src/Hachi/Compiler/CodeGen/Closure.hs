module Hachi.Compiler.CodeGen.Closure (
    -- * Closure representation
    ClosurePtr(..),
    closureTyDef,
    closureSize,
    closureTy,
    closureTyPtr,

    -- * Creating closures
    mkEntryTy,
    clsEntryTy,
    varEntryTy,
    compileClosure,
    compileDynamicClosure,

    -- * Loading data from closures
    ClosureComponent(..),
    loadFromClosure,
    callClosure,
    enterClosure
) where

-------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.Reader

import qualified Data.Map as M
import qualified Data.Set as S

import LLVM.AST
import LLVM.AST.AddrSpace
import LLVM.AST.Constant
import LLVM.IRBuilder as IR

import UntypedPlutusCore as UPLC

import Hachi.Compiler.CodeGen.Common
import Hachi.Compiler.CodeGen.Externals
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types

-------------------------------------------------------------------------------

-- | Represents a pointer to a closure.
newtype ClosurePtr = MkClosurePtr { closurePtr :: Operand }
    deriving (Eq, Show)

-- | `closureTyDef` is a `Type` definition for closures. Note the layout is
-- as follows:
--
-- - A pointer to the code for the closure.
-- - A pointer to the print code for the closure.
-- - Zero or more free variables. (LLVM will accept more elements than the
-- array's indicated size) https://llvm.org/docs/LangRef.html#array-type
closureTyDef :: Type
closureTyDef = StructureType False
    [ clsEntryTy
    , funPtr
    , ArrayType 0 closureTyPtr
    ]

-- | The minimum size of a closure in words.
closureSize :: Num a => a
closureSize = 2

-- | `closureTy` is a `Type` for closures.
closureTy :: Type
closureTy = NamedTypeReference "closure"

-- | `closureTyPtr` is a `Type` representing a pointer to a closure.
closureTyPtr :: Type
closureTyPtr = ptrOf closureTy

-- | Enumerates different components of a closure.
data ClosureComponent
    = ClosureCode
    | ClosurePrint
    | ClosureFreeVar Integer
    deriving (Eq, Show)

-- | `indexForComponent` @component@ determines the index of @component@
-- within a closure.
indexForComponent :: ClosureComponent -> Integer
indexForComponent ClosureCode = 0
indexForComponent ClosurePrint = 1
indexForComponent (ClosureFreeVar n) = closureSize + n

-------------------------------------------------------------------------------

-- | `mkEntryTy` @arity@ generates a function signature for a closure
-- with @arity@-many parameters in addition to the parameter that
-- references the closure itself. In other words, the resulting
-- signature has 1+@arity@ many parameters.
mkEntryTy :: Int -> Type
mkEntryTy arity = ptrOf $ FunctionType closureTyPtr params False
    where params = closureTyPtr : replicate arity closureTyPtr

-- | `clsEntryTy` is the `Type` of closure entry functions.
clsEntryTy :: Type
clsEntryTy = mkEntryTy 0

varEntryTy :: Type
varEntryTy = mkEntryTy 1

-- | `compileClosure` @name entryPtr printPtr@ generates a global variable
-- representing a closure for @name@.
compileClosure
    :: MonadModuleBuilder m
    => String -> Constant -> Constant -> [Constant] -> m Constant
compileClosure name codePtr printPtr fvs = do
    let closureName = mkName $ name <> "_closure"

    void $ global closureName closureTy $ Struct Nothing False $
        codePtr : printPtr : fvs

    pure $ GlobalReference (PointerType closureTy $ AddrSpace 0) closureName

-- | `compileDynamicClosure`
compileDynamicClosure
    :: ( MonadIO m
       , MonadReader CodeGenSt m
       , MonadModuleBuilder m
       , MonadIRBuilder m
       )
    => String -> S.Set UPLC.Name -> UPLC.Name -> IRBuilderT m Operand -> m Operand
compileDynamicClosure name fvs var codeFun = do
    -- we dynamically allocate a closure so that we can store the free
    -- variables along with it - for now, we assume that we are running
    -- on a 64 bit platform and calculate the pointer size accordingly
    let bits = 64
    let ptrSize = ConstantOperand $ LLVM.AST.Constant.sizeof bits closureTyPtr

    let entryName = mkName $ name <> "_entry"
    let entryTy = mkEntryTy 1

    _ <- IR.function entryName [(closureTyPtr, "this"), (closureTyPtr, mkParamName var)] closureTyPtr $
        \[this, arg] -> extendScope var arg $ do
            compileTrace (name <> "_entry")

            -- we need to load the free variables from the closure pointed to
            -- by `this`, which are stored at offset 3 onwards; note that the
            -- free variables are stored in alphabetical order and the code
            -- which loads them here must correspond to the code that stores
            -- them further down
            let loadFrom i = do
                    offset <- mul ptrSize (ConstantOperand $ Int bits i)
                    addr <- add this offset
                    load addr 0

            -- load the free variables from the closure pointed to by `this`
            cvars <- forM (zip [closureSize..] $ S.toList fvs) $ \(idx,_) -> loadFrom idx

            -- update the local environment with mappings to the free variables
            -- we have obtained from the closure represented by `this` and
            -- compile the body of the function
            termClosure <- updateEnv fvs cvars codeFun
            ret termClosure

    let code_fun = GlobalReference entryTy entryName

    print_fun <- compileMsgPrint name "Evaluation resulted in a function."

    -- allocate enough space for the closure on the heap:
    -- 3 code pointers + one pointer for each free variable
    size <- mul ptrSize (ConstantOperand $ Int bits $ 3 + toInteger (length fvs))
    c <- malloc size
    cc <- bitcast c closureTyPtr

    -- store data in the closure: we have the function pointers followed by
    -- any pointers to free variables
    let storeAt i val = do
            offset <- mul ptrSize (ConstantOperand $ Int bits i)
            addr <- add cc offset
            store addr 0 val

    storeAt 0 $ ConstantOperand code_fun
    storeAt 1 $ ConstantOperand print_fun

    env <- asks codeGenEnv

    forM_ (zip [closureSize..] $ S.toList fvs) $ \(idx, v) -> case M.lookup (nameString v) env of
        -- TODO: handle this a bit more nicely
        Nothing -> error "Can't find variable"
        Just val -> storeAt idx val

    -- return an Operand representing a pointer to the dynamic closure that we
    -- have just created
    pure cc

-------------------------------------------------------------------------------

-- | `loadFromClosure` @component ptr@ loads the component described by
-- @component@ from the closure represented by @ptr@.
loadFromClosure
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => ClosureComponent -> ClosurePtr -> m Operand
loadFromClosure prop (MkClosurePtr ptr) = do
    let ix = indexForComponent prop
    addr <- gep ptr [ConstantOperand $ Int 32 0, ConstantOperand $ Int 32 ix]
    load addr 0

-- | `callClosure` @component ptr args@ loads the component described by
-- @component@ from the closure represented by @ptr@, assumes it is a function,
-- and then calls it with @args@ as arguments.
callClosure
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => ClosureComponent
    -> ClosurePtr
    -> [Operand]
    -> m Operand
callClosure prop closure argv = do
    entry <- loadFromClosure prop closure
    call entry $ (closurePtr closure, []) : [(arg, []) | arg <- argv]

-- `enterClosure` @ptr args@ enters the closure represented by @ptr@ and
-- provides the arguments given by @args@.
enterClosure
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => ClosurePtr -> [Operand] -> m Operand
enterClosure = callClosure ClosureCode

-------------------------------------------------------------------------------
