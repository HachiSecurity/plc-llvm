{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module contains code generation functions for constants.
module Hachi.Compiler.CodeGen.Constant (
    generateConstantGlobals,
    forceErrRef,

    CompileConstant,
    compileConst,
    compileConstDynamic,
    retConstDynamic,
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
import Hachi.Compiler.CodeGen.Globals
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types
import Hachi.Compiler.CodeGen.Externals
import Hachi.Compiler.CodeGen.Constant.Data
import Hachi.Compiler.CodeGen.Constant.Pair
import Hachi.Compiler.CodeGen.Constant.List

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

-- | `compileSubConstant` @baseName value@ is a wrapper around
-- `compileConstStatic` for @value@ which additionally generates a fresh
-- name based on the suggestion given by @baseName@ and constructs a global
-- reference to the resulting closure.
compileSubConstant
    :: (MonadCodeGen m, CompileConstant a)
    => String -> a -> m Constant
compileSubConstant baseName v = do
    name <- mkFresh baseName
    _ <- compileConstStatic name v

    -- unfortunately, `compileClosure` already returns a `ClosurePtr` which
    -- is a wrapper around `Operand`, but we need `Constant`s here, so we
    -- need to reconstruct the global references from the closure names;
    -- this could be improved if we better abstract over static and dynamic
    -- closures in `ClosurePtr`
    pure $ GlobalReference closureTyPtr $ mkClosureName name

instance CompileConstant Integer where
    -- we can just stick the integer value directly into the closure
    compileConstant _ val = pure $ Int 64 val

    -- retrieve the integer value from the closure directly
    compileLoadConstant = loadFromClosure (ClosureFreeVar 0) i64

    compilePrintBody _ this = do
        -- retrieve the constant from the closure
        v <- compileLoadConstant @Integer this

        -- use printf to pretty-print it
        void $ printf i64FormatRef [v]

instance CompileConstant BS.ByteString where
    compileConstant name val = do
        let size = BS.length val
        let bytes = BS.unpack val
        let arr = Array i8 $ map (Int 8 . fromIntegral) bytes
        let arrTy = ArrayType (fromIntegral size) i8
        let arrName = mkName $ name <> "_data"

        _ <- global arrName arrTy arr
        let dataRef = GlobalReference (ptrOf arrTy) arrName

        _ <- global (mkName name) bytestringTy $
            Struct Nothing False [Int 64 $ toInteger size, dataRef]

        pure $ GlobalReference bytestringTyPtr (mkName name)

    compileLoadConstant = loadFromClosure (ClosureFreeVar 0) bytestringTyPtr

    compilePrintBody _ this = do
        ptr <- compileLoadConstant @BS.ByteString this
        void $ printBytestring ptr

instance CompileConstant T.Text where

instance CompileConstant () where
    compileConstant _ () =
        pure $ Int 1 1

    compileLoadConstant = loadFromClosure (ClosureFreeVar 0) i1

    compilePrintBody _ _ =
        void $ printf unitRef []

instance CompileConstant Bool where
    -- we can just stick the boolean value directly into the closure
    compileConstant _ val =
        pure $ Int 1 $ toInteger $ fromEnum val

    -- retrieve the boolean value from the closure directly
    compileLoadConstant = loadFromClosure (ClosureFreeVar 0) i1

    compilePrintBody _ this = do
        -- load the bit value from the closure
        v <- compileLoadConstant @Bool this

        -- check whether it is 0, which we treat as False, while any other
        -- value should be treated as True; depending on this test,
        -- select the correct string pointer for pretty-printing
        r <- icmp LLVM.EQ v (ConstantOperand $ Int 1 0)
        bstr <- select r falseRef trueRef

        void $ printf strFormatRef [bstr]

-- | `compileDataConstant` @tag fieldName name value extraArgs@ generates a
-- static data constant named @name@ of kind @tag@. The provided @value@ is
-- compiled to another static constant and stored as argument to the data
-- constant, using a name derived from @fieldName@. For `DataConstr`,
-- @extra@ may include an additional argument.
compileDataConstant
    :: (MonadCodeGen m, CompileConstant a)
    => DataTag -> String -> String -> a -> [Constant] -> m Constant
compileDataConstant tag fieldName name val extra = do
    ref <- compileSubConstant fieldName val

    dataGlobal (mkName name) tag (ref : extra)

instance CompileConstant PLC.Data where
    compileConstant name (Constr t xs) =
        compileDataConstant DataConstr "constr_list" name xs [Int 64 t]
    compileConstant name (Map xs) =
        compileDataConstant DataMap "map_list" name xs []
    compileConstant name (List xs) =
        compileDataConstant DataList "list_list" name xs []
    compileConstant name (I v) =
        compileDataConstant DataI "i_integer" name v []
    compileConstant name (B bs) =
        compileDataConstant DataB "b_bytestring" name bs []

    compileLoadConstant = loadFromClosure (ClosureFreeVar 0) dataTyPtr

    compilePrintBody _ this = do
        -- retrieve the pointer to the data structure from the closure
        ptr <- compileLoadConstant @Data this

        let handleCase ref = do
                _ <- printf ref []
                cls <- loadDataPtr ptr
                _ <- callClosure ClosurePrint cls []
                retVoid

        -- depending on the constructor tag, use the relevant pretty-printer
        withDataTag ptr $ \case
            DataConstr -> do
                tag <- loadConstrTag ptr
                _ <- printf dataConstrRef [tag]
                cls <- loadDataPtr ptr
                _ <- callClosure ClosurePrint cls []
                retVoid
            DataMap -> handleCase dataMapRef
            DataList -> handleCase dataListRef
            DataI -> handleCase dataIntegerRef
            DataB -> handleCase dataByteStringRef

instance (CompileConstant a, CompileConstant b) => CompileConstant (a,b) where
    compileConstant name (x,y) = do
        -- compile the two components to constant closures; the pair is then
        -- just two pointers to those closures; we need them as closures so
        -- that they can have their respective pretty-printing functions
        let fstName = name <> "_fst"
        let sndName = name <> "_snd"

        xr <- compileSubConstant fstName x
        yr <- compileSubConstant sndName y

        -- create a new global for the pair of pointer-sized values
        _ <- global (mkName name) pairTy $ Struct Nothing False [xr, yr]

        -- return a reference to the global
        pure $ GlobalReference pairTyPtr (mkName name)

    -- to load a pair, we just retrieve its pointer from the closure
    compileLoadConstant = loadFromClosure (ClosureFreeVar 0) pairTyPtr

    compilePrintBody _ this = do
        -- retrieve the pointer to the pair
        ptr <- compileLoadConstant @(a,b) this

        -- print the opening bracket
        void $ printf openParenRef []

        -- both components must be pointers to closures; retrieve them and
        -- call their respective pretty-printing functions
        x <- getFst ptr
        _ <- callClosure ClosurePrint x []

        void $ printf commaRef []

        y <- getSnd ptr
        _ <- callClosure ClosurePrint y []

        -- print the closing bracket
        void $ printf closeParenRef []

instance CompileConstant a => CompileConstant [a] where
    -- we represent an empty list as a null pointer while a cons cell is
    -- represented as a non-null pointer to a structure containing two
    -- pointers to closures: the head and the tail
    compileConstant _ [] = pure $ Null listTyPtr
    compileConstant name (x:xs) = do
        -- generate a static closure for the head
        xr <- compileSubConstant (name <> "_head") x

        -- generate a static closure for the tail
        xsr <- compileSubConstant "list" xs

        -- create a new global for this cons cell comprised of the
        -- pointers to the head and tail
        _ <- global (mkName name) listTy $ Struct Nothing False [xr, xsr]

        -- return a reference to the global
        pure $ GlobalReference listTyPtr (mkName name)

    -- to load a list, we just retrieve its pointer from the closure
    compileLoadConstant = loadFromClosure (ClosureFreeVar 0) listTyPtr

    compilePrintBody _ this = do
        -- retrieve the pointer to the pair
        ptr <- compileLoadConstant @[a] this

        -- check whether the list is empty or a cons cell and use the
        -- appropriate pretty-printing code for each case
        listCase ptr (printf emptyListRef [] >> retVoid) $ do
            _ <- printf openParenRef []

            -- list is a cons cell: both components are pointers to closures;
            -- retrieve them and call their respective pretty-printing functions
            x <- getHead ptr
            _ <- callClosure ClosurePrint x []

            _ <- printf consRef []

            xs <- getTail ptr
            _ <- callClosure ClosurePrint xs []

            _ <- printf closeParenRef []

            retVoid

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
compileConst :: MonadCodeGen m => Some (ValueOf DefaultUni) -> m ClosurePtr
compileConst (Some (ValueOf tag (x :: a))) = do
    name <- mkFresh "con"

    bring ccProxy tag $ compileConstStatic name x

-- | `compileConstStatic` @name value@ generates code for a static constant
-- with the name given by @name@ and the value given by @value@.
compileConstStatic
    :: forall a m . (MonadCodeGen m, CompileConstant a) => String -> a -> m ClosurePtr
compileConstStatic name x = do
    ptr <- compileConstant name x

    let entryName = name <> "_entry"

    codePtr <- compileConstEntry entryName (compileLoadConstant @a)

    -- 2. generate print code
    let builder = compilePrintBody @a name
    print_fun <- compileConstPrint name builder

    -- 3. generate a static closure: this should be sufficient since constants
    -- hopefully do not contain any free variables
    compileClosure False name codePtr print_fun [ptr]

-- | `compileConstDynamic` @value@ generates code which allocates a dynamic
-- closure for the value given by @value@. Note that this function must be
-- used with `XTypeApplications` in order to select a type for the type
-- variable @a@.
compileConstDynamic
    :: forall a m. (MonadCodeGen m, MonadIRBuilder m, CompileConstant a)
    => Operand
    -> m ClosurePtr
compileConstDynamic val = do
    name <- mkFresh "con"

    -- 1. generate entry code
    let entryName = name <> "_entry"
    codePtr <- compileConstEntry entryName $ compileLoadConstant @a

    -- 2. generate print code
    printPtr <- compileConstPrint name $ compilePrintBody @a name

    -- 3. generate a dynamic closure
    allocateClosure False codePtr printPtr [val]

-- | `retConstDynamic` @value@ is like `compileConstDynamic`, except that a
-- return instruction is inserted at the end which returns the pointer to the
-- new closure.
retConstDynamic
    :: forall a m. (MonadCodeGen m, MonadIRBuilder m, CompileConstant a)
    => Operand
    -> m ()
retConstDynamic val = compileConstDynamic @a val >>= retClosure

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
