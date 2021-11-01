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
    printFnTy,
    compileClosure,
    allocateClosure,
    compileDynamicClosure,
    compileMsgPrint,

    -- * Loading data from closures
    ClosureComponent(..),
    loadFromClosure,
    callClosure,
    enterClosure,
    lookupVar
) where

-------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.Reader

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Word

import LLVM.AST
import LLVM.AST.AddrSpace
import LLVM.AST.Constant
import LLVM.IRBuilder as IR

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
    , printFnTy
    , ptrOf VoidType
    , ArrayType 0 closureTyPtr
    ]

-- | The minimum size of a closure in words.
closureSize :: Num a => a
closureSize = 3

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
    | ClosureFlags
    | ClosureFreeVar Integer
    deriving (Eq, Show)

-- | `indexForComponent` @component@ determines the index of @component@
-- within a closure.
indexForComponent :: ClosureComponent -> Integer
indexForComponent ClosureCode = 0
indexForComponent ClosurePrint = 1
indexForComponent ClosureFlags = 2
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

-- | `printFnTy` is the `Type` of closure print functions. They are similar to
-- closure entry functions in that they take a pointer to the current closure
-- as argument, but return nothing.
printFnTy :: Type
printFnTy = ptrOf $ FunctionType VoidType [closureTyPtr] False

-- | `compileClosure` @name entryPtr printPtr@ generates a global variable
-- representing a closure for @name@.
compileClosure
    :: MonadModuleBuilder m
    => String -> Constant -> Constant -> [Constant] -> m Constant
compileClosure name codePtr printPtr fvs = do
    let closureName = mkName $ name <> "_closure"

    void $ global closureName closureTy $ Struct Nothing False $
        codePtr : printPtr : Int bits 0 : fvs

    pure $ GlobalReference (PointerType closureTy $ AddrSpace 0) closureName

-- | When we dynamically allocate closures, we need to know how much space to
-- allocate for the pointers; for this purpose we are currently assuming that
-- we are running on a 64-bit system.
bits :: Word32
bits = 64

-- | The size of a pointer as an `Operand`. This should correspond to the
-- value given by `bits`.
ptrSize :: Operand
ptrSize = ConstantOperand $ LLVM.AST.Constant.sizeof bits closureTyPtr

-- | `allocateClosure` @isDelay codePtr printPtr freeVars@ allocates a closure
-- with enough space to store all of @freeVars@ in addition to the code
-- pointers. The code pointers are free variables are stored in the closure and
-- the pointer to the closure is returned.
allocateClosure
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Bool -> Constant -> Constant -> [Operand] -> m Operand
allocateClosure isDelay codePtr printPtr fvs = do
    -- allocate enough space for the closure on the heap:
    -- 2 code pointers + one pointer for each free variable
    size <- mul ptrSize (ConstantOperand $ Int bits $ closureSize + genericLength fvs)
    c <- malloc size
    cc <- bitcast c closureTyPtr

    -- store data in the closure: we have the function pointers followed by
    -- any pointers to free variables
    let storeAt i val = do
            addr <- gep cc [ ConstantOperand $ Int 32 0
                           , ConstantOperand $ Int 32 i
                           ]
            store addr 0 val

    storeAt 0 $ ConstantOperand codePtr
    storeAt 1 $ ConstantOperand printPtr
    storeAt 2 $ ConstantOperand $ Int bits $ toInteger $ fromEnum isDelay

    forM_ (zip [closureSize..] fvs) $ uncurry storeAt

    -- return an Operand representing a pointer to the dynamic closure that we
    -- have just created
    pure cc

-- | `compileDynamicClosure` @name freeVars var codeFun@
compileDynamicClosure
    :: ( MonadIO m
       , MonadReader CodeGenSt m
       , MonadModuleBuilder m
       , MonadIRBuilder m
       )
    => Bool -> String -> S.Set T.Text -> T.Text
    -> (Operand -> Operand -> IRBuilderT m Operand)
    -> m Operand
compileDynamicClosure isDelay name fvs var codeFun = do
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
                    addr <- gep this [ ConstantOperand $ Int 32 0
                                     , ConstantOperand $ Int 32 i
                                     ]
                    ptr <- bitcast addr closureTyPtr
                    load ptr 0

            -- load the free variables from the closure pointed to by `this`
            cvars <- forM (zip [closureSize..] $ S.toList fvs) $ \(idx,_) -> loadFrom idx

            -- update the local environment with mappings to the free variables
            -- we have obtained from the closure represented by `this` and
            -- compile the body of the function
            termClosure <- updateEnv fvs cvars $ codeFun this arg
            ret termClosure

    let code_fun = GlobalReference entryTy entryName

    print_fun <- compileMsgPrint name "Evaluation resulted in a function."

    env <- asks codeGenEnv

    vals <- forM (S.toList fvs) $ \fv -> case M.lookup fv env of
        -- TODO: handle this a bit more nicely
        Nothing -> error "Can't find variable"
        Just val -> pure val

    allocateClosure isDelay code_fun print_fun vals

compileMsgPrint
    :: (MonadReader CodeGenSt m, MonadIO m, MonadModuleBuilder m)
    => String -> String -> m Constant
compileMsgPrint name msg = do
    let printName = mkName $ name <> "_print"

    _ <- IR.function printName [(closureTyPtr, "this")] VoidType $ \[_] -> do
        compileTrace (name <> "_print")
        (ptr, _) <- runIRBuilderT emptyIRBuilder $
            globalStringPtr (msg <> "\n") (mkName $ name <> "_print_msg")
        void $ call
            (ConstantOperand printfRef)
            [(ConstantOperand ptr, [])]
        retVoid

    pure $ GlobalReference printFnTy printName

-------------------------------------------------------------------------------

-- | `loadFromClosure` @component mType ptr@ loads the component described by
-- @component@ from the closure represented by @ptr@ and casts its type to
-- @mType@ if that is not `Nothing`.
loadFromClosure
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => ClosureComponent -> Maybe Type -> ClosurePtr -> m Operand
loadFromClosure prop mty (MkClosurePtr ptr) = do
    let ix = indexForComponent prop
    addr <- gep ptr [ConstantOperand $ Int 32 0, ConstantOperand $ Int 32 ix]

    case mty of
        Nothing -> load addr 0
        Just ty -> do
            r <- bitcast addr ty
            load r 0

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
    entry <- loadFromClosure prop Nothing closure
    call entry $ (closurePtr closure, []) : [(arg, []) | arg <- argv]

-- `enterClosure` @ptr args@ enters the closure represented by @ptr@ and
-- provides the arguments given by @args@.
enterClosure
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => ClosurePtr -> [Operand] -> m Operand
enterClosure = callClosure ClosureCode

-- | `lookupVar` @name type@ generates code which retrieves the variable named
-- @name@ from the local environment. If the variable is not in scope, we
-- generate code which prints an error at runtime. For convenience, we cast
-- the type of the resulting `Operand` to @type@, which will normally be
-- `closureTyPtr`.
lookupVar :: (MonadCodeGen m, MonadIRBuilder m) => T.Text -> Type -> m Operand
lookupVar var ty = do
    name <- mkFresh "var"
    env <- asks codeGenEnv

    case M.lookup var env of
        Nothing -> do
            -- generate code that prints a suitable error message
            ptr <- globalStringPtr
                (T.unpack var <> " is not in scope.\n") (mkName name)
            void $ printf (ConstantOperand ptr) []
            void $ exit (-1)

            -- this part of the program should not be reachable
            unreachable
            pure $ ConstantOperand $ Null closureTyPtr
        Just ptr -> do
            compileTrace $ "Found " <> T.unpack var <> " in " <> name
            bitcast ptr ty

-------------------------------------------------------------------------------