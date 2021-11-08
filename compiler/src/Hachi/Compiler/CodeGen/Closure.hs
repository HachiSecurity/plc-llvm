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
    mkClosureName,
    compileClosure,
    allocateClosure,
    compileDynamicClosure,
    compileFunPrint,

    -- * Loading data from closures
    ClosureComponent(..),
    loadFromClosure,
    callClosure,
    enterClosure,
    lookupVar,

    -- * Misc
    retClosure
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
import Hachi.Compiler.CodeGen.Globals
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types

-------------------------------------------------------------------------------

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

-- | `indicesForComponent` @component@ determines the indices of @component@
-- within a closure.
indicesForComponent :: ClosureComponent -> [Operand]
indicesForComponent ClosureCode = [ConstantOperand $ Int 32 0]
indicesForComponent ClosurePrint = [ConstantOperand $ Int 32 1]
indicesForComponent ClosureFlags = [ConstantOperand $ Int 32 2]
indicesForComponent (ClosureFreeVar n) =
    [ConstantOperand $ Int 32 closureSize, ConstantOperand $ Int 32 n]

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

-- | `mkClosureName` @name@ returns the name of a closure for @name@.
mkClosureName :: String -> Name
mkClosureName name = mkName $ name <> "_closure"

-- | `compileClosure` @isPoly name entryPtr printPtr@ generates a global
-- variable representing a closure for @name@.
compileClosure
    :: MonadModuleBuilder m
    => Bool -> String -> Constant -> Constant -> [Constant] -> m ClosurePtr
compileClosure isPoly name codePtr printPtr fvs = do
    let closureName = mkClosureName name

    void $ global closureName closureTy $ Struct Nothing False $
        codePtr : printPtr : Int bits (toInteger $ fromEnum isPoly) : fvs

    pure $ MkClosurePtr $ ConstantOperand $
        GlobalReference (PointerType closureTy $ AddrSpace 0) closureName

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
    => Bool -> Constant -> Constant -> [Operand] -> m ClosurePtr
allocateClosure isDelay codePtr printPtr fvs = do
    -- allocate enough space for the closure on the heap:
    -- 2 code pointers + one pointer for each free variable
    size <- mul ptrSize (ConstantOperand $ Int bits $ closureSize + genericLength fvs)
    c <- malloc size
    cc <- bitcast c closureTyPtr

    -- store data in the closure: we have the function pointers followed by
    -- any pointers to free variables
    let storeAt ixs val = do
            addr <- gep cc $ ConstantOperand (Int 32 0) : ixs
            store addr 0 val

    storeAt (indicesForComponent ClosureCode) $ ConstantOperand codePtr
    storeAt (indicesForComponent ClosurePrint) $ ConstantOperand printPtr
    storeAt (indicesForComponent ClosureFlags) $
        ConstantOperand $ Int bits $ toInteger $ fromEnum isDelay

    forM_ (zip [0..] fvs) $ \(i,v) ->
        storeAt (indicesForComponent $ ClosureFreeVar i) v

    -- return an Operand representing a pointer to the dynamic closure that we
    -- have just created
    pure $ MkClosurePtr cc

-- | `compileDynamicClosure` @isDelay name freeVars var codeFun@ generates code
-- which dynamically creates a new closure when executed. The static parts of
-- the closure use @name@. The set given by @freeVars@ signals which variables
-- should be stored in the closure's free variables array. The name given by
-- @var@ is the variable that is bound by this closure. @codeFun@ is a
-- computation which generates the code for the body of the closure.
-- The function returns a `ClosurePtr` representing the new closure.
compileDynamicClosure
    :: MonadCodeGen m
    => Bool -> String -> S.Set T.Text -> T.Text
    -> (Operand -> Operand -> IRBuilderT m ())
    -> IRBuilderT m ClosurePtr
compileDynamicClosure isDelay name fvs var codeFun = do
    let entryName = mkName $ name <> "_entry"
    let entryTy = mkEntryTy 1

    _ <- lift $ IR.function entryName [(closureTyPtr, "this"), (closureTyPtr, mkParamName var)] closureTyPtr $
        \[this, arg] -> extendScope var (MkClosurePtr arg) $ do
            compileTrace (name <> "_entry")

            -- we need to load the free variables from the closure pointed to
            -- by `this`, which are stored at offset 3 onwards; note that the
            -- free variables are stored in alphabetical order and the code
            -- which loads them here must correspond to the code that stores
            -- them further down
            cvars <- forM (zip [0..] $ S.toList fvs) $ \(i,_) ->
                loadFromClosure (ClosureFreeVar i) closureTyPtr (MkClosurePtr this)

            -- update the local environment with mappings to the free variables
            -- we have obtained from the closure represented by `this` and
            -- compile the body of the function
            updateEnv fvs (map MkClosurePtr cvars) $ codeFun this arg

    let code_fun = GlobalReference entryTy entryName

    print_fun <- compileFunPrint name

    env <- asks codeGenEnv

    vals <- forM (S.toList fvs) $ \fv -> case M.lookup fv env of
        -- TODO: handle this a bit more nicely
        Nothing -> error "Can't find variable"
        Just val -> pure val

    allocateClosure isDelay code_fun print_fun (map closurePtr vals)

compileFunPrint
    :: (MonadReader CodeGenSt m, MonadIO m, MonadModuleBuilder m)
    => String -> m Constant
compileFunPrint name = do
    let printName = mkName $ name <> "_print"

    _ <- IR.function printName [(closureTyPtr, "this")] VoidType $ \[_] -> do
        compileTrace (name <> "_print")
        void $ call (ConstantOperand printfRef) [(funErrRef, [])]
        retVoid

    pure $ GlobalReference printFnTy printName

-------------------------------------------------------------------------------

-- | `loadFromClosure` @component type ptr@ loads the component described by
-- @component@ from the closure represented by @ptr@ and casts its type to
-- @type@.
loadFromClosure
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => ClosureComponent -> Type -> ClosurePtr -> m Operand
loadFromClosure prop ty (MkClosurePtr ptr) = do
    let ix = indicesForComponent prop
    ptrt <- bitcast ptr closureTyPtr
    addr <- gep ptrt $ ConstantOperand (Int 32 0) : ix

    r <- bitcast addr (ptrOf ty)
    load r 0

-- | `callClosure` @component ptr args@ loads the component described by
-- @component@ from the closure represented by @ptr@, assumes it is a function,
-- and then calls it with @args@ as arguments.
callClosure
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => ClosureComponent
    -> ClosurePtr
    -> [Operand]
    -> m ClosurePtr
callClosure prop closure argv = do
    entry <- loadFromClosure prop clsEntryTy closure
    fmap MkClosurePtr <$> call entry $
        (closurePtr closure, []) : [(arg, []) | arg <- argv]

-- `enterClosure` @ptr args@ enters the closure represented by @ptr@ and
-- provides the arguments given by @args@.
enterClosure
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => ClosurePtr -> [Operand] -> m ClosurePtr
enterClosure = callClosure ClosureCode

-- | `lookupVar` @name type@ generates code which retrieves the variable named
-- @name@ from the local environment. If the variable is not in scope, we
-- generate code which prints an error at runtime. For convenience, we cast
-- the type of the resulting `Operand` to @type@, which will normally be
-- `closureTyPtr`.
lookupVar :: (MonadCodeGen m, MonadIRBuilder m) => T.Text -> Type -> m Operand
lookupVar var ty = do
    name <- mkFresh "var"

    -- retrieve the current environment to see if this variable is free or not
    -- if it is free, we just generate some no-op code so that the pointer to
    -- the closure is returned, which will then cause the rts to print the
    -- variable name if it ends up being the result of the program
    -- otherwise, if the variable is not free, we push the closure that
    -- corresponds to variable in scope
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
            bitcast (closurePtr ptr) ty

-- | `retClosure` @closurePtr@ returns the pointer represented by @closurePtr@.
retClosure :: MonadIRBuilder m => ClosurePtr -> m ()
retClosure = ret . closurePtr

-------------------------------------------------------------------------------
