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
    clsEntryParams,
    varEntryTy,
    printFnTy,
    mkClosureName,
    compileClosure,
    ptrSize,
    bits,
    HasFreeVars(..),
    allocateClosure,
    withFreeVars,
    compileDynamicClosure,
    compileFunPrint,

    -- * Loading data from closures
    ClosureComponent(..),
    loadFromClosure,
    callClosure,
    enterClosure,
    printClosure,
    compileApply,
    lookupVar,

    -- * Misc
    retClosure
) where

-------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.Reader

import Data.IORef
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Word

import LLVM.AST
import LLVM.AST.AddrSpace
import LLVM.AST.Constant as C
import LLVM.AST.Linkage

import Hachi.Compiler.CodeGen.Common
import Hachi.Compiler.CodeGen.Externals
import Hachi.Compiler.CodeGen.Globals as G
import Hachi.Compiler.CodeGen.IRBuilder as IR
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types

-------------------------------------------------------------------------------

-- | `closureTyDef` is a `Type` definition for closures. Note the layout is
-- as follows:
--
-- - A pointer to the code for the closure.
-- - A pointer to the print code for the closure.
-- - Closure flags.
-- - Zero or more free variables. (LLVM will accept more elements than the
-- array's indicated size) https://llvm.org/docs/LangRef.html#array-type
closureTyDef :: Type
closureTyDef = StructureType False
    [ clsEntryTy
    , printFnTy
    , iHost
    , ArrayType 0 (ptrOf i8)
    ]

-- | The minimum size of a closure in words.
closureSize :: Num a => a
closureSize = 3

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

-- | `fnTyForComponent` @component@ determines the function type for the
-- closure function identified by @component@.
fnTyForComponent :: ClosureComponent -> Type
fnTyForComponent ClosureCode = clsEntryTy
fnTyForComponent ClosurePrint = printFnTy
fnTyForComponent _ = error "fnTyForComponent called on non-function component"

-------------------------------------------------------------------------------

-- | `mkEntryTy` @arity@ generates a function signature for a closure
-- with @arity@-many parameters in addition to the parameter that
-- references the closure itself. In other words, the resulting
-- signature has 1+@arity@ many parameters.
mkEntryTy :: Int -> Type
mkEntryTy arity = ptrOf $ FunctionType closureTyPtr params False
    where params = closureTyPtr : replicate arity closureTyPtr

-- | `clsParamTys` is the list of parameter `Type`s for closure
-- entry functions.
clsParamTys :: [Type]
clsParamTys = [closureTyPtr, closureTyPtr, contTyPtr]

-- | `clsEntryTy` is the `Type` of closure entry functions.
clsEntryTy :: Type
clsEntryTy = ptrOf $ FunctionType closureTyPtr clsParamTys False

-- | `clsEntryParams` @varName@ constructs a list of parameters for a closure's
-- entry function, where @varName@ is the name of the variable being bound.
clsEntryParams :: T.Text -> [(Type, ParameterName)]
clsEntryParams var = zip clsParamTys ["this", mkParamName var, "k"]

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
    => Bool
    -> String
    -> Constant
    -> Constant
    -> [Constant]
    -> m (ClosurePtr 'StaticPtr)
compileClosure isPoly name codePtr printPtr fvs = do
    let closureName = mkClosureName name
    let closureType = StructureType False
            [ clsEntryTy, printFnTy
            , iHost
            , ArrayType (fromIntegral $ length fvs) (ptrOf i8)
            ]
    let closureVal = Struct Nothing False $
            codePtr : printPtr : Int platformIntSize (toInteger $ fromEnum isPoly) :
            [Array (ptrOf i8) $ map (`C.BitCast` ptrOf i8) fvs]

    void $ global closureName closureType closureVal $ setLinkage Private

    pure $ MkStaticClosurePtr $
        GlobalReference (PointerType closureType $ AddrSpace 0) closureName

-- | When we dynamically allocate closures, we need to know how much space to
-- allocate for the pointers; for this purpose we are currently assuming that
-- we are running on a 64-bit system.
bits :: Word32
bits = 64

-- | The size of a pointer as an `Operand`. This should correspond to the
-- value given by `bits`.
ptrSize :: Operand
ptrSize = ConstantOperand $ C.sizeof bits closureTyPtr

-- | A class of types which represent runtime structures that contain
-- free variables.
class HasFreeVars ptr where
    -- | `loadFreeVar` @ptr type index@ loads the free variable at @index@
    -- of the closure-like structure pointed to by @ptr@. If required,
    -- the resulting `Operand` is cast to @type@.
    loadFreeVar
        :: (MonadModuleBuilder m, MonadIRBuilder m)
        => ptr -> Type -> Integer -> m Operand

instance HasFreeVars (ClosurePtr k) where
    loadFreeVar this ty i = loadFromClosure (ClosureFreeVar i) ty this

instance HasFreeVars Continuation where
    loadFreeVar this ty i = do
        ptr <- castIfNeeded contTyPtr $ contPtr this
        addr <- gep ptr
            [ ConstantOperand (Int 32 0)
            , ConstantOperand (Int 32 1)
            , ConstantOperand (Int 32 i)
            ]
        load addr 0 >>= castIfNeeded (ptrOf ty)

-- | `allocateClosure` @isDelay codePtr printPtr freeVars@ allocates a closure
-- with enough space to store all of @freeVars@ in addition to the code
-- pointers. The code pointers are free variables are stored in the closure and
-- the pointer to the closure is returned.
allocateClosure
    :: (MonadCodeGen m, MonadIRBuilder m)
    => Bool -> Constant -> Constant -> [Operand]
    -> m (ClosurePtr 'DynamicPtr)
allocateClosure isDelay codePtr printPtr fvs = do
    -- allocate enough space for the closure on the heap:
    -- 2 code pointers + one pointer for each free variable
    size <- mul ptrSize (ConstantOperand $ Int bits $ closureSize + genericLength fvs)
    c <- malloc closureTyPtr size

    -- store data in the closure: we have the function pointers followed by
    -- any pointers to free variables
    let storeAt ixs val = do
            addr <- gep c $ ConstantOperand (Int 32 0) : ixs
            store addr 0 val

    storeAt (indicesForComponent ClosureCode) $ ConstantOperand codePtr
    storeAt (indicesForComponent ClosurePrint) $ ConstantOperand printPtr
    storeAt (indicesForComponent ClosureFlags) $ ConstantOperand $
        Int platformIntSize $ toInteger $ fromEnum isDelay

    forM_ (zip [0..] fvs) $ \(i,v) -> do
        ptr <- bitcast v (ptrOf i8)
        storeAt (indicesForComponent $ ClosureFreeVar i) ptr

    -- return an Operand representing a pointer to the dynamic closure that we
    -- have just created
    pure $ MkClosurePtr c

-- | `withFreeVars` @ptr computation@ runs @computation@ after retrieving all
-- free variables from the closure-like structure pointed to by @ptr@.
withFreeVars
    :: (HasFreeVars ptr, MonadCodeGen m, MonadIRBuilder m)
    => ptr -> m a -> m a
withFreeVars this m = do
    fvs <- currentFreeVars

    -- we need to load the free variables from the closure pointed to
    -- by `this`, which are stored at offset 3 onwards; note that the
    -- free variables are stored in alphabetical order and the code
    -- which loads them here must correspond to the code that stores
    -- them further down
    cvars <- forM (zip [0..] $ S.toList fvs) $
        \(i,(_,isCont)) ->
            if isCont
            then LocalCont . MkCont <$> loadFreeVar this contTyPtr i
            else LocalClosure . MkClosurePtr <$>
                    loadFreeVar this closureTyPtr i

    -- update the local environment with mappings to the free variables
    -- we have obtained from the closure represented by `this` and
    -- compile the body of the function
    updateEnv (S.map fst fvs) cvars m

-- | `compileDynamicClosure` @isDelay name freeVars var codeFun@ generates code
-- which dynamically creates a new closure when executed. The static parts of
-- the closure use @name@. The set given by @freeVars@ signals which variables
-- should be stored in the closure's free variables array. The name given by
-- @var@ is the variable that is bound by this closure. @codeFun@ is a
-- computation which generates the code for the body of the closure.
-- The function returns a `ClosurePtr` representing the new closure.
compileDynamicClosure
    :: MonadCodeGen m
    => Bool -> String -> S.Set (T.Text, Bool) -> T.Text
    -> (Operand -> Operand -> Continuation -> IRBuilderT m ())
    -> IRBuilderT m (ClosurePtr 'DynamicPtr)
compileDynamicClosure isDelay name fvs var codeFun = do
    let entryName = mkName $ name <> "_entry"
    let entryParams = clsEntryParams var

    _ <- lift $ IR.function entryName entryParams closureTyPtr plcFunOpts $
        \[this, arg, k] ->
            localFreeVars fvs $
            extendScope var (LocalClosure $ MkClosurePtr arg) $ do
            compileTrace (name <> "_entry") []

            -- update the local environment with mappings to the free variables
            -- we have obtained from the closure represented by `this` and
            -- compile the body of the function
            withFreeVars (MkClosurePtr this) $ codeFun this arg (MkCont k)

    let codePtr = GlobalReference clsEntryTy entryName

    printPtr <- compileFunPrint

    env <- asks codeGenEnv

    vals <- forM (S.toList fvs) $ \(fv,_) -> case M.lookup fv env of
        -- TODO: handle this a bit more nicely
        Nothing -> error "Can't find variable"
        Just val -> pure val

    allocateClosure isDelay codePtr printPtr (map localOperand vals)

-- | `compileFunPrint` is a computation which generates the shared
-- pretty-printing code for all function closures and returns a
-- reference to it.
compileFunPrint :: MonadCodeGen m => m Constant
compileFunPrint = do
    -- we need to check whether the code has already been generated or not:
    -- for that we try to retrieve it from the environment
    ioRef <- asks codeGenFunPrinter
    mRef <- liftIO $ readIORef ioRef

    case mRef of
        -- it exists: just return the reference
        Just ref -> pure ref
        -- it doesn't: generate the code, store the reference in the
        -- environment, and then return it
        Nothing -> do
            let printName = mkName "Function_print"

            _ <- IR.function printName [(closureTyPtr, "this")] VoidType plcFunOpts $ \[_] -> do
                compileTrace "Function_print" []
                void $ call (ConstantOperand printfRef) [(funErrRef, [])] plcCall
                retVoid

            let ref = GlobalReference printFnTy printName
            liftIO $ writeIORef ioRef $ Just ref

            pure ref

-------------------------------------------------------------------------------

-- | `loadFromClosure` @component type ptr@ loads the component described by
-- @component@ from the closure represented by @ptr@ and casts its type to
-- @type@.
loadFromClosure
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => ClosureComponent -> Type -> ClosurePtr k -> m Operand
loadFromClosure prop ty ptr = do
    let ix = indicesForComponent prop
    ptrt <- castToClosure (closurePtr ptr)
    addr <- gep ptrt $ ConstantOperand (Int 32 0) : ix

    r <- castIfNeeded (ptrOf ty) addr
    load r 0

-- | `callClosure` @component ptr args@ loads the component described by
-- @component@ from the closure represented by @ptr@, assumes it is a function,
-- and then calls it with @args@ as arguments.
callClosure
    :: (MonadFail m, MonadModuleBuilder m, MonadIRBuilder m)
    => ClosureComponent
    -> ClosurePtr k
    -> [Operand]
    -> m (ClosurePtr 'DynamicPtr)
callClosure prop closure argv = do
    let ty = fnTyForComponent prop
    entry <- loadFromClosure prop ty closure
    ptr <- castToClosure (closurePtr closure)

    let callArgs = (ptr, []) : [(arg, []) | arg <- argv]

    MkClosurePtr <$> call entry callArgs plcCall

-- `enterClosure` @ptr args@ enters the closure represented by @ptr@ and
-- provides the arguments given by @args@.
enterClosure
    :: (MonadFail m, MonadModuleBuilder m, MonadIRBuilder m)
    => ClosurePtr k -> Continuation -> Maybe Operand -> m (ClosurePtr 'DynamicPtr)
enterClosure ptr k mArg = do
    targ <- castToClosure arg
    callClosure ClosureCode ptr [targ, contPtr k]
    where arg = fromMaybe (ConstantOperand $ Null closureTyPtr) mArg

-- | `printClosure` @ptr@ invokes the pretty-printing function of the closure
-- represented by @ptr@.
printClosure
    :: (MonadFail m, MonadModuleBuilder m, MonadIRBuilder m)
    => ClosurePtr k -> m ()
printClosure ptr = void $ callClosure ClosurePrint ptr []

-- | `compileApply` @funClosure argClosure@ generates code which causes the
-- program to enter the closure represented by @funClosure@ with the argument
-- given by @argClosure@.
compileApply
    :: (MonadFail m, MonadModuleBuilder m, MonadIRBuilder m)
    => Continuation
    -> ClosurePtr f
    -> ClosurePtr x
    -> m (ClosurePtr 'DynamicPtr)
compileApply k f x =
    -- enter the closure pointed to by f, giving it a pointer to another
    -- closure x as argument
    enterClosure f k $ Just (closurePtr x)

-- | `lookupVar` @name type@ generates code which retrieves the variable named
-- @name@ from the local environment. If the variable is not in scope, we
-- generate code which prints an error at runtime. For convenience, we cast
-- the type of the resulting `Operand` to @type@, which will normally be
-- `closureTyPtr`.
lookupVar
    :: (MonadCodeGen m, MonadIRBuilder m, MonadFail m)
    => T.Text -> Type -> m Operand
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
                (T.unpack var <> " is not in scope.\n") (mkName name) $
                setLinkage Private
            void $ printf (ConstantOperand ptr) []
            void $ exit (-1)

            -- this part of the program should not be reachable
            unreachable
            pure $ ConstantOperand $ Null closureTyPtr
        Just (LocalClosure ptr) -> do
            compileTrace ("Found " <> T.unpack var <> " in " <> name) []
            ifTracing $ do
                printClosure ptr
                void $ printf nlRef []
            castIfNeeded closureTyPtr (closurePtr ptr)
        Just (LocalCont ptr) -> do
            compileTrace ("Found continuation " <> T.unpack var <> " in " <> name) []
            castIfNeeded contTyPtr (contPtr ptr)

-- | `retClosure` @closurePtr@ returns the pointer represented by @closurePtr@.
retClosure :: (MonadModuleBuilder m, MonadIRBuilder m) => ClosurePtr k -> m ()
retClosure ptr = castToClosure (closurePtr ptr) >>= ret

-------------------------------------------------------------------------------
