{-# LANGUAGE RecordWildCards #-}

module Hachi.Compiler.CodeGen ( generateCode ) where

-------------------------------------------------------------------------------

import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.ByteString.Char8 qualified as BS
import Data.IORef
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.String (fromString)
import Data.Text qualified as T

import System.FilePath

import UntypedPlutusCore as UPLC

import LLVM
import LLVM.AST as LLVM
import LLVM.AST.Constant
import LLVM.AST.IntegerPredicate as LLVM
import LLVM.AST.Linkage
import LLVM.Context
import LLVM.Target

import Hachi.Compiler.CodeGen.Builtin
import Hachi.Compiler.CodeGen.Closure
import Hachi.Compiler.CodeGen.Common
import Hachi.Compiler.CodeGen.Constant
import Hachi.Compiler.CodeGen.CPS
import Hachi.Compiler.CodeGen.Driver
import Hachi.Compiler.CodeGen.Externals
import Hachi.Compiler.CodeGen.Globals as G
import Hachi.Compiler.CodeGen.IRBuilder as IR
import Hachi.Compiler.CodeGen.Library
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types
import Hachi.Compiler.Config
import Hachi.Compiler.FreeVars

-------------------------------------------------------------------------------

-- | `mkClosureVar` @name@ constructs a pair of @
mkClosureVar :: UPLC.Name -> (T.Text, Bool)
mkClosureVar n = (nameString n, False)

-- | `isSimpleTerm` @term@ determines whether @term@ is "simple" in the context
-- of a function application. Specifically, @term@ is simple if compiling
-- @term@ is guaranteed not to generate code which performs calls to PLC
-- functions.
isSimpleTerm :: Term UPLC.Name DefaultUni DefaultFun () -> Bool
isSimpleTerm Error{} = True
isSimpleTerm Var{} = True
isSimpleTerm LamAbs{} = True
isSimpleTerm Delay{} = True
isSimpleTerm Constant{} = True
isSimpleTerm Builtin{} = True
isSimpleTerm _ = False

-- | `compileForce` @name continuation closure terminator@ generates code
-- which forces the closure given by @closure@ and passes the result to
-- @continuation@. The computation @terminator@ is used to generate code
-- which handles the result of the continuation.
compileForce
    :: (MonadCodeGen m, MonadIRBuilder m)
    => String -> Continuation -> Operand
    -> (ClosurePtr 'DynamicPtr -> m a)
    -> m a
compileForce name k x terminator = do
    -- We need to determine whether evaluation of the body results in a
    -- delay term or not - the PLC interpreter treats the latter as an
    -- error so we should, too. For this purpose, we store some flags
    -- in the closure that indicate whether the closure belongs to a
    -- delay term or not. Here, we inspect those flags to determine
    -- whether to enter the closure or print an error.
    let trueBr = mkName $ name <> "_delay"
    let falseBr = mkName $ name <> "_fail"

    flags <- loadFromClosure ClosureFlags iHost (MkClosurePtr x)
    cond <- icmp LLVM.EQ flags (ConstantOperand $ Int platformIntSize 1)
    condBr cond trueBr falseBr

    -- Enter the closure that is returned from the body: it corresponds to a
    -- delay term
    emitBlockStart trueBr
    r <- compileApply k (MkClosurePtr x) (MkClosurePtr x) >>= terminator

    -- The closure does not belong to a delay term: this is a runtime error
    emitBlockStart falseBr
    void $ printf forceErrRef []
    void $ exit (-1)
    unreachable

    pure r

-- | `compileNonCPS` @term@ compiles a "simple" @term@.
compileNonCPS
    :: MonadCodeGen m
    => Term UPLC.Name DefaultUni DefaultFun ()
    -> IRBuilderT m (ClosurePtr 'DynamicPtr)
-- (error): as soon as execution reaches this term, print a message indicating
-- that an error condition has been reached and terminate execution
compileNonCPS (Error _) = do
    errMsg <- asks codeGenErrMsg

    -- print the error message and terminate the program
    void $ printf (ConstantOperand errMsg) []
    void $ exit (-1)

    -- this part of the program should not be reachable
    unreachable
    pure $ MkClosurePtr $ ConstantOperand $ Null closureTyPtr
-- (x): for variables, there are two possibilities:
-- 1. the variable is not in scope, in which case we print an error message
-- as soon as execution reaches the variable expression
-- 2. the variable is in scope, in which case it represents a pointer to a
-- closure which we return up the call stack
compileNonCPS (Var _ x) =
    MkClosurePtr <$> lookupVar (nameString x) closureTyPtr
-- (lam x e): since PLC has higher-order functions, functions may escape the
-- scope in which they are defined in. Therefore, we require closures which
-- store pointers to the free variables. We compile functions as follows:
-- 1. We generate a new LLVM function that represents the entry point of the
--    closure which is executed when we enter the closure. This function
--    expects a pointer to the closure and a pointer to its (sole) argument.
--    We assume that all variables represent pointers to closures, so that
--    the representation of the argument is uniform. The entry point assumes
--    that pointers to all free variables are stored in the closure and we
--    generate code to retrieve them from there before updating the local
--    environment used to compile the body.
-- 2. We generate code that dynamically allocates a closure. The closure will
--    consist of a pointer to the entry function that we generated in step 1
--    and pointers to all free variables.
compileNonCPS (LamAbs _ var term) = do
    name <- mkFresh "fun"

    -- TODO: the S.map here might be overly pessimistic, we might be able
    -- to replace it with S.mapMonotonic for better performance
    let fvs = S.map mkClosureVar
            $ S.delete var $ freeVars term

    -- return the pointer to the new closure to the continuation
    compileDynamicClosure False name fvs (UPLC.nameString var) $
        \_ _ k' -> compileBody k' term >>= retClosure
-- (delay t): we compile this just like a lambda abstraction to prevent it
-- from being executed straight away - a force expression is then just like
-- function application, minus the argument
compileNonCPS (Delay _ term) = do
    name <- mkFresh "delay"

    -- TODO: the S.map here might be overly pessimistic, we might be able
    -- to replace it with S.mapMonotonic for better performance
    let fvs = S.map mkClosureVar $ freeVars term

    -- for now we are compiling it exactly the same way as a function and,
    -- therefore, if evaluation results in a delay term, we get a result
    -- equivalent to the one we get when evaluation results in a function;
    -- in the future, we might want to print a different message
    compileDynamicClosure True name fvs "_delayArg" $
        \_ _ k' -> compileBody k' term >>= retClosure
compileNonCPS (Constant _ val) = toDynamicPtr <$> compileConst val
compileNonCPS (Builtin _ f) = do
    builtins <- asks codeGenBuiltins
    case M.lookup f builtins of
        Nothing -> error $ "No such builtin: " <> show f
        Just ref -> pure $ toDynamicPtr ref
compileNonCPS _ = fail "compileNonCPS: called on a complex term"

-- | `compileBody` @term@ compiles @term@ to LLVM.
compileBody
    :: MonadCodeGen m
    => Continuation
    -> Term UPLC.Name DefaultUni DefaultFun ()
    -> IRBuilderT m (ClosurePtr 'DynamicPtr)
compileBody _ t@(Error _) = compileNonCPS t
compileBody k (Apply _ lhs rhs)
    -- compiling applications is the most complicated aspect of the code
    -- generation, since the generated code involves a guaranteed function
    -- call at the end: this is, on its own, not a problem, but it is if
    -- either operand of the application is also an application, because
    -- we must then perform a CPS transformation to ensure that all calls
    -- are tail calls; to avoid introducing this complexity where it is
    -- not required, we perform a simple check here to see if both operands
    -- are "simple" and will not result in code that performs any function
    -- calls, in which case we don't need to perform a CPS transformation
    | isSimpleTerm lhs && isSimpleTerm rhs = do
        name <- mkFresh "app"

        compileTrace name []

        -- both calls to `compileBody` here are guaranteed to not use `k` or
        -- make any PLC function calls, since `isSimpleTerm` is `True` for
        -- both of them
        l <- compileNonCPS lhs
        r <- compileNonCPS rhs

        compileTrace ("Entering closure in " <> name) []

        -- enter the closure pointed to by l, giving it a pointer to another
        -- closure r as argument
        compileApply k l r
    | otherwise = do
        -- if the operands of the function application are not "simple", then
        -- we essentially need to perform some lambda-lifting; consider the
        -- following PLC program:
        --
        -- [[f x] [g y]]
        --
        -- Compiling [f x] will result in at least one function call and
        -- compiling [g y] will result in at least one function call; while
        -- we also need to perform a function call for the outer application.
        -- Therefore, we need to break this up into essentially the following
        -- program (with explicit continuations, using Haskell notation):
        --
        -- \k f g x y -> let _cont_l a = let _cont_r b = a k b
        --                               in g _cont_r y
        --               in f _cont_l x
        --
        -- which is equivalent to
        --
        -- \k f g x y -> f (\a -> g (\b -> a k b) y) x
        --
        -- In particular, note that `k` is free in both of the continuations
        -- and that `a` is free in the second continuation.
        name <- mkFresh "app"
        fvs <- currentFreeVars

        compileTrace name []

        -- generate some names for the parameters of the continuations, as well
        -- as the current continuation that we need to store as a free variable
        -- in the continuations
        let argK = T.pack (name <> "_k")
        let argR = T.pack (name <> "_arg_r")
        let argL = T.pack (name <> "_arg_l")

        -- calculate the sets of free variables of the two continuations
        let fvsR = S.map mkClosureVar (freeVars rhs)
                `S.union` S.singleton (argL, False)
                `S.union` S.singleton (argK, True)
        let fvsL = S.map mkClosureVar (S.union (freeVars lhs) (freeVars rhs))
                `S.union` S.singleton (argK, True)

        -- generate code for the first continuation
        contL <-
            extendScope argK (LocalCont k) $
            compileDynamicCont (name <> "_cont_l") fvsL argL $
            \_ _ -> do
                -- generate code for the second continuation; we need to do
                -- this inside of the first continuation since we wish to
                -- capture its argument as a free variable
                contR <- compileDynamicCont (name <> "_cont_r") fvsR argR $
                    \_ b -> do
                        compileTrace ("Entering closure in " <> name) []

                        -- retrieve the initial continuation and the result of
                        -- evaluating the LHS from the current continuation
                        -- closure
                        k <- MkCont <$> lookupVar argK contTyPtr
                        a <- MkClosurePtr <$> lookupVar argL closureTyPtr

                        -- enter the closure pointed to by `a`, giving it a
                        -- pointer to another closure `b` as argument
                        compileApply k a (MkClosurePtr b) >>= retClosure

                -- compile the body of the right sub-term and instruct the
                -- code generator to use `contR` as the continuation
                r <- compileBody contR rhs
                retClosure r

        -- compile the body of the left sub-term and instruct the code
        -- generator to use `contL` as the continuation
        compileBody contL lhs
compileBody k (Force _ term)
    -- similarly to applications, we can generate simpler code if `term`
    -- is "simple"
    | isSimpleTerm term = do
        name <- mkFresh "force"
        compileTrace name []

        let contBr = mkName $ name <> "_cont"

        -- compile `term`, which we know won't result in any function
        -- calls since the check for `isSimpleTerm` passed; if force is
        -- successful (i.e. applied to a delay term), then we jump to
        -- the `contBr` label, whcih skips past the error branch
        r <- compileNonCPS term
        ptr <- compileForce name k (closurePtr r) $ \ptr -> do
            br contBr
            pure ptr

        emitBlockStart contBr
        pure ptr
    | otherwise = do
        name <- mkFresh "force"
        compileTrace name []

        -- a `Force` term is similar to a function application, except that the
        -- function argument is meaningless and not used. Since `term` may be
        -- a function application itself
        let argK = T.pack (name <> "_k")
        let arg = T.pack (name <> "_arg")

        -- calculate the set of free variables of the continuation
        let fvs = S.map mkClosureVar (freeVars term)
                `S.union` S.singleton (argK, True)

        cont <- extendScope argK (LocalCont k) $ compileDynamicCont (name <> "_cont") fvs arg $
            \_ x -> do
                k <- MkCont <$> lookupVar argK contTyPtr
                compileForce name k x retClosure

        -- Generate code for the term, this can be an arbitrary term and may
        -- not immediately be a delay term - indeed, there may not be a delay
        -- term at all!
        compileBody cont term
compileBody k simpleTerm = compileNonCPS simpleTerm >>= callCont k

-- | `commonEntry` @term@ generates common entry code that is the same for
-- both executables and libraries.
commonEntry
    :: MonadCodeGen m
    => Continuation
    -> Term UPLC.Name DefaultUni DefaultFun ()
    -> IRBuilderT m (ClosurePtr 'DynamicPtr)
commonEntry k body = do
    generateConstantGlobals

    -- initialise the runtime system
    rtsInit

    -- generate code for all the built-in functions: it is easier for us to
    -- do this programmatically rather than in e.g. C since the functions
    -- all need to be curried; however, it may be nice to only compile the
    -- builtins we actually need for the current program in the future
    builtins <- compileBuiltins

    -- compile the program; the resulting Operand represent a pointer to some
    -- kind of closure (function or constant)
    local (\st -> st{codeGenBuiltins = builtins}) $ compileBody k body

-- | `generateEntry` @term@ generates code for @term@ surrounded by a small
-- wrapper which calls the print function of the closure that results from
-- executing the program generated from @term@.
generateEntry
    :: MonadCodeGen m
    => FilePath
    -> Term UPLC.Name DefaultUni DefaultFun () -> m ()
generateEntry outPath body = do
    MkConfig{..} <- asks codeGenCfg

    k <- compileCpsReturn

    if cfgLibrary
    then do
        void $ IR.function "plc_entry" [] closureTyPtr id $ \_ -> do
            ptr <- commonEntry k body

            ret $ closurePtr ptr

        api <- emitLibraryApi

        let headerFile = replaceExtension outPath "h"
        liftIO $ writeFile headerFile $
            "#include \"rts.h\"\n\n" <>
            "extern closure *plc_entry();\n\n" <>
            api <> "\n"
    else void $ IR.function "main" [] VoidType id $ \_ -> do
        ptr <- commonEntry k body

        -- call the print code of the resulting closure
        printClosure ptr
        void $ printf nlRef []

        retVoid

compileProgram
    :: Config
    -> FilePath
    -> Program UPLC.Name DefaultUni DefaultFun ()
    -> ModuleBuilderT IO ()
compileProgram cfg outPath (Program _ _ term) = do
    -- global definitions
    forM_ externalDefinitions emitDefn

    _ <- typedef "closure" $ Just closureTyDef
    _ <- typedef "continuation" $ Just contTyDef
    _ <- typedef "bytestring" $ Just bytestringTyDef
    _ <- typedef "pair" $ Just pairTyDef
    _ <- typedef "list" $ Just listTyDef
    _ <- typedef "data" $ Just dataTyDef
    _ <- typedef "mpz_t" $ Just gmpTyDef

    (errorMsg, _) <- runIRBuilderT emptyIRBuilder $
        globalStringPtr "Something has gone wrong.\n" "errorMsg" $
        setLinkage LinkOnce

    -- initialise the compilation context with empty counters
    counter <- liftIO $ newIORef M.empty
    constEntries <- liftIO $ newIORef M.empty
    constPrinters <- liftIO $ newIORef M.empty
    funPrinter <- liftIO $ newIORef Nothing

    let codeGenSt = MkCodeGenSt{
            codeGenCfg = cfg,
            codeGenErrMsg = errorMsg,
            codeGenCounters = counter,
            codeGenEnv = M.empty,
            codeGenBuiltins = M.empty,
            codeGenConstEntries = constEntries,
            codeGenConstPrinters = constPrinters,
            codeGenFunPrinter = funPrinter,
            codeGenFreeVars = S.empty
        }

    -- compile the program
    void $ runReaderT (runCodeGen (generateEntry outPath term)) codeGenSt

-------------------------------------------------------------------------------

-- | `generateCode` @config path program@ generates code for @program@
-- using the configuration given by @config@.
generateCode
    :: Config
    -> Program UPLC.Name DefaultUni DefaultFun ()
    -> IO ()
generateCode cfg@MkConfig{..} p =
    let outputName = fromMaybe cfgInput cfgOutput
        compiler = compileProgram cfg outputName p
    in withContext $ \ctx ->
    -- withModuleFromLLVMAssembly ctx (File "wrapper.ll") $ \m -> moduleAST m >>= print

    withHostTargetMachineDefault $ \tm ->
    buildModuleT (fromString $ takeBaseName cfgInput) compiler >>= \compiled ->
    withModuleFromAST ctx compiled{
        moduleSourceFileName = fromString cfgInput
    } $ \m -> do
        -- by default, we generate LLVM bitcode (the binary representation),
        -- but if the user has requested plain-text LLVM IR, we generate
        -- that instead
        if cfgNoSerialise
        then do
            let llName = replaceExtension outputName "ll"
            asm <- moduleLLVMAssembly m
            BS.writeFile llName asm
        else do
            let bcName = replaceExtension outputName "bc"
            writeBitcodeToFile (File bcName) m

        -- unless we have been instructed not to assemble the LLVM IR into
        -- an object file, produce an object file
        unless cfgNoAssemble $ do
            let objectFile = replaceExtension outputName "o"
            writeObjectToFile tm (File objectFile) m

            -- unless we have been instructed not to link together an
            -- executable, run Clang to produce an executable from the
            -- object file
            unless (cfgNoLink || cfgLibrary) $
                linkExecutable cfg outputName [objectFile]

            -- if we are compiling a library and the --entry-point option is
            -- specified, we also compile the C program and link it together
            -- with our PLC object file; this has the advantage that we know
            -- what options to pass to the C compiler already, so that a
            -- user doesn't have to figure this out by hand
            when cfgLibrary $ case cfgEntryPoint of
                Nothing -> pure ()
                Just cFile -> do
                    -- compile the wrapper program to an object file
                    programObj <- buildObjectFile cfg cFile

                    -- link everything together
                    linkExecutable cfg outputName [programObj, objectFile]

-------------------------------------------------------------------------------
