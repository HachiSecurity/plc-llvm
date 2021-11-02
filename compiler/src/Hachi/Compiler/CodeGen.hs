{-# LANGUAGE RecordWildCards #-}

module Hachi.Compiler.CodeGen ( generateCode ) where

-------------------------------------------------------------------------------

import Control.Monad.IO.Class
import Control.Monad.Reader

import qualified Data.ByteString.Char8 as BS
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.String (fromString)

import System.FilePath
import System.Process.Typed

import UntypedPlutusCore as UPLC

import LLVM
import LLVM.AST as LLVM
import LLVM.AST.Constant
import LLVM.AST.IntegerPredicate as LLVM
import LLVM.Context
import LLVM.Target
import LLVM.IRBuilder as IR

import Hachi.Compiler.Config
import Hachi.Compiler.FreeVars
import Hachi.Compiler.CodeGen.Builtin
import Hachi.Compiler.CodeGen.Closure
import Hachi.Compiler.CodeGen.Common
import Hachi.Compiler.CodeGen.Constant
import Hachi.Compiler.CodeGen.Externals
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types

-------------------------------------------------------------------------------

-- | `compileBody` @term@ compiles @term@ to LLVM.
compileBody
    :: ( MonadReader CodeGenSt m
       , MonadModuleBuilder m
       , MonadIO m
       )
    => Term UPLC.Name DefaultUni DefaultFun ()
    -> IRBuilderT m ClosurePtr
-- (error): as soon as execution reaches this term, print a message indicating
-- that an error condition has been reached and terminate execution
compileBody (Error _) = do
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
compileBody (Var _ x) = MkClosurePtr <$> lookupVar (nameString x) closureTyPtr
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
compileBody (LamAbs _ var term) = do
    name <- mkFresh "fun"

    -- TODO: the S.map here might be overly pessimistic, we might be able
    -- to replace it with S.mapMonotonic for better performance
    let fvs = S.map nameString $ S.delete var $ freeVars term

    compileDynamicClosure False name fvs (UPLC.nameString var) $
        \_ _ -> compileBody term
compileBody (Apply _ lhs rhs) = do
    name <- mkFresh "app"

    compileTrace name

    -- generate code for both sub-expressions; the resulting operands are
    -- pointers to closures
    l <- compileBody lhs
    r <- compileBody rhs

    compileTrace $ "Entering closure in " <> name

    -- enter the closure pointed to by l, giving it a pointer to another
    -- closure r as argument
    enterClosure l [closurePtr r]
compileBody (Force _ term) = do
    name <- mkFresh "force"
    compileTrace name

    -- Generate code for the term, this can be an arbitrary term and may
    -- not immediately be a delay term - indeed, there may not be a delay
    -- term at all!
    r <- compileBody term

    -- We need to determine whether evaluation of the body results in a
    -- delay term or not - the PLC interpreter treats the latter as an
    -- error so we should, too. For this purpose, we store some flags
    -- in the closure that indicate whether the closure belongs to a
    -- delay term or not. Here, we inspect those flags to determine
    -- whether to enter the closure or print an error.
    let trueBr = mkName $ name <> "_delay"
    let falseBr = mkName $ name <> "_fail"
    let contBr = mkName $ name <> "_cont"

    flags <- loadFromClosure ClosureFlags i64 r
    cond <- icmp LLVM.EQ flags (ConstantOperand $ Int 64 1)
    condBr cond trueBr falseBr

    -- Enter the closure that is returned from the body: it corresponds to a
    -- delay term
    emitBlockStart trueBr
    ptr <- enterClosure r [closurePtr r]
    br contBr

    -- The closure does not belong to a delay term: this is a runtime error
    emitBlockStart falseBr
    void $ printf forceErrRef []
    void $ exit (-1)
    unreachable

    emitBlockStart contBr

    pure ptr
-- (delay t): we compile this just like a lambda abstraction to prevent it
-- from being executed straight away - a force expression is then just like
-- function application, minus the argument
compileBody (Delay _ term) = do
    name <- mkFresh "delay"

    -- TODO: the S.map here might be overly pessimistic, we might be able
    -- to replace it with S.mapMonotonic for better performance
    let fvs = S.map nameString $ freeVars term

    -- for now we are compiling it exactly the same way as a function and,
    -- therefore, if evaluation results in a delay term, we get a result
    -- equivalent to the one we get when evaluation results in a function;
    -- in the future, we might want to print a different message
    compileDynamicClosure True name fvs "_delayArg" $
        \_ _ -> compileBody term
compileBody (Constant _ val) = compileConst val
compileBody (Builtin _ f) = do
    builtins <- asks codeGenBuiltins
    case M.lookup f builtins of
        Nothing -> error $ "No such builtin: " <> show f
        Just ref -> pure ref

-- | `generateEntry` @term@ generates code for @term@ surrounded by a small
-- wrapper which calls the print function of the closure that results from
-- executing the program generated from @term@.
generateEntry
    :: MonadCodeGen m
    => Term UPLC.Name DefaultUni DefaultFun () -> m ()
generateEntry body = void $ IR.function "main" [] VoidType $ \_ -> do
    generateConstantGlobals

    -- generate code for all the built-in functions: it is easier for us to
    -- do this programmatically rather than in e.g. C since the functions
    -- all need to be curried; however, it may be nice to only compile the
    -- builtins we actually need for the current program in the future
    builtins <- compileBuiltins

    -- compile the program; the resulting Operand represent a pointer to some
    -- kind of closure (function or constant)
    ptr <- local (\st -> st{codeGenBuiltins = builtins}) $ compileBody body

    -- call the print code of the resulting closure
    printFun <- loadFromClosure ClosurePrint printFnTy ptr
    void $ call printFun [(closurePtr ptr, [])]

    retVoid

compileProgram
    :: Config
    -> Program UPLC.Name DefaultUni DefaultFun ()
    -> ModuleBuilderT IO ()
compileProgram cfg (Program _ _ term) = do
    -- global definitions
    emitDefn $ GlobalDefinition printfFun
    emitDefn $ GlobalDefinition exitFun
    emitDefn $ GlobalDefinition mallocFun

    _ <- typedef "closure" $ Just closureTyDef
    (errorMsg, _) <- runIRBuilderT emptyIRBuilder $
        globalStringPtr "Something has gone wrong.\n" "errorMsg"

    -- initialise the compilation context with empty counters
    counter <- liftIO $ newIORef M.empty

    let codeGenSt = MkCodeGenSt{
            codeGenCfg = cfg,
            codeGenErrMsg = errorMsg,
            codeGenCounters = counter,
            codeGenEnv = M.empty,
            codeGenBuiltins = M.empty
        }

    -- compile the program
    void $ runReaderT (runCodeGen (generateEntry term)) codeGenSt

-------------------------------------------------------------------------------

-- | `generateCode` @config path program@ generates code for @program@
-- using the configuration given by @config@.
generateCode
    :: Config
    -> Program UPLC.Name DefaultUni DefaultFun ()
    -> IO ()
generateCode cfg@MkConfig{..} p =
    withContext $ \ctx ->
    -- withModuleFromLLVMAssembly ctx (File "wrapper.ll") $ \m -> moduleAST m >>= print

    withHostTargetMachineDefault $ \tm ->
    buildModuleT (fromString $ takeBaseName cfgInput) (compileProgram cfg p) >>= \compiled ->
    withModuleFromAST ctx compiled{ moduleSourceFileName = fromString cfgInput } $ \m -> do
        let objectFile = fromMaybe (replaceExtension cfgInput "o") cfgOutput
        -- writeBitcodeToFile (File outPath) m
        -- writeTargetAssemblyToFile tm (File "out.s") mod
        writeObjectToFile tm (File objectFile) m

        when cfgVerbose $ do
            asm <- moduleLLVMAssembly m
            BS.putStrLn asm

        -- compile with LLVM
        let rtsFile = fromMaybe "./rts/rts.c" cfgRTS
        let exeFile = dropExtension objectFile
        let pcfg = proc "clang" [objectFile, "-o", exeFile]

        ec <- runProcess pcfg

        when cfgVerbose $ do
            putStr "Ran clang and got: "
            print ec

-------------------------------------------------------------------------------
