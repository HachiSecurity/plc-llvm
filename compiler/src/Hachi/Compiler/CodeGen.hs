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
import qualified Data.Text as T

import System.FilePath
import System.Process.Typed

import PlutusCore.Pretty ( Pretty, pretty )
import UntypedPlutusCore as UPLC

import LLVM
import LLVM.AST as LLVM
import LLVM.Context
import LLVM.Target
import LLVM.AST.AddrSpace
import LLVM.AST.Constant
import LLVM.IRBuilder as IR

import Hachi.Compiler.Config
import Hachi.Compiler.FreeVars
import Hachi.Compiler.CodeGen.Externals
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types

import Data.Maybe (fromMaybe)

-------------------------------------------------------------------------------

mkParamName :: UPLC.Name -> ParameterName
mkParamName = fromString . T.unpack . nameString

-- | `compileNoopEntry` @name@ generates a closure entry function which does
-- nothing except return the pointer to itself.
compileNoopEntry
    :: (MonadReader CodeGenSt m, MonadIO m, MonadModuleBuilder m)
    => String -> m Constant
compileNoopEntry name = do
    let entryName = mkName $ name <> "_entry"

    _ <- IR.function entryName [(closureTyPtr, "this")] closureTyPtr $
        \[this] -> compileTrace (name <> "_entry") >> ret this

    pure $ GlobalReference clsEntryTy entryName

-- | `compileConstPrint` @name value@ compiles a print function for a
-- constant.
compileConstPrint
    :: (MonadReader CodeGenSt m, MonadIO m, MonadModuleBuilder m, Pretty a)
    => String -> a -> m Constant
compileConstPrint name val = compileMsgPrint name (show $ pretty val)

compileMsgPrint
    :: (MonadReader CodeGenSt m, MonadIO m, MonadModuleBuilder m)
    => String -> String -> m Constant
compileMsgPrint name msg = do
    let printName = mkName $ name <> "_print"

    _ <- IR.function printName [] VoidType $ \_ -> do
        compileTrace (name <> "_print")
        (ptr, _) <- runIRBuilderT emptyIRBuilder $
            globalStringPtr (msg <> "\n") (mkName $ name <> "_print_msg")
        void $ call
            (ConstantOperand printfRef)
            [(ConstantOperand ptr, [])]
        retVoid

    pure $ GlobalReference funPtr printName

-- | `compileTrace` @message@ generates an instruction which prints @message@
-- to the standard output, if tracing is enabled in the code generation
-- configuration. Otherwise, this produces no code.
compileTrace
    :: ( MonadReader CodeGenSt m
       , MonadIO m
       , MonadModuleBuilder m
       , MonadIRBuilder m
       )
    => String
    -> m ()
compileTrace msg = do
    isTracing <- asks (cfgTrace . codeGenCfg)
    when isTracing $ do
        name <- mkFresh "trace"
        (ptr, _) <- runIRBuilderT emptyIRBuilder $
                globalStringPtr (msg <> "\n") (mkName name )
        void $ call
            (ConstantOperand printfRef)
            [(ConstantOperand ptr, [])]


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

callClosure
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => ClosureComponent
    -> ClosurePtr
    -> [Operand]
    -> m Operand
callClosure prop closure args = do
    entry <- loadFromClosure prop closure
    call entry $ (closurePtr closure, []) : [(arg, []) | arg <- args]

enterClosure
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => ClosurePtr
    -> [Operand]
    -> m Operand
enterClosure = callClosure ClosureCode

-- | `compileBody` @term@
compileBody
    :: ( MonadReader CodeGenSt m
       , MonadIRBuilder m
       , MonadModuleBuilder m
       , MonadIO m
       )
    => Term UPLC.Name DefaultUni DefaultFun ()
    -> m Operand
-- (error): as soon as execution reaches this term, print a message indicating
-- that an error condition has been reached and terminate execution
compileBody (Error _) = do
    errMsg <- asks codeGenErrMsg

    -- print the error message and terminate the program
    void $ printf $ ConstantOperand errMsg
    void $ exit (-1)

    -- this part of the program should not be reachable
    unreachable
    pure $ ConstantOperand $ Null closureTyPtr
-- (x): for variables, there are two possibilities:
-- 1. the variable is not in scope, in which case we print an error message
-- as soon as execution reaches the variable expression
-- 2. the variable is in scope, in which case it represents a pointer to a
-- closure which we return up the call stack
compileBody (Var _ x) = do
    name <- mkFresh "var"

    compileTrace name

    -- retrieve the current environment to see if this variable is free or not
    -- if it is free, we just generate some no-op code so that the pointer to
    -- the closure is returned, which will then cause the rts to print the
    -- variable name if it ends up being the result of the program
    -- otherwise, if the variable is not free, we push the closure that
    -- corresponds to variable in scope
    env <- asks codeGenEnv

    case M.lookup (nameString x) env of
        Nothing -> do
            -- generate code that prints a suitable error message
            ptr <- globalStringPtr
                (T.unpack (nameString x) <> " is not in scope.\n") (mkName name)
            void $ printf $ ConstantOperand ptr
            void $ exit (-1)

            -- this part of the program should not be reachable
            unreachable
            pure $ ConstantOperand $ Null closureTyPtr
        Just ptr -> do
            compileTrace $ "Found " <> T.unpack (nameString x) <> " in " <> name
            ret ptr
            pure ptr
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

    -- we dynamically allocate a closure so that we can store the free
    -- variables along with it - for now, we assume that we are running
    -- on a 64 bit platform and calculate the pointer size accordingly
    let bits = 64
    let ptrSize = ConstantOperand $ LLVM.AST.Constant.sizeof bits closureTyPtr

    let entryName = mkName $ name <> "_entry"
    let fvs = S.delete var $ freeVars term
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
            termClosure <- updateEnv fvs cvars $ compileBody term
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
    enterClosure (MkClosurePtr l) [r]
compileBody (Constant _ val) = do
    name <- mkFresh "con"

    -- 1. generate entry code
    entry_fun <- compileNoopEntry name

    -- 2. generate print code
    print_fun <- compileConstPrint name val

    -- 3. generate a static closure: this should be sufficient since constants
    -- hopefully do not contain any free variables
    ConstantOperand <$> compileClosure name entry_fun print_fun []
compileBody term = error $
    "compileBody for " <> show term <> " not implemented!"

-- | `generateEntry` @term@ generates code for @term@ surrounded by a small
-- wrapper which calls the print function of the closure that results from
-- executing the program generated from @term@.
generateEntry
    :: (MonadReader CodeGenSt m, MonadModuleBuilder m, MonadIO m)
    => Term UPLC.Name DefaultUni DefaultFun () -> m ()
generateEntry body = void $ IR.function "entry" [] VoidType $ \_ -> do
    -- compile the program; the resulting Operand represent a pointer to some
    -- kind of closure (function or constant)
    ptr <- compileBody body

    -- call the print code of the resulting closure
    printFun <- loadFromClosure ClosurePrint $ MkClosurePtr ptr
    void $ call printFun []

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
            codeGenEnv = M.empty
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
        let pcfg = proc "clang" [rtsFile, objectFile, "-o", exeFile]

        ec <- runProcess pcfg

        when cfgVerbose $ do
            putStr "Ran clang and got: "
            print ec

-------------------------------------------------------------------------------
