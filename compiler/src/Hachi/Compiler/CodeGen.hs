{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Hachi.Compiler.CodeGen ( generateCode ) where

-------------------------------------------------------------------------------

import Control.Monad ( void, when )
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.Reader.Class ( MonadReader )
import Control.Monad.Trans (lift)

import qualified Data.ByteString.Char8 as BS
import Data.IORef
import qualified Data.Map as M
import Data.String (fromString)

import System.FilePath
import System.Process.Typed

import PlutusCore.Pretty ( Pretty, pretty )
import UntypedPlutusCore as UPLC

import LLVM
import LLVM.AST as LLVM
import LLVM.AST.CallingConvention
import LLVM.AST.Linkage
import LLVM.AST.Visibility
import LLVM.Context
import LLVM.Target
import LLVM.AST.AddrSpace
import LLVM.AST.Constant
import LLVM.IRBuilder as IR

import Hachi.Compiler.Config
import Data.Maybe (fromMaybe)

-------------------------------------------------------------------------------

i8 :: Type
i8 = IntegerType 8

i32 :: Type
i32 = IntegerType 32

-- | `clsEntryTy` is the `Type` of closure entry functions.
clsEntryTy :: Type
clsEntryTy = PointerType (FunctionType closureTyPtr [closureTyPtr] False) (AddrSpace 0)

lamEntryTy :: Type
lamEntryTy = PointerType (FunctionType closureTyPtr [closureTyPtr, closureTyPtr] False) (AddrSpace 0)

funPtr :: Type
funPtr = PointerType (FunctionType VoidType [] False) (AddrSpace 0)

-- | The type of printf.
printfTy :: Type
printfTy = PointerType fnTy (AddrSpace 0)
    where fnTy = FunctionType i32 [PointerType i8 $ AddrSpace 0] True

-- | The signature of printf.
printfFun :: Global
printfFun =
    Function External Default Nothing C [] i32 (mkName "printf") pTy []
        Nothing Nothing 0 Nothing Nothing [] Nothing []
    where pTy = ([Parameter (PointerType i8 $ AddrSpace 0) (LLVM.Name "") []], True)

-- | A `Constant` which provides a reference to the global printf declaration.
printfRef :: Constant
printfRef = GlobalReference printfTy $ mkName "printf"

exitTy :: Type
exitTy = PointerType fnTy (AddrSpace 0)
    where fnTy = FunctionType VoidType [i32] False

exitFun :: Global
exitFun =
    Function External Default Nothing C [] VoidType (mkName "exit") pTy []
        Nothing Nothing 0 Nothing Nothing [] Nothing []
    where pTy = ([Parameter i32 (mkName "") []], False)

exitRef :: Constant
exitRef = GlobalReference exitTy $ mkName "exit"

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
    , clsEntryTy
    , funPtr
    , ArrayType 0 funPtr
    ]

-- | `closureTy` is a `Type` for closures.
closureTy :: Type
closureTy = NamedTypeReference "closure"

-- | `closureTyPtr` is a `Type` representing a pointer to a closure.
closureTyPtr :: Type
closureTyPtr = PointerType closureTy $ AddrSpace 0

-------------------------------------------------------------------------------

-- | Represents the state of the code generator.
data CodeGenSt = MkCodeGenSt {
    codeGenCfg :: Config,
    codeGenErrMsg :: Constant,
    codeGenCounter :: IORef Integer,
    codeGenEnv :: M.Map String Operand
}

-- | The code generator monad.
newtype CodeGen m a = MkCodeGen { runCodeGen :: ReaderT CodeGenSt m a }
    deriving ( Functor, Applicative, Monad, MonadIO
             , MonadReader CodeGenSt
             , MonadModuleBuilder, MonadIRBuilder
             )

mkFresh :: MonadIO m => String -> CodeGen m String
mkFresh prefix = do
    counter <- asks codeGenCounter
    val <- liftIO $ atomicModifyIORef counter $ \v -> (v+1,v)
    pure $ prefix <> show val

-------------------------------------------------------------------------------

-- | `compileNoopEntry` @name@ generates a closure entry function which does
-- nothing except return the pointer to itself.
compileNoopEntry :: (MonadIO m, MonadModuleBuilder m) => String -> CodeGen m Constant
compileNoopEntry name = do
    let entryName = mkName $ name <> "_entry"

    _ <- IR.function entryName [(closureTyPtr, "this")] closureTyPtr $
        \[this] -> compileTrace (name <> "_entry") >> ret this

    pure $ GlobalReference clsEntryTy entryName

-- | `compileConstPrint` @name value@ compiles a print function for a
-- constant.
compileConstPrint
    :: (MonadIO m, MonadModuleBuilder m, Pretty a)
    => String -> a -> CodeGen m Constant
compileConstPrint name val = compileMsgPrint name (show $ pretty val)

compileMsgPrint
    :: (MonadIO m, MonadModuleBuilder m)
    => String -> String -> CodeGen m Constant
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
    :: (MonadIO m, MonadModuleBuilder m)
    => String
    -> IRBuilderT (CodeGen m) ()
compileTrace msg = lift (asks (cfgTrace . codeGenCfg)) >>=
    \isTracing -> when isTracing $ do
        name <- lift $ mkFresh "trace"
        (ptr, _) <- runIRBuilderT emptyIRBuilder $
                globalStringPtr (msg <> "\n") (mkName name )
        void $ call
            (ConstantOperand printfRef)
            [(ConstantOperand ptr, [])]


-- | `compileClosure` @name entryPtr printPtr@ generates a global variable
-- representing a closure for @name@.
compileClosure
    :: MonadModuleBuilder m
    => String -> Constant -> Constant -> Constant -> m Constant
compileClosure name codePtr entryPtr printPtr = do
    let closureName = mkName $ name <> "_closure"

    void $ global closureName closureTy $ Struct Nothing False [codePtr, entryPtr, printPtr]

    pure $ GlobalReference (PointerType closureTy $ AddrSpace 0) closureName

callClosure
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Integer
    -> Operand
    -> [Operand]
    -> m Operand
callClosure ix closure args = do
    entryAddr <- gep closure
        [ ConstantOperand $ Int 32 0, ConstantOperand $ Int 32 ix ]
    entry <- load entryAddr 0
    call entry $ (closure, []) : [(arg, []) | arg <- args]

pushClosure
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand
    -> [Operand]
    -> m Operand
pushClosure = callClosure 0

enterClosure
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand
    -> [Operand]
    -> m Operand
enterClosure = callClosure 1

-- | `compileTerm` @errMsg term@ compiles @term@ to LLVM.
compileTerm
    :: Term UPLC.Name DefaultUni DefaultFun ()
    -> CodeGen (ModuleBuilderT IO) Constant
compileTerm (Error _) = do
    name <- mkFresh "err"

    let code_name = mkName $ name <> "_code"

    _ <- IR.function code_name [(closureTyPtr, "this")] closureTyPtr $ \[this] -> do
        errMsg <- asks codeGenErrMsg
        void $ call
            (ConstantOperand $ GlobalReference printfTy $ mkName "printf")
            [(ConstantOperand errMsg, [])]
        void $ call (ConstantOperand exitRef) [(ConstantOperand $ Int 32 (-1), [])]
        ret this

    let code_fun = GlobalReference clsEntryTy code_name

    let entry_name = mkName $ name <> "_entry"

    _ <- IR.function entry_name [(closureTyPtr, "this")] closureTyPtr $ \[this] -> do
        errMsg <- asks codeGenErrMsg
        void $ call
            (ConstantOperand $ GlobalReference printfTy $ mkName "printf")
            [(ConstantOperand errMsg, [])]
        -- TODO: exit immediately
        ret this

    let entry_fun = GlobalReference clsEntryTy entry_name

    print_fun <- compileMsgPrint name
        "Print function called for error, this shouldn't happen."

    compileClosure name code_fun entry_fun print_fun
compileTerm (Constant _ val) = do
    name <- mkFresh "con"

    -- 1. generate entry code
    entry_fun <- compileNoopEntry name

    -- 2. generate print code
    print_fun <- compileConstPrint name val

    -- 3. generate the closure
    compileClosure name entry_fun entry_fun print_fun
compileTerm (Var _ x) = do
    name <- mkFresh "var"

    code_fun <- compileNoopEntry (name <> "_code")

    let entryName = mkName $ name <> "_entry"

    _ <- IR.function entryName [(closureTyPtr, "this"), (closureTyPtr, "arg")] closureTyPtr $
        \[this, arg] -> do
            compileTrace (name <> "_entry")
            op <- pushClosure arg []
            ret op

    let entry_fun = GlobalReference lamEntryTy entryName

    print_fun <- compileMsgPrint name (show x)

    compileClosure name code_fun entry_fun print_fun
compileTerm (LamAbs _ var term) = do
    name <- mkFresh "fun"

    code_fun <- compileNoopEntry (name <> "_code")

    termClosure <- compileTerm term

    let entryName = mkName $ name <> "_entry"

    _ <- IR.function entryName [(closureTyPtr, "this"), (closureTyPtr, "arg")] closureTyPtr $
        \[this, arg] -> do
            compileTrace (name <> "_entry")
            op <- enterClosure (ConstantOperand termClosure) [arg]
            ret op

    let entry_fun = GlobalReference lamEntryTy entryName

    print_fun <- compileMsgPrint name "Evaluation resulted in a function."

    compileClosure name code_fun entry_fun print_fun
compileTerm (Apply _ lhs rhs) = do
    name <- mkFresh "app"

    -- compile the lhs and rhs
    lhsClosure <- compileTerm lhs
    rhsClosure <- compileTerm rhs

    let codeName = mkName $ name <> "_code"

    _ <- IR.function codeName [(closureTyPtr, "this")] closureTyPtr $
        \[this] -> do
            compileTrace name
            lhsResult <- pushClosure (ConstantOperand lhsClosure) []
            rhsResult <- pushClosure (ConstantOperand rhsClosure) []
            result <- enterClosure lhsResult [rhsResult]

            ret result

    let code_fun = GlobalReference clsEntryTy codeName

    print_fun <- compileMsgPrint name
        "Print function called for application, this shouldn't happen."

    compileClosure name code_fun code_fun print_fun
-- TODO: the rest of the owl
compileTerm term = error $ "compileTerm for " <> show term <> " not implemented!"

generateEntry :: Constant -> ModuleBuilderT IO ()
generateEntry closure = void $ IR.function "entry" [] VoidType $ \_ -> do
    -- enter the first closure and get back a pointer to the result
    -- of the program (which is also a closure)
    entryAddr <- gep (ConstantOperand closure)
        [ ConstantOperand $ Int 32 0, ConstantOperand $ Int 32 0 ]
    entry <- load entryAddr 0
    resultClosure <- call entry [(ConstantOperand closure, [])]

    -- call the print code of the resulting closure
    printAddr <- gep resultClosure [ConstantOperand $ Int 32 0, ConstantOperand $ Int 32 2]
    printFun <- load printAddr 0
    _ <- call printFun []

    retVoid

compileProgram
    :: Config
    -> Program UPLC.Name DefaultUni DefaultFun ()
    -> ModuleBuilderT IO ()
compileProgram cfg (Program _ _ term) = do
    -- global definitions
    emitDefn $ GlobalDefinition printfFun
    emitDefn $ GlobalDefinition exitFun

    _ <- typedef "closure" $ Just closureTyDef
    (errorMsg, _) <- runIRBuilderT emptyIRBuilder $
        globalStringPtr "Something has gone wrong.\n" "errorMsg"

    -- initialise the compilation context and compile the root term
    counter <- liftIO $ newIORef 0
    closure <- runReaderT (runCodeGen (compileTerm term))
        MkCodeGenSt{
            codeGenCfg = cfg,
            codeGenErrMsg = errorMsg,
            codeGenCounter = counter,
            codeGenEnv = M.empty
        }

    -- generate the entry wrapper
    generateEntry closure

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
        let outPath = fromMaybe (replaceExtension cfgInput ".ll") cfgOutput


        writeBitcodeToFile (File outPath) m
    -- -- writeTargetAssemblyToFile tm (File "out.s") mod
        let objectFile = replaceExtension outPath "o"
        writeObjectToFile tm (File objectFile) m
        asm <- moduleLLVMAssembly m
        BS.putStrLn asm

        -- compile with LLVM
        let rtsFile = fromMaybe "./rts/rts.c" cfgRTS
        let exeFile = dropExtension objectFile
        let pcfg = proc "clang" [rtsFile, objectFile, "-o", exeFile]

        ec <- runProcess pcfg

        putStr "Ran clang and got: "
        print ec

-------------------------------------------------------------------------------
