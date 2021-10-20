{-# LANGUAGE OverloadedStrings #-}

module Hachi.Compiler.CodeGen ( generateCode ) where

-------------------------------------------------------------------------------

import Control.Monad ( void )

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

-------------------------------------------------------------------------------

i8 :: Type
i8 = IntegerType 8

i32 :: Type
i32 = IntegerType 32

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

-- | `compileTerm` @errMsg term@ compiles @term@ to LLVM.
compileTerm 
    :: Constant 
    -> Term UPLC.Name DefaultUni DefaultFun () 
    -> ModuleBuilderT IO ()
compileTerm errMsg (Error _) = void $ IR.function "entry" [] VoidType $ \_ -> do
    void $ call 
        (ConstantOperand $ GlobalReference printfTy $ mkName "printf") 
        [(ConstantOperand errMsg, [])]
    retVoid 
-- TODO: the rest of the owl
compileTerm _ _ = pure ()

compileProgram 
    :: Program UPLC.Name DefaultUni DefaultFun () 
    -> ModuleBuilderT IO ()
compileProgram (Program _ _ term) = do
    emitDefn $ GlobalDefinition printfFun
    errorMsg <- runIRBuilderT emptyIRBuilder $ 
        globalStringPtr "Something has gone wrong.\n" "errorMsg"
    compileTerm (fst errorMsg) term
    pure ()

-------------------------------------------------------------------------------

generateCode :: FilePath -> Program UPLC.Name DefaultUni DefaultFun () -> IO ()
generateCode fp p = 
    withContext $ \ctx ->
    -- withModuleFromLLVMAssembly ctx (File "wrapper.ll") $ \m -> moduleAST m >>= print

    withHostTargetMachineDefault $ \tm ->
    buildModuleT "test" (compileProgram p) >>= \compiled ->
    withModuleFromAST ctx compiled{ moduleSourceFileName  = "test.plc" } $ \m -> do
    
        writeBitcodeToFile (File fp) m
    -- -- writeTargetAssemblyToFile tm (File "out.s") mod
        writeObjectToFile tm (File "out.o") m

-------------------------------------------------------------------------------
