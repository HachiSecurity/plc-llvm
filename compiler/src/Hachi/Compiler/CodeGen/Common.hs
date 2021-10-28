-- | This module contains common code generation functions.
module Hachi.Compiler.CodeGen.Common (
    mkParamName,
    compileConstPrint,
    compileMsgPrint,
    compileTrace
) where

-------------------------------------------------------------------------------

import Control.Monad.Reader

import Data.String ( fromString )
import qualified Data.Text as T

import PlutusCore.Pretty
import qualified UntypedPlutusCore as UPLC

import LLVM.AST.Constant
import LLVM.AST
import LLVM.IRBuilder as IR

import Hachi.Compiler.CodeGen.Externals
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types
import Hachi.Compiler.Config

-------------------------------------------------------------------------------

-- | `mkParamName` @name@ converts a `UPLC.Name` into a `ParameterName`.
mkParamName :: UPLC.Name -> ParameterName
mkParamName = fromString . T.unpack . UPLC.nameString

-------------------------------------------------------------------------------

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
    => String -> m ()
compileTrace msg = do
    -- only generate this code if tracing is enabled
    isTracing <- asks (cfgTrace . codeGenCfg)
    when isTracing $ do
        -- generate a fresh, global variable to store the string in
        name <- mkFresh "trace"
        (ptr, _) <- runIRBuilderT emptyIRBuilder $
                globalStringPtr (msg <> "\n") (mkName name)

        -- call printf
        void $ call
            (ConstantOperand printfRef)
            [(ConstantOperand ptr, [])]

-------------------------------------------------------------------------------
