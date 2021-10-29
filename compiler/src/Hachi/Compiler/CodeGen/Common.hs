-- | This module contains common code generation functions.
module Hachi.Compiler.CodeGen.Common (
    mkParamName,
    compileTrace
) where

-------------------------------------------------------------------------------

import Control.Monad.Reader

import Data.String ( fromString )
import qualified Data.Text as T

import LLVM.AST
import LLVM.IRBuilder as IR

import Hachi.Compiler.CodeGen.Externals
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.Config

-------------------------------------------------------------------------------

-- | `mkParamName` @name@ converts a `T.Text` value into a `ParameterName`.
mkParamName :: T.Text -> ParameterName
mkParamName = fromString . T.unpack

-------------------------------------------------------------------------------

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
