-- | This module contains common code generation functions.
module Hachi.Compiler.CodeGen.Common (
    mkParamName,
    fatal,
    compileTrace
) where

-------------------------------------------------------------------------------

import Control.Monad.Reader

import Data.String ( fromString )
import qualified Data.Text as T

import LLVM.AST
import LLVM.AST.Constant()
import LLVM.IRBuilder as IR

import Hachi.Compiler.CodeGen.Externals
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.Config

-------------------------------------------------------------------------------

-- | `mkParamName` @name@ converts a `T.Text` value into a `ParameterName`.
mkParamName :: T.Text -> ParameterName
mkParamName = fromString . T.unpack

-------------------------------------------------------------------------------

-- | `fatal` @strRef args@ generates code which terminates execution of the
-- resulting program after displaying the string pointed to by @strRef@.
-- Optionally, if the string contains formatting characters, @args@ are used
-- as values for those placeholders.
fatal
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> [Operand] -> m ()
fatal ptr argv = do
    _ <- printf ptr argv
    _ <- exit (-1)
    unreachable

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
