-- | This module contains common code generation functions.
module Hachi.Compiler.CodeGen.Common (
    mkParamName,
    fatal,
    ifTracing,
    compileTrace
) where

-------------------------------------------------------------------------------

import Control.Monad.Extra
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

-- | `ifTracing` @action@ will execute @action@ if tracing is enabled.
ifTracing :: MonadCodeGen m => m () -> m ()
ifTracing = whenM (asks (cfgTrace . codeGenCfg))

-- | `compileTrace` @message arguments@ generates code which prints
-- @message@ to the standard output. The @message@ string may contain
-- formatting characters which will be substituted with values from
-- @arguments@. If tracing is not enabled in the code generation
-- configuration, no code is generated.
compileTrace
    :: ( MonadCodeGen m, MonadIRBuilder m )
    => String -> [Operand] -> m ()
compileTrace msg xs = ifTracing $ do
    -- generate a fresh, global variable to store the string in
    name <- mkFresh "trace"
    (ptr, _) <- runIRBuilderT emptyIRBuilder $
            globalStringPtr (msg <> "\n") (mkName name)

    -- call printf
    void $ call
        (ConstantOperand printfRef) $
        (ConstantOperand ptr, []) : [(x, []) | x <- xs]

-------------------------------------------------------------------------------
