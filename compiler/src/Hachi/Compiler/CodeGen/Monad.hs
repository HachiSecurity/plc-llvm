{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Hachi.Compiler.CodeGen.Monad where

-------------------------------------------------------------------------------

import Control.Monad.Reader

import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import LLVM.AST
import LLVM.AST.Constant
import LLVM.IRBuilder

import qualified UntypedPlutusCore as UPLC

import Hachi.Compiler.Config

-------------------------------------------------------------------------------

-- | Represents the state of the code generator.
data CodeGenSt = MkCodeGenSt {
    -- | The configuration for the compiler.
    codeGenCfg :: Config,
    codeGenErrMsg :: Constant,
    codeGenCounter :: IORef Integer,
    codeGenEnv :: M.Map T.Text Operand
}

-- | The code generator monad.
newtype CodeGen m a = MkCodeGen { runCodeGen :: ReaderT CodeGenSt m a }
    deriving ( Functor, Applicative, Monad, MonadIO
             , MonadReader CodeGenSt
             , MonadModuleBuilder, MonadIRBuilder
             )

-- | `mkFresh` @prefix@ generates a fresh name starting with @prefix@.
mkFresh :: MonadIO m => String -> CodeGen m String
mkFresh prefix = do
    counter <- asks codeGenCounter
    val <- liftIO $ atomicModifyIORef counter $ \v -> (v+1,v)
    pure $ prefix <> show val

-- | `extendScope` @name operand action@ adds @name@ to the scope for
-- @action@ along with @operand@ which represents the LLVM identifier.
extendScope :: MonadReader CodeGenSt m => UPLC.Name -> Operand -> m a -> m a
extendScope name val = local $ \st ->
    st{ codeGenEnv = M.insert (UPLC.nameString name) val (codeGenEnv st) }

-- | `updateEnv` @names operands action@ updates the scope for @action@
-- with @operands@ which correspond to @names@. Important: the names in
-- @names@ must be in the same order as the corresponding entries in
-- @operands@.
updateEnv
    :: MonadReader CodeGenSt m
    => S.Set UPLC.Name -> [Operand] -> m a -> m a
updateEnv names operands = local $ \st ->
    st{ codeGenEnv = M.union namedOperands (codeGenEnv st) }
    where namedOperands = M.fromList
                        $ zip (map UPLC.nameString $ S.toList names) operands

-------------------------------------------------------------------------------
