{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module Hachi.Compiler.CodeGen.Monad where

-------------------------------------------------------------------------------

import Control.Monad.Reader

import Data.IORef
import Data.Kind
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import LLVM.AST.Constant
import LLVM.IRBuilder

import qualified UntypedPlutusCore as UPLC

import Hachi.Compiler.Config
import Hachi.Compiler.CodeGen.Types

-------------------------------------------------------------------------------

-- | Represents the state of the code generator.
data CodeGenSt = MkCodeGenSt {
    -- | The configuration for the compiler.
    codeGenCfg :: Config,
    codeGenErrMsg :: Constant,
    codeGenCounters :: IORef (M.Map String (IORef Integer)),
    codeGenEnv :: M.Map T.Text (ClosurePtr 'DynamicPtr),
    codeGenBuiltins :: M.Map UPLC.DefaultFun (ClosurePtr 'StaticPtr)
}

-- | The code generator monad.
newtype CodeGen m a = MkCodeGen { runCodeGen :: ReaderT CodeGenSt m a }
    deriving ( Functor, Applicative, Monad, MonadIO
             , MonadReader CodeGenSt
             , MonadModuleBuilder, MonadIRBuilder
             )

-- | A constraint synonym for the type class constraints we usually expect our
-- code generation monad to satisfy.
type MonadCodeGen :: (* -> *) -> Constraint
type MonadCodeGen m =
    (MonadIO m, MonadModuleBuilder m, MonadReader CodeGenSt m)

-- | `mkFresh` @prefix@ generates a fresh name starting with @prefix@.
mkFresh :: (MonadReader CodeGenSt m, MonadIO m) => String -> m String
mkFresh prefix = asks codeGenCounters >>= \countersRef-> liftIO $ do
    counters <- readIORef countersRef
    case M.lookup prefix counters of
        Nothing -> do
            ref <- newIORef 1
            writeIORef countersRef $ M.insert prefix ref counters
            pure $ prefix <> "0"
        Just counter -> do
            val <- liftIO $ atomicModifyIORef counter $ \v -> (v+1,v)
            pure $ prefix <> show val

-- | `extendScope` @name operand action@ adds @name@ to the scope for
-- @action@ along with @operand@ which represents the LLVM identifier.
extendScope
    :: MonadReader CodeGenSt m
    => T.Text -> ClosurePtr 'DynamicPtr -> m a -> m a
extendScope name val = local $ \st ->
    st{ codeGenEnv = M.insert name val (codeGenEnv st) }

-- | `updateEnv` @names operands action@ updates the scope for @action@
-- with @operands@ which correspond to @names@. Important: the names in
-- @names@ must be in the same order as the corresponding entries in
-- @operands@.
updateEnv
    :: MonadReader CodeGenSt m
    => S.Set T.Text -> [ClosurePtr 'DynamicPtr] -> m a -> m a
updateEnv names operands = local $ \st ->
    st{ codeGenEnv = M.union namedOperands (codeGenEnv st) }
    where namedOperands = M.fromList
                        $ zip (S.toList names) operands

-------------------------------------------------------------------------------
