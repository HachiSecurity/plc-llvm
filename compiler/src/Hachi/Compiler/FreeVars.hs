
module Hachi.Compiler.FreeVars ( freeVars ) where

-------------------------------------------------------------------------------

import qualified Data.Set as S

import UntypedPlutusCore as UPLC

-------------------------------------------------------------------------------

-- | `freeVars` @term@ calculates the set of free variables for @term@.
-- For some reason, this function does not seem to exist in the plutus
-- libraries (it does exist for typed plutus and plutus IR though).
freeVars :: Ord name => Term name uni fun ann -> S.Set name
freeVars (Var _ n) = S.singleton n
freeVars (LamAbs _ n t) = S.delete n (freeVars t)
freeVars (Apply _ l r) = S.union (freeVars l) (freeVars r)
freeVars (Force _ t) = freeVars t
freeVars (Delay _ t) = freeVars t
freeVars (Constant _ _) = S.empty
freeVars (Builtin _ _) = S.empty
freeVars (Error _) = S.empty

-------------------------------------------------------------------------------
