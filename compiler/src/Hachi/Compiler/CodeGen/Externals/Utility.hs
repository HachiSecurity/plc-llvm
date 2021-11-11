module Hachi.Compiler.CodeGen.Externals.Utility (
    globalFromType
) where

-------------------------------------------------------------------------------

import LLVM.AST
import LLVM.AST.CallingConvention
import LLVM.AST.Linkage
import LLVM.AST.Visibility

-------------------------------------------------------------------------------

-- | `globalFromType` @name type@ constructs a `Global` given its @name@ and
-- @type@.
globalFromType :: String -> Type -> Global
globalFromType name (PointerType (FunctionType rt pts varArgs) _) =
    Function External Default Nothing C [] rt (mkName name) pTy []
        Nothing Nothing 0 Nothing Nothing [] Nothing []
    where pTy = ([Parameter ty (mkName "") [] | ty <- pts], varArgs)
globalFromType _ _ =
    error "globalFromType must be applied to a function pointer"

-------------------------------------------------------------------------------
