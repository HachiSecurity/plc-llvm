
module Hachi.Compiler.CodeGen.Constant.Pair (
    getFst,
    getSnd
) where

-------------------------------------------------------------------------------

import LLVM.AST
import LLVM.AST.Constant
import LLVM.IRBuilder

import Hachi.Compiler.CodeGen.Types

-------------------------------------------------------------------------------

-- | `getFst` @pairPtr@ generates code which retrieves the first component
-- from the pair pointed at by @pairPtr@.
getFst :: (MonadModuleBuilder m, MonadIRBuilder m) => Operand -> m ClosurePtr
getFst ptr = MkClosurePtr <$> load ptr 0

-- | `getSnd` @pairPtr@ generates code which retrieves the second component
-- from the pair pointed at by @pairPtr@.
getSnd :: (MonadModuleBuilder m, MonadIRBuilder m) => Operand -> m ClosurePtr
getSnd ptr = do
    addr <- gep ptr [ ConstantOperand $ Int 32 0, ConstantOperand $ Int 32 1 ]
    MkClosurePtr <$> load addr 0

-------------------------------------------------------------------------------
