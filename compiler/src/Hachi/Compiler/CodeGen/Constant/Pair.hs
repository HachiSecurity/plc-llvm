
module Hachi.Compiler.CodeGen.Constant.Pair (
    newPair,
    getFst,
    getSnd
) where

-------------------------------------------------------------------------------

import LLVM.AST
import LLVM.AST.Constant
import LLVM.IRBuilder as IR

import Hachi.Compiler.CodeGen.Closure
import Hachi.Compiler.CodeGen.Externals
import Hachi.Compiler.CodeGen.Types

-------------------------------------------------------------------------------

-- | `newPair` @fst snd@ generates code which constructs a new pair value,
-- where the first component is @fst@ and the second component is @snd@.
newPair
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => ClosurePtr p -> ClosurePtr q -> m Operand
newPair x y = do
    -- allocate memory for the new pair
    size <- IR.sizeof 64 pairTy
    ptr <- malloc pairTyPtr size

    -- store the two components of the pair
    store ptr 0 $ closurePtr x

    addr <- gep ptr [ ConstantOperand $ Int 32 0, ConstantOperand $ Int 32 1 ]
    store addr 0 $ closurePtr y

    -- return the pointer to the pair
    pure ptr

-- | `getFst` @pairPtr@ generates code which retrieves the first component
-- from the pair pointed at by @pairPtr@.
getFst
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> m (ClosurePtr 'DynamicPtr)
getFst ptr = do
    addr <- gep ptr [ ConstantOperand $ Int 32 0, ConstantOperand $ Int 32 0 ]
    clsPtr <- load addr 0
    MkClosurePtr <$> bitcast clsPtr closureTyPtr

-- | `getSnd` @pairPtr@ generates code which retrieves the second component
-- from the pair pointed at by @pairPtr@.
getSnd
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> m (ClosurePtr 'DynamicPtr)
getSnd ptr = do
    addr <- gep ptr [ ConstantOperand $ Int 32 0, ConstantOperand $ Int 32 1 ]
    clsPtr <- load addr 0
    MkClosurePtr <$> bitcast clsPtr closureTyPtr

-------------------------------------------------------------------------------
