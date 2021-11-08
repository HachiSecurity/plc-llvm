
module Hachi.Compiler.CodeGen.Constant.List (
    getHead,
    getTail,
    listCase
) where

-------------------------------------------------------------------------------

import LLVM.AST
import LLVM.AST.Constant
import LLVM.AST.IntegerPredicate as LLVM
import LLVM.IRBuilder

import Hachi.Compiler.CodeGen.Types

-------------------------------------------------------------------------------

-- | `getHead` @listPtr@ generates code which retrieves the head of the list
-- pointed at by @listPtr@.
getHead :: (MonadModuleBuilder m, MonadIRBuilder m) => Operand -> m ClosurePtr
getHead ptr = MkClosurePtr <$> load ptr 0

-- | `getTail` @listPtr@ generates code which retrieves the tail of the list
-- pointed at by @listPtr@.
getTail :: (MonadModuleBuilder m, MonadIRBuilder m) => Operand -> m ClosurePtr
getTail ptr = do
    addr <- gep ptr [ ConstantOperand $ Int 32 0, ConstantOperand $ Int 32 1 ]
    MkClosurePtr <$> load addr 0

-- | `listCase` @listPtr nullBuilder consBuilder@ generates code which tests
-- if @listPtr@ is a pointer to null or to a cons cell and uses @nullBuilder@
-- and @consBuilder@ to generate code for the respective cases.
listCase :: (MonadIRBuilder m) => Operand -> m () -> m () -> m ()
listCase ptr nullBuilder consBuilder = do
    consBr <- freshName "cons"
    nullBr <- freshName "null"

    b <- icmp LLVM.NE ptr (ConstantOperand $ Null listTyPtr)
    condBr b consBr nullBr

    -- list is a cons cell
    emitBlockStart consBr
    consBuilder

    -- list is empty
    emitBlockStart nullBr
    nullBuilder

-------------------------------------------------------------------------------
