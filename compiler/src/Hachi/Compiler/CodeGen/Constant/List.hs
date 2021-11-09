
module Hachi.Compiler.CodeGen.Constant.List (
    listNew,
    getHead,
    getTail,
    listCase
) where

-------------------------------------------------------------------------------

import LLVM.AST
import LLVM.AST.Constant
import LLVM.AST.IntegerPredicate as LLVM
import LLVM.IRBuilder as IR

import Hachi.Compiler.CodeGen.Externals
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types

-------------------------------------------------------------------------------

-- | `listNew` @head tail@ generates code which allocates enough space for a new
-- list structure. @head@ and @tail@ are then stored in this structure.
listNew
    :: (MonadCodeGen m, MonadIRBuilder m)
    => Operand -> Operand -> m Operand
listNew x xs = do
    -- allocate space for the list structure (two pointers)
    size <- IR.sizeof 64 listTy
    ptr <- malloc listTyPtr size

    -- store the head pointer
    store ptr 0 x

    -- store the tail pointer
    tailAddr <- gep ptr [ ConstantOperand $ Int 32 0
                        , ConstantOperand $ Int 32 1
                        ]
    store tailAddr 0 xs

    -- return the pointer to the list structure
    pure ptr

-- | `getHead` @listPtr@ generates code which retrieves the head of the list
-- pointed at by @listPtr@.
getHead :: (MonadModuleBuilder m, MonadIRBuilder m) => Operand -> m ClosurePtr
getHead ptr = do
    addr <- gep ptr [ ConstantOperand $ Int 32 0, ConstantOperand $ Int 32 0 ]
    MkClosurePtr <$> load addr 0

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
