{-# LANGUAGE RecursiveDo #-}

module Hachi.Compiler.CodeGen.Constant.Text (
    strlen
) where

-------------------------------------------------------------------------------

import LLVM.IRBuilder as IR
import LLVM.AST
import LLVM.AST.Constant
import qualified LLVM.AST.IntegerPredicate as LLVM

import Hachi.Compiler.CodeGen.Monad

-------------------------------------------------------------------------------

-- | `strlen` @str@ generates code which implements the same behaviour as
-- the @strlen@ function. I.e. it computes the length of @str@.
strlen :: (MonadCodeGen m, MonadIRBuilder m) => Operand -> m Operand
strlen ptr = mdo
    -- get the name of the current block and generate fresh names for two
    -- new blocks
    startBr <- currentBlock
    loopBr <- freshName "strlen.loop"
    endBr <- freshName "strlen.end"

    -- we are essentially implementing a do-while loop, so we immediately
    -- jump to the start of the loop
    br loopBr

    -- if we are entering the loop from the previous block, we initialise our
    -- index/result variable to 0, otherwise we take the incremented index
    -- from the previous iteration
    emitBlockStart loopBr

    ix <- phi [ (ConstantOperand $ Int 64 0, startBr)
              , (ix', loopBr)
              ]

    -- retrieve the byte from the current index in the string
    addr <- gep ptr [ix]
    c <- load addr 0

    -- increment the index for the next iteration, if there is one
    ix' <- add ix (ConstantOperand $ Int 64 1)

    -- check whether the byte is \0: if so, we exit the loop; otherwise,
    -- we perform another iteration
    r <- icmp LLVM.EQ c (ConstantOperand $ Int 8 0)
    condBr r endBr loopBr

    -- return the index we were at when we last retrieved a byte, since this
    -- is equivalent to the length of the string
    emitBlockStart endBr
    pure ix


-------------------------------------------------------------------------------
