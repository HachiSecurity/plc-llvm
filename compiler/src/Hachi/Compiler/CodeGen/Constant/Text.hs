{-# LANGUAGE RecursiveDo #-}

module Hachi.Compiler.CodeGen.Constant.Text (
    strlen,
    strcpy,
    streq
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

-- | `strcpy` @dest src@ copies the string from @src@ to @dest.
strcpy :: (MonadCodeGen m, MonadIRBuilder m) => Operand -> Operand -> m Operand
strcpy dst src = mdo
    -- get the name of the current block and generate fresh names for two
    -- new blocks
    startBr <- currentBlock
    loopBr <- freshName "strcpy.loop"
    endBr <- freshName "strcpy.end"

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

    -- compute the addresses for the source and destination arrays
    srcAddr <- gep src [ix]
    destAddr <- gep dst [ix]

    -- load the byte from the source array and write it to the destination
    byte <- load srcAddr 0
    store destAddr 0 byte

    -- increment the index for the next iteration, if there is one
    ix' <- add ix (ConstantOperand $ Int 64 1)

    -- check whether the byte is \0: if so, we exit the loop; otherwise,
    -- we perform another iteration
    r <- icmp LLVM.EQ byte (ConstantOperand $ Int 8 0)
    condBr r endBr loopBr

    -- return the destination address, which is something the C function does
    -- for some reason
    emitBlockStart endBr
    pure dst

-- | `streq` @s0 s1@ computes if @s0@ and @s1@ are equal.
streq :: (MonadCodeGen m, MonadIRBuilder m) => Operand -> Operand -> m Operand
streq s0 s1 = mdo
    -- get the name of the current block and generate fresh names for two
    -- new blocks
    startBr <- currentBlock
    loopBr <- freshName "streq.loop"
    checkBr <- freshName "streq.loop.check"
    endBr <- freshName "streq.end"

    -- we are essentially implementing a do-while loop, so we immediately
    -- jump to the start of the loop
    br loopBr

    -- if we are entering the loop from the previous block, we initialise our
    -- index/result variable to 0, otherwise we take the incremented index
    -- from the previous iteration
    emitBlockStart loopBr

    ix <- phi [ (ConstantOperand $ Int 64 0, startBr)
              , (ix', checkBr)
              ]

    -- compute the addresses for both arrays
    addr0 <- gep s0 [ix]
    addr1 <- gep s1 [ix]

    -- load the bytes from both arrays
    byte0 <- load addr0 0
    byte1 <- load addr1 0

    -- increment the index for the next iteration, if there is one
    ix' <- add ix (ConstantOperand $ Int 64 1)

    -- check if the two bytes are the same: if they are not, we fail fast,
    -- otherwise we need to check if this is the end of the strings
    eq <- icmp LLVM.EQ byte0 byte1
    condBr eq checkBr endBr

    -- check if this is the end of the strings (we only need to check one,
    -- since we know that the bytes are the same at this point)
    emitBlockStart checkBr
    cr <- icmp LLVM.EQ byte0 (ConstantOperand $ Int 8 0)
    condBr cr endBr loopBr

    -- we can infer what the result is depending on where we came from: if we
    -- came here from the loopBr block, the bytes aren't equal and therefore
    -- the strings are not the same; otherwise, if we came from the checkBr
    -- block, then we reached the end of the strings and they are the same
    emitBlockStart endBr
    phi [ (ConstantOperand $ Int 1 0, loopBr)
        , (ConstantOperand $ Int 1 1, checkBr)
        ]

-------------------------------------------------------------------------------
