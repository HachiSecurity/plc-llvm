{-# LANGUAGE RecursiveDo #-}

module Hachi.Compiler.CodeGen.Constant.ByteString (
    bsNewStruct,
    bsNew,
    bsLen,
    bsDataPtr,

    bsPrint,
    bsEquals
) where

-------------------------------------------------------------------------------

import LLVM.IRBuilder as IR
import LLVM.AST
import LLVM.AST.Constant
import qualified LLVM.AST.IntegerPredicate as LLVM

import Hachi.Compiler.CodeGen.Externals as E
import Hachi.Compiler.CodeGen.Globals
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types

-------------------------------------------------------------------------------


-- | `bsNewStruct` @size@ generates code which allocates enough space for a new
-- bytestring structure and stores @size@ in it. The pointer to the new
-- structure is returned. The data pointer of the bytestring is not set
-- and no memory is allocated for the data.
bsNewStruct :: (MonadCodeGen m, MonadIRBuilder m) => Operand -> m Operand
bsNewStruct l = do
    -- calculate the size of the bytestring structure and allocate memory
    -- for it
    size <- IR.sizeof 64 bytestringTy
    ptr <- E.malloc bytestringTyPtr size

    -- store the length
    addr <- gep ptr [ConstantOperand $ Int 32 0, ConstantOperand $ Int 32 0]
    store addr 0 l

    -- return the pointer to the bytestring structure
    pure ptr

-- | `bsNew` @size@ generates code which allocates enough space for a new
-- bytestring with @size@-many elements. The actual memory allocated is
-- greater than @size@, since we need to store the size in the bytestring
-- as well. This function performs the latter task as well before returning
-- the pointer to the new bytestring.
bsNew :: (MonadCodeGen m, MonadIRBuilder m) => Operand -> m Operand
bsNew l = do
    -- allocate the byte array that will store the actual data
    arrPtr <- E.malloc (ptrOf i8) l

    -- create the new bytestring structure
    tptr <- bsNewStruct l

    -- store the pointer to the byte array
    dataAddr <- gep tptr [ ConstantOperand $ Int 32 0
                         , ConstantOperand $ Int 32 1
                         ]
    store dataAddr 0 arrPtr

    -- return the pointer to the bytestring structure
    pure tptr

-- | `bsLen` @ptr@ generates code which loads the length of a bytestring
-- which is represented by @ptr@.
bsLen :: (MonadCodeGen m, MonadIRBuilder m) => Operand -> m Operand
bsLen str = do
    addr <- gep str [ConstantOperand $ Int 32 0, ConstantOperand $ Int 32 0]
    load addr 0

-- | `bsDataPtr` @ptr@ generates code which calculates the address of the
-- data component of a bytestring represented by @ptr@.
bsDataPtr :: (MonadCodeGen m, MonadIRBuilder m) => Operand -> m Operand
bsDataPtr str = do
    addr <- gep str [ConstantOperand $ Int 32 0, ConstantOperand $ Int 32 1]
    load addr 0

-------------------------------------------------------------------------------

-- | `bsPrint` @str@ generates code which prints the bytestring pointed at by
-- @str@ as a hexadecimal string to the standard output.
bsPrint
    :: (MonadCodeGen m, MonadIRBuilder m)
    => Operand -> m ()
bsPrint str = mdo
    -- generate the entry block and jump to it; we mainly just use this as an
    -- origin for the phi node in the loop body, but we also retrieve the
    -- bytestring's length and data pointer here
    entryBr <- freshName "bsPrint.entry"
    br entryBr

    emitBlockStart entryBr

    d <- bsDataPtr str
    l <- bsLen str

    -- jump to the loop start and initialise some labels
    loopStartBr <- freshName "bsPrint.loop.start"
    br loopStartBr

    loopBr <- freshName "bsPrint.loop"
    endBr <- freshName "bsPrint.end"

    -- `for(size_t i=0; i<str->length; i++) {`
    --
    -- if we have just entered the loop, initialise the index to 0;
    -- otherwise take the existing value
    emitBlockStart loopStartBr
    i <- phi [ (ConstantOperand $ Int 64 0, entryBr)
             , (i', loopBr)
             ]

    -- check that the index is less than the length of the bytestring
    lc <- icmp LLVM.ULT i l
    condBr lc loopBr endBr

    -- `printf("%02X", (*str->arr)[i]);`
    --
    -- index into the byte array and pretty-print the byte
    emitBlockStart loopBr

    addr <- gep d [i]
    byte <- load addr 0
    _ <- E.printf hexFormatRef [byte]

    -- increment the index and jump back to the start of the loop
    i' <- add i (ConstantOperand $ Int 64 1)
    br loopStartBr

    -- `}`
    emitBlockStart endBr

-- | `bsEquals` @s0 s1@ determines whether the two bytestrings pointed at by
-- @s0@ and @s1@ are equal or not.
bsEquals
    :: (MonadCodeGen m, MonadIRBuilder m)
    => Operand -> Operand -> m Operand
bsEquals s0 s1 = do
    trueBr <- freshName "bsEquals.true"
    falseBr <- freshName "bsEquals.false"
    loopStartBr <- freshName "bsEquals.loop.start"
    loopBr <- freshName "bsEquals.loop"
    endBr <- freshName "bsEquals.end"

    d0 <- bsDataPtr s0
    d1 <- bsDataPtr s1

    -- allocate a local variable for the loop counter
    i <- alloca i64 Nothing 0
    store i 0 (ConstantOperand $ Int 64 0)

    -- `if(s0->length != s1->length) return false;`
    --
    -- since we already have the lengths of the strings, we can decide that
    -- they are trivially not the same if their lengths do not match
    l0 <- bsLen s0
    l1 <- bsLen s1

    leq <- icmp LLVM.NE l0 l1
    condBr leq falseBr loopStartBr

    -- `for(size_t i=0; i<s0->length; i++) {`
    --
    -- check that our counter is less than the length of one of the strings
    -- (we know that they are the same length at this point, so we can just
    -- arbitrarily pick one of them)
    emitBlockStart loopStartBr
    iv <- load i 0
    lc <- icmp LLVM.ULT iv l0
    condBr lc loopBr trueBr

    -- `if((*s0->arr)[i] != (*s1->arr)[i]) return false;`
    --
    -- the index is less than the length of the strings: retrieve the bytes
    -- at the current index and compare them
    emitBlockStart loopBr
    addr0 <- gep d0 [iv]
    addr1 <- gep d1 [iv]

    c0 <- load addr0 0
    c1 <- load addr1 0

    -- we pre-emptively increment the index to avoid a branch
    ni <- add iv (ConstantOperand $ Int 64 1)
    store i 0 ni

    -- actually compare the two characters: if they are not the same, then
    -- we can fail fast and do not need to check the rest of the string
    cc <- icmp LLVM.NE c0 c1
    condBr cc falseBr loopStartBr

    -- `}`

    -- destination blocks for true and false; we use a phi to actually
    -- pick the value
    emitBlockStart falseBr
    br endBr

    emitBlockStart trueBr
    br endBr

    emitBlockStart endBr
    phi [ (ConstantOperand $ Int 1 0, falseBr)
        , (ConstantOperand $ Int 1 1, trueBr)
        ]

-------------------------------------------------------------------------------
