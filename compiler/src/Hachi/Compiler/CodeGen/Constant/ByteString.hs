module Hachi.Compiler.CodeGen.Constant.ByteString (
    bsNewStruct,
    bsNew,
    bsLen,
    bsDataPtr
) where

-------------------------------------------------------------------------------

import LLVM.AST.Constant
import LLVM.IRBuilder as IR
import LLVM.AST

import Hachi.Compiler.CodeGen.Externals as E
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
