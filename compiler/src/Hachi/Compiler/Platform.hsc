
-- | This module exports platform-specific information.
module Hachi.Compiler.Platform (
    -- * Platform-specific sizes
    platformPtrSize,
    platformIntSize,
    platformUnsignedLongIntSize,
    platformGmpLimbSize,
    platformGmpSize,

    -- * Platform-specific LLVM types
    iHost,
    iUnsignedLongInt,
    gmpLimb
) where

-------------------------------------------------------------------------------

#include <gmp.h>

import LLVM.AST

-------------------------------------------------------------------------------

platformPtrSize :: Num a => a
platformPtrSize = #size void*

platformIntSize :: Num a => a
platformIntSize = (#size int) * 8

platformUnsignedLongIntSize :: Num a => a
platformUnsignedLongIntSize = (#size unsigned long int) * 8

platformGmpLimbSize :: Num a => a
platformGmpLimbSize = (#size mp_limb_t) * 8

platformGmpSize :: Num a => a
platformGmpSize = #size mpz_t

-------------------------------------------------------------------------------

iHost :: Type
iHost = IntegerType platformIntSize

iUnsignedLongInt :: Type
iUnsignedLongInt = IntegerType platformUnsignedLongIntSize

gmpLimb :: Type
gmpLimb = IntegerType platformGmpLimbSize

-------------------------------------------------------------------------------
