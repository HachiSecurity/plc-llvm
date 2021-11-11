
-- | This module exports platform-specific information.
module Hachi.Compiler.Platform (
    -- * Platform-specific sizes
    platformPtrSize,
    platformIntSize,
    platformGmpLimbSize,
    platformGmpSize,

    -- * Platform-specific LLVM types
    iHost,
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

platformGmpLimbSize :: Num a => a
platformGmpLimbSize = (#size mp_limb_t) * 8

platformGmpSize :: Num a => a
platformGmpSize = #size mpz_t

-------------------------------------------------------------------------------

iHost :: Type
iHost = IntegerType platformIntSize

gmpLimb :: Type
gmpLimb = IntegerType platformGmpLimbSize

-------------------------------------------------------------------------------
