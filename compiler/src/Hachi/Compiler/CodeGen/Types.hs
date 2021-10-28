
-- | This module contains various LLVM `Type` values used in the code
-- generator.
module Hachi.Compiler.CodeGen.Types (
    -- * Basic LLVM types
    i8,
    i32,
    i64,
    ptrOf,
    funPtr
) where

-------------------------------------------------------------------------------

import LLVM.AST
import LLVM.AST.AddrSpace

-------------------------------------------------------------------------------

i8 :: Type
i8 = IntegerType 8

i32 :: Type
i32 = IntegerType 32

i64 :: Type
i64 = IntegerType 64

ptrOf :: Type -> Type
ptrOf ty = PointerType ty $ AddrSpace 0

funPtr :: Type
funPtr = ptrOf $ FunctionType VoidType [] False

-------------------------------------------------------------------------------
