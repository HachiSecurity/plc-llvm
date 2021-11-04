
-- | This module contains various LLVM `Type` values used in the code
-- generator.
module Hachi.Compiler.CodeGen.Types (
    -- * Basic LLVM types
    i1,
    i8,
    i32,
    i64,
    ptrOf,
    funPtr,
    char,
    stringPtr,
    asStringPtr,

    -- * Bytestrings
    bytestringTyDef,
    bytestringTy,
    bytestringTyPtr,

    ClosurePtr(..)
) where

-------------------------------------------------------------------------------

import Data.List ( genericLength )

import LLVM.AST
import LLVM.AST.AddrSpace
import LLVM.AST.Constant

-------------------------------------------------------------------------------

i1 :: Type
i1 = IntegerType 1

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

char :: Type
char = i8

stringPtr :: String -> Type
stringPtr val = ptrOf $ ArrayType (genericLength val + 1) char

-- | `asStringPtr` @constant@ casts @constant@ to a char pointer.
asStringPtr :: Constant -> Constant
asStringPtr ref = LLVM.AST.Constant.BitCast ref $ ptrOf char

-------------------------------------------------------------------------------

-- | `bytestringTyDef` is the type definition for bytestrings.
bytestringTyDef :: Type
bytestringTyDef = StructureType False
    [ i64
    , ptrOf (ArrayType 0 i8)
    ]

-- | `bytestringTy` is a `Type` for bytestrings.
bytestringTy :: Type
bytestringTy = NamedTypeReference "bytestring"

-- | `bytestringTyPtr` is a `Type` representing a pointer to a bytestring.
bytestringTyPtr :: Type
bytestringTyPtr = ptrOf bytestringTy

-------------------------------------------------------------------------------

-- | Represents a pointer to a closure.
newtype ClosurePtr = MkClosurePtr { closurePtr :: Operand }
    deriving (Eq, Show)

-------------------------------------------------------------------------------
