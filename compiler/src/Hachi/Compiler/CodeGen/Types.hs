{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module contains various LLVM `Type` values used in the code
-- generator.
module Hachi.Compiler.CodeGen.Types (
    -- * Re-exports
    module Hachi.Compiler.Platform,

    -- * Utility functions
    isPtr,

    -- * Basic LLVM types
    i1,
    i8,
    i32,
    i64,
    ptrOf,
    char,
    stringPtr,
    asStringPtr,

    -- * Bytestrings
    bytestringTyDef,
    bytestringTy,
    bytestringTyPtr,

    -- * Pairs
    pairTyDef,
    pairTy,
    pairTyPtr,

    -- * Lists
    listTyDef,
    listTy,
    listTyPtr,
    ListPtr(..),

    -- * Data
    dataTyDef,
    dataTy,
    dataTyPtr,

    -- * GMP
    gmpTyDef,
    emptyGmpTy,
    gmpTy,
    gmpTyPtr,

    -- * Closures
    closureTy,
    closureTyPtr,

    PtrKind(..),
    ClosurePtr(..),
    FromClosurePtr(..),
    toDynamicPtr,

    -- * Continuations
    contTy,
    contTyPtr,
    Continuation(..)
) where

-------------------------------------------------------------------------------

import Data.List ( genericLength )

import LLVM.AST
import LLVM.AST.AddrSpace
import LLVM.AST.Constant

import Hachi.Compiler.Platform

-------------------------------------------------------------------------------

-- | `isPtr` @type@ determines whether @type@ represents a pointer type.
isPtr :: Type -> Bool
isPtr PointerType{} = True
isPtr _ = False

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
    , ptrOf i8
    ]

-- | `bytestringTy` is a `Type` for bytestrings.
bytestringTy :: Type
bytestringTy = NamedTypeReference "bytestring"

-- | `bytestringTyPtr` is a `Type` representing a pointer to a bytestring.
bytestringTyPtr :: Type
bytestringTyPtr = ptrOf bytestringTy

-------------------------------------------------------------------------------

-- | `pairTyDef` is the type definition for pairs.
pairTyDef :: Type
pairTyDef = StructureType False
    [ closureTyPtr
    , closureTyPtr
    ]

-- | `pairTy` is a `Type` for pairs.
pairTy :: Type
pairTy = NamedTypeReference "pair"

-- | `pairTyPtr` is a `Type` representing a pointer to a pair.
pairTyPtr :: Type
pairTyPtr = ptrOf pairTy

-------------------------------------------------------------------------------

-- | `listTyDef` is the type definition for lists.
listTyDef :: Type
listTyDef = StructureType False
    [ closureTyPtr
    , closureTyPtr
    ]

-- | `listTy` is a `Type` for lists.
listTy :: Type
listTy = NamedTypeReference "list"

-- | `listTyPtr` is a `Type` representing a pointer to a list.
listTyPtr :: Type
listTyPtr = ptrOf listTy

-- | Represents a pointer to a linked list.
newtype ListPtr = MkListPtr { listPtr :: Operand }
    deriving (Eq, Show)

-------------------------------------------------------------------------------

-- | The Plutus `Data` type is a sum type with five different constructors.
-- Our representation is as a tagged array of pointers:
-- For `Constr`, we have a constructor tag and a pointer to a @list@ structure
-- For `Map`, we have a pointer to a @list@ structure
-- For `List`, we have a pointer to a @list@ structure
-- For `I`, we have an integer value
-- For `B`, we have a pointer to a @bytestring@ structure
dataTyDef :: Type
dataTyDef = StructureType False
    [ i8
    , ArrayType 0 closureTyPtr
    ]

-- | `dataTy` is a `Type` for data values.
dataTy :: Type
dataTy = NamedTypeReference "data"

-- | `dataTyPtr` is a `Type` representing a pointer to a data value.
dataTyPtr :: Type
dataTyPtr = ptrOf dataTy

-------------------------------------------------------------------------------

-- | `gmpTyDef` attempts to capture the structure of @__mpz_struct@ from the
-- gmp library.
gmpTyDef :: Type
gmpTyDef = StructureType False [ iHost, iHost, ptrOf gmpLimb ]

emptyGmpTy :: Constant
emptyGmpTy = Array gmpTyDef [
    Struct Nothing False [
        Int platformIntSize 0,
        Int platformIntSize 0,
        Null (ptrOf gmpLimb)
    ]
 ]

-- | `gmpTy` is equivalent to @mpz_t@ from the gmp library.
gmpTy :: Type
gmpTy = NamedTypeReference "mpz_t"

-- | `gmpTyPtr` is a pointer to `gmpTy`.
gmpTyPtr :: Type
gmpTyPtr = ptrOf gmpTy

-------------------------------------------------------------------------------

-- | `closureTy` is a `Type` for closures.
closureTy :: Type
closureTy = NamedTypeReference "closure"

-- | `closureTyPtr` is a `Type` representing a pointer to a closure.
closureTyPtr :: Type
closureTyPtr = ptrOf closureTy

-- | Enumerates different kinds of pointers we may have:
--
-- - `StaticPtr` is guaranteed to represent a pointer to a static global
-- - `DynamicPtr` _may_ represent a pointer to a dynamically allocated value.
--
-- In other words, we use `StaticPtr` when we know what sort of pointer we
-- have and `DynamicPtr` in other cases.
data PtrKind = StaticPtr | DynamicPtr

-- | Represents a pointer to a closure.
data ClosurePtr (k :: PtrKind) where
    MkClosurePtr :: Operand -> ClosurePtr 'DynamicPtr
    MkStaticClosurePtr :: Constant -> ClosurePtr 'StaticPtr

deriving instance Eq (ClosurePtr k)
deriving instance Show (ClosurePtr k)

class FromClosurePtr k to where
    -- | `closurePtr` @closurePtr@ returns @closurePtr@'s underlying
    -- LLVM representation.
    closurePtr :: ClosurePtr k -> to

instance FromClosurePtr 'StaticPtr Constant where
    closurePtr (MkStaticClosurePtr c) = c

instance FromClosurePtr k Operand where
    closurePtr (MkClosurePtr ptr) = ptr
    closurePtr (MkStaticClosurePtr c) = ConstantOperand c

-- | `toDynamicPtr` @closurePtr@ turns a closure pointer into a dynamic
-- closure pointer. This is conceptually a no-op and just allows us to forget
-- information in case we only need a `DynamicPtr`, but have a `StaticPtr`
-- or don't know which one we have.
-- Static pointers are always acceptable where dynamic pointers are expected.
toDynamicPtr :: ClosurePtr k -> ClosurePtr 'DynamicPtr
toDynamicPtr (MkStaticClosurePtr ptr) = MkClosurePtr (ConstantOperand ptr)
toDynamicPtr (MkClosurePtr ptr) = MkClosurePtr ptr

-------------------------------------------------------------------------------

-- | `contTy` is a `Type` for continuations.
contTy :: Type
contTy = NamedTypeReference "continuation"

-- | `contTyPtr` is a `Type` representing a pointer to a continuation.
contTyPtr :: Type
contTyPtr = ptrOf contTy

-- | Represents pointers to continuations.
newtype Continuation = MkCont { contPtr :: Operand }
    deriving (Eq, Show)

-------------------------------------------------------------------------------

