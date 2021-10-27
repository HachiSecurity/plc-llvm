
-- | This module contains various LLVM `Type` values used in the code
-- generator.
module Hachi.Compiler.CodeGen.Types where

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

-- | `mkEntryTy` @arity@ generates a function signature for a closure
-- with @arity@-many parameters in addition to the parameter that
-- references the closure itself. In other words, the resulting
-- signature has 1+@arity@ many parameters.
mkEntryTy :: Int -> Type
mkEntryTy arity = ptrOf $ FunctionType closureTyPtr params False
    where params = closureTyPtr : replicate arity closureTyPtr

-- | `clsEntryTy` is the `Type` of closure entry functions.
clsEntryTy :: Type
clsEntryTy = mkEntryTy 0

varEntryTy :: Type
varEntryTy = mkEntryTy 1

funPtr :: Type
funPtr = ptrOf $ FunctionType VoidType [] False

-- | `closureTyDef` is a `Type` definition for closures. Note the layout is
-- as follows:
--
-- - A pointer to the code for the closure.
-- - A pointer to the print code for the closure.
-- - Zero or more free variables. (LLVM will accept more elements than the
-- array's indicated size) https://llvm.org/docs/LangRef.html#array-type
closureTyDef :: Type
closureTyDef = StructureType False
    [ clsEntryTy
    , clsEntryTy
    , funPtr
    , ArrayType 0 funPtr
    ]

-- | `closureTy` is a `Type` for closures.
closureTy :: Type
closureTy = NamedTypeReference "closure"

-- | `closureTyPtr` is a `Type` representing a pointer to a closure.
closureTyPtr :: Type
closureTyPtr = ptrOf closureTy

-------------------------------------------------------------------------------
