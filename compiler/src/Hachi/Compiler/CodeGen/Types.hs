
-- | This module contains various LLVM `Type` values used in the code
-- generator.
module Hachi.Compiler.CodeGen.Types (
    -- * Basic LLVM types
    i8,
    i32,
    i64,
    ptrOf,

    -- * Helpers
    mkEntryTy,
    clsEntryTy,
    varEntryTy,
    funPtr,

    -- * Closures
    ClosurePtr(..),
    closureTyDef,
    closureSize,
    closureTy,
    closureTyPtr,

    ClosureComponent(..),
    loadFromClosure
) where

-------------------------------------------------------------------------------

import LLVM.AST
import LLVM.AST.AddrSpace
import LLVM.AST.Constant
import LLVM.IRBuilder

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

-------------------------------------------------------------------------------

-- | Represents a pointer to a closure.
newtype ClosurePtr = MkClosurePtr { closurePtr :: Operand }
    deriving (Eq, Show)

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
    , funPtr
    , ArrayType 0 closureTyPtr
    ]

-- | The minimum size of a closure in words.
closureSize :: Num a => a
closureSize = 2

-- | `closureTy` is a `Type` for closures.
closureTy :: Type
closureTy = NamedTypeReference "closure"

-- | `closureTyPtr` is a `Type` representing a pointer to a closure.
closureTyPtr :: Type
closureTyPtr = ptrOf closureTy

-- | Enumerates different components of a closure.
data ClosureComponent
    = ClosureCode
    | ClosurePrint
    | ClosureFreeVar Integer
    deriving (Eq, Show)

-- | `indexForComponent` @component@ determines the index of @component@
-- within a closure.
indexForComponent :: ClosureComponent -> Integer
indexForComponent ClosureCode = 0
indexForComponent ClosurePrint = 1
indexForComponent (ClosureFreeVar n) = closureSize + n

-- | `loadFromClosure` @component ptr@ assumes that @ptr@ is a pointer to
-- a closure
loadFromClosure
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => ClosureComponent -> ClosurePtr -> m Operand
loadFromClosure prop (MkClosurePtr ptr) = do
    let ix = indexForComponent prop
    addr <- gep ptr [ConstantOperand $ Int 32 0, ConstantOperand $ Int 32 ix]
    load addr 0

-------------------------------------------------------------------------------
