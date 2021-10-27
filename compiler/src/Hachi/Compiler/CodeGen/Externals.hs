-- | This module contains the types, declarations, and references to external
-- functions that we import from stdlib.
module Hachi.Compiler.CodeGen.Externals where

-------------------------------------------------------------------------------

import LLVM.AST
import LLVM.AST.AddrSpace
import LLVM.AST.Constant

import Hachi.Compiler.CodeGen.Types
import LLVM.AST.Linkage
import LLVM.AST.Visibility
import LLVM.AST.CallingConvention

-------------------------------------------------------------------------------

-- | The type of printf.
printfTy :: Type
printfTy = ptrOf $ FunctionType i32 [PointerType i8 $ AddrSpace 0] True

-- | The signature of printf.
printfFun :: Global
printfFun =
    Function External Default Nothing C [] i32 (mkName "printf") pTy []
        Nothing Nothing 0 Nothing Nothing [] Nothing []
    where pTy = ([Parameter (PointerType i8 $ AddrSpace 0) (Name "") []], True)

-- | A `Constant` which provides a reference to the global printf declaration.
printfRef :: Constant
printfRef = GlobalReference printfTy $ mkName "printf"

exitTy :: Type
exitTy = ptrOf $ FunctionType VoidType [i32] False

exitFun :: Global
exitFun =
    Function External Default Nothing C [] VoidType (mkName "exit") pTy []
        Nothing Nothing 0 Nothing Nothing [] Nothing []
    where pTy = ([Parameter i32 (mkName "") []], False)

exitRef :: Constant
exitRef = GlobalReference exitTy $ mkName "exit"

mallocTy :: Type
mallocTy = ptrOf $ FunctionType (PointerType i8 (AddrSpace 0)) [i64] False

mallocFun :: Global
mallocFun =
    Function External Default Nothing C [] (ptrOf i8) (mkName "malloc") pTy []
        Nothing Nothing 0 Nothing Nothing [] Nothing []
    where pTy = ([Parameter i64 (mkName "") []], False)

mallocRef :: Constant
mallocRef = GlobalReference mallocTy $ mkName "malloc"

-------------------------------------------------------------------------------
