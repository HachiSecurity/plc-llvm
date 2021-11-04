-- | This module contains the types, declarations, and references to external
-- functions that we import from stdlib.
module Hachi.Compiler.CodeGen.Externals (
    externalDefinitions,

    -- * Standard C library
    printfTy,
    printfRef,
    printf,
    exitTy,
    exitRef,
    exit,
    mallocTy,
    mallocRef,
    malloc,
    memcpyTy,
    memcpyRef,
    memcpy,

    -- * Bytestrings
    printBytestringTy,
    printBytestringRef,
    printBytestring,

    indexBytestring,

    equalsByteStringTy,
    equalsByteStringRef,
    equalsByteString,

    lessThanByteStringTy,
    lessThanByteStringRef,
    lessThanByteString,

    lessThanEqualsByteString
) where

-------------------------------------------------------------------------------

import LLVM.AST
import LLVM.AST.AddrSpace
import LLVM.AST.Constant
import LLVM.AST.Linkage
import LLVM.AST.Visibility
import LLVM.AST.CallingConvention
import LLVM.IRBuilder

import Hachi.Compiler.CodeGen.Types

-------------------------------------------------------------------------------

globalFromType :: String -> Type -> Global
globalFromType name (PointerType (FunctionType rt pts varArgs) _) =
    Function External Default Nothing C [] rt (mkName name) pTy []
        Nothing Nothing 0 Nothing Nothing [] Nothing []
    where pTy = ([Parameter ty (mkName "") [] | ty <- pts], varArgs)
globalFromType _ _ =
    error "globalFromType must be applied to a function pointer"

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

printf :: (MonadModuleBuilder m, MonadIRBuilder m) => Operand -> [Operand] -> m Operand
printf msg xs = call (ConstantOperand printfRef) $ (msg, []) : [(x, []) | x <- xs]

exitTy :: Type
exitTy = ptrOf $ FunctionType VoidType [i32] False

exitFun :: Global
exitFun =
    Function External Default Nothing C [] VoidType (mkName "exit") pTy []
        Nothing Nothing 0 Nothing Nothing [] Nothing []
    where pTy = ([Parameter i32 (mkName "") []], False)

exitRef :: Constant
exitRef = GlobalReference exitTy $ mkName "exit"

exit :: (MonadModuleBuilder m, MonadIRBuilder m) => Integer -> m Operand
exit code = call (ConstantOperand exitRef) [(ConstantOperand $ Int 32 code, [])]

mallocTy :: Type
mallocTy = ptrOf $ FunctionType (PointerType i8 (AddrSpace 0)) [i64] False

mallocFun :: Global
mallocFun =
    Function External Default Nothing C [] (ptrOf i8) (mkName "malloc") pTy []
        Nothing Nothing 0 Nothing Nothing [] Nothing []
    where pTy = ([Parameter i64 (mkName "") []], False)

mallocRef :: Constant
mallocRef = GlobalReference mallocTy $ mkName "malloc"

malloc :: (MonadModuleBuilder m, MonadIRBuilder m) => Operand -> m Operand
malloc size = call (ConstantOperand mallocRef) [(size, [])]

memcpyTy :: Type
memcpyTy = ptrOf $
    FunctionType (ptrOf VoidType) [ptrOf VoidType, ptrOf VoidType, i64] False

memcpyFun :: Global
memcpyFun = globalFromType "memcpy" memcpyTy

memcpyRef :: Constant
memcpyRef = GlobalReference memcpyTy $ mkName "memcpy"

memcpy
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> Operand -> Operand -> m Operand
memcpy dst src n =
    call (ConstantOperand memcpyRef) [(dst, []), (src, []), (n, [])]

-------------------------------------------------------------------------------

printBytestringTy :: Type
printBytestringTy = ptrOf $ FunctionType VoidType [bytestringTyPtr] False

printBytestringFun :: Global
printBytestringFun = globalFromType "print_bytestring" printBytestringTy

printBytestringRef :: Constant
printBytestringRef = GlobalReference printBytestringTy $ mkName "print_bytestring"

printBytestring :: (MonadModuleBuilder m, MonadIRBuilder m) => Operand -> m Operand
printBytestring ptr = call (ConstantOperand printBytestringRef) [(ptr, [])]

indexBytestringTy :: Type
indexBytestringTy = ptrOf $ FunctionType i8 [bytestringTyPtr, i64] False

indexBytestringFun :: Global
indexBytestringFun = globalFromType "index_bytestring" indexBytestringTy

indexBytestringRef :: Constant
indexBytestringRef =
    GlobalReference indexBytestringTy $ mkName "index_bytestring"

indexBytestring
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> Operand -> m Operand
indexBytestring ptr n =
    call (ConstantOperand indexBytestringRef) [(ptr, []), (n, [])]

equalsByteStringTy :: Type
equalsByteStringTy = ptrOf $
    FunctionType i8 [bytestringTyPtr, bytestringTyPtr] False

equalsByteStringFun :: Global
equalsByteStringFun = globalFromType "equals_bytestring" equalsByteStringTy

equalsByteStringRef :: Constant
equalsByteStringRef =
    GlobalReference equalsByteStringTy $ mkName "equals_bytestring"

equalsByteString
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> Operand -> m Operand
equalsByteString p0 p1 =
    call (ConstantOperand equalsByteStringRef) [(p0, []), (p1, [])]

lessThanByteStringTy :: Type
lessThanByteStringTy = ptrOf $
    FunctionType i8 [bytestringTyPtr, bytestringTyPtr] False

lessThanByteStringFun :: Global
lessThanByteStringFun =
    globalFromType "less_than_bytestring" lessThanByteStringTy

lessThanByteStringRef :: Constant
lessThanByteStringRef =
    GlobalReference lessThanByteStringTy $ mkName "less_than_bytestring"

lessThanByteString
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> Operand -> m Operand
lessThanByteString p0 p1 =
    call (ConstantOperand lessThanByteStringRef) [(p0, []), (p1, [])]

lessThanEqualsByteStringFun :: Global
lessThanEqualsByteStringFun =
    globalFromType "less_than_equals_bytestring" lessThanByteStringTy

lessThanEqualsByteStringRef :: Constant
lessThanEqualsByteStringRef =
    GlobalReference lessThanByteStringTy $ mkName "less_than_equals_bytestring"

lessThanEqualsByteString
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> Operand -> m Operand
lessThanEqualsByteString p0 p1 =
    call (ConstantOperand lessThanEqualsByteStringRef) [(p0, []), (p1, [])]

-------------------------------------------------------------------------------

externalDefinitions :: [Definition]
externalDefinitions = map GlobalDefinition
    [ printfFun
    , exitFun
    , mallocFun
    , memcpyFun
    , printBytestringFun
    , indexBytestringFun
    , equalsByteStringFun
    , lessThanByteStringFun
    , lessThanEqualsByteStringFun
    ]

-------------------------------------------------------------------------------
