-- | This module contains the types, declarations, and references to external
-- functions that we import from stdlib.
module Hachi.Compiler.CodeGen.Externals (
    externalDefinitions,
    rtsInit,

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
    strlen,
    strcpy,
    strcmp,

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

    lessThanEqualsByteString,

    -- * Cryptography
    sha2_256,
    sha3_256,
    blake2b,
    verifySig,

    -- * GMP
    mpzInitSetStr,
    mpzInitSetUInt,
    mpzGetStr,
    mpzGetUInt,
    mpzAdd,
    mpzSub,
    mpzMul,
    mpzFDivQ,
    mpzFDivR,
    mpzTDivQ,
    mpzTDivR,
    mpzCmp
) where

-------------------------------------------------------------------------------

import Control.Monad

import LLVM.AST
import LLVM.AST.AddrSpace
import LLVM.AST.Constant
import LLVM.AST.Linkage
import LLVM.AST.Visibility
import LLVM.AST.CallingConvention
import LLVM.IRBuilder

import Hachi.Compiler.CodeGen.Externals.GMP
import Hachi.Compiler.CodeGen.Externals.Utility
import Hachi.Compiler.CodeGen.Types

-------------------------------------------------------------------------------

rtsInitTy :: Type
rtsInitTy = ptrOf $ FunctionType VoidType [] False

rtsInitFun :: Global
rtsInitFun = globalFromType "rts_init" rtsInitTy

rtsInitRef :: Constant
rtsInitRef = GlobalReference rtsInitTy "rts_init"

rtsInit :: (MonadModuleBuilder m, MonadIRBuilder m) => m ()
rtsInit = void $ call (ConstantOperand rtsInitRef) []

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

-- | `malloc` @type size@ generates code which calls @malloc@ to allocate
-- @size@-many bytes. The return value is cast to @type@, which should
-- normally be a pointer to something.
malloc
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Type -> Operand -> m Operand
malloc ty size = do
    addr <- call (ConstantOperand mallocRef) [(size, [])]
    bitcast addr ty

memcpyTy :: Type
memcpyTy = ptrOf $
    FunctionType (ptrOf i8) [ptrOf i8, ptrOf i8, i64] False

memcpyFun :: Global
memcpyFun = globalFromType "memcpy" memcpyTy

memcpyRef :: Constant
memcpyRef = GlobalReference memcpyTy $ mkName "memcpy"

memcpy
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> Operand -> Operand -> m Operand
memcpy dst src n =
    call (ConstantOperand memcpyRef) [(dst, []), (src, []), (n, [])]

strlenTy :: Type
strlenTy = ptrOf $ FunctionType i64 [ptrOf i8] False

strlenFun :: Global
strlenFun = globalFromType "strlen" strlenTy

strlenRef :: Constant
strlenRef = GlobalReference strlenTy $ mkName "strlen"

strlen :: (MonadModuleBuilder m, MonadIRBuilder m) => Operand -> m Operand
strlen str = call (ConstantOperand strlenRef) [(str, [])]

strcpyTy :: Type
strcpyTy = ptrOf $ FunctionType (ptrOf i8) [ptrOf i8, ptrOf i8] False

strcpyFun :: Global
strcpyFun = globalFromType "strcpy" strcpyTy

strcpyRef :: Constant
strcpyRef = GlobalReference strcpyTy $ mkName "strcpy"

strcpy
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> Operand -> m Operand
strcpy dst src = call (ConstantOperand strcpyRef) [(dst, []), (src, [])]

strcmpTy :: Type
strcmpTy = ptrOf $ FunctionType i32 [ptrOf i8, ptrOf i8] False

strcmpFun :: Global
strcmpFun = globalFromType "strcmp" strcmpTy

strcmpRef :: Constant
strcmpRef = GlobalReference strcmpTy "strcmp"

strcmp
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> Operand -> m Operand
strcmp xs ys = call (ConstantOperand strcmpRef) [(xs, []), (ys, [])]

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

hashTy :: Type
hashTy = ptrOf $ FunctionType (ptrOf i8) [bytestringTyPtr] False

sha2_256Fun :: Global
sha2_256Fun = globalFromType "sha2_256" hashTy

sha2_256Ref :: Constant
sha2_256Ref = GlobalReference hashTy $ mkName "sha2_256"

sha2_256 :: (MonadModuleBuilder m, MonadIRBuilder m) => Operand -> m Operand
sha2_256 ptr = call (ConstantOperand sha2_256Ref) [(ptr, [])]

sha3_256Fun :: Global
sha3_256Fun = globalFromType "sha3_256" hashTy

sha3_256Ref :: Constant
sha3_256Ref = GlobalReference hashTy $ mkName "sha3_256"

sha3_256 :: (MonadModuleBuilder m, MonadIRBuilder m) => Operand -> m Operand
sha3_256 ptr = call (ConstantOperand sha3_256Ref) [(ptr, [])]

blake2bFun :: Global
blake2bFun = globalFromType "blake2b_256" hashTy

blake2bRef :: Constant
blake2bRef = GlobalReference hashTy $ mkName "blake2b_256"

blake2b :: (MonadModuleBuilder m, MonadIRBuilder m) => Operand -> m Operand
blake2b ptr = call (ConstantOperand blake2bRef) [(ptr, [])]

verifySigTy :: Type
verifySigTy = ptrOf $
    FunctionType i8 [bytestringTyPtr,bytestringTyPtr,bytestringTyPtr] False

verifySigFun :: Global
verifySigFun = globalFromType "verify_signature" verifySigTy

verifySigRef :: Constant
verifySigRef = GlobalReference verifySigTy $ mkName "verify_signature"

verifySig
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> Operand -> Operand -> m Operand
verifySig pubKey msg sig =
    call (ConstantOperand verifySigRef) [(pubKey, []), (msg, []), (sig, [])]

-------------------------------------------------------------------------------

externalDefinitions :: [Definition]
externalDefinitions = map GlobalDefinition
    [ rtsInitFun
    , printfFun
    , exitFun
    , mallocFun
    , memcpyFun
    , strlenFun
    , strcpyFun
    , strcmpFun
    , printBytestringFun
    , indexBytestringFun
    , equalsByteStringFun
    , lessThanByteStringFun
    , lessThanEqualsByteStringFun
    , sha2_256Fun
    , sha3_256Fun
    , blake2bFun
    , verifySigFun
    , mpzInitSetStrFun
    , mpzInitSetUIntFun
    , mpzGetStrFun
    , mpzGetUIntFun
    , mpzAddFun
    , mpzSubFun
    , mpzMulFun
    , mpzFDivQFun
    , mpzFDivRFun
    , mpzTDivQFun
    , mpzTDivRFun
    , mpzCmpFun
    ]

-------------------------------------------------------------------------------
