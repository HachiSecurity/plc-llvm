{-# LANGUAGE RecordWildCards #-}

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
    memcmp,

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
import Control.Monad.Reader

import LLVM.AST
import LLVM.AST.AddrSpace
import LLVM.AST.Constant
import LLVM.AST.IntegerPredicate as LLVM
import LLVM.AST.Linkage
import LLVM.AST.Visibility
import LLVM.AST.CallingConvention
import LLVM.IRBuilder

import Hachi.Compiler.CodeGen.Externals.GMP
import Hachi.Compiler.CodeGen.Externals.Utility
import Hachi.Compiler.CodeGen.Globals
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types
import Hachi.Compiler.Config

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
    :: (MonadModuleBuilder m, MonadIRBuilder m, MonadCodeGen m)
    => Type -> Operand -> m Operand
malloc ty size = do
    addr <- call (ConstantOperand mallocRef) [(size, [])]

    MkConfig{..} <- asks codeGenCfg

    if not cfgCheckOOM
    then bitcast addr ty
    else do
        oomBranch <- freshName "oom"
        successBranch <- freshName "allocated"

        -- check whether we received a null pointer from malloc
        r <- icmp LLVM.EQ addr $ ConstantOperand $ Null $ ptrOf i8
        condBr r oomBranch successBranch

        -- if yes: print an error and exit
        emitBlockStart oomBranch
        _ <- printf oomErrRef []
        _ <- exit (-1)
        unreachable

        -- otherwise, cast the pointer to the desired type and continue
        emitBlockStart successBranch
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

memcmpTy :: Type
memcmpTy = ptrOf $
    FunctionType i32 [ptrOf i8, ptrOf i8, i64] False

memcmpFun :: Global
memcmpFun = globalFromType "memcmp" memcmpTy

memcmpRef :: Constant
memcmpRef = GlobalReference memcmpTy $ mkName "memcmp"

memcmp
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> Operand -> Operand -> m Operand
memcmp p0 p1 len =
    call (ConstantOperand memcmpRef) [(p0, []), (p1, []), (len, [])]

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
    , memcmpFun
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
