{-# LANGUAGE TemplateHaskell #-}

module Hachi.Compiler.CodeGen.Externals.GMP (
    mpzInitSetStrFun,
    mpzInitSetStr,
    mpzGetStrFun,
    mpzGetStr,
    mpzAddFun,
    mpzAdd,
    mpzSubFun,
    mpzSub,
    mpzMulFun,
    mpzMul,
    mpzCmpFun,
    mpzCmp,
    mpzFDivQFun,
    mpzFDivQ,
    mpzFDivRFun,
    mpzFDivR,
    mpzTDivQFun,
    mpzTDivQ,
    mpzTDivRFun,
    mpzTDivR
) where

-------------------------------------------------------------------------------

import Control.Monad

import LLVM.AST
import LLVM.AST.Constant
import LLVM.IRBuilder

import Hachi.Compiler.CodeGen.Externals.Utility
import Hachi.Compiler.CodeGen.Types
import Hachi.Compiler.TH
import Hachi.Compiler.Platform

-------------------------------------------------------------------------------

mpzInitSetStrTy :: Type
mpzInitSetStrTy = ptrOf $ FunctionType VoidType [gmpTy, ptrOf i8, iHost] False

$(mkExternal "mpzInitSetStr" "__gmpz_init_set_str" 'mpzInitSetStrTy)

mpzInitSetStr
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Name -> Constant -> String -> m ()
mpzInitSetStr name ptr str = do
    addr <- globalStringPtr str name
    void $ call (ConstantOperand mpzInitSetStrRef)
        [ (ConstantOperand ptr, [])
        , (ConstantOperand addr, [])
        , (ConstantOperand $ Int platformIntSize 10, [])
        ]

mpzGetStrTy :: Type
mpzGetStrTy = ptrOf $ FunctionType (ptrOf i8) [ptrOf i8, iHost, gmpTy] False

$(mkExternal "mpzGetStr" "__gmpz_get_str" 'mpzGetStrTy)

mpzGetStr
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> Operand -> Operand -> m Operand
mpzGetStr strPtr base ptr = call (ConstantOperand mpzGetStrRef)
    [(strPtr, []), (base, []), (ptr, [])]

mpzBinTy :: Type
mpzBinTy = ptrOf $ FunctionType VoidType [gmpTy, gmpTy, gmpTy] False

mpzBin
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Constant -> Operand -> Operand -> Operand -> m ()
mpzBin fun rPtr xPtr yPtr = void $ call (ConstantOperand fun)
    [(rPtr, []), (xPtr, []), (yPtr, [])]

$(mkExternal "mpzAdd" "__gmpz_add" 'mpzBinTy)

mpzAdd
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> Operand -> Operand -> m ()
mpzAdd = mpzBin mpzAddRef

$(mkExternal "mpzSub" "__gmpz_sub" 'mpzBinTy)

mpzSub
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> Operand -> Operand -> m ()
mpzSub = mpzBin mpzSubRef

$(mkExternal "mpzMul" "__gmpz_mul" 'mpzBinTy)

mpzMul
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> Operand -> Operand -> m ()
mpzMul = mpzBin mpzMulRef

$(mkExternal "mpzFDivQ" "__gmpz_fdiv_q" 'mpzBinTy)

mpzFDivQ
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> Operand -> Operand -> m ()
mpzFDivQ = mpzBin mpzFDivQRef

$(mkExternal "mpzFDivR" "__gmpz_fdiv_r" 'mpzBinTy)

mpzFDivR
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> Operand -> Operand -> m ()
mpzFDivR = mpzBin mpzFDivRRef

$(mkExternal "mpzTDivQ" "__gmpz_tdiv_q" 'mpzBinTy)

mpzTDivQ
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> Operand -> Operand -> m ()
mpzTDivQ = mpzBin mpzTDivQRef

$(mkExternal "mpzTDivR" "__gmpz_tdiv_r" 'mpzBinTy)

mpzTDivR
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> Operand -> Operand -> m ()
mpzTDivR = mpzBin mpzTDivRRef

mpzCmpTy :: Type
mpzCmpTy = ptrOf $ FunctionType iHost [gmpTy, gmpTy] False

$(mkExternal "mpzCmp" "__gmpz_cmp" 'mpzCmpTy)

mpzCmp
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> Operand -> m Operand
mpzCmp x y = call (ConstantOperand mpzCmpRef) [(x, []), (y, [])]

-------------------------------------------------------------------------------
