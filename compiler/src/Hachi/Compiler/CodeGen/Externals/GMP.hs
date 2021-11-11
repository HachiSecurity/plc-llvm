{-# LANGUAGE TemplateHaskell #-}

module Hachi.Compiler.CodeGen.Externals.GMP (
    mpzInitSetStrFun,
    mpzInitSetStr,
    mpzGetStrFun,
    mpzGetStr
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

-------------------------------------------------------------------------------
