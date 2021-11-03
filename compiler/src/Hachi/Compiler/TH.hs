{-# LANGUAGE TemplateHaskell #-}

module Hachi.Compiler.TH (
    mkGlobalStrRefs
) where

-------------------------------------------------------------------------------

import Control.Monad

import Language.Haskell.TH as TH

import LLVM.AST as LLVM

-------------------------------------------------------------------------------

-- | `mkGlobalStrRefs` @globals@ generates LLVM `Constant`s which refer to
-- global string variables specified by @globals@.
mkGlobalStrRefs :: [(String, String)] -> Q [Dec]
mkGlobalStrRefs globals = fmap concat $ forM globals $ \(name, val) -> do
    let defName = TH.mkName $ take (length name - 3) name <> "Ref"
    ty <- [t| Operand |]
    body <- [| ConstantOperand $ asStringPtr $
                GlobalReference (stringPtr val) (LLVM.mkName name) |]
    pure [
        SigD defName ty,
        ValD (VarP defName) (NormalB body) []
     ]

-------------------------------------------------------------------------------
