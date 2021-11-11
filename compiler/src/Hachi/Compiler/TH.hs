{-# LANGUAGE TemplateHaskell #-}

module Hachi.Compiler.TH (
    mkGlobalStrRefs,
    mkExternal
) where

-------------------------------------------------------------------------------

import Control.Monad

import Language.Haskell.TH as TH

import LLVM.AST as LLVM
import LLVM.AST.Constant

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

mkExternal :: String -> String -> TH.Name -> Q [Dec]
mkExternal name extName tyDec = do
    let tyDecVar = pure $ VarE tyDec
    let globalName = TH.mkName $ name <> "Fun"
    globalTy <- [t| Global |]
    globalBody <- [| globalFromType extName $(tyDecVar) |]

    let refName = TH.mkName $ name <> "Ref"
    refTy <- [t| Constant |]
    refBody <- [| GlobalReference $(tyDecVar) $ LLVM.mkName extName |]

    pure [
        SigD globalName globalTy,
        ValD (VarP globalName) (NormalB globalBody) [],
        SigD refName refTy,
        ValD (VarP refName) (NormalB refBody) []
     ]

-------------------------------------------------------------------------------
