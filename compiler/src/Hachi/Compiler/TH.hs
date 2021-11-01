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
mkGlobalStrRefs globals = forM globals $ \(name, val) -> do
    let defName = take (length name - 3) name
    body <- [| ConstantOperand $ asStringPtr $
                GlobalReference (stringPtr val) (LLVM.mkName name) |]
    pure $ ValD (VarP $ TH.mkName $ defName <> "Ref") (NormalB body) []

-------------------------------------------------------------------------------
