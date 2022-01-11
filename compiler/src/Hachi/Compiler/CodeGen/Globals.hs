{-# LANGUAGE TemplateHaskell #-}


module Hachi.Compiler.CodeGen.Globals where

-------------------------------------------------------------------------------

import Control.Monad

import LLVM.AST
import LLVM.AST.Constant
import LLVM.AST.Linkage

import Hachi.Compiler.CodeGen.Constant.String
import Hachi.Compiler.CodeGen.IRBuilder
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types
import Hachi.Compiler.TH

-------------------------------------------------------------------------------

-- | `generateConstantGlobals` is a computation which emits global definitions
-- related to constants.
generateConstantGlobals :: (MonadCodeGen m, MonadFail m) => m ()
generateConstantGlobals = void $ do
    void $ global "returnRegister" (ptrOf i8) (Null (ptrOf i8))
         $ setLinkage LinkOnce

    runIRBuilderT emptyIRBuilder $ forM_ globalStrs $ \(nm, val) ->
        globalStringPtr val (mkName nm) $ setLinkage LinkOnce

$(mkGlobalStrRefs globalStrs)

returnRef :: Operand
returnRef = ConstantOperand
          $ GlobalReference (ptrOf $ ptrOf i8) "returnRegister"

-------------------------------------------------------------------------------
