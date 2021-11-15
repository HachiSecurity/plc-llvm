{-# LANGUAGE TemplateHaskell #-}


module Hachi.Compiler.CodeGen.Globals where

-------------------------------------------------------------------------------

import Control.Monad

import LLVM.AST
import LLVM.AST.Constant
import LLVM.IRBuilder

import Hachi.Compiler.TH
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types
import Hachi.Compiler.CodeGen.Constant.String

-------------------------------------------------------------------------------

-- | `generateConstantGlobals` is a computation which emits global definitions
-- related to constants.
generateConstantGlobals :: MonadCodeGen m => m ()
generateConstantGlobals = void $ do
    void $ global "returnRegister" (ptrOf i8) $ Null (ptrOf i8)

    runIRBuilderT emptyIRBuilder $ forM_ globalStrs $ \(name, val) ->
        globalStringPtr val $ mkName name

$(mkGlobalStrRefs globalStrs)

returnRef :: Operand
returnRef = ConstantOperand
          $ GlobalReference (ptrOf $ ptrOf i8) "returnRegister"

-------------------------------------------------------------------------------
