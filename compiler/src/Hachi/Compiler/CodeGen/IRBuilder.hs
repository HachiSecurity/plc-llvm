-- | This module provides convenience functions for constructing LLVM IR,
-- including more flexible versions of functions found in 'LLVM.IRBuilder'.
module Hachi.Compiler.CodeGen.IRBuilder (
    -- * Re-exports
    module IR,

    -- * Modifiers
    setLinkage,
    HasCC(..),
    HasReturnAttr(..),
    HasFunctionAttr(..),
    HasMetadata(..),
    plcFunOpts,

    -- * Globals
    global,
    globalStringPtr,
    Hachi.Compiler.CodeGen.IRBuilder.function,

    -- * Instructions
    plcCall,
    call
) where

-------------------------------------------------------------------------------

import Control.Monad hiding (void)

import Data.ByteString.Short
import Data.Char

import GHC.Stack

import LLVM.AST as AST
import LLVM.AST.Attribute
import LLVM.AST.CallingConvention
import LLVM.AST.Constant as C
import LLVM.AST.Linkage
import LLVM.AST.Global as G
import LLVM.AST.Type (void)
import LLVM.AST.Typed
import LLVM.IRBuilder as IR hiding (global, globalStringPtr, function, call)

import Hachi.Compiler.CodeGen.Types

-------------------------------------------------------------------------------

-- | `plcCC` is the `CallingConvention` used by PLC code.
plcCC :: CallingConvention
plcCC = Fast

-- | `plcLinkage` is the `Linkage` used by PLC code.
plcLinkage :: Linkage
plcLinkage = Private

-------------------------------------------------------------------------------

-- | `setLinkage` @linkage global@ sets @global@'s linkage type to @linkage@.
setLinkage :: Linkage -> Global -> Global
setLinkage link gl = gl{ linkage = link }

class HasCC a where
    -- | `setCC` @cc object@ sets the calling convention of @object@ to @cc@.
    setCC :: CallingConvention -> a -> a

instance HasCC Global where
    setCC cc gl = gl{ G.callingConvention = cc }

class HasReturnAttr a where
    -- | `setReturnAttr` @attributes object@ sets the return attributes of
    -- @object@ to @attributes@.
    setReturnAttr :: [ParameterAttribute] -> a -> a

instance HasReturnAttr Global where
    setReturnAttr attr gl = gl{ G.returnAttributes = attr }

class HasFunctionAttr a where
    -- | `setFunctionAttr` @attributes object@ sets the function attributes of
    -- @object@ to @attributes@.
    setFunctionAttr :: [Either GroupID FunctionAttribute] -> a -> a

instance HasFunctionAttr Global where
    setFunctionAttr attr gl = gl{ G.functionAttributes = attr }

class HasMetadata a where
    -- | `setMetadata` @metadata object@ sets the metadata of @object@ to
    -- @metadata@.
    setMetadata :: [(ShortByteString, MDRef MDNode)] -> a -> a

instance HasMetadata Global where
    setMetadata md gl = gl{ G.metadata = md }

-- | `plcFunOpts` @global@ applies the default function settings we use for
-- PLC code to @global@.
plcFunOpts :: Global -> Global
plcFunOpts = setLinkage plcLinkage . setCC plcCC

-- | `global` @name type initialValue transformation@ emits a new global named
-- @name@ of @type@ with an initial value of @initialValue@. Additionally,
-- @transformation@ is applied to the global before it is emitted, so that
-- arbitrary transformations can be performed on it.
global
    :: IR.MonadModuleBuilder m
    => Name -> Type -> Constant -> (Global -> Global) -> m Operand
global nm ty initVal f = do
    IR.emitDefn $ GlobalDefinition $ f globalVariableDefaults{
        G.name = nm,
        G.type' = ty,
        linkage = External,
        initializer = Just initVal
    }
    pure $ ConstantOperand $ GlobalReference (ptrOf ty) nm

-- | `globalStringPtr` @string name transformation@ emits a new global string
-- constant named @name@ with a value of @string@. The @transformation@ is
-- applied to the `Global` before it is emitted.
globalStringPtr
    :: (IR.MonadModuleBuilder m, MonadFail m)
    => String -> Name -> (Global -> Global) -> m Constant
globalStringPtr str nm f = do
    let asciiVals = map (fromIntegral . ord) str
    let llvmVals = map (Int 8) (asciiVals ++ [0])
    let charArray = Array i8 llvmVals
    tyr <- LLVM.AST.Typed.typeOf charArray

    case tyr of
        Left err -> fail err
        Right ty -> do
            _ <- global nm ty charArray f

            pure $ C.GetElementPtr True (GlobalReference (ptrOf ty) nm)
                [Int 32 0, Int 32 0]

-- | `function` @name parameters returnType transformation bodyBuilder@ emits
-- a new function named @name@.
function
    :: IR.MonadModuleBuilder m
    => Name -> [(Type, IR.ParameterName)] -> Type
    -> (Global -> Global)
    -> ([Operand] -> IR.IRBuilderT m ())
    -> m Operand
function nm params retTy f builder = do
    let paramTys = map fst params
    let funTy = ptrOf $ FunctionType retTy paramTys False
    (paramNames, blocks) <- IR.runIRBuilderT IR.emptyIRBuilder $ do
        paramNames <- forM params $ \(_, paramName) -> case paramName of
            IR.NoParameterName -> IR.fresh
            IR.ParameterName p -> IR.fresh `IR.named` p
        builder $ zipWith LocalReference paramTys paramNames
        pure paramNames
    IR.emitDefn $ GlobalDefinition $ f functionDefaults{
        G.name = nm,
        parameters = (zipWith (\ty pnm -> Parameter ty pnm []) paramTys paramNames, False),
        returnType = retTy,
        basicBlocks = blocks
    }
    pure $ ConstantOperand $ GlobalReference funTy nm

-------------------------------------------------------------------------------

instance HasCC Instruction where
    setCC cc instr = instr{ AST.callingConvention = cc }

instance HasReturnAttr Instruction where
    setReturnAttr attr instr = instr{ AST.returnAttributes = attr }

instance HasFunctionAttr Instruction where
    setFunctionAttr attr instr = instr{ AST.functionAttributes = attr }

instance HasMetadata Instruction where
    setMetadata md instr = instr{ AST.metadata = md }

-- | `emitCallWithResult` @callInstr returnType@
emitCallWithResult :: IR.MonadIRBuilder m => Instruction -> Type -> m Operand
emitCallWithResult instr VoidType =
    IR.emitInstrVoid instr >> pure (ConstantOperand $ Undef void)
emitCallWithResult instr r = IR.emitInstr r instr

-- | `plcCall` @instruction@ modified a `Call` instruction so that it is
-- suitable for calls to PLC functions.
plcCall :: Instruction -> Instruction
plcCall = setCC plcCC

-- | `call` @function arguments transformation@ emits a new `Call` instruction
-- for a call to @function@ with @arguments@. The @transformation@ is applied
-- to the `Call` instruction before it is emitted.
call
    :: ( HasCallStack, MonadFail m
       , IR.MonadIRBuilder m, IR.MonadModuleBuilder m
       )
    => Operand
    -> [(Operand, [ParameterAttribute])]
    -> (Instruction -> Instruction)
    -> m Operand
call fun args f = do
    let instr = f Call{
        AST.tailCallKind = Nothing,
        AST.callingConvention = C,
        AST.returnAttributes = [],
        AST.function = Right fun,
        AST.arguments = args,
        AST.functionAttributes = [],
        AST.metadata = []
    }

    tyr <- typeOf fun
    case tyr of
        Left err -> fail err
        Right (FunctionType r _ _) ->
            emitCallWithResult instr r
        Right (PointerType (FunctionType r _ _) _) ->
            emitCallWithResult instr r
        ty -> fail $
            "[CodeGen:call] Cannot call non-function (Malformed AST):\n" ++
            show ty

-------------------------------------------------------------------------------
