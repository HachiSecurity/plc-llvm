-- | This module contains definitions related to continuations.
module Hachi.Compiler.CodeGen.CPS (
    -- * Types
    contEntryTy,
    contTyDef,

    -- * Code generation
    compileDynamicCont,

    -- * Identity continuation
    cpsReturnOp,
    cpsReturnCont,
    compileCpsReturn,

    -- * Calling continuations
    callCont
) where

-------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.Reader

import Data.List (genericLength)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T

import LLVM.AST
import LLVM.AST.AddrSpace
import LLVM.AST.Constant as C
import LLVM.AST.Linkage

import Hachi.Compiler.CodeGen.Closure
import Hachi.Compiler.CodeGen.Common
import Hachi.Compiler.CodeGen.Externals
import Hachi.Compiler.CodeGen.IRBuilder as IR
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types

-------------------------------------------------------------------------------

-- | `contParamTys` is the list of parameter `Type`s for continuation
-- entry functions.
contParamTys :: [Type]
contParamTys =
    [ contTyPtr -- a pointer to the closure
    , closureTyPtr -- a pointer to the argument
    ]

-- | `contEntryTy` is the `Type` of continuation entry functions.
contEntryTy :: Type
contEntryTy = ptrOf $ FunctionType closureTyPtr contParamTys False

-- | `contComponents` is a list of components of a continuation closure.
contComponents :: [Type]
contComponents =
    [ contEntryTy
    , ArrayType 0 (ptrOf i8)
    ]

-- | `contTyDef` is the definition of the continuation closure type.
contTyDef :: Type
contTyDef = StructureType False contComponents

-- | `mkContStruct` @freeVarCount@ returns a `Type` representing a continuation
-- closure structure with @freeVarCount@-many free variables.
mkContStruct :: Int -> Type
mkContStruct fvCount =
    StructureType False
        [ contEntryTy
        , ArrayType (fromIntegral fvCount) (ptrOf i8)
        ]

-------------------------------------------------------------------------------

-- | `allocateCont` @codePtr freeVars@ generates code which dynamically
-- allocates a continuation closure where the entry code is the function
-- pointed to by @codePtr@ and the free variables are given by @freeVars@.
allocateCont
    :: (MonadCodeGen m, MonadIRBuilder m)
    => Constant -> [Operand]
    -> m Continuation
allocateCont codePtr fvs = do
    -- allocate enough space for the closure on the heap:
    -- 1 code pointer + one pointer for each free variable
    size <- mul ptrSize (ConstantOperand $ Int bits $ 1 + genericLength fvs)
    c <- malloc contTyPtr size

    -- store data in the closure: we have the function pointer followed by
    -- any pointers to free variables
    let storeAt ixs val = do
            addr <- gep c $ ConstantOperand (Int 32 0) : ixs
            store addr 0 val

    storeAt [ConstantOperand $ Int 32 0] $ ConstantOperand codePtr

    forM_ (zip [0..] fvs) $ \(i,v) -> do
        ptr <- bitcast v (ptrOf i8)
        storeAt [ConstantOperand (Int 32 1), ConstantOperand (Int 32 i)] ptr

    -- return an Operand representing a pointer to the dynamic closure that we
    -- have just created
    pure $ MkCont c

-- | `compileDynamicCont` @name freeVars parameter builder@ generates code
-- which dynamically allocates a continuation closure.
compileDynamicCont
    :: MonadCodeGen m
    => String -> S.Set (T.Text, Bool) -> T.Text
    -> (Operand -> Operand -> IRBuilderT m ())
    -> IRBuilderT m Continuation
compileDynamicCont name fvs var codeFun = do
    let entryName = mkName $ name <> "_entry"
    -- let entryTy = clsEntryTy
    let entryParams = [(contTyPtr, "this"), (closureTyPtr, "arg")]

    _ <- lift $ IR.function entryName entryParams closureTyPtr plcFunOpts $
        \[this, arg] -> localFreeVars fvs $ extendScope var (LocalClosure $ MkClosurePtr arg) $ do
            compileTrace (name <> "_entry") []

            -- update the local environment with mappings to the free variables
            -- we have obtained from the closure represented by `this` and
            -- compile the body of the function
            withFreeVars (MkCont this) $ codeFun this arg

    let code_fun = GlobalReference contEntryTy entryName

    env <- asks codeGenEnv

    vals <- forM (S.toList fvs) $ \(fv,_) -> case M.lookup fv env of
        -- TODO: handle this a bit more nicely
        Nothing -> error "Can't find variable"
        Just val -> pure val

    allocateCont code_fun (map localOperand vals)

-- | `compileCont` @name entryPtr freeVars@ generates a global variable
-- representing a continuation closure for @name@.
compileCont
    :: MonadModuleBuilder m
    => String
    -> Constant
    -> [Constant]
    -> m Constant
compileCont name codePtr fvs = do
    let closureName = mkClosureName name
    let closureType = mkContStruct $ length fvs
    let closureVal = Struct Nothing False $
            codePtr : [Array (ptrOf i8) $ map (`C.BitCast` ptrOf i8) fvs]

    void $ global closureName closureType closureVal $ setLinkage Private

    pure $ flip C.BitCast contTyPtr $
        GlobalReference (PointerType closureType $ AddrSpace 0) closureName

-------------------------------------------------------------------------------

-- | `cpsReturnOp` is a reference to the global identity continuation.
cpsReturnOp :: Operand
cpsReturnOp = ConstantOperand $ GlobalReference ty name
    where name = mkClosureName "cps_return"
          ty = ptrOf $ mkContStruct 0

-- | `cpsReturnCont` is a reference to the global identity continuation.
cpsReturnCont :: Continuation
cpsReturnCont = MkCont cpsReturnOp

-- | `compileCpsReturn` is a computation which generates code for the identity
-- continuation, which just returns its argument and makes no further function
-- calls. This is used to terminate a CPS style sequence of function calls.
compileCpsReturn :: MonadCodeGen m => m Continuation
compileCpsReturn = do
    let entryName = "cps_return_entry"
    let params = [(contTyPtr, "this"), (closureTyPtr, "arg")]

    _ <- IR.function entryName params closureTyPtr plcFunOpts $
        \[_, arg] -> ret arg

    let entryPtr = GlobalReference contEntryTy entryName

    ref <- compileCont "cps_return" entryPtr []

    pure $ MkCont $ ConstantOperand ref

-------------------------------------------------------------------------------

-- | `callCont` @continuationPtr argument@ generates code which calls the
-- continuation pointed at by @continuationPtr@ with the @argument@.
callCont
    :: (MonadCodeGen m, MonadIRBuilder m)
    => Continuation -> Operand -> m (ClosurePtr 'DynamicPtr)
callCont (MkCont k) arg = do
    -- retrieve the function pointer from the continuation closure
    funPtr <- flip load 0 =<<
              gep k [ConstantOperand (Int 32 0), ConstantOperand (Int 32 0)]

    -- cast it to the right type and call it with a pointer to the
    -- continuation closure and the argument for the parameter as arguments
    contFunPtr <- bitcast funPtr contEntryTy
    MkClosurePtr <$> call contFunPtr [(k, []), (arg, [])] plcCall

-------------------------------------------------------------------------------
