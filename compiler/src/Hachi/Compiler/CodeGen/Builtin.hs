{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hachi.Compiler.CodeGen.Builtin ( compileBuiltins ) where

-------------------------------------------------------------------------------

import Control.Monad

import qualified Data.Map as M
import qualified Data.Set as S

import PlutusCore.Default

import LLVM.AST
import LLVM.AST.Constant
import LLVM.IRBuilder as IR

import Hachi.Compiler.CodeGen.Closure
import Hachi.Compiler.CodeGen.Common
import Hachi.Compiler.CodeGen.Constant
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types

-------------------------------------------------------------------------------

-- | `compileBinary` @name argType0 argType1 builder@ generates code for a
-- built-in function with two arguments of types @argType0@ and @argType1@.
-- The result function is curried and the body of the inner-most function is
-- produced by applying @builder@ to the `Operand`s representing the runtime
-- arguments.
compileBinary
    :: MonadCodeGen m
    => String
    -> Type
    -> Type
    -> (Operand -> Operand -> IRBuilderT (IRBuilderT m) Operand)
    -> m Constant
compileBinary name lTy rTy builder = do
    let entryName = name <> "_entry"

    _ <- IR.function (mkName entryName) [(closureTyPtr, "this"), (closureTyPtr, "x")] closureTyPtr $
        \[_, arg] -> extendScope "x" arg $ do
            compileTrace entryName

            op <- compileDynamicClosure (entryName <> "_2") (S.singleton "x") "y" $ \_ arg2 -> do
                -- the first argument is free in the body of this function;
                -- retrieve it from the environment and enter the closure
                -- represented by it; this should be a constant closure
                -- which stores a pointer to its value in the result register
                xc <- lookupVar "x" closureTyPtr
                _ <- enterClosure (MkClosurePtr xc) []
                x <- loadConstVal lTy

                -- enter the closure for the second argument; this should be
                -- a constant closure which stores a pointer to its value
                -- in the result register
                _ <- enterClosure (MkClosurePtr arg2) []
                y <- loadConstVal rTy

                -- generate the body of the function
                builder x y
            ret op

    let codePtr = GlobalReference (mkEntryTy 1) (mkName entryName)

    printPtr <- compileMsgPrint name "Evaluation resulted in a function."

    compileClosure name codePtr printPtr []

-------------------------------------------------------------------------------

compileBinaryInteger
    :: forall a m. (MonadCodeGen m, CompileConstant a)
    => String -> (Operand -> Operand -> IRBuilderT (IRBuilderT m) Operand)
    -> m Constant
compileBinaryInteger name builder = compileBinary name i64 i64 $ \x y -> do
    builder x y >>= compileConstDynamic @a

compileAddInteger :: MonadCodeGen m => m Constant
compileAddInteger = compileBinaryInteger @Integer "addInteger" add

compileSubtractInteger :: MonadCodeGen m => m Constant
compileSubtractInteger = compileBinaryInteger @Integer "subtractInteger" sub

-------------------------------------------------------------------------------

-- | `builtins` is a mapping from built-in function tags to code generators
-- for them. These are used by `compileBuiltins` to generate the code for each
-- supported built-in function.
builtins :: MonadCodeGen m => [(DefaultFun, m Constant)]
builtins =
    [ (AddInteger, compileAddInteger)
    , (SubtractInteger, compileSubtractInteger)
    ]

-- | `compileBuiltins` is a computation which generates code for all the
-- built-in functions and returns a mapping from their tags to the
-- corresponding function pointers.
compileBuiltins :: MonadCodeGen m => m (M.Map DefaultFun Constant)
compileBuiltins = fmap M.fromList $ forM builtins $ \(f, compile) -> do
    ref <- compile
    pure (f, ref)

-------------------------------------------------------------------------------
