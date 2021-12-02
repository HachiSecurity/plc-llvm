{-# LANGUAGE TypeApplications #-}

-- | Code generation for functions we want to export when compiling to a
-- library.
module Hachi.Compiler.CodeGen.Library (
    emitLibraryApi
) where

-------------------------------------------------------------------------------

import Control.Monad

import Data.List

import LLVM.AST
import LLVM.AST.Constant
import LLVM.IRBuilder as IR

import Hachi.Compiler.CodeGen.Closure
import Hachi.Compiler.CodeGen.Constant
import Hachi.Compiler.CodeGen.Constant.Integer
import Hachi.Compiler.CodeGen.Externals.GMP
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types
import Hachi.Compiler.Platform

-------------------------------------------------------------------------------

-- | `plcApply` generates a function which can be used to apply a closure to
-- an argument.
plcApply :: MonadCodeGen m => m ()
plcApply = do
    let name = "plc_apply"
    let params = [(closureTyPtr, "f"), (closureTyPtr, "x")]

    void $ IR.function name params closureTyPtr $ \[f,x] ->
        compileApply (MkClosurePtr f) (MkClosurePtr x) >>= retClosure

-- | `plcPrintClosure` generates a function which can be used to invoke a
-- closure's pretty-printing function.
plcPrintClosure :: MonadCodeGen m => m ()
plcPrintClosure = do
    let name = "plc_print_closure"
    let params = [(closureTyPtr, "ptr")]

    void $ IR.function name params VoidType $ \[ptr] -> do
        printFun <- loadFromClosure ClosurePrint printFnTy (MkClosurePtr ptr)
        void $ call printFun [(ptr, [])]

-- | `plcNewInteger` generates a function which can be used to create a new
-- arbitrary-precision integer from a C string.
plcNewInteger :: MonadCodeGen m => m ()
plcNewInteger = do
    let name = "plc_new_integer"
    let params = [(ptrOf i8, "str")]

    void $ IR.function name params closureTyPtr $ \[str] -> do
        int <- newInteger

        void $ call (ConstantOperand mpzInitSetStrRef)
            [ (int, [])
            , (str, [])
            , (ConstantOperand $ Int platformIntSize 10, [])
            ]

        retConstDynamic @Integer int

-------------------------------------------------------------------------------

libraryApi :: MonadCodeGen m => [(m (), String)]
libraryApi =
    [ (plcApply, "extern closure *plc_apply(closure *f, closure *x);")
    , (plcPrintClosure, "extern void plc_print_closure(closure *ptr);")
    , (plcNewInteger, "extern closure *plc_new_integer(const char *str);")
    ]

-- | `emitLibraryApi` is a computation which generates all functions that
-- should be available as part of the library API. It also returns a `String`
-- value containing all of the C function signatures for the API.
emitLibraryApi :: MonadCodeGen m => m String
emitLibraryApi = fmap (intercalate "\n") $ forM libraryApi $
    \(emitCode, sig) -> emitCode >> pure sig

-------------------------------------------------------------------------------
