{-# LANGUAGE TypeApplications #-}

-- | Code generation for functions we want to export when compiling to a
-- library.
module Hachi.Compiler.CodeGen.Library (
    emitLibraryApi
) where

-------------------------------------------------------------------------------

import Control.Monad

import Data.ByteString (ByteString)
import Data.List
import Data.Text (Text)

import PlutusCore.Data (Data)

import LLVM.AST
import LLVM.AST.CallingConvention
import LLVM.AST.Constant as C
import LLVM.AST.Linkage

import Hachi.Compiler.CodeGen.Closure
import Hachi.Compiler.CodeGen.Constant
import Hachi.Compiler.CodeGen.Constant.ByteString
import Hachi.Compiler.CodeGen.Constant.Data
import Hachi.Compiler.CodeGen.Constant.Integer
import Hachi.Compiler.CodeGen.Constant.List
import Hachi.Compiler.CodeGen.Constant.Pair
import Hachi.Compiler.CodeGen.CPS
import Hachi.Compiler.CodeGen.Externals.GMP
import Hachi.Compiler.CodeGen.IRBuilder as IR
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types

-------------------------------------------------------------------------------

-- | `libFunction` @name parameters returnType bodyBuilder@ emits a function
-- named @name@ that is callable from C.
libFunction
    :: MonadModuleBuilder m
    => Name -> [(Type, ParameterName)] -> Type
    -> ([Operand] -> IRBuilderT m ())
    -> m Operand
libFunction nm params retTy
    = IR.function nm params retTy
    $ setLinkage External
    . setCC C

-- | `plcApply` generates a function which can be used to apply a closure to
-- an argument.
plcApply :: MonadCodeGen m => m ()
plcApply = do
    let name = "plc_apply"
    let params = [(closureTyPtr, "f"), (closureTyPtr, "x")]

    void $ libFunction name params closureTyPtr $ \[f,x] ->
        compileApply cpsReturnCont (MkClosurePtr f) (MkClosurePtr x) >>=
        retClosure

-- | `plcPrintClosure` generates a function which can be used to invoke a
-- closure's pretty-printing function.
plcPrintClosure :: MonadCodeGen m => m ()
plcPrintClosure = do
    let name = "plc_print_closure"
    let params = [(closureTyPtr, "ptr")]

    void $ libFunction name params VoidType $ \[ptr] -> do
        printFun <- loadFromClosure ClosurePrint printFnTy (MkClosurePtr ptr)
        void $ call printFun [(ptr, [])] id

-- | `plcNewInteger` generates a function which can be used to create a new
-- arbitrary-precision integer from a C string.
plcNewInteger :: MonadCodeGen m => m ()
plcNewInteger = do
    let name = "plc_new_integer"
    let params = [(ptrOf i8, "str")]

    void $ libFunction name params closureTyPtr $ \[str] -> do
        int <- newInteger

        void $ call (ConstantOperand mpzInitSetStrRef)
            [ (int, [])
            , (str, [])
            , (ConstantOperand $ Int platformIntSize 10, [])
            ] id

        retConstDynamic @Integer cpsReturnCont int

plcNewByteString :: MonadCodeGen m => m ()
plcNewByteString = do
    let name = "plc_new_bytestring"
    let params = [(i64, "size"), (ptrOf i8, "ptr")]

    void $ libFunction name params closureTyPtr $ \[size, ptr] -> do
        addr <- bsNewStruct size

        dataAddr <- gep addr [ ConstantOperand $ Int 32 0
                             , ConstantOperand $ Int 32 1
                             ]
        store dataAddr 0 ptr

        retConstDynamic @ByteString cpsReturnCont addr

plcNewText :: MonadCodeGen m => m ()
plcNewText = do
    let name = "plc_new_text"
    let params = [(ptrOf i8, "ptr")]

    void $ libFunction name params closureTyPtr $ \[ptr] ->
        retConstDynamic @Text cpsReturnCont ptr

plcNewUnit :: MonadCodeGen m => m ()
plcNewUnit = do
    let name = "plc_new_unit"
    let params = []

    void $ libFunction name params closureTyPtr $ \[] ->
        retConstDynamic @() cpsReturnCont $
        ConstantOperand $ Int 1 1

plcNewBool :: MonadCodeGen m => m ()
plcNewBool = do
    let name = "plc_new_bool"
    let params = [(i8, "val")]

    void $ libFunction name params closureTyPtr $ \[val] ->
        trunc val i1 >>= retConstDynamic @Bool cpsReturnCont

plcNewPair :: MonadCodeGen m => m ()
plcNewPair = do
    let name = "plc_new_pair"
    let params = [(closureTyPtr, "fst"), (closureTyPtr, "snd")]

    void $ libFunction name params closureTyPtr $ \[x, y] ->
        newPair (MkClosurePtr x) (MkClosurePtr y) >>=
        retConstDynamic @((),()) cpsReturnCont

plcEmptyList :: MonadCodeGen m => m ()
plcEmptyList = do
    let name = "plc_empty_list"
    let params = []

    void $ libFunction name params closureTyPtr $ \[] ->
        retConstDynamic @[()] cpsReturnCont $
        ConstantOperand $ Null listTyPtr


plcNewList :: MonadCodeGen m => m ()
plcNewList = do
    let name = "plc_new_list"
    let params = [(closureTyPtr, "hd"), (closureTyPtr, "tl")]

    void $ libFunction name params closureTyPtr $ \[x, y] -> do
        listNew x y >>= retConstDynamic @[()] cpsReturnCont

plcNewDataConstr :: MonadCodeGen m => m ()
plcNewDataConstr = do
    let name = "plc_new_data_constr"
    let params = [(closureTyPtr, "tag"), (closureTyPtr, "list")]

    void $ libFunction name params closureTyPtr $ \[tag, xs] ->
        newData DataConstr xs (Just tag) >>=
        retConstDynamic @Data cpsReturnCont

plcNewDataMap :: MonadCodeGen m => m ()
plcNewDataMap = do
    let name = "plc_new_data_map"
    let params = [(closureTyPtr, "list")]

    void $ libFunction name params closureTyPtr $ \[xs] ->
        newData DataMap xs Nothing >>=
        retConstDynamic @Data cpsReturnCont

plcNewDataList :: MonadCodeGen m => m ()
plcNewDataList = do
    let name = "plc_new_data_list"
    let params = [(closureTyPtr, "list")]

    void $ libFunction name params closureTyPtr $ \[xs] ->
        newData DataList xs Nothing >>=
        retConstDynamic @Data cpsReturnCont

plcNewDataInteger :: MonadCodeGen m => m ()
plcNewDataInteger = do
    let name = "plc_new_data_integer"
    let params = [(closureTyPtr, "n")]

    void $ libFunction name params closureTyPtr $ \[n] ->
        newData DataI n Nothing >>=
        retConstDynamic @Data cpsReturnCont

plcNewDataByteString :: MonadCodeGen m => m ()
plcNewDataByteString = do
    let name = "plc_new_data_bytestring"
    let params = [(closureTyPtr, "str")]

    void $ libFunction name params closureTyPtr $ \[str] ->
        newData DataB str Nothing >>=
        retConstDynamic @Data cpsReturnCont

-------------------------------------------------------------------------------

libraryApi :: MonadCodeGen m => [(m (), String)]
libraryApi =
    [ (plcApply, "extern closure *plc_apply(closure *f, closure *x);")
    , (plcPrintClosure, "extern void plc_print_closure(closure *ptr);")
    , (plcNewInteger, "extern closure *plc_new_integer(const char *str);")
    , (plcNewByteString, "extern closure *plc_new_bytestring(size_t len, const char *ptr);")
    , (plcNewText, "extern closure *plc_new_text(const char* ptr);")
    , (plcNewUnit, "extern closure *plc_new_unit();")
    , (plcNewBool, "extern closure *plc_new_bool(unsigned char val);")
    , (plcNewPair, "extern closure *plc_new_pair(closure *fst, closure *snd);")
    , (plcEmptyList, "extern closure *plc_empty_list();")
    , (plcNewList, "extern closure *plc_new_list(closure *head, closure *tail);")
    , (plcNewDataConstr, "extern closure *plc_new_data_constr(closure *tag, closure *args);")
    , (plcNewDataMap, "extern closure *plc_new_data_map(closure *list);")
    , (plcNewDataList, "extern closure *plc_new_data_list(closure *list);")
    , (plcNewDataInteger, "extern closure *plc_new_data_integer(closure *n);")
    , (plcNewDataByteString, "extern closure *plc_new_data_bytestring(closure *str);")
    ]

-- | `emitLibraryApi` is a computation which generates all functions that
-- should be available as part of the library API. It also returns a `String`
-- value containing all of the C function signatures for the API.
emitLibraryApi :: MonadCodeGen m => m String
emitLibraryApi = fmap (intercalate "\n") $ forM libraryApi $
    \(emitCode, sig) -> emitCode >> pure sig

-------------------------------------------------------------------------------
