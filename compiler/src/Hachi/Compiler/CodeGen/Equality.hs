{-# LANGUAGE LambdaCase #-}

-- | This module contains code generators for runtime, value equality checks.
module Hachi.Compiler.CodeGen.Equality (
    eqData
) where

-------------------------------------------------------------------------------

import LLVM.AST
import LLVM.AST.Constant
import LLVM.AST.IntegerPredicate as LLVM
import LLVM.IRBuilder as IR

import Hachi.Compiler.CodeGen.Closure
import Hachi.Compiler.CodeGen.Constant
import Hachi.Compiler.CodeGen.Constant.Data
import Hachi.Compiler.CodeGen.Externals
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types
import Hachi.Compiler.CodeGen.Constant.Pair
import Hachi.Compiler.CodeGen.Constant.List
import Hachi.Compiler.Platform

-------------------------------------------------------------------------------


type EqFun m = ClosurePtr -> ClosurePtr -> m Operand

eqType :: Type
eqType = ptrOf $ FunctionType i8 [closureTyPtr, closureTyPtr] False

eqDataRef :: Operand
eqDataRef = ConstantOperand $ GlobalReference eqType "eqData"

-- | `eqInteger` @ptr0 ptr1@ forces two closures representing integer values
-- and checks that the resulting values are the same.
eqInteger
    :: (MonadCodeGen m, MonadIRBuilder m)
    => ClosurePtr -> ClosurePtr -> m ()
eqInteger n m = do
    _ <- enterClosure n []
    x <- loadConstVal gmpTyPtr

    _ <- enterClosure m []
    y <- loadConstVal gmpTyPtr

    r <- mpzCmp x y
    icmp LLVM.EQ r (ConstantOperand $ Int platformIntSize 0) >>= ret

-- | `eqByteString` @ptr0 ptr1@ forces two closures representing bytestring
-- values and checks that the resulting values are the same.
eqByteString
    :: (MonadCodeGen m, MonadIRBuilder m)
    => ClosurePtr -> ClosurePtr -> m ()
eqByteString xs ys = do
    _ <- enterClosure xs []
    s0 <- loadConstVal bytestringTyPtr

    _ <- enterClosure ys []
    s1 <- loadConstVal bytestringTyPtr

    equalsByteString s0 s1 >>= ret

-- | `eqPair` @ptr0 ptr1@ forces two closures representing pairs containing
-- `Data` values and checks that both components of the pair are the same.
eqPair
    :: (MonadCodeGen m, MonadIRBuilder m)
    => ClosurePtr -> ClosurePtr -> m Operand
eqPair x y = do
    -- force the two argument closures
    _ <- enterClosure x []
    p0 <- loadConstVal pairTyPtr

    _ <- enterClosure y []
    p1 <- loadConstVal pairTyPtr

    -- retrieve the first component from each pair and check that they
    -- are the same
    p0x <- getFst p0
    p1x <- getFst p1

    xeq <- call eqDataRef [(closurePtr p0x, []), (closurePtr p1x, [])]

    -- retrieve the second component from each pair and check that they
    -- are the same
    p0y <- getSnd p0
    p1y <- getSnd p1

    yeq <- call eqDataRef [(closurePtr p0y, []), (closurePtr p1y, [])]

    -- succeed if both components are equal
    IR.and xeq yeq

-- | `eqList` @compare ptr0 ptr1@ forces two closures representing lists
-- and checks that all elements are the same using a comparison check
-- produced by @compare@.
eqList
    :: (MonadCodeGen m, MonadIRBuilder m)
    => EqFun m -> ClosurePtr -> ClosurePtr -> m ()
eqList xEq xs ys = do
    -- allocate stack space for the two list closure pointers and store the
    -- initial pointers in it
    l0Var <- alloca (ptrOf closureTyPtr) Nothing 0
    l1Var <- alloca (ptrOf closureTyPtr) Nothing 0
    store l0Var 0 (closurePtr xs)
    store l1Var 0 (closurePtr ys)

    -- comparing lists is a big loop, which we immediately jump to
    loopBranch <- freshName "loop"
    br loopBranch

    emitBlockStart loopBranch

    -- retrieve the list closure pointers from the stack and retrieve the
    -- actual list pointers from them
    l0Val <- load l0Var 0
    _ <- enterClosure (MkClosurePtr l0Val) []
    l0 <- loadConstVal listTyPtr

    l1Val <- load l1Var 0
    _ <- enterClosure (MkClosurePtr l1Val) []
    l1 <- loadConstVal listTyPtr

    -- we create block names for all the possibilities we might encounter
    eqBranch <- freshName "eq"
    neqBranch <- freshName "neq"
    l0EmptyBranch <- freshName "l0empty"
    l0ConsBranch <- freshName "l0cons"
    bothConsBranch <- freshName "cons"
    headEqualBranch <- freshName "tails"

    l0Null <- icmp LLVM.EQ l0 (ConstantOperand $ Null listTyPtr)
    l1Null <- icmp LLVM.EQ l1 (ConstantOperand $ Null listTyPtr)

    -- is the first list empty?
    condBr l0Null l0EmptyBranch l0ConsBranch

    -- the first list is empty: check if the second list is also empty;
    -- if both are empty, they are equal - otherwise they are not
    emitBlockStart l0EmptyBranch
    condBr l1Null eqBranch neqBranch

    -- the first list is not empty: check if the second list is also non-empty;
    -- if both are non-empty, we need to perform more checks - otherwise, if
    -- the second list is empty, the two lists are not equal
    emitBlockStart l0ConsBranch
    condBr l1Null neqBranch bothConsBranch

    -- both lists are not empty: we need to check the heads and tails
    emitBlockStart bothConsBranch

    -- retrieve both heads and compare them; if they are equal, we need to then
    -- check the tails as well - otherwise the two lists are not equal
    l0h <- getHead l0
    l1h <- getHead l1

    heq <- xEq l0h l1h
    condBr heq headEqualBranch neqBranch

    -- the heads of both lists are the same: retrieve the tails, store them on
    -- the stack and jump back to the start of the loop
    emitBlockStart headEqualBranch
    l0t <- getTail l0
    store l0Var 0 (closurePtr l0t)

    l1t <- getTail l1
    store l1Var 0 (closurePtr l1t)

    br loopBranch

    -- the lists are equal
    emitBlockStart eqBranch
    ret (ConstantOperand $ Int 1 1)

    -- the lists are not equal
    emitBlockStart neqBranch
    ret (ConstantOperand $ Int 1 0)

-- | `eqData` is a computation which generates a function for comparing
-- `Data` values for value equality.
eqData :: (MonadCodeGen m, MonadIRBuilder m) => m Operand
eqData = IR.function "eqData" [(closureTyPtr, "x"), (closureTyPtr, "y")] i8 $ \[x,y] -> do
    -- force both arguments and obtain their values, which should be pointers
    -- to Data objects
    _ <- enterClosure (MkClosurePtr x) []
    d0 <- loadConstVal dataTyPtr

    _ <- enterClosure (MkClosurePtr y) []
    d1 <- loadConstVal dataTyPtr

    -- first of all, we need to see that the two Data values were constructed
    -- using the same constructor: for this we compare the tags
    d0tag <- loadDataTag d0
    d1tag <- loadDataTag d1

    eqBranch <- freshName "eq"
    neqBranch <- freshName "neq"

    tagMatch <- icmp LLVM.EQ d0tag d1tag
    condBr tagMatch eqBranch neqBranch

    -- the tags aren't the same, return false
    emitBlockStart neqBranch
    ret (ConstantOperand $ Int 1 0)

    -- the tags are the same, we have to perform more comparisons: all data
    -- objects have a pointer to some payload so we retrieve that before
    -- performing checks specific to each individual constructor
    emitBlockStart eqBranch

    d0ptr <- loadDataPtr d0
    d1ptr <- loadDataPtr d1

    let callEqData a b =
            call eqDataRef [(closurePtr a, []), (closurePtr b, [])]

    withDataTag d0 $ \case
        DataConstr -> do
            -- force both constructor tags and obtain the integer values
            d0ctr <- loadConstrTag d0
            _ <- enterClosure d0ctr []
            d0ctrTag <- loadConstVal gmpTyPtr

            d1ctr <- loadConstrTag d1
            _ <- enterClosure d1ctr []
            d1ctrTag <- loadConstVal gmpTyPtr

            -- check to see if the constructor tags match
            ctrBranch <- freshName "ctr"
            cmpResult <- mpzCmp d0ctrTag d1ctrTag
            ctrMatch <- icmp LLVM.EQ cmpResult
                (ConstantOperand $ Int platformIntSize 0)
            condBr ctrMatch ctrBranch neqBranch

            -- they match: check the arguments
            emitBlockStart ctrBranch
            eqList callEqData d0ptr d1ptr
        DataMap -> eqList eqPair d0ptr d1ptr
        DataList -> eqList callEqData d0ptr d1ptr
        DataI -> eqInteger d0ptr d1ptr
        DataB -> eqByteString d0ptr d1ptr

-------------------------------------------------------------------------------
