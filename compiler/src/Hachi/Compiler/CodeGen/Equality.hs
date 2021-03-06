{-# LANGUAGE LambdaCase #-}

-- | This module contains code generators for runtime, value equality checks.
module Hachi.Compiler.CodeGen.Equality (
    eqData
) where

-------------------------------------------------------------------------------

import LLVM.AST
import LLVM.AST.Constant
import LLVM.AST.IntegerPredicate as LLVM

import Hachi.Compiler.CodeGen.Closure
import Hachi.Compiler.CodeGen.Constant
import Hachi.Compiler.CodeGen.Constant.ByteString
import Hachi.Compiler.CodeGen.Constant.Data
import Hachi.Compiler.CodeGen.Constant.List
import Hachi.Compiler.CodeGen.Constant.Pair
import Hachi.Compiler.CodeGen.CPS
import Hachi.Compiler.CodeGen.Externals
import Hachi.Compiler.CodeGen.IRBuilder as IR
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types

-------------------------------------------------------------------------------

type EqFun m = ClosurePtr 'DynamicPtr -> ClosurePtr 'DynamicPtr -> m Operand

eqType :: Type
eqType = ptrOf $ FunctionType i1 [closureTyPtr, closureTyPtr] False

eqDataRef :: Operand
eqDataRef = ConstantOperand $ GlobalReference eqType "eqData"

-- | `eqInteger` @ptr0 ptr1@ forces two closures representing integer values
-- and checks that the resulting values are the same.
eqInteger
    :: (MonadCodeGen m, MonadIRBuilder m)
    => ClosurePtr 'DynamicPtr -> ClosurePtr 'DynamicPtr -> m ()
eqInteger n m = do
    _ <- enterClosure n cpsReturnCont Nothing
    x <- loadConstVal gmpTyPtr

    _ <- enterClosure m cpsReturnCont Nothing
    y <- loadConstVal gmpTyPtr

    r <- mpzCmp x y
    icmp LLVM.EQ r (ConstantOperand $ Int platformIntSize 0) >>= ret

-- | `eqByteString` @ptr0 ptr1@ forces two closures representing bytestring
-- values and checks that the resulting values are the same.
eqByteString
    :: (MonadCodeGen m, MonadIRBuilder m)
    => ClosurePtr 'DynamicPtr -> ClosurePtr 'DynamicPtr -> m ()
eqByteString xs ys = do
    _ <- enterClosure xs cpsReturnCont Nothing
    s0 <- loadConstVal bytestringTyPtr

    _ <- enterClosure ys cpsReturnCont Nothing
    s1 <- loadConstVal bytestringTyPtr

    -- call equalsByteString which returns a char, but it will always just use
    -- one bit, so we can safely truncate it
    r <- bsEquals s0 s1
    ret r

-- | `eqPair` @ptr0 ptr1@ forces two closures representing pairs containing
-- `Data` values and checks that both components of the pair are the same.
eqPair
    :: (MonadCodeGen m, MonadIRBuilder m)
    => ClosurePtr 'DynamicPtr -> ClosurePtr 'DynamicPtr -> m Operand
eqPair x y = do
    -- force the two argument closures
    _ <- enterClosure x cpsReturnCont Nothing
    p0 <- loadConstVal pairTyPtr

    _ <- enterClosure y cpsReturnCont Nothing
    p1 <- loadConstVal pairTyPtr

    -- retrieve the first component from each pair and check that they
    -- are the same
    p0x <- getFst p0
    p1x <- getFst p1

    xeq <- call eqDataRef [(closurePtr p0x, []), (closurePtr p1x, [])] plcCall

    -- retrieve the second component from each pair and check that they
    -- are the same
    p0y <- getSnd p0
    p1y <- getSnd p1

    yeq <- call eqDataRef [(closurePtr p0y, []), (closurePtr p1y, [])] plcCall

    -- succeed if both components are equal
    IR.and xeq yeq

-- | `eqList` @compare ptr0 ptr1@ forces two closures representing lists
-- and checks that all elements are the same using a comparison check
-- produced by @compare@.
eqList
    :: (MonadCodeGen m, MonadIRBuilder m)
    => EqFun m -> ClosurePtr 'DynamicPtr -> ClosurePtr 'DynamicPtr -> m ()
eqList xEq xs ys = do
    -- allocate stack space for the two list closure pointers and store the
    -- initial pointers in it
    l0Var <- alloca closureTyPtr Nothing 0
    l1Var <- alloca closureTyPtr Nothing 0
    store l0Var 0 (closurePtr xs)
    store l1Var 0 (closurePtr ys)

    -- comparing lists is a big loop, which we immediately jump to
    loopBranch <- freshName "loop"
    br loopBranch

    emitBlockStart loopBranch

    -- retrieve the list closure pointers from the stack and retrieve the
    -- actual list pointers from them
    l0Val <- load l0Var 0
    _ <- enterClosure (MkClosurePtr l0Val) cpsReturnCont Nothing
    l0 <- loadConstVal listTyPtr

    l1Val <- load l1Var 0
    _ <- enterClosure (MkClosurePtr l1Val) cpsReturnCont Nothing
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
eqData = IR.function "eqData" [(closureTyPtr, "x"), (closureTyPtr, "y")] i1 plcFunOpts $ \[x,y] -> do
    -- force both arguments and obtain their values, which should be pointers
    -- to Data objects
    _ <- enterClosure (MkClosurePtr x) cpsReturnCont Nothing
    d0 <- loadConstVal dataTyPtr

    _ <- enterClosure (MkClosurePtr y) cpsReturnCont Nothing
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
            call eqDataRef [(closurePtr a, []), (closurePtr b, [])] plcCall

    withDataTag d0 $ \case
        DataConstr -> do
            -- force both constructor tags and obtain the integer values
            d0ctr <- loadConstrTag d0
            _ <- enterClosure d0ctr cpsReturnCont Nothing
            d0ctrTag <- loadConstVal gmpTyPtr

            d1ctr <- loadConstrTag d1
            _ <- enterClosure d1ctr cpsReturnCont Nothing
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
