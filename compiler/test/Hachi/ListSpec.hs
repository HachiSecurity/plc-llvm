{-# LANGUAGE TypeApplications #-}

module Hachi.ListSpec where

-------------------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.HUnit

import Hachi.TestCommon

-------------------------------------------------------------------------------

-- lists get their own specification here because the PLC parser doesn't
-- support them; therefore we have to generate the test cases programmatically
test_lists :: [TestTree]
test_lists =
    [ testCase "List constants" $
        runTest "list0" $ mkList [True, False, True, False]
    , testCase "chooseList: empty list" $
        runTest "chooseList0" $
            mkChooseList (mkList @Bool []) (mkConst True) (mkConst False)
    , testCase "chooseList: non-empty list" $
        runTest "chooseList1" $
            mkChooseList (mkList @Bool [False]) (mkConst False) (mkConst True)
    , testCase "mkCons" $
        runTest "mkCons0" $ mkCons (mkConst True) (mkList @Bool [False])
    , testCase "headList: empty list" $
        runTest "headList0" $ mkHead $ mkList @Bool []
    , testCase "headList: non-empty list" $
        runTest "headList1" $ mkHead $ mkList @Bool [True]
    , testCase "tailList: empty list" $
        runTest "tailList0" $ mkTail $ mkList @Bool []
    , testCase "tailList: non-empty list" $
        runTest "tailList1" $ mkTail $ mkList @Bool [True, False]
    ]

-------------------------------------------------------------------------------
