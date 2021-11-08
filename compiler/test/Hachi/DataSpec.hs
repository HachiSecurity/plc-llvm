{-# LANGUAGE TypeApplications #-}

module Hachi.DataSpec where

-------------------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.HUnit

import PlutusCore.Data

import Hachi.TestCommon

-------------------------------------------------------------------------------

-- data gets its own specification here because the PLC parser doesn't
-- support it; therefore we have to generate the test cases programmatically
test_data :: [TestTree]
test_data =
    [ testCase "Data constants: constructors" $
        runTest "data0" $ mkData $ Constr 23 [I 108, B "TheCakeIsALie"]
    , testCase "Data constants: list" $
        runTest "data1" $ mkData $ Map [(I 10, B "TheCakeIsALie")]
    , testCase "Data constants: list" $
        runTest "data2" $ mkData $ List [I 108, B "TheCakeIsALie"]
    , testCase "Data constants: integer" $
        runTest "data3" $ mkData $ I 108
    , testCase "Data constants: bytestring" $
        runTest "data4" $ mkData $ B "TheCakeIsALie"
    ]

-------------------------------------------------------------------------------
