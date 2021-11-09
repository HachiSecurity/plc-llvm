{-# LANGUAGE TypeApplications #-}

module Hachi.DataSpec where

-------------------------------------------------------------------------------

import Data.ByteString (ByteString)

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
    , testCase "Data constants: map" $
        runTest "data1" $ mkData $ Map [(I 10, B "TheCakeIsALie")]
    , testCase "Data constants: list" $
        runTest "data2" $ mkData $ List [I 108, B "TheCakeIsALie"]
    , testCase "Data constants: integer" $
        runTest "data3" $ mkData $ I 108
    , testCase "Data constants: bytestring" $
        runTest "data4" $ mkData $ B "TheCakeIsALie"
    , testCase "chooseData: constructors" $
        runTest "chooseData0" $
            mkChooseData (mkData $ Constr 42 []) true false false false false
    , testCase "chooseData: map" $
        runTest "chooseData1" $
            mkChooseData (mkData $ Map []) false true false false false
    , testCase "chooseData: list" $
        runTest "chooseData2" $
            mkChooseData (mkData $ List []) false false true false false
    , testCase "chooseData: integer" $
        runTest "chooseData3" $
            mkChooseData (mkData $ I 108) false false false true false
    , testCase "chooseData: bytestring" $
        runTest "chooseData4" $
            mkChooseData (mkData $ B "Lie") false false false false true
    , testCase "constrData" $
        runTest "constrData0" $
            mkConstrData (mkConst @Integer 42) (mkConst @[Data] [])
    , testCase "mapData" $
        runTest "mapData0" $
            mkMapData (mkConst @[(Data,Data)] [])
    , testCase "listData" $
        runTest "listData0" $
            mkListData (mkConst @[Data] [])
    , testCase "iData" $
        runTest "iData0" $
            mkIData (mkConst @Integer 42)
    , testCase "bData" $
        runTest "bData0" $
            mkBData (mkConst @ByteString "Cake")
    ]

-------------------------------------------------------------------------------
