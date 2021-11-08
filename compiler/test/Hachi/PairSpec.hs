module Hachi.PairSpec where

-------------------------------------------------------------------------------

import qualified Data.ByteString as BS

import Test.Tasty
import Test.Tasty.HUnit

import Hachi.TestCommon

-------------------------------------------------------------------------------

-- pairs get their own specification here because the PLC parser doesn't
-- support them; therefore we have to generate the test cases programmatically
test_pairs :: [TestTree]
test_pairs =
    [ testCase "Pair constants" $
        runTest "pair0" $ mkPair (23 :: Integer, 42 :: Integer)
    , testCase "FstPair" $
        runTest "fstPair0" $ mkFst (mkPair ("hello" :: BS.ByteString, True))
    , testCase "SndPair" $
        runTest "sndPair0" $ mkSnd (mkPair ("hello" :: BS.ByteString, True))
    , testCase "Nested" $
        runTest "nested0" $ mkSnd $ mkFst $ mkPair ((True, 42 :: Integer), False)
    ]

-------------------------------------------------------------------------------
