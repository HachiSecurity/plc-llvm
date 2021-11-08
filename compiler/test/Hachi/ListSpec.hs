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
    ]

-------------------------------------------------------------------------------
