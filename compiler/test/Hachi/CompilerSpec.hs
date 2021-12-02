
module Hachi.CompilerSpec (
    test_untyped_samples,
    test_typed_samples
) where

-------------------------------------------------------------------------------

import Test.Tasty

import Hachi.Compiler
import Hachi.Compiler.Config
import Hachi.TestCommon

-------------------------------------------------------------------------------

test_untyped_samples :: IO [TestTree]
test_untyped_samples =
    runDirectoryTests "./test-data/untyped" $ \fp ->
    compile (mkDefaultConfig fp) >> runAndVerify fp

mkTypedConfig :: FilePath -> Config
mkTypedConfig fp = (mkDefaultConfig fp){ cfgTyped = True }

test_typed_samples :: IO [TestTree]
test_typed_samples =
    runDirectoryTests "./test-data/typed" $ \fp ->
    compile (mkTypedConfig fp) >> runAndVerify fp

-------------------------------------------------------------------------------
