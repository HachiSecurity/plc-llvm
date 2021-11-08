
module Hachi.CompilerSpec where

-------------------------------------------------------------------------------

import Control.Monad

import Data.List (sort)

import System.Directory
import System.FilePath

import Test.Tasty
import Test.Tasty.HUnit

import Hachi.Compiler
import Hachi.Compiler.Config
import Hachi.TestCommon

-------------------------------------------------------------------------------

listDirectories :: FilePath -> IO [FilePath]
listDirectories root =
    listDirectory root >>=
    filterM (doesDirectoryExist . (root </>))

test_samples :: IO [TestTree]
test_samples = do
    let root = "./test-data/untyped"
    ds <- listDirectories root

    forM (sort ds) $ \dir ->
        pure $ testCase dir $ do
            -- compile the code
            let fp = root </> dir </> dir <.> "plc"
            compile (mkDefaultConfig fp)

            -- run the resulting program
            runAndVerify fp

-------------------------------------------------------------------------------
