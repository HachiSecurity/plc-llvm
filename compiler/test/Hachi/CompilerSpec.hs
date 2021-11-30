
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

-- | `runDirectoryTests` @mkConfig root@ loads programs from all directories in
-- the path given by @root@ using the configuration produced by applying
-- @mkConfig@ to a given source file's path.
runDirectoryTests :: (FilePath -> Config) -> FilePath -> IO [TestTree]
runDirectoryTests mkCfg root = do
    ds <- listDirectories root

    forM (sort ds) $ \dir ->
        pure $ testCase dir $ do
            -- compile the code
            let fp = root </> dir </> dir <.> "plc"
            compile (mkCfg fp)

            -- run the resulting program
            runAndVerify fp

test_untyped_samples :: IO [TestTree]
test_untyped_samples = runDirectoryTests mkDefaultConfig "./test-data/untyped"

mkTypedConfig :: FilePath -> Config
mkTypedConfig fp = (mkDefaultConfig fp){ cfgTyped = True }

test_typed_samples :: IO [TestTree]
test_typed_samples = runDirectoryTests mkTypedConfig "./test-data/typed"

-------------------------------------------------------------------------------
