
module Hachi.CompilerSpec where

-------------------------------------------------------------------------------

import Control.Monad

import qualified Data.ByteString.Lazy as LBS

import System.Directory
import System.FilePath
import System.Process.Typed

import Test.Tasty
import Test.Tasty.HUnit

import Hachi.Compiler
import Hachi.Compiler.Config

-------------------------------------------------------------------------------

listDirectories :: FilePath -> IO [FilePath]
listDirectories root =
    listDirectory root >>=
    filterM (doesDirectoryExist . (root </>))

test_samples :: IO [TestTree]
test_samples = do
    let root = "./test-data/untyped"
    ds <- listDirectories root

    forM ds $ \dir ->
        pure $ testCase dir $ do
            -- compile the code
            let fp = root </> dir </> dir <.> "plc"
            compile (mkDefaultConfig fp)

            -- run the resulting program
            let pcfg = proc (dropExtension fp) []
            (ec, stdout, stderr) <- readProcess pcfg

            -- read the expected output
            exStdout <- LBS.readFile $ replaceExtension fp "out"

            -- check that the output we got from the program matches the
            -- expected output
            assertEqual "Expected output does not match actual output"
                exStdout stdout

-------------------------------------------------------------------------------
