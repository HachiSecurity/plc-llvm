
module Hachi.LibrarySpec (
    test_library_samples
) where

-------------------------------------------------------------------------------

import System.FilePath

import Test.Tasty

import Hachi.Compiler
import Hachi.Compiler.Config
import Hachi.Compiler.CodeGen.Driver
import Hachi.TestCommon

-------------------------------------------------------------------------------

mkLibraryConfig :: FilePath -> Config
mkLibraryConfig fp = (mkDefaultConfig fp){ cfgLibrary = True }

test_library_samples :: IO [TestTree]
test_library_samples = runDirectoryTests "./test-data/library" $ \fp -> do
    let cfg = mkLibraryConfig fp
    let programSrc = takeDirectory fp </> "program" <.> "c"
    let plcObj = replaceExtension fp "o"

    -- compile the PLC program to an object file
    compile cfg

    -- compile the wrapper program to an object file
    programObj <- buildObjectFile cfg programSrc

    -- link everything together, run the resulting program, and check that
    -- the output is as expected
    linkExecutable cfg fp [programObj, plcObj]
    runAndVerify fp

-------------------------------------------------------------------------------
