
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
mkLibraryConfig fp = (mkDefaultConfig fp){
    cfgLibrary = True,
    cfgNoSerialise = True,
    cfgEntryPoint = Just $ takeDirectory fp </> "program" <.> "c"
}

test_library_samples :: IO [TestTree]
test_library_samples = runDirectoryTests "./test-data/library" $ \fp -> do
    let cfg = mkLibraryConfig fp
    let programSrc = takeDirectory fp </> "program" <.> "c"
    let plcObj = replaceExtension fp "o"

    -- compile the PLC program and wrapper to object files, and link them
    -- together into an executable
    compile cfg

    -- check that the output is as expected
    runAndVerify fp

-------------------------------------------------------------------------------
