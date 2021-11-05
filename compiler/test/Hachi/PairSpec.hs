module Hachi.PairSpec where

-------------------------------------------------------------------------------

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Some

import Test.Tasty
import Test.Tasty.HUnit

import System.Directory
import System.Exit
import System.FilePath
import System.Process.Typed

import PlutusCore.Default
import UntypedPlutusCore as UPLC

import Hachi.Compiler.Config
import Hachi.Compiler.CodeGen

-------------------------------------------------------------------------------

type TestTerm = UPLC.Term UPLC.Name DefaultUni DefaultFun ()

-- this feels like it should exist in the plutus library somewhere, but I
-- couldn't find it; if there is something like this there, replace this
class Tagged a where
    uniTag :: a -> DefaultUni (Esc a)

instance (Tagged a, Tagged b) => Tagged (a,b) where
    uniTag (x,y) = DefaultUniPair (uniTag x) (uniTag y)

instance Tagged Integer where
    uniTag _ = DefaultUniInteger

instance Tagged Bool where
    uniTag _ = DefaultUniBool

instance Tagged BS.ByteString where
    uniTag _ = DefaultUniByteString

mkPair :: (Tagged a, Tagged b) => (a,b) -> TestTerm
mkPair p@(x,y) = Constant () $ Some $
    ValueOf (DefaultUniPair (uniTag x) (uniTag y)) p

mkFst :: TestTerm -> TestTerm
mkFst = Apply () (Builtin () FstPair)

mkSnd :: TestTerm -> TestTerm
mkSnd = Apply () (Builtin () SndPair)

runPairTest :: String -> TestTerm -> Assertion
runPairTest name term = do
    -- we pretend that there is a PLC source file for the test, so that we get
    -- the output files automatically in the place we want them to go
    let root = "./test-data/generated"
    let fp = root </> name <.> "plc"

    createDirectoryIfMissing True root

    generateCode (mkDefaultConfig fp) $ Program () (Version () 1 0 0) term

    -- run the resulting program
    let pcfg = proc (dropExtension fp) []
    (ec, stdout, _) <- readProcess pcfg

    -- assertEqual "Program didn't run successfully" ExitSuccess ec

    -- read the expected output
    exStdout <- LBS.readFile $ replaceExtension fp "out"

    -- check that the output we got from the program matches the
    -- expected output
    assertEqual "Expected output does not match actual output"
        exStdout stdout

-- pairs get their own specification here because the PLC parser doesn't
-- support them; therefore we have to generate the test cases programmatically
test_pairs :: [TestTree]
test_pairs =
    [ testCase "Pair constants" $
        runPairTest "pair0" $ mkPair (23 :: Integer, 42 :: Integer)
    , testCase "FstPair" $
        runPairTest "fstPair0" $ mkFst (mkPair ("hello" :: BS.ByteString, True))
    , testCase "SndPair" $
        runPairTest "sndPair0" $ mkSnd (mkPair ("hello" :: BS.ByteString, True))
    , testCase "Nested" $
        runPairTest "nested0" $ mkSnd $ mkFst $ mkPair ((True, 42 :: Integer), False)
    ]

-------------------------------------------------------------------------------
