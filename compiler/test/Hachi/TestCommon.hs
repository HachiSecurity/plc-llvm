{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module contains types and definitions which are shared among the
-- different test suites.
module Hachi.TestCommon where

-------------------------------------------------------------------------------

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Some

import Test.Tasty.HUnit

import System.Directory
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
    uniTag :: DefaultUni (Esc a)

instance (Tagged a, Tagged b) => Tagged (a,b) where
    uniTag = DefaultUniPair (uniTag @a) (uniTag @b)

instance Tagged a => Tagged [a] where
    uniTag = DefaultUniList (uniTag @a)

instance Tagged Integer where
    uniTag = DefaultUniInteger

instance Tagged Bool where
    uniTag = DefaultUniBool

instance Tagged BS.ByteString where
    uniTag = DefaultUniByteString

-------------------------------------------------------------------------------

mkPair :: forall a b. (Tagged a, Tagged b) => (a,b) -> TestTerm
mkPair p = Constant () $ Some $ ValueOf (uniTag @(a,b)) p

mkFst :: TestTerm -> TestTerm
mkFst = Apply () (Force () (Force () (Builtin () FstPair)))

mkSnd :: TestTerm -> TestTerm
mkSnd = Apply () (Force () (Force () (Builtin () SndPair)))

mkList :: forall a. Tagged a => [a] -> TestTerm
mkList xs = Constant () $ Some $ ValueOf (uniTag @[a]) xs

-------------------------------------------------------------------------------

runTest :: String -> TestTerm -> Assertion
runTest name term = do
    -- we pretend that there is a PLC source file for the test, so that we get
    -- the output files automatically in the place we want them to go
    let root = "./test-data/generated"
    let fp = root </> name <.> "plc"

    createDirectoryIfMissing True root

    generateCode (mkDefaultConfig fp) $ Program () (Version () 1 0 0) term

    -- run the resulting program and verify that the output is as expected
    runAndVerify fp

runAndVerify :: FilePath -> Assertion
runAndVerify fp = do
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

-------------------------------------------------------------------------------
