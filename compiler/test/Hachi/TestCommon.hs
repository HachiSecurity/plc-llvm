{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module contains types and definitions which are shared among the
-- different test suites.
module Hachi.TestCommon where

-------------------------------------------------------------------------------

import Control.Monad

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Some

import Test.Tasty
import Test.Tasty.HUnit

import System.Directory
import System.FilePath
import System.Process.Typed

import PlutusCore.Data
import PlutusCore.Default
import UntypedPlutusCore as UPLC

import Hachi.Compiler.Config
import Hachi.Compiler.CodeGen
import Data.List

-------------------------------------------------------------------------------

-- | `listDirectories` @path@ lists all directories in the directory at
-- @path@. Note that no checks are performed to see whether @path@ actually
-- refers to a directory.
listDirectories :: FilePath -> IO [FilePath]
listDirectories root =
    listDirectory root >>=
    filterM (doesDirectoryExist . (root </>))

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

instance Tagged () where
    uniTag = DefaultUniUnit

instance Tagged Integer where
    uniTag = DefaultUniInteger

instance Tagged Bool where
    uniTag = DefaultUniBool

instance Tagged BS.ByteString where
    uniTag = DefaultUniByteString

instance Tagged Data where
    uniTag = DefaultUniData

-------------------------------------------------------------------------------

true :: TestTerm
true = mkConst True

false :: TestTerm
false = mkConst False

-------------------------------------------------------------------------------

mkConst :: forall a. Tagged a => a -> TestTerm
mkConst = Constant () . Some . ValueOf (uniTag @a)

mkBuiltIn :: DefaultFun -> TestTerm
mkBuiltIn = Builtin ()

infixl 5 `mkApp`
mkApp :: TestTerm -> TestTerm -> TestTerm
mkApp = Apply ()

mkForce :: TestTerm -> TestTerm
mkForce = Force ()

mkPair :: forall a b. (Tagged a, Tagged b) => (a,b) -> TestTerm
mkPair = mkConst @(a,b)

mkFst :: TestTerm -> TestTerm
mkFst = mkApp (Force () (Force () (Builtin () FstPair)))

mkSnd :: TestTerm -> TestTerm
mkSnd = mkApp (Force () (Force () (Builtin () SndPair)))

mkList :: forall a. Tagged a => [a] -> TestTerm
mkList = mkConst @[a]

mkChooseList :: TestTerm -> TestTerm -> TestTerm -> TestTerm
mkChooseList xs a b =
    mkForce (mkForce $ Builtin () ChooseList) `mkApp` xs `mkApp` a `mkApp` b

mkCons :: TestTerm -> TestTerm -> TestTerm
mkCons x xs = mkForce (Builtin () MkCons) `mkApp` x `mkApp` xs

mkHead :: TestTerm -> TestTerm
mkHead xs = mkForce (Builtin () HeadList) `mkApp` xs

mkTail :: TestTerm -> TestTerm
mkTail xs = mkForce (Builtin () TailList) `mkApp` xs

mkNull :: TestTerm -> TestTerm
mkNull xs = mkForce (Builtin () NullList) `mkApp` xs

mkData :: Data -> TestTerm
mkData = mkConst @Data

mkChooseData
    :: TestTerm -> TestTerm -> TestTerm -> TestTerm -> TestTerm -> TestTerm
    -> TestTerm
mkChooseData d kConstr kMap kList kI kB =
    mkForce (Builtin () ChooseData) `mkApp` d `mkApp`
    kConstr `mkApp` kMap `mkApp` kList `mkApp` kI `mkApp` kB

mkConstrData :: TestTerm -> TestTerm -> TestTerm
mkConstrData ix xs = Builtin () ConstrData `mkApp` ix `mkApp` xs

mkMapData :: TestTerm -> TestTerm
mkMapData xs = Builtin () MapData `mkApp` xs

mkListData :: TestTerm -> TestTerm
mkListData xs = Builtin () ListData `mkApp` xs

mkIData :: TestTerm -> TestTerm
mkIData i = Builtin () IData `mkApp` i

mkBData :: TestTerm -> TestTerm
mkBData xs = Builtin () BData `mkApp` xs

mkUnConstrData :: TestTerm -> TestTerm
mkUnConstrData t = Builtin () UnConstrData `mkApp` t

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

-- | `runDirectoryTests` @root compiler@ tests programs from all
-- directories in the path given by @root@ using the compilation action
-- given by @compiler@.
runDirectoryTests
    :: FilePath -> (FilePath -> IO ()) -> IO [TestTree]
runDirectoryTests root compiler = do
    ds <- listDirectories root

    forM (sort ds) $ \dir ->
        pure $ testCase dir $ compiler $ root </> dir </> dir <.> "plc"

-------------------------------------------------------------------------------
