{-# LANGUAGE RecordWildCards #-}

module Hachi.Compiler.CodeGen.Driver (
    runDriver,
    buildObjectFile,
    linkExecutable
) where

-------------------------------------------------------------------------------

import Control.Monad

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe

import System.Exit
import System.FilePath
import System.Process.Typed

import Hachi.Compiler.Config

-------------------------------------------------------------------------------

-- | `runPkgConfig` @options packageName@ runs `pkg-config` for @packageName@
-- with @options@ to get configuration options for the C compiler.
runPkgConfig :: Bool -> [String] -> String -> IO [String]
runPkgConfig verbose args pkg = do
    let pkgcfg = proc "pkg-config" (pkg : args)
    (ec, stdout, _) <- readProcess pkgcfg

    case ec of
        ExitSuccess -> pure $
            map BS.unpack $ BS.split ' ' $ BS.strip $ LBS.toStrict stdout
        ExitFailure _ -> do
            when verbose $
                putStrLn $ "Warning: pkg-config failed for " ++ show pkgcfg
            -- if pkg-config fails, let's just try to guess
            pure ["-l" <> pkg]

-- | `runDriver` @config pkgConfigArgs clangArgs@ runs Clang with arguments
-- given by @clangArgs@ and additional arguments obtained by running
-- pkg-config with the arguments given by @pkgConfigArgs@ for all of our
-- dependencies.
runDriver :: Config -> [String] -> [String] -> IO ()
runDriver MkConfig{..} pkCfgArgs clangArgs = do
    -- run pkg-config for our dependencies
    sodiumOpts <- runPkgConfig cfgVerbose pkCfgArgs "libsodium"
    gmpOpts <- runPkgConfig cfgVerbose pkCfgArgs "gmp"

    let pcfg = proc "clang" $ clangArgs <> sodiumOpts <> gmpOpts

    ec <- runProcess pcfg

    when cfgVerbose $ do
        putStr "Ran clang and got: "
        print ec

-- | `buildObjectFile` @config sourceFile@ uses Clang to compile the source
-- file at @sourceFile@ to an object file. The path of the generated object
-- file is returned.
buildObjectFile :: Config -> FilePath -> IO FilePath
buildObjectFile cfg sourceFile = do
    let rtsPath = maybe "./rts/" takeDirectory (cfgRTS cfg)
    let outputFile = replaceExtension sourceFile "o"

    runDriver cfg ["--cflags"]
        [sourceFile, "-c", "-o", outputFile, "-I" <> rtsPath]

    pure outputFile

-- | `linkExecutable` @config outputName objectFiles@ runs Clang to produce an
-- executable using the configuration given by @config@, where @outputName@
-- is the path we computed for the output, and @objectFiles@ are the paths of
-- the object files that we want to link together.
linkExecutable :: Config -> FilePath -> [FilePath] -> IO ()
linkExecutable cfg outputName objectFiles = do
    let rtsFile = fromMaybe "./rts/rts.c" (cfgRTS cfg)
    let sha3File = takeDirectory rtsFile
               </> "tiny_sha3" </> "sha3" <.> "c"
    let exeFile = dropExtension outputName

    runDriver cfg ["--libs", "--cflags"] $
        objectFiles <> [rtsFile, sha3File, "-o", exeFile]

-------------------------------------------------------------------------------
