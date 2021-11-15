{-# LANGUAGE RecordWildCards #-}

module Hachi.Compiler ( compile ) where

-------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.Trans.Except ( runExceptT )

import qualified Data.ByteString.Lazy as BSL

import System.Exit ( exitFailure )

import Flat

import PlutusCore
import qualified UntypedPlutusCore as UPLC
import qualified UntypedPlutusCore.Parser as UPLC

import Hachi.Compiler.Config (Config(..))
import Hachi.Compiler.CodeGen ( generateCode )

-------------------------------------------------------------------------------

type DefaultError a = Error DefaultUni DefaultFun a

type UntypedProgram = UPLC.Program Name DefaultUni DefaultFun

-- | `deserialiseUntyped` is a type-specialised version of `unflat` for
-- deserialising a `UntypedProgram`.
deserialiseUntyped
  :: BSL.ByteString
  -> Either DecodeException (UntypedProgram ())
deserialiseUntyped = unflat

-- | `parseUntyped` @fileData@ parses an untyped PLC program from @fileData@.
parseUntyped
  :: BSL.ByteString
  -> Either (DefaultError AlexPosn) (UntypedProgram AlexPosn)
parseUntyped = runQuote . runExceptT . UPLC.parseProgram

-- | `loadUntyped` @config fileData@ parses or deserialises an untyped PLC
-- program from the data given by @fileData@ using options from @config@
-- and returns the program.
loadUntyped :: Config -> BSL.ByteString -> IO (UntypedProgram ())
loadUntyped MkConfig{..} xs
  | cfgDeserialise = case deserialiseUntyped xs of
    Left err -> print err >> exitFailure
    Right p -> pure p
  | otherwise = case parseUntyped xs of
    Left err -> print err >> exitFailure
    Right p -> pure $ void p

-------------------------------------------------------------------------------

type TypedProgram = Program TyName Name DefaultUni DefaultFun

-- | `deserialiseTyped` is a type-specialised version of `unflat` for
-- deserialising a `TypedProgram`.
deserialiseTyped
    :: BSL.ByteString
    -> Either DecodeException (TypedProgram ())
deserialiseTyped = unflat

-- | `parseTyped` @fileData@ parses a typed PLC program from @fileData@.
parseTyped
  :: BSL.ByteString
  -> Either (DefaultError AlexPosn) (TypedProgram AlexPosn)
parseTyped = runQuote . runExceptT . parseProgram

-- | `loadTyped` @config fileData@ parses or deserialises a typed PLC
-- program from the data given by @fileData@ using options from @config@
-- and returns the program with its types erased.
loadTyped :: Config -> BSL.ByteString -> IO (UntypedProgram ())
loadTyped MkConfig{..} xs
    | cfgDeserialise = case deserialiseTyped xs of
        Left err -> print err >> exitFailure
        Right p -> pure $ UPLC.eraseProgram p
    | otherwise = case parseTyped xs of
        Left err -> print err >> exitFailure
        Right p -> pure $ UPLC.eraseProgram $ void p

-------------------------------------------------------------------------------

-- | `loadProgram` @config fileData@ parses or deserialises an untyped PLC
-- program from the data given by @fileData@ using options from @config@.
loadProgram :: Config -> BSL.ByteString -> IO (UntypedProgram ())
loadProgram cfg xs
    | cfgTyped cfg = loadTyped cfg xs
    | otherwise = loadUntyped cfg xs

compile :: Config -> IO ()
compile cfg = do
    -- read the file contents and turn them into a UPLC AST
    xs <- BSL.readFile $ cfgInput cfg
    p <- loadProgram cfg xs

    when (cfgVerbose cfg) $ do
      putStrLn "Generating code for:"
      print p

    -- let (UPLC.Program _ _ t) = p
    -- print $ UPLC.evaluateCekNoEmit defaultCekParameters t
    generateCode cfg p

-------------------------------------------------------------------------------
