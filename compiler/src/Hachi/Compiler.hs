{-# LANGUAGE RecordWildCards #-}

module Hachi.Compiler ( compile ) where

-------------------------------------------------------------------------------

import Control.Monad (void)
import Control.Monad.Trans.Except ( runExceptT )

import qualified Data.ByteString.Lazy as BSL

import System.Exit ( exitFailure )

import Flat

import PlutusCore
import qualified UntypedPlutusCore as UPLC
import qualified UntypedPlutusCore.Parser as UPLC
import qualified UntypedPlutusCore.Evaluation.Machine.Cek as UPLC

import Hachi.Compiler.Config (Config(..))
import Hachi.Compiler.CodeGen ( generateCode )

-------------------------------------------------------------------------------

type DefaultTerm  a = Term TyName Name DefaultUni DefaultFun a

type DefaultError a = Error DefaultUni DefaultFun a

type UntypedProgram = UPLC.Program Name DefaultUni DefaultFun

deserialiseUntyped 
  :: BSL.ByteString 
  -> Either DecodeException (UntypedProgram ())
deserialiseUntyped = unflat

parseUntyped 
  :: BSL.ByteString 
  -> Either (DefaultError AlexPosn) (UntypedProgram AlexPosn)
parseUntyped = runQuote . runExceptT . UPLC.parseProgram

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

parseTyped 
  :: BSL.ByteString 
  -> Either (DefaultError AlexPosn) (TypedProgram AlexPosn)
parseTyped = runQuote . runExceptT . parseProgram

compileTyped :: Config -> BSL.ByteString -> IO ()
compileTyped _ xs = print $ parseTyped xs

-------------------------------------------------------------------------------

compile :: Config -> FilePath -> IO ()
compile cfg fp = do
  xs <- BSL.readFile fp

  if cfgTyped cfg
  then compileTyped cfg xs
  else do
    p <- loadUntyped cfg xs 
    putStrLn "Generating code for:"
    print p
    let (UPLC.Program _ _ t) = p
    print $ UPLC.evaluateCekNoEmit defaultCekParameters t
    generateCode cfg "out.ll" p

-------------------------------------------------------------------------------
