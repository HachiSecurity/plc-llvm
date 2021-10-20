{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hachi.Compiler ( compile ) where

-------------------------------------------------------------------------------

import Control.Monad.Trans.Except

import qualified Data.ByteString.Lazy as BSL

import Flat

import PlutusCore
import qualified UntypedPlutusCore as UPLC
import qualified UntypedPlutusCore.Parser as UPLC

import Hachi.Compiler.Config (Config(..))

-------------------------------------------------------------------------------

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

compileUntyped :: Config -> BSL.ByteString -> IO ()
compileUntyped MkConfig{..} xs
  | cfgDeserialise = print $ deserialiseUntyped xs
  | otherwise = print $ parseUntyped xs

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
  else compileUntyped cfg xs

-------------------------------------------------------------------------------
