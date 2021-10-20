
module Hachi.Compiler.Config (
  Config(..),
  parseCmdLineArgs
) where

-------------------------------------------------------------------------------

import Options.Applicative

-------------------------------------------------------------------------------

data Config = MkConfig {
  cfgInput :: FilePath,
  cfgDeserialise :: Bool,
  cfgTyped :: Bool
} deriving (Eq, Show)

cmdArgsP :: Parser Config
cmdArgsP = MkConfig 
  <$> strArgument (metavar "INPUT" <> help "The path to the PLC file")
  <*> switch (long "deserialise" <> help "Deserialise PLC from binary")
  <*> switch (long "typed" <> help "The input is typed PLC")

parseCmdLineArgs :: IO Config
parseCmdLineArgs = 
    customExecParser (prefs showHelpOnError) $ 
    info (cmdArgsP <**> helper) idm

-------------------------------------------------------------------------------
