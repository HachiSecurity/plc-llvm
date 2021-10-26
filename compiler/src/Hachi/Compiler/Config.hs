
module Hachi.Compiler.Config (
  Config(..),
  parseCmdLineArgs
) where

-------------------------------------------------------------------------------

import Options.Applicative

-------------------------------------------------------------------------------

data Config = MkConfig {
    cfgInput :: FilePath,
    cfgOutput :: Maybe FilePath,
    cfgDeserialise :: Bool,
    cfgTyped :: Bool,
    cfgTrace :: Bool
} deriving (Eq, Show)

cfgInputP :: Parser FilePath
cfgInputP = strArgument $
    metavar "INPUT" <>
    help "The path to the PLC source file"

cfgOutputP :: Parser (Maybe FilePath)
cfgOutputP = optional $ option auto $
    short 'o' <>
    long "output" <>
    metavar "OUTPUT" <>
    help "The path of the output file to generate"

cfgDeserialiseP :: Parser Bool
cfgDeserialiseP = switch $
    long "deserialise" <>
    help "Deserialise PLC from binary"

cfgTypedP :: Parser Bool
cfgTypedP = switch $
    long "typed" <>
    help "Parse the input as typed PLC"

cfgTraceP :: Parser Bool
cfgTraceP = switch $
    long "trace" <>
    help "Inject tracing code into the LLVM IR"

cmdArgsP :: Parser Config
cmdArgsP = MkConfig
    <$> cfgInputP
    <*> cfgOutputP
    <*> cfgDeserialiseP
    <*> cfgTypedP
    <*> cfgTraceP

-- | `parseCmdLineArgs` parses command-line arguments into a `Config` value.
-- If parsing fails, the program is terminated and a help message is shown.
parseCmdLineArgs :: IO Config
parseCmdLineArgs =
    customExecParser (prefs showHelpOnError) $
    info (cmdArgsP <**> helper) idm

-------------------------------------------------------------------------------
