
module Hachi.Compiler.Config (
  Config(..),
  mkDefaultConfig,
  parseCmdLineArgs
) where

-------------------------------------------------------------------------------

import Options.Applicative

-------------------------------------------------------------------------------

data Config = MkConfig {
    cfgInput :: FilePath,
    cfgOutput :: Maybe FilePath,
    cfgRTS :: Maybe FilePath,
    -- | Is the input a binary file that must be deserialised?
    cfgDeserialise :: Bool,
    -- | Is the input typed?
    cfgTyped :: Bool,
    -- | Should the resulting LLVM code print diagnostic information?
    cfgTrace :: Bool,
    -- | Should the compiler print diagnostic information while running?
    cfgVerbose :: Bool,
    -- | Should the compiler generate an object file?
    cfgNoAssemble :: Bool,
    -- | Should the compiler generate an executable?
    cfgNoLink :: Bool,
    -- | Should the LLVM IR be in plain-text?
    cfgNoSerialise :: Bool,
    -- | Should the generated code check for OOM after every malloc?
    cfgCheckOOM :: Bool,
    -- | Should we produce a library?
    cfgLibrary :: Bool,
    -- | If --library is specified, an optional path to a .c source file which
    -- contains the entry point for the program.
    cfgEntryPoint :: Maybe FilePath
} deriving (Eq, Show)

-- | `mkDefaultConfig` @input@ constructs a `Config` with reasonable defaults
-- and using @input@ as the input source file.
mkDefaultConfig :: FilePath -> Config
mkDefaultConfig fp = MkConfig{
    cfgInput = fp,
    cfgOutput = Nothing,
    cfgRTS = Nothing,
    cfgDeserialise = False,
    cfgTyped = False,
    cfgTrace = False,
    cfgVerbose = False,
    cfgNoAssemble = False,
    cfgNoLink = False,
    cfgNoSerialise = False,
    cfgCheckOOM = False,
    cfgLibrary = False,
    cfgEntryPoint = Nothing
}

cfgInputP :: Parser FilePath
cfgInputP = strArgument $
    metavar "INPUT" <>
    help "The path to the PLC source file"

cfgOutputP :: Parser (Maybe FilePath)
cfgOutputP = optional $ strOption $
    short 'o' <>
    long "output" <>
    metavar "OUTPUT" <>
    help "The path of the output file to generate"

cfgRTSP :: Parser (Maybe FilePath)
cfgRTSP = optional $ strOption $
    long "rts" <>
    help "The path to rts.c"

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

cfgVerboseP :: Parser Bool
cfgVerboseP = switch $
    short 'v' <>
    long "verbose" <>
    help "Print diagnostic messages during compilation"

cfgNoAssembleP :: Parser Bool
cfgNoAssembleP = switch $
    short 'S' <>
    help "Do not assemble the LLVM IR into an object file"

cfgNoLinkP :: Parser Bool
cfgNoLinkP = switch $
    short 'c' <>
    help "Do not link the program into an executable"

cfgNoSerialiseP :: Parser Bool
cfgNoSerialiseP = switch $
    long "emit-llvm" <>
    help "Generate plain-text LLVM IR instead of bitcode"

cfgCheckOOMP :: Parser Bool
cfgCheckOOMP = switch $
    long "check-oom" <>
    help "Generate code which checks whether malloc succeeds"

cfgLibraryP :: Parser Bool
cfgLibraryP = switch $
    long "library" <>
    help "Generate a library instead of an executable"

cfgEntryPointP :: Parser (Maybe FilePath)
cfgEntryPointP = optional $ strOption $
    long "entry-point" <>
    help "The path to a C source file which contains the entry point"

cmdArgsP :: Parser Config
cmdArgsP = MkConfig
    <$> cfgInputP
    <*> cfgOutputP
    <*> cfgRTSP
    <*> cfgDeserialiseP
    <*> cfgTypedP
    <*> cfgTraceP
    <*> cfgVerboseP
    <*> cfgNoAssembleP
    <*> cfgNoLinkP
    <*> cfgNoSerialiseP
    <*> cfgCheckOOMP
    <*> cfgLibraryP
    <*> cfgEntryPointP

-- | `parseCmdLineArgs` parses command-line arguments into a `Config` value.
-- If parsing fails, the program is terminated and a help message is shown.
parseCmdLineArgs :: IO Config
parseCmdLineArgs =
    customExecParser (prefs showHelpOnError) $
    info (cmdArgsP <**> helper) idm

-------------------------------------------------------------------------------
