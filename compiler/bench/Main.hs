{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main ( main ) where

-------------------------------------------------------------------------------

import Control.Monad

import System.Exit
import System.FilePath
import System.Process.Typed

import PlutusCore
import PlutusCore.Util
import PlutusCore.Test.UPLC
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as UPLC

import Criterion.Main

import Hachi.Compiler.Config
import Hachi.Compiler.CodeGen

-------------------------------------------------------------------------------

-- | Represents benchmark configurations.
data BenchConfig = MkBenchConfig {
    benchName :: String,
    benchPath :: FilePath,
    benchProgram :: UntypedProgram ()
} deriving Show

-- | `mkInterpretBenchmark` @config argument@ constructs a `Benchmark` which
-- interprets the program given by @config@ with an extra @argument@.
mkInterpretBenchmark :: BenchConfig -> Integer -> Benchmark
mkInterpretBenchmark MkBenchConfig{ benchProgram = UPLC.Program _ _ t} arg =
    bench (show arg) $
    nf (UPLC.evaluateCekNoEmit defaultCekParameters) (t `applyE` constantE arg)

-- | `compileBenchmark` @config argument@ compiles the program given by
-- @config@ into an executable, whose path is returned. The program is provided
-- with @argument@ as an extra argument.
compileBenchmark :: BenchConfig -> Integer -> IO FilePath
compileBenchmark MkBenchConfig{..} arg = do
    let (UPLC.Program ann ver term) = benchProgram
    generateCode (mkDefaultConfig benchPath) $
        UPLC.Program ann ver (term `applyE` constantE arg)
    pure $ dropExtension benchPath

-- | `runCompiledBenchmark` @exeFile@ runs the program at @exeFile@, while
-- suppressing all standard output.
runCompiledBenchmark :: FilePath -> IO ()
runCompiledBenchmark fp = do
    let cfg = setStdout nullStream $ proc fp []
    void $ runProcess cfg

-- | `mkCompiledBenchmark` @config argument@ constructs a `Benchmark` which
-- compiles and runs the program given by @config@ with an extra @argument@.
mkCompiledBenchmark :: BenchConfig -> Integer -> Benchmark
mkCompiledBenchmark cfg arg = env (compileBenchmark cfg arg) $ \fp ->
    bench (show arg) $ nfIO $ runCompiledBenchmark fp

-- | `mkBenchmark` @configArgsTuple@ constructs a `Benchmark` which comprises
-- both interpreted and compiled benchmarks for the program given by the
-- `BenchConfig` component of @configArgsTuple@. Each of the two groups of
-- benchmarks comprises of individual benchmarks for each program argument
-- from the second component of @configArgsTuple@.
mkBenchmark :: (BenchConfig, [Integer]) -> Benchmark
mkBenchmark (cfg, args) =
    bgroup (benchName cfg) [ bgroup "interpreted" iBenchs
                           , bgroup "compiled" cBenchs
                           ]
    where iBenchs = [ mkInterpretBenchmark cfg x | x <- args ]
          cBenchs = [ mkCompiledBenchmark cfg x | x <- args ]

-- | `benchmarks` @configArgsTuples@ turns @configArgsTuples@ into a list
-- of `Benchmark` values.
benchmarks :: [(BenchConfig, [Integer])] -> [Benchmark]
benchmarks = map mkBenchmark

-------------------------------------------------------------------------------

-- | `requireProgram` @sourceFile@ attempts to parse an UPLC program from
-- @sourceFile@. If this fails, the error is sent to the standard output
-- and the benchmark program is terminated.
requireProgram :: FilePath -> IO (UntypedProgram ())
requireProgram fp = loadProgramFromFile False False fp >>= \case
    Left err -> print err >> exitFailure
    Right p -> pure p

-- | `mkBenchConfig` @name sourceFile@ constructs a `BenchConfig` value for
-- a benchmark named @name@ which consists of a base program loaded from
-- @sourceFile@.
mkBenchConfig :: String -> FilePath -> IO BenchConfig
mkBenchConfig name fp = do
    p <- requireProgram fp
    pure MkBenchConfig{
        benchName = name,
        benchPath = fp,
        benchProgram = p
    }

main :: IO ()
main = do
    -- initialise the benchmark configurations by loading the relevant PLC
    -- programs from disk
    fib <- mkBenchConfig "fib" "./test-data/library/fib0/fib0.plc"
    fac <- mkBenchConfig "fac" "./test-data/library/fac0/fac0.plc"

    defaultMain $ benchmarks
        [ (fib, [10, 20, 30])
        , (fac, [100000 {- , 200000, 300000 -}])
        ]

-------------------------------------------------------------------------------
