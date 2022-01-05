module Hachi.Compiler ( compile ) where

-------------------------------------------------------------------------------

import Control.Monad

import System.Exit ( exitFailure )

import PlutusCore.Util

import Hachi.Compiler.Config (Config(..))
import Hachi.Compiler.CodeGen ( generateCode )

-------------------------------------------------------------------------------

compile :: Config -> IO ()
compile cfg = do
    -- read the file contents and turn them into a UPLC AST
    pr <- loadProgramFromFile
        (cfgTyped cfg) (cfgDeserialise cfg) (cfgInput cfg)

    case pr of
        Left err -> print err >> exitFailure
        Right p -> do
            when (cfgVerbose cfg) $ do
                putStrLn "Generating code for:"
                print p

            -- let (UPLC.Program _ _ t) = p
            -- print $ UPLC.evaluateCekNoEmit defaultCekParameters t
            generateCode cfg p

-------------------------------------------------------------------------------
