
module Main ( main ) where

-------------------------------------------------------------------------------

import Hachi.Compiler ( compile )
import Hachi.Compiler.Config ( parseCmdLineArgs )

-------------------------------------------------------------------------------

-- | `main` is the main entry point for this application.
main :: IO ()
main = do
    args <- parseCmdLineArgs
    compile args

-------------------------------------------------------------------------------
