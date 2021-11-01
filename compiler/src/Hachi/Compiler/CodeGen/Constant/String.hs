
module Hachi.Compiler.CodeGen.Constant.String ( globalStrs ) where

-------------------------------------------------------------------------------

globalStrs :: [(String, String)]
globalStrs =
    [ ("i64FormatStr", "%d\n")
    , ("strFormatStr", "%s\n")
    , ("trueStr", "True")
    , ("falseStr", "False")
    , ("forceErrStr", "Attempted to instantiate a non-polymorphic term.\n")
    , ("funErrStr", "Evaluation resulted in a function.\n")
    ]

-------------------------------------------------------------------------------
