
module Hachi.Compiler.CodeGen.Constant.String ( globalStrs ) where

-------------------------------------------------------------------------------

globalStrs :: [(String, String)]
globalStrs =
    [ ("i64FormatStr", "%d\n")
    , ("strFormatStr", "%s\n")
    , ("trueStr", "True")
    , ("falseStr", "False")
    , ("unitStr", "()\n")
    , ("forceErrStr", "Attempted to instantiate a non-polymorphic term.\n")
    , ("funErrStr", "Evaluation resulted in a function.\n")
    , ("headErrStr", "headList called on an empty list\n")
    , ("tailErrStr", "tailList called on an empty list\n")
    , ("pairResultStr", "Pair of:\n")
    ]

-------------------------------------------------------------------------------
