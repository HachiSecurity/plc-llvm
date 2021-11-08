
module Hachi.Compiler.CodeGen.Constant.String ( globalStrs ) where

-------------------------------------------------------------------------------

globalStrs :: [(String, String)]
globalStrs =
    [ ("nlStr", "\n")
    , ("i64FormatStr", "%d")
    , ("strFormatStr", "%s")
    , ("trueStr", "True")
    , ("falseStr", "False")
    , ("unitStr", "()")
    , ("forceErrStr", "Attempted to instantiate a non-polymorphic term.\n")
    , ("funErrStr", "Evaluation resulted in a function.")
    , ("headErrStr", "headList called on an empty list.\n")
    , ("tailErrStr", "tailList called on an empty list.\n")
    , ("openParenStr", "(")
    , ("closeParenStr", ")")
    , ("commaStr", ",")
    , ("consStr", ":")
    , ("emptyListStr", "[]")
    ]

-------------------------------------------------------------------------------
