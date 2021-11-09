
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
    , ("dataConstrStr", "Constr ")
    , ("dataMapStr", "Map ")
    , ("dataListStr", "List ")
    , ("dataIntegerStr", "I ")
    , ("dataByteStringStr", "B ")
    , ("forceErrStr", "Attempted to instantiate a non-polymorphic term.\n")
    , ("funErrStr", "Evaluation resulted in a function.")
    , ("dataMatchErrStr", "Unknown tag %d for Data value.\n")
    , ("headErrStr", "headList called on an empty list.\n")
    , ("tailErrStr", "tailList called on an empty list.\n")
    , ("spaceStr", " ")
    , ("openParenStr", "(")
    , ("closeParenStr", ")")
    , ("commaStr", ",")
    , ("consStr", ":")
    , ("emptyListStr", "[]")
    ]

-------------------------------------------------------------------------------
