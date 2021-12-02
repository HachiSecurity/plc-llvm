
module Hachi.Compiler.CodeGen.Constant.String ( globalStrs ) where

-------------------------------------------------------------------------------

globalStrs :: [(String, String)]
globalStrs =
    [ ("nlStr", "\n")
    , ("i64FormatStr", "%d")
    , ("strFormatStr", "%s")
    , ("hexFormatStr", "%02X")
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
    , ("unConstrDataErrStr", "Pattern-match failure in unConstrData\n")
    , ("unMapDataErrStr", "Pattern-match failure in unMapData\n")
    , ("unListDataErrStr", "Pattern-match failure in unListData\n")
    , ("unIDataErrStr", "Pattern-match failure in unIData\n")
    , ("unBDataErrStr", "Pattern-match failure in unBData\n")
    , ("bsIndexErrStr", "Trying to access element at index %zu of bytestring with length %zu!\n")
    , ("spaceStr", " ")
    , ("openParenStr", "(")
    , ("closeParenStr", ")")
    , ("commaStr", ",")
    , ("consStr", ":")
    , ("emptyListStr", "[]")
    ]

-------------------------------------------------------------------------------
