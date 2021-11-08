module Hachi.Compiler.CodeGen.Constant.Data (
    -- * Tags
    DataTag(..),

    -- * Code generation
    dataGlobal,
    withDataTag,
    loadDataPtr,
    loadConstrTag
) where

-------------------------------------------------------------------------------

import LLVM.AST.Constant
import LLVM.IRBuilder
import LLVM.AST

import Hachi.Compiler.CodeGen.Closure
import Hachi.Compiler.CodeGen.Externals
import Hachi.Compiler.CodeGen.Globals
import Hachi.Compiler.CodeGen.Types

-------------------------------------------------------------------------------

data DataTag
    = DataConstr
    | DataMap
    | DataList
    | DataI
    | DataB
    deriving (Eq, Show)

constrTag :: Constant
constrTag = Int 8 0

mapTag :: Constant
mapTag = Int 8 1

listTag :: Constant
listTag = Int 8 2

iTag :: Constant
iTag = Int 8 3

bTag :: Constant
bTag = Int 8 4

tagToConstant :: DataTag -> Constant
tagToConstant DataConstr = constrTag
tagToConstant DataMap = mapTag
tagToConstant DataList = listTag
tagToConstant DataI = iTag
tagToConstant DataB = bTag

-------------------------------------------------------------------------------

-- | `dataGlobal` @name tag params@ constructs a new Data global named @name@,
-- for the constructor identified by @tag@ and with the parameters given by
-- @params@.
dataGlobal
    :: MonadModuleBuilder m
    => Name -> DataTag -> [Constant] -> m Constant
dataGlobal name tag ps = do
    _ <- global name dataTy $ Struct Nothing False (tagToConstant tag : ps)

    pure $ GlobalReference dataTyPtr name

-- | `withDataTag` @ptr builder@ generates code which loads the tag from @ptr@
-- and then performs different actions, produced by @builder@, depending on
-- the tag value.
withDataTag
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> (DataTag -> m ()) -> m ()
withDataTag ptr k = do
    addr <- bitcast ptr (ptrOf i8)
    val <- load addr 0

    constrBr <- freshName "constr"
    mapBr <- freshName "map"
    listBr <- freshName "list"
    iBr <- freshName "i"
    bBr <- freshName "b"
    defBr <- freshName "otherwise"

    switch val defBr [ (constrTag, constrBr)
                     , (mapTag, mapBr)
                     , (listTag, listBr)
                     , (iTag, iBr)
                     , (bTag, bBr)
                     ]

    emitBlockStart constrBr
    k DataConstr

    emitBlockStart mapBr
    k DataMap

    emitBlockStart listBr
    k DataList

    emitBlockStart iBr
    k DataI

    emitBlockStart bBr
    k DataB

    emitBlockStart defBr
    _ <- printf dataMatchErrRef [val]
    _ <- exit (-1)
    unreachable

-- | `loadDataPtr` @ptr@ retrieves the payload from the Data value pointed at
-- by @ptr@. This function works for all forms of Data value.
loadDataPtr
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> m ClosurePtr
loadDataPtr ptr = do
    addr <- gep ptr [ ConstantOperand $ Int 32 0
                    , ConstantOperand $ Int 32 1
                    , ConstantOperand $ Int 32 0
                    ]
    val <- load addr 0
    MkClosurePtr <$> bitcast val closureTyPtr

-- | `loadConstrTag` @ptr@ retrieves the constructor tag from a Data value
-- pointed at by @ptr@. This function assumes that the Data value represents
-- a data constructor and no such check is performed.
loadConstrTag
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> m Operand
loadConstrTag ptr = do
    addr <- gep ptr [ ConstantOperand $ Int 32 0
                    , ConstantOperand $ Int 32 1
                    , ConstantOperand $ Int 32 1
                    ]
    val <- load addr 0
    bitcast val i64

-------------------------------------------------------------------------------
