module Hachi.Compiler.CodeGen.Constant.Data (
    -- * Tags
    DataTag(..),

    -- * Code generation
    dataGlobal,
    newData,
    withDataTag,
    loadDataTag,
    loadDataPtr,
    loadConstrTag
) where

-------------------------------------------------------------------------------

import Control.Monad

import LLVM.AST.Constant
import LLVM.IRBuilder as IR
import LLVM.AST

import Hachi.Compiler.CodeGen.Closure
import Hachi.Compiler.CodeGen.Externals
import Hachi.Compiler.CodeGen.Globals
import Hachi.Compiler.CodeGen.Monad
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

-- | `newData` @tag dataPtr extraData@ allocates a new Data value on the heap
-- for the constructor identified by @tag@ where the data pointer is given by
-- @dataPtr@ and an additional argument by @extraData@ which is normally empty
-- or an extra tag in the case of `DataConstr`.
newData
    :: (MonadCodeGen m, MonadIRBuilder m)
    => DataTag -> Operand -> Maybe Operand -> m Operand
newData tag dPtr ps = do
    -- `sizeof` calculates the base size of a Data value at 16 bytes, which
    -- is enough for the tag+data pointer
    baseSize <- IR.sizeof 64 listTy

    -- however, if we have additional arguments, we need space for those too
    -- calculating this by *8 assumes a 64-bit platform
    size <- add baseSize (ConstantOperand $ Int 64 $ toInteger $ length ps * 8)

    -- allocate the memory we need
    ptr <- malloc dataTyPtr size

    -- store the tag
    tAddr <- tagAddr ptr
    store tAddr 0 $ ConstantOperand (tagToConstant tag)

    -- store the data pointer
    dAddr <- gep ptr dataIndex
    store dAddr 0 dPtr

    -- if there is a parameter, store it
    forM_ ps $ \p -> do
        eAddr <- gep ptr constrTagIndex
        void $ store eAddr 0 p

    pure ptr

-- | `withDataTag` @ptr builder@ generates code which loads the tag from @ptr@
-- and then performs different actions, produced by @builder@, depending on
-- the tag value.
withDataTag
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> (DataTag -> m ()) -> m ()
withDataTag ptr k = do
    val <- loadDataTag ptr

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

-------------------------------------------------------------------------------

tagIndex :: [Operand]
tagIndex =
    [ ConstantOperand $ Int 32 0
    , ConstantOperand $ Int 32 0
    ]

dataIndex :: [Operand]
dataIndex =
    [ ConstantOperand $ Int 32 0
    , ConstantOperand $ Int 32 1
    , ConstantOperand $ Int 32 0
    ]

constrTagIndex :: [Operand]
constrTagIndex =
    [ ConstantOperand $ Int 32 0
    , ConstantOperand $ Int 32 1
    , ConstantOperand $ Int 32 1
    ]

tagAddr :: (MonadModuleBuilder m, MonadIRBuilder m) => Operand -> m Operand
tagAddr ptr = gep ptr tagIndex >>= \addr -> bitcast addr (ptrOf i8)

-- | `loadDataTag` @ptr@ retrieves the data tag from @ptr@.
loadDataTag
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> m Operand
loadDataTag ptr = do
    addr <- tagAddr ptr
    load addr 0

-- | `loadDataPtr` @ptr@ retrieves the payload from the Data value pointed at
-- by @ptr@. This function works for all forms of Data value.
loadDataPtr
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> m (ClosurePtr 'DynamicPtr)
loadDataPtr ptr = do
    addr <- gep ptr dataIndex
    val <- load addr 0
    MkClosurePtr <$> bitcast val closureTyPtr

-- | `loadConstrTag` @ptr@ retrieves the constructor tag from a Data value
-- pointed at by @ptr@. This function assumes that the Data value represents
-- a data constructor and no such check is performed.
loadConstrTag
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> m (ClosurePtr 'DynamicPtr)
loadConstrTag ptr = do
    addr <- gep ptr constrTagIndex
    val <- load addr 0
    MkClosurePtr <$> bitcast val closureTyPtr

-------------------------------------------------------------------------------
