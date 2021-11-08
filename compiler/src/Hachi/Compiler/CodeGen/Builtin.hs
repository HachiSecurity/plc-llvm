{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hachi.Compiler.CodeGen.Builtin ( compileBuiltins ) where

-------------------------------------------------------------------------------

import Control.Monad

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import PlutusCore.Default

import LLVM.AST
import LLVM.AST.Constant
import LLVM.AST.IntegerPredicate as LLVM
import LLVM.IRBuilder as IR

import Hachi.Compiler.CodeGen.Closure
import Hachi.Compiler.CodeGen.Common
import Hachi.Compiler.CodeGen.Constant
import Hachi.Compiler.CodeGen.Constant.Data
import Hachi.Compiler.CodeGen.Constant.List
import Hachi.Compiler.CodeGen.Constant.Pair
import Hachi.Compiler.CodeGen.Globals
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types
import qualified Hachi.Compiler.CodeGen.Externals as E

-------------------------------------------------------------------------------

class ToParams a where
    -- | `mkParams` @tyVars params@ constructs a list of parameter
    -- specifications where @tyVars@ is the number of type variables that are
    -- being bound and @params@ is the list of actual parameter types.
    mkParams :: Int -> [a] -> [(a, Bool)]

instance ToParams T.Text where
    mkParams n vs = take n tyVars ++ [(v, False) | v <- vs]
        where tyVars = map (\i -> (T.pack $ "_tyVar" ++ show i, True)) [0..]

instance ToParams Type where
    mkParams n ts = replicate n (closureTyPtr, True) ++ [(t, False) | t <- ts]

-- | `withCurried` @name params bodyBuilder@ constructs a curried function
-- with @params@-many parameters, where the body of the inner-most function
-- is generated by @bodyBuilder@.
withCurried
    :: forall m. MonadCodeGen m
    => String
    -> [(T.Text,Bool)]
    -> ([Operand] -> IRBuilderT m ())
    -> m ClosurePtr
withCurried _ [] _ = error "withCurried needs at least one argument"
withCurried name ps@((sn,isTyVar):dyn) builder = do
    let entryName = name <> "_static"

    -- this helper function generates code which creates dynamic closures for
    -- the functions that are returned to bind the 2nd argument onwards
    let mkCurry :: Int -> S.Set T.Text -> [(T.Text,Bool)]
                -> IRBuilderT m ()
        mkCurry _ _ [] = do
            -- if there are no further parameters, retrieve all of the
            -- parameters from the current environment
            argv <- forM ps $ \(pn,_) -> lookupVar pn closureTyPtr

            -- then generate the body of the inner-most function
            builder argv
        mkCurry ix fvs ((vn,isTyVar'):vs) = do
            -- if we have another parameter, generate code which allocates a
            -- dynamic closure for the function that binds it
            let dynName = name <> "_dynamic_" <> show ix
            ptr <- compileDynamicClosure isTyVar' dynName fvs vn $ \_ arg -> do
                extendScope vn (MkClosurePtr arg) $
                    mkCurry (ix+1) (S.union (S.singleton vn) fvs) vs
            retClosure ptr

    -- construct the static closure for this built-in function, which brings
    -- the first argument into scope and (if there is more than one parameter)
    -- returns a function that binds the next argument, and so on
    let staticParams = [(closureTyPtr, "this"), (closureTyPtr, mkParamName sn)]
    _ <- IR.function (mkName entryName) staticParams closureTyPtr $
        \[_, arg] -> extendScope sn (MkClosurePtr arg) $ do
            compileTrace entryName

            mkCurry 0 (S.singleton sn) dyn

    let codePtr = GlobalReference (mkEntryTy 1) (mkName entryName)

    printPtr <- compileFunPrint name

    compileClosure isTyVar name codePtr printPtr []

-- | `compileCurried` @name argTypes builder@ generates code for a built-in
-- function with @argTypes@-many parameters of the given types. The resulting
-- function is curried and the body of the inner-most function is produced by
-- applying @builder@ to the `Operand`s representing the runtime arguments'
-- values that are obtained after entering the arguments' closures by loading
-- them from the result register.
compileCurried
    :: MonadCodeGen m
    => String
    -> [(Type,Bool)]
    -> ([Operand] -> IRBuilderT m ())
    -> m ClosurePtr
compileCurried name tys builder =
    let params = [ (T.pack $ 'v' : show i, v) | ((_,v),i) <- zip tys [0..]]
    in withCurried name params $ \argv -> do
        -- do not enter type variable arguments or bad things may happen
        let nonTyVars = filter (not . snd . snd) $ zip argv tys
        vars <- forM nonTyVars $ \(arg, ty) -> do
            _ <- enterClosure (MkClosurePtr arg) []
            loadConstVal $ fst ty
        builder vars

-- | `compileBinary` @name argType0 argType1 builder@ generates code for a
-- built-in function with two parameters of types @argType0@ and @argType1@.
-- The resulting function is curried and the body of the inner-most function is
-- produced by applying @builder@ to the `Operand`s representing the runtime
-- arguments' values that are obtained after entering the arguments' closures
-- by loading them from the result register.
compileBinary
    :: MonadCodeGen m
    => String
    -> Type
    -> Type
    -> (Operand -> Operand -> IRBuilderT m ())
    -> m ClosurePtr
compileBinary name lTy rTy builder =
    compileCurried name [(lTy, False), (rTy, False)] $ \[x,y] -> builder x y

-------------------------------------------------------------------------------

-- | `compileMin` @x y@ generates code which returns the lesser of
-- @x@ and @y@ where both are assumed to be signed integers.
compileMin
    :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> Operand -> m Operand
compileMin x y = do
    b <- icmp LLVM.SLT x y
    select b x y

-------------------------------------------------------------------------------

compileBinaryInteger
    :: forall a m. (MonadCodeGen m, CompileConstant a)
    => String -> (Operand -> Operand -> IRBuilderT m Operand)
    -> m ClosurePtr
compileBinaryInteger name builder = compileBinary name i64 i64 $ \x y -> do
    builder x y >>= retConstDynamic @a

addInteger :: MonadCodeGen m => m ClosurePtr
addInteger = compileBinaryInteger @Integer "addInteger" add

subtractInteger :: MonadCodeGen m => m ClosurePtr
subtractInteger = compileBinaryInteger @Integer "subtractInteger" sub

multiplyInteger :: MonadCodeGen m => m ClosurePtr
multiplyInteger = compileBinaryInteger @Integer "multiplyInteger" mul


equalsInteger :: MonadCodeGen m => m ClosurePtr
equalsInteger =
    compileBinaryInteger @Bool "equalsInteger" $ icmp LLVM.EQ

lessThanInteger :: MonadCodeGen m => m ClosurePtr
lessThanInteger =
    compileBinaryInteger @Bool "lessThanInteger" $ icmp LLVM.SLT

lessThanEqualsInteger :: MonadCodeGen m => m ClosurePtr
lessThanEqualsInteger =
    compileBinaryInteger @Bool "lessThanEqualsInteger" $ icmp LLVM.SLE

-------------------------------------------------------------------------------

-- | `bsNewStruct` @size@ generates code which allocates enough space for a new
-- bytestring structure and stores @size@ in it. The pointer to the new
-- structure is returned. The data pointer of the bytestring is not set
-- and no memory is allocated for the data.
bsNewStruct :: (MonadCodeGen m, MonadIRBuilder m) => Operand -> m Operand
bsNewStruct l = do
    -- calculate the size of the bytestring structure and allocate memory
    -- for it
    size <- IR.sizeof 64 bytestringTy
    ptr <- E.malloc size
    tptr <- bitcast ptr bytestringTyPtr

    -- store the length
    store tptr 0 l

    -- return the pointer to the bytestring structure
    pure tptr

-- | `bsNew` @size@ generates code which allocates enough space for a new
-- bytestring with @size@-many elements. The actual memory allocated is
-- greater than @size@, since we need to store the size in the bytestring
-- as well. This function performs the latter task as well before returning
-- the pointer to the new bytestring.
bsNew :: (MonadCodeGen m, MonadIRBuilder m) => Operand -> m Operand
bsNew l = do
    -- allocate the byte array that will store the actual data
    arrPtr <- E.malloc l

    -- create the new bytestring structure
    tptr <- bsNewStruct l

    -- store the pointer to the byte array
    dataAddr <- gep tptr [ ConstantOperand $ Int 32 0
                         , ConstantOperand $ Int 32 1
                         ]
    store dataAddr 0 arrPtr

    -- return the pointer to the bytestring structure
    pure tptr

-- | `bsLen` @ptr@ generates code which loads the length of a bytestring
-- which is represented by @ptr@.
bsLen :: (MonadCodeGen m, MonadIRBuilder m) => Operand -> m Operand
bsLen str = do
    addr <- gep str [ConstantOperand $ Int 32 0, ConstantOperand $ Int 32 0]
    load addr 0

-- | `bsDataPtr` @ptr@ generates code which calculates the address of the
-- data component of a bytestring represented by @ptr@.
bsDataPtr :: (MonadCodeGen m, MonadIRBuilder m) => Operand -> m Operand
bsDataPtr str = do
    addr <- gep str [ConstantOperand $ Int 32 0, ConstantOperand $ Int 32 1]
    load addr 0

appendByteString :: MonadCodeGen m => m ClosurePtr
appendByteString =
    compileBinary "appendByteString" bytestringTyPtr bytestringTyPtr $
    \s0 s1 -> do
        l0 <- bsLen s0
        l1 <- bsLen s1

        -- allocate a new bytestring which is big enough to store the data
        -- of both the existing bytestrings combined
        size <- add l0 l1
        ptr <- bsNew size

        -- copy data
        addr <- bsDataPtr ptr
        srcAddr <- bsDataPtr s0
        _ <- E.memcpy addr srcAddr l0

        addr1 <- add addr l0
        srcAddr1 <- bsDataPtr s1
        _ <- E.memcpy addr1 srcAddr1 l1

        -- allocate a new closure
        retConstDynamic @BS.ByteString ptr

consByteString :: MonadCodeGen m => m ClosurePtr
consByteString =
    compileBinary "consByteString" i64 bytestringTyPtr $
    \x xs -> do
        l <- bsLen xs

        -- allocate a new bytestring which is big enough to store the data
        -- of the existing bytestring + 1
        size <- add l (ConstantOperand $ Int 64 1)
        ptr <- bsNew size

        -- copy data
        addr <- bsDataPtr ptr
        xt <- bitcast x i8
        store addr 0 xt

        addr1 <- add addr (ConstantOperand $ Int 64 1)
        srcAddr1 <- bsDataPtr xs
        _ <- E.memcpy addr1 srcAddr1 l

        -- allocate a new closure
        retConstDynamic @BS.ByteString ptr

sliceByteString :: MonadCodeGen m => m ClosurePtr
sliceByteString =
    let ps = [("s",False),("n",False),("str",False)]
    in withCurried "sliceByteString" ps $ \[sv,nv,strv] -> do
    -- enter all three arguments and load the resulting values
    _ <- enterClosure (MkClosurePtr sv) []
    s <- loadConstVal i64

    _ <- enterClosure (MkClosurePtr nv) []
    n <- loadConstVal i64

    _ <- enterClosure (MkClosurePtr strv) []
    str <- loadConstVal bytestringTyPtr

    -- retrieve the length of the existing bytestring
    strLen <- bsLen str

    -- calculate the start offset, which is the minimum of the given start
    -- index and the length of the existing bytestring - i.e. we can't
    -- choose an index greater than the length of the existing bytestring
    start <- compileMin s strLen

    -- we need the size of the new bytestring
    remLen <- sub strLen start
    newLen <- compileMin remLen n

    -- create the new bytestring structure, but do not allocate a new
    -- array backing it, so that we can reuse the existing one
    ptr <- bsNewStruct newLen

    -- calculate the address we want to use as the data pointer; this is the
    -- data pointer of the existing bytestring + the start offset
    strData <- bsDataPtr str
    startAddr <- add strData start

    -- store the pointer to the byte array
    dataAddr <- gep ptr [ ConstantOperand $ Int 32 0
                        , ConstantOperand $ Int 32 1
                        ]
    store dataAddr 0 startAddr

    -- allocate a new closure
    retConstDynamic @BS.ByteString ptr

lengthOfByteString :: MonadCodeGen m => m ClosurePtr
lengthOfByteString =
    withCurried "lengthOfByteString" [("str",False)] $ \[str] -> do
    -- enter the constant closure and load the pointer from the result register
    _ <- enterClosure (MkClosurePtr str) []
    ptr <- loadConstVal bytestringTyPtr

    -- the size is stored in the bytestring structure, so we just need to
    -- retrieve it from there
    val <- bsLen ptr

    -- allocate a new closure for the size value
    retConstDynamic @Integer val

indexByteString :: MonadCodeGen m => m ClosurePtr
indexByteString =
    compileBinary "indexByteString" bytestringTyPtr i64 $
    \str n -> E.indexBytestring str n >>= retConstDynamic @Integer

equalsByteString :: MonadCodeGen m => m ClosurePtr
equalsByteString =
    compileBinary "equalsByteString" bytestringTyPtr bytestringTyPtr $
    \s0 s1 -> E.equalsByteString s0 s1 >>= retConstDynamic @Bool

lessThanByteString :: MonadCodeGen m => m ClosurePtr
lessThanByteString =
    compileBinary "lessThanByteString" bytestringTyPtr bytestringTyPtr $
    \s0 s1 -> E.lessThanByteString s0 s1 >>= retConstDynamic @Bool

lessThanEqualsByteString :: MonadCodeGen m => m ClosurePtr
lessThanEqualsByteString =
    compileBinary "lessThanEqualsByteString" bytestringTyPtr bytestringTyPtr $
    \s0 s1 -> E.lessThanEqualsByteString s0 s1 >>= retConstDynamic @Bool

-------------------------------------------------------------------------------

sha256 :: MonadCodeGen m => m ClosurePtr
sha256 =
    compileCurried "sha2_256" [(bytestringTyPtr, False)] $ \[str] -> do
    -- calculate the SHA256 hash of the bytestring
    r <- E.sha256 str

    -- allocate a new bytestring with space for 256 bits
    ptr <- bsNew (ConstantOperand $ Int 64 32)

    -- store the pointer to the byte array
    dataAddr <- gep ptr [ ConstantOperand $ Int 32 0
                        , ConstantOperand $ Int 32 1
                        ]
    store dataAddr 0 r

    -- create a new dynamic closure for the new bytestring
    retConstDynamic @BS.ByteString ptr

blake2b :: MonadCodeGen m => m ClosurePtr
blake2b =
    compileCurried "blake2b_256" [(bytestringTyPtr, False)] $ \[str] -> do
    -- calculate the blake2b-256 hash of the bytestring
    r <- E.blake2b str

    -- allocate a new bytestring with space for 256 bits
    ptr <- bsNew (ConstantOperand $ Int 64 32)

    -- store the pointer to the byte array
    dataAddr <- gep ptr [ ConstantOperand $ Int 32 0
                        , ConstantOperand $ Int 32 1
                        ]
    store dataAddr 0 r

    -- create a new dynamic closure for the new bytestring
    retConstDynamic @BS.ByteString ptr

verifySignature :: MonadCodeGen m => m ClosurePtr
verifySignature =
    let pty = (bytestringTyPtr, False)
    in compileCurried "verifySignature" [pty,pty,pty] $
    \[pubKey,message,signature] ->
        E.verifySig pubKey message signature >>=
        compileConstDynamic @Bool >>=
        retClosure

-------------------------------------------------------------------------------

ifThenElse :: MonadCodeGen m => m ClosurePtr
ifThenElse =
    let ps = [("s",True),("c",False),("t",False),("f",False)]
    in withCurried "ifThenElse" ps $ \[_,cv,tv,fv] -> do
    -- enter the closure for the condition, this should be some expression
    -- that results in a boolean value, which will then be stored in the
    -- result register
    _ <- enterClosure (MkClosurePtr cv) []
    c <- loadConstVal i1

    -- compare the resulting value to 0, which represents false, while all
    -- other values represent true; accordingly choose either the pointer
    -- to the closure representing the true branch or the false branch and
    -- return that as the result of this function
    cr <- icmp LLVM.NE c (ConstantOperand $ Int 1 0)
    ptr <- select cr tv fv

    retClosure $ MkClosurePtr ptr

-------------------------------------------------------------------------------

chooseUnit :: MonadCodeGen m => m ClosurePtr
chooseUnit =
    let ps = mkParams 1 ["v","k"]
    in withCurried "chooseUnit" ps $ \[_,v,k] -> do
        _ <- enterClosure (MkClosurePtr v) []
        retClosure $ MkClosurePtr k

-------------------------------------------------------------------------------

fstPair :: MonadCodeGen m => m ClosurePtr
fstPair =
    let ps = mkParams 2 [pairTyPtr]
    in compileCurried "fstPair" ps $ \[ptr] -> getFst ptr >>= retClosure

sndPair :: MonadCodeGen m => m ClosurePtr
sndPair =
    let ps = mkParams 2 [pairTyPtr]
    in compileCurried "sndPair" ps $ \[ptr] -> getSnd ptr >>= retClosure

-------------------------------------------------------------------------------

chooseList :: MonadCodeGen m => m ClosurePtr
chooseList =
    let ps = mkParams 2 ["xs", "a", "b"]
    in withCurried "chooseList" ps $ \[_,_,list,a,b] -> do
        -- enter the closure for the list and get the pointer to it
        _ <- enterClosure (MkClosurePtr list) []
        xs <- loadConstVal listTyPtr

        -- return either a or b depending on whether the list is empty or not,
        -- i.e. whether it is a null pointer or not
        r <- icmp LLVM.EQ xs (ConstantOperand $ Null listTyPtr)
        ptr <- select r a b
        retClosure $ MkClosurePtr ptr

mkCons :: MonadCodeGen m => m ClosurePtr
mkCons =
    let ps = mkParams 1 ["x", "xs"]
    in withCurried "mkCons" ps $ \[_,x,xs] -> do
        ptr <- listNew x xs

        -- the element type only matters for `compileConstant` so we can
        -- safely just set it to `()` here and it will work regardless of the
        -- actual element type
        retConstDynamic @[()] ptr

headList :: MonadCodeGen m => m ClosurePtr
headList =
    let ps = mkParams 1 [listTyPtr]
    in compileCurried "headList" ps $ \[xs] -> do
        let nullCode = do
                void $ E.printf headErrRef []
                void $ E.exit (-1)
                ret $ ConstantOperand $ Null closureTyPtr

        listCase xs nullCode $ getHead xs >>= retClosure

tailList :: MonadCodeGen m => m ClosurePtr
tailList =
    let ps = mkParams 1 [listTyPtr]
    in compileCurried "tailList" ps $ \[xs] -> do
        let nullCode = do
                void $ E.printf tailErrRef []
                void $ E.exit (-1)
                ret $ ConstantOperand $ Null closureTyPtr

        listCase xs nullCode $ getTail xs >>= retClosure

nullList :: MonadCodeGen m => m ClosurePtr
nullList =
    let ps = mkParams 1 [listTyPtr]
    in compileCurried "nullList" ps $ \[xs] -> do
        b <- icmp LLVM.EQ xs (ConstantOperand $ Null listTyPtr)
        retConstDynamic @Bool b

-------------------------------------------------------------------------------

chooseData :: MonadCodeGen m => m ClosurePtr
chooseData =
    let ps = mkParams 1 ["d","constr","map","list","i","b"]
    in withCurried "chooseData" ps $ \[_,d,kConstr,kMap,kList,kI,kB] -> do
        -- enter the closure for the data value and get the pointer to it
        _ <- enterClosure (MkClosurePtr d) []
        ptr <- loadConstVal dataTyPtr

        withDataTag ptr $ \case
            DataConstr -> ret kConstr
            DataMap -> ret kMap
            DataList -> ret kList
            DataI -> ret kI
            DataB -> ret kB

-------------------------------------------------------------------------------

-- | `builtins` is a mapping from built-in function tags to code generators
-- for them. These are used by `compileBuiltins` to generate the code for each
-- supported built-in function.
builtins :: MonadCodeGen m => [(DefaultFun, m ClosurePtr)]
builtins =
    [ (AddInteger, addInteger)
    , (SubtractInteger, subtractInteger)
    , (MultiplyInteger, multiplyInteger)
    , (EqualsInteger, equalsInteger)
    , (LessThanInteger, lessThanInteger)
    , (LessThanEqualsInteger, lessThanEqualsInteger)
    -- Bytestrings
    , (AppendByteString, appendByteString)
    , (ConsByteString, consByteString)
    , (SliceByteString, sliceByteString)
    , (LengthOfByteString, lengthOfByteString)
    , (IndexByteString, indexByteString)
    , (EqualsByteString, equalsByteString)
    , (LessThanByteString, lessThanByteString)
    , (LessThanEqualsByteString, lessThanEqualsByteString)
    -- Cryptography and hashes
    , (Sha2_256, sha256)
    , (Blake2b_256, blake2b)
    , (VerifySignature, verifySignature)
    -- Booleans
    , (IfThenElse, ifThenElse)
    -- Unit
    , (ChooseUnit, chooseUnit)
    -- Pairs
    , (FstPair, fstPair)
    , (SndPair, sndPair)
    -- Lists
    , (ChooseList, chooseList)
    , (MkCons, mkCons)
    , (HeadList, headList)
    , (TailList, tailList)
    , (NullList, nullList)
    -- Data
    , (ChooseData, chooseData)
    ]

-- | `compileBuiltins` is a computation which generates code for all the
-- built-in functions and returns a mapping from their tags to the
-- corresponding function pointers.
compileBuiltins :: MonadCodeGen m => m (M.Map DefaultFun ClosurePtr)
compileBuiltins = fmap M.fromList $ forM builtins $ \(f, compile) -> do
    ref <- compile
    pure (f, ref)

-------------------------------------------------------------------------------
