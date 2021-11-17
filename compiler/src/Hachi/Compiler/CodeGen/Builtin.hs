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

import PlutusCore.Data (Data)
import PlutusCore.Default

import LLVM.AST
import LLVM.AST.Constant
import LLVM.AST.IntegerPredicate as LLVM
import LLVM.IRBuilder as IR

import Hachi.Compiler.CodeGen.Closure
import Hachi.Compiler.CodeGen.Common
import Hachi.Compiler.CodeGen.Constant
import Hachi.Compiler.CodeGen.Constant.Data
import Hachi.Compiler.CodeGen.Constant.Integer
import Hachi.Compiler.CodeGen.Constant.List
import Hachi.Compiler.CodeGen.Constant.Pair
import Hachi.Compiler.CodeGen.Equality
import Hachi.Compiler.CodeGen.Globals
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types
import qualified Hachi.Compiler.CodeGen.Externals as E
import Hachi.Compiler.Platform

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
    -> m (ClosurePtr 'StaticPtr)
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

    printPtr <- compileFunPrint

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
    -> m (ClosurePtr 'StaticPtr)
compileCurried name tys builder =
    let params = [ (T.pack $ 'v' : show i, v) | ((_,v),i) <- zip tys [0..]]
    in withCurried name params $ \argv -> do
        -- do not enter type variable arguments or bad things may happen
        let nonTyVars = filter (not . snd . snd) $ zip argv tys
        vars <- forM nonTyVars $ \(arg, ty) -> do
            _ <- enterClosure (MkClosurePtr arg) Nothing
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
    -> m (ClosurePtr 'StaticPtr)
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

-- | `compileBinaryInteger` @name builder@ generates a built-in function
-- named @name@ with two integer parameters whose body is generated
-- using @builder@.
compileBinaryInteger
    :: forall a m. (MonadCodeGen m, CompileConstant a)
    => String -> (Operand -> Operand -> IRBuilderT m Operand)
    -> m (ClosurePtr 'StaticPtr)
compileBinaryInteger name builder =
    compileBinary name gmpTyPtr gmpTyPtr $ \x y ->
    builder x y >>= retConstDynamic @a

-- | `compileBinaryNewInteger` @name builder@ generates a built-in function
-- named @name@ with two integer parameters whose body is generated using
-- @builder@ and which returns a new integer as result.
compileBinaryNewInteger
    :: forall a m. (MonadCodeGen m, CompileConstant a)
    => String -> (Operand -> Operand -> Operand -> IRBuilderT m ())
    -> m (ClosurePtr 'StaticPtr)
compileBinaryNewInteger name builder =
    compileBinaryInteger @a name $ \x y -> do
    ptr <- newInteger
    builder ptr x y
    pure ptr

addInteger :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
addInteger = compileBinaryNewInteger @Integer "addInteger" E.mpzAdd

subtractInteger :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
subtractInteger = compileBinaryNewInteger @Integer "subtractInteger" E.mpzSub

multiplyInteger :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
multiplyInteger = compileBinaryNewInteger @Integer "multiplyInteger" E.mpzMul

divideInteger :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
divideInteger = compileBinaryNewInteger @Integer "divideInteger" E.mpzFDivQ

quotientInteger :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
quotientInteger = compileBinaryNewInteger @Integer "quotientInteger" E.mpzTDivQ

remainderInteger :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
remainderInteger =
    compileBinaryNewInteger @Integer "remainderInteger" E.mpzTDivR

modInteger :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
modInteger = compileBinaryNewInteger @Integer "modInteger" E.mpzFDivR

equalsInteger :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
equalsInteger =
    compileBinaryInteger @Bool "equalsInteger" $ \x y -> do
    r <- E.mpzCmp x y
    icmp LLVM.EQ r $ ConstantOperand $ Int platformIntSize 0

lessThanInteger :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
lessThanInteger =
    compileBinaryInteger @Bool "lessThanInteger" $ \x y -> do
    r <- E.mpzCmp x y
    icmp LLVM.SLT r $ ConstantOperand $ Int platformIntSize 0

lessThanEqualsInteger :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
lessThanEqualsInteger =
    compileBinaryInteger @Bool "lessThanEqualsInteger" $ \x y -> do
    r <- E.mpzCmp x y
    icmp LLVM.SLE r $ ConstantOperand $ Int platformIntSize 0

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
    ptr <- E.malloc bytestringTyPtr size

    -- store the length
    addr <- gep ptr [ConstantOperand $ Int 32 0, ConstantOperand $ Int 32 0]
    store addr 0 l

    -- return the pointer to the bytestring structure
    pure ptr

-- | `bsNew` @size@ generates code which allocates enough space for a new
-- bytestring with @size@-many elements. The actual memory allocated is
-- greater than @size@, since we need to store the size in the bytestring
-- as well. This function performs the latter task as well before returning
-- the pointer to the new bytestring.
bsNew :: (MonadCodeGen m, MonadIRBuilder m) => Operand -> m Operand
bsNew l = do
    -- allocate the byte array that will store the actual data
    arrPtr <- E.malloc (ptrOf i8) l

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

appendByteString :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
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

        addr1 <- gep addr [l0]
        srcAddr1 <- bsDataPtr s1
        _ <- E.memcpy addr1 srcAddr1 l1

        -- allocate a new closure
        retConstDynamic @BS.ByteString ptr

consByteString :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
consByteString =
    compileBinary "consByteString" gmpTyPtr bytestringTyPtr $
    \xp xs -> do
        -- obtain the current length of the bystring and convert the arbitrary
        -- precision integer to an unsigned long int
        l <- bsLen xs
        x <- E.mpzGetUInt xp

        -- allocate a new bytestring which is big enough to store the data
        -- of the existing bytestring + 1
        size <- add l (ConstantOperand $ Int 64 1)
        ptr <- bsNew size

        -- copy data
        addr <- bsDataPtr ptr
        xt <- trunc x i8
        store addr 0 xt

        addr1 <- gep addr [ConstantOperand $ Int 64 1]
        srcAddr1 <- bsDataPtr xs
        _ <- E.memcpy addr1 srcAddr1 l

        -- allocate a new closure
        retConstDynamic @BS.ByteString ptr

sliceByteString :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
sliceByteString =
    let ps = mkParams 0 [gmpTyPtr,gmpTyPtr,bytestringTyPtr]
    in compileCurried "sliceByteString" ps $ \[sp,np,str] -> do
    -- retrieve the length of the existing bytestring
    strLen <- bsLen str

    -- convert the arbitrary precision integers to unsigned long ints
    s <- E.mpzGetUInt sp
    n <- E.mpzGetUInt np

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
    startAddr <- gep strData [start]

    -- store the pointer to the byte array
    dataAddr <- gep ptr [ ConstantOperand $ Int 32 0
                        , ConstantOperand $ Int 32 1
                        ]
    store dataAddr 0 startAddr

    -- allocate a new closure
    retConstDynamic @BS.ByteString ptr

lengthOfByteString :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
lengthOfByteString =
    withCurried "lengthOfByteString" [("str",False)] $ \[str] -> do
    -- enter the constant closure and load the pointer from the result register
    _ <- enterClosure (MkClosurePtr str) Nothing
    ptr <- loadConstVal bytestringTyPtr

    -- the size is stored in the bytestring structure, so we just need to
    -- retrieve it from there
    val <- bsLen ptr

    -- initialise an integer
    int <- newInteger
    E.mpzInitSetUInt int val

    -- allocate a new closure for the size value
    retConstDynamic @Integer int

indexByteString :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
indexByteString =
    compileBinary "indexByteString" bytestringTyPtr gmpTyPtr $
    \str n -> do
        -- conver the arbitrary precision integer to an unsigned long int
        ix <- E.mpzGetUInt n

        -- index into the bytestring to retrieve the character at the
        -- given index
        c <- E.indexBytestring str ix
        val <- zext c i64

        -- we only have arbitrary precision integers, so we allocate a new one
        -- as the result to store the character
        int <- newInteger
        E.mpzInitSetUInt int val
        retConstDynamic @Integer int

equalsByteString :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
equalsByteString =
    compileBinary "equalsByteString" bytestringTyPtr bytestringTyPtr $
    \s0 s1 -> E.equalsByteString s0 s1 >>= retConstDynamic @Bool

lessThanByteString :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
lessThanByteString =
    compileBinary "lessThanByteString" bytestringTyPtr bytestringTyPtr $
    \s0 s1 -> E.lessThanByteString s0 s1 >>= retConstDynamic @Bool

lessThanEqualsByteString :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
lessThanEqualsByteString =
    compileBinary "lessThanEqualsByteString" bytestringTyPtr bytestringTyPtr $
    \s0 s1 -> E.lessThanEqualsByteString s0 s1 >>= retConstDynamic @Bool

-------------------------------------------------------------------------------

sha256 :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
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

blake2b :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
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

verifySignature :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
verifySignature =
    let pty = (bytestringTyPtr, False)
    in compileCurried "verifySignature" [pty,pty,pty] $
    \[pubKey,message,signature] ->
        E.verifySig pubKey message signature >>=
        compileConstDynamic @Bool >>=
        retClosure

-------------------------------------------------------------------------------

appendString :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
appendString =
    let ps = mkParams 0 [ptrOf i8, ptrOf i8]
    in compileCurried "appendString" ps $ \[xs, ys] -> do
        -- determine the lengths of the two strings
        l0 <- E.strlen xs
        l1 <- E.strlen ys

        -- allocate memory for the new string
        lb <- add l0 l1
        ln <- add lb (ConstantOperand $ Int 64 1)
        ptr <- E.malloc (ptrOf i8) ln

        -- copy the first string
        _ <- E.strcpy ptr xs

        -- copy the second string
        addr <- gep ptr [l0]
        _ <- E.strcpy addr ys

        retConstDynamic @T.Text ptr

equalsString :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
equalsString =
    let ps = mkParams 0 [ptrOf i8, ptrOf i8]
    in compileCurried "equalsString" ps $ \[xs, ys] -> do
        r <- E.strcmp xs ys
        b <- icmp LLVM.EQ r (ConstantOperand $ Int 32 0)
        retConstDynamic @Bool b

encodeUtf8 :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
encodeUtf8 =
    let ps = mkParams 0 [ptrOf i8]
    in compileCurried "encodeUtf8" ps $ \[str] -> do
        -- our strings are already UTF-8 so we just need to construct a
        -- bytestring for it
        l <- E.strlen str
        ptr <- bsNew l

        -- store the pointer to the byte array
        dataAddr <- gep ptr [ ConstantOperand $ Int 32 0
                            , ConstantOperand $ Int 32 1
                            ]
        store dataAddr 0 str

        retConstDynamic @BS.ByteString ptr

decodeUtf8 :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
decodeUtf8 =
    let ps = mkParams 0 [bytestringTyPtr]
    in compileCurried "decodeUtf8" ps $ \[bs] -> do
        -- assuming that the bytestring is already UTF-8, we don't have to do
        -- much since we just store UTF-8 strings internally anyway except
        -- allocate enough memory for the bytestring + 1, for the \0 char
        len <- bsLen bs
        addr <- bsDataPtr bs

        size <- add len $ ConstantOperand (Int 64 1)
        ptr <- E.malloc (ptrOf i8) size

        -- copy the actual string
        _ <- E.memcpy ptr addr len

        -- add the \0
        zeroAddr <- gep ptr [len]
        store zeroAddr 0 $ ConstantOperand (Int 8 0)

        -- return a new closure
        retConstDynamic @T.Text ptr

-------------------------------------------------------------------------------

ifThenElse :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
ifThenElse =
    let ps = [("s",True),("c",False),("t",False),("f",False)]
    in withCurried "ifThenElse" ps $ \[_,cv,tv,fv] -> do
    -- enter the closure for the condition, this should be some expression
    -- that results in a boolean value, which will then be stored in the
    -- result register
    _ <- enterClosure (MkClosurePtr cv) Nothing
    c <- loadConstVal i1

    -- compare the resulting value to 0, which represents false, while all
    -- other values represent true; accordingly choose either the pointer
    -- to the closure representing the true branch or the false branch and
    -- return that as the result of this function
    cr <- icmp LLVM.NE c (ConstantOperand $ Int 1 0)
    ptr <- select cr tv fv

    retClosure $ MkClosurePtr ptr

-------------------------------------------------------------------------------

chooseUnit :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
chooseUnit =
    let ps = mkParams 1 ["v","k"]
    in withCurried "chooseUnit" ps $ \[_,v,k] -> do
        _ <- enterClosure (MkClosurePtr v) Nothing
        retClosure $ MkClosurePtr k

-------------------------------------------------------------------------------

trace :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
trace =
    let ps = mkParams 1 ["text","a"]
    in withCurried "trace" ps $ \[_,txt,a] -> do
        -- this should be some string, so let's just call its pretty-printing
        -- function to render it
        _ <- callClosure ClosurePrint (MkClosurePtr txt) []
        -- then add a \n character and return the pointer to the other
        -- argument as the result of this built-in
        _ <- E.printf nlRef []
        ret a

-------------------------------------------------------------------------------

fstPair :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
fstPair =
    let ps = mkParams 2 [pairTyPtr]
    in compileCurried "fstPair" ps $ \[ptr] -> getFst ptr >>= retClosure

sndPair :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
sndPair =
    let ps = mkParams 2 [pairTyPtr]
    in compileCurried "sndPair" ps $ \[ptr] -> getSnd ptr >>= retClosure

-------------------------------------------------------------------------------

chooseList :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
chooseList =
    let ps = mkParams 2 ["xs", "a", "b"]
    in withCurried "chooseList" ps $ \[_,_,list,a,b] -> do
        -- enter the closure for the list and get the pointer to it
        _ <- enterClosure (MkClosurePtr list) Nothing
        xs <- loadConstVal listTyPtr

        -- return either a or b depending on whether the list is empty or not,
        -- i.e. whether it is a null pointer or not
        r <- icmp LLVM.EQ xs (ConstantOperand $ Null listTyPtr)
        ptr <- select r a b
        retClosure $ MkClosurePtr ptr

mkCons :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
mkCons =
    let ps = mkParams 1 ["x", "xs"]
    in withCurried "mkCons" ps $ \[_,x,xs] -> do
        ptr <- listNew x xs

        -- the element type only matters for `compileConstant` so we can
        -- safely just set it to `()` here and it will work regardless of the
        -- actual element type
        retConstDynamic @[()] ptr

headList :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
headList =
    let ps = mkParams 1 [listTyPtr]
    in compileCurried "headList" ps $ \[xs] -> do
        let nullCode = do
                void $ E.printf headErrRef []
                void $ E.exit (-1)
                ret $ ConstantOperand $ Null closureTyPtr

        listCase xs nullCode $ getHead xs >>= retClosure

tailList :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
tailList =
    let ps = mkParams 1 [listTyPtr]
    in compileCurried "tailList" ps $ \[xs] -> do
        let nullCode = do
                void $ E.printf tailErrRef []
                void $ E.exit (-1)
                ret $ ConstantOperand $ Null closureTyPtr

        listCase xs nullCode $ getTail xs >>= retClosure

nullList :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
nullList =
    let ps = mkParams 1 [listTyPtr]
    in compileCurried "nullList" ps $ \[xs] -> do
        b <- icmp LLVM.EQ xs (ConstantOperand $ Null listTyPtr)
        retConstDynamic @Bool b

-------------------------------------------------------------------------------

chooseData :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
chooseData =
    let ps = mkParams 1 ["d","constr","map","list","i","b"]
    in withCurried "chooseData" ps $ \[_,d,kConstr,kMap,kList,kI,kB] -> do
        -- enter the closure for the data value and get the pointer to it
        _ <- enterClosure (MkClosurePtr d) Nothing
        ptr <- loadConstVal dataTyPtr

        withDataTag ptr $ \case
            DataConstr -> ret kConstr
            DataMap -> ret kMap
            DataList -> ret kList
            DataI -> ret kI
            DataB -> ret kB

constrData :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
constrData =
    let ps = mkParams 0 ["ix", "xs"]
    in withCurried "constrData" ps $ \[ix,xs] ->
        -- instantiate the new Data value and construct a new, dynamic
        -- closure for it
        newData DataConstr xs (Just ix) >>= retConstDynamic @Data

mapData :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
mapData =
    let ps = mkParams 0 ["xs"]
    in withCurried "mapData" ps $ \[xs] -> do
        newData DataMap xs Nothing >>= retConstDynamic @Data


listData :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
listData =
    let ps = mkParams 0 ["xs"]
    in withCurried "listData" ps $ \[xs] -> do
        newData DataList xs Nothing >>= retConstDynamic @Data

iData :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
iData =
    let ps = mkParams 0 ["i"]
    in withCurried "iData" ps $ \[i] -> do
        newData DataI i Nothing >>= retConstDynamic @Data

bData :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
bData =
    let ps = mkParams 0 ["xs"]
    in withCurried "bData" ps $ \[xs] -> do
        newData DataB xs Nothing >>= retConstDynamic @Data

unConstrData :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
unConstrData =
    let ps = mkParams 0 [dataTyPtr]
    in compileCurried "unConstrData" ps $ \[d] ->
    withDataTag d $ \case
        DataConstr -> do
            -- retrieve the two components from the constructor
            ptr <- loadDataPtr d
            tag <- loadConstrTag d

            -- construct a new pair with them
            pair <- newPair tag ptr

            -- return a new closure for the pair
            retConstDynamic @((),()) pair
        _ -> fatal unConstrDataErrRef []

unMapData :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
unMapData =
    let ps = mkParams 0 [dataTyPtr]
    in compileCurried "unMapData" ps $ \[d] ->
    withDataTag d $ \case
        DataMap -> loadDataPtr d >>= retClosure
        _ -> fatal unMapDataErrRef []

unListData :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
unListData =
    let ps = mkParams 0 [dataTyPtr]
    in compileCurried "unListData" ps $ \[d] ->
    withDataTag d $ \case
        DataList -> loadDataPtr d >>= retClosure
        _ -> fatal unListDataErrRef []

unIData :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
unIData =
    let ps = mkParams 0 [dataTyPtr]
    in compileCurried "unIData" ps $ \[d] ->
    withDataTag d $ \case
        DataI -> loadDataPtr d >>= retClosure
        _ -> fatal unIDataErrRef []

unBData :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
unBData =
    let ps = mkParams 0 [dataTyPtr]
    in compileCurried "unBData" ps $ \[d] ->
    withDataTag d $ \case
        DataB -> loadDataPtr d >>= retClosure
        _ -> fatal unBDataErrRef []

equalsData :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
equalsData =
    let ps = mkParams 0 ["x","y"]
    in withCurried "equalsData" ps $ \[x,y] -> do
        cmpFun <- eqData

        -- call the data equality function and construct a new dynamic closure
        -- for the resulting boolean value
        r <- call cmpFun [(x, []), (y, [])]
        retConstDynamic @Bool r

mkPairData :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
mkPairData =
    let ps = mkParams 0 ["x","y"]
    in withCurried "mkPairData" ps $ \[x,y] ->
        newPair (MkClosurePtr x) (MkClosurePtr y) >>=
        retConstDynamic @(Data, Data)

mkNilData :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
mkNilData =
    let ps = mkParams 0 [i8]
    in compileCurried "mkNilData" ps $ \[_] ->
        retConstDynamic @[Data] $ ConstantOperand $ Null listTyPtr

mkNilPairData :: MonadCodeGen m => m (ClosurePtr 'StaticPtr)
mkNilPairData =
    let ps = mkParams 0 [i8]
    in compileCurried "mkNilPairData" ps $ \[_] ->
        retConstDynamic @[(Data,Data)] $ ConstantOperand $ Null listTyPtr

-------------------------------------------------------------------------------

-- | `builtins` is a mapping from built-in function tags to code generators
-- for them. These are used by `compileBuiltins` to generate the code for each
-- supported built-in function.
builtins :: MonadCodeGen m => [(DefaultFun, m (ClosurePtr 'StaticPtr))]
builtins =
    [ (AddInteger, addInteger)
    , (SubtractInteger, subtractInteger)
    , (MultiplyInteger, multiplyInteger)
    , (DivideInteger, divideInteger)
    , (QuotientInteger, quotientInteger)
    , (RemainderInteger, remainderInteger)
    , (ModInteger, modInteger)
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
    -- Strings
    , (AppendString, appendString)
    , (EqualsString, equalsString)
    , (EncodeUtf8, encodeUtf8)
    , (DecodeUtf8, decodeUtf8)
    -- Booleans
    , (IfThenElse, ifThenElse)
    -- Unit
    , (ChooseUnit, chooseUnit)
    -- Trace
    , (Trace, trace)
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
    , (ConstrData, constrData)
    , (MapData, mapData)
    , (ListData, listData)
    , (IData, iData)
    , (BData, bData)
    , (UnConstrData, unConstrData)
    , (UnMapData, unMapData)
    , (UnListData, unListData)
    , (UnIData, unIData)
    , (UnBData, unBData)
    , (EqualsData, equalsData)
    -- Misc constructors
    , (MkPairData, mkPairData)
    , (MkNilData, mkNilData)
    , (MkNilPairData, mkNilPairData)
    ]

-- | `compileBuiltins` is a computation which generates code for all the
-- built-in functions and returns a mapping from their tags to the
-- corresponding function pointers.
compileBuiltins
    :: MonadCodeGen m
    => m (M.Map DefaultFun (ClosurePtr 'StaticPtr))
compileBuiltins = fmap M.fromList $ forM builtins $ \(f, compile) -> do
    ref <- compile
    pure (f, ref)

-------------------------------------------------------------------------------
