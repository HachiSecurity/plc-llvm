{-# LANGUAGE AllowAmbiguousTypes #-}
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
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types
import qualified Hachi.Compiler.CodeGen.Externals as E

-------------------------------------------------------------------------------

-- | `withCurried` @name params bodyBuilder@ constructs a curried function
-- with @params@-many parameters, where the body of the inner-most function
-- is generated by @bodyBuilder@.
withCurried
    :: forall m. MonadCodeGen m
    => Bool
    -> String
    -> [T.Text]
    -> ([Operand] -> IRBuilderT m ClosurePtr)
    -> m ClosurePtr
withCurried _ _ [] _ = error "withCurried needs at least one argument"
withCurried isPoly name ps@(sn:dyn) builder = do
    let entryName = name <> "_static"

    -- this helper function generates code which creates dynamic closures for
    -- the functions that are returned to bind the 2nd argument onwards
    let mkCurry :: Int -> S.Set T.Text -> [T.Text]
                -> IRBuilderT m ClosurePtr
        mkCurry _ _ [] = do
            -- if there are no further parameters, retrieve all of the
            -- parameters from the current environment
            argv <- forM ps $ \pn -> lookupVar pn closureTyPtr

            -- then generate the body of the inner-most function
            builder argv
        mkCurry ix fvs (vn:vs) = do
            -- if we have another parameter, generate code which allocates a
            -- dynamic closure for the function that binds it
            let dynName = name <> "_dynamic_" <> show ix
            compileDynamicClosure False dynName fvs vn $ \_ arg -> do
                extendScope vn (MkClosurePtr arg) $
                    mkCurry (ix+1) (S.union (S.singleton vn) fvs) vs

    -- construct the static closure for this built-in function, which brings
    -- the first argument into scope and (if there is more than one parameter)
    -- returns a function that binds the next argument, and so on
    let staticParams = [(closureTyPtr, "this"), (closureTyPtr, mkParamName sn)]
    _ <- IR.function (mkName entryName) staticParams closureTyPtr $
        \[_, arg] -> extendScope sn (MkClosurePtr arg) $ do
            compileTrace entryName

            ptr <- mkCurry 0 (S.singleton sn) dyn
            ret $ closurePtr ptr

    let codePtr = GlobalReference (mkEntryTy 1) (mkName entryName)

    printPtr <- compileFunPrint name

    compileClosure isPoly name codePtr printPtr []

-- | `compileBinary` @name argType0 argType1 builder@ generates code for a
-- built-in function with two arguments of types @argType0@ and @argType1@.
-- The result function is curried and the body of the inner-most function is
-- produced by applying @builder@ to the `Operand`s representing the runtime
-- arguments.
compileBinary
    :: MonadCodeGen m
    => String
    -> Type
    -> Type
    -> (Operand -> Operand -> IRBuilderT m ClosurePtr)
    -> m ClosurePtr
compileBinary name lTy rTy builder =
    withCurried False name ["x", "y"] $ \[xv,yv] -> do
        _ <- enterClosure (MkClosurePtr xv) []
        x <- loadConstVal lTy

        _ <- enterClosure (MkClosurePtr yv) []
        y <- loadConstVal rTy

        builder x y

-------------------------------------------------------------------------------

compileBinaryInteger
    :: forall a m. (MonadCodeGen m, CompileConstant a)
    => String -> (Operand -> Operand -> IRBuilderT m Operand)
    -> m ClosurePtr
compileBinaryInteger name builder = compileBinary name i64 i64 $ \x y -> do
    builder x y >>= compileConstDynamic @a

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

-- | `bsNew` @size@ generates code which allocates enough space for a new
-- bytestring with @size@-many elements. The actual memory allocated is
-- greater than @size@, since we need to store the size in the bytestring
-- as well. This function performs the latter task as well before returning
-- the pointer to the new bytestring.
bsNew :: (MonadCodeGen m, MonadIRBuilder m) => Operand -> m Operand
bsNew l = do
    -- we need 8 extra bytes to store the length
    size <- add l (ConstantOperand $ Int 64 8)
    ptr <- E.malloc size
    tptr <- bitcast ptr bytestringTyPtr

    -- store length
    store tptr 0 l

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
bsDataPtr str =
    gep str [ConstantOperand $ Int 32 0, ConstantOperand $ Int 32 1]

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
        compileConstDynamic @BS.ByteString ptr

lengthOfByteString :: MonadCodeGen m => m ClosurePtr
lengthOfByteString = withCurried False "lengthOfByteString" ["str"] $ \[str] -> do
    -- enter the constant closure and load the pointer from the result register
    _ <- enterClosure (MkClosurePtr str) []
    ptr <- loadConstVal bytestringTyPtr

    -- the size is stored in the bytestring structure, so we just need to
    -- retrieve it from there
    val <- bsLen ptr

    -- allocate a new closure for the size value
    compileConstDynamic @Integer val

equalsByteString :: MonadCodeGen m => m ClosurePtr
equalsByteString =
    compileBinary "equalsByteString" bytestringTyPtr bytestringTyPtr $
    \s0 s1 -> E.equalsByteString s0 s1 >>= compileConstDynamic @Bool

lessThanByteString :: MonadCodeGen m => m ClosurePtr
lessThanByteString =
    compileBinary "lessThanByteString" bytestringTyPtr bytestringTyPtr $
    \s0 s1 -> E.lessThanByteString s0 s1 >>= compileConstDynamic @Bool

-------------------------------------------------------------------------------

ifThenElse :: MonadCodeGen m => m ClosurePtr
ifThenElse = withCurried True "ifThenElse" ["s","c","t","f"] $ \[_,cv,tv,fv] -> do
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

    pure $ MkClosurePtr ptr

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
    , (LengthOfByteString, lengthOfByteString)
    , (EqualsByteString, equalsByteString)
    , (LessThanByteString, lessThanByteString)
    -- Booleans
    , (IfThenElse, ifThenElse)
    ]

-- | `compileBuiltins` is a computation which generates code for all the
-- built-in functions and returns a mapping from their tags to the
-- corresponding function pointers.
compileBuiltins :: MonadCodeGen m => m (M.Map DefaultFun ClosurePtr)
compileBuiltins = fmap M.fromList $ forM builtins $ \(f, compile) -> do
    ref <- compile
    pure (f, ref)

-------------------------------------------------------------------------------
