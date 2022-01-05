module Hachi.Compiler.CodeGen.Constant.Integer (
    newInteger
) where

-------------------------------------------------------------------------------

import LLVM.AST
import LLVM.IRBuilder as IR

import qualified Hachi.Compiler.CodeGen.Externals as E
import Hachi.Compiler.CodeGen.Monad
import Hachi.Compiler.CodeGen.Types

-------------------------------------------------------------------------------

newInteger
    :: (MonadCodeGen m, MonadIRBuilder m)
    => m Operand
newInteger = do
    size <- IR.sizeof 64 gmpTyDef
    E.malloc gmpTyPtr size

-------------------------------------------------------------------------------
