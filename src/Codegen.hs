module Codegen where

import LLVM.AST

double :: Type
double = FloatingPointType DoubleFP

type SymbolTable = [(String, Operand)]

data CodegenState
  = CodegenState {
    currentBlock  :: Name
  , blocks        :: Map.Map Name BlockState
  , symtab        :: SymbolTable
  , blockCount    :: Int
  , count         :: Word
  , names         :: Names
} deriving Show

data BlockState
  = BlockState {
    idx :: Int
  , stack :: [Named Instruction]
  , term :: Maybe (Named Terminator)
}
