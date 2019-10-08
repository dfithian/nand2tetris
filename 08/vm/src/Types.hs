module Types where

import ClassyPrelude hiding (Index)
import Control.Lens.TH (makePrisms)

newtype Code = Code { unCode :: Text }
  deriving (Eq, Ord, Show)

newtype Index = Index { unIndex :: Int }
  deriving (Eq, Ord, Show)

newtype Literal = Literal { unLiteral :: Int }
  deriving (Eq, Ord, Show)

newtype Symbol = Symbol { unSymbol :: Text }
  deriving (Eq, Ord, Show)

data MemoryOperation
  = MemoryOperationPop
  | MemoryOperationPush
  deriving (Eq, Ord, Show)

data MemoryLocation
  = MemoryLocationArgument
  | MemoryLocationLocal
  | MemoryLocationStatic
  | MemoryLocationConstant
  | MemoryLocationThis
  | MemoryLocationThat
  | MemoryLocationPointer
  | MemoryLocationTemp
  deriving (Eq, Ord, Show)

data ArithmeticCommand
  = ArithmeticCommandAdd
  | ArithmeticCommandSub
  | ArithmeticCommandNeg
  | ArithmeticCommandEq
  | ArithmeticCommandGt
  | ArithmeticCommandLt
  | ArithmeticCommandAnd
  | ArithmeticCommandOr
  | ArithmeticCommandNot
  deriving (Eq, Ord, Show)

data MemoryCommand
  = MemoryCommand MemoryOperation MemoryLocation Index
  deriving (Eq, Ord, Show)

data ProgramCommand
  = ProgramCommandLabel Symbol
  | ProgramCommandGoto Symbol
  | ProgramCommandIfGoto Symbol
  deriving (Eq, Ord, Show)

data FunctionCommand
  = FunctionCommandDeclaration Symbol Index
  | FunctionCommandInvocation Symbol Index
  | FunctionCommandReturn
  deriving (Eq, Ord, Show)

data ParsedCommand
  = ParsedCommandArithmetic ArithmeticCommand
  | ParsedCommandMemory MemoryCommand
  | ParsedCommandProgram ProgramCommand
  | ParsedCommandFunction FunctionCommand
  deriving (Eq, Ord, Show)

makePrisms ''Code
makePrisms ''Index
makePrisms ''Literal
makePrisms ''Symbol
makePrisms ''ArithmeticCommand
makePrisms ''MemoryCommand
makePrisms ''MemoryLocation
makePrisms ''ProgramCommand
makePrisms ''FunctionCommand
makePrisms ''ParsedCommand
