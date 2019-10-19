module Types where

import ClassyPrelude hiding (Index)
import Control.Lens.TH (makePrisms)
import Data.List.NonEmpty (NonEmpty)

newtype Code = Code { unCode :: Text }
  deriving (Eq, Ord, Show)

data Keyword
  = KeywordClass
  | KeywordConstructor
  | KeywordFunction
  | KeywordMethod
  | KeywordField
  | KeywordStatic
  | KeywordVar
  | KeywordInt
  | KeywordChar
  | KeywordBoolean
  | KeywordVoid
  | KeywordTrue
  | KeywordFalse
  | KeywordNull
  | KeywordThis
  | KeywordLet
  | KeywordDo
  | KeywordIf
  | KeywordElse
  | KeywordWhile
  | KeywordReturn
  deriving (Eq, Ord, Show)

data Symbol
  = SymbolOpenCurly
  | SymbolCloseCurly
  | SymbolOpenParen
  | SymbolCloseParen
  | SymbolOpenBracket
  | SymbolCloseBracket
  | SymbolDot
  | SymbolComma
  | SymbolSemicolon
  | SymbolPlus
  | SymbolDash
  | SymbolStar
  | SymbolSlash
  | SymbolAnd
  | SymbolOr
  | SymbolLt
  | SymbolGt
  | SymbolEq
  | SymbolTilde
  deriving (Eq, Ord, Show)

newtype IntConstant = IntConstant { unIntConstant :: Int }
  deriving (Eq, Ord, Show)

newtype StringConstant = StringConstant { unStringConstant :: Text }
  deriving (Eq, Ord, Show)

newtype Identifier = Identifier { unIdentifier :: Text }
  deriving (Eq, Ord, Show)

data Token
  = TokenKeyword Keyword
  | TokenSymbol Symbol
  | TokenInt IntConstant
  | TokenString StringConstant
  | TokenIdentifier Identifier
  deriving (Eq, Ord, Show)

data Class = Class ClassName [ClassVarDec] [SubroutineDec]
  deriving (Eq, Ord, Show)

data ClassVarDec = ClassVarDec ClassVarDecType FieldType (NonEmpty VarName)
  deriving (Eq, Ord, Show)

data ClassVarDecType = ClassVarDecTypeStatic | ClassVarDecTypeField
  deriving (Eq, Ord, Show)

data SubroutineDec = SubroutineDec SubroutineDecType FieldTypePlus SubroutineName [(FieldType, VarName)] [VarDec] [Statement]
  deriving (Eq, Ord, Show)

data SubroutineDecType = SubroutineDecTypeConstructor | SubroutineDecTypeFunction | SubroutineDecTypeMethod
  deriving (Eq, Ord, Show)

data FieldType = FieldTypeInt | FieldTypeChar | FieldTypeBool | FieldTypeClass ClassName
  deriving (Eq, Ord, Show)

data FieldTypePlus = FieldTypePlusVoid | FieldTypePlusReg FieldType
  deriving (Eq, Ord, Show)

data VarDec = VarDec FieldType (NonEmpty VarName)
  deriving (Eq, Ord, Show)

newtype ClassName = ClassName Identifier
  deriving (Eq, Ord, Show)

newtype ClassOrVarName = ClassOrVarName Identifier
  deriving (Eq, Ord, Show)

newtype SubroutineName = SubroutineName Identifier
  deriving (Eq, Ord, Show)

newtype VarName = VarName Identifier
  deriving (Eq, Ord, Show)

data Statement
  = StatementLet VarName (Maybe Expression) Expression
  | StatementIf Expression [Statement] [Statement]
  | StatementWhile Expression [Statement]
  | StatementDo SubroutineCall
  | StatementReturn (Maybe Expression)
  deriving (Eq, Ord, Show)

data Expression = Expression Term [(Op, Term)]
  deriving (Eq, Ord, Show)

data Term
  = TermInt IntConstant
  | TermString StringConstant
  | TermKeyword KeywordConstant
  | TermVar VarName
  | TermVarExpression VarName Expression
  | TermSubroutine SubroutineCall
  | TermExpression Expression
  | TermUnaryOp UnaryOp Term
  deriving (Eq, Ord, Show)

data SubroutineCall = SubroutineCall (Maybe ClassOrVarName) SubroutineName [Expression]
  deriving (Eq, Ord, Show)

data Op = OpPlus | OpDash | OpStar | OpSlash | OpAnd | OpOr | OpLt | OpGt | OpEq
  deriving (Eq, Ord, Show)

data UnaryOp = UnaryOpDash | UnaryOpTilde
  deriving (Eq, Ord, Show)

data KeywordConstant = KeywordConstantTrue | KeywordConstantFalse | KeywordConstantNull | KeywordConstantThis
  deriving (Eq, Ord, Show)

newtype Index = Index { unIndex :: Int }
  deriving (Eq, Ord, Show)

newtype Literal = Literal { unLiteral :: Int }
  deriving (Eq, Ord, Show)

newtype Sym = Sym { unSym :: Text }
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
  = ProgramCommandLabel Sym
  | ProgramCommandGoto Sym
  | ProgramCommandIfGoto Sym
  deriving (Eq, Ord, Show)

data FunctionCommand
  = FunctionCommandDeclaration Sym Index
  | FunctionCommandInvocation Sym Index
  | FunctionCommandReturn
  deriving (Eq, Ord, Show)

data Command
  = CommandArithmetic ArithmeticCommand
  | CommandMemory MemoryCommand
  | CommandProgram ProgramCommand
  | CommandFunction FunctionCommand
  deriving (Eq, Ord, Show)

unVarName :: VarName -> Identifier
unVarName (VarName x) = x

makePrisms ''Keyword
makePrisms ''Symbol
makePrisms ''IntConstant
makePrisms ''StringConstant
makePrisms ''Identifier
makePrisms ''Token

makePrisms ''Class
makePrisms ''ClassVarDec
makePrisms ''ClassVarDecType
makePrisms ''SubroutineDec
makePrisms ''SubroutineDecType
makePrisms ''FieldType
makePrisms ''FieldTypePlus
makePrisms ''VarDec
makePrisms ''ClassName
makePrisms ''ClassOrVarName
makePrisms ''SubroutineName
makePrisms ''VarName
makePrisms ''Statement
makePrisms ''Expression
makePrisms ''Term
makePrisms ''SubroutineCall
makePrisms ''Op
makePrisms ''UnaryOp
makePrisms ''KeywordConstant

makePrisms ''Index
makePrisms ''Literal
makePrisms ''Sym
makePrisms ''ArithmeticCommand
makePrisms ''MemoryCommand
makePrisms ''MemoryLocation
makePrisms ''ProgramCommand
makePrisms ''FunctionCommand
makePrisms ''Command
