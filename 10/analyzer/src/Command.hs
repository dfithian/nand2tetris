module Command where

import ClassyPrelude

import qualified Types as T

showMemoryCommand :: T.MemoryCommand -> T.Code
showMemoryCommand (T.MemoryCommand op loc (T.Index i)) =
  let opStr = case op of
        T.MemoryOperationPush -> "push"
        T.MemoryOperationPop -> "pop"
      locStr = case loc of
        T.MemoryLocationArgument -> "argument"
        T.MemoryLocationLocal -> "local"
        T.MemoryLocationStatic -> "static"
        T.MemoryLocationConstant -> "constant"
        T.MemoryLocationThis -> "this"
        T.MemoryLocationThat -> "that"
        T.MemoryLocationPointer -> "pointer"
        T.MemoryLocationTemp -> "temp"
  in T.Code $ opStr <> " " <> locStr <> " " <> tshow i

showArithmeticCommand :: T.ArithmeticCommand -> T.Code
showArithmeticCommand = \ case
  T.ArithmeticCommandAdd -> T.Code "add"
  T.ArithmeticCommandSub -> T.Code "sub"
  T.ArithmeticCommandNeg -> T.Code "neg"
  T.ArithmeticCommandEq -> T.Code "eq"
  T.ArithmeticCommandGt -> T.Code "gt"
  T.ArithmeticCommandLt -> T.Code "lt"
  T.ArithmeticCommandAnd -> T.Code "and"
  T.ArithmeticCommandOr -> T.Code "or"
  T.ArithmeticCommandNot -> T.Code "not"

showFunctionCommand :: T.FunctionCommand -> T.Code
showFunctionCommand = \ case
  T.FunctionCommandDeclaration (T.Sym s) (T.Index i) -> T.Code $ "function " <> s <> " " <> tshow i
  T.FunctionCommandInvocation (T.Sym s) (T.Index i) -> T.Code $ "call " <> s <> " " <> tshow i
  T.FunctionCommandReturn -> T.Code "return"

showProgramCommand :: T.ProgramCommand -> T.Code
showProgramCommand = \ case
  T.ProgramCommandGoto (T.Sym s) -> T.Code $ "goto " <> s
  T.ProgramCommandIfGoto (T.Sym s) -> T.Code $ "if-goto " <> s
  T.ProgramCommandLabel (T.Sym s) -> T.Code $ "label " <> s

showCommand :: T.Command -> T.Code
showCommand = \ case
  T.CommandMemory c -> showMemoryCommand c
  T.CommandArithmetic c -> showArithmeticCommand c
  T.CommandFunction c -> showFunctionCommand c
  T.CommandProgram c -> showProgramCommand c

showCommands :: [T.Command] -> [T.Code]
showCommands = map showCommand
