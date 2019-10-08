module Code where

import ClassyPrelude
import Control.Lens (assign, modifying, use)
import Control.Lens.TH (makeLenses)
import Control.Monad.State (MonadState, evalStateT)

import qualified Types as T

data VMError = VMError Text
  deriving (Eq, Show)

instance Exception VMError

data VMState = VMState
  { _vmStateCommandNum         :: Int
  , _vmStateFileIdentifier     :: Text
  , _vmStateFunctionIdentifier :: [Text]
  , _vmStateReturnIndex        :: Int
  } deriving (Eq, Ord, Show)

data Location
  = LocationLiteral T.Literal
  | LocationSymbol T.Symbol T.Literal
  | LocationOffset T.Symbol T.Literal
  | LocationStatic T.Symbol
  deriving (Eq, Ord, Show)

makeLenses ''VMState

incCommandNum :: (MonadState VMState m) => m ()
incCommandNum = modifying vmStateCommandNum (+1)

getCommandNum :: (MonadState VMState m) => m Int
getCommandNum = use vmStateCommandNum

getFileIdentifier :: (MonadState VMState m) => m Text
getFileIdentifier = use vmStateFileIdentifier

setFunctionIdentifier :: (MonadState VMState m) => [Text] -> m ()
setFunctionIdentifier = assign vmStateFunctionIdentifier

getFunctionIdentifier :: (MonadState VMState m) => m [Text]
getFunctionIdentifier = use vmStateFunctionIdentifier

incReturnIndex :: (MonadState VMState m) => m ()
incReturnIndex = modifying vmStateReturnIndex (+1)

getReturnIndex :: (MonadState VMState m) => m Int
getReturnIndex = use vmStateReturnIndex

emptyVMState :: Text -> VMState
emptyVMState fileIdentifier = VMState 0 fileIdentifier [] 0

toCode :: [Text] -> [T.Code]
toCode = map T.Code

getLocation :: (MonadState VMState m) => (T.MemoryLocation, T.Index) -> m Location
getLocation (loc, T.Index i) = case loc of
  T.MemoryLocationArgument -> pure $ LocationSymbol arg (T.Literal i)
  T.MemoryLocationLocal    -> pure $ LocationSymbol lcl (T.Literal i)
  T.MemoryLocationStatic   -> do
    fileIdentifier <- getFileIdentifier
    pure $ LocationStatic (T.Symbol $ fileIdentifier <> "." <> tshow i)
  T.MemoryLocationConstant -> pure $ LocationLiteral (T.Literal i)
  T.MemoryLocationThis     -> pure $ LocationSymbol this (T.Literal i)
  T.MemoryLocationThat     -> pure $ LocationSymbol that (T.Literal i)
  T.MemoryLocationPointer  -> pure $ LocationOffset (T.Symbol "R3") (T.Literal i)
  T.MemoryLocationTemp     -> pure $ LocationOffset (T.Symbol "R5") (T.Literal i)

sp, r13, r7, r14, arg, lcl, this, that :: T.Symbol
sp = T.Symbol "SP"
r13 = T.Symbol "R13"
r7 = T.Symbol "R7"
r14 = T.Symbol "R14"
arg = T.Symbol "ARG"
lcl = T.Symbol "LCL"
this = T.Symbol "THIS"
that = T.Symbol "THAT"

incSym, decSym :: T.Symbol -> [T.Code]
incSym (T.Symbol s) = toCode ["@" <> s, "M=M+1"]
decSym (T.Symbol s) = toCode ["@" <> s, "M=M-1"]

incStack, decStack :: [T.Code]
incStack = incSym sp
decStack = decSym sp

loadC :: T.Literal -> [T.Code]
loadC (T.Literal i) = toCode ["@" <> tshow i, "D=A"]

shiftD :: T.Literal -> [T.Code]
shiftD (T.Literal i) = toCode ["@" <> tshow i, "D=D+A"]

shiftA :: T.Literal -> [T.Code]
shiftA (T.Literal i) = toCode ["@" <> tshow i, "A=D+A"]

shift_D :: T.Literal -> [T.Code]
shift_D (T.Literal i) = toCode ["@" <> tshow i, "D=D-A"]

shift_A :: T.Literal -> [T.Code]
shift_A (T.Literal i) = toCode ["@" <> tshow i, "A=D-A"]

loadD :: T.Symbol -> [T.Code]
loadD (T.Symbol s) = toCode ["@" <> s, "D=A"]

loadDM :: T.Symbol -> [T.Code]
loadDM (T.Symbol s) = toCode ["@" <> s, "D=M"]

loadAD :: T.Symbol -> [T.Code]
loadAD (T.Symbol s) = toCode ["@" <> s, "A=M", "D=M"]

loadA :: T.Symbol -> [T.Code]
loadA (T.Symbol s) = toCode ["@" <> s, "A=M"]

assignD :: T.Symbol -> [T.Code]
assignD (T.Symbol s) = toCode ["@" <> s] <> setMD

setMD :: [T.Code]
setMD = toCode ["M=D"]

setDM :: [T.Code]
setDM = toCode ["D=M"]

memoryCommand :: (MonadIO m, MonadState VMState m) => T.MemoryCommand -> m [T.Code]
memoryCommand = \ case
  T.MemoryCommand op loc ind -> do
    location <- getLocation (loc, ind)
    case op of
      T.MemoryOperationPush -> do
        resultInD <- case location of
          LocationLiteral l  -> pure $ loadC l
          LocationSymbol s l -> pure $ loadDM s <> shiftA l <> setDM
          LocationOffset s l -> pure $ loadD s <> shiftA l <> setDM
          LocationStatic s   -> pure $ loadDM s
        pure $ resultInD <> loadA sp <> setMD <> incStack
      T.MemoryOperationPop -> do
        addressInD <- case location of
          LocationLiteral l  -> throwIO . VMError $ "Tried to pop a literal " <> tshow l
          LocationSymbol s l -> pure $ loadDM s <> shiftD l
          LocationOffset s l -> pure $ loadD s <> shiftD l
          LocationStatic s   -> pure $ loadD s
        pure $ addressInD <> assignD r13 <> decStack <> loadAD sp <> loadA r13 <> setMD

arithmeticCommand :: (MonadState VMState m) => T.ArithmeticCommand -> m [T.Code]
arithmeticCommand cmd = do
  x <- getCommandNum
  let binaryOp o = decStack <> loadAD sp <> decStack <> loadA sp <> toCode ["M=M" <> o <> "D"] <> incStack
      unaryOp o = decStack <> loadAD sp <> toCode ["M=" <> o <> "D"] <> incStack
      jumpOp o = toCode
        [ "@TRUE" <> tshow x
        , "D;" <> o
        , "@SP"
        , "A=M"
        , "M=0"
        , "@END" <> tshow x
        , "0;JMP"
        , "(TRUE" <> tshow x <> ")"
        , "@SP"
        , "A=M"
        , "M=-1"
        , "(END" <> tshow x <> ")"
        ]
      compOp o = decStack <> loadAD sp <> decStack <> loadA sp <> toCode ["D=M-D"] <> jumpOp o <> incStack
  pure $ case cmd of
    T.ArithmeticCommandAdd -> binaryOp "+"
    T.ArithmeticCommandSub -> binaryOp "-"
    T.ArithmeticCommandNeg -> unaryOp "-"
    T.ArithmeticCommandEq  -> compOp "JEQ"
    T.ArithmeticCommandGt  -> compOp "JGT"
    T.ArithmeticCommandLt  -> compOp "JLT"
    T.ArithmeticCommandAnd -> binaryOp "&"
    T.ArithmeticCommandOr  -> binaryOp "|"
    T.ArithmeticCommandNot -> unaryOp "!"

getToLabel :: (MonadState VMState m) => m (T.Symbol -> Text)
getToLabel = do
  fileIdentifier <- getFileIdentifier
  functionIdentifier <- getFunctionIdentifier
  let prefix = intercalate "." $ fileIdentifier : functionIdentifier
  pure $ \ (T.Symbol x) -> prefix <> "." <> x

programCommand :: (MonadIO m, MonadState VMState m) => T.ProgramCommand -> m [T.Code]
programCommand cmd = do
  toLabel <- getToLabel
  case cmd of
    T.ProgramCommandLabel s -> pure $ toCode ["(" <> toLabel s <> ")"]
    T.ProgramCommandGoto s -> pure $ toCode ["@" <> toLabel s, "0;JMP"]
    T.ProgramCommandIfGoto s -> pure $ decStack <> loadAD sp <> toCode ["@" <> toLabel s, "D;JNE"]

functionCommand :: (MonadIO m, MonadState VMState m) => T.FunctionCommand -> m [T.Code]
functionCommand = \ case
  T.FunctionCommandDeclaration s (T.Index i) -> do
    label <- ($ s) <$> getToLabel
    let decLabel = toCode ["(" <> label <> ")"]
    pushes <-  map mconcat . replicateM i . memoryCommand $ T.MemoryCommand T.MemoryOperationPush T.MemoryLocationConstant (T.Index 0)
    pure $ decLabel <> pushes
  T.FunctionCommandInvocation s@(T.Symbol name) (T.Index i) -> do
    lastFunctionIdentifier <- getFunctionIdentifier
    let functionIdentifier = name : lastFunctionIdentifier
    setFunctionIdentifier functionIdentifier
    label <- ($ s) <$> getToLabel
    returnIndex <- getReturnIndex
    incReturnIndex
    let returnLabel = "RETURN_" <> tshow returnIndex
        pushReturn = loadD (T.Symbol returnLabel) <> loadA sp <> setMD <> incStack
        pushPointer sym = loadDM sym <> loadA sp <> setMD <> incStack
        pushLcl = pushPointer lcl
        pushArg = pushPointer arg
        pushThis = pushPointer this
        pushThat = pushPointer that
        setArg = loadDM sp <> shift_D (T.Literal $ 5 + i) <> assignD arg
        setLcl = loadDM sp <> assignD lcl
        goto = toCode ["@" <> label, "0;JMP"]
        ret = toCode ["(" <> returnLabel <> ")"]
    pure $ pushReturn <> pushLcl <> pushArg <> pushThis <> pushThat <> setArg <> setLcl <> goto <> ret
  T.FunctionCommandReturn -> do
    lastFunctionIdentifier <- getFunctionIdentifier
    let functionIdentifier = fromMaybe [] $ tailMay lastFunctionIdentifier
    setFunctionIdentifier functionIdentifier
    let getLcl = loadDM lcl <> assignD r7
        getRet = shift_A (T.Literal 5) <> setDM <> assignD r14
        restoreStack = loadDM arg <> toCode ["@SP", "M=D+1"]
        restoreFrame sym shift = loadDM r7 <> shift_A (T.Literal shift) <> setDM <> assignD sym
        restoreThat = restoreFrame that 1
        restoreThis = restoreFrame this 2
        restoreArg = restoreFrame arg 3
        restoreLcl = restoreFrame lcl 4
        goto = loadA r14 <> toCode ["0;JMP"]
    popArg <- memoryCommand $ T.MemoryCommand T.MemoryOperationPop T.MemoryLocationArgument (T.Index 0)
    pure $ getLcl <> getRet <> popArg <> restoreStack <> restoreThat <> restoreThis <> restoreArg <> restoreLcl <> goto

assemble :: (MonadIO m) => Text -> [T.ParsedCommand] -> m [T.Code]
assemble fileIdentifier parsedCommands = flip evalStateT (emptyVMState fileIdentifier) $ do
  let initStack = loadC (T.Literal 256) <> assignD sp
  initClass <- functionCommand $ T.FunctionCommandInvocation (T.Symbol "Sys.init") (T.Index 0)
  commands <- mconcat <$> traverse assembleCommand parsedCommands
  pure $ initStack <> initClass <> commands

assembleCommand :: (MonadIO m, MonadState VMState m) => T.ParsedCommand -> m [T.Code]
assembleCommand cmd = do
  incCommandNum
  case cmd of
    T.ParsedCommandArithmetic arith -> arithmeticCommand arith
    T.ParsedCommandMemory mem       -> memoryCommand mem
    T.ParsedCommandProgram prog     -> programCommand prog
    T.ParsedCommandFunction func    -> functionCommand func
