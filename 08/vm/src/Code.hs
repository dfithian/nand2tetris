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
  { _vmStateSymbols            :: Map T.Symbol Int
  , _vmStateCommandNum         :: Int
  , _vmStateFileIdentifier     :: Text
  , _vmStateFunctionIdentifier :: Maybe Text
  } deriving (Eq, Ord, Show)

data Location
  = LocationLiteral T.Literal
  | LocationSymbol T.Symbol T.Literal
  | LocationOffset T.Symbol T.Literal
  | LocationStatic T.Symbol
  deriving (Eq, Ord, Show)

makeLenses ''VMState

incCommandNum :: (MonadState VMState m) => m ()
incCommandNum = modifying vmStateCommandNum (+ 1)

getCommandNum :: (MonadState VMState m) => m Int
getCommandNum = use vmStateCommandNum

getFileIdentifier :: (MonadState VMState m) => m Text
getFileIdentifier = use vmStateFileIdentifier

setFunctionIdentifier :: (MonadState VMState m) => Maybe Text -> m ()
setFunctionIdentifier = assign vmStateFunctionIdentifier

getFunctionIdentifier :: (MonadState VMState m) => m (Maybe Text)
getFunctionIdentifier = use vmStateFunctionIdentifier

emptyVMState :: Text -> VMState
emptyVMState fileIdentifier = VMState mempty 0 fileIdentifier Nothing

toCode :: [Text] -> [T.Code]
toCode = map T.Code

getLocation :: (MonadState VMState m) => (T.MemoryLocation, T.Index) -> m Location
getLocation (loc, T.Index i) = case loc of
  T.MemoryLocationArgument -> pure $ LocationSymbol (T.Symbol "ARG") (T.Literal i)
  T.MemoryLocationLocal    -> pure $ LocationSymbol (T.Symbol "LCL") (T.Literal i)
  T.MemoryLocationStatic   -> do
    fileIdentifier <- getFileIdentifier
    pure $ LocationStatic (T.Symbol $ fileIdentifier <> "." <> tshow i)
  T.MemoryLocationConstant -> pure $ LocationLiteral (T.Literal i)
  T.MemoryLocationThis     -> pure $ LocationSymbol (T.Symbol "THIS") (T.Literal i)
  T.MemoryLocationThat     -> pure $ LocationSymbol (T.Symbol "THAT") (T.Literal i)
  T.MemoryLocationPointer  -> pure $ LocationOffset (T.Symbol "R3") (T.Literal i)
  T.MemoryLocationTemp     -> pure $ LocationOffset (T.Symbol "R5") (T.Literal i)

sp, r13 :: T.Symbol
sp = T.Symbol "SP"
r13 = T.Symbol "R13"

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

programCommand :: (MonadIO m, MonadState VMState m) => T.ProgramCommand -> m [T.Code]
programCommand cmd = do
  toLabel <- (,) <$> getFunctionIdentifier <*> getFileIdentifier >>= \ case
    (Nothing, fileIdentifier) -> pure $ \ x -> fileIdentifier <> "." <> x
    (Just functionIdentifier, fileIdentifier) -> pure $ \ x -> fileIdentifier <> "." <> functionIdentifier <> "." <> x
  case cmd of
    T.ProgramCommandLabel (T.Symbol s) -> pure $ toCode ["(" <> toLabel s <> ")"]
    T.ProgramCommandGoto (T.Symbol s) -> pure $ toCode ["@" <> toLabel s, "0;JMP"]
    T.ProgramCommandIfGoto (T.Symbol s) -> pure $ decStack <> loadAD sp <> toCode ["@" <> toLabel s, "D;JNE"]

assemble :: (MonadIO m) => Text -> [T.ParsedCommand] -> m [T.Code]
assemble fileIdentifier parsedCommands = flip evalStateT (emptyVMState fileIdentifier) $ do
  let initStack = loadC (T.Literal 256) <> assignD sp
  (initStack <>) . mconcat <$> traverse assembleCommand parsedCommands

assembleCommand :: (MonadIO m, MonadState VMState m) => T.ParsedCommand -> m [T.Code]
assembleCommand cmd = do
  incCommandNum
  case cmd of
    T.ParsedCommandArithmetic arith -> arithmeticCommand arith
    T.ParsedCommandMemory mem       -> memoryCommand mem
    T.ParsedCommandProgram prog     -> programCommand prog
    _                               -> pure []
