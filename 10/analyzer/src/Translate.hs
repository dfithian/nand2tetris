module Translate where

import ClassyPrelude
import Control.Lens
  (Lens', Prism', assign, at, failing, has, modifying, preuse, use, uses, view, _2, _Just)
import Control.Lens.TH (makeLenses, makePrisms)
import Control.Monad.State (MonadState, execStateT)
import Data.Char (ord)

import qualified Types as T

data TranslateError = TranslateError Text
  deriving (Eq, Show)

instance Exception TranslateError

data SymbolKind
  = SymbolKindStatic
  | SymbolKindField
  | SymbolKindArg
  | SymbolKindVar
  deriving (Eq, Ord, Show)

data SymbolInfo = SymbolInfo
  { _symbolInfoKind  :: SymbolKind
  , _symbolInfoType  :: Text
  , _symbolInfoIndex :: Int
  } deriving (Eq, Ord, Show)

data TranslateState = TranslateState
  { _translateStateClassSymbolTable      :: Map T.Identifier SymbolInfo
  , _translateStateSubroutineSymbolTable :: Map T.Identifier SymbolInfo
  , _translateStateRunningIndex          :: Int
  , _translateStateVmCommands            :: [T.Command]
  , _translateStateSubroutine            :: Maybe Text
  , _translateStateClass                 :: Text
  } deriving (Eq, Ord, Show)

makePrisms ''SymbolKind
makeLenses ''SymbolInfo
makeLenses ''TranslateState

emptyTranslateState :: Text -> TranslateState
emptyTranslateState cls = TranslateState mempty mempty 1 [] Nothing cls

nextIndex :: (MonadState TranslateState m) => m Int
nextIndex = do
  i <- use translateStateRunningIndex
  modifying translateStateRunningIndex (+1)
  pure i

getClass :: (MonadState TranslateState m) => m Text
getClass = use translateStateClass

getSubroutine :: (MonadState TranslateState m) => m Text
getSubroutine = do
  cls <- getClass
  use translateStateSubroutine >>= \ case
    Nothing -> pure cls
    Just s -> pure $ cls <> "_" <> s

newLabel :: (MonadState TranslateState m) => Text -> m T.Sym
newLabel t = do
  i <- nextIndex
  s <- getSubroutine
  pure . T.Sym $ s <> "_" <> t <> "_" <> tshow i

resetSubroutineSymbolTable :: (MonadState TranslateState m) => Text -> m ()
resetSubroutineSymbolTable s = do
  assign translateStateSubroutineSymbolTable mempty
  assign translateStateSubroutine (Just s)

newSymbol :: (MonadState TranslateState m) => T.Identifier -> T.FieldType -> SymbolKind -> m ()
newSymbol i typ kind = do
  idx <- nextIndex
  let typeName = case typ of
        T.FieldTypeInt -> "int"
        T.FieldTypeChar -> "char"
        T.FieldTypeBool -> "boolean"
        T.FieldTypeClass (T.ClassName (T.Identifier className)) -> className
      info = SymbolInfo kind typeName idx
  case kind of
    SymbolKindStatic -> modifying translateStateClassSymbolTable (insertMap i info)
    SymbolKindField  -> modifying translateStateClassSymbolTable (insertMap i info)
    SymbolKindArg    -> modifying translateStateSubroutineSymbolTable (insertMap i info)
    SymbolKindVar    -> modifying translateStateSubroutineSymbolTable (insertMap i info)

getSymbol :: (MonadIO m, MonadState TranslateState m) => T.Identifier -> m SymbolInfo
getSymbol name = getSymbolMay name >>= \ case
  Nothing -> throwIO . TranslateError $ tshow name <> " not found"
  Just x -> pure x

getSymbolMay :: (MonadState TranslateState m) => T.Identifier -> m (Maybe SymbolInfo)
getSymbolMay name = preuse
  ( failing (translateStateClassSymbolTable . at name . _Just)
    (translateStateSubroutineSymbolTable . at name . _Just) )

indexOf :: (MonadState TranslateState m) => Lens' TranslateState (Map T.Identifier SymbolInfo) -> T.Identifier -> m (Maybe Int)
indexOf l name = preuse (l . at name . _Just . symbolInfoIndex)

varCount :: (MonadState TranslateState m) => Lens' TranslateState (Map T.Identifier SymbolInfo) -> Prism' SymbolKind () -> m Int
varCount l p = uses l (length . filter (has $ _2 . symbolInfoKind . p) . mapToList)

write :: (MonadState TranslateState m) => T.Command -> m ()
write cmd = modifying translateStateVmCommands (cmd:)

writePush :: (MonadState TranslateState m) => T.MemoryLocation -> T.Index -> m ()
writePush loc i = write $ T.CommandMemory $ T.MemoryCommand T.MemoryOperationPush loc i

writePop :: (MonadState TranslateState m) => T.MemoryLocation -> T.Index -> m ()
writePop loc i = write $ T.CommandMemory $ T.MemoryCommand T.MemoryOperationPop loc i

writeDeclaration :: (MonadState TranslateState m) => T.Sym -> T.Index -> m ()
writeDeclaration sym i = write $ T.CommandFunction $ T.FunctionCommandDeclaration sym i

writeInvocation :: (MonadState TranslateState m) => T.Sym -> T.Index -> m ()
writeInvocation sym i = write $ T.CommandFunction $ T.FunctionCommandInvocation sym i

writeReturn :: (MonadState TranslateState m) => m ()
writeReturn = write $ T.CommandFunction T.FunctionCommandReturn

writeArithmetic :: (MonadState TranslateState m) => T.ArithmeticCommand -> m ()
writeArithmetic = write . T.CommandArithmetic

writeIf :: (MonadState TranslateState m) => T.Sym -> m ()
writeIf = write . T.CommandProgram . T.ProgramCommandIfGoto

writeGoto :: (MonadState TranslateState m) => T.Sym -> m ()
writeGoto = write . T.CommandProgram . T.ProgramCommandGoto

writeLabel :: (MonadState TranslateState m) => T.Sym -> m ()
writeLabel = write . T.CommandProgram . T.ProgramCommandLabel

symbolKindToSegment :: SymbolKind -> T.MemoryLocation
symbolKindToSegment = \ case
  SymbolKindStatic -> T.MemoryLocationStatic
  SymbolKindField -> T.MemoryLocationThis
  SymbolKindVar -> T.MemoryLocationLocal
  SymbolKindArg -> T.MemoryLocationArgument

translateTerm :: (MonadIO m, MonadState TranslateState m) => T.Term -> m ()
translateTerm = \ case
  T.TermInt (T.IntConstant i) -> writePush T.MemoryLocationConstant $ T.Index i
  T.TermString (T.StringConstant s) -> do
    writePush T.MemoryLocationConstant $ T.Index $ length s
    writeInvocation (T.Sym "String.new") $ T.Index 1
    forM_ s $ \ c -> do
      writePush T.MemoryLocationConstant $ T.Index $ ord c
      writeInvocation (T.Sym "String.appendChar") $ T.Index 2
  T.TermKeyword keyword -> case keyword of
    T.KeywordConstantTrue -> do
      writePush T.MemoryLocationConstant $ T.Index 0
      writeArithmetic T.ArithmeticCommandNot
    T.KeywordConstantFalse -> writePush T.MemoryLocationConstant $ T.Index 0
    T.KeywordConstantNull -> writePush T.MemoryLocationConstant $ T.Index 0
    T.KeywordConstantThis -> writePush T.MemoryLocationPointer $ T.Index 0
  T.TermVar (T.VarName name) -> do
    SymbolInfo kind _ i <- getSymbol name
    writePush (symbolKindToSegment kind) $ T.Index i
  T.TermVarExpression (T.VarName name) expression -> do
    SymbolInfo kind _ i <- getSymbol name
    writePush (symbolKindToSegment kind) $ T.Index i
    translateExpression expression
    writeArithmetic T.ArithmeticCommandAdd
    writePop T.MemoryLocationPointer $ T.Index 1
    writePush T.MemoryLocationThat $ T.Index 1
  T.TermSubroutine subroutineCall -> translateSubroutineCall subroutineCall
  T.TermExpression expression -> translateExpression expression
  T.TermUnaryOp op term -> do
    translateTerm term
    case op of
      T.UnaryOpDash  -> writeArithmetic T.ArithmeticCommandNeg
      T.UnaryOpTilde -> writeArithmetic T.ArithmeticCommandNot

translateOp :: (MonadState TranslateState m) => T.Op -> m ()
translateOp = \ case
  T.OpPlus -> writeArithmetic T.ArithmeticCommandAdd
  T.OpDash -> writeArithmetic T.ArithmeticCommandSub
  T.OpStar -> writeInvocation (T.Sym "Math.multiply") $ T.Index 2
  T.OpSlash -> writeInvocation (T.Sym "Math.divide") $ T.Index 2
  T.OpAnd -> writeArithmetic T.ArithmeticCommandAnd
  T.OpOr -> writeArithmetic T.ArithmeticCommandOr
  T.OpLt -> writeArithmetic T.ArithmeticCommandLt
  T.OpGt -> writeArithmetic T.ArithmeticCommandGt
  T.OpEq -> writeArithmetic T.ArithmeticCommandEq

translateExpression :: (MonadIO m, MonadState TranslateState m) => T.Expression -> m ()
translateExpression (T.Expression term opsTerms) = do
  translateTerm term
  forM_ opsTerms $ \ (op, t) -> do
    translateTerm t
    translateOp op

translateSubroutineCall :: (MonadIO m, MonadState TranslateState m) => T.SubroutineCall -> m ()
translateSubroutineCall (T.SubroutineCall classOrVarNameMay (T.SubroutineName (T.Identifier name)) expressions) = do
  case classOrVarNameMay of
    Nothing -> do
      writePush T.MemoryLocationPointer $ T.Index 0
      forM_ expressions translateExpression
      cls <- getClass
      writeInvocation (T.Sym $ cls <> "." <> name) $ T.Index $ length expressions + 1
    Just (T.ClassOrVarName classOrVarName) -> do
      getSymbolMay classOrVarName >>= \ case
        Nothing -> do
          forM_ expressions translateExpression
          writeInvocation (T.Sym $ T.unIdentifier classOrVarName <> "." <> name) $ T.Index $ length expressions
        Just (SymbolInfo kind typ i) -> do
          writePush (symbolKindToSegment kind) $ T.Index i
          forM_ expressions translateExpression
          writeInvocation (T.Sym $ typ <> "." <> name) $ T.Index $ length expressions + 1

translateStatement :: (MonadIO m, MonadState TranslateState m) => T.Statement -> m ()
translateStatement = \ case
  T.StatementLet (T.VarName varName) expressionMay expression -> do
    SymbolInfo kind _ i <- getSymbol varName
    case expressionMay of
      Nothing -> do
        translateExpression expression
        writePop (symbolKindToSegment kind) $ T.Index i
      Just indexExpression -> do
        writePush (symbolKindToSegment kind) $ T.Index i
        translateExpression indexExpression
        writeArithmetic T.ArithmeticCommandAdd
        translateExpression expression
        writePop T.MemoryLocationTemp $ T.Index 0
        writePop T.MemoryLocationPointer $ T.Index 1
        writePush T.MemoryLocationTemp $ T.Index 0
        writePop T.MemoryLocationThat $ T.Index 0
  T.StatementIf expression ifStatements elseStatements -> do
    labelTrue <- newLabel "IF_TRUE"
    labelFalse <- newLabel "IF_FALSE"
    translateExpression expression
    writeArithmetic T.ArithmeticCommandNot
    writeIf labelFalse
    forM_ ifStatements translateStatement
    writeGoto labelTrue
    writeLabel labelFalse
    forM_ elseStatements translateStatement
    writeLabel labelTrue
  T.StatementWhile expression statements -> do
    labelStart <- newLabel "WHILE_START"
    labelEnd <- newLabel "WHILE_END"
    writeLabel labelStart
    translateExpression expression
    writeArithmetic T.ArithmeticCommandNot
    writeIf labelEnd
    forM_ statements translateStatement
    writeGoto labelStart
    writeLabel labelEnd
  T.StatementDo subroutineCall -> translateSubroutineCall subroutineCall
  T.StatementReturn expressionMay -> do
    case expressionMay of
      Nothing         -> writePush T.MemoryLocationConstant $ T.Index 0
      Just expression -> translateExpression expression
    writeReturn

translateClass :: (MonadIO m, MonadState TranslateState m) => T.Class -> m ()
translateClass (T.Class _ classVarDecs subroutineDecs) = do
  forM_ classVarDecs $ \ (T.ClassVarDec typ fieldType names) ->
    forM_ names $ \ (T.VarName name) -> case typ of
      T.ClassVarDecTypeField  -> newSymbol name fieldType SymbolKindField
      T.ClassVarDecTypeStatic -> newSymbol name fieldType SymbolKindStatic
  forM_ subroutineDecs $ \ (T.SubroutineDec typ _ (T.SubroutineName name) fieldTypesVarNames varDecs statements) -> do
    resetSubroutineSymbolTable (T.unIdentifier name)
    case typ of
      T.SubroutineDecTypeMethod -> do
        className <- use translateStateClass
        newSymbol (T.Identifier "this") (T.FieldTypeClass . T.ClassName . T.Identifier $ className) SymbolKindVar
      _ -> pure ()
    forM_ fieldTypesVarNames $ \ (fieldType, (T.VarName varName)) ->
      newSymbol varName fieldType SymbolKindArg
    forM_ varDecs $ \ (T.VarDec fieldType vars) ->
      forM_ vars $ \ (T.VarName varName) ->
        newSymbol varName fieldType SymbolKindVar
    varsCount <- varCount translateStateSubroutineSymbolTable _SymbolKindVar
    writeDeclaration (T.Sym $ T.unIdentifier name) $ T.Index varsCount
    case typ of
      T.SubroutineDecTypeMethod -> do
        writePush T.MemoryLocationArgument $ T.Index 0
        writePop T.MemoryLocationPointer $ T.Index 0
      T.SubroutineDecTypeConstructor -> do
        fieldCount <- varCount translateStateClassSymbolTable _SymbolKindField
        writePush T.MemoryLocationConstant $ T.Index fieldCount
        writeInvocation (T.Sym "Memory.alloc") $ T.Index 1
        writePop T.MemoryLocationPointer $ T.Index 0
      _ -> pure ()
    forM_ statements translateStatement

translate :: MonadIO m => T.Class -> m [T.Command]
translate cls =
  let T.Class (T.ClassName (T.Identifier name)) _ _ = cls
      state = emptyTranslateState name
  in view translateStateVmCommands
     <$> execStateT (translateClass cls) state
