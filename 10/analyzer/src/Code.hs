module Code where

import ClassyPrelude
import Control.Lens (Contravariant, Over, failing, has, preview, re, to)
import Control.Monad.State (MonadState, evalStateT, get, put)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable, typeRep)

import qualified Types as T

data AnalyzerError = AnalyzerError Text
  deriving (Eq, Show)

instance Exception AnalyzerError

type AnalyzerOptic a = forall f. (Applicative f, Contravariant f) => Over (->) f T.Token T.Token a a

analyze :: MonadIO m => [T.Token] -> m T.Class
analyze = evalStateT analyzeClass

getNext :: (MonadIO m, MonadState [T.Token] m) => m T.Token
getNext =
  get >>= \ case
    [] -> throwIO . AnalyzerError $ "No more tokens"
    x:xs -> put xs >> pure x

peekNext :: (MonadIO m, MonadState [T.Token] m) => m T.Token
peekNext =
  get >>= \ case
    [] -> throwIO . AnalyzerError $ "No more tokens"
    x:_ -> pure x

nextIs :: (MonadIO m, MonadState [T.Token] m, Typeable a) => AnalyzerOptic a -> m a
nextIs (p :: AnalyzerOptic a) = do
  x <- getNext
  case preview p x of
    Just y -> pure y
    Nothing -> throwIO . AnalyzerError $ "Next token didn't match: " <> tshow x <> ", " <> tshow (typeRep (Proxy :: Proxy a))

putBack :: (MonadIO m, MonadState [T.Token] m) => T.Token -> m ()
putBack x = do
  xs <- get
  put $ x:xs

peekNextIs :: (MonadIO m, MonadState [T.Token] m) => AnalyzerOptic a -> m Bool
peekNextIs p = do
  x <- peekNext
  pure $ has p x

doWhilePeekNextIs :: (MonadIO m, MonadState [T.Token] m) => AnalyzerOptic a -> m b -> m [b]
doWhilePeekNextIs p mb = peekNextIs p >>= \ case
  False -> pure []
  True -> do
    b <- mb
    bs <- doWhilePeekNextIs p mb
    pure $ b:bs

doWhilePeekNextIsnt :: (MonadIO m, MonadState [T.Token] m) => AnalyzerOptic a -> m b -> m [b]
doWhilePeekNextIsnt p mb = peekNextIs p >>= \ case
  True -> pure []
  False -> do
    b <- mb
    bs <- doWhilePeekNextIsnt p mb
    pure $ b:bs

isClassVarDecType :: AnalyzerOptic T.ClassVarDecType
isClassVarDecType =
  failing (T._TokenKeyword . T._KeywordStatic . to (const T.ClassVarDecTypeStatic))
  $ (T._TokenKeyword . T._KeywordField . to (const T.ClassVarDecTypeField))

isFieldType :: AnalyzerOptic T.FieldType
isFieldType =
  failing (T._TokenKeyword . T._KeywordInt . to (const T.FieldTypeInt))
  . failing (T._TokenKeyword . T._KeywordChar . to (const T.FieldTypeChar))
  . failing (T._TokenKeyword . T._KeywordBoolean . to (const T.FieldTypeBool))
  $ (T._TokenIdentifier . re T._ClassName . re T._FieldTypeClass)

isFieldTypePlus :: AnalyzerOptic T.FieldTypePlus
isFieldTypePlus =
  failing (T._TokenKeyword . T._KeywordVoid . to (const T.FieldTypePlusVoid)) (isFieldType . re T._FieldTypePlusReg)

analyzeFieldType :: (MonadIO m, MonadState [T.Token] m) => m T.FieldType
analyzeFieldType = do
  putStrLn "FieldType"
  nextIs isFieldType

analyzeVarName :: (MonadIO m, MonadState [T.Token] m) => m T.VarName
analyzeVarName = do
  putStrLn "VarName"
  nextIs (T._TokenIdentifier . re T._VarName)

analyzeFieldTypeVarName :: (MonadIO m, MonadState [T.Token] m) => m (T.FieldType, T.VarName)
analyzeFieldTypeVarName = do
  putStrLn "FieldTypeVarName"
  (,) <$> analyzeFieldType <*> analyzeVarName

analyzeClassVarDec :: (MonadIO m, MonadState [T.Token] m) => m T.ClassVarDec
analyzeClassVarDec = do
  putStrLn "ClassVarDec"
  classVarDecType <- nextIs isClassVarDecType
  fieldType <- analyzeFieldType
  firstVarName <- analyzeVarName
  otherVarNames <- doWhilePeekNextIsnt (T._TokenSymbol . T._SymbolSemicolon) $ do
    void $ nextIs (T._TokenSymbol . T._SymbolComma)
    analyzeVarName
  void $ nextIs (T._TokenSymbol . T._SymbolSemicolon)
  pure $ T.ClassVarDec classVarDecType fieldType $ firstVarName :| otherVarNames

analyzeClassVarDecs :: (MonadIO m, MonadState [T.Token] m) => m [T.ClassVarDec]
analyzeClassVarDecs = doWhilePeekNextIs isClassVarDecType analyzeClassVarDec

isSubroutineDecType :: AnalyzerOptic T.SubroutineDecType
isSubroutineDecType =
  failing (T._TokenKeyword . T._KeywordConstructor . re T._SubroutineDecTypeConstructor)
  . failing (T._TokenKeyword . T._KeywordFunction . re T._SubroutineDecTypeFunction)
  $ (T._TokenKeyword . T._KeywordMethod . re T._SubroutineDecTypeMethod)

analyzeVarDec :: (MonadIO m, MonadState [T.Token] m) => m T.VarDec
analyzeVarDec = do
  putStrLn "VarDec"
  void $ nextIs (T._TokenKeyword . T._KeywordVar)
  fieldType <- analyzeFieldType
  firstVarName <- analyzeVarName
  otherVarNames <- doWhilePeekNextIsnt (T._TokenSymbol . T._SymbolSemicolon) $ do
    void $ nextIs (T._TokenSymbol . T._SymbolComma)
    v <- analyzeVarName
    pure v
  void $ nextIs (T._TokenSymbol . T._SymbolSemicolon)
  pure $ T.VarDec fieldType $ firstVarName :| otherVarNames

analyzeVarDecs :: (MonadIO m, MonadState [T.Token] m) => m [T.VarDec]
analyzeVarDecs = doWhilePeekNextIs (T._TokenKeyword . T._KeywordVar) analyzeVarDec

analyzeSubroutineCall :: (MonadIO m, MonadState [T.Token] m) => m T.SubroutineCall
analyzeSubroutineCall = do
  putStrLn "SubroutineCall"
  firstIdentifier <- nextIs T._TokenIdentifier
  (classOrVarNameMay, subroutineName) <- peekNextIs (T._TokenSymbol . T._SymbolDot) >>= \ case
    False -> pure (Nothing, T.SubroutineName firstIdentifier)
    True -> do
      void $ nextIs (T._TokenSymbol . T._SymbolDot)
      subroutineName <- nextIs (T._TokenIdentifier . re T._SubroutineName)
      pure (Just $ T.ClassOrVarName firstIdentifier, subroutineName)
  void $ nextIs (T._TokenSymbol . T._SymbolOpenParen)
  expressions <- peekNextIs (T._TokenSymbol . T._SymbolCloseParen) >>= \ case
    True -> pure []
    False -> do
      firstExpression <- analyzeExpression
      otherExpressions <- doWhilePeekNextIsnt (T._TokenSymbol . T._SymbolCloseParen) $ do
        void $ nextIs (T._TokenSymbol . T._SymbolComma)
        analyzeExpression
      pure $ firstExpression : otherExpressions
  void $ nextIs (T._TokenSymbol . T._SymbolCloseParen)
  pure $ T.SubroutineCall classOrVarNameMay subroutineName expressions

isOp :: AnalyzerOptic T.Op
isOp =
  failing (T._TokenSymbol . T._SymbolPlus . to (const T.OpPlus))
  . failing (T._TokenSymbol . T._SymbolDash . to (const T.OpDash))
  . failing (T._TokenSymbol . T._SymbolStar . to (const T.OpStar))
  . failing (T._TokenSymbol . T._SymbolSlash . to (const T.OpSlash))
  . failing (T._TokenSymbol . T._SymbolAnd . to (const T.OpAnd))
  . failing (T._TokenSymbol . T._SymbolOr . to (const T.OpOr))
  . failing (T._TokenSymbol . T._SymbolLt . to (const T.OpLt))
  . failing (T._TokenSymbol . T._SymbolGt . to (const T.OpGt))
  $ (T._TokenSymbol . T._SymbolEq . to (const T.OpEq))

analyzeOpTerm :: (MonadIO m, MonadState [T.Token] m) => m (T.Op, T.Term)
analyzeOpTerm = do
  putStrLn "Op Term"
  op <- nextIs isOp
  term <- analyzeTerm
  pure (op, term)

analyzeTerm :: (MonadIO m, MonadState [T.Token] m) => m T.Term
analyzeTerm = do
  putStrLn "Term"
  getNext >>= \ case
    T.TokenInt i -> pure $ T.TermInt i
    T.TokenString s -> pure $ T.TermString s
    T.TokenKeyword T.KeywordTrue -> pure $ T.TermKeyword T.KeywordConstantTrue
    T.TokenKeyword T.KeywordFalse -> pure $ T.TermKeyword T.KeywordConstantFalse
    T.TokenKeyword T.KeywordNull -> pure $ T.TermKeyword T.KeywordConstantNull
    T.TokenKeyword T.KeywordThis -> pure $ T.TermKeyword T.KeywordConstantThis
    T.TokenIdentifier i -> peekNextIs (T._TokenSymbol . T._SymbolOpenBracket) >>= \ case
      True -> do
        void $ nextIs (T._TokenSymbol . T._SymbolOpenBracket)
        expression <- analyzeExpression
        void $ nextIs (T._TokenSymbol . T._SymbolCloseBracket)
        pure $ T.TermVarExpression (T.VarName i) expression
      False -> peekNextIs (failing (T._TokenSymbol . T._SymbolOpenParen) (T._TokenSymbol . T._SymbolDot)) >>= \ case
        True -> do
          putBack $ T.TokenIdentifier i
          T.TermSubroutine <$> analyzeSubroutineCall
        False -> pure $ T.TermVar (T.VarName i)
    T.TokenSymbol T.SymbolOpenParen -> do
      expression <- analyzeExpression
      void $ nextIs (T._TokenSymbol . T._SymbolCloseParen)
      pure $ T.TermExpression expression
    T.TokenSymbol T.SymbolDash -> T.TermUnaryOp T.UnaryOpDash <$> analyzeTerm
    T.TokenSymbol T.SymbolTilde -> T.TermUnaryOp T.UnaryOpTilde <$> analyzeTerm
    other -> throwIO . AnalyzerError $ "Next is not a term: " <> tshow other

analyzeExpression :: (MonadIO m, MonadState [T.Token] m) => m T.Expression
analyzeExpression = do
  putStrLn "Expression"
  term <- analyzeTerm
  opTerms <- doWhilePeekNextIs isOp analyzeOpTerm
  lastOpTerm <- peekNextIs isOp >>= \ case
    False -> pure []
    True -> doWhilePeekNextIs isOp analyzeOpTerm
  pure $ T.Expression term (opTerms <> lastOpTerm)

analyzeLetStatement :: (MonadIO m, MonadState [T.Token] m) => m T.Statement
analyzeLetStatement = do
  putStrLn "Let Statement"
  varName <- analyzeVarName
  firstExpressionMay <- peekNextIs (T._TokenSymbol . T._SymbolOpenBracket) >>= \ case
    False -> pure Nothing
    True -> do
      void $ nextIs (T._TokenSymbol . T._SymbolOpenBracket)
      expression <- analyzeExpression
      void $ nextIs (T._TokenSymbol . T._SymbolCloseBracket)
      pure $ Just expression
  void $ nextIs (T._TokenSymbol . T._SymbolEq)
  expression <- analyzeExpression
  void $ nextIs (T._TokenSymbol . T._SymbolSemicolon)
  pure $ T.StatementLet varName firstExpressionMay expression

analyzeIfStatement :: (MonadIO m, MonadState [T.Token] m) => m T.Statement
analyzeIfStatement = do
  putStrLn "If Statement"
  void $ nextIs (T._TokenSymbol . T._SymbolOpenParen)
  expression <- analyzeExpression
  void $ nextIs (T._TokenSymbol . T._SymbolCloseParen)
  void $ nextIs (T._TokenSymbol . T._SymbolOpenCurly)
  ifStatements <- analyzeStatements
  void $ nextIs (T._TokenSymbol . T._SymbolCloseCurly)
  elseStatements <- peekNextIs (T._TokenKeyword . T._KeywordElse) >>= \ case
    False -> pure []
    True -> do
      void $ nextIs (T._TokenKeyword . T._KeywordElse)
      void $ nextIs (T._TokenSymbol . T._SymbolOpenCurly)
      statements <- analyzeStatements
      void $ nextIs (T._TokenSymbol . T._SymbolCloseCurly)
      pure statements
  pure $ T.StatementIf expression ifStatements elseStatements

analyzeWhileStatement :: (MonadIO m, MonadState [T.Token] m) => m T.Statement
analyzeWhileStatement = do
  putStrLn "While Statement"
  void $ nextIs (T._TokenSymbol . T._SymbolOpenParen)
  expression <- analyzeExpression
  void $ nextIs (T._TokenSymbol . T._SymbolCloseParen)
  void $ nextIs (T._TokenSymbol . T._SymbolOpenCurly)
  statements <- analyzeStatements
  void $ nextIs (T._TokenSymbol . T._SymbolCloseCurly)
  pure $ T.StatementWhile expression statements

analyzeDoStatement :: (MonadIO m, MonadState [T.Token] m) => m T.Statement
analyzeDoStatement = do
  putStrLn "Do Statement"
  subroutineCall <- analyzeSubroutineCall
  void $ nextIs (T._TokenSymbol . T._SymbolSemicolon)
  pure $ T.StatementDo subroutineCall

analyzeReturnStatement :: (MonadIO m, MonadState [T.Token] m) => m T.Statement
analyzeReturnStatement = do
  putStrLn "Return Statement"
  expressionMay <- peekNextIs (T._TokenSymbol . T._SymbolSemicolon) >>= \ case
    True -> pure Nothing
    False -> Just <$> analyzeExpression
  void $ nextIs (T._TokenSymbol . T._SymbolSemicolon)
  pure $ T.StatementReturn expressionMay

analyzeStatement :: (MonadIO m, MonadState [T.Token] m) => m T.Statement
analyzeStatement = do
  putStrLn "Statement"
  getNext >>= \ case
    T.TokenKeyword T.KeywordLet -> analyzeLetStatement
    T.TokenKeyword T.KeywordIf -> analyzeIfStatement
    T.TokenKeyword T.KeywordWhile -> analyzeWhileStatement
    T.TokenKeyword T.KeywordDo -> analyzeDoStatement
    T.TokenKeyword T.KeywordReturn -> analyzeReturnStatement
    other -> throwIO . AnalyzerError $ "Next is not a statement: " <> tshow other

analyzeStatements :: (MonadIO m, MonadState [T.Token] m) => m [T.Statement]
analyzeStatements = doWhilePeekNextIsnt (T._TokenSymbol . T._SymbolCloseCurly) analyzeStatement

analyzeSubroutineDec :: (MonadIO m, MonadState [T.Token] m) => m T.SubroutineDec
analyzeSubroutineDec = do
  putStrLn "SubroutineDec"
  subroutineDecType <- nextIs isSubroutineDecType
  fieldTypePlus <- nextIs isFieldTypePlus
  subroutineName <- nextIs (T._TokenIdentifier . re T._SubroutineName)
  void $ nextIs (T._TokenSymbol . T._SymbolOpenParen)
  fieldTypesVarNames <- peekNextIs (T._TokenSymbol . T._SymbolCloseParen) >>= \ case
    True -> pure []
    False -> do
      firstFieldTypeVarName <- (,) <$> analyzeFieldType <*> analyzeVarName
      otherFieldTypesVarNames <- doWhilePeekNextIsnt (T._TokenSymbol . T._SymbolCloseParen) $ do
        void $ nextIs (T._TokenSymbol . T._SymbolComma)
        analyzeFieldTypeVarName
      pure $ firstFieldTypeVarName : otherFieldTypesVarNames
  void $ nextIs (T._TokenSymbol . T._SymbolCloseParen)
  void $ nextIs (T._TokenSymbol . T._SymbolOpenCurly)
  varDecs <- analyzeVarDecs
  statements <- analyzeStatements
  void $ nextIs (T._TokenSymbol . T._SymbolCloseCurly)
  pure $ T.SubroutineDec subroutineDecType fieldTypePlus subroutineName fieldTypesVarNames varDecs statements

analyzeSubroutineDecs :: (MonadIO m, MonadState [T.Token] m) => m [T.SubroutineDec]
analyzeSubroutineDecs = doWhilePeekNextIs isSubroutineDecType analyzeSubroutineDec

analyzeClass :: (MonadIO m, MonadState [T.Token] m) => m T.Class
analyzeClass = do
  putStrLn "Class"
  void $ nextIs (T._TokenKeyword . T._KeywordClass)
  className <- T.ClassName <$> nextIs T._TokenIdentifier
  void $ nextIs (T._TokenSymbol . T._SymbolOpenCurly)
  classVarDecs <- analyzeClassVarDecs
  subroutineDecs <- analyzeSubroutineDecs
  void $ nextIs (T._TokenSymbol . T._SymbolCloseCurly)
  pure $ T.Class className classVarDecs subroutineDecs
