module Tokenizer where

import ClassyPrelude
import Data.Attoparsec.Text
import Data.Char (isAlphaNum, isDigit, isSpace)

import qualified Types as T

data TokenizerError = TokenizerError Text
  deriving (Eq, Show)

instance Exception TokenizerError

tokenizeFile :: MonadIO m => FilePath -> m [T.Token]
tokenizeFile fp = readFile fp >>= runParser tokenizeWords . decodeUtf8

runParser :: MonadIO m => Parser a -> Text -> m a
runParser parser input = case parse parser input of
  Done _ x -> pure x
  Fail _ _ err -> throwIO . TokenizerError $ "Failed to parse " <> input <> " due to " <> pack err
  Partial f -> case f "" of
    Done _ x -> pure x
    Fail _ _ err -> throwIO . TokenizerError $ "Partial failed to parse " <> input <> " due to " <> pack err
    Partial _ -> throwIO . TokenizerError $ "Got partial for " <> input

toKeyword :: Text -> Maybe T.Keyword
toKeyword = \ case
  "class" -> Just T.KeywordClass
  "constructor" -> Just T.KeywordConstructor
  "function" -> Just T.KeywordFunction
  "method" -> Just T.KeywordMethod
  "field" -> Just T.KeywordField
  "static" -> Just T.KeywordStatic
  "var" -> Just T.KeywordVar
  "int" -> Just T.KeywordInt
  "char" -> Just T.KeywordChar
  "boolean" -> Just T.KeywordBoolean
  "void" -> Just T.KeywordVoid
  "true" -> Just T.KeywordTrue
  "false" -> Just T.KeywordFalse
  "null" -> Just T.KeywordNull
  "this" -> Just T.KeywordThis
  "let" -> Just T.KeywordLet
  "do" -> Just T.KeywordDo
  "if" -> Just T.KeywordIf
  "else" -> Just T.KeywordElse
  "while" -> Just T.KeywordWhile
  "return" -> Just T.KeywordReturn
  _ -> Nothing

toSymbol :: Text -> Maybe T.Symbol
toSymbol = \ case
  "{" -> Just T.SymbolOpenCurly
  "}" -> Just T.SymbolCloseCurly
  "(" -> Just T.SymbolOpenParen
  ")" -> Just T.SymbolCloseParen
  "[" -> Just T.SymbolOpenBracket
  "]" -> Just T.SymbolCloseBracket
  "." -> Just T.SymbolDot
  "," -> Just T.SymbolComma
  ";" -> Just T.SymbolSemicolon
  "+" -> Just T.SymbolPlus
  "-" -> Just T.SymbolDash
  "*" -> Just T.SymbolStar
  "/" -> Just T.SymbolSlash
  "&" -> Just T.SymbolAnd
  "|" -> Just T.SymbolOr
  "<" -> Just T.SymbolLt
  ">" -> Just T.SymbolGt
  "=" -> Just T.SymbolEq
  "~" -> Just T.SymbolTilde
  _ -> Nothing

toIntConstant :: Text -> Maybe T.IntConstant
toIntConstant = map T.IntConstant . readMay

toStringConstant :: Text -> Maybe T.StringConstant
toStringConstant = Just . T.StringConstant <=< stripPrefix "\"" <=< stripSuffix "\""

toIdentifier :: Text -> Maybe T.Identifier
toIdentifier x = case headMay x of
  Nothing -> Nothing
  Just x' -> if not (isDigit x') && all (\ c -> isAlphaNum c || c == '_') x then Just (T.Identifier x) else Nothing

runTokenChoice :: Text -> Parser T.Token
runTokenChoice w =
  let choices =
        [ map T.TokenKeyword . toKeyword
        , map T.TokenSymbol . toSymbol
        , map T.TokenInt . toIntConstant
        , map T.TokenString . toStringConstant
        , map T.TokenIdentifier . toIdentifier
        ]
  in case headMay . catMaybes $ ($ w) <$> choices of
    Nothing -> fail $ "Word " <> unpack w <> " is not a valid token"
    Just x  -> pure x

isAlphaNumUnderscore :: Char -> Bool
isAlphaNumUnderscore c = isAlphaNum c || c == '_'

wordGroups :: Char -> Char -> Bool
wordGroups x y = isAlphaNumUnderscore x && isAlphaNumUnderscore y

isSpaceOrQuote :: Char -> Bool
isSpaceOrQuote c = isSpace c || c == '"'

tokenizeWords :: Parser [T.Token]
tokenizeWords = peekChar >>= \ case
  Nothing -> pure []
  Just '"' -> do
    stringTok <- string "\"" *> (T.TokenString . T.StringConstant <$> takeWhile1 (not . (==) '"')) <* string "\""
    (stringTok:) <$> tokenizeWords
  Just _ -> do
    otherToks <- skipSpace *> (traverse runTokenChoice . groupBy wordGroups =<< takeWhile1 (not . isSpaceOrQuote)) <* skipSpace
    (otherToks <>) <$> tokenizeWords
