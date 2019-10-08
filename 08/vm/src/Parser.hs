module Parser where

import ClassyPrelude hiding (index)
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8 as C8
import Data.Text (strip)
import Text.Read (read)

import qualified Types as T

data ParseError = ParseError Text
  deriving (Eq, Show)

instance Exception ParseError

parseFile :: MonadIO m => FilePath -> m [T.ParsedCommand]
parseFile fp = readFile fp >>= parseCommands

parseCommands :: MonadIO m => ByteString -> m [T.ParsedCommand]
parseCommands = map catMaybes . traverse parseLine . map (encodeUtf8 . strip) . lines . decodeUtf8

parseLine :: MonadIO m => ByteString -> m (Maybe T.ParsedCommand)
parseLine = runParser command

runParser :: MonadIO m => Parser a -> ByteString -> m a
runParser parser bs = case parse parser bs of
  Done _ x -> pure x
  Fail _ _ err -> throwIO . ParseError $ "Failed to parse " <> decodeUtf8 bs <> " due to " <> pack err
  Partial f -> case f "" of
    Done _ x -> pure x
    Fail _ _ err -> throwIO . ParseError $ "Partial failed to parse " <> decodeUtf8 bs <> " due to " <> pack err
    Partial _ -> throwIO . ParseError $ "Got partial for " <> decodeUtf8 bs

-- lowercase or uppercase
isAlpha :: Word8 -> Bool
isAlpha w = (w >= 65 && w <= 90) || (w >= 97 && w <= 122)

isDigit :: Word8 -> Bool
isDigit w = w >= 48 && w <= 57

-- underscore, dot, dollar, colon
isSymbol :: Word8 -> Bool
isSymbol w = isAlpha w || isDigit w || w == 95 || w == 46 || w == 36 || w == 58

spaces :: Parser ()
spaces = void $ many' (string " ")

comment :: Parser ()
comment = spaces *> choice [ endOfInput
                           , void (string "//") <* many' anyWord8 <* endOfInput ]

index :: Parser T.Index
index = T.Index . read . C8.unpack <$> takeWhile1 isDigit

-- FIXME not starting with digit
symbol :: Parser T.Symbol
symbol = T.Symbol . decodeUtf8 <$> takeWhile1 isSymbol

memoryOperation :: Parser T.MemoryOperation
memoryOperation = choice
  [ T.MemoryOperationPop <$ string "pop"
  , T.MemoryOperationPush <$ string "push"
  ]

memoryLocation :: Parser T.MemoryLocation
memoryLocation = choice
  [ T.MemoryLocationArgument <$ string "argument"
  , T.MemoryLocationLocal <$ string "local"
  , T.MemoryLocationStatic <$ string "static"
  , T.MemoryLocationConstant <$ string "constant"
  , T.MemoryLocationThis <$ string "this"
  , T.MemoryLocationThat <$ string "that"
  , T.MemoryLocationPointer <$ string "pointer"
  , T.MemoryLocationTemp <$ string "temp"
  ]

arithmeticCommand :: Parser T.ArithmeticCommand
arithmeticCommand = choice
  [ T.ArithmeticCommandAdd <$ string "add"
  , T.ArithmeticCommandSub <$ string "sub"
  , T.ArithmeticCommandNeg <$ string "neg"
  , T.ArithmeticCommandEq <$ string "eq"
  , T.ArithmeticCommandGt <$ string "gt"
  , T.ArithmeticCommandLt <$ string "lt"
  , T.ArithmeticCommandAnd <$ string "and"
  , T.ArithmeticCommandOr <$ string "or"
  , T.ArithmeticCommandNot <$ string "not"
  ]

memoryCommand :: Parser T.MemoryCommand
memoryCommand = T.MemoryCommand <$> (memoryOperation <* spaces) <*> (memoryLocation <* spaces) <*> index

programCommand :: Parser T.ProgramCommand
programCommand = choice
  [ T.ProgramCommandLabel <$ (string "label" <* spaces) <*> symbol
  , T.ProgramCommandGoto <$ (string "goto" <* spaces) <*> symbol
  , T.ProgramCommandIfGoto <$ (string "if-goto" <* spaces) <*> symbol
  ]

functionCommand :: Parser T.FunctionCommand
functionCommand = choice
  [ T.FunctionCommandDeclaration <$ (string "function" <* spaces) <*> (symbol <* spaces) <*> index
  , T.FunctionCommandInvocation <$ (string "call" <* spaces) <*> (symbol <* spaces) <*> index
  , T.FunctionCommandReturn <$ string "return"
  ]

command :: Parser (Maybe T.ParsedCommand)
command = spaces *> choice
  [ Just . T.ParsedCommandArithmetic <$> arithmeticCommand
  , Just . T.ParsedCommandMemory <$> memoryCommand
  , Just . T.ParsedCommandProgram <$> programCommand
  , Just . T.ParsedCommandFunction <$> functionCommand
  , Nothing <$ comment
  ] <* comment
