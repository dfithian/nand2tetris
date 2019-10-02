module Parser where

import ClassyPrelude hiding (takeWhile, until)
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8 as C8
import Data.Text (strip)
import Text.Read (read)

import qualified Types as T

data ParseError = ParseError Text
  deriving (Eq, Show)

instance Exception ParseError

parseFile :: MonadIO m => FilePath -> m [T.Command]
parseFile fp = readFile fp >>= parseCommands

parseCommands :: MonadIO m => ByteString -> m [T.Command]
parseCommands = map catMaybes . traverse parseLine . map (encodeUtf8 . strip) . lines . decodeUtf8

parseLine :: MonadIO m => ByteString -> m (Maybe T.Command)
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

isParen :: Word8 -> Bool
isParen = (==) 41

-- underscore, dot, dollar, colon
isSymbol :: Word8 -> Bool
isSymbol w = isAlpha w || isDigit w || w == 95 || w == 46 || w == 36 || w == 58

spaces :: Parser ()
spaces = void $ many' (string " ")

comment :: Parser ()
comment = spaces *> choice [ endOfInput
                           , void (string "//") <* many' anyWord8 <* endOfInput ]

at :: Parser ()
at = void (string "@")

-- FIXME not starting with digit
atSymbol :: Parser T.Symbol
atSymbol = at *> (T.Symbol . decodeUtf8 <$> takeWhile1 isSymbol)

literal :: Parser T.Literal
literal = T.Literal . read . C8.unpack <$> takeWhile1 isDigit

atLiteral :: Parser T.Literal
atLiteral = at *> literal

parenSymbol :: Parser T.Symbol
parenSymbol = void (string "(") *> (T.Symbol . decodeUtf8 <$> takeWhile1 (not . isParen)) <* void (string ")")

until :: String -> Parser a -> Parser a
until s p = p <* void (string $ C8.pack s)

dest :: Parser T.Dest
dest = choice [ T.DestAMD <$ string "AMD"
              , T.DestAM <$ string "AM"
              , T.DestAD <$ string "AD"
              , T.DestMD <$ string "MD"
              , T.DestA <$ string "A"
              , T.DestD <$ string "D"
              , T.DestM <$ string "M" ]

comp :: Parser T.Comp
comp = choice [ T.CompD1 <$ string "D+1"
              , T.CompA1 <$ string "A+1"
              , T.CompM1 <$ string "M+1"
              , T.CompD_1 <$ string "D-1"
              , T.CompA_1 <$ string "A-1"
              , T.CompM_1 <$ string "M-1"
              , T.CompDA <$ string "D+A"
              , T.CompDM <$ string "D+M"
              , T.CompD_A <$ string "D-A"
              , T.CompD_M <$ string "D-M"
              , T.CompA_D <$ string "A-D"
              , T.CompM_D <$ string "M-D"
              , T.CompDXM <$ string "D&M"
              , T.CompAXD <$ string "A&D"
              , T.CompDOA <$ string "D|A"
              , T.CompDOM <$ string "D|M"
              , T.CompBangD <$ string "!D"
              , T.CompBangA <$ string "!A"
              , T.CompBangM <$ string "!M"
              , T.Comp_1 <$ string "-1"
              , T.Comp_D <$ string "-D"
              , T.Comp_A <$ string "-A"
              , T.Comp_M <$ string "-M"
              , T.Comp0 <$ string "0"
              , T.Comp1 <$ string "1"
              , T.CompD <$ string "D"
              , T.CompA <$ string "A"
              , T.CompM <$ string "M" ]

cond :: Parser T.Cond
cond = choice [ T.CondA <$ string "A"
              , T.CondD <$ string "D"
              , T.CondM <$ string "M"
              , T.Cond0 <$ string "0" ]

jump :: Parser T.Jump
jump = choice [ T.JumpGT <$ string "JGT"
              , T.JumpEQ <$ string "JEQ"
              , T.JumpGE <$ string "JGE"
              , T.JumpLT <$ string "JLT"
              , T.JumpNE <$ string "JNE"
              , T.JumpLE <$ string "JLE"
              , T.JumpAny <$ string "JMP" ]

command :: Parser (Maybe T.Command)
command = spaces *> choice [ Just . T.CommandLabel <$> parenSymbol
                           , Just . T.CommandLiteral <$> atLiteral
                           , Just . T.CommandSymbol <$> atSymbol
                           , map Just $ T.CommandComp <$> until "=" dest <*> comp
                           , map Just $ T.CommandJump <$> until ";" cond <*> jump
                           , Nothing <$ comment ] <* comment
