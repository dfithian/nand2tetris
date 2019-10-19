module Options where

import ClassyPrelude hiding (FilePath)
import qualified Options.Applicative as Opt
import Turtle (FilePath, decodeString)

data Opts = Opts
  { optsInput  :: FilePath
  , optsOutput :: FilePath
  }

-- |Create an options parser
createParser :: String -> Opt.Parser a -> IO a
createParser desc parser = Opt.execParser $
  Opt.info (Opt.helper <*> parser) (Opt.fullDesc <> Opt.progDesc desc)

-- |Create a string field
stringField :: String -> String -> Opt.Parser String
stringField long help =
  Opt.strOption $ Opt.long long <> Opt.metavar (toUpper long) <> Opt.help help

parseArgs :: IO Opts
parseArgs = createParser "analyzer" $ Opts
  <$> (decodeString <$> stringField "input" "Input file or directory")
  <*> (decodeString <$> stringField "output" "Output directory")
