module Options where

import ClassyPrelude
import qualified Options.Applicative as Opt

data Opts = Opts
  { optsInputFile  :: FilePath
  , optsOutputFile :: FilePath
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
parseArgs = createParser "assembler" $ Opts
  <$> stringField "input" "Input file"
  <*> stringField "output" "Output file"
