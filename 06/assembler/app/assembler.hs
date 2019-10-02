import ClassyPrelude

import Code (assemble)
import Options (Opts (Opts), optsInputFile, optsOutputFile, parseArgs)
import Parser (parseFile)
import qualified Types as T

main :: IO ()
main = do
  Opts {..} <- parseArgs
  commands <- parseFile optsInputFile
  codes <- assemble commands
  writeFile optsOutputFile . encodeUtf8 . unlines . map T.unCode $ codes
