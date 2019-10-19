import ClassyPrelude hiding (fold, (</>))
import Filesystem.Path (replaceExtension)
import Turtle
  ( Fold (Fold)
  , encodeString
  , filename
  , fold
  , hasExtension
  , isDirectory
  , ls
  , splitExtension
  , stat
  , (</>)
  )

import Code (analyze)
import Options (Opts (Opts), optsInput, optsOutput, parseArgs)
import Tokenizer (tokenizeFile)
import Translate (translate)

main :: IO ()
main = do
  Opts {..} <- parseArgs
  files <- isDirectory <$> stat optsInput >>= \ case
    False ->
      let file = filename optsInput
      in case splitExtension file of
        (_, Just "jack") -> pure [optsInput]
        _ -> fail $ "Invalid file " <> encodeString optsInput <> " - must have .jack extension"
    True -> filter (flip hasExtension "jack") <$> fold (ls optsInput) (Fold (flip (:)) [] id)
  forM_ files $ \ file -> do
    tokens <- tokenizeFile $ encodeString file
    code <- analyze tokens
    commands <- translate code
    let outputFile = encodeString $ optsOutput </> replaceExtension (filename file) "vm"
    writeFile outputFile . encodeUtf8 . unlines $ tshow <$> commands
