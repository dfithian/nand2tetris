import ClassyPrelude hiding (fold, (<.>), (</>))
import Turtle
  ( Fold (Fold)
  , directory
  , encodeString
  , filename
  , fold
  , hasExtension
  , isDirectory
  , ls
  , splitExtension
  , stat
  , (<.>)
  , (</>)
  )

import Code (assemble)
import Options (Opts (Opts), optsInput, optsOutput, parseArgs)
import Parser (parseFile)
import qualified Types as T

main :: IO ()
main = do
  Opts {..} <- parseArgs
  (dir, files) <- isDirectory <$> stat optsInput >>= \ case
    False ->
      let dir = directory optsInput
          file = filename optsInput
      in case splitExtension file of
        (base, Just "vm") -> pure (dir, [base])
        _                 -> fail $ "Invalid file " <> encodeString optsInput <> " - must have .vm extension"
    True -> (optsInput,) . filter (flip hasExtension "vm") <$> fold (ls optsInput) (Fold (flip (:)) [] id)
  commands <- forM files $ \ file -> map (pack $ encodeString file,) . parseFile . encodeString $ dir </> file <.> "vm"
  code <- mconcat <$> traverse (uncurry assemble) commands
  writeFile (encodeString optsOutput) (encodeUtf8 . unlines . map T.unCode $ code)
