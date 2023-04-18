module Ipe.Parser (parseText, parseFile) where

import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Ipe.Grammar
import qualified Ipe.Parser.Module
import Text.Megaparsec as Parsec.Common

parseFile :: FilePath -> IO (Either String Ipe.Grammar.Module)
parseFile filePath = do
  fileContents <- Text.readFile filePath

  return $ parseTextWithFileName filePath fileContents

parseText :: Text -> Either String Ipe.Grammar.Module
parseText =
  parseTextWithFileName ""

parseTextWithFileName :: FilePath -> Text -> Either String Ipe.Grammar.Module
parseTextWithFileName filePath moduleText =
  case Parsec.Common.parse
    Ipe.Parser.Module.parser
    filePath
    moduleText of
    Left err -> Left $ Parsec.Common.errorBundlePretty err
    Right m -> Right m
