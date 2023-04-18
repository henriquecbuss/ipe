{-# LANGUAGE OverloadedStrings #-}

module Ipe.Emitter (commaSeparatedList, emitString) where

import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter

commaSeparatedList :: [Doc ann] -> Doc ann
commaSeparatedList =
  align . concatWith (surround (comma <> space))

emitString :: Text -> Doc ann
emitString str = squotes $ pretty $ T.replace "'" "\\'" str
