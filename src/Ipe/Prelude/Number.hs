{-# LANGUAGE OverloadedStrings #-}

module Ipe.Prelude.Number (moduleTypes) where

import qualified Data.Map as Map
import Data.Text (Text)
import Ipe.TypeChecker.Utils (Type (..))

moduleTypes :: Map.Map Text Type
moduleTypes =
  Map.fromList
    [ ( "toString",
        TFun
          TNum
          ( TCustom "Maybe" [TStr] []
          )
      ),
      ( "fromString",
        TFun TStr (TCustom "Maybe" [TNum] [])
      )
    ]
