{-# LANGUAGE OverloadedStrings #-}

module Ipe.Prelude.Json.Encode (moduleTypes) where

import qualified Data.Map as Map
import Data.Text (Text)
import Ipe.TypeChecker.Utils (Type (..))

moduleTypes :: Map.Map Text Type
moduleTypes =
  Map.fromList
    [ ("string", TFun TStr jsonValue),
      ("number", TFun TNum jsonValue),
      ("nothing", TFun (TRec []) jsonValue),
      ( "list",
        TFun
          (TFun (TVar "input") jsonValue)
          ( TFun
              (TList (TVar "input"))
              jsonValue
          )
      ),
      ( "object",
        TFun dictType jsonValue
      )
    ]

jsonValue :: Type
jsonValue = TCustom "Json.Value" [] []

dictType :: Type
dictType = TCustom "Dict.Dict" [TStr, jsonValue] []
