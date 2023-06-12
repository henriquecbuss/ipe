{-# LANGUAGE OverloadedStrings #-}

module Ipe.Prelude.Json.Decode (moduleTypes) where

import qualified Data.Map as Map
import Data.Text (Text)
import Ipe.TypeChecker.Utils (Type (..))

moduleTypes :: Map.Map Text Type
moduleTypes =
  Map.fromList
    [ ("Decoder", decoderType (TVar "a")),
      ( "parseJson",
        TFun
          (decoderType (TVar "a"))
          ( TFun
              jsonValue
              (TCustom "Result" [TStr, TVar "a"] [])
          )
      ),
      ( "parseString",
        TFun (decoderType (TVar "a")) (TFun TStr (TCustom "Result" [TStr, TVar "a"] []))
      ),
      ( "succeed",
        TFun (TVar "a") (decoderType (TVar "a"))
      ),
      ( "map2",
        TFun
          ( TFun (TVar "a") (TFun (TVar "b") (TVar "c"))
          )
          ( TFun
              (decoderType (TVar "a"))
              ( TFun
                  (decoderType (TVar "b"))
                  (decoderType (TVar "c"))
              )
          )
      ),
      ("field", TFun TStr (TFun (decoderType (TVar "a")) (decoderType (TVar "a")))),
      ("string", decoderType TStr),
      ("number", decoderType TNum),
      ( "list",
        TFun
          (decoderType (TVar "a"))
          (decoderType (TList (TVar "a")))
      )
    ]

jsonValue :: Type
jsonValue = TCustom "Json.Value" [] []

decoderType :: Type -> Type
decoderType typeArg = TCustom "Decoder" [typeArg] []
