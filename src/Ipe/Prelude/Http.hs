{-# LANGUAGE OverloadedStrings #-}

module Ipe.Prelude.Http (moduleTypes) where

import qualified Data.Map as Map
import Data.Text (Text)
import Ipe.TypeChecker.Utils (Type (..))

moduleTypes :: Map.Map Text Type
moduleTypes =
  Map.fromList
    [ ( "createApp",
        TFun
          ( TRec
              [ ("port", TNum),
                ( "createContext",
                  TFun
                    (TRec [])
                    (TCustom "Promise.Promise" [TVar "context"] [])
                ),
                ( "handleRequest",
                  TFun
                    (TVar "context")
                    ( TFun
                        request
                        ( TCustom
                            "Promise.Promise"
                            [ TRec
                                [ ("response", response),
                                  ("newContext", TVar "context")
                                ]
                            ]
                            []
                        )
                    )
                )
              ]
          )
          (TRec [])
      ),
      ("jsonResponse", TFun (TCustom "Json.Value" [] []) response),
      ("withStatus", TFun TNum (TFun response response)),
      ("Request", request),
      ("Method", method),
      ("Response", response)
    ]

request :: Type
request =
  TRec
    [ ("endpoint", TList TStr),
      ("searchParams", TCustom "Dict.Dict" [TStr, TStr] []),
      ("body", TCustom "Json.Value" [] []),
      ("headers", TCustom "Dict.Dict" [TStr, TStr] []),
      ("method", method)
    ]

response :: Type
response = TCustom "Response" [] []

method :: Type
method =
  TCustom
    "Method"
    []
    [ ("Get", []),
      ("Head", []),
      ("Post", []),
      ("Put", []),
      ("Delete", []),
      ("Connect", []),
      ("Options", []),
      ("Trace", []),
      ("Patch", [])
    ]
