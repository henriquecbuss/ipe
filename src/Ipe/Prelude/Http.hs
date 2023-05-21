{-# LANGUAGE OverloadedStrings #-}

module Ipe.Prelude.Http (moduleTypes) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Ipe.Prelude.Dict as IpeDict
import qualified Ipe.Prelude.Json as IpeJson
import qualified Ipe.Prelude.Promise as IpePromise
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
                    (IpePromise.typeDef (TVar "context"))
                ),
                ( "handleRequest",
                  TFun
                    (TVar "context")
                    ( TFun
                        request
                        ( IpePromise.typeDef
                            ( TRec
                                [ ("response", response),
                                  ("newContext", TVar "context")
                                ]
                            )
                        )
                    )
                )
              ]
          )
          (TRec [])
      ),
      ("jsonResponse", TFun IpeJson.typeDef response),
      ("withStatus", TFun TNum (TFun response response)),
      ("Request", request),
      ("Method", method),
      ("Response", response)
    ]

request :: Type
request =
  TRec
    [ ("endpoint", TCustom "List" [TStr] []),
      ("searchParams", IpeDict.typeDef TStr TStr),
      ("body", IpeJson.typeDef),
      ("headers", IpeDict.typeDef TStr TStr),
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
