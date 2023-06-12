{-# LANGUAGE OverloadedStrings #-}

module Ipe.Prelude.Dict (moduleTypes, typeDef, genericTypeDef) where

import qualified Data.Map as Map
import Data.Text (Text)
import Ipe.TypeChecker.Utils (Type (..))

moduleTypes :: Map.Map Text Type
moduleTypes =
  Map.fromList
    [ ("Dict", genericTypeDef),
      ("empty", TFun (TRec []) genericTypeDef),
      ("insert", TFun (TVar "key") (TFun (TVar "value") (TFun genericTypeDef genericTypeDef))),
      ("remove", TFun (TVar "key") (TFun genericTypeDef genericTypeDef)),
      ( "map",
        TFun
          ( TFun
              (TVar "key")
              ( TFun
                  (TVar "value")
                  (TVar "result")
              )
          )
          ( TFun
              genericTypeDef
              (typeDef (TVar "key") (TVar "result"))
          )
      ),
      ( "toList",
        TFun genericTypeDef (TList (TRec [("key", TVar "key"), ("value", TVar "value")]))
      ),
      ( "get",
        TFun
          (TVar "key")
          ( TFun
              genericTypeDef
              (TCustom "Maybe" [TVar "value"] [])
          )
      )
    ]

typeDef :: Type -> Type -> Type
typeDef key value = TCustom "Dict" [key, value] []

genericTypeDef :: Type
genericTypeDef = typeDef (TVar "key") (TVar "value")
