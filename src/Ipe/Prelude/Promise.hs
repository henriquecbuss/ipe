{-# LANGUAGE OverloadedStrings #-}

module Ipe.Prelude.Promise (moduleTypes, typeDef) where

import qualified Data.Map as Map
import Data.Text (Text)
import Ipe.TypeChecker.Utils (Type (..))

moduleTypes :: Map.Map Text Type
moduleTypes =
  Map.fromList
    [ ("Promise", typeDef (TVar "a")),
      ( "andThen",
        TFun
          (typeDef (TVar "original"))
          ( TFun
              ( TFun
                  (TVar "original")
                  (typeDef (TVar "transformed"))
              )
              (typeDef (TVar "transformed"))
          )
      ),
      ( "map",
        TFun
          (typeDef (TVar "original"))
          ( TFun
              ( TFun
                  (TVar "original")
                  (TVar "transformed")
              )
              (typeDef (TVar "transformed"))
          )
      ),
      ( "succeed",
        TFun (TVar "a") (typeDef (TVar "a"))
      )
    ]

typeDef :: Type -> Type
typeDef innerType = TCustom "Promise" [innerType] []
