{-# LANGUAGE OverloadedStrings #-}

module Ipe.Prelude.Dict (moduleTypes) where

import qualified Data.Map as Map
import Data.Text (Text)
import Ipe.TypeChecker.Utils (Type (..))

moduleTypes :: Map.Map Text Type
moduleTypes =
  Map.fromList
    [ ("Dict", typeDef),
      ("empty", TFun (TRec []) typeDef),
      ("insert", TFun (TVar "key") (TFun (TVar "value") (TFun typeDef typeDef))),
      ("remove", TFun (TVar "key") (TFun typeDef typeDef))
    ]

typeDef :: Type
typeDef = TCustom "Dict" [TVar "key", TVar "value"] []
