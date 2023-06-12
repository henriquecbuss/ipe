{-# LANGUAGE OverloadedStrings #-}

module Ipe.Prelude.List (moduleTypes) where

import qualified Data.Map as Map
import Data.Text (Text)
import Ipe.TypeChecker.Utils (Type (..))

moduleTypes :: Map.Map Text Type
moduleTypes =
  Map.fromList
    [ ( "map",
        TFun
          (TFun (TVar "a") (TVar "b"))
          ( TFun (TList (TVar "a")) (TList (TVar "b"))
          )
      )
    ]
