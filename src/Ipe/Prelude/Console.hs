{-# LANGUAGE OverloadedStrings #-}

module Ipe.Prelude.Console (moduleTypes) where

import qualified Data.Map as Map
import Data.Text (Text)
import Ipe.TypeChecker.Utils (Type (..))

moduleTypes :: Map.Map Text Type
moduleTypes =
  Map.fromList
    [ ("log", TFun TStr TStr),
      ("andLog", TFun TStr (TFun (TVar "a") (TVar "a"))),
      ( "andLogValue",
        TFun
          (TFun (TVar "a") TStr)
          (TFun (TVar "a") (TVar "a"))
      )
    ]
