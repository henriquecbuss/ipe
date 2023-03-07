{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module Ipe.TypeChecker (IpeType (..), TypeCheckingMonad, get, put, run, runWith) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as Except
import Control.Monad.Trans.State.Lazy (State)
import qualified Control.Monad.Trans.State.Lazy as State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

data IpeType
  = IpeNumber
  | IpeString
  | IpeFunction IpeType IpeType
  | IpeUnit
  | IpeRecord (Map Text IpeType)
  deriving (Eq)

instance Show IpeType where
  show IpeNumber = "Number"
  show IpeString = "String"
  show (IpeFunction fnInput fnOutput) = "(" <> show fnInput <> " -> " <> show fnOutput <> ")"
  show IpeUnit = "()"
  show (IpeRecord record) = T.unpack $ T.concat ["{", mapString, "}"]
    where
      mapString :: Text
      mapString =
        T.intercalate "\n, " $
          Map.foldrWithKey
            ( \k v accum ->
                T.concat [k, ": ", T.pack (show v)] : accum
            )
            []
            record

class Monad m => MonadState k v m | m -> k v where
  get :: k -> m (Maybe v)
  put :: k -> v -> m ()

instance Ord k => MonadState k v (ExceptT e (State (Map k v))) where
  get k = lift (Map.lookup k <$> State.get)
  put k v = lift . State.modify $ Map.insert k v

type TypeCheckingMonad = ExceptT Text (State (Map ([Text], Text) IpeType)) IpeType

run :: (a -> TypeCheckingMonad) -> a -> (Either Text IpeType, Map ([Text], Text) IpeType)
run typeCheckingFn value =
  State.runState
    (Except.runExceptT $ typeCheckingFn value)
    Map.empty

runWith :: Map ([Text], Text) IpeType -> (a -> TypeCheckingMonad) -> a -> (Either Text IpeType, Map ([Text], Text) IpeType)
runWith initialState typeCheckingFn value =
  State.runState
    (Except.runExceptT $ typeCheckingFn value)
    initialState
