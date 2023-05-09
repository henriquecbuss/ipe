{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Ipe.Emitter.Utils
  ( (<+>),
    Context (..),
    EmitterMonad,
    initialState,
    brackets,
    emptyDoc,
    commaSeparatedList,
    defaultNesting,
    newVar,
    group,
    vsep,
    hsep,
    emitString,
    nest,
    space,
    braces,
    comma,
    surround,
    concatWith,
    align,
    line,
    punctuate,
    parens,
    pretty,
    squotes,
  )
where

import Control.Monad.Trans.State (State, get, put)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter (Doc)
import qualified Prettyprinter

newtype Context = Context {lastVarIndex :: Int}

initialState :: Context
initialState = Context {lastVarIndex = 0}

type EmitterMonad a = State Context (Doc a)

instance IsString (EmitterMonad a) where
  fromString = pretty

instance Semigroup (EmitterMonad a) where
  left <> right = do
    leftResult <- left
    rightResult <- right

    return $ leftResult Prettyprinter.<> rightResult

(<+>) :: EmitterMonad a -> EmitterMonad a -> EmitterMonad a
left <+> right = do
  leftResult <- left
  rightResult <- right

  return $ leftResult Prettyprinter.<+> rightResult

commaSeparatedList :: [EmitterMonad a] -> EmitterMonad a
commaSeparatedList =
  align . concatWith (surround (comma <> space))

emitString :: Text -> EmitterMonad a
emitString str = squotes $ pretty $ T.replace "'" "\\'" str

nest :: Int -> EmitterMonad a -> EmitterMonad a
nest depth x = Prettyprinter.nest depth <$> x

space :: EmitterMonad a
space = return Prettyprinter.space

comma :: EmitterMonad a
comma = return Prettyprinter.comma

surround :: EmitterMonad a -> EmitterMonad a -> EmitterMonad a -> EmitterMonad a
surround x left right = do
  leftResult <- left
  rightResult <- right
  xResult <- x

  return $ leftResult Prettyprinter.<> xResult Prettyprinter.<> rightResult

concatWith :: (EmitterMonad a -> EmitterMonad a -> EmitterMonad a) -> [EmitterMonad a] -> EmitterMonad a
concatWith f ds
  | null ds = return mempty
  | otherwise = foldr1 f ds

align :: EmitterMonad a -> EmitterMonad a
align x = Prettyprinter.align <$> x

braces :: EmitterMonad a -> EmitterMonad a
braces x = Prettyprinter.braces <$> x

group :: EmitterMonad a -> EmitterMonad a
group x = Prettyprinter.group <$> x

line :: EmitterMonad a
line = return Prettyprinter.line

parens :: EmitterMonad a -> EmitterMonad a
parens x = Prettyprinter.parens <$> x

pretty :: (Prettyprinter.Pretty a) => a -> EmitterMonad b
pretty = return . Prettyprinter.pretty

squotes :: EmitterMonad a -> EmitterMonad a
squotes x = Prettyprinter.squotes <$> x

vsep :: [EmitterMonad a] -> EmitterMonad a
vsep = concatWith (\x y -> x <> line <> y)

hsep :: [EmitterMonad a] -> EmitterMonad a
hsep = concatWith (<>)

brackets :: EmitterMonad a -> EmitterMonad a
brackets x = Prettyprinter.brackets <$> x

defaultNesting :: Int
defaultNesting = 2

newVar :: EmitterMonad Text
newVar = do
  Context {lastVarIndex} <- get
  put $ Context {lastVarIndex = lastVarIndex + 1}
  return $ "var" <> Prettyprinter.pretty lastVarIndex

emptyDoc :: EmitterMonad a
emptyDoc = return Prettyprinter.emptyDoc

punctuate :: EmitterMonad a -> [EmitterMonad a] -> [EmitterMonad a]
punctuate _ [] = []
punctuate _ [d] = [d]
punctuate p (d : ds) = (d <> p) : punctuate p ds
