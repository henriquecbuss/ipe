{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Ipe.Emitter.Expression (emit, emitWithState) where

import qualified Control.Monad
import Control.Monad.Trans.State (evalState)
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Ipe.Emitter.Utils
import Ipe.Grammar
import qualified Prettyprinter

emit :: Expression -> Prettyprinter.Doc ann
emit expression = evalState (emitWithState expression) initialState

emitWithState :: Expression -> EmitterMonad a
emitWithState (IpeBinaryOperation op exp1 exp2) =
  case op of
    Add -> emitWithState exp1 <+> return "+" <+> emitWithState exp2
    Subtract -> emitWithState exp1 <+> "-" <+> emitWithState exp2
    Divide -> emitWithState exp1 <+> "/" <+> emitWithState exp2
    Multiply -> emitWithState exp1 <+> "*" <+> emitWithState exp2
    Exponentiation -> emitWithState exp1 <+> "**" <+> emitWithState exp2
    PipeRight -> emitWithState exp2 <> parens (emitWithState exp1)
    PipeLeft -> emitWithState exp1 <> parens (emitWithState exp2)
emitWithState (IpeNumber num) = pretty num
emitWithState (IpeString str) = emitString str
-- anonymous function called right away (IIFE)
emitWithState (IpeMatch matchExpr branches) = do
  constName <- T.pack . show <$> newVar

  parens
    ( "()"
        <+> "=>"
        <+> nest
          defaultNesting
          ( braces
              ( line
                  <> group ("const" <+> pretty constName <+> "=" <+> align (emitWithState matchExpr) <> ";")
                  <> line
                  <> vsep (map (patternMatchBranch constName) branches)
              )
              <> nest (-1 * defaultNesting) line
          )
    )
    <> "()"
emitWithState
  ( IpeFunctionCallOrValue
      ( FunctionCallOrValue
          { functionCallOrValuePath,
            functionCallOrValueName,
            functionCallOrValueRecordAccessors,
            functionCallOrValueArguments
          }
        )
    ) = do
    let nameParts = map pretty $ functionCallOrValuePath ++ [functionCallOrValueName] ++ functionCallOrValueRecordAccessors
    let name = concatWith (surround (align ".")) nameParts

    -- If is a type constructor
    if Char.isUpper $ T.head functionCallOrValueName
      then
        brackets $
          hsep
            ( emitString (T.intercalate "." (functionCallOrValuePath ++ [functionCallOrValueName]))
                : map (\x -> comma <+> emitWithState x) functionCallOrValueArguments
            )
      else name <> align (concatWith (<>) $ map (parens . emitWithState) functionCallOrValueArguments)
emitWithState (IpeFunction args body) = functionArguments args <+> functionBody body
emitWithState (IpeRecord fields) = braces (commaSeparatedList (map (uncurry recordField) fields))
emitWithState (IpeList items) = brackets (commaSeparatedList (map emitWithState items))

patternMatchBranch :: Text -> (IpeMatchPattern, [(Text, Expression)], Expression) -> EmitterMonad a
patternMatchBranch matchVariable (matchPattern, attributions, returnExpression) =
  case matchPattern of
    IpeWildCardPattern ->
      if null attributions
        then returnValue returnExpression
        else
          vsep
            [ vsep (map (uncurry attribution) attributions),
              returnValue returnExpression
            ]
    IpeVariablePattern varName ->
      if null attributions
        then
          vsep
            [ attribution varName (IpeFunctionCallOrValue (FunctionCallOrValue [] matchVariable [] [])),
              returnValue returnExpression
            ]
        else
          vsep
            [ attribution varName (IpeFunctionCallOrValue (FunctionCallOrValue [] matchVariable [] [])),
              vsep (map (uncurry attribution) attributions),
              returnValue returnExpression
            ]
    IpeCustomTypePattern path constructorName args ->
      "if"
        <+> parens
          ( pretty matchVariable <> brackets "0"
              <+> "==="
              <+> emitString (T.intercalate "." (path ++ [constructorName]))
          )
        <+> nest
          defaultNesting
          ( braces
              ( line
                  <> ( if null args
                         then emptyDoc
                         else
                           group
                             ( "const"
                                 <+> brackets
                                   ( commaSeparatedList ("_" : map pretty args)
                                   )
                                 <+> "="
                                 <+> pretty matchVariable
                                   <> ";"
                             )
                             <> line
                     )
                  <> ( if null attributions
                         then emptyDoc
                         else
                           vsep (map (uncurry attribution) attributions)
                             <> line
                     )
                  <> returnValue returnExpression
              )
              <> nest (-2 * defaultNesting) line
          )
    IpeLiteralNumberPattern num ->
      "if"
        <+> parens (pretty matchVariable <+> "===" <+> pretty num)
        <+> nest
          defaultNesting
          ( braces
              ( line
                  <> ( if null attributions
                         then emptyDoc
                         else
                           vsep (map (uncurry attribution) attributions)
                             <> line
                     )
                  <> returnValue returnExpression
              )
              <> nest (-2 * defaultNesting) line
          )
    IpeLiteralStringPattern str ->
      "if"
        <+> parens (pretty matchVariable <+> "===" <+> emitString str)
        <+> nest
          defaultNesting
          ( braces
              ( line
                  <> ( if null attributions
                         then emptyDoc
                         else
                           vsep (map (uncurry attribution) attributions)
                             <> line
                     )
                  <> returnValue returnExpression
              )
              <> nest (-2 * defaultNesting) line
          )
    IpeLiteralListPattern elements ->
      "if"
        <+> parens
          ( pretty matchVariable <> ".length"
              <+> "==="
              <+> pretty (length elements)
              <+> ( if null elements
                      then emptyDoc
                      else
                        "&&"
                          <+> hsep
                            ( punctuate
                                " && "
                                ( zipWith
                                    ( \element index ->
                                        checkElement
                                          ( matchVariable
                                              <> "["
                                              <> T.pack (show index)
                                              <> "]"
                                          )
                                          element
                                    )
                                    elements
                                    [0 :: Integer ..]
                                )
                            )
                  )
          )
        <+> nest
          defaultNesting
          ( braces
              ( line
                  <> ( if null innerLines
                         then emptyDoc
                         else
                           vsep innerLines
                             <> line
                     )
                  <> returnValue returnExpression
              )
              <> nest (-2 * defaultNesting) line
          )
      where
        innerLines :: [EmitterMonad a]
        innerLines =
          Maybe.catMaybes
            ( zipWith
                ( \element index ->
                    case element of
                      IpeWildCardPattern -> Nothing
                      IpeVariablePattern varName -> Just $ "const" <+> pretty varName <+> "=" <+> pretty matchVariable <> brackets (pretty index) <> ";"
                      IpeCustomTypePattern _path _constructorName _args -> undefined
                      IpeLiteralNumberPattern _ -> Nothing
                      IpeLiteralStringPattern _ -> Nothing
                      IpeLiteralListPattern _ -> undefined
                )
                elements
                [0 :: Integer ..]
            )
            ++ map (uncurry attribution) attributions

        checkElement :: Text -> IpeMatchPattern -> EmitterMonad a
        checkElement _ IpeWildCardPattern = "true"
        checkElement _ (IpeVariablePattern _) = "true"
        checkElement variable (IpeCustomTypePattern path constructor _args) =
          pretty variable <> brackets "0" <+> "===" <+> emitString (T.intercalate "." (path ++ [constructor]))
        checkElement variable (IpeLiteralNumberPattern num) = pretty variable <+> "===" <+> pretty num
        checkElement variable (IpeLiteralStringPattern str) = pretty variable <+> "===" <+> pretty str
        checkElement variable (IpeLiteralListPattern patterns) = do
          conditions <-
            Control.Monad.zipWithM
              ( \pat index ->
                  checkElement
                    (variable <> "[" <> T.pack (show index) <> "]")
                    pat
              )
              patterns
              [0 :: Integer ..]

          return $ Prettyprinter.hsep $ Prettyprinter.punctuate " &&" conditions

functionArguments :: [Text] -> EmitterMonad a
functionArguments [] = "()" <+> "=>"
functionArguments args =
  align $ group $ vsep (map (\arg -> pretty arg <+> "=>") args)

functionBody :: IpeFunctionBody -> EmitterMonad a
functionBody (IpeFunctionBody {functionBodyAttributions, functionReturn})
  | null functionBodyAttributions = braces ("return" <+> emitWithState functionReturn)
  | otherwise =
      nest defaultNesting $
        braces
          ( nest defaultNesting $
              line
                <> vsep (map (uncurry attribution) functionBodyAttributions)
                <> line
                <> returnValue functionReturn
          )

attribution :: Text -> Expression -> EmitterMonad a
attribution name expression = "const" <+> pretty name <+> "=" <+> emitWithState expression <> ";"

returnValue :: Expression -> EmitterMonad a
returnValue expression = "return" <+> nest defaultNesting (emitWithState expression) <> nest (-1 * defaultNesting) line

recordField :: Text -> Expression -> EmitterMonad a
recordField name expression = emitString name <> ":" <+> emitWithState expression
