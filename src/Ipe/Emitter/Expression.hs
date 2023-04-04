{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Ipe.Emitter.Expression (emit) where

import Data.Text (Text)
import qualified Data.Text as T
import Ipe.Emitter
import Ipe.Grammar
import Prettyprinter

emit :: Expression -> Doc ann
emit (IpeBinaryOperation op exp1 exp2) =
  case op of
    Add -> emit exp1 <+> "+" <+> emit exp2
    Subtract -> emit exp1 <+> "-" <+> emit exp2
    Divide -> emit exp1 <+> "/" <+> emit exp2
    Multiply -> emit exp1 <+> "*" <+> emit exp2
    Exponentiation -> emit exp1 <+> "**" <+> emit exp2
    PipeRight -> emit exp2 <> parens (emit exp1)
    PipeLeft -> emit exp1 <> parens (emit exp2)
emit (IpeNumber num) = pretty num
emit (IpeString str) = emitString str
-- anonymous function called right away (IIFE)
emit (IpeMatch matchExpr branches) =
  "()"
    <+> "=>"
    <+> nest
      defaultNesting
      ( braces
          ( line
              <> group ("const" <+> pretty defaultConstName <+> "=" <+> align (emit matchExpr) <> ";")
              <> line
              <> vsep (map patternMatchBranch branches)
          )
          <> nest (-1 * defaultNesting) line
      )
      <> "()"
emit
  ( IpeFunctionCallOrValue
      ( FunctionCallOrValue
          { functionCallOrValuePath,
            functionCallOrValueName,
            functionCallOrValueRecordAccessors,
            functionCallOrValueArguments
          }
        )
    ) =
    let parts = map pretty $ functionCallOrValuePath ++ [functionCallOrValueName] ++ functionCallOrValueRecordAccessors
     in concatWith (surround (align ".")) parts
          <> align (concatWith (<>) $ map (parens . emit) functionCallOrValueArguments)
emit (IpeFunction args body) = functionArguments args <+> functionBody body
emit (IpeRecord fields) = braces (commaSeparatedList (map (uncurry recordField) fields))

patternMatchBranch :: (IpeMatchPattern, [(Text, Expression)], Expression) -> Doc ann
patternMatchBranch (matchPattern, attributions, returnExpression) =
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
            [ attribution varName (IpeFunctionCallOrValue (FunctionCallOrValue [] defaultConstName [] [])),
              returnValue returnExpression
            ]
        else
          vsep
            [ attribution varName (IpeFunctionCallOrValue (FunctionCallOrValue [] defaultConstName [] [])),
              vsep (map (uncurry attribution) attributions),
              returnValue returnExpression
            ]
    IpeCustomTypePattern path constructorName args ->
      "if"
        <+> parens
          ( pretty defaultConstName <> brackets "0"
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
                                 <+> pretty defaultConstName
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
        <+> parens (pretty defaultConstName <+> "===" <+> pretty num)
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
        <+> parens (pretty defaultConstName <+> "===" <+> emitString str)
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

functionBody :: IpeFunctionBody -> Doc ann
functionBody (IpeFunctionBody {functionBodyAttributions, functionReturn})
  | null functionBodyAttributions = braces ("return" <+> emit functionReturn)
  | otherwise =
      nest defaultNesting $
        braces
          ( nest defaultNesting $
              line
                <> vsep (map (uncurry attribution) functionBodyAttributions)
                <> line
                <> returnValue functionReturn
          )

functionArguments :: [Text] -> Doc ann
functionArguments [] = "()" <+> "=>"
functionArguments args =
  align $ group $ vsep (map (\arg -> pretty arg <+> "=>") args)

attribution :: Text -> Expression -> Doc ann
attribution name expression = "const" <+> pretty name <+> "=" <+> emit expression <> ";"

returnValue :: Expression -> Doc ann
returnValue expression = "return" <+> nest defaultNesting (emit expression) <> nest (-1 * defaultNesting) line

recordField :: Text -> Expression -> Doc ann
recordField name expression = emitString name <> ":" <+> emit expression

defaultNesting :: Int
defaultNesting = 2

defaultConstName :: Text
defaultConstName = "__ipeConst"
