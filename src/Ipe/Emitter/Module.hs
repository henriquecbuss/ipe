{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Ipe.Emitter.Module (emit) where

import Control.Monad.Trans.State (evalState)
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Ipe.Emitter.Expression
import Ipe.Emitter.Utils
import Ipe.Grammar
import Prettyprinter (Doc)

emit :: Module -> Doc a
emit m = evalState (emitHelper m) initialState

emitHelper :: Module -> EmitterMonad a
emitHelper (Module {moduleDefinition, moduleImports, topLevelDefinitions}) =
  ( if null moduleImports
      then emptyDoc
      else
        vsep (map (emitImport (moduleDefinitionPath moduleDefinition)) moduleImports)
          <> line
          <> line
  )
    <> ( if null topLevelDefinitions
           then emptyDoc
           else
             vsep (map (\tld -> emitTopLevelDefinition tld <> line) topLevelDefinitions)
               <> line
       )
    <> emitModuleExports (exportedDefinitions moduleDefinition)
    <> line

emitImport :: [Text] -> ImportExpression -> EmitterMonad a
emitImport currPath (ImportExpression {importedModulePath, importedModule, importedModuleAlias}) =
  "import"
    <+> pretty name
    <+> "from"
    <+> emitString (T.intercalate "/" (importPathPrefix ++ importedModulePath ++ [importedModule <> ".ipe"]))
  where
    name :: Text
    name = case importedModuleAlias of
      Nothing -> T.intercalate "_" (importedModulePath ++ [importedModule])
      Just (path, n) -> T.intercalate "_" (path ++ [n])

    importPathPrefix :: [Text]
    importPathPrefix =
      if null currPath
        then ["."]
        else replicate (length currPath) ".."

emitTopLevelDefinition :: TopLevelDefinition -> EmitterMonad a
emitTopLevelDefinition (TopLevelDefinition {topLevelDefinitionName, topLevelDefinitionValue}) =
  "const" <+> pretty topLevelDefinitionName <+> "=" <+> Ipe.Emitter.Expression.emitWithState topLevelDefinitionValue <> ";"

emitModuleExports :: [Text] -> EmitterMonad a
emitModuleExports exports =
  "export"
    <+> "default"
    <+> braces
      ( commaSeparatedList
          ( map pretty $
              -- Remove type definitions
              filter
                (not . Char.isUpper . T.head)
                exports
          )
      )
