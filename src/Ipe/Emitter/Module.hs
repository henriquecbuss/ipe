{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Ipe.Emitter.Module (emit) where

import Data.Text (Text)
import qualified Data.Text as T
import Ipe.Emitter
import qualified Ipe.Emitter.Expression
import Ipe.Grammar
import Prettyprinter

emit :: Module -> Doc ann
emit (Module {moduleDefinition, moduleImports, topLevelDefinitions}) =
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

emitImport :: [Text] -> ImportExpression -> Doc ann
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

emitTopLevelDefinition :: TopLevelDefinition -> Doc ann
emitTopLevelDefinition (TopLevelDefinition {topLevelDefinitionName, topLevelDefinitionValue}) =
  "const" <+> pretty topLevelDefinitionName <+> "=" <+> Ipe.Emitter.Expression.emit topLevelDefinitionValue <> ";"

emitModuleExports :: [Text] -> Doc ann
emitModuleExports exports =
  "export"
    <+> "default"
    <+> braces (commaSeparatedList (map pretty exports))
