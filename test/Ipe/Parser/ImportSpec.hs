{-# LANGUAGE OverloadedStrings #-}

module Ipe.Parser.ImportSpec (spec) where

import qualified Ipe.Grammar
import qualified Ipe.Parser.Import
import Test.Hspec
import Test.Hspec.Megaparsec (elabel, err, shouldFailWith, shouldParse, ueof, utok)
import qualified Text.Megaparsec as Parsec.Common

spec :: Spec
spec = do
  singleParserSpec
  listParserSpec

singleParserSpec :: Spec
singleParserSpec =
  describe "the single import parser" $ do
    it "should parse a simple import" $
      Parsec.Common.parse
        Ipe.Parser.Import.singleParser
        ""
        "import SomeModule"
        `shouldParse` Ipe.Grammar.ImportExpression
          { Ipe.Grammar.importedModulePath = [],
            Ipe.Grammar.importedModule = "SomeModule",
            Ipe.Grammar.importedModuleAlias = Nothing
          }

    it "should parse a nested import" $
      Parsec.Common.parse
        Ipe.Parser.Import.singleParser
        ""
        "import Some.Module"
        `shouldParse` Ipe.Grammar.ImportExpression
          { Ipe.Grammar.importedModulePath = ["Some"],
            Ipe.Grammar.importedModule = "Module",
            Ipe.Grammar.importedModuleAlias = Nothing
          }

    it "should parse a simple import with an alias" $
      Parsec.Common.parse
        Ipe.Parser.Import.singleParser
        ""
        "import Some.Module as SomeAlias"
        `shouldParse` Ipe.Grammar.ImportExpression
          { Ipe.Grammar.importedModulePath = ["Some"],
            Ipe.Grammar.importedModule = "Module",
            Ipe.Grammar.importedModuleAlias = Just ([], "SomeAlias")
          }

    it "should parse a simple import with a nested alias" $
      Parsec.Common.parse
        Ipe.Parser.Import.singleParser
        ""
        "import Some.Module as Some.Alias"
        `shouldParse` Ipe.Grammar.ImportExpression
          { Ipe.Grammar.importedModulePath = ["Some"],
            Ipe.Grammar.importedModule = "Module",
            Ipe.Grammar.importedModuleAlias = Just (["Some"], "Alias")
          }

    it "should parse a nested import with an alias" $
      Parsec.Common.parse
        Ipe.Parser.Import.singleParser
        ""
        "import Some.Module as SomeAlias"
        `shouldParse` Ipe.Grammar.ImportExpression
          { Ipe.Grammar.importedModulePath = ["Some"],
            Ipe.Grammar.importedModule = "Module",
            Ipe.Grammar.importedModuleAlias = Just ([], "SomeAlias")
          }

    it "should parse a nested import with a nested alias" $
      Parsec.Common.parse
        Ipe.Parser.Import.singleParser
        ""
        "import Some.Module as Some.Alias"
        `shouldParse` Ipe.Grammar.ImportExpression
          { Ipe.Grammar.importedModulePath = ["Some"],
            Ipe.Grammar.importedModule = "Module",
            Ipe.Grammar.importedModuleAlias = Just (["Some"], "Alias")
          }

    it "should fail with a lower case letter after many dots" $
      Parsec.Common.parse
        Ipe.Parser.Import.singleParser
        ""
        "import Some.Very.Very.Very.Very.Deeply.nested.Module"
        `shouldFailWith` err 39 (utok 'n' <> elabel "uppercase letter")

    it "should fail when forgetting the alias name" $
      Parsec.Common.parse
        Ipe.Parser.Import.singleParser
        ""
        "import SomeModule as"
        `shouldFailWith` err 20 (ueof <> elabel "the alias for the imported module. It must be a valid module name, starting with an upper case letter, and containing only letters, numbers, `.` or `_`")

    it "should fail with an invalid alias" $
      Parsec.Common.parse
        Ipe.Parser.Import.singleParser
        ""
        "import SomeModule as !SomeAlias"
        `shouldFailWith` err 21 (utok '!' <> elabel "the alias for the imported module. It must be a valid module name, starting with an upper case letter, and containing only letters, numbers, `.` or `_`")

    it "should fail with an invalid nested alias" $
      Parsec.Common.parse
        Ipe.Parser.Import.singleParser
        ""
        "import SomeModule as Some.!Alias"
        `shouldFailWith` err 26 (utok '!' <> elabel "uppercase letter")

listParserSpec :: Spec
listParserSpec =
  describe "the import list parser" $ do
    it "should parse a single import" $
      Parsec.Common.parse
        Ipe.Parser.Import.listParser
        ""
        "import Some.Module as Some.Alias"
        `shouldParse` [ Ipe.Grammar.ImportExpression
                          { Ipe.Grammar.importedModulePath = ["Some"],
                            Ipe.Grammar.importedModule = "Module",
                            Ipe.Grammar.importedModuleAlias = Just (["Some"], "Alias")
                          }
                      ]

    it "should parse two imports" $
      Parsec.Common.parse
        Ipe.Parser.Import.listParser
        ""
        "import Some.Module as Some.Alias\n\
        \import Some.Other.Module"
        `shouldParse` [ Ipe.Grammar.ImportExpression
                          { Ipe.Grammar.importedModulePath = ["Some"],
                            Ipe.Grammar.importedModule = "Module",
                            Ipe.Grammar.importedModuleAlias = Just (["Some"], "Alias")
                          },
                        Ipe.Grammar.ImportExpression
                          { Ipe.Grammar.importedModulePath = ["Some", "Other"],
                            Ipe.Grammar.importedModule = "Module",
                            Ipe.Grammar.importedModuleAlias = Nothing
                          }
                      ]

    it "should parse a bunch of imports" $
      Parsec.Common.parse
        Ipe.Parser.Import.listParser
        ""
        "import Some.Module as Some.Alias\n\
        \import Some.Other.Module\n\
        \import Some.Other.Module2 as M2\n\
        \import Some.Other.Module3\n\
        \import Some.Other.Module4 as M4\n\
        \import Some.Other.Module5\n\
        \"
        `shouldParse` [ Ipe.Grammar.ImportExpression
                          { Ipe.Grammar.importedModulePath = ["Some"],
                            Ipe.Grammar.importedModule = "Module",
                            Ipe.Grammar.importedModuleAlias = Just (["Some"], "Alias")
                          },
                        Ipe.Grammar.ImportExpression
                          { Ipe.Grammar.importedModulePath = ["Some", "Other"],
                            Ipe.Grammar.importedModule = "Module",
                            Ipe.Grammar.importedModuleAlias = Nothing
                          },
                        Ipe.Grammar.ImportExpression
                          { Ipe.Grammar.importedModulePath = ["Some", "Other"],
                            Ipe.Grammar.importedModule = "Module2",
                            Ipe.Grammar.importedModuleAlias = Just ([], "M2")
                          },
                        Ipe.Grammar.ImportExpression
                          { Ipe.Grammar.importedModulePath = ["Some", "Other"],
                            Ipe.Grammar.importedModule = "Module3",
                            Ipe.Grammar.importedModuleAlias = Nothing
                          },
                        Ipe.Grammar.ImportExpression
                          { Ipe.Grammar.importedModulePath = ["Some", "Other"],
                            Ipe.Grammar.importedModule = "Module4",
                            Ipe.Grammar.importedModuleAlias = Just ([], "M4")
                          },
                        Ipe.Grammar.ImportExpression
                          { Ipe.Grammar.importedModulePath = ["Some", "Other"],
                            Ipe.Grammar.importedModule = "Module5",
                            Ipe.Grammar.importedModuleAlias = Nothing
                          }
                      ]
