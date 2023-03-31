{-# LANGUAGE OverloadedStrings #-}

module Ipe.TypeChecker.ModuleSpec (spec) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Ipe.Grammar
import Ipe.TypeChecker (Error (..), Type (..))
import qualified Ipe.TypeChecker as TypeChecker
import qualified Ipe.TypeChecker.Module as ModTypeChecker
import Test.Hspec

spec :: Spec
spec = describe "the module type checker" $ do
  addingTypeDefinitions
  addingTopLevelDefinitions
  usingImportedValues

sampleModule :: Module
sampleModule =
  Module
    { moduleDefinition = sampleModuleDefinition,
      moduleImports = [],
      typeDefinitions = [],
      topLevelDefinitions = []
    }

sampleModuleWithImport :: Maybe ([Text], Text) -> Module
sampleModuleWithImport alias =
  sampleModule
    { moduleImports =
        [ ImportExpression
            { importedModulePath = ["Some", "Imported"],
              importedModule = "Module",
              importedModuleAlias = alias
            }
        ]
    }

sampleImportedModule :: Module
sampleImportedModule =
  Module
    { moduleDefinition =
        sampleModuleDefinition
          { moduleDefinitionName = "Module",
            moduleDefinitionPath = ["Some", "Imported"],
            exportedDefinitions = ["UnionTest", "OpaqueTest", "A", "B", "C", "test", "test2"]
          },
      moduleImports = [],
      typeDefinitions =
        [ TypeUnionDefinition typeUnion0,
          TypeOpaqueDefinition typeOpaque0
        ],
      topLevelDefinitions = sampleTopLevelDefinitions
    }

sampleModuleDefinition :: ModuleDefinition
sampleModuleDefinition =
  ModuleDefinition
    { moduleDefinitionPath = [],
      moduleDefinitionName = "Test",
      exportedDefinitions = [],
      moduleDocComment = Nothing
    }

sampleTypeDefinitions0 :: [TypeDefinition]
sampleTypeDefinitions0 =
  [ TypeAliasDefinition $ typeAlias "AliasTest" [] $ ConcreteType [] "UnionTest" [],
    TypeUnionDefinition typeUnion0,
    TypeOpaqueDefinition typeOpaque0
  ]

typeUnion0 :: TypeUnion
typeUnion0 =
  typeUnion
    "UnionTest"
    ["a", "b"]
    [ ("A", [ConcreteType [] "Number" []]),
      ("B", [ConcreteType [] "String" [], ConcreteType [] "Number" [], ParameterType "b"]),
      ("C", [ConcreteType [] "OpaqueTest" [ParameterType "a", ParameterType "b"]])
    ]

typeOpaque0 :: TypeOpaque
typeOpaque0 =
  typeOpaque
    "OpaqueTest"
    ["a", "b"]
    [ ("OA", [ParameterType "a"]),
      ("OB", [ParameterType "b"])
    ]

tCustom0 :: TypeChecker.Type
tCustom0 =
  TCustom
    "UnionTest"
    [TVar "a", TVar "b"]
    [ ("A", [TNum]),
      ("B", [TStr, TNum, TVar "b"]),
      ("C", [tOpaque0])
    ]

tOpaque0 :: TypeChecker.Type
tOpaque0 =
  TCustom
    "OpaqueTest"
    [TVar "a", TVar "b"]
    [ ("OA", [TVar "a"]),
      ("OB", [TVar "b"])
    ]

importedTOpaque0 :: Maybe ([Text], Text) -> TypeChecker.Type
importedTOpaque0 alias =
  TCustom
    (prefixed "OpaqueTest")
    [TVar "a", TVar "b"]
    [ (prefixed "OA", [TVar "a"]),
      (prefixed "OB", [TVar "b"])
    ]
  where
    (prefixPath, prefixModule) = Maybe.fromMaybe (["Some", "Imported"], "Module") alias
    prefixed x = T.unpack $ T.intercalate "." (prefixPath ++ [prefixModule, x])

importedTCustom0 :: Maybe ([Text], Text) -> TypeChecker.Type
importedTCustom0 alias =
  TCustom
    (prefixed "UnionTest")
    [TVar "a", TVar "b"]
    [ (prefixed "A", [TNum]),
      (prefixed "B", [TStr, TNum, TVar "b"]),
      (prefixed "C", [importedTOpaque0 alias])
    ]
  where
    (prefixPath, prefixModule) = Maybe.fromMaybe (["Some", "Imported"], "Module") alias
    prefixed x = T.unpack $ T.intercalate "." (prefixPath ++ [prefixModule, x])

sampleTopLevelDefinitions :: [TopLevelDefinition]
sampleTopLevelDefinitions =
  [ topLevelDefinition
      "test"
      (IpeNumber 1)
      ( Just $
          TypeAnnotation
            { typeAnnotationName = "test",
              typeAnnotationArguments = [],
              typeAnnotationReturnType = ConcreteType [] "Number" []
            }
      ),
    topLevelDefinition "test2" (IpeString "abc") Nothing
  ]

typeAlias :: Text -> [Text] -> IpeType -> TypeAlias
typeAlias name parameters type_ =
  TypeAlias
    { typeAliasDefinitionName = name,
      typeAliasDefinitionParameters = parameters,
      typeAliasDefinitionDocComment = Nothing,
      typeAliasType = type_
    }

typeUnion :: Text -> [Text] -> [(Text, [IpeType])] -> TypeUnion
typeUnion name parameters constructors =
  TypeUnion
    { typeUnionDefinitionName = name,
      typeUnionDefinitionParameters = parameters,
      typeUnionDefinitionDocComment = Nothing,
      typeUnionDefinitionConstructors = map customTypeConstructor constructors
    }

typeOpaque :: Text -> [Text] -> [(Text, [IpeType])] -> TypeOpaque
typeOpaque name parameters constructors =
  TypeOpaque
    { typeOpaqueDefinitionName = name,
      typeOpaqueDefinitionParameters = parameters,
      typeOpaqueDefinitionDocComment = Nothing,
      typeOpaqueDefinitionConstructors = map customTypeConstructor constructors
    }

customTypeConstructor :: (Text, [IpeType]) -> CustomTypeConstructor
customTypeConstructor (name, args) =
  CustomTypeConstructor
    { customTypeConstructorName = name,
      customTypeConstructorDocComment = Nothing,
      customTypeConstructorArgs = args
    }

topLevelDefinition :: Text -> Expression -> Maybe TypeAnnotation -> TopLevelDefinition
topLevelDefinition name value annotation =
  TopLevelDefinition
    { topLevelDefinitionName = name,
      topLevelDefinitionDocComment = Nothing,
      topLevelDefinitionValue = value,
      topLevelDefinitionTypeAnnotation = annotation
    }

addingTypeDefinitions :: Spec
addingTypeDefinitions =
  describe "when adding type definitions" $ do
    it "should type check alias + union" $
      ModTypeChecker.run [] (sampleModule {typeDefinitions = sampleTypeDefinitions0})
        `shouldBe` Right
          ( Map.fromList
              [ ("Number", TNum),
                ("String", TStr),
                ("AliasTest", tCustom0),
                ("UnionTest", tCustom0),
                ("OpaqueTest", tOpaque0),
                ("A", TFun TNum tCustom0),
                ("B", TFun TStr (TFun TNum (TFun (TVar "b") tCustom0))),
                ("C", TFun tOpaque0 tCustom0),
                ("OA", TFun (TVar "a") tOpaque0),
                ("OB", TFun (TVar "b") tOpaque0)
              ]
          )

    it "should recognize when a type doesn't exist" $
      ModTypeChecker.run
        []
        ( sampleModule
            { typeDefinitions =
                [ TypeAliasDefinition . typeAlias "AliasTest" [] $
                    ConcreteType [] "UnionTest" []
                ]
            }
        )
        `shouldBe` Left (ModTypeChecker.UnknownType "UnionTest")

    it "should recognize when a type isn't declaring all variables" $
      ModTypeChecker.run
        []
        ( sampleModule
            { typeDefinitions =
                [ TypeAliasDefinition . typeAlias "AliasTest" ["a"] $
                    ConcreteType [] "UnionTest" [ParameterType "a", ParameterType "b"],
                  TypeUnionDefinition $
                    typeUnion
                      "UnionTest"
                      ["a", "b"]
                      [("A", [ConcreteType [] "Number" []])]
                ]
            }
        )
        `shouldBe` Left (ModTypeChecker.NotAllVariablesDeclared ["a", "b"] ["a"])

    it "should replace variables with concrete values" $
      ModTypeChecker.run
        []
        ( sampleModule
            { typeDefinitions =
                [ TypeAliasDefinition . typeAlias "AliasTest" [] $
                    ConcreteType [] "UnionTest" [ConcreteType [] "Number" []],
                  TypeUnionDefinition $
                    typeUnion
                      "UnionTest"
                      ["a"]
                      [("A", [ParameterType "a"])]
                ]
            }
        )
        `shouldBe` Right
          ( Map.fromList
              [ ("Number", TNum),
                ("String", TStr),
                ("AliasTest", TCustom "UnionTest" [TNum] [("A", [TNum])]),
                ("UnionTest", TCustom "UnionTest" [TVar "a"] [("A", [TVar "a"])]),
                ("A", TFun (TVar "a") (TCustom "UnionTest" [TVar "a"] [("A", [TVar "a"])]))
              ]
          )

    it "should replace variables with other variables" $
      ModTypeChecker.run
        []
        ( sampleModule
            { typeDefinitions =
                [ TypeAliasDefinition . typeAlias "AliasTest" ["newName"] $
                    ConcreteType [] "UnionTest" [ParameterType "newName"],
                  TypeUnionDefinition $
                    typeUnion
                      "UnionTest"
                      ["a"]
                      [("A", [ParameterType "a"])]
                ]
            }
        )
        `shouldBe` Right
          ( Map.fromList
              [ ("Number", TNum),
                ("String", TStr),
                ("AliasTest", TCustom "UnionTest" [TVar "newName"] [("A", [TVar "newName"])]),
                ("UnionTest", TCustom "UnionTest" [TVar "a"] [("A", [TVar "a"])]),
                ("A", TFun (TVar "a") (TCustom "UnionTest" [TVar "a"] [("A", [TVar "a"])]))
              ]
          )

addingTopLevelDefinitions :: Spec
addingTopLevelDefinitions =
  describe "when adding top level definitions" $ do
    it "should get correct values" $
      ModTypeChecker.run
        []
        ( sampleModule {topLevelDefinitions = sampleTopLevelDefinitions}
        )
        `shouldBe` Right
          ( Map.fromList
              [ ("test", TNum),
                ("test2", TStr),
                ("Number", TNum),
                ("String", TStr)
              ]
          )

    it "should check that the type annotation is correct" $
      ModTypeChecker.run
        []
        ( sampleModule
            { topLevelDefinitions =
                [ topLevelDefinition
                    "test"
                    (IpeNumber 1)
                    ( Just $
                        TypeAnnotation
                          { typeAnnotationName = "test",
                            typeAnnotationArguments = [],
                            typeAnnotationReturnType = ConcreteType [] "String" []
                          }
                    )
                ]
            }
        )
        `shouldBe` Left (ModTypeChecker.TopLevelDefinitionError "test" (NoMatch TNum TStr))

usingImportedValues :: Spec
usingImportedValues =
  describe "when using imported values" $ do
    it "should correctly import values and types" $
      ModTypeChecker.run [sampleImportedModule] (sampleModuleWithImport Nothing)
        `shouldBe` Right
          ( Map.fromList
              [ ( "Number",
                  TNum
                ),
                ( "String",
                  TStr
                ),
                ( "Some.Imported.Module.A",
                  TFun TNum (importedTCustom0 Nothing)
                ),
                ( "Some.Imported.Module.B",
                  TFun TStr (TFun TNum (TFun (TVar "b") (importedTCustom0 Nothing)))
                ),
                ( "Some.Imported.Module.C",
                  TFun (importedTOpaque0 Nothing) (importedTCustom0 Nothing)
                ),
                ( "Some.Imported.Module.OpaqueTest",
                  importedTOpaque0 Nothing
                ),
                ( "Some.Imported.Module.UnionTest",
                  importedTCustom0 Nothing
                ),
                ("Some.Imported.Module.test", TNum),
                ("Some.Imported.Module.test2", TStr)
              ]
          )
