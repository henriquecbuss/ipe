{-# LANGUAGE OverloadedStrings #-}

module Ipe.TypeChecker.ModuleSpec (spec) where

import qualified Data.Map as Map
import Data.Text (Text)
import Ipe.Grammar
import qualified Ipe.TypeChecker.Module as ModTypeChecker
import Ipe.TypeChecker.Utils (Error (..), Type (..))
import qualified Ipe.TypeChecker.Utils as TypeChecker
import Test.Hspec

spec :: Spec
spec = describe "the module type checker" $ do
  addingTypeDefinitions
  addingTopLevelDefinitions

sampleModule :: Module
sampleModule =
  Module
    { moduleDefinition = sampleModuleDefinition,
      moduleImports = [],
      typeDefinitions = [],
      topLevelDefinitions = []
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
      ModTypeChecker.run
        Map.empty
        ( sampleModule
            { typeDefinitions = sampleTypeDefinitions0,
              moduleDefinition =
                sampleModuleDefinition
                  { exportedDefinitions = ["AliasTest", "UnionTest", "OpaqueTest"]
                  }
            }
        )
        `shouldBe` Right
          ( Map.fromList
              [ ( ([], "Test"),
                  ( sampleModule
                      { typeDefinitions = sampleTypeDefinitions0,
                        moduleDefinition =
                          sampleModuleDefinition
                            { exportedDefinitions = ["AliasTest", "UnionTest", "OpaqueTest"]
                            }
                      },
                    Map.fromList
                      [ ("AliasTest", tCustom0),
                        ("UnionTest", tCustom0),
                        ("OpaqueTest", tOpaque0),
                        ("A", TFun TNum tCustom0),
                        ("B", TFun TStr (TFun TNum (TFun (TVar "b") tCustom0))),
                        ("C", TFun tOpaque0 tCustom0)
                      ]
                  )
                )
              ]
          )

    it "should recognize when a type doesn't exist" $
      ModTypeChecker.run
        Map.empty
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
        Map.empty
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
        Map.empty
        ( sampleModule
            { typeDefinitions =
                [ TypeAliasDefinition . typeAlias "AliasTest" [] $
                    ConcreteType [] "UnionTest" [ConcreteType [] "Number" []],
                  TypeUnionDefinition $
                    typeUnion
                      "UnionTest"
                      ["a"]
                      [("A", [ParameterType "a"])]
                ],
              moduleDefinition =
                sampleModuleDefinition
                  { exportedDefinitions = ["AliasTest", "UnionTest"]
                  }
            }
        )
        `shouldBe` Right
          ( Map.fromList
              [ ( ([], "Test"),
                  ( sampleModule
                      { typeDefinitions =
                          [ TypeAliasDefinition . typeAlias "AliasTest" [] $
                              ConcreteType [] "UnionTest" [ConcreteType [] "Number" []],
                            TypeUnionDefinition $
                              typeUnion
                                "UnionTest"
                                ["a"]
                                [("A", [ParameterType "a"])]
                          ],
                        moduleDefinition =
                          sampleModuleDefinition
                            { exportedDefinitions = ["AliasTest", "UnionTest"]
                            }
                      },
                    Map.fromList
                      [ ("AliasTest", TCustom "UnionTest" [TNum] [("A", [TNum])]),
                        ("UnionTest", TCustom "UnionTest" [TVar "a"] [("A", [TVar "a"])]),
                        ("A", TFun (TVar "a") (TCustom "UnionTest" [TVar "a"] [("A", [TVar "a"])]))
                      ]
                  )
                )
              ]
          )

    it "should replace variables with other variables" $
      ModTypeChecker.run
        Map.empty
        ( sampleModule
            { typeDefinitions =
                [ TypeAliasDefinition . typeAlias "AliasTest" ["newName"] $
                    ConcreteType [] "UnionTest" [ParameterType "newName"],
                  TypeUnionDefinition $
                    typeUnion
                      "UnionTest"
                      ["a"]
                      [("A", [ParameterType "a"])]
                ],
              moduleDefinition =
                sampleModuleDefinition
                  { exportedDefinitions =
                      ["AliasTest", "UnionTest"]
                  }
            }
        )
        `shouldBe` Right
          ( Map.fromList
              [ ( ([], "Test"),
                  ( sampleModule
                      { typeDefinitions =
                          [ TypeAliasDefinition . typeAlias "AliasTest" ["newName"] $
                              ConcreteType [] "UnionTest" [ParameterType "newName"],
                            TypeUnionDefinition $
                              typeUnion
                                "UnionTest"
                                ["a"]
                                [("A", [ParameterType "a"])]
                          ],
                        moduleDefinition =
                          sampleModuleDefinition
                            { exportedDefinitions =
                                ["AliasTest", "UnionTest"]
                            }
                      },
                    Map.fromList
                      [ ("AliasTest", TCustom "UnionTest" [TVar "newName"] [("A", [TVar "newName"])]),
                        ("UnionTest", TCustom "UnionTest" [TVar "a"] [("A", [TVar "a"])]),
                        ("A", TFun (TVar "a") (TCustom "UnionTest" [TVar "a"] [("A", [TVar "a"])]))
                      ]
                  )
                )
              ]
          )

addingTopLevelDefinitions :: Spec
addingTopLevelDefinitions =
  describe "when adding top level definitions" $ do
    it "should get correct values" $
      ModTypeChecker.run
        Map.empty
        ( sampleModule
            { topLevelDefinitions = sampleTopLevelDefinitions,
              moduleDefinition = sampleModuleDefinition {exportedDefinitions = ["test", "test2"]}
            }
        )
        `shouldBe` Right
          ( Map.fromList
              [ ( ([], "Test"),
                  ( sampleModule
                      { topLevelDefinitions = sampleTopLevelDefinitions,
                        moduleDefinition = sampleModuleDefinition {exportedDefinitions = ["test", "test2"]}
                      },
                    Map.fromList
                      [ ("test", TNum),
                        ("test2", TStr)
                      ]
                  )
                )
              ]
          )

    it "should check that the type annotation is correct" $
      ModTypeChecker.run
        Map.empty
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
