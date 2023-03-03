{-# LANGUAGE OverloadedStrings #-}

module Ipe.Parser.ModuleSpec (spec) where

import qualified Ipe.Grammar
import qualified Ipe.Parser.Module
import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse)
import qualified Text.Megaparsec as Parsec.Common

spec :: Spec
spec = do
  parserSpec

parserSpec :: Spec
parserSpec =
  describe "the module parser" $ do
    it "should parse a valid module" $
      Parsec.Common.parse
        Ipe.Parser.Module.parser
        ""
        "module Nested.Module exports [ fromString, OpaqueType ]\n\
        \/|* This is just a simple module that should be parsed */\n\
        \\n\
        \\n\
        \/|* Try to turn a String into an OpaqueType */\n\
        \fromString : String -> Maybe OpaqueType\n\
        \fromString =\n\
        \   \\stringValue ->\n\
        \       match Alias.Module.parseString stringValue with\n\
        \           | Just parsedString ->\n\
        \               Just (OpaqueTypeConstructor2 parsedString)\n\
        \\n\
        \           | Nothing ->\n\
        \               String.toNumber stringValue\n\
        \               |> Maybe.map OpaqueTypeConstructor\n\
        \\n\
        \\n"
        `shouldParse` Ipe.Grammar.Module
          { Ipe.Grammar.moduleDefinition =
              Ipe.Grammar.ModuleDefinition
                { Ipe.Grammar.moduleDefinitionPath = ["Nested"],
                  Ipe.Grammar.moduleDefinitionName = "Module",
                  Ipe.Grammar.exportedDefinitions = ["fromString", "OpaqueType"],
                  Ipe.Grammar.moduleDocComment = Just "This is just a simple module that should be parsed "
                },
            Ipe.Grammar.moduleImports = [],
            Ipe.Grammar.typeDefinitions = [],
            Ipe.Grammar.topLevelDefinitions =
              [ Ipe.Grammar.TopLevelDefinition
                  { Ipe.Grammar.topLevelDefinitionName = "fromString",
                    Ipe.Grammar.topLevelDefinitionDocComment = Just "Try to turn a String into an OpaqueType ",
                    Ipe.Grammar.topLevelDefinitionValue =
                      Ipe.Grammar.IpeFunction
                        ["stringValue"]
                        ( Ipe.Grammar.IpeFunctionBody
                            { Ipe.Grammar.attributions = [],
                              Ipe.Grammar.functionReturn =
                                Ipe.Grammar.IpeMatch
                                  ( Ipe.Grammar.IpeFunctionCallOrValue
                                      ( Ipe.Grammar.FunctionCallOrValue
                                          { Ipe.Grammar.functionCallOrValuePath = ["Alias", "Module"],
                                            Ipe.Grammar.functionCallOrValueName = "parseString",
                                            Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                            Ipe.Grammar.functionCallOrValueArguments =
                                              [ Ipe.Grammar.IpeFunctionCallOrValue
                                                  ( Ipe.Grammar.FunctionCallOrValue
                                                      { Ipe.Grammar.functionCallOrValuePath = [],
                                                        Ipe.Grammar.functionCallOrValueName = "stringValue",
                                                        Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                        Ipe.Grammar.functionCallOrValueArguments = []
                                                      }
                                                  )
                                              ]
                                          }
                                      )
                                  )
                                  [ ( Ipe.Grammar.IpeCustomTypePattern
                                        []
                                        "Just"
                                        [ Ipe.Grammar.IpeVariablePattern "parsedString"
                                        ],
                                      Ipe.Grammar.IpeFunctionCallOrValue
                                        ( Ipe.Grammar.FunctionCallOrValue
                                            { Ipe.Grammar.functionCallOrValuePath = [],
                                              Ipe.Grammar.functionCallOrValueName = "Just",
                                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                              Ipe.Grammar.functionCallOrValueArguments =
                                                [ Ipe.Grammar.IpeFunctionCallOrValue
                                                    ( Ipe.Grammar.FunctionCallOrValue
                                                        { Ipe.Grammar.functionCallOrValuePath = [],
                                                          Ipe.Grammar.functionCallOrValueName = "OpaqueTypeConstructor2",
                                                          Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                          Ipe.Grammar.functionCallOrValueArguments =
                                                            [ Ipe.Grammar.IpeFunctionCallOrValue
                                                                ( Ipe.Grammar.FunctionCallOrValue
                                                                    { Ipe.Grammar.functionCallOrValuePath = [],
                                                                      Ipe.Grammar.functionCallOrValueName = "parsedString",
                                                                      Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                                      Ipe.Grammar.functionCallOrValueArguments = []
                                                                    }
                                                                )
                                                            ]
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                    ),
                                    ( Ipe.Grammar.IpeCustomTypePattern
                                        []
                                        "Nothing"
                                        [],
                                      Ipe.Grammar.IpeBinaryOperation
                                        Ipe.Grammar.PipeRight
                                        ( Ipe.Grammar.IpeFunctionCallOrValue
                                            ( Ipe.Grammar.FunctionCallOrValue
                                                { Ipe.Grammar.functionCallOrValuePath = ["String"],
                                                  Ipe.Grammar.functionCallOrValueName = "toNumber",
                                                  Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                  Ipe.Grammar.functionCallOrValueArguments =
                                                    [ Ipe.Grammar.IpeFunctionCallOrValue
                                                        ( Ipe.Grammar.FunctionCallOrValue
                                                            { Ipe.Grammar.functionCallOrValuePath = [],
                                                              Ipe.Grammar.functionCallOrValueName = "stringValue",
                                                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                              Ipe.Grammar.functionCallOrValueArguments = []
                                                            }
                                                        )
                                                    ]
                                                }
                                            )
                                        )
                                        ( Ipe.Grammar.IpeFunctionCallOrValue
                                            ( Ipe.Grammar.FunctionCallOrValue
                                                { Ipe.Grammar.functionCallOrValuePath = ["Maybe"],
                                                  Ipe.Grammar.functionCallOrValueName = "map",
                                                  Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                  Ipe.Grammar.functionCallOrValueArguments =
                                                    [ Ipe.Grammar.IpeFunctionCallOrValue
                                                        ( Ipe.Grammar.FunctionCallOrValue
                                                            { Ipe.Grammar.functionCallOrValuePath = [],
                                                              Ipe.Grammar.functionCallOrValueName = "OpaqueTypeConstructor",
                                                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                              Ipe.Grammar.functionCallOrValueArguments = []
                                                            }
                                                        )
                                                    ]
                                                }
                                            )
                                        )
                                    )
                                  ]
                            }
                        ),
                    Ipe.Grammar.topLevelDefinitionTypeAnnotation =
                      Just $
                        Ipe.Grammar.TypeAnnotation
                          { Ipe.Grammar.typeAnnotationName = "fromString",
                            Ipe.Grammar.typeAnnotationArguments = [Ipe.Grammar.ConcreteType [] "String" []],
                            Ipe.Grammar.typeAnnotationReturnType = Ipe.Grammar.ConcreteType [] "Maybe" [Ipe.Grammar.ConcreteType [] "OpaqueType" []]
                          }
                  }
              ]
          }

    it "should parse a valid module with types" $
      Parsec.Common.parse
        Ipe.Parser.Module.parser
        ""
        "module Nested.Module exports [ OpaqueType ]\n\
        \/|* This is just a simple module that should be parsed */\n\
        \\n\
        \\n\
        \import RootModule\n\
        \import Nested.Module as Alias.Module\n\
        \\n\
        \\n\
        \\n\
        \/|* Some opaque type */\n\
        \type opaque OpaqueType =\n\
        \   /|* Some constructor */\n\
        \   | OpaqueTypeConstructor Number\n\
        \   | OpaqueTypeConstructor2 RootModule.Type\n\
        \\n\
        \\n\
        \type alias AliasType a = OpaqueType\n\
        \\n"
        `shouldParse` Ipe.Grammar.Module
          { Ipe.Grammar.moduleDefinition =
              Ipe.Grammar.ModuleDefinition
                { Ipe.Grammar.moduleDefinitionPath = ["Nested"],
                  Ipe.Grammar.moduleDefinitionName = "Module",
                  Ipe.Grammar.exportedDefinitions = ["OpaqueType"],
                  Ipe.Grammar.moduleDocComment = Just "This is just a simple module that should be parsed "
                },
            Ipe.Grammar.moduleImports =
              [ Ipe.Grammar.ImportExpression
                  { Ipe.Grammar.importedModulePath = [],
                    Ipe.Grammar.importedModule = "RootModule",
                    Ipe.Grammar.importedModuleAlias = Nothing
                  },
                Ipe.Grammar.ImportExpression
                  { Ipe.Grammar.importedModulePath = ["Nested"],
                    Ipe.Grammar.importedModule = "Module",
                    Ipe.Grammar.importedModuleAlias = Just (["Alias"], "Module")
                  }
              ],
            Ipe.Grammar.typeDefinitions =
              [ Ipe.Grammar.TypeOpaqueDefinition
                  ( Ipe.Grammar.TypeOpaque
                      { Ipe.Grammar.typeOpaqueDefinitionName = "OpaqueType",
                        Ipe.Grammar.typeOpaqueDefinitionParameters = [],
                        Ipe.Grammar.typeOpaqueDefinitionDocComment = Just "Some opaque type ",
                        Ipe.Grammar.typeOpaqueDefinitionConstructors =
                          [ Ipe.Grammar.CustomTypeConstructor
                              { Ipe.Grammar.customTypeConstructorName = "OpaqueTypeConstructor",
                                Ipe.Grammar.customTypeConstructorDocComment = Just "Some constructor ",
                                Ipe.Grammar.customTypeConstructorArgs = [Ipe.Grammar.ConcreteType [] "Number" []]
                              },
                            Ipe.Grammar.CustomTypeConstructor
                              { Ipe.Grammar.customTypeConstructorName = "OpaqueTypeConstructor2",
                                Ipe.Grammar.customTypeConstructorDocComment = Nothing,
                                Ipe.Grammar.customTypeConstructorArgs = [Ipe.Grammar.ConcreteType ["RootModule"] "Type" []]
                              }
                          ]
                      }
                  ),
                Ipe.Grammar.TypeAliasDefinition
                  ( Ipe.Grammar.TypeAlias
                      { Ipe.Grammar.typeAliasDefinitionName = "AliasType",
                        Ipe.Grammar.typeAliasDefinitionParameters = ["a"],
                        Ipe.Grammar.typeAliasDefinitionDocComment = Nothing,
                        Ipe.Grammar.typeAliasType = Ipe.Grammar.ConcreteType [] "OpaqueType" []
                      }
                  )
              ],
            Ipe.Grammar.topLevelDefinitions = []
          }

    it "should parse a valid module with functions" $
      Parsec.Common.parse
        Ipe.Parser.Module.parser
        ""
        "module Nested.Module exports [ fromString, OpaqueType ]\n\
        \/|* This is just a simple module that should be parsed */\n\
        \\n\
        \\n\
        \import RootModule\n\
        \import Nested.Module as Alias.Module\n\
        \\n\
        \\n\
        \\n\
        \/|* Try to turn a String into an OpaqueType */\n\
        \fromString : String -> Maybe OpaqueType\n\
        \fromString =\n\
        \   \\stringValue ->\n\
        \       match Alias.Module.parseString stringValue with\n\
        \           | Just parsedString ->\n\
        \               Just (OpaqueTypeConstructor2 parsedString)\n\
        \\n\
        \           | Nothing ->\n\
        \               String.toNumber stringValue\n\
        \               |> Maybe.map OpaqueTypeConstructor\n\
        \\n\
        \\n\
        \toString : OpaqueType -> String\n\
        \toString =\n\
        \   \\opaqueType ->\n\
        \       match opaqueType with\n\
        \           | OpaqueTypeConstructor number ->\n\
        \               String.fromNumber number\n\
        \\n\
        \           | OpaqueTypeConstructor2 rootModuleType ->\n\
        \               Alias.Module.toString rootModuleType\n\
        \\n"
        `shouldParse` Ipe.Grammar.Module
          { Ipe.Grammar.moduleDefinition =
              Ipe.Grammar.ModuleDefinition
                { Ipe.Grammar.moduleDefinitionPath = ["Nested"],
                  Ipe.Grammar.moduleDefinitionName = "Module",
                  Ipe.Grammar.exportedDefinitions = ["fromString", "OpaqueType"],
                  Ipe.Grammar.moduleDocComment = Just "This is just a simple module that should be parsed "
                },
            Ipe.Grammar.moduleImports =
              [ Ipe.Grammar.ImportExpression
                  { Ipe.Grammar.importedModulePath = [],
                    Ipe.Grammar.importedModule = "RootModule",
                    Ipe.Grammar.importedModuleAlias = Nothing
                  },
                Ipe.Grammar.ImportExpression
                  { Ipe.Grammar.importedModulePath = ["Nested"],
                    Ipe.Grammar.importedModule = "Module",
                    Ipe.Grammar.importedModuleAlias = Just (["Alias"], "Module")
                  }
              ],
            Ipe.Grammar.typeDefinitions = [],
            Ipe.Grammar.topLevelDefinitions =
              [ Ipe.Grammar.TopLevelDefinition
                  { Ipe.Grammar.topLevelDefinitionName = "fromString",
                    Ipe.Grammar.topLevelDefinitionDocComment = Just "Try to turn a String into an OpaqueType ",
                    Ipe.Grammar.topLevelDefinitionValue =
                      Ipe.Grammar.IpeFunction
                        ["stringValue"]
                        ( Ipe.Grammar.IpeFunctionBody
                            { Ipe.Grammar.attributions = [],
                              Ipe.Grammar.functionReturn =
                                Ipe.Grammar.IpeMatch
                                  ( Ipe.Grammar.IpeFunctionCallOrValue
                                      ( Ipe.Grammar.FunctionCallOrValue
                                          { Ipe.Grammar.functionCallOrValuePath = ["Alias", "Module"],
                                            Ipe.Grammar.functionCallOrValueName = "parseString",
                                            Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                            Ipe.Grammar.functionCallOrValueArguments =
                                              [ Ipe.Grammar.IpeFunctionCallOrValue
                                                  ( Ipe.Grammar.FunctionCallOrValue
                                                      { Ipe.Grammar.functionCallOrValuePath = [],
                                                        Ipe.Grammar.functionCallOrValueName = "stringValue",
                                                        Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                        Ipe.Grammar.functionCallOrValueArguments = []
                                                      }
                                                  )
                                              ]
                                          }
                                      )
                                  )
                                  [ ( Ipe.Grammar.IpeCustomTypePattern
                                        []
                                        "Just"
                                        [ Ipe.Grammar.IpeVariablePattern "parsedString"
                                        ],
                                      Ipe.Grammar.IpeFunctionCallOrValue
                                        ( Ipe.Grammar.FunctionCallOrValue
                                            { Ipe.Grammar.functionCallOrValuePath = [],
                                              Ipe.Grammar.functionCallOrValueName = "Just",
                                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                              Ipe.Grammar.functionCallOrValueArguments =
                                                [ Ipe.Grammar.IpeFunctionCallOrValue
                                                    ( Ipe.Grammar.FunctionCallOrValue
                                                        { Ipe.Grammar.functionCallOrValuePath = [],
                                                          Ipe.Grammar.functionCallOrValueName = "OpaqueTypeConstructor2",
                                                          Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                          Ipe.Grammar.functionCallOrValueArguments =
                                                            [ Ipe.Grammar.IpeFunctionCallOrValue
                                                                ( Ipe.Grammar.FunctionCallOrValue
                                                                    { Ipe.Grammar.functionCallOrValuePath = [],
                                                                      Ipe.Grammar.functionCallOrValueName = "parsedString",
                                                                      Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                                      Ipe.Grammar.functionCallOrValueArguments = []
                                                                    }
                                                                )
                                                            ]
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                    ),
                                    ( Ipe.Grammar.IpeCustomTypePattern
                                        []
                                        "Nothing"
                                        [],
                                      Ipe.Grammar.IpeBinaryOperation
                                        Ipe.Grammar.PipeRight
                                        ( Ipe.Grammar.IpeFunctionCallOrValue
                                            ( Ipe.Grammar.FunctionCallOrValue
                                                { Ipe.Grammar.functionCallOrValuePath = ["String"],
                                                  Ipe.Grammar.functionCallOrValueName = "toNumber",
                                                  Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                  Ipe.Grammar.functionCallOrValueArguments =
                                                    [ Ipe.Grammar.IpeFunctionCallOrValue
                                                        ( Ipe.Grammar.FunctionCallOrValue
                                                            { Ipe.Grammar.functionCallOrValuePath = [],
                                                              Ipe.Grammar.functionCallOrValueName = "stringValue",
                                                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                              Ipe.Grammar.functionCallOrValueArguments = []
                                                            }
                                                        )
                                                    ]
                                                }
                                            )
                                        )
                                        ( Ipe.Grammar.IpeFunctionCallOrValue
                                            ( Ipe.Grammar.FunctionCallOrValue
                                                { Ipe.Grammar.functionCallOrValuePath = ["Maybe"],
                                                  Ipe.Grammar.functionCallOrValueName = "map",
                                                  Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                  Ipe.Grammar.functionCallOrValueArguments =
                                                    [ Ipe.Grammar.IpeFunctionCallOrValue
                                                        ( Ipe.Grammar.FunctionCallOrValue
                                                            { Ipe.Grammar.functionCallOrValuePath = [],
                                                              Ipe.Grammar.functionCallOrValueName = "OpaqueTypeConstructor",
                                                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                              Ipe.Grammar.functionCallOrValueArguments = []
                                                            }
                                                        )
                                                    ]
                                                }
                                            )
                                        )
                                    )
                                  ]
                            }
                        ),
                    Ipe.Grammar.topLevelDefinitionTypeAnnotation =
                      Just $
                        Ipe.Grammar.TypeAnnotation
                          { Ipe.Grammar.typeAnnotationName = "fromString",
                            Ipe.Grammar.typeAnnotationArguments = [Ipe.Grammar.ConcreteType [] "String" []],
                            Ipe.Grammar.typeAnnotationReturnType = Ipe.Grammar.ConcreteType [] "Maybe" [Ipe.Grammar.ConcreteType [] "OpaqueType" []]
                          }
                  },
                Ipe.Grammar.TopLevelDefinition
                  { Ipe.Grammar.topLevelDefinitionName = "toString",
                    Ipe.Grammar.topLevelDefinitionDocComment = Nothing,
                    Ipe.Grammar.topLevelDefinitionValue =
                      Ipe.Grammar.IpeFunction
                        ["opaqueType"]
                        ( Ipe.Grammar.IpeFunctionBody
                            { Ipe.Grammar.attributions = [],
                              Ipe.Grammar.functionReturn =
                                Ipe.Grammar.IpeMatch
                                  ( Ipe.Grammar.IpeFunctionCallOrValue
                                      ( Ipe.Grammar.FunctionCallOrValue
                                          { Ipe.Grammar.functionCallOrValuePath = [],
                                            Ipe.Grammar.functionCallOrValueName = "opaqueType",
                                            Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                            Ipe.Grammar.functionCallOrValueArguments = []
                                          }
                                      )
                                  )
                                  [ ( Ipe.Grammar.IpeCustomTypePattern
                                        []
                                        "OpaqueTypeConstructor"
                                        [ Ipe.Grammar.IpeVariablePattern "number"
                                        ],
                                      Ipe.Grammar.IpeFunctionCallOrValue
                                        ( Ipe.Grammar.FunctionCallOrValue
                                            { Ipe.Grammar.functionCallOrValuePath = ["String"],
                                              Ipe.Grammar.functionCallOrValueName = "fromNumber",
                                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                              Ipe.Grammar.functionCallOrValueArguments =
                                                [ Ipe.Grammar.IpeFunctionCallOrValue
                                                    ( Ipe.Grammar.FunctionCallOrValue
                                                        { Ipe.Grammar.functionCallOrValuePath = [],
                                                          Ipe.Grammar.functionCallOrValueName = "number",
                                                          Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                          Ipe.Grammar.functionCallOrValueArguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                    ),
                                    ( Ipe.Grammar.IpeCustomTypePattern
                                        []
                                        "OpaqueTypeConstructor2"
                                        [Ipe.Grammar.IpeVariablePattern "rootModuleType"],
                                      Ipe.Grammar.IpeFunctionCallOrValue
                                        ( Ipe.Grammar.FunctionCallOrValue
                                            { Ipe.Grammar.functionCallOrValuePath = ["Alias", "Module"],
                                              Ipe.Grammar.functionCallOrValueName = "toString",
                                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                              Ipe.Grammar.functionCallOrValueArguments =
                                                [ Ipe.Grammar.IpeFunctionCallOrValue
                                                    ( Ipe.Grammar.FunctionCallOrValue
                                                        { Ipe.Grammar.functionCallOrValuePath = [],
                                                          Ipe.Grammar.functionCallOrValueName = "rootModuleType",
                                                          Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                          Ipe.Grammar.functionCallOrValueArguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                    )
                                  ]
                            }
                        ),
                    Ipe.Grammar.topLevelDefinitionTypeAnnotation =
                      Just $
                        Ipe.Grammar.TypeAnnotation
                          { Ipe.Grammar.typeAnnotationName = "toString",
                            Ipe.Grammar.typeAnnotationArguments = [Ipe.Grammar.ConcreteType [] "OpaqueType" []],
                            Ipe.Grammar.typeAnnotationReturnType = Ipe.Grammar.ConcreteType [] "String" []
                          }
                  }
              ]
          }

    it "should parse a valid module with types and functions" $
      Parsec.Common.parse
        Ipe.Parser.Module.parser
        ""
        "module Nested.Module exports [ fromString, OpaqueType ]\n\
        \/|* This is just a simple module that should be parsed */\n\
        \\n\
        \\n\
        \import RootModule\n\
        \import Nested.Module as Alias.Module\n\
        \\n\
        \\n\
        \\n\
        \/|* Some opaque type */\n\
        \type opaque OpaqueType =\n\
        \   /|* Some constructor */\n\
        \   | OpaqueTypeConstructor Number\n\
        \   | OpaqueTypeConstructor2 RootModule.Type\n\
        \\n\
        \\n\
        \/|* Try to turn a String into an OpaqueType */\n\
        \fromString : String -> Maybe OpaqueType\n\
        \fromString =\n\
        \   \\stringValue ->\n\
        \       match Alias.Module.parseString stringValue with\n\
        \           | Just parsedString ->\n\
        \               Just (OpaqueTypeConstructor2 parsedString)\n\
        \\n\
        \           | Nothing ->\n\
        \               String.toNumber stringValue\n\
        \               |> Maybe.map OpaqueTypeConstructor\n\
        \\n\
        \\n\
        \toString : OpaqueType -> String\n\
        \toString =\n\
        \   \\opaqueType ->\n\
        \       match opaqueType with\n\
        \           | OpaqueTypeConstructor number ->\n\
        \               String.fromNumber number\n\
        \\n\
        \           | OpaqueTypeConstructor2 rootModuleType ->\n\
        \               Alias.Module.toString rootModuleType\n\
        \\n\
        \\n\
        \type alias AliasType a = OpaqueType\n\
        \\n"
        `shouldParse` Ipe.Grammar.Module
          { Ipe.Grammar.moduleDefinition =
              Ipe.Grammar.ModuleDefinition
                { Ipe.Grammar.moduleDefinitionPath = ["Nested"],
                  Ipe.Grammar.moduleDefinitionName = "Module",
                  Ipe.Grammar.exportedDefinitions = ["fromString", "OpaqueType"],
                  Ipe.Grammar.moduleDocComment = Just "This is just a simple module that should be parsed "
                },
            Ipe.Grammar.moduleImports =
              [ Ipe.Grammar.ImportExpression
                  { Ipe.Grammar.importedModulePath = [],
                    Ipe.Grammar.importedModule = "RootModule",
                    Ipe.Grammar.importedModuleAlias = Nothing
                  },
                Ipe.Grammar.ImportExpression
                  { Ipe.Grammar.importedModulePath = ["Nested"],
                    Ipe.Grammar.importedModule = "Module",
                    Ipe.Grammar.importedModuleAlias = Just (["Alias"], "Module")
                  }
              ],
            Ipe.Grammar.typeDefinitions =
              [ Ipe.Grammar.TypeOpaqueDefinition
                  ( Ipe.Grammar.TypeOpaque
                      { Ipe.Grammar.typeOpaqueDefinitionName = "OpaqueType",
                        Ipe.Grammar.typeOpaqueDefinitionParameters = [],
                        Ipe.Grammar.typeOpaqueDefinitionDocComment = Just "Some opaque type ",
                        Ipe.Grammar.typeOpaqueDefinitionConstructors =
                          [ Ipe.Grammar.CustomTypeConstructor
                              { Ipe.Grammar.customTypeConstructorName = "OpaqueTypeConstructor",
                                Ipe.Grammar.customTypeConstructorDocComment = Just "Some constructor ",
                                Ipe.Grammar.customTypeConstructorArgs = [Ipe.Grammar.ConcreteType [] "Number" []]
                              },
                            Ipe.Grammar.CustomTypeConstructor
                              { Ipe.Grammar.customTypeConstructorName = "OpaqueTypeConstructor2",
                                Ipe.Grammar.customTypeConstructorDocComment = Nothing,
                                Ipe.Grammar.customTypeConstructorArgs = [Ipe.Grammar.ConcreteType ["RootModule"] "Type" []]
                              }
                          ]
                      }
                  ),
                Ipe.Grammar.TypeAliasDefinition
                  ( Ipe.Grammar.TypeAlias
                      { Ipe.Grammar.typeAliasDefinitionName = "AliasType",
                        Ipe.Grammar.typeAliasDefinitionParameters = ["a"],
                        Ipe.Grammar.typeAliasDefinitionDocComment = Nothing,
                        Ipe.Grammar.typeAliasType = Ipe.Grammar.ConcreteType [] "OpaqueType" []
                      }
                  )
              ],
            Ipe.Grammar.topLevelDefinitions =
              [ Ipe.Grammar.TopLevelDefinition
                  { Ipe.Grammar.topLevelDefinitionName = "fromString",
                    Ipe.Grammar.topLevelDefinitionDocComment = Just "Try to turn a String into an OpaqueType ",
                    Ipe.Grammar.topLevelDefinitionValue =
                      Ipe.Grammar.IpeFunction
                        ["stringValue"]
                        ( Ipe.Grammar.IpeFunctionBody
                            { Ipe.Grammar.attributions = [],
                              Ipe.Grammar.functionReturn =
                                Ipe.Grammar.IpeMatch
                                  ( Ipe.Grammar.IpeFunctionCallOrValue
                                      ( Ipe.Grammar.FunctionCallOrValue
                                          { Ipe.Grammar.functionCallOrValuePath = ["Alias", "Module"],
                                            Ipe.Grammar.functionCallOrValueName = "parseString",
                                            Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                            Ipe.Grammar.functionCallOrValueArguments =
                                              [ Ipe.Grammar.IpeFunctionCallOrValue
                                                  ( Ipe.Grammar.FunctionCallOrValue
                                                      { Ipe.Grammar.functionCallOrValuePath = [],
                                                        Ipe.Grammar.functionCallOrValueName = "stringValue",
                                                        Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                        Ipe.Grammar.functionCallOrValueArguments = []
                                                      }
                                                  )
                                              ]
                                          }
                                      )
                                  )
                                  [ ( Ipe.Grammar.IpeCustomTypePattern
                                        []
                                        "Just"
                                        [ Ipe.Grammar.IpeVariablePattern "parsedString"
                                        ],
                                      Ipe.Grammar.IpeFunctionCallOrValue
                                        ( Ipe.Grammar.FunctionCallOrValue
                                            { Ipe.Grammar.functionCallOrValuePath = [],
                                              Ipe.Grammar.functionCallOrValueName = "Just",
                                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                              Ipe.Grammar.functionCallOrValueArguments =
                                                [ Ipe.Grammar.IpeFunctionCallOrValue
                                                    ( Ipe.Grammar.FunctionCallOrValue
                                                        { Ipe.Grammar.functionCallOrValuePath = [],
                                                          Ipe.Grammar.functionCallOrValueName = "OpaqueTypeConstructor2",
                                                          Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                          Ipe.Grammar.functionCallOrValueArguments =
                                                            [ Ipe.Grammar.IpeFunctionCallOrValue
                                                                ( Ipe.Grammar.FunctionCallOrValue
                                                                    { Ipe.Grammar.functionCallOrValuePath = [],
                                                                      Ipe.Grammar.functionCallOrValueName = "parsedString",
                                                                      Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                                      Ipe.Grammar.functionCallOrValueArguments = []
                                                                    }
                                                                )
                                                            ]
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                    ),
                                    ( Ipe.Grammar.IpeCustomTypePattern
                                        []
                                        "Nothing"
                                        [],
                                      Ipe.Grammar.IpeBinaryOperation
                                        Ipe.Grammar.PipeRight
                                        ( Ipe.Grammar.IpeFunctionCallOrValue
                                            ( Ipe.Grammar.FunctionCallOrValue
                                                { Ipe.Grammar.functionCallOrValuePath = ["String"],
                                                  Ipe.Grammar.functionCallOrValueName = "toNumber",
                                                  Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                  Ipe.Grammar.functionCallOrValueArguments =
                                                    [ Ipe.Grammar.IpeFunctionCallOrValue
                                                        ( Ipe.Grammar.FunctionCallOrValue
                                                            { Ipe.Grammar.functionCallOrValuePath = [],
                                                              Ipe.Grammar.functionCallOrValueName = "stringValue",
                                                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                              Ipe.Grammar.functionCallOrValueArguments = []
                                                            }
                                                        )
                                                    ]
                                                }
                                            )
                                        )
                                        ( Ipe.Grammar.IpeFunctionCallOrValue
                                            ( Ipe.Grammar.FunctionCallOrValue
                                                { Ipe.Grammar.functionCallOrValuePath = ["Maybe"],
                                                  Ipe.Grammar.functionCallOrValueName = "map",
                                                  Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                  Ipe.Grammar.functionCallOrValueArguments =
                                                    [ Ipe.Grammar.IpeFunctionCallOrValue
                                                        ( Ipe.Grammar.FunctionCallOrValue
                                                            { Ipe.Grammar.functionCallOrValuePath = [],
                                                              Ipe.Grammar.functionCallOrValueName = "OpaqueTypeConstructor",
                                                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                              Ipe.Grammar.functionCallOrValueArguments = []
                                                            }
                                                        )
                                                    ]
                                                }
                                            )
                                        )
                                    )
                                  ]
                            }
                        ),
                    Ipe.Grammar.topLevelDefinitionTypeAnnotation =
                      Just $
                        Ipe.Grammar.TypeAnnotation
                          { Ipe.Grammar.typeAnnotationName = "fromString",
                            Ipe.Grammar.typeAnnotationArguments = [Ipe.Grammar.ConcreteType [] "String" []],
                            Ipe.Grammar.typeAnnotationReturnType = Ipe.Grammar.ConcreteType [] "Maybe" [Ipe.Grammar.ConcreteType [] "OpaqueType" []]
                          }
                  },
                Ipe.Grammar.TopLevelDefinition
                  { Ipe.Grammar.topLevelDefinitionName = "toString",
                    Ipe.Grammar.topLevelDefinitionDocComment = Nothing,
                    Ipe.Grammar.topLevelDefinitionValue =
                      Ipe.Grammar.IpeFunction
                        ["opaqueType"]
                        ( Ipe.Grammar.IpeFunctionBody
                            { Ipe.Grammar.attributions = [],
                              Ipe.Grammar.functionReturn =
                                Ipe.Grammar.IpeMatch
                                  ( Ipe.Grammar.IpeFunctionCallOrValue
                                      ( Ipe.Grammar.FunctionCallOrValue
                                          { Ipe.Grammar.functionCallOrValuePath = [],
                                            Ipe.Grammar.functionCallOrValueName = "opaqueType",
                                            Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                            Ipe.Grammar.functionCallOrValueArguments = []
                                          }
                                      )
                                  )
                                  [ ( Ipe.Grammar.IpeCustomTypePattern
                                        []
                                        "OpaqueTypeConstructor"
                                        [ Ipe.Grammar.IpeVariablePattern "number"
                                        ],
                                      Ipe.Grammar.IpeFunctionCallOrValue
                                        ( Ipe.Grammar.FunctionCallOrValue
                                            { Ipe.Grammar.functionCallOrValuePath = ["String"],
                                              Ipe.Grammar.functionCallOrValueName = "fromNumber",
                                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                              Ipe.Grammar.functionCallOrValueArguments =
                                                [ Ipe.Grammar.IpeFunctionCallOrValue
                                                    ( Ipe.Grammar.FunctionCallOrValue
                                                        { Ipe.Grammar.functionCallOrValuePath = [],
                                                          Ipe.Grammar.functionCallOrValueName = "number",
                                                          Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                          Ipe.Grammar.functionCallOrValueArguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                    ),
                                    ( Ipe.Grammar.IpeCustomTypePattern
                                        []
                                        "OpaqueTypeConstructor2"
                                        [Ipe.Grammar.IpeVariablePattern "rootModuleType"],
                                      Ipe.Grammar.IpeFunctionCallOrValue
                                        ( Ipe.Grammar.FunctionCallOrValue
                                            { Ipe.Grammar.functionCallOrValuePath = ["Alias", "Module"],
                                              Ipe.Grammar.functionCallOrValueName = "toString",
                                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                              Ipe.Grammar.functionCallOrValueArguments =
                                                [ Ipe.Grammar.IpeFunctionCallOrValue
                                                    ( Ipe.Grammar.FunctionCallOrValue
                                                        { Ipe.Grammar.functionCallOrValuePath = [],
                                                          Ipe.Grammar.functionCallOrValueName = "rootModuleType",
                                                          Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                          Ipe.Grammar.functionCallOrValueArguments = []
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                    )
                                  ]
                            }
                        ),
                    Ipe.Grammar.topLevelDefinitionTypeAnnotation =
                      Just $
                        Ipe.Grammar.TypeAnnotation
                          { Ipe.Grammar.typeAnnotationName = "toString",
                            Ipe.Grammar.typeAnnotationArguments = [Ipe.Grammar.ConcreteType [] "OpaqueType" []],
                            Ipe.Grammar.typeAnnotationReturnType = Ipe.Grammar.ConcreteType [] "String" []
                          }
                  }
              ]
          }
