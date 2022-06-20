{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Newtype.ParserSpec (spec) where

import Data.Text
import Newtype.Parser
import Newtype.Syntax
import Newtype.Syntax.Conditionals
import Test.Hspec
import Text.Megaparsec
import Prelude hiding (unlines)

spec :: Spec
spec = do
  let a = mkIdent "A"
  let b = mkIdent "B"
  let c = mkIdent "C"
  let d = mkIdent "D"
  let e = mkIdent "E"
  let parse' parser source =
        case parse (parser <* eof) "" source of
          Left err -> error $ errorBundlePretty err
          Right x -> x
  describe "NewtypeParser" $ do
    describe "programs" $ do
      let subject = parse' pProgram

      it "parses a program with multiple type defintions that use dot access" $ do
        subject
          ( unlines
              [ "type A = a.b",
                "type B = b.c"
              ]
          )
          `shouldBe` Program
            [ TypeDefinition "A" [] (DotAccess (mkIdent "a") (mkIdent "b")),
              TypeDefinition "B" [] (DotAccess (mkIdent "b") (mkIdent "c"))
            ]

      it "can start with a comment" $ do
        subject
          ( unlines
              [ "-- comment",
                "type A = a.b"
              ]
          )
          `shouldBe` Program
            [TypeDefinition "A" [] (DotAccess (mkIdent "a") (mkIdent "b"))]

      it "can start with a blank line" $ do
        subject
          ( unlines
              [ "",
                "type A = a.b"
              ]
          )
          `shouldBe` Program
            [TypeDefinition "A" [] (DotAccess (mkIdent "a") (mkIdent "b"))]

    describe "statements" $ do
      let subject = parse' pStatement

      describe "interface definition" $ do
        it "parses an interface definition" $ do
          let expected =
                InterfaceDefinition
                  { name = "A",
                    params =
                      [ TypeParam
                          { name = "t1",
                            defaultValue = Just (PrimitiveType PrimitiveAny),
                            constraint = Just (PrimitiveType PrimitiveString)
                          },
                        TypeParam
                          { name = "t2",
                            defaultValue = Just (PrimitiveType PrimitiveAny),
                            constraint = Just (PrimitiveType PrimitiveNumber)
                          }
                      ],
                    extends =
                      Just
                        ( ExtendGeneric
                            ( GenericApplication
                                (Ident "Foo")
                                [ ExprIdent
                                    (Ident "Bar")
                                ]
                            )
                        ),
                    props =
                      [ DataProperty
                          { isReadonly = Nothing,
                            isOptional = Nothing,
                            accessor = Nothing,
                            key = "a",
                            value = ExprIdent (Ident "A")
                          },
                        DataProperty
                          { isReadonly = Nothing,
                            isOptional = Nothing,
                            accessor = Nothing,
                            key = "b",
                            value = ExprIdent (Ident "B")
                          }
                      ]
                  }

          subject
            ( unlines
                [ "interface A t1 t2",
                  "  when",
                  "    t1 <: string",
                  "    t2 <: number",
                  "  defaults",
                  "    t1 = any",
                  "    t2 = any",
                  "  extends Foo Bar",
                  "  where",
                  "    a = A",
                  "    b = B"
                ]
            )
            `shouldBe` expected

      describe "type definition" $ do
        it "parses a type definition" $ do
          subject "type A = B" `shouldBe` TypeDefinition "A" [] b

        it "parses type definitions with default values for type parameters" $ do
          let expected =
                TypeDefinition
                  { name = "Type",
                    params =
                      [ TypeParam
                          { name = "A",
                            defaultValue = Just (NumberIntegerLiteral 1),
                            constraint = Just (PrimitiveType PrimitiveNumber)
                          },
                        TypeParam
                          { name = "B",
                            defaultValue = Just (NumberIntegerLiteral 2),
                            constraint = Just (PrimitiveType PrimitiveNumber)
                          }
                      ],
                    body =
                      ObjectLiteral
                        [ DataProperty
                            { isReadonly = Nothing,
                              isOptional = Nothing,
                              accessor = Nothing,
                              key = "a",
                              value = ExprIdent (Ident "A")
                            },
                          DataProperty
                            { isReadonly = Nothing,
                              isOptional = Nothing,
                              accessor = Nothing,
                              key = "b",
                              value = ExprIdent (Ident "B")
                            }
                        ]
                  }
          subject
            ( unlines
                [ "type Type A B",
                  "  when",
                  "    A <: number",
                  "    B <: number",
                  "  defaults",
                  "    A = 1",
                  "    B = 2",
                  "  = { a: A, b: B }"
                ]
            )
            `shouldBe` expected

        it "parses a type definition with type parameters" $ do
          subject "type A b = B"
            `shouldBe` TypeDefinition
              { name = "A",
                params =
                  [ TypeParam
                      { name = "b",
                        defaultValue = Nothing,
                        constraint = Nothing
                      }
                  ],
                body = b
              }

        it "parses a type definition with a definition that uses dot access" $ do
          parse' pProgram "type BuiltIn = M.BuiltIn"
            `shouldBe` Program
              [TypeDefinition "BuiltIn" [] (DotAccess (mkIdent "M") (mkIdent "BuiltIn"))]

    describe "expressions" $ do
      let subject = parse' pExpr

      describe "dot access" $ do
        it "parses a dot access" $ do
          subject "a.b.c" `shouldBe` (mkIdent "a" `DotAccess` mkIdent "b" `DotAccess` mkIdent "c")

      describe "access" $ do
        it "parses access with the bang operator" $ do
          subject "a ! b" `shouldBe` (mkIdent "a" `Access` mkIdent "b")

        it "parses access with the bang operator and no whitespace" $ do
          subject "a!b" `shouldBe` (mkIdent "a" `Access` mkIdent "b")

      describe "literal values" $
        it "parses numbers" $ do
          let source = "1"
          subject source `shouldBe` NumberIntegerLiteral 1

      describe "conditional types" $ do
        it "should parse if ... then ... else" $ do
          let source = "if A <: B then C else D"
          parse' pExpr source `shouldBe` ctExpr a b c d

        it "should parse if ... then" $ do
          let source = "if A <: B then C"
          parse' pExpr source `shouldBe` ctExpr a b c never

      describe "mapped types" $ do
        it "should parse mapped types" $ do
          let source = "{ Value : Key <- Type}"
          let expected =
                MappedType
                  { value = mkIdent "Value",
                    key = mkIdent "Key",
                    source = mkIdent "Type",
                    asExpr = Nothing,
                    isReadonly = Nothing,
                    isOptional = Nothing
                  }
          parse' pExpr source `shouldBe` expected

        it "should parse key remapping with the `as` keyword" $ do
          let source = "{ Value : Key as OtherKey <- Type}"
          let expected =
                MappedType
                  { value = mkIdent "Value",
                    key = mkIdent "Key",
                    source = mkIdent "Type",
                    asExpr = Just (mkIdent "OtherKey"),
                    isReadonly = Nothing,
                    isOptional = Nothing
                  }
          parse' pExpr source `shouldBe` expected

        it "should parse the `readonly` prefix modifier applied to the key" $ do
          let source = "{ Value : readonly Key <- Type}"
          let expected =
                MappedType
                  { value = mkIdent "Value",
                    key = mkIdent "Key",
                    source = mkIdent "Type",
                    asExpr = Nothing,
                    isReadonly = Just True,
                    isOptional = Nothing
                  }
          parse' pExpr source `shouldBe` expected

      describe "generic application" $ do
        it "parses" $ do
          let source = "A B C"
          let expected =
                ExprGenericApplication
                  ( GenericApplication
                      (Ident "A")
                      [ ExprIdent (Ident "B"),
                        ExprIdent (Ident "C")
                      ]
                  )
          subject source `shouldBe` expected

        it "parses parenthesized expressions" $ do
          let source = "(A B C)"
          let expected =
                ExprGenericApplication
                  ( GenericApplication
                      (Ident "A")
                      [ ExprIdent (Ident "B"),
                        ExprIdent (Ident "C")
                      ]
                  )
          subject source `shouldBe` expected

        it "doesn't matter how many parens there are" $ do
          let source = "((A B C))"
          let expected =
                ExprGenericApplication
                  ( GenericApplication
                      (Ident "A")
                      [ ExprIdent (Ident "B"),
                        ExprIdent (Ident "C")
                      ]
                  )

          subject source `shouldBe` expected

        it "should parse nested expressions in the arguments" $ do
          let source = "A (B C)"
          let expected =
                ExprGenericApplication
                  ( GenericApplication
                      (Ident "A")
                      [ ExprGenericApplication
                          ( GenericApplication
                              (Ident "B")
                              [ExprIdent (Ident "C")]
                          )
                      ]
                  )
          subject source `shouldBe` expected

    describe "conditional expr" $ do
      let subject = parse' pBoolExpr

      it "parses extends left" $ do
        let source = "A <: B"
        subject source `shouldBe` ExtendsLeft a b

      it "parses extends right" $ do
        let source = "A :> B"
        subject source `shouldBe` ExtendsRight a b

      it "parses equals" $ do
        let source = "A == B"
        subject source `shouldBe` Equals a b

      it "parses not equals" $ do
        let source = "A != B"
        subject source `shouldBe` NotEquals a b

      it "parses not" $ do
        let source = "not A <: B"
        subject source `shouldBe` Not (ExtendsLeft a b)

      it "parses and" $ do
        let source = "A <: B and B <: C"
        subject source `shouldBe` And (ExtendsLeft a b) (ExtendsLeft b c)

      it "parses left associative and" $ do
        let source = "A <: B and B <: C or D <: E"
        subject source `shouldBe` Or (And (ExtendsLeft a b) (ExtendsLeft b c)) (ExtendsLeft d e)

      it "parses parenthesized expressions" $ do
        let source = "(A <: B and B <: C) or D <: E"
        subject source `shouldBe` Or (And (ExtendsLeft a b) (ExtendsLeft b c)) (ExtendsLeft d e)

    describe "case expression" $ do
      let subject = parse' pCaseStatement

      it "should parse multiple cases" $ do
        let source =
              unlines
                [ "case A of",
                  " B -> B",
                  " C -> C"
                ]
        subject source
          `shouldBe` CaseStatement
            (ExprIdent (Ident "A"))
            [ Case (ExprIdent (Ident "B")) (ExprIdent (Ident "B")),
              Case (ExprIdent (Ident "C")) (ExprIdent (Ident "C"))
            ]

      it "should parse default case" $ do
        let source =
              unlines
                [ "case A of",
                  " B -> B",
                  " C -> C",
                  " _ -> A"
                ]
        subject source
          `shouldBe` CaseStatement
            (ExprIdent (Ident "A"))
            [ Case (ExprIdent (Ident "B")) (ExprIdent (Ident "B")),
              Case (ExprIdent (Ident "C")) (ExprIdent (Ident "C")),
              Case Hole (ExprIdent (Ident "A"))
            ]
