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
  describe "NewtypeParser" $ do
    let parse' parser source =
          let result = parse (parser <* eof) "" source
           in case result of
                Left err -> error $ errorBundlePretty err
                Right x -> x

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

    describe "statements" $ do
      let subject = parse' pStatement

      describe "type definition" $ do
        it "parses a type definition" $ do
          subject "type A = B" `shouldBe` TypeDefinition "A" [] b

        it "parses a type definition with type parameters" $ do
          subject "type A b = B" `shouldBe` TypeDefinition "A" [TypeParam "b"] b

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

      describe "generic application" $ do
        it "parses" $ do
          let source = "A B C"
          subject source `shouldBe` GenericApplication (Ident "A") [b, c]

        it "parses parenthesized expressions" $ do
          let source = "(A B C)"
          subject source `shouldBe` GenericApplication (Ident "A") [b, c]

        it "doesn't matter how man parens there are" $ do
          let source = "((A B C))"
          subject source `shouldBe` GenericApplication (Ident "A") [b, c]

        it "should parse nested expressions in the arguments" $ do
          let source = "A (B C)"
          subject source
            `shouldBe` GenericApplication
              (Ident "A")
              [GenericApplication (Ident "B") [c]]

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
