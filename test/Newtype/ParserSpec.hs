{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Newtype.ParserSpec (spec) where

import Newtype.Parser
import Newtype.Syntax
import Test.Hspec
import Text.Megaparsec

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
      it "parses a program with multiple type defintions that use dot access" $ do
        pendingWith "TODO"

    describe "statements" $ do
      let subject = parse' pStatement

      describe "type definition" $ do
        it "parses a type definition" $ do
          subject "type A = B" `shouldBe` TypeDefinition "A" [] b

        it "parses a type definition with type parameters" $ do
          subject "type A b = B" `shouldBe` TypeDefinition "A" [TypeParam "b"] b

        it "parses a type definition with a definition that uses dot access" $ do
          (parse' pProgram) "type BuiltIn = M.BuiltIn" `shouldBe` Program [TypeDefinition "BuiltIn" [] (DotAccess (mkIdent "M") (mkIdent "BuiltIn"))]


    describe "expressions" $ do
      let subject = parse' pExpr

      describe "dot access" $ do
        it "parses a dot access" $ do
          subject "a.b.c" `shouldBe` (mkIdent "a" `DotAccess` mkIdent "b" `DotAccess` mkIdent "c")

      describe "literal values" $
        it "parses numbers" $ do
          let source = "1"
          subject source `shouldBe` NumberIntegerLiteral 1

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

    describe "conditional types" $ do
      it "should parse if ... then ... else" $ do
        let source = "if A <: B then C else D"
        parse' pExpr source `shouldBe` ctExpr a b c d

      it "should parse if ... then" $ do
        let source = "if A <: B then C"
        parse' pExpr source `shouldBe` ctExpr a b c never
