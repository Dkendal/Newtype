{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Newtype.ParserSpec (spec) where

import Newtype.Parser
import Newtype.Syntax
import Test.Hspec
import Text.Megaparsec

spec :: Spec
spec = do
  let a = ID "A"
  let b = ID "B"
  let c = ID "C"
  let d = ID "D"
  let e = ID "E"
  describe "NewtypeParser" $ do
    let parse' parser source =
          let result = parse (parser <* eof) "" source
           in case result of
                Left err -> error $ errorBundlePretty err
                Right x -> x

    describe "expressions" $ do
      let subject = parse' pExpr

      describe "literal values" $
        it "parses numbers" $ do
          let source = "1"
          subject source `shouldBe` NumberIntegerLiteral 1

      describe "type application" $ do
        it "parses" $ do
          let source = "A B C"
          subject source `shouldBe` GenericApplication "A" [b, c]

        it "parses parenthesized expressions" $ do
          let source = "(A B C)"
          subject source `shouldBe` GenericApplication "A" [b, c]

        it "doesn't matter how man parens there are" $ do
          let source = "((A B C))"
          subject source `shouldBe` GenericApplication "A" [b, c]

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
      it "should parse" $ do
        let source = "if A <: B then C else D"
        parse' pExpr source `shouldBe` ExprConditionalType (ConditionalType a b c d)

    fdescribe "generic application" $ do
      it "should parse single argument application" $ do
        let source = "A B"
        parse' pExpr source `shouldBe` GenericApplication "A" [b]
