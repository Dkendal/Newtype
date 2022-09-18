{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Newtype.ParserSpec (spec) where

import Data.Text
import Newtype.Eval
import Newtype.Parser
import Newtype.Syntax
import Prettyprinter
import Test.Hspec hiding (Expectation, expectationFailure, shouldBe)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.Megaparsec
import Text.Megaparsec hiding (parse)
import Prelude as P hiding (lines, unlines)

spec :: Spec
spec =
  describe "Black box tests" $ do
    describe "test statements" $ do
      it "parses a test statement" $ do
        parse pProgram
          `shouldSucceedOn` unlines
            [ "test \"does the thing\" where",
              "  AssertEqual 1 1"
            ]

    -- it "can eval the test program" $ do
    --   let src = unlines
    --         [ "test \"does the thing\" where"
    --         , "  AssertEqual 1 1"
    --         ]
    --   fmap eval (parse pProgram src) `shouldBe` Right []

    describe "types" $ do
      it "can be a simple alias" $ do
        parse pProgram "A : a" `shouldCompileTo` "type A = a;"

      it "must be not be indented" $ do
        parse pProgram `shouldFailOn` " A : a"

      it "can't have a type variable that starts with an upppercase character" $ do
        parse pProgram `shouldFailOn` " A B : B"

      it "the rest of the definition can be indented though" $ do
        parse pProgram "A\n : a" `shouldCompileTo` "type A = a;"

      it "the rest of the definition must be indented though" $ do
        parse pProgram `shouldFailOn` "A\n: a"

      it "can have parameters" $ do
        let src = "A (a <: string) (b = any) c : a"
        let out = "type A<a extends string, b = any, c> = a;"
        parse pProgram src `shouldCompileTo` out

    describe "interfaces" $ do
      it "can have no properties" $ do
        parse pProgram "interface A" `shouldCompileTo` "interface A {\n  \n}"

      it "must have properties if the where keyword is present" $ do
        parse pProgram `shouldFailOn` "interface A where"

      it "can have properties" $ do
        let src =
              unlines
                [ "interface A where",
                  "  a : string"
                ]
        let out =
              unlines'
                [ "interface A {",
                  "  a: string;",
                  "}"
                ]
        parse pProgram src `shouldCompileTo` out

      it "can apply modifiers to properties" $ do
        let src =
              unlines
                [ "interface A where",
                  "  readonly index a? : string"
                ]
        let out =
              unlines'
                [ "interface A {",
                  "  readonly [a]?: string;",
                  "}"
                ]
        parse pProgram src `shouldCompileTo` out

    describe "union types" $ do
      it "can parse" $ do
        parse pProgram "A : 1 | 2" `shouldCompileTo` "type A = 1 | 2;"

    describe "intersection types" $ do
      it "can parse" $ do
        parse pProgram "A : 1 & 2" `shouldCompileTo` "type A = 1 & 2;"

    describe "expressions" $ do
      describe "if-then-else" $ do
        it "can parse" $ do
          let src = "if a <: b then c else d"
          let out = "(a extends b ? c : d)"
          expr src `shouldCompileTo` out

        it "defaults to never for the else case if it's omitted" $ do
          let src = "if a <: b then c"
          let out = "(a extends b ? c : never)"
          expr src `shouldCompileTo` out

        it "can combine conditions with `and`" $ do
          let src = "if a <: b and b <: c then c"
          let out = "(a extends b ? (b extends c ? c : never) : never)"
          expr src `shouldCompileTo` out

        it "can combine conditions with `or`" $ do
          let src = "if a <: b or b <: c then c"
          let out = "(a extends b ? c : (b extends c ? c : never))"
          expr src `shouldCompileTo` out

        it "can negate conditions with `not`" $ do
          let src = "if not a <: b then c"
          let out = "(a extends b ? never : c)"
          expr src `shouldCompileTo` out

      describe "mapped types" $ do
        it "can parse" $ do
          let src = "{ v : k <- t }"
          let out = "{[k in t]: v}"
          expr src `shouldCompileTo` out

      describe "case statements" $ do
        it "can have one case and default to never for the else case" $ do
          let src =
                unlines
                  [ "case n of",
                    "  string -> 1"
                  ]
          let out = "(n extends string ? 1 : never)"
          expr src `shouldCompileTo` out

        it "can't just have a fallthrough case" $ do
          let src =
                unlines
                  [ "case n of",
                    "  _ -> 1"
                  ]
          expr `shouldFailOn` src

        it "can have have a fallthrough case" $ do
          let src =
                unlines
                  [ "case n of",
                    "  string -> 1",
                    "  _ -> 2"
                  ]
          let out = "(n extends string ? 1 : 2)"
          expr src `shouldCompileTo` out

parse parser = runNewTypeParser (parser <* eof) ""

expr :: Text -> Either CompilerError Expr
expr = parse pExpr

shouldCompileTo :: Pretty a => Either CompilerError a -> Text -> Expectation
shouldCompileTo (Left e) actual =
  expectationFailure $
    "expected: "
      ++ show actual
      ++ "\nbut parsing failed with error:\n"
      ++ errorBundlePretty e
shouldCompileTo (Right actual) expected = (show . pretty) actual `shouldBe` unpack expected

unlines' = stripEnd . unlines
