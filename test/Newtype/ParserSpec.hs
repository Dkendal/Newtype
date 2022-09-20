{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Newtype.ParserSpec (spec) where

import Data.Text
import Newtype.Parser
import Newtype.Syntax
import Prettyprinter
import Test.Hspec hiding (Expectation, expectationFailure, shouldBe)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.Megaparsec
import Text.Heredoc (str)
import Text.Megaparsec hiding (parse)
import Text.Nicify
import Prelude as P hiding (lines, unlines)
import Test.Hspec.Newtype

spec :: Spec
spec =
  describe "Black box tests" $ do
    describe "test statements" $ do
      it "parses a test statement" $ do
        parse pProgram
          `shouldSucceedOn` unlines
            [ "test \"does the thing\" where"
            , "  AssertEqual 1 1"
            ]

    -- it "can eval the test program" $ do
    --   let src = unlines
    --         [ "test \"does the thing\" where"
    --         , "  AssertEqual 1 1"
    --         ]
    --   fmap eval (parse pProgram src) `shouldBe` Right []

    describe "types" $ do
      it "can be a simple alias" $ do
        shouldCompile pProgram "A : a" "type A = a;"

      it "must be not be indented" $ do
        parse pProgram `shouldFailOn` " A : a"

      it "can't have a type variable that starts with an upppercase character" $ do
        parse pProgram `shouldFailOn` " A B : B"

      it "the rest of the definition can be indented though" $ do
        shouldCompile pProgram "A\n : a" "type A = a;"

      it "the rest of the definition must be indented though" $ do
        parse pProgram `shouldFailOn` "A\n: a"

      it "can have parameters" $ do
        let src = "A (a <: string) (b = any) c : a"
        let out = "type A<a extends string, b = any, c> = a;"
        shouldCompile pProgram src out

    describe "interfaces" $ do
      it "can have no properties" $ do
        shouldCompile pProgram "interface A" "interface A {\n  \n}"

      it "must have properties if the where keyword is present" $ do
        parse pProgram `shouldFailOn` "interface A where"

      it "can have properties" $ do
        let src =
              unlines
                [ "interface A where"
                , "  a : string"
                ]
        let out =
              unlines'
                [ "interface A {"
                , "  a: string;"
                , "}"
                ]
        shouldCompile pProgram src out

      it "can apply modifiers to properties" $ do
        let src =
              unlines
                [ "interface A where"
                , "  readonly index a? : string"
                ]
        let out =
              unlines'
                [ "interface A {"
                , "  readonly [a]?: string;"
                , "}"
                ]
        shouldCompile pProgram src out

    describe "union types" $ do
      it "can parse" $ do
        shouldCompile pProgram "A : 1 | 2" "type A = 1 | 2;"

    describe "intersection types" $ do
      it "can parse" $ do
        shouldCompile pProgram "A : 1 & 2" "type A = 1 & 2;"

    describe "template string literals" $ do
      it "empty string" $ do
        shouldCompile
          pExpr
          "``"
          "``"

      it "no substitutions" $ do
        shouldCompile
          pExpr
          "`hello world`"
          "`hello world`"

      it "sub in the first position" $ do
        shouldCompile
          pExpr
          [str|`${T hello} world`|]
          [str|`${T<hello>} world`|]

      it "with a single sub" $ do
        shouldCompile
          pExpr
          "`hello ${T world}`"
          "`hello ${T<world>}`"

      it "with trailing text" $ do
        shouldCompile
          pExpr
          "`hello ${T world} how are you?`"
          "`hello ${T<world>} how are you?`"

      it "with multiple subs" $ do
        shouldCompile
          pExpr
          "`a ${T a} b ${T b} c ${T c}`"
          "`a ${T<a>} b ${T<b>} c ${T<c>}`"

    describe "expressions" $ do
      describe "if-then-else" $ do
        it "can parse" $ do
          shouldCompile
            pExpr
            "if a <: b then c else d"
            "(a extends b ? c : d)"

        it "defaults to never for the else case if it's omitted" $ do
          shouldCompile
            pExpr
            "if a <: b then c"
            "(a extends b ? c : never)"

        it "can combine conditions with `and`" $ do
          shouldCompile
            pExpr
            "if a <: b and b <: c then c"
            "(a extends b ? (b extends c ? c : never) : never)"

        it "can combine conditions with `or`" $ do
          shouldCompile
            pExpr
            "if a <: b or b <: c then c"
            "(a extends b ? c : (b extends c ? c : never))"

        it "can negate conditions with `not`" $ do
          shouldCompile
            pExpr
            "if not a <: b then c"
            "(a extends b ? never : c)"

      describe "mapped types" $ do
        it "can parse" $ do
          shouldCompile
            pExpr
            "{ v : k <- t }"
            "{[k in t]: v}"

      describe "case statements" $ do
        it "can have one case and default to never for the else case" $ do
          shouldCompile
            pExpr
            [str|case n of
                |  string -> 1
                |]
            "(n extends string ? 1 : never)"

        it "can't just have a fallthrough case" $ do
          let src =
                unlines
                  [ "case n of"
                  , "  _ -> 1"
                  ]
          expr `shouldFailOn` src

        it "can have have a fallthrough case" $ do
          shouldCompile
            pExpr
            [str|case n of
                |  string -> 1
                |  _ -> 2
                |]
            "(n extends string ? 1 : 2)"

expr :: Text -> Either CompilerError Expr
expr = parse pExpr

unlines' = stripEnd . unlines
