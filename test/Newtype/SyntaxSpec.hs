{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Newtype.SyntaxSpec (spec) where

import Data.Text
import Newtype.Syntax
import Prettyprinter (LayoutOptions (..), PageWidth (..), layoutPretty, pretty)
import Prettyprinter.Render.String (renderString)
import Test.Hspec
import Prelude hiding (unlines, (&&), (||))

(<:) :: Expr -> Expr -> BoolExpr
a <: b = ExtendsLeft a b

(>:) :: Expr -> Expr -> BoolExpr
a >: b = ExtendsRight a b

(===) :: Expr -> Expr -> BoolExpr
a === b = Equals a b

(!==) :: Expr -> Expr -> BoolExpr
a !== b = NotEquals a b

(&&) :: BoolExpr -> BoolExpr -> BoolExpr
a && b = And a b

(||) :: BoolExpr -> BoolExpr -> BoolExpr
a || b = Or a b

ct :: ExtendsExpr -> Expr
ct = ConditionalType

spec :: Spec
spec = do
  let a = ID "A"
  let b = ID "B"
  let c = ID "C"
  let then' = ID "Then"
  let else' = ID "Else"
  let fmt = unpack . stripEnd . unlines
  let prettyShort ast = renderString (layoutPretty (LayoutOptions (AvailablePerLine 1 1)) (pretty ast))
  describe "pretty" $ do
    context "when Tuple" $ do
      it "formats properly when empty" $ do
        let ast = Tuple []
        show (pretty ast) `shouldBe` "[]"

      it "formats properly with multiple elements" $ do
        let ast = Tuple [a, b, c]
        show (pretty ast) `shouldBe` "[A, B, C]"

    context "when ExtendsExpr" $ do
      it "outputs equivalent typescript code" $ do
        let ast = ExtendsExpr a b then' else'
        show (pretty ast) `shouldBe` "A extends B ? Then : Else"
    context "when ConditionalExpr" $ do
      it "outputs equivalent typescript code" $ do
        let ast = ConditionalExpr (a <: b) then' else'
        show (pretty ast) `shouldBe` "A extends B ? Then : Else"
      it "expands the condition first the expression first" $ do
        let ast = ConditionalExpr (((a <: b) && (a !== c)) || (b === c)) then' else'
        let str = prettyShort ast
        str
          `shouldBe` fmt
            [ "A extends B",
              "  ? [A] extends [C]",
              "    ? [B] extends [C]",
              "      ? Then",
              "      : Else",
              "    : Then",
              "  : [B] extends [C]",
              "    ? Then",
              "    : Else"
            ]

  describe "expandConditional" $ do
    let cond = ConditionalExpr
    let ext = ExtendsExpr
    describe "extends left" $
      it "left as is" $ do
        let expr = cond (a <: b) then' else'
        let expected = ext a b then' else'
        expandConditional expr `shouldBe` expected

    describe "negation" $
      it "flips `then` with `else`" $ do
        let expr = cond (Not (a <: b)) then' else'
        let expected = ext a b else' then'
        expandConditional expr `shouldBe` expected

    describe "extends right" $ do
      it "flips the conditional" $ do
        let expr = cond (a >: b) then' else'
        let expected = ext b a then' else'
        expandConditional expr `shouldBe` expected

    describe "equals" $ do
      it "wraps args in a tuple" $ do
        let expr = cond (a === b) then' else'
        let expected = ext (Tuple [a]) (Tuple [b]) then' else'
        expandConditional expr `shouldBe` expected

    describe "not equals" $ do
      it "wraps args in a tuple, flips branches" $ do
        let expr = cond (a !== b) then' else'
        let expected = ext (Tuple [a]) (Tuple [b]) else' then'
        expandConditional expr `shouldBe` expected

    describe "logical AND" $ do
      it "produces a nested if statement where `then'` is used for the success cases" $ do
        let expr = cond ((a <: b) && (b <: c)) then' else'
        let expected = ext a b (ct (ext b c then' else')) else'
        expandConditional expr `shouldBe` expected

    describe "logical OR" $ do
      it "produces a nested if statement, chainining the then' case through the first else" $ do
        let expr = cond ((a <: b) || (b <: c)) then' else'
        let expected = ext a b then' (ct (ext b c then' else'))
        expandConditional expr `shouldBe` expected
