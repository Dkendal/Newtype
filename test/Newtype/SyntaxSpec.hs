{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Newtype.SyntaxSpec (spec) where

import Data.Text
import Newtype.Syntax
import Prettyprinter (LayoutOptions (..), PageWidth (..), layoutPretty, pretty)
import Prettyprinter.Render.String (renderString)
import Test.Hspec
import Prelude hiding (unlines, (&&), (||))
import Data.Function ((&))

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

ct :: Expr -> Expr -> Expr -> Expr -> ConditionalType
ct =  ConditionalType

ct' :: Expr -> Expr -> Expr -> Expr -> Expr
ct' a b c d =  ExprConditionalType (ConditionalType a b c d)

spec :: Spec
spec = do
  let a = mkIdent "A"
  let b = mkIdent "B"
  let c = mkIdent "C"
  let then' = mkIdent "Then"
  let else' = mkIdent "Else"
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
        let ast = ConditionalType a b then' else'
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
    describe "extends left" $
      it "left as is" $ do
        let expr = ConditionalExpr (a <: b) then' else'
        let expected = ct a b then' else'
        expandConditional expr `shouldBe` expected

    describe "negation" $
      it "flips `then` with `else`" $ do
        let expr = ConditionalExpr (Not (a <: b)) then' else'
        let expected = ct a b else' then'
        expandConditional expr `shouldBe` expected

    describe "extends right" $ do
      it "flips the conditional" $ do
        let expr = ConditionalExpr (a >: b) then' else'
        let expected = ct b a then' else'
        expandConditional expr `shouldBe` expected

    describe "equals" $ do
      it "wraps args in a tuple" $ do
        let expr = ConditionalExpr (a === b) then' else'
        let expected = ct (Tuple [a]) (Tuple [b]) then' else'
        expandConditional expr `shouldBe` expected

    describe "not equals" $ do
      it "wraps args in a tuple, flips branches" $ do
        let expr = ConditionalExpr (a !== b) then' else'
        let expected = ct (Tuple [a]) (Tuple [b]) else' then'
        expandConditional expr `shouldBe` expected

    describe "logical AND" $ do
      it "produces a nested if statement where `then'` is used for the success cases" $ do
        let expr = ConditionalExpr ((a <: b) && (b <: c)) then' else'
        let expected = ct a b (ct' b c then' else') else'
        expandConditional expr `shouldBe` expected

    describe "logical OR" $ do
      it "produces a nested if statement, chainining the then' case through the first else" $ do
        let expr = ConditionalExpr ((a <: b) || (b <: c)) then' else'
        let expected = ct a b then' (ct' b c then' else')
        expandConditional expr `shouldBe` expected
