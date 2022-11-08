{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Newtype.Syntax.NewtypeSpec where

import Data.Text
import Newtype.Syntax.Newtype
import Prettyprinter (
  LayoutOptions (..),
  PageWidth (..),
  layoutPretty,
  pretty,
 )
import Prettyprinter.Render.String (renderString)
import Test.Hspec
import Prelude hiding (unlines, (&&), (||))

spec :: Spec
spec = do
  let a = mkIdent "A"
  let b = mkIdent "B"
  let c = mkIdent "C"
  let then' = mkIdent "Then"
  let else' = mkIdent "Else"
  let fmt = unpack . stripEnd . unlines
  let prettyShort ast = renderString (layoutPretty (LayoutOptions (AvailablePerLine 1 1)) (pretty ast))

  describe "expandCaseStatement" $ do
    it "expands a case statement" $ do
      let case' =
            CaseStatement
              (ExprIdent (Ident "A"))
              [ Case (ExprIdent (Ident "B")) (ExprIdent (Ident "B"))
              , Case (ExprIdent (Ident "C")) (ExprIdent (Ident "C"))
              , Case Hole (ExprIdent (Ident "A"))
              ]

      expandCaseStatement case'
        `shouldBe` ExprConditionalType
          ( ConditionalType
              { lhs = ExprIdent (Ident "A")
              , rhs = ExprIdent (Ident "B")
              , thenExpr = ExprIdent (Ident "B")
              , elseExpr =
                  ExprConditionalType
                    ( ConditionalType
                        { lhs = ExprIdent (Ident "A")
                        , rhs = ExprIdent (Ident "C")
                        , thenExpr = ExprIdent (Ident "C")
                        , elseExpr = ExprIdent (Ident "A")
                        }
                    )
              }
          )

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
        let expected = ct (mkTuple [a]) (mkTuple [b]) then' else'
        expandConditional expr `shouldBe` expected

    describe "not equals" $ do
      it "wraps args in a tuple, flips branches" $ do
        let expr = ConditionalExpr (a !== b) then' else'
        let expected = ct (mkTuple [a]) (mkTuple [b]) else' then'
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

  describe "Pretty" $ do
    describe "Statements" $ do
      describe "InterfaceDefinition" $ do
        it "formats the statement" $ do
          let ast =
                InterfaceDefinition
                  "A"
                  [ TypeParam
                      "T"
                      (Just (PrimitiveType PrimitiveAny))
                      (Just (PrimitiveType PrimitiveAny))
                  ]
                  Nothing
                  []
          let src =
                fmt
                  [ "interface A<T extends any = any> {"
                  , "  "
                  , "}"
                  ]
          show (pretty ast) `shouldBe` src

      describe "ExportStatement" $ do
        it "formats the statement" $ do
          let ast = ExportStatement [Ident "A", Ident "B"]
          show (pretty ast) `shouldBe` "export {A, B};"

    context "when Tuple" $ do
      it "formats properly when empty" $ do
        let ast = mkTuple []
        show (pretty ast) `shouldBe` "[]"

      it "formats properly with multiple elements" $ do
        let ast = mkTuple [a, b, c]
        show (pretty ast) `shouldBe` "[A, B, C]"

    context "when ExtendsExpr" $ do
      it "outputs equivalent typescript code" $ do
        let ast = ConditionalType a b then' else'
        show (pretty ast) `shouldBe` "(A extends B ? Then : Else)"

    context "when ConditionalExpr" $ do
      it "outputs equivalent typescript code" $ do
        let ast = expandConditional $ ConditionalExpr (a <: b) then' else'
        show (pretty ast) `shouldBe` "(A extends B ? Then : Else)"

      it "expands the condition first the expression first" $ do
        let ast = expandConditional $ ConditionalExpr (((a <: b) && (a !== c)) || (b === c)) then' else'
        let str = prettyShort ast
        str
          `shouldBe` fmt
            [ "(A extends B"
            , "  ? ([A] extends [C]"
            , "    ? ([B] extends [C]"
            , "      ? Then"
            , "      : Else)"
            , "    : Then)"
            , "  : ([B] extends [C]"
            , "    ? Then"
            , "    : Else))"
            ]

    describe "properties" $ do
      context "index properties" $ do
        it "formats output" $ do
          let ast =
                IndexSignature
                  { key = "key"
                  , keySource = PrimitiveType PrimitiveString
                  , value = PrimitiveType PrimitiveAny
                  , isReadonly = Nothing
                  }
          let str = "[key: string]: any"
          show (pretty ast) `shouldBe` str

      context "optional properties" $ do
        it "formats output" $ do
          let ast =
                DataProperty
                  { isOptional = Just True
                  , isReadonly = Nothing
                  , key = "key"
                  , value = PrimitiveType PrimitiveAny
                  }
          let str = "key?: any"
          show (pretty ast) `shouldBe` str

      context "readonly properties" $ do
        it "formats output" $ do
          let ast =
                DataProperty
                  { isOptional = Nothing
                  , isReadonly = Just True
                  , key = "key"
                  , value = PrimitiveType PrimitiveAny
                  }
          let str = "readonly key: any"
          show (pretty ast) `shouldBe` str

      it "formats output" $ do
        let ast =
              DataProperty
                { isOptional = Nothing
                , isReadonly = Nothing
                , key = "key"
                , value = PrimitiveType PrimitiveAny
                }
        let str = "key: any"
        show (pretty ast) `shouldBe` str

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
