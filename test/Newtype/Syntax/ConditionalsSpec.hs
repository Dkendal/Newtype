{-# LANGUAGE OverloadedStrings #-}

module Newtype.Syntax.ConditionalsSpec where

import Newtype.Syntax
import Newtype.Syntax.Conditionals
import Test.Hspec

spec :: Spec
spec = do
  describe "Newtype.Syntax.Conditionals" $ do
    describe "expandCaseStatement" $ do
      it "expands a case statement" $ do
        let case' =
              CaseStatement
                (ExprIdent (Ident "A"))
                [ Case (ExprIdent (Ident "B")) (ExprIdent (Ident "B")),
                  Case (ExprIdent (Ident "C")) (ExprIdent (Ident "C")),
                  Case Hole (ExprIdent (Ident "A"))
                ]

        expandCaseStatement case'
          `shouldBe` ExprConditionalType
            ( ConditionalType
                { lhs = ExprIdent (Ident "A"),
                  rhs = ExprIdent (Ident "B"),
                  thenExpr = ExprIdent (Ident "B"),
                  elseExpr =
                    ExprConditionalType
                      ( ConditionalType
                          { lhs = ExprIdent (Ident "A"),
                            rhs = ExprIdent (Ident "C"),
                            thenExpr = ExprIdent (Ident "C"),
                            elseExpr = ExprIdent (Ident "A")
                          }
                      )
                }
            )
