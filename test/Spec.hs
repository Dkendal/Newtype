{-# LANGUAGE OverloadedStrings #-}

module Main where

import Newtype.Parser hiding (main)
import Prettyprinter
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Pretty printing"
    [ testCase "Program" $
        assertPretty
          pProgram
          "type A = 1"
          "type A = 1",
      testCase "if extends else then expression" $
        assertPretty
          pExpression
          "if LHS <: RHS then Then else Else"
          "LHS extends RHS ? Then : Else",
      testCase "if extends else then expression" $
        assertPretty
          pExpression
          "if LHS <: RHS then Then"
          "LHS extends RHS ? Then : never",
      testCase "if right-extends then expression" $
        assertPretty
          pExpression
          "if LHS :> RHS then Then"
          "RHS extends LHS ? Then : never",
      testCase "if expressions may be negated" $
        assertPretty
          pExpression
          "if not LHS <: RHS then Then"
          "LHS extends RHS ? never : Then",
      testCase "if with infer operator" $
        assertPretty
          pExpression
          "if Left <: Right ?Infer then Then"
          "Left extends Right<infer Infer> ? Then : never"
    ]

assertPretty ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e, Pretty a) =>
  Parsec e s a ->
  s ->
  String ->
  IO ()
assertPretty parser input output =
  case parse parser "" input of
    Left a ->
      assertFailure (errorBundlePretty a)
    Right a ->
      show (pretty a) @?= output
