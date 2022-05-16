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
      testCase "interface definition" $
        assertPretty
          (pStatement <* eof)
          "interface A where\n\
          \  foo: 0\n\
          \  bar: 1"
          "interface A {\n\
          \  foo: 0;\n\
          \  bar: 1;\n\
          \}",
      testCase "if extends else then expression" $
        assertPretty
          (pExpr <* eof)
          "if LHS <: RHS then Then else Else"
          "LHS extends RHS ? Then : Else",
      testCase "if extends else then expression" $
        assertPretty
          (pExpr <* eof)
          "if LHS <: RHS then Then"
          "LHS extends RHS ? Then : never",
      testCase "if right-extends then expression" $
        assertPretty
          (pExpr <* eof)
          "if LHS :> RHS then Then"
          "RHS extends LHS ? Then : never",
      testCase "if expressions may be negated" $
        assertPretty
          (pExpr <* eof)
          "if not LHS <: RHS then Then"
          "LHS extends RHS ? never : Then",
      testCase "if with infer operator" $
        assertPretty
          (pExpr <* eof)
          "if Left <: Right ?Infer then Then"
          "Left extends Right<infer Infer> ? Then : never",
      testCase "comparision with complex term" $
        assertPretty
          (pExtendsExpr <* eof)
          "if A B (C D) <: 0 then 1"
          "A<B, C<D>> extends 0 ? 1 : never",
      -- testCase "case statement with fall through" $
      --   assertPretty
      --     (pExpr <* eof)
      --     "case A of\n\
      --     \  B -> 1\n\
      --     \  C -> 2\n\
      --     \  _ -> 3"
      --     "A extends B ? 1 : A extends C ? 2 : 3",
      testCase "case statement type on rhs" $
        assertPretty
          (pExpr <* eof)
          "case A of\n\
          \B -> B\n\
          \C -> C"
          "A extends B ? B : A extends C ? C : never"
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
