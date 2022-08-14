{-# LANGUAGE OverloadedStrings #-}

module Newtype.EvalSpec (spec) where

import Control.Monad (forM_)
import Data.Text (Text, stripEnd, unlines, unpack)
import Newtype.Eval (evalExpr, evalProgram, isAssignable)
import Newtype.Parser (CompilerError, pExpr, pProgram)
import Newtype.Syntax (Expr, Program)
import Test.Hspec (Spec, describe, it, pendingWith)
import Test.Hspec.Expectations.Pretty
  ( expectationFailure,
    shouldBe,
  )
import Test.Hspec.Newtype (parse, parseAdjMatrix)
import Text.Megaparsec (errorBundlePretty)
import Prelude as P hiding (lines, unlines)

spec :: Spec
spec = do
  describe "parseAdjMatrix" $ do
    it "can parse a matrix table" $ do
      let tbl =
            parseAdjMatrix
              '|'
              [ "|   | a | b |",
                "| x | 1 | 0 |",
                "| y |   | 1 |"
              ]

      let out =
            [ ("x", [("a", "1"), ("b", "0")]),
              ("y", [("a", ""), ("b", "1")])
            ]
      tbl `shouldBe` out

  describe "evalProgram" $ do
    it "can expand a generic" $ do
      let input = unlines ["Id t : t", "A : Id 1"]
          output = unlines ["Id t : t", "A : 1"]
       in program' input `shouldBe` program output

  describe "isAssignable" $ do
    let tbl =
          parseAdjMatrix
            '|'
            [ "|           | any | unknown | object | void | undefined | null | never |",
              "| any       |     | t       | t      | t    | t         | t    | f     |",
              "| unknown   | t   |         | f      | f    | f         | f    | f     |",
              "| object    | t   | t       |        | f    | f         | f    | f     |",
              "| void      | t   | t       | f      |      | f         | f    | f     |",
              "| undefined | t   | t       | o      | t    |           | o    | f     |",
              "| null      | t   | t       | o      | o    | o         |      | f     |",
              "| never     | t   | t       | t      | t    | t         | t    |       |"
            ]
    forM_ tbl $ \(lhs, m0) ->
      describe (unpack lhs) $
        forM_ m0 $ \(rhs, cell) -> do
          let expected =
                case cell of
                  "t" -> True
                  "" -> True
                  -- This is to indicate that it's supported when strictNullChecks is enabled
                  -- which we don't support at the moment.
                  "o" -> False
                  _ -> False
          let expectation = do
                l <- expr lhs
                r <- expr rhs
                return $ isAssignable l r `shouldBe` expected
          let msg =
                if expected
                  then "is assignable to `" ++ unpack rhs ++ "`"
                  else "is not assignable to `" ++ unpack rhs ++ "`"
          it msg $ case expectation of
            Left e -> expectationFailure $ errorBundlePretty e
            Right ex -> ex

  describe "evalExpr" $ do
    it "distributes over both branches when the lhs is `any`" $ do
      let src = "if any <: 1 then 2 else 3"
      fmap evalExpr (expr src) `shouldBe` expr "2 | 3"

    it "reduces to the lhs when literals are the same" $ do
      let src = "if 1 <: 1 then true"
      fmap evalExpr (expr src) `shouldBe` expr "true"

    it "different literals are not assignable" $ do
      let src = "if not 1 <: 2 then true"
      fmap evalExpr (expr src) `shouldBe` expr "true"

    it "bigint is not assignable to number" $ do
      -- let src = "if not 1n <: number then true"
      -- fmap evalExpr (expr src) `shouldBe` expr out
      pendingWith "bigint literal not implemented"

    it "a literal is assignable to `any`" $ do
      let src = "if 1 <: any then true"
      fmap evalExpr (expr src) `shouldBe` expr "true"

    it "reduces to the lhs when a literal extends a primitive type" $ do
      let src = "if 1 <: number then true"
      fmap evalExpr (expr src) `shouldBe` expr "true"

expr' :: Text -> Either CompilerError Expr
expr' t = evalExpr <$> expr t

expr :: Text -> Either CompilerError Expr
expr = parse pExpr

program' :: Text -> Either CompilerError Program
program' t = evalProgram <$> program t

program :: Text -> Either CompilerError Program
program = parse pProgram

unlines' = stripEnd . unlines
