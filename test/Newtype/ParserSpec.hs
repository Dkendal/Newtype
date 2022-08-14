{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Newtype.ParserSpec (spec) where

import Control.Monad
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

parse parser = runNewTypeParser (parser <* eof) ""

spec :: Spec
spec =
  describe "Black box tests" $ do
    evalExprSpec
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

{-
 Example input:
 "
 |   | a | b |
 | x | 1 | 0 |
 | y |   | 1 |
 "
 Example output:
 [("x", [("a", "1"), ("b", "0")]), ("y", [("a", ""), ("b", "1")])]
 -}
parseAdjMatrix :: Char -> [Text] -> [(Text, [(Text, Text)])]
parseAdjMatrix colSep textLines =
  do
    let headers = P.tail . P.head $ tbl
    do
      row <- P.tail tbl
      let colHeader = P.head row
      let rowWithHeaders = P.zip headers (P.tail row)
      return (colHeader, rowWithHeaders)
  where
    tbl =
      do
        l <- textLines
        return $ do
          cell <- splitOn (singleton colSep) (dropAround (colSep ==) l)
          return $ strip cell

-- in Prelude.map (\(name, row) -> (name, row)) rows''

evalExprSpec :: Spec
evalExprSpec = do
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
    fit "can expand a generic" $ do
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

  fdescribe "evalExpr" $ do
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

shouldCompileTo :: Pretty a => Either CompilerError a -> Text -> Expectation
shouldCompileTo (Left e) actual =
  expectationFailure $
    "expected: "
      ++ show actual
      ++ "\nbut parsing failed with error:\n"
      ++ errorBundlePretty e
shouldCompileTo (Right actual) expected = (show . pretty) actual `shouldBe` unpack expected

unlines' = stripEnd . unlines
