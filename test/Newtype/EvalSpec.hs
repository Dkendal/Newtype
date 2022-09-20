{-# LANGUAGE OverloadedStrings #-}

module Newtype.EvalSpec (spec) where

import Control.Monad (forM_)
import Data.Text (Text, stripEnd, unlines, unpack)
import Newtype.Eval (evalExpr, evalProgram, isAssignable)
import Newtype.Parser (ParserResult, pExpr, pProgram)
import Newtype.Syntax (Expr, Program)
import qualified Prettyprinter as PP
import Test.Hspec hiding (expectationFailure, shouldBe)
import Test.Hspec.Expectations.Pretty (
  expectationFailure,
  shouldBe,
 )
import Test.Hspec.Newtype (parse, parseAdjMatrix, shouldCompile)
import Text.Heredoc
import Text.Megaparsec (errorBundlePretty)
import Prelude as P hiding (lines, unlines)

spec :: Spec
spec = do
  describe "evalProgram" $ do
    it "can expand a generic" $ do
      shouldEvalTo
        [str|Id t : t
            |A : Id 1
            |]
        [str|Id t : t
            |A : 1
            |]

    it "can recursively expand a generic" $ do
      shouldEvalTo
        [str|A t : t
            |B t : t
            |C t : t
            |Out : A (B (C 1))
            |]
        [str|A t : t
            |B t : t
            |C t : t
            |Out : 1
            |]

    it "can perform substituion within the type defintion" $ do
      shouldEvalTo
        [str|F t : [t, t]
            |Out : F 1
            |]
        [str|F t : [t, t]
            |Out : [1, 1]
            |]

    describe "template literals" $ do
      it "can be reduced to a string literal" $ do
        shouldEvalPretty
          [str|Greeting t : `hello ${t}`
              |Out : Greeting "world"
              |]
          [str|Greeting t : `hello ${t}`
              |Out : "hello world"
              |]

      it "simpilified to strings when interpolations blocks contains strings" $ do
        shouldEvalPretty
          [str|Out : `${"a"} ${"b"} ${"c"}`|]
          [str|Out : "a b c"|]

      it "empty template strings are reduces to empty strings" $ do
        shouldEvalPretty
          [str|Out : ``|]
          [str|Out : ""|]

      describe "built in string type functions" $ do
        it "supports `Uppercase<T>`" $ do
          shouldEvalTo
            [str|Greeting t : `hello ${Uppercase t}`
                |Out : Greeting "world"
                |]
            [str|Greeting t : `hello ${Uppercase t}`
                |Out : "hello WORLD"
                |]

        it "supports `Lowercase<T>`" $ do
          shouldEvalTo
            [str|Greeting t : `hello ${Lowercase t}`
                |Out : Greeting "WORLD"
                |]
            [str|Greeting t : `hello ${Lowercase t}`
                |Out : "hello world"
                |]

        it "supports `Capitalize<T>`" $ do
          shouldEvalTo
            [str|LowercaseGreeting : "hello, world"
                |Greeting : Capitalize LowercaseGreeting
                |]
            [str|LowercaseGreeting : "hello, world"
                |Greeting : "Hello, world"
                |]

        it "supports `Uncapitalize<T>`" $ do
          shouldEvalTo
            [str|UppercaseGreeting : "HELLO WORLD"
                |Greeting : Uncapitalize UppercaseGreeting
                |]
            [str|UppercaseGreeting : "HELLO WORLD"
                |Greeting : "hELLO WORLD"
                |]

    it "can expand a conditional" $ do
      shouldEvalTo
        [str|A : if 1 <: number then true else false|]
        [str|A : true|]

    it "can expand `keyof`" $ do
      shouldEvalTo
        [str|A : keyof { a : 1, b : 2}|]
        [str|A : "a" | "b"|]

    it "can expand a mapped type" $ do
      shouldEvalTo
        [str|A : { [t, t] : t <- "a" | "b" }|]
        [str|A : { a : ["a", "a"], b : ["b", "b"] }|]

  describe "isAssignable" $ do
    let tbl =
          parseAdjMatrix
            '|'
            [ "|           | any | unknown | object | void | undefined | null | never |"
            , "| any       |     | t       | t      | t    | t         | t    | f     |"
            , "| unknown   | t   |         | f      | f    | f         | f    | f     |"
            , "| object    | t   | t       |        | f    | f         | f    | f     |"
            , "| void      | t   | t       | f      |      | f         | f    | f     |"
            , "| undefined | t   | t       | o      | t    |           | o    | f     |"
            , "| null      | t   | t       | o      | o    | o         |      | f     |"
            , "| never     | t   | t       | t      | t    | t         | t    |       |"
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

type ParserHelper a = Text -> ParserResult a

shouldEvalTo :: HasCallStack => Text -> Text -> Expectation
shouldEvalTo input output =
  program' input `shouldBe` program output

-- | Like `shouldEvalTo` but compares the pretty printed output.
shouldEvalPretty :: HasCallStack => Text -> Text -> Expectation
shouldEvalPretty input output =
  case (program input, program output) of
    (Right p1, Right p2) ->
      let p1' = show . PP.pretty . evalProgram $ p1
          p2' = show . PP.pretty $ p2
       in p1' `shouldBe` p2'
    (Left e, _) -> expectationFailure $ errorBundlePretty e
    (_, Left e) -> expectationFailure $ errorBundlePretty e

expr :: ParserHelper Expr
expr = parse pExpr

program' :: ParserHelper Program
program' t = evalProgram <$> program t

program :: ParserHelper Program
program = parse pProgram

unlines' :: [Text] -> Text
unlines' = stripEnd . unlines
