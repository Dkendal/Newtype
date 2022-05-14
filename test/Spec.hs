{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import Newtype.Parser hiding (main)
import Newtype.Syntax
import Prettyprinter
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

main :: IO ()
main = defaultMain tests

assertPretty :: Text -> String -> Assertion
assertPretty input output =
  case parse pProgram "" input of
    Left a ->
      assertFailure (errorBundlePretty a)
    Right a ->
      show (pretty a) @?= output

tests :: TestTree
tests =
  testGroup
    "Parser tests"
    [ testCase "Pretty printing" $
        assertPretty "type A = 1" "type A = 1"
    ]
