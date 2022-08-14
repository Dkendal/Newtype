{-# LANGUAGE OverloadedStrings #-}

module Test.Hspec.NewtypeSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.Newtype

spec :: Spec
spec = do
  describe "Test.Hspec.NewtypeSpec" $
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
