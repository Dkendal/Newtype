{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Newtype.ParserSpec (spec) where

import Newtype.Parser
import Test.Hspec hiding (Expectation, expectationFailure, shouldBe)
import Test.Hspec.Newtype
import Prelude as P hiding (lines, unlines)

spec :: Spec
spec = do
  describe "primitive types" $ do
    specify "string" $ do
      shouldCompile
        pProgram
        [nt|A : string|]
        [ts|type A = string|]

    specify "number" $ do
      shouldCompile
        pProgram
        [nt|A : number|]
        [ts|type A = number|]

    specify "object" $ do
      shouldCompile
        pProgram
        [nt|A : object|]
        [ts|type A = object|]

    specify "boolean" $ do
      shouldCompile
        pProgram
        [nt|A : boolean|]
        [ts|type A = boolean|]

    specify "any" $ do
      shouldCompile
        pProgram
        [nt|A : any|]
        [ts|type A = any|]

    specify "unknown" $ do
      shouldCompile
        pProgram
        [nt|A : unknown|]
        [ts|type A = unknown|]

    specify "never" $ do
      shouldCompile
        pProgram
        [nt|A : never|]
        [ts|type A = never|]

  describe "objects" $ do
    specify "empty" $ do
      shouldCompile
        pProgram
        [nt|A : {}|]
        [ts|type A = {}|]

    specify "with properties" $ do
      shouldCompile
        pProgram
        [nt|A : {x: 1, y: 2}|]
        [ts|type A = {x: 1, y: 2}|]

    specify "with readonly properties" $ do
      shouldCompile
        pProgram
        [nt|A : {readonly x: 1}|]
        [ts|type A = {readonly x: 1}|]

    specify "with optional properties" $ do
      shouldCompile
        pProgram
        [nt|A : {x?: 1}|]
        [ts|type A = {x?: 1}|]

    specify "with readonly optional properties" $ do
      shouldCompile
        pProgram
        [nt|A : {readonly x?: 1}|]
        [ts|type A = {readonly x?: 1}|]

    specify "with index property" $ do
      shouldCompile
        pProgram
        [nt|A : {index x: any}|]
        [ts|type A = {[x]: any}|] -- FIXME: this is not valid TypeScript
  describe "interfaces" $ do
    specify "empty interface" $ do
      shouldCompile
        pProgram
        [nt|interface A|]
        [ts|interface A {}|]

    specify "with properties" $ do
      shouldCompile
        pProgram
        [nt|interface A where
           |  x : 1
           |  y : 2
           |]
        [ts|interface A {
           |  x: 1,
           |  y: 2,
           |}
           |]

    specify "with readonly properties" $ do
      shouldCompile
        pProgram
        [nt|interface A where
           |  readonly x : 1
           |]
        [ts|interface A {
           |  readonly x: 1,
           |}
           |]

    specify "with optional properties" $ do
      shouldCompile
        pProgram
        [nt|interface A where
           |  x? : 1
           |]
        [ts|interface A {
           |  x?: 1,
           |}
           |]

    specify "with readonly optional properties" $ do
      shouldCompile
        pProgram
        [nt|interface A where
           |  readonly x? : 1
           |]
        [ts|interface A {
           |  readonly x?: 1,
           |}
           |]

    specify "with index property" $ do
      shouldCompile
        pProgram
        [nt|interface A where
           |  index x : any
           |]
        -- FIXME: this is not valid TypeScript
        [ts|interface A {
           |  [x]: any,
           |}
           |]

  describe "conditional types" $ do
    specify "if-then-else" $ do
      shouldCompile
        pProgram
        [nt|A : if T <: any then 1 else 2|]
        [ts|type A = T extends true ? 1 : 2|]

    specify "case expr" $ do
      shouldCompile
        pProgram
        [nt|A : case T of
           |      true  -> 1
           |      false -> 2
           |]
        [ts|type A = T extends true ? 1 : A extends false ? 2 : never|]
