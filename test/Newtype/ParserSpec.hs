{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Newtype.ParserSpec (spec) where

import Data.Text (Text)
import Newtype.Parser
import Newtype.Syntax.IntermediateRepresentation qualified as IR
import Newtype.Syntax.Typescript qualified as TS
import Test.Hspec hiding (Expectation, expectationFailure, shouldBe)
import Test.Hspec.Expectations.Pretty (Expectation)
import Test.Hspec.Newtype
import Prelude as P hiding (lines, unlines)

shouldCompileProgram :: HasCallStack => Text -> Text -> Expectation
shouldCompileProgram =
  shouldCompileT
    pProgram
    (show . TS.fromIR . IR.fromProgram)

spec :: Spec
spec = do
  describe "primitive types" $ do
    specify "string" $ do
      shouldCompileProgram
        [nt|A : string|]
        [ts|type A = string;
           |]

    specify "number" $ do
      shouldCompileProgram
        [nt|A : number|]
        [ts|type A = number;
           |]

    specify "object" $ do
      shouldCompileProgram
        [nt|A : object|]
        [ts|type A = object;
           |]

    specify "boolean" $ do
      shouldCompileProgram
        [nt|A : boolean|]
        [ts|type A = boolean;
           |]

    specify "any" $ do
      shouldCompileProgram
        [nt|A : any|]
        [ts|type A = any;
           |]

    specify "unknown" $ do
      shouldCompileProgram
        [nt|A : unknown|]
        [ts|type A = unknown;
           |]

    specify "never" $ do
      shouldCompileProgram
        [nt|A : never|]
        [ts|type A = never;
           |]

  describe "objects" $ do
    specify "empty" $ do
      shouldCompileProgram
        [nt|A : {}
           |]
        [ts|type A = {};
           |]

    specify "with properties" $ do
      shouldCompileProgram
        [nt|A : {x: 1, y: 2}
           |]
        [ts|type A = {x: 1, y: 2};
           |]

    specify "with readonly properties" $ do
      shouldCompileProgram
        [nt|A : {readonly x: 1}
           |]
        [ts|type A = {readonly x: 1};
           |]

    specify "with optional properties" $ do
      shouldCompileProgram
        [nt|A : {x?: 1}
           |]
        [ts|type A = {x?: 1};
           |]

    specify "with readonly optional properties" $ do
      shouldCompileProgram
        [nt|A : {readonly x?: 1}
           |]
        [ts|type A = {readonly x?: 1};
           |]

    specify "with index property" $ do
      shouldCompileProgram
        [nt|A : {index x: any}
           |]
        [ts|type A = {[key: x]: any};
           |]
  describe "interfaces" $ do
    specify "empty interface" $ do
      shouldCompileProgram
        [nt|interface A|]
        [ts|interface A {
           |  
           |}
           |]

    specify "with properties" $ do
      shouldCompileProgram
        [nt|interface A where
           |  x : 1
           |  y : 2
           |]
        [ts|interface A {
           |  x: 1;
           |  y: 2;
           |}
           |]

    specify "with readonly properties" $ do
      shouldCompileProgram
        [nt|interface A where
           |  readonly x : 1
           |]
        [ts|interface A {
           |  readonly x: 1;
           |}
           |]

    specify "with optional properties" $ do
      shouldCompileProgram
        [nt|interface A where
           |  x? : 1
           |]
        [ts|interface A {
           |  x?: 1;
           |}
           |]

    specify "with readonly optional properties" $ do
      shouldCompileProgram
        [nt|interface A where
           |  readonly x? : 1
           |]
        [ts|interface A {
           |  readonly x?: 1;
           |}
           |]

    specify "with index property" $ do
      shouldCompileProgram
        [nt|interface A where
           |  index x : any
           |]
        [ts|interface A {
           |  [key: x]: any;
           |}
           |]

  describe "conditional types" $ do
    specify "if-then-else" $ do
      shouldCompileProgram
        [nt|A : if T <: any then 1 else 2|]
        [ts|type A = (T extends any ? 1 : 2);
           |]

    specify "case expr" $ do
      shouldCompileProgram
        [nt|A : case T of
           |      true  -> 1
           |      false -> 2
           |]
        [ts|type A = (T extends true ? 1 : (T extends false ? 2 : never));
           |]

  describe "unquote quote semantics" $ do
    specify "simple single value" $ do
      shouldCompileProgram
        [nt|A : 1
           |B : unquote A
           |]
        [ts|type A = 1;
           |
           |type B = 1;
           |]

    specify "multiple symbols" $ do
      shouldCompileProgram
        [nt|A : 1
           |B : unquote [A, A]
           |]
        [ts|type A = 1;
           |
           |type B = [1, 1];
           |]

    specify "n-depth symbols" $ do
      shouldCompileProgram
        [nt|A : 1
           |B : unquote [A, [A, [A]]]
           |]
        [ts|type A = 1;
           |
           |type B = [1, [1, [1]]];
           |]

    specify "quote specifier" $ do
      shouldCompileProgram
        [nt|A : 1
           |B : unquote [A, quote A]
           |]
        [ts|type A = 1;
           |
           |type B = [1, A];
           |]
