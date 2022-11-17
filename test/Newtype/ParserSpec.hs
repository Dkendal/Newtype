{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Newtype.ParserSpec (spec) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Functor
import Data.Text (Text)
import Newtype.Parser
import Newtype.Syntax.IntermediateRepresentation qualified as IR
import Newtype.Syntax.Typescript qualified as TS
import Test.Hspec hiding (Expectation, expectationFailure, shouldBe)
import Test.Hspec.Expectations.Pretty (Expectation)
import Test.Hspec.Newtype
import Prelude as P hiding (lines, unlines)
import Newtype.Syntax.Eval

shouldCompileProgram :: HasCallStack => Text -> Text -> Expectation
shouldCompileProgram =
  shouldCompileT
    pProgram
    (show . TS.fromIR . IR.fromProgram)

-- https://github.com/microsoft/TypeScript/blob/main/tests/cases/conformance/types/
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

    specify "with computed properties" $ do
      shouldCompileProgram
        [nt|A : {'search: 1}
           |]
        [ts|type A = {[Symbol.search]: 1};
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

    specify "regression property merged following interface def" $ do
      shouldCompileProgram
        [nt|interface A where
           |  x : 1
           |  y : 2
           |
           |B : 1
           |]
        [ts|interface A {
           |  x: 1;
           |  y: 2;
           |}
           |
           |type B = 1;
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

  describe "mapped types" $ do
    specify "simple" $ do
      shouldCompileProgram
        [nt|A : { k : v for k in src }
           |]
        [ts|type A = {[k in src]: v};
           |]
    specify "with `as` clause" $ do
      shouldCompileProgram
        [nt|A : { `foo${Capitalize k}` : v for k in src }
           |]
        [ts|type A = {[k in src as `foo${Capitalize<k>}`]: v};
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

    specify "unknown symbols are left as is" $ do
      shouldCompileProgram
        [nt|A : 1
           |B : unquote [A, C]
           |]
        [ts|type A = 1;
           |
           |type B = [1, C];
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

    xspecify "todo: unbounded recursive expansion" $ do
      shouldCompileProgram
        [nt|A : unquote [A]
           |]
        [ts|should error "unbounded recursive expansion"
           |]

    -- todo "todo: bounded recursive expansion"

    specify "n-depth symbols" $ do
      shouldCompileProgram
        [nt|A : 1
           |B : unquote [A, [A, [A]]]
           |]
        [ts|type A = 1;
           |
           |type B = [1, [1, [1]]];
           |]

    specify "quote specifier terminates symbol resolution" $ do
      shouldCompileProgram
        [nt|A : 1
           |B : unquote [A, quote A]
           |]
        [ts|type A = 1;
           |
           |type B = [1, A];
           |]

    -- specify "generic type wrong arity" $ do
    -- specify "generic type default arguments" $ do
    specify "generic type resolution" $ do
      shouldCompileProgram
        [nt|ID a : a
           |B : unquote ID 1
           |]
        [ts|type ID<a> = a;
           |
           |type B = 1;
           |]

    describe "keyof" $ do
      specify "object literal" $ do
        shouldCompileProgram
          [nt|A : unquote (keyof {x: 1, y: 2})
             |]
          [ts|type A = "x" | "y";
             |]
      specify "interface" $ do
        shouldCompileProgram
          [nt|interface A where
             |  x : 1
             |  y : 2
             |
             |B : unquote (keyof A)
             |]
          -- TODO: I don't know if interfaces should be supported here as
          -- I have to make a guarantee about properties that are extended.
          -- [ts|interface A {
          --    |  x: 1;
          --    |}
          --    |
          --    |type B = "x" | "y";
          --    |]
          [ts|interface A {
             |  x: 1;
             |  y: 2;
             |}
             |
             |type B = keyof A;
             |]

    describe "conditional types" $ do
      specify "trivial" $ do
        shouldCompileProgram
          [nt|A : unquote if 1 <: number then 1 else 2|]
          [ts|type A = 1;
             |]

      specify "trivial multiple conditions" $ do
        shouldCompileProgram
          [nt|A : unquote if 1 <: number and 2 <: number then 3 else 4|]
          [ts|type A = 3;
             |]

      specify "undecidable `any`" $ do
        shouldCompileProgram
          [nt|A : unquote if any <: 1 then 1 else 2|]
          [ts|type A = 1 | 2;
             |]

      specify "decidable `any`" $ do
        shouldCompileProgram
          [nt|A : unquote if 1 <: any then 1 else 2|]
          [ts|type A = 1;
             |]

  describe "test" $ do
    specify "simple passing case" $ do
      shouldCompileProgram
        [nt|test "my test" where
           |  assertAssignable 1 number
           |]
        [ts|
           |]
    -- TODO: can't figure out how to get this expression to evaluate
    specify "simple failing case" $ do
      f `shouldThrow` (const True :: Selector TestFailureException)
  where
    src =
      [nt|test "my test" where
                 |  assertAssignable 1 string
                 |]
    f = evaluate . force $ do
      !_ <- IR.fromProgram <$> parse pProgram src
      return ()
