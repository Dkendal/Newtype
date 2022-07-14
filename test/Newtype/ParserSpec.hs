{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Newtype.ParserSpec (spec) where

import Control.Monad (forM_)
import Data.String.Here.Uninterpolated
import Data.Text
import Debug.Trace
import Newtype.Parser
import Newtype.Syntax
import Newtype.Syntax.Conditionals
import Prettyprinter
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Prelude hiding (unlines)

a = mkIdent "A"

b = mkIdent "B"

c = mkIdent "C"

d = mkIdent "D"

e = mkIdent "E"

parse' parser source =
  case runNewTypeParser (parser <* eof) "" source of
    Left err -> error $ errorBundlePretty err
    Right x -> x

spec :: Spec
spec = do
  describe "Newtype.Parser" $ do
    describe "pProgram" $ do
      let subject = parse' pProgram

      it "parses a program with multiple type defintions that use dot access" $ do
        subject
          ( unlines
              [ "type A = a.b",
                "type B = b.c"
              ]
          )
          `shouldBe` Program
            [ TypeDefinition "A" [] (DotAccess (mkIdent "a") (mkIdent "b")),
              TypeDefinition "B" [] (DotAccess (mkIdent "b") (mkIdent "c"))
            ]

      it "can start with a comment" $ do
        subject
          ( unlines
              [ "-- comment",
                "type A = a.b"
              ]
          )
          `shouldBe` Program
            [TypeDefinition "A" [] (DotAccess (mkIdent "a") (mkIdent "b"))]

      it "can start with a blank line" $ do
        subject
          ( unlines
              [ "",
                "type A = a.b"
              ]
          )
          `shouldBe` Program
            [TypeDefinition "A" [] (DotAccess (mkIdent "a") (mkIdent "b"))]

      fit "can parse sample program" $ do
        let src =
              [here|
import "ts-toolbelt" (A, F, L)

import "." ( __ )

import "./const.js"
  ( __capture__
  , __kind__
  , any_
  , bigint_
  , boolean_
  , function_
  , number_
  , object_
  , rest_
  , string_
  , symbol_
  )

True : 1

ExtractSubcapture t :
  if not t <: M.Primitive | M.BuiltIn and t <: object
    then t ! (Exclude (keyof t) (keyof [] | keyof {}))

PartialAssignment key value :
  if not value <: never and key <: string
    then { value : k <- key }

EmptyToNever t :
  case t of
    {} -> never
    _ -> t

Kind
  : typeof any_
  | typeof rest_
  | typeof string_
  | typeof number_
  | typeof boolean_
  | typeof function_
  | typeof symbol_
  | typeof bigint_
  | typeof object_

interface Hole (type_ = any) (label = any) where
  readonly index __kind__ : Label
  T : Type

HoleInnerType t :
  case t of
    Hole ?u -> u
    _ -> t

interface Capture (name <: string = any) (pattern = any) where
  readonly index __capture__ : name
  readonly pattern : pattern

RecursiveExpandCapture t :
  -- Guard against any as mapping it will cause an infite descent
  if t == any
    then t
    else
      case t of
        Hole | Capture -> ExpandCapture T
        Record string any -> { ExpandCapture T!K  : k <- keyof T }
        _ -> t

PatternHandler pattern : (arg : VariableCapture pattern) => unknown

GenericFunctionLiteral : forall t, u . (arg : t) => u

                    |]
        let out = show (pretty (subject src))
        trace out out `shouldBe` ""

      it "can parse sample program" $ do
        let src =
              [here|
                -- Basic type def
                A : B

                -- empty object literal
                A : {}

                -- object literal
                A : { readonly a? : B, b : C }

                -- Type def with conditions
                A a b (c <: Object = any) : B

                -- infer type
                A a :
                  if a <: B ?b
                    then b
                    else a

                -- case
                A a :
                  case a of
                    B -> B
                    D -> D

                -- case with fall through
                A a :
                  case a of
                    B -> B
                    D -> D
                    _ -> a

                -- Empty interface
                interface A

                -- Interface with props
                interface A (b <: any) where
                  readonly index a? : b

                |]
        let out = show (pretty (subject src))
        defaultGolden (goldenName 'pProgram "simple-program-example") (trace out out)

    pStatementSpec

    pExprSpec

    describe "pProperty" $ do
      let subject = parse' pProperty
      it "can parse index property" $ do
        let src = "index key : string"
        let ast =
              DataProperty
                { isIndex = True,
                  isReadonly = Nothing,
                  isOptional = Nothing,
                  accessor = Nothing,
                  key = "key",
                  value = PrimitiveType PrimitiveString
                }
        subject src `shouldBe` ast

    describe "pBoolExpr" $ do
      let subject = parse' pBoolExpr

      it "parses extends left" $ do
        let source = "A <: B"
        subject source `shouldBe` ExtendsLeft a b

      it "parses extends right" $ do
        let source = "A :> B"
        subject source `shouldBe` ExtendsRight a b

      it "parses equals" $ do
        let source = "A == B"
        subject source `shouldBe` Equals a b

      it "parses not equals" $ do
        let source = "A != B"
        subject source `shouldBe` NotEquals a b

      it "parses not" $ do
        let source = "not A <: B"
        subject source `shouldBe` Not (ExtendsLeft a b)

      it "parses and" $ do
        let source = "A <: B and B <: C"
        subject source `shouldBe` And (ExtendsLeft a b) (ExtendsLeft b c)

      it "parses left associative and" $ do
        let source = "A <: B and B <: C or D <: E"
        subject source `shouldBe` Or (And (ExtendsLeft a b) (ExtendsLeft b c)) (ExtendsLeft d e)

      it "parses parenthesized expressions" $ do
        let source = "(A <: B and B <: C) or D <: E"
        subject source `shouldBe` Or (And (ExtendsLeft a b) (ExtendsLeft b c)) (ExtendsLeft d e)

    describe "pCaseStatement" $ do
      let subject = parse' pCaseStatement

      it "should parse multiple cases" $ do
        let source =
              unlines
                [ "case A of",
                  " B -> B",
                  " C -> C"
                ]
        subject source
          `shouldBe` CaseStatement
            (ExprIdent (Ident "A"))
            [ Case (ExprIdent (Ident "B")) (ExprIdent (Ident "B")),
              Case (ExprIdent (Ident "C")) (ExprIdent (Ident "C"))
            ]

      it "should parse default case" $ do
        let source =
              unlines
                [ "case A of",
                  " B -> B",
                  " C -> C",
                  " _ -> A"
                ]
        subject source
          `shouldBe` CaseStatement
            (ExprIdent (Ident "A"))
            [ Case (ExprIdent (Ident "B")) (ExprIdent (Ident "B")),
              Case (ExprIdent (Ident "C")) (ExprIdent (Ident "C")),
              Case Hole (ExprIdent (Ident "A"))
            ]

goldenName :: Show a => a -> String -> String
goldenName s1 s2 = show s1 ++ s2

pStatementSpec :: Spec
pStatementSpec = do
  describe "pStatement" $ do
    let subject = parse' pStatement

    describe "interface definition" $ do
      it "can parse an empty interface" $ do
        let src = unlines ["interface A"]
        subject src `shouldBe` InterfaceDefinition "A" [] Nothing []

      it "can parse an empty interface with params" $ do
        let src = unlines ["interface A t1"]
        let ast =
              InterfaceDefinition
                "A"
                [TypeParam "t1" Nothing Nothing]
                Nothing
                []
        subject src `shouldBe` ast

      it "parses an interface definition" $ do
        let expected =
              InterfaceDefinition
                { name = "A",
                  params =
                    [ TypeParam
                        { name = "t1",
                          defaultValue = Just (PrimitiveType PrimitiveAny),
                          constraint = Just (PrimitiveType PrimitiveString)
                        },
                      TypeParam
                        { name = "t2",
                          defaultValue = Just (PrimitiveType PrimitiveAny),
                          constraint = Just (PrimitiveType PrimitiveNumber)
                        }
                    ],
                  extends =
                    Just
                      ( ExtendGeneric
                          ( GenericApplication
                              (Ident "Foo")
                              [ ExprIdent
                                  (Ident "Bar")
                              ]
                          )
                      ),
                  props =
                    [ DataProperty
                        { isReadonly = Nothing,
                          isOptional = Nothing,
                          isIndex = False,
                          accessor = Nothing,
                          key = "a",
                          value = ExprIdent (Ident "A")
                        },
                      DataProperty
                        { isReadonly = Nothing,
                          isOptional = Nothing,
                          isIndex = False,
                          accessor = Nothing,
                          key = "b",
                          value = ExprIdent (Ident "B")
                        }
                    ]
                }

        subject
          ( unlines
              [ "interface A t1 t2",
                "  when",
                "    t1 <: string",
                "    t2 <: number",
                "  defaults",
                "    t1 = any",
                "    t2 = any",
                "  extends Foo Bar",
                "  where",
                "    a : A",
                "    b : B"
              ]
          )
          `shouldBe` expected

    describe "type definition" $ do
      it "parses a type definition" $ do
        subject "type A = B" `shouldBe` TypeDefinition "A" [] b

      it "parses a type definition with type parameters" $ do
        subject "type A b = B"
          `shouldBe` TypeDefinition
            { name = "A",
              params =
                [ TypeParam
                    { name = "b",
                      defaultValue = Nothing,
                      constraint = Nothing
                    }
                ],
              body = b
            }

      it "parses type definitions with default values for type parameters" $ do
        (defaultGolden (goldenName 'pStatement "-type-when-defaults") . show . subject . unlines)
          [ "type Type A B",
            "  when",
            "    A <: number",
            "    B <: number",
            "  defaults",
            "    A = 1",
            "    B = 2",
            "  = { a: A, b: B }"
          ]

      it "parses a type definition with a definition that uses dot access" $ do
        parse' pProgram "type BuiltIn = M.BuiltIn"
          `shouldBe` Program
            [TypeDefinition "BuiltIn" [] (DotAccess (mkIdent "M") (mkIdent "BuiltIn"))]

pExprSpec :: Spec
pExprSpec = describe "pExpr" $ do
  let subject = parse' pExpr

  literalValueSpec

  describe "dot access" $ do
    it "parses a dot access" $ do
      subject "a.b.c" `shouldBe` (mkIdent "a" `DotAccess` mkIdent "b" `DotAccess` mkIdent "c")

  describe "access" $ do
    it "parses access with the bang operator" $ do
      subject "a ! b" `shouldBe` (mkIdent "a" `Access` mkIdent "b")

    it "parses access with the bang operator and no whitespace" $ do
      subject "a!b" `shouldBe` (mkIdent "a" `Access` mkIdent "b")

  describe "primitive types" $ do
    let cases =
          [ ("string", PrimitiveString),
            ("number", PrimitiveNumber),
            ("boolean", PrimitiveBoolean),
            ("null", PrimitiveNull),
            ("undefined", PrimitiveUndefined),
            ("any", PrimitiveAny),
            ("void", PrimitiveVoid),
            ("never", PrimitiveNever),
            ("object", PrimitiveObject),
            ("unknown", PrimitiveUnknown)
          ]
    forM_ cases $ \(name, expected) ->
      it ("parses " <> name) $ do
        subject (pack name) `shouldBe` PrimitiveType expected

  describe "conditional types" $ do
    it "should parse if ... then ... else" $ do
      let source = "if A <: B then C else D"
      parse' pExpr source `shouldBe` ctExpr a b c d

    it "should parse if ... then" $ do
      let source = "if A <: B then C"
      parse' pExpr source `shouldBe` ctExpr a b c never

  describe "mapped types" $ do
    it "should parse mapped types" $ do
      let source = "{ Value : Key <- Type}"
      let expected =
            MappedType
              { value = mkIdent "Value",
                key = mkIdent "Key",
                source = mkIdent "Type",
                asExpr = Nothing,
                isReadonly = Nothing,
                isOptional = Nothing
              }
      parse' pExpr source `shouldBe` expected

    it "should parse key remapping with the `as` keyword" $ do
      let source = "{ Value : Key as OtherKey <- Type}"
      let expected =
            MappedType
              { value = mkIdent "Value",
                key = mkIdent "Key",
                source = mkIdent "Type",
                asExpr = Just (mkIdent "OtherKey"),
                isReadonly = Nothing,
                isOptional = Nothing
              }
      parse' pExpr source `shouldBe` expected

    it "should parse the `readonly` prefix modifier applied to the key" $ do
      let source = "{ Value : readonly Key <- Type}"
      let expected =
            MappedType
              { value = mkIdent "Value",
                key = mkIdent "Key",
                source = mkIdent "Type",
                asExpr = Nothing,
                isReadonly = Just True,
                isOptional = Nothing
              }
      parse' pExpr source `shouldBe` expected

  describe "generic application" $ do
    it "parses" $ do
      let source = "A B C"
      let expected =
            ExprGenericApplication
              ( GenericApplication
                  (Ident "A")
                  [ ExprIdent (Ident "B"),
                    ExprIdent (Ident "C")
                  ]
              )
      subject source `shouldBe` expected

    it "parses parenthesized expressions" $ do
      let source = "(A B C)"
      let expected =
            ExprGenericApplication
              ( GenericApplication
                  (Ident "A")
                  [ ExprIdent (Ident "B"),
                    ExprIdent (Ident "C")
                  ]
              )
      subject source `shouldBe` expected

    it "doesn't matter how many parens there are" $ do
      let source = "((A B C))"
      let expected =
            ExprGenericApplication
              ( GenericApplication
                  (Ident "A")
                  [ ExprIdent (Ident "B"),
                    ExprIdent (Ident "C")
                  ]
              )

      subject source `shouldBe` expected

    it "should parse nested expressions in the arguments" $ do
      let source = "A (B C)"
      let expected =
            ExprGenericApplication
              ( GenericApplication
                  (Ident "A")
                  [ ExprGenericApplication
                      ( GenericApplication
                          (Ident "B")
                          [ExprIdent (Ident "C")]
                      )
                  ]
              )
      subject source `shouldBe` expected

literalValueSpec :: Spec
literalValueSpec = describe "literal values" $ do
  let subject = parse' pExpr

  describe "numbers" $ do
    it "parses integers" $ do
      let source = "1"
      let ast = NumberIntegerLiteral 1
      subject source `shouldBe` ast

  describe "strings" $
    it "produces an ast" $ do
      let source = "\"foobar\""
      let ast = StringLiteral "foobar"
      subject source `shouldBe` ast

  describe "booleans" $
    it "produces an ast" $ do
      let source = "true"
      let ast = BooleanLiteral True
      subject source `shouldBe` ast

  describe "functions" $ do
    it "parses functions with no formal parameters" $ do
      let source = "() => 1"
      let ast = FunctionLiteral {typeParams = Nothing, params = [], rest = Nothing, returnType = NumberIntegerLiteral 1}
      subject source `shouldBe` ast

    it "parses functions with formal parameters" $ do
      let source = "(x: number, y: number) => number"
      runNewTypeParser pExpr "" source
        `shouldParse` FunctionLiteral
          { typeParams = Nothing,
            params =
              [ ("x", PrimitiveType PrimitiveNumber),
                ("y", PrimitiveType PrimitiveNumber)
              ],
            rest = Nothing,
            returnType = PrimitiveType PrimitiveNumber
          }

    it "parses functions with a rest parameter" $ do
      let source = "(hd: number, ... tl: Array number) => number"
      runNewTypeParser pExpr "" source
        `shouldParse` FunctionLiteral
          { typeParams = Nothing,
            params = [("hd", PrimitiveType PrimitiveNumber)],
            rest =
              Just
                ( "tl",
                  ExprGenericApplication
                    ( GenericApplication
                        (Ident "Array")
                        [ PrimitiveType PrimitiveNumber
                        ]
                    )
                ),
            returnType = PrimitiveType PrimitiveNumber
          }
