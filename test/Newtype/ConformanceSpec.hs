{-# LANGUAGE OverloadedStrings #-}

module Newtype.ConformanceSpec (spec) where

import Data.Text
import Newtype.Parser
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Test.Hspec
import Text.Megaparsec
import Prelude hiding (unlines)

spec :: Spec
spec = do
  describe "Newtype.Parser" $ do
    describe "a program" specProgram
    describe "an expression" specExpression

specProgram :: SpecWith ()
specProgram =
  do
    it "can have successive type definitions with simple definitions" $ do
      let actual =
            [ "type A = 1",
              "type B = 2"
            ]
          expected =
            [ "type A = 1",
              "",
              "type B = 2"
            ]
       in pp pProgram (unlines actual) `shouldBe` here expected
    it "can have successive type definitions with complex terms" $ do
      let actual =
            [ "type T1 = (A.B)",
              "type T2 = (B.C)"
            ]
          expected =
            [ "type T1 = A.B",
              "",
              "type T2 = B.C"
            ]
       in pp pProgram (unlines actual) `shouldBe` here expected

specExpression :: SpecWith ()
specExpression = do
  describe "as a primitive type" $ do
    it "[]" $ do
      let src =
            ["[]"]
       in pp (pExpr <* eof) (unlines src) `shouldBe` here src
    it "unknown" $ do
      let src =
            ["unknown"]
       in pp (pExpr <* eof) (unlines src) `shouldBe` here src
    it "any" $ do
      let src =
            ["any"]
       in pp (pExpr <* eof) (unlines src) `shouldBe` here src
    it "never" $ do
      let src =
            ["never"]
       in pp (pExpr <* eof) (unlines src) `shouldBe` here src
    it "{}" $ do
      let src =
            ["{}"]
       in pp (pExpr <* eof) (unlines src) `shouldBe` here src
    it "number" $ do
      let src =
            ["number"]
       in pp (pExpr <* eof) (unlines src) `shouldBe` here src
    it "false" $ do
      let src =
            ["false"]
       in pp (pExpr <* eof) (unlines src) `shouldBe` here src
    it "true" $ do
      let src =
            ["true"]
       in pp (pExpr <* eof) (unlines src) `shouldBe` here src
  specIfThenElse
  describe "can be a generic type" $ do
    it "can have one parameter" $ do
      let nt =
            ["A 1"]
          ts =
            ["A<1>"]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts
    it "can have multiple parameters" $ do
      let nt =
            ["A 1 true {} 4"]
          ts =
            ["A<1, true, {}, 4>"]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts
    it "can be nested" $ do
      let nt =
            ["Node (Node 1) 2"]
          ts =
            ["Node<Node<1>, 2>"]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts
    it "can span multiple lines" $ do
      let nt =
            [ "Node",
              "  (Node 1)",
              "  2"
            ]
          ts =
            ["Node<Node<1>, 2>"]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts

specIfThenElse :: SpecWith ()
specIfThenElse =
  xdescribe "if ... then ... else" $ do
    it "can have an extends condition" $ do
      let nt =
            ["if LHS <: RHS then Then else Else"]
          ts =
            ["LHS extends RHS ? Then : Else"]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts

    it "supports boolean expressions" $ do
      let nt =
            ["if A <: C and B <: C and A <: C then 1"]
          ts =
            [""]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts

    it "doesn't need an else expr" $ do
      let nt = ["if LHS <: RHS then Then"]
          ts = ["LHS extends RHS ? Then : never"]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts

    it "can use the extends-right operator" $ do
      let nt = ["if LHS :> RHS then Then"]
          ts = ["RHS extends LHS ? Then : never"]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts

    it "can negate conditions" $ do
      let nt = ["if not LHS <: RHS then Then"]
          ts = ["LHS extends RHS ? never : Then"]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts

    it "can use the infer operator (?)" $ do
      let nt = ["if Left <: Right ?T then Then"]
          ts = ["Left extends Right<infer T> ? Then : never"]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts

-- testWhitespaceSensitivity :: TestTree
-- testWhitespaceSensitivity =
--   describe
--     "white space sensitivity"
--     [ it "empty program" $
--         assertPretty
--           pProgram
--           "\n\n\n"
--           "",
--       it "Program with extra newlines at the start" $
--         assertPretty
--           pProgram
--           (unlines [" ", "type A = 1", "type B = 2"])
--           (here ["type A = 1", "", "type B = 2"]),
--       it "Program with extra newlines at the end" $
--         assertPretty
--           pProgram
--           (unlines ["type A = 1", "type B = 2", ""])
--           (here ["type A = 1", "", "type B = 2"]),
--       it "Program with extra newlines between" $
--         assertPretty
--           pProgram
--           (unlines ["type A = 1", " ", "type B = 2"])
--           (here ["type A = 1", "", "type B = 2"]),
--       it "type definition with no indent" $
--         assertPretty
--           pProgram
--           "type A =\n\
--           \1"
--           "",
--       it "type definition with indent after equals" $
--         assertPretty
--           pProgram
--           (unlines ["type A =", "  1"])
--           "type A = 1",
--       it "type definition with indent before equals" $
--         assertPretty
--           pProgram
--           (unlines ["type A", "  = 1"])
--           "type A = 1",
--       it "type definition must be indented" $
--         assertParserError
--           pProgram
--           (unlines ["type A", "= 1"])
--           ["incorrect indentation (got 1, should be greater than 1)\n"],
--       it "type definition must be indented" $
--         assertParserError
--           pProgram
--           (unlines ["type A =", "1"])
--           ["incorrect indentation (got 1, should be greater than 1)\n"]
--     ]
specType :: SpecWith ()
specType =
  describe "type" $ do
    it "Program" $
      let nt =
            [ "type A = 1",
              "type B = 2"
            ]
          ts =
            [ "type A = 1",
              "",
              "type B = 2"
            ]
       in pp pProgram (unlines nt) `shouldBe` here ts

    it "type with parameters" $
      let nt = ["type A T = T"]
          ts = ["type A<T> = T"]
       in pp pProgram (unlines nt) `shouldBe` here ts

    it "type alias" $
      let nt = ["type A = B"]
          ts = ["type A = B"]
       in pp pProgram (unlines nt) `shouldBe` here ts

    it "type alias dot access" $
      let nt = ["type A = B.C"]
          ts = ["type A = B.C"]
       in pp pProgram (unlines nt) `shouldBe` here ts

specImport :: SpecWith ()
specImport =
  describe "Import" $ do
    it "import" $
      let nt = ["import \"mod\" (x)"]
          ts = ["import {x} from \"mod\""]
       in pp (pImport <* eof) (unlines nt) `shouldBe` here ts

    it "import" $
      let nt = ["import \"mod\" (x, y)"]
          ts = ["import {x, y} from \"mod\""]
       in pp (pImport <* eof) (unlines nt) `shouldBe` here ts

specInterface :: SpecWith ()
specInterface =
  describe "Interface" $ do
    it "interface definition" $
      let nt =
            [ "interface A where",
              "  foo : 0",
              "  bar : 1"
            ]
          ts =
            [ "interface A {",
              "  foo: 0;",
              "  bar: 1;",
              "}"
            ]
       in pp (pStatement <* eof) (unlines nt) `shouldBe` here ts

-- TODO refute
-- it "interface must be indent" $
--   assertPretty
--     (pStatement <* eof)
--     "interface A where\n\
--     \foo: 0\n\
--     \bar: 1"
--     "",
specMappedType :: SpecWith ()
specMappedType =
  describe "Mapped Type" $ do
    it "parsed as an expression" $
      let nt = ["v for k in K"]
          ts = ["{[k in K]: v}"]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts

    it "implicit as expr" $
      let nt = ["v for k in K"]
          ts = ["{[k in K]: v}"]
       in pp (pMappedType <* eof) (unlines nt) `shouldBe` here ts

    it "explict as expr" $
      let nt = ["v for k in K as foo"]
          ts = ["{[k in K as foo]: v}"]
       in pp (pMappedType <* eof) (unlines nt) `shouldBe` here ts

    it "as expr is optimized" $
      let nt = ["v for k in K as k"]
          ts = ["{[k in K]: v}"]
       in pp (pMappedType <* eof) (unlines nt) `shouldBe` here ts

    it "readonly" $
      let nt = ["v for k in K as readonly k"]
          ts = ["{readonly [k in K]: v}"]
       in pp (pMappedType <* eof) (unlines nt) `shouldBe` here ts

    it "optional" $
      let nt = ["v for k in K as k?"]
          ts = ["{[k in K]?: v}"]
       in pp (pMappedType <* eof) (unlines nt) `shouldBe` here ts

    it "optional" $
      let nt = ["(1 | 2) for k in K"]
          ts = ["{[k in K]: 1 | 2}"]
       in pp (pMappedType <* eof) (unlines nt) `shouldBe` here ts

specAccess :: SpecWith ()
specAccess =
  describe "Access" $ do
    it "dot access" $
      let nt = ["A.B"]
          ts = ["A.B"]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts

    it "access" $
      let nt = ["A ! B"]
          ts = ["A[B]"]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts

specBuiltin :: SpecWith ()
specBuiltin =
  describe "BuiltIn" $ do
    it "keyof" $
      let nt = ["keyof A"]
          ts = ["keyof A"]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts

specExpr :: SpecWith ()
specExpr =
  describe "Expression" $ do
    it "id" $
      let nt = ["A"]
          ts = ["A"]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts

    it "generic type" $
      let nt = ["A 1"]
          ts = ["A<1>"]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts

    it "multi parameter generic type" $
      let nt = ["A 1 true {}"]
          ts = ["A<1, true, {}>"]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts

    it "nested generic type" $
      let nt = ["A (B true) {}"]
          ts = ["A<B<true>, {}>"]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts

-- it "multi line generic type" $
--   assertPretty
--     (pExpr <* eof)
--     ((stripEnd . unlines) ["A", "  1", "  2"])
--     "A<1, 2>",

specCase :: SpecWith ()
specCase =
  describe "case expression" $ do
    it "case statement" $
      let nt =
            [ "case A of",
              " B ->",
              "   B a a a",
              " C ->",
              "   C a a a"
            ]
          ts = ["A extends B ? B<a, a, a> : A extends C ? C<a, a, a> : never"]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts

    it "case statement with fall through" $
      let nt =
            [ "case A of",
              " 0 -> 1",
              " _ -> 2"
            ]
          ts = ["A extends 0 ? 1 : 2"]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts

    it "case statement" $
      let nt =
            [ "case A of",
              " B -> B a a a",
              " C -> C a a a"
            ]
          ts = ["A extends B ? B<a, a, a> : A extends C ? C<a, a, a> : never"]
       in pp (pExpr <* eof) (unlines nt) `shouldBe` here ts

pp :: (VisualStream s, TraversableStream s, ShowErrorComponent e, Pretty a) => Parsec e s a -> s -> [Char]
pp parser input =
  case parse parser "" input of
    Left a ->
      show (errorBundlePretty a)
    Right a ->
      unpack (renderStrict (layoutPretty (LayoutOptions Unbounded) (pretty a)))

here :: [Text] -> String
here = unpack . stripEnd . unlines
