module Newtype.CompilerSpec (spec) where

import Newtype.Compiler
import Newtype.Syntax.Newtype
import Test.Hspec

spec :: Spec
spec = do
  describe "Compiler" $ do
    describe "addImplicitExport" $ do
      it "adds an export statement with all top level defs" $ do
        let e = ExportStatement [Ident "A"]
        let t = TypeDefinition "A" [] (Literal (LNumberInteger 1))
        let prog = Program [t]
        addImplicitExport prog `shouldBe` Program [e, t]
