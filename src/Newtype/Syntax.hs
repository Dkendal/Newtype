{-# LANGUAGE FlexibleInstances #-}
module Newtype.Syntax (
  module Newtype.Syntax,
  module Newtype.Syntax.Ident,
  module Newtype.Syntax.Expr,
  module Newtype.Syntax.Statement,
) where

import qualified Data.Maybe as Maybe
import Newtype.Syntax.Expr
import Newtype.Syntax.Ident
import Newtype.Syntax.Statement
import qualified Newtype.Syntax.Typescript as TS
import Prettyprinter
import Data.Generics
import qualified Data.Generics.Uniplate.Data as Uniplate

newtype Program = Program {statements :: [Statement]}
  deriving (Eq, Show)

instance Pretty Program where
  pretty (Program statements) = prettyList statements

instance TS.Typescript Program TS.Program where
  toTypescript (Program statements) =
    TS.Program $ Maybe.mapMaybe TS.toTypescript statements

-- Statements may be removed during compilation
instance TS.Typescript Statement (Maybe TS.Statement) where
  toTypescript = \case
    ImportDeclaration _ _ -> error "TODO"
    MacroDefinition {} -> error "TODO"
    InterfaceDefinition {..} -> Just . TS.SInterface $
          TS.Interface 
            { name
            , params = map TS.toTypescript params
            , extends = fmap TS.toTypescript extends
            , props = map TS.toTypescript props
            }
    TestDefinition _ _ -> error "TODO"
    ExportStatement exports -> Just . TS.SExport $ map getIdent exports
    TypeDefinition {..} ->
      Just . TS.SType $
        TS.Type
          { name = name
          , params = map TS.toTypescript params
          , body = TS.toTypescript body
          }


instance TS.Typescript TypeParam TS.TypeParam where
  toTypescript (TypeParam name defaultValue constraint) =
    TS.TypeParam
      name
      (TS.toTypescript <$> defaultValue)
      (TS.toTypescript <$> constraint)
