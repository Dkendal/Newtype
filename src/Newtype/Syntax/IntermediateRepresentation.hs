{-# LANGUAGE OverloadedStrings #-}

module Newtype.Syntax.IntermediateRepresentation where

import Control.Applicative ((<|>))
import Control.Monad
import Data.Char qualified
import Data.Data qualified as D
import Data.Dynamic
import Data.Functor
import Data.Generics
import Data.Generics qualified as Generics
import Data.Generics.Uniplate.Data qualified as Uniplate
import Data.List qualified as List
import Data.Map.Strict qualified as FM
import Data.Maybe qualified as Maybe
import Data.String qualified
import Data.String.Here.Interpolated
import Debug.Trace
import Language.Haskell.TH
import Newtype.Prettyprinter
import Newtype.Syntax.Typescript qualified as TS
import Prettyprinter
import Text.Nicify
import Text.Regex.TDFA
import Prelude hiding (any)

import Newtype.Syntax.Newtype qualified as NT

-- Type Definitions {{{
-- These are basically a carbon copy of the Newtype AST, with some nodes
-- removed.
type IRConditionalType = NT.ConditionalType Expr
type IRGenericApplication = NT.GenericApplication Expr
type IRTemplateString = NT.TemplateString Expr
type IRListValue = NT.ListValue Expr
type IRLiteral = NT.Literal Expr
type IRTypeParam = NT.TypeParam Expr
type IRMappedType = NT.MappedType Expr

newtype Program = Program {statements :: [Statement]}
  deriving (Eq, Show)

data Statement
  = ImportDeclaration
      { importClause :: NT.ImportClause
      , fromClause :: String
      }
  | ExportStatement [NT.Ident]
  | TypeDefinition
      { name :: String
      , params :: [IRTypeParam]
      , body :: Expr
      }
  | InterfaceDefinition
      { name :: String
      , params :: [IRTypeParam]
      , extends :: Maybe NT.Extensible
      , props :: [NT.Property]
      }
  deriving (Eq, Show, D.Data, D.Typeable)

data Expr
  = Access Expr Expr
  | Array Expr
  | DotAccess Expr Expr
  | ExprConditionalType ConditionalType
  | ExprGenericApplication IRGenericApplication
  | ExprIdent NT.Ident
  | ExprInferIdent NT.Ident
  | ExprInferIdentConstraint NT.Ident Expr
  | Intersection Expr Expr
  | Keyof Expr
  | Literal IRLiteral
  | ExprMappedType IRMappedType
  | PrimitiveType NT.PrimitiveType
  | Readonly Expr
  | TemplateLiteral [IRTemplateString]
  | Tuple [IRListValue]
  | Typeof Expr
  | Union Expr Expr
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

data ConditionalType = ConditionalType
  { lhs :: Expr
  , rhs :: Expr
  , thenExpr :: Expr
  , elseExpr :: Expr
  }
  deriving (Show, Eq, Generics.Data, Generics.Typeable)

-- Type Definitions }}}
