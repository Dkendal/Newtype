{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Newtype.Syntax.IntermediateRepresentation where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.State qualified as State
import Data.Char qualified
import Data.Data qualified as D
import Data.Dynamic
import Data.Functor
import Data.Generics
import Data.Generics qualified as Generics
import Data.Generics.Uniplate.Data qualified as Uniplate
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.String qualified
import Data.String.Here.Interpolated
import Debug.Trace
import Language.Haskell.TH
import Newtype.Prettyprinter
import Prettyprinter
import Text.Nicify
import Text.Regex.TDFA
import Prelude hiding (any)

import Data.Generics.Uniplate.Data qualified as U
import Debug qualified
import Newtype.Syntax.Internal
import Newtype.Syntax.Newtype qualified as NT

-- Type Definitions {{{
-- These are basically a carbon copy of the Newtype AST, with some nodes
-- removed.
type IRConditionalType = ConditionalType Expr
type IRGenericApplication = GenericApplication Expr
type IRTemplateString = TemplateString Expr
type IRListValue = ListValue Expr
type IRLiteral = Literal Expr
type IRTypeParam = TypeParam Expr

type IRMappedType = MappedType Expr
type IRProperty = Property Expr

newtype Program = Program {statements :: [Statement]}
  deriving (Eq, Show)

data Statement
  = ImportDeclaration
      { importClause :: ImportClause
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
      , props :: [IRProperty]
      }
  deriving (Eq, Show, D.Data, D.Typeable)

data Expr
  = Access Expr Expr
  | Array Expr
  | DotAccess Expr Expr
  | ExprConditionalType IRConditionalType
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

-- Type Definitions }}}
data Symbol
  = -- | A callable function that needs to have the parameters resolved
    SymbolFunc {params :: [NT.NTTypeParam], expr :: NT.Expr}
  | -- | A literal expression that can be inlined
    SymbolLit {expr :: NT.Expr}
  deriving (Eq, Show)

type SymbolTable = Map.Map String Symbol

fromProgram :: NT.Program -> Program
fromProgram prgm@(NT.Program stmts) =
  Program (fromStatment tbl <$> stmts)
  where
    tbl = collectDefinitions prgm

collectDefinitions :: NT.Program -> SymbolTable
collectDefinitions (NT.Program stmts) =
  Map.fromList (Maybe.mapMaybe f stmts)
  where
    f NT.TypeDefinition {params = [], ..} =
      Just (name, SymbolLit body)
    f NT.TypeDefinition {..} =
      Just (name, SymbolFunc params body)
    f _ = Nothing

--   p = Program (fromStatment <$> stmts)

fromStatment :: SymbolTable -> NT.Statement -> Statement
fromStatment tbl =
  \case
    NT.ImportDeclaration {..} -> ImportDeclaration {..}
    NT.ExportStatement a -> ExportStatement a
    NT.TypeDefinition {..} ->
      TypeDefinition
        { params = fmapExpr' <$> params
        , body = fromExpr tbl body
        , name
        }
    NT.InterfaceDefinition {..} ->
      InterfaceDefinition
        { params = fmapExpr' <$> params
        , extends = extends
        , props = fmapExpr' <$> props
        , ..
        }
  where
    fmapExpr' :: forall (f :: * -> *). Functor f => f NT.Expr -> f Expr
    fmapExpr' = fmapExpr tbl

fromExpr :: SymbolTable -> NT.Expr -> Expr
fromExpr tbl = \case
  NT.Access a b -> Access (f a) (f b)
  NT.Array a -> Array (f a)
  NT.DotAccess a b -> DotAccess (f a) (f b)
  NT.ExprConditionalType a -> ExprConditionalType $ fmapExpr' a
  NT.ExprGenericApplication a -> ExprGenericApplication $ fmapExpr' a
  NT.ExprIdent a -> ExprIdent a
  NT.ExprInferIdent a -> ExprInferIdent a
  NT.ExprInferIdentConstraint a b -> ExprInferIdentConstraint a (f b)
  NT.ExprMappedType a -> ExprMappedType $ fmapExpr' a
  NT.Intersection a b -> Intersection (f a) (f b)
  NT.Keyof a -> Keyof (f a)
  NT.Literal a -> Literal $ fmapExpr' a
  NT.PrimitiveType a -> PrimitiveType a
  NT.Readonly a -> Readonly (f a)
  NT.TemplateLiteral l -> TemplateLiteral $ fmapExpr' <$> l
  NT.Tuple l -> Tuple $ fmapExpr' <$> l
  NT.Typeof a -> Typeof (f a)
  NT.Union a b -> Union (f a) (f b)
  -- Features that nave no mapping to typescript that need to be simlified.
  NT.Hole -> error "Hole"
  NT.Let a b -> error "Let"
  NT.Quote a -> error "Quote"
  NT.Unquote a -> f (resolveSymbol tbl a)
  where
    f = fromExpr tbl
    fmapExpr' :: forall (f :: * -> *). Functor f => f NT.Expr -> f Expr
    fmapExpr' = fmapExpr tbl

resolveSymbol :: SymbolTable -> NT.Expr -> NT.Expr
resolveSymbol tbl = \case
  NT.ExprIdent (Ident a) -> case Map.lookup a tbl of
    Just (SymbolLit b) -> b
    Just (SymbolFunc _ b) -> b
    Nothing -> error $ "Symbol not found: " <> a
  a -> a

-- | Transform all Expression fields in a record.
fmapExpr :: forall (f :: * -> *). Functor f => SymbolTable -> f NT.Expr -> f Expr
fmapExpr tbl = fmap (fromExpr tbl)

never = PrimitiveType PrimitiveNever
