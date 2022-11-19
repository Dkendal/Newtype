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
import Debug.Trace
import Language.Haskell.TH
import Newtype.Prettyprinter
import Prettyprinter
import Text.Nicify
import Text.Regex.TDFA
import Prelude hiding (any)

import Data.Generics.Uniplate.Data qualified as U
import Debug qualified
import Newtype.Syntax.Eval qualified as Eval
import Newtype.Syntax.Internal
import Newtype.Syntax.Newtype (LetBinding (LetBinding))
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

fromProgram :: NT.Program -> Program
fromProgram prgm@(NT.Program stmts) =
  Program (fromStatment tbl `Maybe.mapMaybe` stmts)
  where
    tbl = Eval.collectDefinitions prgm

fromStatment :: Eval.SymbolTable -> NT.Statement -> Maybe Statement
fromStatment tbl = \case
  NT.ImportDeclaration {..} -> Just ImportDeclaration {..}
  NT.ExportStatement a -> Just $ ExportStatement a
  NT.STestDefinition a -> Eval.execTest tbl a
  NT.TypeDefinition {..} ->
    Just $
      TypeDefinition
        { params = fmapExpr' <$> params
        , body = fromExpr tbl body
        , name
        }
  NT.InterfaceDefinition {..} ->
    Just $
      InterfaceDefinition
        { params = fmapExpr' <$> params
        , extends = extends
        , props = fmapExpr' <$> props
        , ..
        }
  where
    fmapExpr' :: forall (f :: * -> *). Functor f => f NT.Expr -> f Expr
    fmapExpr' = fmapExpr tbl

fromExpr :: Eval.SymbolTable -> NT.Expr -> Expr
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
  NT.Hole -> error "'holes' should only appear as the last branch of a case statement"
  -- FIXME: this will have issues with shadowing I think?
  -- Maybe it won't though as this should be bottom up
  -- FIXME: ignore quoted forms
  NT.ExprLet (NT.Let bindings body) -> f (U.transform t body)
    where
      tbl :: Eval.SymbolTable
      tbl = Map.fromList [(k, v) | LetBinding k v <- bindings]

      -- | Retrieve current expr from the let binding, if it's an ident.
      get a = do
        name <- getIdentName a
        Map.lookup name tbl

      t :: NT.Expr -> NT.Expr
      t expr = case get expr of
        Just (NT.SymbolLit a) -> a
        Just (NT.SymbolFunc params body) -> error "not implemented"
        Nothing -> expr
  NT.Quote a -> error "quoted form should exist within an unquoted form"
  NT.Unquote a -> f . simplify' $ a
  where
    f = fromExpr tbl
    simplify' = Eval.simplify tbl
    fmapExpr' :: forall (f :: * -> *). Functor f => f NT.Expr -> f Expr
    fmapExpr' = fmapExpr tbl

-- | Is an ident with the matching name
isMatchIdent :: String -> NT.Expr -> Bool
isMatchIdent name expr =
  getIdentName expr == Just name

getIdentName :: NT.Expr -> Maybe String
getIdentName = \case
  NT.ExprIdent (NT.Ident name) -> Just name
  _ -> Nothing

-- | Transform all Expression fields in a record.
fmapExpr :: forall (f :: * -> *). Functor f => Eval.SymbolTable -> f NT.Expr -> f Expr
fmapExpr tbl = fmap (fromExpr tbl)
