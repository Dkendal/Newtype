{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Newtype.Syntax.Expr where

import Control.Monad
import qualified Data.Data as D
import qualified Data.List as List
import Data.Maybe
import Newtype.Prettyprinter
import Newtype.Syntax.Ident
import Prettyprinter
import Text.Regex.TDFA

data Expr
  = Literal Literal
  | ExprGenericApplication GenericApplication
  | Let [Binding] Expr
  | Access Expr Expr
  | DotAccess Expr Expr
  | PrimitiveType PrimitiveType
  | Keyof Expr
  | Typeof Expr
  | Readonly Expr
  | ExprIdent Ident
  | ExprInferIdent Ident
  | ExprInferIdentConstraint Ident Expr
  | Array Expr
  | Tuple [ListValue]
  | ExprConditionalType ConditionalType
  | TemplateLiteral [TemplateString]
  | MappedType
      { value :: Expr
      , key :: String
      , source :: Expr
      , asExpr :: Maybe Expr
      , isReadonly :: Maybe Bool
      , isOptional :: Maybe Bool
      }
  | Union Expr Expr
  | Intersection Expr Expr
  | Hole
  deriving (Eq, Show, D.Data, D.Typeable)

instance Pretty Expr where
  pretty = \case
    (Literal x) -> pretty x
    Hole -> "_"
    (PrimitiveType t) -> pretty t
    -- If asExpr and key are equal
    MappedType {asExpr = Just (Literal (StringLiteral as)), key, ..}
      | as == key ->
          pretty MappedType {asExpr = Nothing, ..}
    MappedType {..} ->
      braces (lhs <+> pretty value)
      where
        as = case asExpr of
          Nothing -> emptyDoc
          (Just expr) -> space <> "as" <+> pretty expr
        index = pretty key <+> "in" <+> pretty source <> as
        lhs = prettyReadonly isReadonly <> (brackets index <> prettyOptional isOptional <> colon)
    (Keyof a) -> group ("keyof" <+> pretty a)
    (Readonly a) -> group ("readonly" <+> pretty a)
    (Typeof a) -> group ("typeof" <+> pretty a)
    (Access a b) -> pretty a <> "[" <> pretty b <> "]"
    (DotAccess a b) -> pretty a <> "." <> pretty b
    (ExprGenericApplication a) -> pretty a
    (ExprIdent id) -> pretty id
    (ExprInferIdent (Ident id)) -> group "infer" <+> pretty id
    (ExprInferIdentConstraint (Ident id) constraint) -> group "infer" <+> pretty id <+> "extends" <+> pretty constraint
    (Array expr) ->
      case expr of
        ExprInferIdent {} -> p
        ExprInferIdentConstraint {} -> p
        _ -> pretty expr <> "[]"
      where
        p = (parens . pretty $ expr) <> "[]"
    (Tuple []) -> "[]"
    (Tuple l) -> (brackets . hsep) (punctuate comma (map pretty l))
    (Intersection left right) ->
      fmt left <> softline <> "&" <> softline <> fmt right
      where
        fmt (Union a b) = prettyOpList . pretty $ Union a b
        fmt a = pretty a
    (Union left right) ->
      fmt left <> softline <> "|" <+> fmt right
      where
        fmt (Intersection a b) = prettyOpList . pretty $ Intersection a b
        fmt a = pretty a
    (ExprConditionalType a) -> pretty a
    (TemplateLiteral []) -> "``"
    (TemplateLiteral a) -> "`" <> cat (map pretty a) <> "`"
    (Let _ _) -> error "Expected let statement to have been evaluated"

data TypeParam = TypeParam
  { name :: String
  , defaultValue :: Maybe Expr
  , constraint :: Maybe Expr
  }
  deriving (Eq, Show, D.Data, D.Typeable)

instance Pretty TypeParam where
  pretty (TypeParam name defaultValue constraint) =
    pretty name
      <> maybe emptyDoc (\d -> " extends " <> pretty d) constraint
      <> maybe emptyDoc (\d -> " = " <> pretty d) defaultValue
  prettyList [] = emptyDoc
  prettyList l = angles . hsep . punctuate comma . map pretty $ l

type FnFormalParam = (String, Expr)

data Binding = Binding
  { name :: String
  , value :: Expr
  }
  deriving (Eq, Show, D.Data, D.Typeable)

data ListValue
  = ListValue {label :: Maybe String, value :: Expr}
  | ListRest {label :: Maybe String, value :: Expr}
  deriving (Eq, Show, D.Data, D.Typeable)

instance Pretty ListValue where
  pretty (ListValue Nothing a) = pretty a
  pretty (ListRest Nothing a) = "..." <> pretty a
  pretty (ListValue (Just l) a) = pretty l <> ":" <+> pretty a
  pretty (ListRest (Just l) a) = pretty l <> ": ..." <> pretty a

data Literal
  = StringLiteral String
  | NumberIntegerLiteral Integer
  | NumberDoubleLiteral Double
  | FunctionLiteral
      { typeParams :: Maybe [String]
      , params :: [FnFormalParam]
      , rest :: Maybe FnFormalParam
      , returnType :: Expr
      }
  | BooleanLiteral Bool
  | ObjectLiteral [Property]
  deriving (Eq, Show, D.Data, D.Typeable)

instance Pretty Literal where
  pretty FunctionLiteral {..} = doc
    where
      doc :: Doc ann
      doc = maybe emptyDoc prettyTypeParams typeParams <> parens (prettyParams params <+> prettyRest rest <+> "=>" <+> pretty returnType)
      prettyTypeParams l = "<" <> hsep (punctuate comma (map pretty l)) <> ">"
      prettyParams params = hsep $ punctuate comma $ map prettyParam params
      prettyParam (name, type') = pretty name <> ":" <+> pretty type'
      prettyRest Nothing = emptyDoc
      prettyRest (Just t) = "..." <> prettyParam t
  pretty (NumberIntegerLiteral value) = pretty value
  pretty (NumberDoubleLiteral value) = pretty value
  pretty (BooleanLiteral True) = "true"
  pretty (BooleanLiteral False) = "false"
  pretty (StringLiteral value) = dquotes . pretty $ value
  pretty (ObjectLiteral []) = "{}"
  pretty (ObjectLiteral props) =
    group
      ( encloseSep
          (flatAlt "{ " "{")
          (flatAlt " }" "}")
          ", "
          (map pretty props)
      )

data TemplateString
  = TemplateRaw String
  | TemplateSubstitution Expr
  deriving (Eq, Show, D.Data, D.Typeable)

instance Pretty TemplateString where
  pretty (TemplateRaw s) = pretty s
  pretty (TemplateSubstitution e) = "${" <> pretty e <> "}"

data GenericApplication
  = GenericApplication Ident [Expr]
  deriving (Eq, Show, D.Data, D.Typeable)

instance Pretty GenericApplication where
  pretty (GenericApplication ident []) = pretty ident
  pretty (GenericApplication typeName params) =
    pretty typeName <> (angles . hsep . punctuate comma . map pretty $ params)

data PrimitiveType
  = PrimitiveNever
  | PrimitiveAny
  | PrimitiveUnknown
  | PrimitiveNumber
  | PrimitiveBigInt
  | PrimitiveString
  | PrimitiveBoolean
  | PrimitiveNull
  | PrimitiveUndefined
  | PrimitiveVoid
  | PrimitiveObject
  | PrimitiveSymbol
  deriving (Eq, Show, D.Data, D.Typeable)

instance Pretty PrimitiveType where
  pretty PrimitiveNever = "never"
  pretty PrimitiveAny = "any"
  pretty PrimitiveUnknown = "unknown"
  pretty PrimitiveNumber = "number"
  pretty PrimitiveString = "string"
  pretty PrimitiveBoolean = "boolean"
  pretty PrimitiveNull = "null"
  pretty PrimitiveUndefined = "undefined"
  pretty PrimitiveVoid = "void"
  pretty PrimitiveBigInt = "bigint"
  pretty PrimitiveSymbol = "symbol"
  pretty PrimitiveObject = "object"

data ConditionalType = ConditionalType
  { lhs :: Expr
  , rhs :: Expr
  , thenExpr :: Expr
  , elseExpr :: Expr
  }
  deriving (Show, Eq, D.Data, D.Typeable)

instance Pretty ConditionalType where
  pretty (ConditionalType lhs rhs then' else') =
    group . parens $ doc
    where
      doc = condition <> nest 2 body

      condition = pretty lhs <+> "extends" <+> pretty rhs

      body = thenDoc <> elseDoc

      thenDoc = line <> "?" <+> pretty then'

      elseDoc = line <> ":" <+> pretty else'

data Property
  = DataProperty
      { isReadonly :: Maybe Bool
      , isOptional :: Maybe Bool
      , key :: String
      , value :: Expr
      }
  | IndexSignature
      { isReadonly :: Maybe Bool
      , key :: String
      , keySource :: Expr
      , value :: Expr
      }
  deriving (Eq, Show, D.Data, D.Typeable)

instance Pretty Property where
  pretty IndexSignature {..} =
    doc
    where
      doc = lhs <+> pretty value
      lhs = group readonly <> brackets keyDoc <> colon
      keyDoc = pretty key <> colon <+> pretty keySource
      readonly = prettyReadonly isReadonly
  pretty DataProperty {..} =
    doc
    where
      doc = lhs <+> pretty value
      readonly = prettyReadonly isReadonly
      optional = prettyOptional isOptional
      lhs =
        group readonly
          <> pretty key
          <> optional
          <> ":"

-------------------------------------------------------------------------------
-- Smart constructors                                                        --
-------------------------------------------------------------------------------

mkIdent :: String -> Expr
mkIdent = ExprIdent . Ident

mkString :: String -> Expr
mkString = Literal . StringLiteral

lv :: Expr -> ListValue
lv = ListValue Nothing

mkTuple :: [Expr] -> Expr
mkTuple l = Tuple (map lv l)

genericAp :: Ident -> [Expr] -> Expr
genericAp = (ExprGenericApplication .) . GenericApplication

ctExpr :: Expr -> Expr -> Expr -> Expr -> Expr
ctExpr lhs rhs then' else' = ExprConditionalType (ConditionalType lhs rhs then' else')

never :: Expr
never = PrimitiveType PrimitiveNever

any = PrimitiveType PrimitiveAny

union :: [Expr] -> Expr
union [] = never
union [h] = h
union (h : t) = List.foldl Union h t

intersection :: [Expr] -> Expr
intersection [] = never
intersection [h] = h
intersection (h : t) = List.foldl Intersection h t
