{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Newtype.Syntax.Expr (
  module Newtype.Syntax.Expr,
  module Newtype.Syntax.Expr.Primitive,
) where

import Data.Generics qualified as Generics
import Data.List qualified as List
import Language.Haskell.TH
import Newtype.Prettyprinter
import Newtype.Syntax.Expr.Primitive
import Newtype.Syntax.Ident
import Newtype.Syntax.Typescript (Typescript, toTypescript)
import Newtype.Syntax.Typescript qualified as TS
import Prettyprinter

data Expr
  = Access Expr Expr
  | Array Expr
  | DotAccess Expr Expr
  | ExprConditionalType ConditionalType
  | ExprGenericApplication GenericApplication
  | ExprIdent Ident
  | ExprInferIdent Ident
  | ExprInferIdentConstraint Ident Expr
  | Hole
  | Intersection Expr Expr
  | Keyof Expr
  | Let [Binding] Expr
  | Literal Literal
  | MappedType
      { value :: Expr
      , key :: String
      , source :: Expr
      , asExpr :: Maybe Expr
      , isReadonly :: Maybe Bool
      , isOptional :: Maybe Bool
      }
  | PrimitiveType PrimitiveType
  | Readonly Expr
  | TemplateLiteral [TemplateString]
  | Tuple [ListValue]
  | Typeof Expr
  | Union Expr Expr
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty Expr where
  pretty e = case e of
    Literal x -> pretty x
    Hole -> "_"
    PrimitiveType t -> pretty t
    -- If asExpr and key are equal
    MappedType {asExpr = Just (Literal (LString as)), key, ..}
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
    Keyof a -> group ("keyof" <+> pretty a)
    Readonly a -> group ("readonly" <+> pretty a)
    Typeof a -> group ("typeof" <+> pretty a)
    Access a b -> pretty a <> "[" <> pretty b <> "]"
    DotAccess a b -> pretty a <> "." <> pretty b
    ExprGenericApplication a -> pretty a
    ExprIdent id -> pretty id
    ExprInferIdent (Ident id) -> group "infer" <+> pretty id
    ExprInferIdentConstraint (Ident id) constraint -> group "infer" <+> pretty id <+> "extends" <+> pretty constraint
    Array expr ->
      case expr of
        ExprInferIdent {} -> p
        ExprInferIdentConstraint {} -> p
        _ -> pretty expr <> "[]"
      where
        p = (parens . pretty $ expr) <> "[]"
    Tuple [] -> "[]"
    Tuple l -> (brackets . hsep) (punctuate comma (map pretty l))
    Intersection left right ->
      fmt left <> softline <> "&" <> softline <> fmt right
      where
        fmt (Union a b) = prettyOpList . pretty $ Union a b
        fmt a = pretty a
    Union left right ->
      fmt left <> softline <> "|" <+> fmt right
      where
        fmt (Intersection a b) = prettyOpList . pretty $ Intersection a b
        fmt a = pretty a
    ExprConditionalType a -> pretty a
    TemplateLiteral [] -> "``"
    TemplateLiteral a -> "`" <> cat (map pretty a) <> "`"
    Let _ _ -> error "Expected let statement to have been evaluated"

instance Typescript Expr TS.Expr where
  toTypescript = \case
    Access lhs rhs -> TS.Access (TS.toTypescript lhs) (TS.toTypescript rhs)
    Array a -> TS.Array . TS.toTypescript $ a
    DotAccess lhs rhs -> TS.DotAccess (TS.toTypescript lhs) (TS.toTypescript rhs)
    ExprConditionalType ct -> TS.EConditionalType $ TS.toTypescript ct
    ExprGenericApplication ga -> TS.EGenericApplication $ TS.toTypescript ga
    ExprIdent id -> TS.Ident $ TS.toTypescript id
    ExprInferIdent id -> TS.Infer $ TS.toTypescript id
    ExprInferIdentConstraint id ex -> TS.EInferConstraint (TS.toTypescript id) (TS.toTypescript ex)
    Hole -> error "Hole"
    Intersection lhs rhs -> TS.Intersection (TS.toTypescript lhs) (TS.toTypescript rhs)
    Keyof ex -> TS.Keyof $ TS.toTypescript ex
    Let binds ex -> error "Let"
    Literal lit -> TS.ELiteral $ TS.toTypescript lit
    MappedType {..} -> TS.EMappedType $ TS.MappedType (TS.toTypescript value) key (TS.toTypescript source) (TS.toTypescript <$> asExpr) isReadonly isOptional
    PrimitiveType pt -> TS.EPrimitiveType $ TS.toTypescript pt
    Readonly ex -> TS.Readonly $ TS.toTypescript ex
    TemplateLiteral tss -> TS.ETemplate $ map TS.toTypescript tss
    Tuple lvs -> TS.Tuple $ map TS.toTypescript lvs
    Typeof ex -> TS.Typeof $ TS.toTypescript ex
    Union lhs rhs -> TS.Union (TS.toTypescript lhs) (TS.toTypescript rhs)

data TypeParam = TypeParam
  { name :: String
  , defaultValue :: Maybe Expr
  , constraint :: Maybe Expr
  }
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

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
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

data ListValue
  = ListValue {label :: Maybe String, value :: Expr}
  | ListRest {label :: Maybe String, value :: Expr}
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty ListValue where
  pretty (ListValue Nothing a) = pretty a
  pretty (ListRest Nothing a) = "..." <> pretty a
  pretty (ListValue (Just l) a) = pretty l <> ":" <+> pretty a
  pretty (ListRest (Just l) a) = pretty l <> ": ..." <> pretty a

instance Typescript ListValue TS.ListValue where
  toTypescript (ListValue a b) = TS.ListValue a (TS.toTypescript b)
  toTypescript (ListRest a b) = TS.ListRest a (TS.toTypescript b)

data Literal
  = LString String
  | LNumberInteger Integer
  | LNumberDouble Double
  | LFunction
      { typeParams :: Maybe [String]
      , params :: [FnFormalParam]
      , rest :: Maybe FnFormalParam
      , returnType :: Expr
      }
  | LBoolean Bool
  | LObject [Property]
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty Literal where
  pretty LFunction {..} = doc
    where
      doc :: Doc ann
      doc = maybe emptyDoc prettyTypeParams typeParams <> parens (prettyParams params <+> prettyRest rest <+> "=>" <+> pretty returnType)
      prettyTypeParams l = "<" <> hsep (punctuate comma (map pretty l)) <> ">"
      prettyParams params = hsep $ punctuate comma $ map prettyParam params
      prettyParam (name, type') = pretty name <> ":" <+> pretty type'
      prettyRest Nothing = emptyDoc
      prettyRest (Just t) = "..." <> prettyParam t
  pretty (LNumberInteger value) = pretty value
  pretty (LNumberDouble value) = pretty value
  pretty (LBoolean True) = "true"
  pretty (LBoolean False) = "false"
  pretty (LString value) = dquotes . pretty $ value
  pretty (LObject []) = "{}"
  pretty (LObject props) =
    group
      ( encloseSep
          (flatAlt "{ " "{")
          (flatAlt " }" "}")
          ", "
          (map pretty props)
      )

instance Typescript Literal TS.Literal where
  toTypescript = \case
    LBoolean b -> TS.LBoolean b
    LNumberInteger n -> TS.LNumberInteger n
    LNumberDouble n -> TS.LNumberDouble n
    LString s -> TS.LString s
    LFunction {} -> error "Cannot convert function to typescript"
    LObject {} -> error "Cannot convert object to typescript"

data TemplateString
  = TemplateRaw String
  | TemplateSubstitution Expr
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty TemplateString where
  pretty (TemplateRaw s) = pretty s
  pretty (TemplateSubstitution e) = "${" <> pretty e <> "}"

instance Typescript TemplateString TS.TemplateString where
  toTypescript (TemplateRaw s) = TS.TemplateRaw s
  toTypescript (TemplateSubstitution e) = TS.TemplateSubstitution $ TS.toTypescript e

data GenericApplication
  = GenericApplication Ident [Expr]
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty GenericApplication where
  pretty (GenericApplication ident []) = pretty ident
  pretty (GenericApplication typeName params) =
    pretty typeName <> (angles . hsep . punctuate comma . map pretty $ params)

instance Typescript GenericApplication TS.GenericApplication where
  toTypescript (GenericApplication ident params) =
    TS.GenericApplication (TS.toTypescript ident) (map TS.toTypescript params)

data ConditionalType = ConditionalType
  { lhs :: Expr
  , rhs :: Expr
  , thenExpr :: Expr
  , elseExpr :: Expr
  }
  deriving (Show, Eq, Generics.Data, Generics.Typeable)

instance Pretty ConditionalType where
  pretty (ConditionalType lhs rhs then' else') =
    group . parens $ doc
    where
      doc = condition <> nest 2 body

      condition = pretty lhs <+> "extends" <+> pretty rhs

      body = thenDoc <> elseDoc

      thenDoc = line <> "?" <+> pretty then'

      elseDoc = line <> ":" <+> pretty else'

instance Typescript ConditionalType TS.ConditionalType where
  toTypescript (ConditionalType lhs rhs then' else') =
    TS.ConditionalType
      { lhs = TS.toTypescript lhs
      , rhs = TS.toTypescript rhs
      , thenExpr = TS.toTypescript then'
      , elseExpr = TS.toTypescript else'
      }

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
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Typescript Property TS.Property where
  toTypescript (DataProperty isReadonly isOptional key value) =
    TS.DataProperty
      { value = TS.toTypescript value
      , ..
      }
  toTypescript (IndexSignature isReadonly key keySource value) =
    TS.IndexSignature
      { keySource = TS.toTypescript keySource
      , value = TS.toTypescript value
      , ..
      }

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
mkString = Literal . LString

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
