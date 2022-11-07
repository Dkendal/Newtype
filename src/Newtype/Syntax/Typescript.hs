{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Newtype.Syntax.Typescript (
  module Newtype.Syntax.Typescript,
) where

import Data.Generics qualified as Generics
import Newtype.Prettyprinter
import Prettyprinter

newtype Program = Program [Statement]
  deriving (Eq, Show)

instance Pretty Program where
  pretty (Program statements) = prettyList statements

data Statement
  = SExport [String]
  | SType Type
  | SInterface Interface
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty Statement where
  pretty s = case s of
    SType s -> pretty s
    SInterface s -> pretty s
    SExport s ->
      "export" <+> (braces . hsep . punctuate comma . map pretty) s <> semi

  prettyList statements = vsep (punctuate line (map pretty statements))

data Type = Type
  { name :: String
  , params :: [TypeParam]
  , body :: Expr
  }
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty Type where
  pretty Type {..} =
    group head <> group (nest 2 body') <> semi
    where
      head = "type" <+> pretty name <> prettyList params
      body' = line <> "=" <+> pretty body

data Interface = Interface
  { name :: String
  , params :: [TypeParam]
  , extends :: Maybe Extend
  , props :: [Property]
  }
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty Interface where
  pretty Interface {..} =
    head <+> bodyDoc
    where
      bodyDoc =
        if null props
          then "{}"
          else vsep [lbrace, innerDoc, rbrace]
      head = group "interface" <+> pretty name <> prettyList params
      innerDoc = indent 2 (align propsDoc)
      propsDoc = vsep (map f props)
      -- Format each property with a semi-colon.
      f = (<> semi) . pretty

data Extend
  = ExtendIdent String
  | ExtendGeneric GenericApplication
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty Extend where
  pretty (ExtendIdent s) = "extends" <+> pretty s
  pretty (ExtendGeneric s) = "extends" <+> pretty s

data ImportClause
  = ICDefault String
  | ICNamespace String
  | ICNamed [ImportSpecifier]
  | ICDefaultAndNamespace
      { defaultBinding :: String
      , namespaceBinding :: String
      }
  | ICDefaultAndNamed
      { defaultBinding :: String
      , namedBindings :: [ImportSpecifier]
      }
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty ImportClause where
  pretty expr = case expr of
    ICDefault binding -> pretty binding
    ICNamespace binding -> "* as " <> pretty binding
    ICNamed namedBindings -> prettyList namedBindings
    ICDefaultAndNamespace {..} -> pretty defaultBinding <+> pretty namespaceBinding
    ICDefaultAndNamed {..} -> pretty defaultBinding <+> prettyList namedBindings

data ImportSpecifier
  = ImportedBinding String
  | ImportedAlias {from :: String, to :: String}
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty ImportSpecifier where
  prettyList = braces . hsep . punctuate comma . map pretty
  pretty expr = case expr of
    ImportedBinding binding -> pretty binding
    ImportedAlias {..} -> pretty from <+> "as" <+> pretty to

data Expr
  = Access Expr Expr
  | Array Expr
  | DotAccess Expr Expr
  | EConditionalType ConditionalType
  | EGenericApplication GenericApplication
  | EInferConstraint String Expr
  | ELiteral Literal
  | EMappedType MappedType
  | EPrimitiveType PrimitiveType
  | ETemplate [TemplateString]
  | Ident String
  | Infer String
  | Intersection Expr Expr
  | Keyof Expr
  | Let [Binding] Expr
  | Readonly Expr
  | Tuple [ListValue]
  | Typeof Expr
  | Union Expr Expr
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty Expr where
  pretty e = case e of
    EMappedType a -> pretty a
    ELiteral x -> pretty x
    EPrimitiveType t -> pretty t
    Keyof a -> group ("keyof" <+> pretty a)
    Readonly a -> group ("readonly" <+> pretty a)
    Typeof a -> group ("typeof" <+> pretty a)
    Access a b -> pretty a <> "[" <> pretty b <> "]"
    DotAccess a b -> pretty a <> "." <> pretty b
    EGenericApplication a -> pretty a
    Ident id -> pretty id
    Infer id -> group "infer" <+> pretty id
    EInferConstraint id constraint -> group "infer" <+> pretty id <+> "extends" <+> pretty constraint
    Array expr ->
      case expr of
        Infer {} -> p
        EInferConstraint {} -> p
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
    EConditionalType a -> pretty a
    ETemplate [] -> "``"
    ETemplate a -> "`" <> cat (map pretty a) <> "`"
    Let _ _ -> error "Expected let statement to have been evaluated"

data MappedType = MappedType
  { value :: Expr
  , key :: String
  , source :: Expr
  , asExpr :: Maybe Expr
  , isReadonly :: Maybe Bool
  , isOptional :: Maybe Bool
  }
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty MappedType where
  pretty MappedType {asExpr = Just (ELiteral (LString as)), key, ..}
    | as == key =
        pretty MappedType {asExpr = Nothing, ..}
  pretty MappedType {..} =
    braces (lhs <+> pretty value)
    where
      as = case asExpr of
        Nothing -> emptyDoc
        (Just expr) -> space <> "as" <+> pretty expr
      index = pretty key <+> "in" <+> pretty source <> as
      lhs = prettyReadonly isReadonly <> (brackets index <> prettyOptional isOptional <> colon)

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

data TemplateString
  = TemplateRaw String
  | TemplateSubstitution Expr
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty TemplateString where
  pretty (TemplateRaw s) = pretty s
  pretty (TemplateSubstitution e) = "${" <> pretty e <> "}"

data GenericApplication
  = GenericApplication String [Expr]
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty GenericApplication where
  pretty (GenericApplication ident []) = pretty ident
  pretty (GenericApplication typeName params) =
    pretty typeName <> (angles . hsep . punctuate comma . map pretty $ params)

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
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty PrimitiveType where
  pretty a = case a of
    PrimitiveNever -> "never"
    PrimitiveAny -> "any"
    PrimitiveUnknown -> "unknown"
    PrimitiveNumber -> "number"
    PrimitiveString -> "string"
    PrimitiveBoolean -> "boolean"
    PrimitiveNull -> "null"
    PrimitiveUndefined -> "undefined"
    PrimitiveVoid -> "void"
    PrimitiveBigInt -> "bigint"
    PrimitiveSymbol -> "symbol"
    PrimitiveObject -> "object"
