{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Newtype.Syntax where

import Control.Monad
import qualified Data.Data as D
import qualified Data.List as List
import Data.Maybe
import Prettyprinter
import Text.Regex.TDFA

newtype Program = Program {statements :: [Statement]}
  deriving (Eq, Show)

data Statement
  = ImportDeclaration
      { importClause :: ImportClause
      , fromClause :: String
      }
  | ExportStatement [Ident]
  | TypeDefinition
      { name :: String
      , params :: [TypeParam]
      , body :: Expr
      }
  | InterfaceDefinition
      { name :: String
      , params :: [TypeParam]
      , extends :: Maybe Extensible
      , props :: [Property]
      }
  | TestDefinition
      { name :: String
      , body :: Expr
      }
  deriving (Eq, Show, D.Data, D.Typeable)

data Extensible
  = ExtendIdent Ident
  | ExtendGeneric GenericApplication
  deriving (Eq, Show, D.Data, D.Typeable)

data ImportClause
  = ImportClauseDefault String
  | ImportClauseNS String
  | ImportClauseNamed [ImportSpecifier]
  | ImportClauseDefaultAndNS
      { defaultBinding :: String
      , namespaceBinding :: String
      }
  | ImportClauseDefaultAndNamed
      { defaultBinding :: String
      , namedBindings :: [ImportSpecifier]
      }
  deriving (Eq, Show, D.Data, D.Typeable)

data ImportSpecifier
  = ImportedBinding String
  | ImportedAlias {from :: String, to :: String}
  deriving (Eq, Show, D.Data, D.Typeable)

data TypeParam = TypeParam
  { name :: String
  , defaultValue :: Maybe Expr
  , constraint :: Maybe Expr
  }
  deriving (Eq, Show, D.Data, D.Typeable)

type FnFormalParam = (String, Expr)

data Expr
  = Literal Literal
  | ExprGenericApplication GenericApplication
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

data ListValue
  = ListValue {label :: Maybe String, value :: Expr}
  | ListRest {label :: Maybe String, value :: Expr}
  deriving (Eq, Show, D.Data, D.Typeable)

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

data TemplateString
  = TemplateRaw String
  | TemplateSubstitution Expr
  deriving (Eq, Show, D.Data, D.Typeable)

data GenericApplication
  = GenericApplication Ident [Expr]
  deriving (Eq, Show, D.Data, D.Typeable)

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

newtype Ident = Ident String
  deriving (Eq, Ord, Show, D.Data, D.Typeable)

data ConditionalType = ConditionalType
  { lhs :: Expr
  , rhs :: Expr
  , thenExpr :: Expr
  , elseExpr :: Expr
  }
  deriving (Show, Eq, D.Data, D.Typeable)

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

-------------------------------------------------------------------------------
-- Pretty instances                                                          --
-------------------------------------------------------------------------------
instance Pretty Program where
  pretty (Program statements) = prettyList statements

instance Pretty Statement where
  pretty ImportDeclaration {..} =
    "import" <+> pretty importClause <+> "from" <+> dquotes (pretty fromClause) <> semi
  --
  pretty TypeDefinition {..} =
    group head <> group (nest 2 body') <> semi
    where
      head = "type" <+> pretty name <> prettyList params
      body' = line <> "=" <+> pretty body
  --
  pretty (ExportStatement s) =
    "export" <+> (braces . hsep . punctuate comma . map pretty) s <> semi
  --
  pretty InterfaceDefinition {..} =
    head <+> vsep [lbrace, body, rbrace]
    where
      head = group "interface" <+> pretty name <> prettyList params
      body = indent 2 (align (vsep (map ((<> semi) . pretty) props)))
  pretty TestDefinition {} = error "TODO"

  prettyList statements = vsep (punctuate line (map pretty statements))

instance Pretty ImportClause where
  pretty (ImportClauseDefault binding) = pretty binding
  pretty (ImportClauseNS binding) = "* as " <> pretty binding
  pretty (ImportClauseNamed namedBindings) = prettyList namedBindings
  pretty ImportClauseDefaultAndNS {..} = pretty defaultBinding <+> pretty namespaceBinding
  pretty ImportClauseDefaultAndNamed {..} = pretty defaultBinding <+> prettyList namedBindings

instance Pretty ImportSpecifier where
  prettyList = braces . hsep . punctuate comma . map pretty
  pretty (ImportedBinding binding) = pretty binding
  pretty ImportedAlias {..} = pretty from <+> "as" <+> pretty to

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

instance Pretty Expr where
  pretty (Literal x) = pretty x
  pretty Hole = "_"
  pretty (PrimitiveType t) = pretty t
  -- If asExpr and key are equal
  pretty MappedType {asExpr = Just (Literal (StringLiteral as)), key, ..}
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
  pretty (Keyof a) = group ("keyof" <+> pretty a)
  pretty (Readonly a) = group ("readonly" <+> pretty a)
  pretty (Typeof a) = group ("typeof" <+> pretty a)
  pretty (Access a b) = pretty a <> "[" <> pretty b <> "]"
  pretty (DotAccess a b) = pretty a <> "." <> pretty b
  pretty (ExprGenericApplication a) = pretty a
  pretty (ExprIdent id) = pretty id
  pretty (ExprInferIdent (Ident id)) = group "infer" <+> pretty id
  pretty (ExprInferIdentConstraint (Ident id) constraint) = group "infer" <+> pretty id <+> "extends" <+> pretty constraint
  pretty (Array expr) =
    case expr of
      ExprInferIdent {} -> p
      ExprInferIdentConstraint {} -> p
      _ -> pretty expr <> "[]"
    where
      p = (parens . pretty $ expr) <> "[]"
  pretty (Tuple l) =
    case l of
      [] -> "[]"
      _ -> (brackets . hsep) (punctuate comma (map pretty l))
  pretty (Intersection left right) =
    fmt left <> softline <> "&" <> softline <> fmt right
    where
      fmt (Union a b) = prettyOpList (Union a b)
      fmt a = pretty a
  pretty (Union left right) =
    fmt left <> softline <> "|" <+> fmt right
    where
      fmt (Intersection a b) = prettyOpList (Intersection a b)
      fmt a = pretty a
  pretty (ExprConditionalType a) = pretty a
  pretty (TemplateLiteral a) = case a of
    [] -> "``"
    _ -> "`" <> cat (map pretty a) <> "`"

instance Pretty ListValue where
  pretty (ListValue Nothing a) = pretty a
  pretty (ListRest Nothing a) = "..." <> pretty a
  pretty (ListValue (Just l) a) = pretty l <> ":" <+> pretty a
  pretty (ListRest (Just l) a) = pretty l <> ": ..." <> pretty a

instance Pretty TemplateString where
  pretty (TemplateRaw s) = pretty s
  pretty (TemplateSubstitution e) = "${" <> pretty e <> "}"

instance Pretty GenericApplication where
  pretty (GenericApplication ident []) = pretty ident
  pretty (GenericApplication typeName params) =
    pretty typeName <> (angles . hsep . punctuate comma . map pretty $ params)

instance Pretty Ident where
  pretty (Ident s) = pretty s

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

instance Pretty TypeParam where
  pretty (TypeParam name defaultValue constraint) =
    pretty name
      <> maybe emptyDoc (\d -> " extends " <> pretty d) constraint
      <> maybe emptyDoc (\d -> " = " <> pretty d) defaultValue
  prettyList [] = emptyDoc
  prettyList l = angles . hsep . punctuate comma . map pretty $ l

instance Pretty Property where
  pretty IndexSignature {..} =
    doc
    where
      doc = lhs <+> pretty value
      lhs = group readonly <> brackets keyDoc <> colon
      keyDoc = (pretty key <> colon <+> pretty keySource)
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

instance Pretty ConditionalType where
  pretty (ConditionalType lhs rhs then' else') =
    group . parens $ doc
    where
      doc = condition <> nest 2 body

      condition = pretty lhs <+> "extends" <+> pretty rhs

      body = thenDoc <> elseDoc

      thenDoc = line <> "?" <+> pretty then'

      elseDoc = line <> ":" <+> pretty else'

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

-------------------------------------------------------------------------------
-- Helpers                                                                   --
-------------------------------------------------------------------------------

prettyReadonly :: Maybe Bool -> Doc ann
prettyReadonly Nothing = emptyDoc
prettyReadonly (Just False) = "-readonly" <> space
prettyReadonly (Just True) = "readonly" <> space

prettyOptional :: Maybe Bool -> Doc ann
prettyOptional Nothing = emptyDoc
prettyOptional (Just False) = "-?"
prettyOptional (Just True) = "?"

prettyOpList :: Expr -> Doc ann
prettyOpList a =
  group $ align $ enclose (flatAlt "( " "(") (flatAlt " )" ")") $ pretty a

-- Test if a string is a valid Javascript identifier name
isIdentifierName :: String -> Bool
isIdentifierName str = str =~ ("^[a-zA-Z_$][a-zA-Z0-9_$]*$" :: String)
