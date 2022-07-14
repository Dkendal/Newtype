{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Newtype.Syntax where

import Control.Monad
import Data.Generics.Uniplate
import Data.Maybe
import Prettyprinter

newtype Program = Program {statements :: [Statement]}
  deriving (Eq, Show)

data Statement
  = ImportDeclaration
      { importClause :: ImportClause,
        fromClause :: String
      }
  | ExportStatement [Ident]
  | TypeDefinition
      { name :: String,
        params :: [TypeParam],
        body :: Expr
      }
  | InterfaceDefinition
      { name :: String,
        params :: [TypeParam],
        extends :: Maybe Extensible,
        props :: [Property]
      }
  deriving (Eq, Show)

data Extensible
  = ExtendIdent Ident
  | ExtendGeneric GenericApplication
  deriving (Eq, Show)

data ImportClause
  = ImportClauseDefault String
  | ImportClauseNS String
  | ImportClauseNamed [ImportSpecifier]
  | ImportClauseDefaultAndNS
      { defaultBinding :: String,
        namespaceBinding :: String
      }
  | ImportClauseDefaultAndNamed
      { defaultBinding :: String,
        namedBindings :: [ImportSpecifier]
      }
  deriving (Eq, Show)

data ImportSpecifier
  = ImportedBinding String
  | ImportedAlias {from :: String, to :: String}
  deriving (Eq, Show)

data TypeParam = TypeParam
  { name :: String,
    defaultValue :: Maybe Expr,
    constraint :: Maybe Expr
  }
  deriving (Eq, Show)

type FnFormalParam = (String, Expr)

data Expr
  = StringLiteral String
  | NumberIntegerLiteral Integer
  | NumberDoubleLiteral Double
  | FunctionLiteral
      { typeParams :: Maybe [String],
        params :: [FnFormalParam],
        rest :: Maybe FnFormalParam,
        returnType :: Expr
      }
  | BooleanLiteral Bool
  | ObjectLiteral [Property]
  | ExprGenericApplication GenericApplication
  | Access Expr Expr
  | DotAccess Expr Expr
  | PrimitiveType PrimitiveType
  | Builtin String Expr
  | ExprIdent Ident
  | ExprInferIdent Ident
  | Tuple [Expr]
  | ExprConditionalType ConditionalType
  | MappedType
      { value :: Expr,
        key :: Expr,
        source :: Expr,
        asExpr :: Maybe Expr,
        isReadonly :: Maybe Bool,
        isOptional :: Maybe Bool
      }
  | Union Expr Expr
  | Intersection Expr Expr
  | Hole
  deriving (Eq, Show)

data GenericApplication
  = GenericApplication Ident [Expr]
  deriving (Eq, Show)

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
  deriving (Eq, Show)

newtype Ident = Ident String
  deriving (Eq, Ord, Show)

data ConditionalType = ConditionalType
  { lhs :: Expr,
    rhs :: Expr,
    thenExpr :: Expr,
    elseExpr :: Expr
  }
  deriving (Show, Eq)

data Property
  = DataProperty
      { isIndex :: Bool,
        isReadonly :: Maybe Bool,
        isOptional :: Maybe Bool,
        accessor :: Maybe Accessor,
        key :: String,
        value :: Expr
      }
  | AccessorProperty
      { accessor :: Maybe Accessor,
        key :: String,
        value :: Expr
      }
  deriving (Eq, Show)

data Accessor = Getter | Setter
  deriving (Eq, Show)

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

  prettyList statements = vsep (punctuate line (map pretty statements))

instance Pretty ImportClause where
  pretty (ImportClauseDefault binding) = pretty binding
  pretty (ImportClauseNS binding) = "* as " <> pretty binding
  pretty (ImportClauseNamed namedBindings) = prettyList namedBindings
  pretty ImportClauseDefaultAndNS {..} = pretty defaultBinding <+> pretty namespaceBinding
  pretty ImportClauseDefaultAndNamed {..} = pretty defaultBinding <+> prettyList namedBindings

instance Pretty ImportSpecifier where
  pretty (ImportedBinding binding) = pretty binding
  pretty ImportedAlias {..} = pretty from <+> "as" <+> pretty to
  prettyList lst =
    braces . hsep . punctuate comma . map pretty $ lst

instance Pretty Expr where
  pretty FunctionLiteral {..} = doc
    where
      doc :: Doc ann
      doc = (maybe emptyDoc prettyTypeParams typeParams) <> (parens $ prettyParams params <+> prettyRest rest <+> "=>" <+> pretty returnType)
      prettyTypeParams l = "<" <> hsep (punctuate comma (map pretty l)) <> ">"
      prettyParams params = hsep $ punctuate comma $ map prettyParam params
      prettyParam (name, type') = pretty name <> ":" <+> pretty type'
      prettyRest Nothing = emptyDoc
      prettyRest (Just t) = "..." <> prettyParam t
  pretty Hole = "_"
  pretty (PrimitiveType t) = pretty t
  pretty MappedType {asExpr = Just asExpr, key, ..}
    | asExpr == key =
      pretty MappedType {asExpr = Nothing, ..}
  pretty MappedType {..} =
    braces (lhs <+> pretty value)
    where
      as = case asExpr of
        Nothing -> emptyDoc
        (Just expr) -> space <> "as" <+> pretty expr
      index = pretty key <+> "in" <+> pretty source <> as
      lhs = prettyReadonly isReadonly <> (brackets index <> prettyOptional isOptional <> colon)
  pretty (Builtin a b) = group (pretty a <+> pretty b)
  pretty (Access a b) = pretty a <> "[" <> pretty b <> "]"
  pretty (DotAccess a b) = pretty a <> "." <> pretty b
  pretty (NumberIntegerLiteral value) = pretty value
  pretty (NumberDoubleLiteral value) = pretty value
  pretty (BooleanLiteral True) = "true"
  pretty (BooleanLiteral False) = "false"
  pretty (StringLiteral value) = dquotes . pretty $ value
  pretty (ExprGenericApplication a) = pretty a
  pretty (ObjectLiteral []) = "{}"
  pretty (ObjectLiteral props) =
    group
      ( encloseSep
          (flatAlt "{ " "{")
          (flatAlt " }" "}")
          ", "
          (map pretty props)
      )
  pretty (ExprIdent id) = pretty id
  pretty (ExprInferIdent (Ident id)) = group "infer" <+> pretty id
  pretty (Tuple []) = "[]"
  pretty (Tuple exprs) = (brackets . hsep) (punctuate comma (map pretty exprs))
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
  pretty DataProperty {..} =
    lhs <+> pretty value
    where
      readonly = prettyReadonly isReadonly
      optional = prettyOptional isOptional
      lhs =
        group readonly
          <> (if isIndex then "[" <> pretty key <> "]" else pretty key)
          <> optional
          <> ":"
  pretty AccessorProperty {..} =
    error "not implemented"

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

genericAp :: Ident -> [Expr] -> Expr
genericAp = (ExprGenericApplication .) . GenericApplication

ctExpr :: Expr -> Expr -> Expr -> Expr -> Expr
ctExpr lhs rhs then' else' = ExprConditionalType (ConditionalType lhs rhs then' else')

never :: Expr
never = PrimitiveType PrimitiveNever

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
