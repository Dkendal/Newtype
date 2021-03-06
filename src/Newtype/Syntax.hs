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
  | ExportStatement
  | TypeDefinition
      { name :: String,
        params :: [TypeParam],
        body :: Expr
      }
  | InterfaceDefinition
      { name :: String,
        params :: [TypeParam],
        extends :: Maybe Ident,
        props :: [ObjectLiteralProperty]
      }
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

data Expr
  = StringLiteral String
  | NumberIntegerLiteral Integer
  | NumberDoubleLiteral Double
  | BooleanLiteral Bool
  | ObjectLiteral [ObjectLiteralProperty]
  | GenericApplication Ident [Expr]
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
  deriving (Eq, Show)

newtype Ident = Ident String
  deriving (Eq, Show)

data ConditionalType = ConditionalType
  { lhs :: Expr,
    rhs :: Expr,
    thenExpr :: Expr,
    elseExpr :: Expr
  }
  deriving (Show, Eq)

data ObjectLiteralProperty = KeyValue
  { isReadonly :: Maybe Bool,
    isOptional :: Maybe Bool,
    key :: String,
    value :: Expr
  }
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Pretty instances                                                          --
-------------------------------------------------------------------------------
instance Pretty Program where
  pretty (Program statements) = prettyList statements

instance Pretty Statement where
  pretty ImportDeclaration {..} =
    "import" <+> pretty importClause <+> "from" <+> dquotes (pretty fromClause)
  pretty TypeDefinition {..} =
    group ("type" <+> pretty name <> prettyList params) <> group (nest 2 (line <> "=" <+> pretty body))
  pretty ExportStatement = emptyDoc
  pretty InterfaceDefinition {..} =
    (group "interface" <+> pretty name) <+> vsep [lbrace, body, rbrace]
    where
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
  pretty (GenericApplication ident []) = pretty ident
  pretty (GenericApplication typeName params) = pretty typeName <> (angles . hsep . punctuate comma . map pretty $ params)
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
    fmt left <+> "&" <+> fmt right
    where
      fmt (Union a b) = prettyOpList (Union a b)
      fmt a = pretty a
  pretty (Union left right) =
    fmt left <+> "|" <+> fmt right
    where
      fmt (Intersection a b) = prettyOpList (Intersection a b)
      fmt a = pretty a
  pretty (ExprConditionalType a) = pretty a

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

instance Pretty TypeParam where
  pretty (TypeParam name _ _) = pretty name
  prettyList [] = emptyDoc
  prettyList l = angles . hsep . punctuate comma . map pretty $ l

instance Pretty ObjectLiteralProperty where
  pretty KeyValue {..} =
    lhs <+> pretty value
    where
      readonly = prettyReadonly isReadonly
      optional = prettyOptional isOptional
      lhs = group readonly <> pretty key <> optional <> ":"

instance Pretty ConditionalType where
  pretty (ConditionalType a b then' else') =
    group (pretty a <+> "extends" <+> pretty b)
      <> nest
        2
        ( softline <> (group "?" <+> pretty then')
            <> softline
            <> (group ":" <+> pretty else')
        )

-------------------------------------------------------------------------------
-- Smart constructors                                                        --
-------------------------------------------------------------------------------

mkIdent :: String -> Expr
mkIdent = ExprIdent . Ident

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
