{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Newtype.Syntax
  ( Program (..),
    Statement (..),
    Expr (..),
    ImportClause (..),
    TypeParam (..),
    Case (..),
    ObjectLiteralProperty (..),
    ComparisionOperator (..),
    ImportSpecifier (..),
  )
where

import Control.Monad
import Data.Generics.Uniplate
import Data.Maybe
import Prettyprinter

newtype Program = Program {statements :: [Statement]}
  deriving (Eq, Show)

instance Pretty Program where
  pretty (Program statements) = prettyList statements

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
        extends :: [Expr],
        props :: [ObjectLiteralProperty]
      }
  deriving (Eq, Show)

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

instance Pretty ImportClause where
  pretty (ImportClauseDefault binding) = pretty binding
  pretty (ImportClauseNS binding) = "* as " <> pretty binding
  pretty (ImportClauseNamed namedBindings) = prettyList namedBindings
  pretty ImportClauseDefaultAndNS {..} = pretty defaultBinding <+> pretty namespaceBinding
  pretty ImportClauseDefaultAndNamed {..} = pretty defaultBinding <+> prettyList namedBindings

data ImportSpecifier
  = ImportedBinding String
  | ImportedAlias {from :: String, to :: String}
  deriving (Eq, Show)

instance Pretty ImportSpecifier where
  pretty (ImportedBinding binding) = pretty binding
  pretty ImportedAlias {..} = pretty from <+> "as" <+> pretty to
  prettyList lst =
    braces . hsep . punctuate comma . map pretty $ lst

data TypeParam = TypeParam
  { name :: String
  }
  deriving (Eq, Show)

instance Pretty TypeParam where
  pretty (TypeParam s) = pretty s
  prettyList [] = emptyDoc
  prettyList l = angles . hsep . punctuate comma . map pretty $ l

data Expr
  = StringLiteral String
  | NumberIntegerLiteral Integer
  | NumberDoubleLiteral Double
  | BooleanLiteral Bool
  | ObjectLiteral [ObjectLiteralProperty]
  | TypeApplication String [Expr]
  | Access Expr Expr
  | DotAccess Expr Expr
  | Builtin String Expr
  | ID String
  | InferID String
  | Tuple [Expr]
  | ExtendsExpr
      { lhs :: Expr,
        negate :: Bool,
        op :: ComparisionOperator,
        rhs :: Expr,
        ifBody :: Expr,
        elseBody :: Expr
      }
  | MappedType
      { value :: Expr,
        propertyKey :: Expr,
        propertyKeySource :: Expr,
        asExpr :: Maybe Expr,
        isReadonly :: Maybe Bool,
        isOptional :: Maybe Bool
      }
  | Union Expr Expr
  | Intersection Expr Expr
  | CaseStatement Expr [Case]
  deriving (Eq, Show)

instance Pretty Expr where
  pretty MappedType {asExpr = Just asExpr, propertyKey, ..}
    | asExpr == propertyKey =
      pretty MappedType {asExpr = Nothing, ..}
  pretty MappedType {..} =
    braces (lhs <+> pretty value)
    where
      as = case asExpr of
        Nothing -> emptyDoc
        (Just expr) -> space <> "as" <+> pretty expr
      index = pretty propertyKey <+> "in" <+> pretty propertyKeySource <> as
      lhs = prettyReadonly isReadonly <> (brackets index <> prettyOptional isOptional <> colon)
  pretty (Builtin a b) = group (pretty a <+> pretty b)
  pretty (Access a b) = pretty a <> "[" <> pretty b <> "]"
  pretty (DotAccess a b) = pretty a <> "." <> pretty b
  pretty (NumberIntegerLiteral value) = pretty value
  pretty (NumberDoubleLiteral value) = pretty value
  pretty (BooleanLiteral True) = "true"
  pretty (BooleanLiteral False) = "false"
  pretty (StringLiteral value) = dquotes . pretty $ value
  pretty (TypeApplication typeName []) = pretty typeName
  pretty (TypeApplication typeName params) = pretty typeName <> (angles . hsep . punctuate comma . map pretty $ params)
  pretty (ObjectLiteral []) = "{}"
  pretty (ObjectLiteral props) =
    group
      ( encloseSep
          (flatAlt "{ " "{")
          (flatAlt " }" "}")
          ", "
          (map pretty props)
      )
  pretty (ID name) = pretty name
  pretty (InferID name) = group "infer" <+> pretty name
  pretty ExtendsExpr {negate = True, ..} =
    pretty
      ExtendsExpr
        { negate = False,
          ifBody = elseBody,
          elseBody = ifBody,
          ..
        }
  pretty ExtendsExpr {op = ExtendsLeft, ..} =
    pretty lhs <+> "extends" <+> pretty rhs
      <+> "?"
      <+> pretty ifBody
      <+> ":"
      <+> pretty elseBody
  pretty ExtendsExpr {op = ExtendsRight, ..} =
    pretty
      ExtendsExpr
        { lhs = rhs,
          rhs = lhs,
          op = ExtendsLeft,
          ..
        }
  pretty ExtendsExpr {op = Equals, ..} =
    pretty
      ExtendsExpr
        { lhs = Tuple [lhs],
          rhs = Tuple [rhs],
          op = ExtendsLeft,
          ..
        }
  pretty ExtendsExpr {op = NotEquals, ..} =
    pretty
      ExtendsExpr
        { lhs = Tuple [lhs],
          rhs = Tuple [rhs],
          op = ExtendsLeft,
          ifBody = elseBody,
          elseBody = ifBody,
          ..
        }
  pretty (Tuple []) = "[]"
  pretty (Tuple exprs) = prettyList exprs
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
  pretty (CaseStatement a b) =
    pretty (simplify (CaseStatement a b))

data Case
  = Case Expr Expr
  | ElseCase Expr
  deriving (Eq, Show)

data BinaryOp

data ComparisionOperator
  = ExtendsLeft
  | ExtendsRight
  | Equals
  | NotEquals
  deriving (Eq, Show)

data ObjectLiteralProperty = KeyValue
  { isReadonly :: Maybe Bool,
    isOptional :: Maybe Bool,
    key :: String,
    value :: Expr
  }
  deriving (Eq, Show)

instance Pretty ObjectLiteralProperty where
  pretty KeyValue {..} =
    lhs <+> pretty value
    where
      readonly = prettyReadonly isReadonly
      optional = prettyOptional isOptional
      lhs = group readonly <> pretty key <> optional <> ":"

prettyReadonly :: Maybe Bool -> Doc ann
prettyReadonly Nothing = emptyDoc
prettyReadonly (Just False) = "-readonly" <> space
prettyReadonly (Just True) = "readonly" <> space

prettyOptional :: Maybe Bool -> Doc ann
prettyOptional Nothing = emptyDoc
prettyOptional (Just False) = "-?"
prettyOptional (Just True) = "?"

never :: Expr
never = ID "never"

prettyOpList :: Expr -> Doc ann
prettyOpList a =
  group $ align $ enclose (flatAlt "( " "(") (flatAlt " )" ")") $ pretty a

simplify :: Expr -> Expr
simplify (CaseStatement term [Case rhs ifBody, Case (ID "_") elseBody]) =
  ExtendsExpr
    { lhs = simplify term,
      op = ExtendsLeft,
      negate = False,
      elseBody = elseBody,
      ..
    }
simplify (CaseStatement term [Case rhs ifBody]) =
  ExtendsExpr
    { lhs = simplify term,
      op = ExtendsLeft,
      negate = False,
      elseBody = never,
      ..
    }
simplify (CaseStatement term (Case rhs ifBody : tl)) =
  ExtendsExpr
    { lhs = simplify term,
      op = ExtendsLeft,
      negate = False,
      elseBody = simplify (CaseStatement term tl),
      ..
    }
simplify a = a
