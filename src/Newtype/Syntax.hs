{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Newtype.Syntax where

import Control.Monad
import Data.Generics.Uniplate
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
        params :: Maybe TypeParams,
        body :: Expr
      }
  | InterfaceDefinition
      { name :: String,
        params :: Maybe TypeParams,
        extends :: [Expr],
        props :: [ObjectLiteralProperty]
      }
  deriving (Eq, Show)

instance Pretty Statement where
  pretty ImportDeclaration {..} =
    "import" <+> pretty importClause <+> "from" <+> dquotes (pretty fromClause)
  pretty TypeDefinition {..} =
    group ("type" <+> pretty name) <> group (nest 2 (line <> "=" <+> pretty body))
  pretty ExportStatement = emptyDoc
  pretty InterfaceDefinition {..} =
    (group "interface" <+> pretty name) <+> vsep [lbrace, body, rbrace]
    where
      body = indent 2 (align (vsep (map ((<> semi) . pretty) props)))

  prettyList statements = vsep (map pretty statements)

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

data TypeParams = TypeParams
  deriving (Eq, Show)

data Expr
  = StringLiteral String
  | NumberIntegerLiteral Integer
  | NumberDoubleLiteral Double
  | BooleanLiteral Bool
  | ObjectLiteral [ObjectLiteralProperty]
  | TypeApplication String [Expr]
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
  | Union Expr Expr
  | Intersection Expr Expr
  | CaseStatement Expr [Case]
  deriving (Eq, Show)

data Case
  = Case Expr Expr
  | ElseCase Expr
  deriving (Eq, Show)

instance Pretty Expr where
  pretty (NumberIntegerLiteral value) = pretty value
  pretty (NumberDoubleLiteral value) = pretty value
  pretty (BooleanLiteral True) = "true"
  pretty (BooleanLiteral False) = "false"
  pretty (StringLiteral value) = dquotes . pretty $ value
  pretty (TypeApplication typeName []) = pretty typeName
  pretty (TypeApplication typeName params) = pretty typeName <> (angles . hsep . punctuate comma . map pretty $ params)
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
  pretty (Tuple exprs) = prettyList exprs
  pretty (Intersection left right) =
    fmt left <> line <> "&" <+> fmt right
    where
      fmt (Union a b) = prettyOpList (Union a b)
      fmt a = pretty a
  pretty (Union left right) =
    fmt left <> line <> "|" <+> fmt right
    where
      fmt (Intersection a b) = prettyOpList (Intersection a b)
      fmt a = pretty a
  pretty (CaseStatement a b) =
    pretty (simplify (CaseStatement a b))

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
    (group readonly <> pretty key <> optional <> ":") <+> pretty value
    where
      readonly =
        case isReadonly of
          Just True -> "readonly" <> space
          Just False -> "-readonly" <> space
          Nothing -> emptyDoc
      optional =
        case isOptional of
          Just True -> "?"
          Just False -> "-?"
          Nothing -> emptyDoc

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
