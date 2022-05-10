{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Newtype.Syntax where

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
        params :: Maybe TypeParams,
        body :: Expression
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

data TypeParams = TypeParams
  deriving (Eq, Show)

data Expression
  = StringLiteral String
  | NumberIntegerLiteral Integer
  | NumberDoubleLiteral Double
  | BooleanLiteral Bool
  | ObjectLiteral [ObjectLiteralProperty]
  | TypeApplication String [Expression]
  | Identifier String
  | InferIdentifier String
  | Tuple [Expression]
  | ExtendsExpression
      { lhs :: Expression,
        op :: ComparisionOperator,
        rhs :: Expression,
        ifBody :: Expression,
        elseBody :: Expression
      }
  deriving (Eq, Show)

data ComparisionOperator
  = ExtendsLeft
  | ExtendsRight
  deriving (Eq, Show)

data ObjectLiteralProperty = KeyValue
  { isReadonly :: Maybe Bool,
    isOptional :: Maybe Bool,
    key :: String,
    value :: Expression
  }
  deriving (Eq, Show)

instance Pretty Program where
  pretty (Program statements) = prettyList statements

instance Pretty Statement where
  pretty ImportDeclaration {..} =
    "import" <+> pretty importClause <+> "from" <+> dquotes (pretty fromClause)
  pretty TypeDefinition {..} =
    group ("type" <+> pretty name <+> "=") <> group (nest 2 (line <> pretty body))
  pretty ExportStatement = emptyDoc

  prettyList statements = vsep (map pretty statements)

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

instance Pretty Expression where
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
  pretty (Identifier name) = pretty name
  pretty (InferIdentifier name) = group "infer" <+> pretty name
  pretty ExtendsExpression {op = ExtendsLeft, ..} =
    pretty lhs <+> "extends" <+> pretty rhs
      <+> "?"
      <+> pretty ifBody
      <+> ":"
      <+> pretty elseBody
  pretty ExtendsExpression {op = ExtendsRight, ..} =
    pretty
      ExtendsExpression
        { lhs = rhs,
          rhs = lhs,
          op = ExtendsLeft,
          ifBody = elseBody,
          elseBody = ifBody
        }
  pretty (Tuple items) = prettyList items

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
