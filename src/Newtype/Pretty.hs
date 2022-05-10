{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Newtype.Pretty where

import Control.Monad
import Newtype.Syntax
import Prettyprinter

instance Pretty Program where
  pretty (Program statements) = prettyList statements

instance Pretty Statement where
  pretty ImportDeclaration {..} =
    "import" <+> pretty importClause <+> "from" <+> dquotes (pretty fromClause)
  pretty TypeDefinition {..} =
    "type" <+> pretty name <+> "=" <+> pretty body
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
  pretty (TypeApplication typeName params) = pretty typeName <> (angles . hsep . punctuate comma . map pretty $ params)
  pretty (ObjectLiteral props) =
    braces . vsep . punctuate comma . map pretty $ props
  pretty (Identifier name) = pretty name

instance Pretty ObjectLiteralProperty where
  pretty KeyValue {..} =
    readonly <+> pretty key <> optional <> ":" <+> pretty value
    where
      readonly =
        case isReadonly of
          Just True -> "readonly"
          Just False -> "-readonly"
          Nothing -> emptyDoc
      optional =
        case isOptional of
          Just True -> "?"
          Just False -> "-?"
          Nothing -> emptyDoc
