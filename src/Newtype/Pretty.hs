{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Newtype.Pretty where

import Newtype.Syntax
import Prettyprinter

instance Pretty Program where
  pretty (Program statements) = prettyList statements

instance Pretty Statement where
  pretty ImportDeclaration {..} =
    "import" <+> pretty importClause <+> "from" <+> dquotes (pretty fromClause)
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
