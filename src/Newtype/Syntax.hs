{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Newtype.Syntax (module Newtype.Syntax, module Newtype.Syntax.Ident, module Newtype.Syntax.Expr) where

import Control.Monad
import qualified Data.Data as D
import qualified Data.List as List
import Data.Maybe
import Newtype.Prettyprinter
import Newtype.Syntax.Expr
import Newtype.Syntax.Ident
import Prettyprinter
import Text.Regex.TDFA

class MacroExpandable a where
  macroExpand :: a -> a

newtype Program = Program {statements :: [Statement]}
  deriving (Eq, Show)

instance Pretty Program where
  pretty (Program statements) = prettyList statements

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
  | MacroDefinition
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

instance Pretty Statement where
  pretty s = case s of
    MacroDefinition {} -> emptyDoc
    TestDefinition {} -> emptyDoc
    ImportDeclaration {..} ->
      "import" <+> pretty importClause <+> "from" <+> dquotes (pretty fromClause) <> semi
    TypeDefinition {..} ->
      group head <> group (nest 2 body') <> semi
      where
        head = "type" <+> pretty name <> prettyList params
        body' = line <> "=" <+> pretty body
    (ExportStatement s) ->
      "export" <+> (braces . hsep . punctuate comma . map pretty) s <> semi
    InterfaceDefinition {..} ->
      head <+> vsep [lbrace, body, rbrace]
      where
        head = group "interface" <+> pretty name <> prettyList params
        body = indent 2 (align (vsep (map ((<> semi) . pretty) props)))

  prettyList statements = vsep (punctuate line (map pretty statements))

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

instance Pretty ImportClause where
  pretty (ImportClauseDefault binding) = pretty binding
  pretty (ImportClauseNS binding) = "* as " <> pretty binding
  pretty (ImportClauseNamed namedBindings) = prettyList namedBindings
  pretty ImportClauseDefaultAndNS {..} = pretty defaultBinding <+> pretty namespaceBinding
  pretty ImportClauseDefaultAndNamed {..} = pretty defaultBinding <+> prettyList namedBindings

data ImportSpecifier
  = ImportedBinding String
  | ImportedAlias {from :: String, to :: String}
  deriving (Eq, Show, D.Data, D.Typeable)

instance Pretty ImportSpecifier where
  prettyList = braces . hsep . punctuate comma . map pretty
  pretty (ImportedBinding binding) = pretty binding
  pretty ImportedAlias {..} = pretty from <+> "as" <+> pretty to
