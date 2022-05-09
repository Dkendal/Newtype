module Newtype.Syntax where

newtype Program = Program {statements :: [Statement]}
  deriving (Eq, Show)

data Statement
  = ImportDeclaration
      { importClause :: ImportClause,
        fromClause :: String
      }
  | ExportStatement
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

data Declaration
  = Var String
  | Const String
  | Let String
  | Function String
  deriving (Eq, Show)

data Definition
  = Type String
  | Interface String
  deriving (Eq, Show)
