{-# LANGUAGE DuplicateRecordFields #-}

module Newtype.Syntax where

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
