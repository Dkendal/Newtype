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
    ConditionalExpr (..),
    BoolExpr (..),
    ConditionalType (..),
    ImportSpecifier (..),
    Ident (..),
    expandConditional,
    mkIdent
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

newtype TypeParam = TypeParam {name :: String}
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
  pretty (PrimitiveType t) = pretty t
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
  pretty (CaseStatement a b) =
    pretty (simplify (CaseStatement a b))
  pretty (ExprConditionalType a) = pretty a

data PrimitiveType
  = TNever
  | TAny
  | TUnknown
  | TNumber
  | TString
  | TBoolean
  | TNull
  | TUndefined
  | TVoid
  deriving (Eq, Show)

instance Pretty PrimitiveType where
  pretty TNever = "never"
  pretty TAny = "any"
  pretty TUnknown = "unknown"
  pretty TNumber = "number"
  pretty TString = "string"
  pretty TBoolean = "boolean"
  pretty TNull = "null"
  pretty TUndefined = "undefined"
  pretty TVoid = "void"

newtype Ident = Ident String
  deriving (Eq, Show)

instance Pretty Ident where
  pretty (Ident s) = pretty s

data ConditionalType = ConditionalType
  { lhs :: Expr,
    rhs :: Expr,
    thenExpr :: Expr,
    elseExpr :: Expr
  }
  deriving (Show, Eq)

instance Pretty ConditionalType where
  pretty (ConditionalType a b then' else') =
    group (pretty a <+> "extends" <+> pretty b)
      <> nest
        2
        ( softline <> (group "?" <+> pretty then')
            <> softline
            <> (group ":" <+> pretty else')
        )

data ConditionalExpr = ConditionalExpr
  { condition :: BoolExpr,
    thenExpr :: Expr,
    elseExpr :: Expr
  }
  deriving (Show, Eq)

instance Pretty ConditionalExpr where
  pretty cexp = (pretty . expandConditional) cexp

data BoolExpr
  = And BoolExpr BoolExpr
  | Or BoolExpr BoolExpr
  | Not BoolExpr
  | ExtendsLeft Expr Expr
  | ExtendsRight Expr Expr
  | Equals Expr Expr
  | NotEquals Expr Expr
  deriving (Show, Eq)

data Case
  = Case Expr Expr
  | ElseCase Expr
  deriving (Eq, Show)

data BinaryOp

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

prettyOpList :: Expr -> Doc ann
prettyOpList a =
  group $ align $ enclose (flatAlt "( " "(") (flatAlt " )" ")") $ pretty a

-- simplify :: Expr -> Expr
-- simplify (CaseStatement term [Case rhs ifBody, Case (ID "_") elseBody]) =
--   ExtendsExpr
--     { lhs = simplify term,
--       op = ExtendsLeft,
--       negate = False,
--       elseBody = elseBody,
--       ..
--     }
-- simplify (CaseStatement term [Case rhs ifBody]) =
--   ExtendsExpr
--     { lhs = simplify term,
--       op = ExtendsLeft,
--       negate = False,
--       elseBody = never,
--       ..
--     }
-- simplify (CaseStatement term (Case rhs ifBody : tl)) =
--   ExtendsExpr
--     { lhs = simplify term,
--       op = ExtendsLeft,
--       negate = False,
--       elseBody = simplify (CaseStatement term tl),
--       ..
--     }
simplify a = a

cx :: BoolExpr -> Expr -> Expr -> ConditionalExpr
cx = ConditionalExpr

ct :: ConditionalType -> Expr
ct = ExprConditionalType

extends' :: Expr -> Expr -> Expr -> Expr -> ConditionalType
extends' = ConditionalType

expandConditional' :: ConditionalExpr -> Expr
expandConditional' = ct . expandConditional

expandConditional :: ConditionalExpr -> ConditionalType
expandConditional (ConditionalExpr (ExtendsLeft a b) then' else') =
  extends' a b then' else'
expandConditional (ConditionalExpr (ExtendsRight b a) then' else') =
  extends' a b then' else'
expandConditional (ConditionalExpr (Not con) then' else') =
  expandConditional (cx con else' then')
expandConditional (ConditionalExpr (Equals a b) then' else') =
  extends' (Tuple [a]) (Tuple [b]) then' else'
expandConditional (ConditionalExpr (NotEquals a b) then' else') =
  expandConditional (cx (Not (Equals a b)) then' else')
expandConditional (ConditionalExpr (And a b) then' else') =
  let outer then'' = cx a then'' else'
      inner = cx b then' else'
   in expandConditional (outer (expandConditional' inner))
expandConditional (ConditionalExpr (Or a b) then' else') =
  let outer else'' = cx a then' else''
      inner = cx b then' else'
   in expandConditional (outer (expandConditional' inner))

mkIdent :: String -> Expr
mkIdent = ExprIdent . Ident

never :: Expr
never = PrimitiveType TNever

