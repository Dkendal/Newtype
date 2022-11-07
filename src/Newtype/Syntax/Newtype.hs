{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}

module Newtype.Syntax.Newtype where

import Control.Monad
import Data.Data qualified as D
import Data.Generics
import Data.Generics qualified as Generics
import Data.Generics.Uniplate.Data qualified as Uniplate
import Data.List qualified as List
import Data.Maybe
import Data.Maybe qualified as Maybe
import Language.Haskell.TH
import Newtype.Prettyprinter
import Newtype.Syntax.Typescript qualified as TS
import Prettyprinter
import Text.Regex.TDFA

-- All types in this file should implement the `Typescript` class.

class Typescript a b | a -> b where
  toTypescript :: (Pretty b) => a -> b

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
  | PrimitiveObject
  | PrimitiveSymbol
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty PrimitiveType where
  pretty a = case a of
    PrimitiveNever -> "never"
    PrimitiveAny -> "any"
    PrimitiveUnknown -> "unknown"
    PrimitiveNumber -> "number"
    PrimitiveString -> "string"
    PrimitiveBoolean -> "boolean"
    PrimitiveNull -> "null"
    PrimitiveUndefined -> "undefined"
    PrimitiveVoid -> "void"
    PrimitiveBigInt -> "bigint"
    PrimitiveSymbol -> "symbol"
    PrimitiveObject -> "object"

instance Typescript PrimitiveType TS.PrimitiveType where
  toTypescript a = case a of
    PrimitiveNever -> TS.PrimitiveNever
    PrimitiveAny -> TS.PrimitiveAny
    PrimitiveUnknown -> TS.PrimitiveUnknown
    PrimitiveNumber -> TS.PrimitiveNumber
    PrimitiveString -> TS.PrimitiveString
    PrimitiveBoolean -> TS.PrimitiveBoolean
    PrimitiveNull -> TS.PrimitiveNull
    PrimitiveUndefined -> TS.PrimitiveUndefined
    PrimitiveVoid -> TS.PrimitiveVoid
    PrimitiveBigInt -> TS.PrimitiveBigInt
    PrimitiveSymbol -> TS.PrimitiveSymbol
    PrimitiveObject -> TS.PrimitiveObject

newtype Ident = Ident {getIdent :: String}
  deriving (Eq, Ord, Show, Generics.Data, Generics.Typeable)

instance Pretty Ident where
  pretty (Ident s) = pretty s

instance Typescript Ident String where
  toTypescript (Ident x) = x

newtype Program = Program {statements :: [Statement]}
  deriving (Eq, Show)

instance Pretty Program where
  pretty (Program statements) = prettyList statements

instance Typescript Program TS.Program where
  toTypescript (Program statements) =
    TS.Program $ Maybe.mapMaybe toTypescript statements

-- Statements may be removed during compilation
instance Typescript Statement (Maybe TS.Statement) where
  toTypescript = \case
    ImportDeclaration _ _ -> error "TODO"
    MacroDefinition {} -> error "TODO"
    InterfaceDefinition {..} ->
      Just . TS.SInterface $
        TS.Interface
          { name
          , params = map toTypescript params
          , extends = fmap toTypescript extends
          , props = map toTypescript props
          }
    TestDefinition _ _ -> error "TODO"
    ExportStatement exports -> Just . TS.SExport $ map getIdent exports
    TypeDefinition {..} ->
      Just . TS.SType $
        TS.Type
          { name = name
          , params = map toTypescript params
          , body = toTypescript body
          }

instance Typescript TypeParam TS.TypeParam where
  toTypescript (TypeParam name defaultValue constraint) =
    TS.TypeParam
      name
      (toTypescript <$> defaultValue)
      (toTypescript <$> constraint)

--------------------------------------------------------------------------------
-- Conditionals
--------------------------------------------------------------------------------

data ConditionalExpr = ConditionalExpr
  { condition :: BoolExpr
  , thenExpr :: Expr
  , elseExpr :: Expr
  }
  deriving (Show, Eq)

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
  deriving (Eq, Show)

data CaseStatement = CaseStatement Expr [Case]
  deriving (Eq, Show)

expandCaseStatement :: CaseStatement -> Expr
expandCaseStatement (CaseStatement lhs [Case rhs then', Case Hole else']) =
  ct' lhs rhs then' else'
expandCaseStatement (CaseStatement lhs [Case rhs then']) =
  ct' lhs rhs then' never
expandCaseStatement (CaseStatement lhs (Case rhs then' : tl)) =
  ct' lhs rhs then' (expandCaseStatement (CaseStatement lhs tl))
expandCaseStatement (CaseStatement lhs []) = never

expandConditional' :: ConditionalExpr -> Expr
expandConditional' = ExprConditionalType . expandConditional

expandConditional :: ConditionalExpr -> ConditionalType
expandConditional (ConditionalExpr (ExtendsLeft a b) then' else') =
  ct a b then' else'
expandConditional (ConditionalExpr (ExtendsRight b a) then' else') =
  ct a b then' else'
expandConditional (ConditionalExpr (Not con) then' else') =
  expandConditional (cx con else' then')
expandConditional (ConditionalExpr (Equals a b) then' else') =
  ct (t1 a) (t1 b) then' else'
expandConditional (ConditionalExpr (NotEquals a b) then' else') =
  expandConditional (cx (Not (Equals a b)) then' else')
expandConditional (ConditionalExpr (And a b) then' else') =
  let outer then'' = cx a then'' else'
      inner = cx b then' else'
   in expandConditional (outer (expandConditional' inner))
expandConditional (ConditionalExpr (Or a b) then' else') =
  let outer  = cx a then'
      inner = cx b then' else'
   in expandConditional (outer (expandConditional' inner))

cx :: BoolExpr -> Expr -> Expr -> ConditionalExpr
cx = ConditionalExpr

ct' :: Expr -> Expr -> Expr -> Expr -> Expr
ct' lhs rhs then' else' = ExprConditionalType $ ConditionalType lhs rhs then' else'

ct :: Expr -> Expr -> Expr -> Expr -> ConditionalType
ct = ConditionalType

-- | Single element tuple
t1 a = Tuple [ListValue Nothing a]

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

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

instance Typescript Extensible TS.Extend where
  toTypescript (ExtendIdent a) = TS.ExtendIdent (toTypescript a)
  toTypescript (ExtendGeneric a) = TS.ExtendGeneric (toTypescript a)

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

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

data Expr
  = Access Expr Expr
  | Array Expr
  | DotAccess Expr Expr
  | ExprConditionalType ConditionalType
  | ExprGenericApplication GenericApplication
  | ExprIdent Ident
  | ExprInferIdent Ident
  | ExprInferIdentConstraint Ident Expr
  | Hole
  | Intersection Expr Expr
  | Keyof Expr
  | Let [Binding] Expr
  | Literal Literal
  | MappedType
      { value :: Expr
      , key :: String
      , source :: Expr
      , asExpr :: Maybe Expr
      , isReadonly :: Maybe Bool
      , isOptional :: Maybe Bool
      }
  | PrimitiveType PrimitiveType
  | Readonly Expr
  | TemplateLiteral [TemplateString]
  | Tuple [ListValue]
  | Typeof Expr
  | Union Expr Expr
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty Expr where
  pretty e = case e of
    Literal x -> pretty x
    Hole -> "_"
    PrimitiveType t -> pretty t
    -- If asExpr and key are equal
    MappedType {asExpr = Just (Literal (LString as)), key, ..}
      | as == key ->
          pretty MappedType {asExpr = Nothing, ..}
    MappedType {..} ->
      braces (lhs <+> pretty value)
      where
        as = case asExpr of
          Nothing -> emptyDoc
          (Just expr) -> space <> "as" <+> pretty expr
        index = pretty key <+> "in" <+> pretty source <> as
        lhs = prettyReadonly isReadonly <> (brackets index <> prettyOptional isOptional <> colon)
    Keyof a -> group ("keyof" <+> pretty a)
    Readonly a -> group ("readonly" <+> pretty a)
    Typeof a -> group ("typeof" <+> pretty a)
    Access a b -> pretty a <> "[" <> pretty b <> "]"
    DotAccess a b -> pretty a <> "." <> pretty b
    ExprGenericApplication a -> pretty a
    ExprIdent id -> pretty id
    ExprInferIdent (Ident id) -> group "infer" <+> pretty id
    ExprInferIdentConstraint (Ident id) constraint -> group "infer" <+> pretty id <+> "extends" <+> pretty constraint
    Array expr ->
      case expr of
        ExprInferIdent {} -> p
        ExprInferIdentConstraint {} -> p
        _ -> pretty expr <> "[]"
      where
        p = (parens . pretty $ expr) <> "[]"
    Tuple [] -> "[]"
    Tuple l -> (brackets . hsep) (punctuate comma (map pretty l))
    Intersection left right ->
      fmt left <> softline <> "&" <> softline <> fmt right
      where
        fmt (Union a b) = prettyOpList . pretty $ Union a b
        fmt a = pretty a
    Union left right ->
      fmt left <> softline <> "|" <+> fmt right
      where
        fmt (Intersection a b) = prettyOpList . pretty $ Intersection a b
        fmt a = pretty a
    ExprConditionalType a -> pretty a
    TemplateLiteral [] -> "``"
    TemplateLiteral a -> "`" <> cat (map pretty a) <> "`"
    Let _ _ -> error "Expected let statement to have been evaluated"

instance Typescript Expr TS.Expr where
  toTypescript = \case
    Access lhs rhs -> TS.Access (toTypescript lhs) (toTypescript rhs)
    Array a -> TS.Array . toTypescript $ a
    DotAccess lhs rhs -> TS.DotAccess (toTypescript lhs) (toTypescript rhs)
    ExprConditionalType ct -> TS.EConditionalType $ toTypescript ct
    -- ExprGenericApplication (GenericApplication "Unquote" [expr]) -> Eval.evalExpr expr
    ExprGenericApplication ga -> TS.EGenericApplication $ toTypescript ga
    ExprIdent id -> TS.Ident $ toTypescript id
    ExprInferIdent id -> TS.Infer $ toTypescript id
    ExprInferIdentConstraint id ex -> TS.EInferConstraint (toTypescript id) (toTypescript ex)
    Hole -> error "Hole"
    Intersection lhs rhs -> TS.Intersection (toTypescript lhs) (toTypescript rhs)
    Keyof ex -> TS.Keyof $ toTypescript ex
    Let binds ex -> error "Let"
    Literal lit -> TS.ELiteral $ toTypescript lit
    MappedType {..} -> TS.EMappedType $ TS.MappedType (toTypescript value) key (toTypescript source) (toTypescript <$> asExpr) isReadonly isOptional
    PrimitiveType pt -> TS.EPrimitiveType $ toTypescript pt
    Readonly ex -> TS.Readonly $ toTypescript ex
    TemplateLiteral tss -> TS.ETemplate $ map toTypescript tss
    Tuple lvs -> TS.Tuple $ map toTypescript lvs
    Typeof ex -> TS.Typeof $ toTypescript ex
    Union lhs rhs -> TS.Union (toTypescript lhs) (toTypescript rhs)

data TypeParam = TypeParam
  { name :: String
  , defaultValue :: Maybe Expr
  , constraint :: Maybe Expr
  }
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty TypeParam where
  pretty (TypeParam name defaultValue constraint) =
    pretty name
      <> maybe emptyDoc (\d -> " extends " <> pretty d) constraint
      <> maybe emptyDoc (\d -> " = " <> pretty d) defaultValue
  prettyList [] = emptyDoc
  prettyList l = angles . hsep . punctuate comma . map pretty $ l

type FnFormalParam = (String, Expr)

data Binding = Binding
  { name :: String
  , value :: Expr
  }
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

data ListValue
  = ListValue {label :: Maybe String, value :: Expr}
  | ListRest {label :: Maybe String, value :: Expr}
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty ListValue where
  pretty (ListValue Nothing a) = pretty a
  pretty (ListRest Nothing a) = "..." <> pretty a
  pretty (ListValue (Just l) a) = pretty l <> ":" <+> pretty a
  pretty (ListRest (Just l) a) = pretty l <> ": ..." <> pretty a

instance Typescript ListValue TS.ListValue where
  toTypescript (ListValue a b) = TS.ListValue a (toTypescript b)
  toTypescript (ListRest a b) = TS.ListRest a (toTypescript b)

data Literal
  = LString String
  | LNumberInteger Integer
  | LNumberDouble Double
  | LFunction
      { typeParams :: Maybe [String]
      , params :: [FnFormalParam]
      , rest :: Maybe FnFormalParam
      , returnType :: Expr
      }
  | LBoolean Bool
  | LObject [Property]
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty Literal where
  pretty LFunction {..} = doc
    where
      doc :: Doc ann
      doc = maybe emptyDoc prettyTypeParams typeParams <> parens (prettyParams params <+> prettyRest rest <+> "=>" <+> pretty returnType)
      prettyTypeParams l = "<" <> hsep (punctuate comma (map pretty l)) <> ">"
      prettyParams params = hsep $ punctuate comma $ map prettyParam params
      prettyParam (name, type') = pretty name <> ":" <+> pretty type'
      prettyRest Nothing = emptyDoc
      prettyRest (Just t) = "..." <> prettyParam t
  pretty (LNumberInteger value) = pretty value
  pretty (LNumberDouble value) = pretty value
  pretty (LBoolean True) = "true"
  pretty (LBoolean False) = "false"
  pretty (LString value) = dquotes . pretty $ value
  pretty (LObject []) = "{}"
  pretty (LObject props) =
    group
      ( encloseSep
          (flatAlt "{ " "{")
          (flatAlt " }" "}")
          ", "
          (map pretty props)
      )

instance Typescript Literal TS.Literal where
  toTypescript = \case
    LBoolean b -> TS.LBoolean b
    LNumberInteger n -> TS.LNumberInteger n
    LNumberDouble n -> TS.LNumberDouble n
    LString s -> TS.LString s
    LFunction {} -> error "Cannot convert function to typescript"
    LObject {} -> error "Cannot convert object to typescript"

data TemplateString
  = TemplateRaw String
  | TemplateSubstitution Expr
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty TemplateString where
  pretty (TemplateRaw s) = pretty s
  pretty (TemplateSubstitution e) = "${" <> pretty e <> "}"

instance Typescript TemplateString TS.TemplateString where
  toTypescript (TemplateRaw s) = TS.TemplateRaw s
  toTypescript (TemplateSubstitution e) = TS.TemplateSubstitution $ toTypescript e

data GenericApplication
  = GenericApplication Ident [Expr]
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Pretty GenericApplication where
  pretty (GenericApplication ident []) = pretty ident
  pretty (GenericApplication typeName params) =
    pretty typeName <> (angles . hsep . punctuate comma . map pretty $ params)

instance Typescript GenericApplication TS.GenericApplication where
  toTypescript (GenericApplication ident params) =
    TS.GenericApplication
      (toTypescript ident)
      (map toTypescript params)

data ConditionalType = ConditionalType
  { lhs :: Expr
  , rhs :: Expr
  , thenExpr :: Expr
  , elseExpr :: Expr
  }
  deriving (Show, Eq, Generics.Data, Generics.Typeable)

instance Pretty ConditionalType where
  pretty (ConditionalType lhs rhs then' else') =
    group . parens $ doc
    where
      doc = condition <> nest 2 body

      condition = pretty lhs <+> "extends" <+> pretty rhs

      body = thenDoc <> elseDoc

      thenDoc = line <> "?" <+> pretty then'

      elseDoc = line <> ":" <+> pretty else'

instance Typescript ConditionalType TS.ConditionalType where
  toTypescript (ConditionalType lhs rhs then' else') =
    TS.ConditionalType
      { lhs = toTypescript lhs
      , rhs = toTypescript rhs
      , thenExpr = toTypescript then'
      , elseExpr = toTypescript else'
      }

data Property
  = DataProperty
      { isReadonly :: Maybe Bool
      , isOptional :: Maybe Bool
      , key :: String
      , value :: Expr
      }
  | IndexSignature
      { isReadonly :: Maybe Bool
      , key :: String
      , keySource :: Expr
      , value :: Expr
      }
  deriving (Eq, Show, Generics.Data, Generics.Typeable)

instance Typescript Property TS.Property where
  toTypescript (DataProperty isReadonly isOptional key value) =
    TS.DataProperty
      { value = toTypescript value
      , ..
      }
  toTypescript (IndexSignature isReadonly key keySource value) =
    TS.IndexSignature
      { keySource = toTypescript keySource
      , value = toTypescript value
      , ..
      }

instance Pretty Property where
  pretty IndexSignature {..} =
    doc
    where
      doc = lhs <+> pretty value
      lhs = group readonly <> brackets keyDoc <> colon
      keyDoc = pretty key <> colon <+> pretty keySource
      readonly = prettyReadonly isReadonly
  pretty DataProperty {..} =
    doc
    where
      doc = lhs <+> pretty value
      readonly = prettyReadonly isReadonly
      optional = prettyOptional isOptional
      lhs =
        group readonly
          <> pretty key
          <> optional
          <> ":"

-------------------------------------------------------------------------------
-- Smart constructors                                                        --
-------------------------------------------------------------------------------

mkIdent :: String -> Expr
mkIdent = ExprIdent . Ident

mkString :: String -> Expr
mkString = Literal . LString

lv :: Expr -> ListValue
lv = ListValue Nothing

mkTuple :: [Expr] -> Expr
mkTuple l = Tuple (map lv l)

genericAp :: Ident -> [Expr] -> Expr
genericAp = (ExprGenericApplication .) . GenericApplication

ctExpr :: Expr -> Expr -> Expr -> Expr -> Expr
ctExpr lhs rhs then' else' = ExprConditionalType (ConditionalType lhs rhs then' else')

never :: Expr
never = PrimitiveType PrimitiveNever

any = PrimitiveType PrimitiveAny

union :: [Expr] -> Expr
union [] = never
union [h] = h
union (h : t) = List.foldl Union h t

intersection :: [Expr] -> Expr
intersection [] = never
intersection [h] = h
intersection (h : t) = List.foldl Intersection h t
