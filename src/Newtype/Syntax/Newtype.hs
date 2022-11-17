{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Newtype.Syntax.Newtype (
  module Newtype.Syntax.Newtype,
  module Newtype.Syntax.Internal,
) where

import Control.Applicative ((<|>))
import Control.Monad
import Data.Char qualified
import Data.Data qualified as D
import Data.Dynamic
import Data.Functor
import Data.Generics
import Data.Generics qualified as Generics
import Data.Generics.Uniplate.Data qualified as Uniplate
import Data.List qualified as List
import Data.Map.Strict qualified as FM
import Data.Maybe qualified as Maybe
import Data.String qualified
import Data.String.Here.Interpolated
import Debug.Trace
import Language.Haskell.TH
import Newtype.Prettyprinter
import Newtype.Syntax.Internal
import Prettyprinter
import Text.Nicify
import Text.Regex.TDFA
import Prelude hiding (any)

-- Type Definitions {{{
type NTConditionalType = ConditionalType Expr
type NTGenericApplication = GenericApplication Expr
type NTTemplateString = TemplateString Expr
type NTListValue = ListValue Expr
type NTLiteral = Literal Expr
type NTTypeParam = TypeParam Expr
type NTMappedType = MappedType Expr
type NTProperty = Property Expr

newtype Program = Program {statements :: [Statement]}
  deriving (Show, Eq, Data, Typeable)

data ConditionalExpr = ConditionalExpr
  { condition :: BoolExpr
  , thenExpr :: Expr
  , elseExpr :: Expr
  }
  deriving (Show, Eq, Data, Typeable)

data BoolExpr
  = And BoolExpr BoolExpr
  | Or BoolExpr BoolExpr
  | Not BoolExpr
  | ExtendsLeft Expr Expr
  | ExtendsRight Expr Expr
  | Equals Expr Expr
  | NotEquals Expr Expr
  deriving (Show, Eq, Data, Typeable)

data Case
  = Case Expr Expr
  deriving (Show, Eq, Data, Typeable)

data CaseStatement = CaseStatement Expr [Case]
  deriving (Show, Eq, Data, Typeable)

data Statement
  = ImportDeclaration
      { importClause :: ImportClause
      , fromClause :: String
      }
  | ExportStatement [Ident]
  | TypeDefinition
      { name :: String
      , params :: [NTTypeParam]
      , body :: Expr
      }
  | InterfaceDefinition
      { name :: String
      , params :: [NTTypeParam]
      , extends :: Maybe Extensible
      , props :: [NTProperty]
      }
  | STestDefinition TestDefinition
  deriving (Show, Eq, Data, Typeable)

data Assertion
  = AssertAssignable Expr Expr
  deriving (Show, Eq, Data, Typeable)

data TestDefinition = TestDefinition
  { name :: String
  , assertion :: Assertion
  }
  deriving (Show, Eq, Data, Typeable)

data Extensible
  = ExtendIdent Ident
  | ExtendGeneric NTGenericApplication
  deriving (Show, Eq, Data, Typeable)

data Expr
  = Access Expr Expr
  | Array Expr
  | DotAccess Expr Expr
  | ExprConditionalType NTConditionalType
  | ExprGenericApplication NTGenericApplication
  | ExprIdent Ident
  | ExprInferIdent Ident
  | ExprInferIdentConstraint Ident Expr
  | ExprMappedType NTMappedType
  | Hole
  | Intersection Expr Expr
  | Keyof Expr
  | Let [Binding] Expr
  | Literal NTLiteral
  | PrimitiveType PrimitiveType
  | Quote Expr
  | Readonly Expr
  | TemplateLiteral [NTTemplateString]
  | Tuple [NTListValue]
  | Typeof Expr
  | Union Expr Expr
  | Unquote Expr
  deriving (Show, Eq, Data, Typeable)

data Binding = Binding
  { name :: String
  , value :: Expr
  }
  deriving (Show, Eq, Data, Typeable)

-- Type Definitions }}}

-- Pretty Instances {{{
-- instance Pretty PrimitiveType where
--   pretty a = case a of
--     PrimitiveNever -> "never"
--     PrimitiveAny -> "any"
--     PrimitiveUnknown -> "unknown"
--     PrimitiveNumber -> "number"
--     PrimitiveString -> "string"
--     PrimitiveBoolean -> "boolean"
--     PrimitiveNull -> "null"
--     PrimitiveUndefined -> "undefined"
--     PrimitiveVoid -> "void"
--     PrimitiveBigInt -> "bigint"
--     PrimitiveSymbol -> "symbol"
--     PrimitiveObject -> "object"

-- instance Pretty Ident where
--   pretty (Ident s) = pretty s

-- instance Pretty Program where
--   pretty (Program statements) = prettyList statements

-- instance Pretty Statement where
--   pretty s = case s of
--     TestDefinition {} -> emptyDoc
--     ImportDeclaration {..} ->
--       "import" <+> pretty importClause <+> "from" <+> dquotes (pretty fromClause) <> semi
--     TypeDefinition {..} ->
--       group head <> group (nest 2 body') <> semi
--       where
--         head = "type" <+> pretty name <> prettyList params
--         body' = line <> "=" <+> pretty body
--     (ExportStatement s) ->
--       "export" <+> (braces . hsep . punctuate comma . map pretty) s <> semi
--     InterfaceDefinition {..} ->
--       head <+> vsep [lbrace, body, rbrace]
--       where
--         head = group "interface" <+> pretty name <> prettyList params
--         body = indent 2 (align (vsep (map ((<> semi) . pretty) props)))

--   prettyList statements = vsep (punctuate line (map pretty statements))

-- instance Pretty ImportClause where
--   pretty (ImportClauseDefault binding) = pretty binding
--   pretty (ImportClauseNS binding) = "* as " <> pretty binding
--   pretty (ImportClauseNamed namedBindings) = prettyList namedBindings
--   pretty ImportClauseDefaultAndNS {..} = pretty defaultBinding <+> pretty namespaceBinding
--   pretty ImportClauseDefaultAndNamed {..} = pretty defaultBinding <+> prettyList namedBindings

-- instance Pretty ImportSpecifier where
--   prettyList = braces . hsep . punctuate comma . map pretty
--   pretty (ImportedBinding binding) = pretty binding
--   pretty ImportedAlias {..} = pretty from <+> "as" <+> pretty to

-- instance Pretty Expr where
--   pretty e = case e of
--     Literal x -> pretty x
--     Hole -> "_"
--     PrimitiveType t -> pretty t
--     -- If asExpr and key are equal
--     MappedType {asExpr = Just (Literal (LString as)), key, ..}
--       | as == key ->
--           pretty MappedType {asExpr = Nothing, ..}
--     MappedType {..} ->
--       braces (lhs <+> pretty value)
--       where
--         as = case asExpr of
--           Nothing -> emptyDoc
--           (Just expr) -> space <> "as" <+> pretty expr
--         index = pretty key <+> "in" <+> pretty source <> as
--         lhs = prettyReadonly isReadonly <> (brackets index <> prettyOptional isOptional <> colon)
--     Keyof a -> group ("keyof" <+> pretty a)
--     Readonly a -> group ("readonly" <+> pretty a)
--     Typeof a -> group ("typeof" <+> pretty a)
--     Access a b -> pretty a <> "[" <> pretty b <> "]"
--     DotAccess a b -> pretty a <> "." <> pretty b
--     ExprGenericApplication a -> pretty a
--     ExprIdent id -> pretty id
--     ExprInferIdent (Ident id) -> group "infer" <+> pretty id
--     ExprInferIdentConstraint (Ident id) constraint -> group "infer" <+> pretty id <+> "extends" <+> pretty constraint
--     Array expr ->
--       case expr of
--         ExprInferIdent {} -> p
--         ExprInferIdentConstraint {} -> p
--         _ -> pretty expr <> "[]"
--       where
--         p = (parens . pretty $ expr) <> "[]"
--     Tuple [] -> "[]"
--     Tuple l -> (brackets . hsep) (punctuate comma (map pretty l))
--     Intersection left right ->
--       fmt left <> softline <> "&" <> softline <> fmt right
--       where
--         fmt (Union a b) = prettyOpList . pretty $ Union a b
--         fmt a = pretty a
--     Union left right ->
--       fmt left <> softline <> "|" <+> fmt right
--       where
--         fmt (Intersection a b) = prettyOpList . pretty $ Intersection a b
--         fmt a = pretty a
--     ExprConditionalType a -> pretty a
--     TemplateLiteral [] -> "``"
--     TemplateLiteral a -> "`" <> cat (map pretty a) <> "`"
--     Let _ _ -> error "Expected let statement to have been evaluated"

-- instance Pretty TypeParam where
--   pretty (TypeParam name defaultValue constraint) =
--     pretty name
--       <> maybe emptyDoc (\d -> " extends " <> pretty d) constraint
--       <> maybe emptyDoc (\d -> " = " <> pretty d) defaultValue
--   prettyList [] = emptyDoc
--   prettyList l = angles . hsep . punctuate comma . map pretty $ l

-- instance Pretty ListValue where
--   pretty (ListValue Nothing a) = pretty a
--   pretty (ListRest Nothing a) = "..." <> pretty a
--   pretty (ListValue (Just l) a) = pretty l <> ":" <+> pretty a
--   pretty (ListRest (Just l) a) = pretty l <> ": ..." <> pretty a

-- instance Pretty Literal where
--   pretty LFunction {..} = doc
--     where
--       doc :: Doc ann
--       doc = maybe emptyDoc prettyTypeParams typeParams <> parens (prettyParams params <+> prettyRest rest <+> "=>" <+> pretty returnType)
--       prettyTypeParams l = "<" <> hsep (punctuate comma (map pretty l)) <> ">"
--       prettyParams params = hsep $ punctuate comma $ map prettyParam params
--       prettyParam (name, type') = pretty name <> ":" <+> pretty type'
--       prettyRest Nothing = emptyDoc
--       prettyRest (Just t) = "..." <> prettyParam t
--   pretty (LNumberInteger value) = pretty value
--   pretty (LNumberDouble value) = pretty value
--   pretty (LBoolean True) = "true"
--   pretty (LBoolean False) = "false"
--   pretty (LString value) = dquotes . pretty $ value
--   pretty (LObject []) = "{}"
--   pretty (LObject props) =
--     group
--       ( encloseSep
--           (flatAlt "{ " "{")
--           (flatAlt " }" "}")
--           ", "
--           (map pretty props)
--       )

-- instance Pretty TemplateString where
--   pretty (TemplateRaw s) = pretty s
--   pretty (TemplateSubstitution e) = "${" <> pretty e <> "}"

-- instance Pretty GenericApplication where
--   pretty (GenericApplication ident []) = pretty ident
--   pretty (GenericApplication typeName params) =
--     pretty typeName <> (angles . hsep . punctuate comma . map pretty $ params)

-- instance Pretty ConditionalType where
--   pretty (ConditionalType lhs rhs then' else') =
--     group . parens $ doc
--     where
--       doc = condition <> nest 2 body

--       condition = pretty lhs <+> "extends" <+> pretty rhs

--       body = thenDoc <> elseDoc

--       thenDoc = line <> "?" <+> pretty then'

--       elseDoc = line <> ":" <+> pretty else'

-- instance Pretty Property where
--   pretty IndexSignature {..} =
--     doc
--     where
--       doc = lhs <+> pretty value
--       lhs = group readonly <> brackets keyDoc <> colon
--       keyDoc = pretty key <> colon <+> pretty keySource
--       readonly = prettyReadonly isReadonly
--   pretty DataProperty {..} =
--     doc
--     where
--       doc = lhs <+> pretty value
--       readonly = prettyReadonly isReadonly
--       optional = prettyOptional isOptional
--       lhs =
--         group readonly
--           <> pretty key
--           <> optional
--           <> ":"

-- Pretty Instances }}}

-- Typescript Instances {{{
-- class Typescript a b | a -> b where
--  toTypescript :: (Pretty b) => a -> b

-- instance Typescript PrimitiveType TS.PrimitiveType where
--  toTypescript = \case
--    PrimitiveNever -> TS.PrimitiveNever
--    PrimitiveAny -> TS.PrimitiveAny
--    PrimitiveUnknown -> TS.PrimitiveUnknown
--    PrimitiveNumber -> TS.PrimitiveNumber
--    PrimitiveString -> TS.PrimitiveString
--    PrimitiveBoolean -> TS.PrimitiveBoolean
--    PrimitiveNull -> TS.PrimitiveNull
--    PrimitiveUndefined -> TS.PrimitiveUndefined
--    PrimitiveVoid -> TS.PrimitiveVoid
--    PrimitiveBigInt -> TS.PrimitiveBigInt
--    PrimitiveSymbol -> TS.PrimitiveSymbol
--    PrimitiveObject -> TS.PrimitiveObject

-- instance Typescript Ident String where
--  toTypescript (Ident x) = x

-- instance Typescript Program TS.Program where
--  toTypescript (Program statements) =
--    TS.Program $ Maybe.mapMaybe toTypescript statements

---- Statements may be removed during compilation, which is why we need a Maybe here

-- instance Typescript TypeParam TS.TypeParam where
--  toTypescript (TypeParam name defaultValue constraint) =
--    TS.TypeParam
--      name
--      (toTypescript <$> defaultValue)
--      (toTypescript <$> constraint)

----------------------------------------------------------------------------------
---- Conditionals
----------------------------------------------------------------------------------

-- instance Typescript Extensible TS.Extend where
--  toTypescript (ExtendIdent a) = TS.ExtendIdent (toTypescript a)
--  toTypescript (ExtendGeneric a) = TS.ExtendGeneric (toTypescript a)

-- instance Typescript Expr TS.Expr where
--  toTypescript = \case
--    Access lhs rhs -> TS.Access (toTypescript lhs) (toTypescript rhs)
--    Array a -> TS.Array . toTypescript $ a
--    DotAccess lhs rhs -> TS.DotAccess (toTypescript lhs) (toTypescript rhs)
--    ExprConditionalType ct -> TS.EConditionalType $ toTypescript ct
--    ExprGenericApplication ga -> TS.EGenericApplication $ toTypescript ga
--    ExprIdent id -> TS.Ident $ toTypescript id
--    ExprInferIdent id -> TS.Infer $ toTypescript id
--    ExprInferIdentConstraint id ex -> TS.EInferConstraint (toTypescript id) (toTypescript ex)
--    Hole -> error "Hole"
--    Intersection lhs rhs -> TS.Intersection (toTypescript lhs) (toTypescript rhs)
--    Keyof ex -> TS.Keyof $ toTypescript ex
--    Let binds ex -> error "Let"
--    Literal lit -> TS.ELiteral $ toTypescript lit
--    MappedType {..} -> TS.EMappedType $ TS.MappedType (toTypescript value) key (toTypescript source) (toTypescript <$> asExpr) isReadonly isOptional
--    PrimitiveType pt -> TS.EPrimitiveType $ toTypescript pt
--    Quote ex -> error "TODO"
--    Readonly ex -> TS.Readonly $ toTypescript ex
--    TemplateLiteral tss -> TS.ETemplate $ map toTypescript tss
--    Tuple lvs -> TS.Tuple $ map toTypescript lvs
--    Typeof ex -> TS.Typeof $ toTypescript ex
--    Union lhs rhs -> TS.Union (toTypescript lhs) (toTypescript rhs)
--    Unquote ex -> error "TODO"

-- instance Typescript ListValue TS.ListValue where
--  toTypescript (ListValue a b) = TS.ListValue a (toTypescript b)
--  toTypescript (ListRest a b) = TS.ListRest a (toTypescript b)

-- instance Typescript Literal TS.Literal where
--  toTypescript = \case
--    LBoolean b -> TS.LBoolean b
--    LNumberInteger n -> TS.LNumberInteger n
--    LNumberDouble n -> TS.LNumberDouble n
--    LString s -> TS.LString s
--    LFunction {} -> error "Cannot convert function to typescript"
--    LObject {} -> error "Cannot convert object to typescript"

-- instance Typescript TemplateString TS.TemplateString where
--  toTypescript (TemplateRaw s) = TS.TemplateRaw s
--  toTypescript (TemplateSubstitution e) = TS.TemplateSubstitution $ toTypescript e

-- instance Typescript GenericApplication TS.GenericApplication where
--  toTypescript (GenericApplication ident params) =
--    TS.GenericApplication
--      (toTypescript ident)
--      (map toTypescript params)

-- instance Typescript ConditionalType TS.ConditionalType where
--  toTypescript (ConditionalType lhs rhs then' else') =
--    TS.ConditionalType
--      { lhs = toTypescript lhs
--      , rhs = toTypescript rhs
--      , thenExpr = toTypescript then'
--      , elseExpr = toTypescript else'
--      }

-- instance Typescript Property TS.Property where
--  toTypescript (DataProperty isReadonly isOptional key value) =
--    TS.DataProperty
--      { value = toTypescript value
--      , ..
--      }
--  toTypescript (IndexSignature isReadonly key keySource value) =
--    TS.IndexSignature
--      { keySource = toTypescript keySource
--      , value = toTypescript value
--      , ..
--      }

-- instance Typescript Statement (Maybe TS.Statement) where
--  toTypescript = \case
--    ImportDeclaration _ _ -> undefined
--    InterfaceDefinition {..} ->
--      Just . TS.SInterface $
--        TS.Interface
--          { name
--          , params = map toTypescript params
--          , extends = fmap toTypescript extends
--          , props = map toTypescript props
--          }
--    TestDefinition _ _ -> undefined
--    ExportStatement exports -> Just . TS.SExport $ map getIdent exports
--    TypeDefinition {..} ->
--      Just . TS.SType $
--        TS.Type
--          { name = name
--          , params = map toTypescript params
--          , body = toTypescript body
--          }
-- Typescript instances }}}

--
-- Conditional Eval {{{
expandCaseStatement :: CaseStatement -> Expr
expandCaseStatement (CaseStatement lhs branches) = case branches of
  [Case rhs then', Case Hole else'] ->
    ct' lhs rhs then' else'
  [Case rhs then'] ->
    ct' lhs rhs then' never
  (Case rhs then' : tl) ->
    ct' lhs rhs then' (expandCaseStatement (CaseStatement lhs tl))
  [] -> never

expandConditional' :: ConditionalExpr -> Expr
expandConditional' = ExprConditionalType . expandConditional

expandConditional :: ConditionalExpr -> NTConditionalType
expandConditional (ConditionalExpr condition then' else') = case condition of
  ExtendsLeft a b ->
    ct a b then' else'
  ExtendsRight b a ->
    ct a b then' else'
  Not con ->
    expandConditional (cx con else' then')
  -- Convert a == b to [a] <: [b]
  Equals a b ->
    ct (t1 a) (t1 b) then' else'
  NotEquals a b ->
    expandConditional (cx (Not (Equals a b)) then' else')
  And a b ->
    let outer then'' = cx a then'' else'
        inner = cx b then' else'
     in expandConditional (outer (expandConditional' inner))
  Or a b ->
    let outer = cx a then'
        inner = cx b then' else'
     in expandConditional (outer (expandConditional' inner))

-- Conditional Eval }}}

-- Smart Constructors {{{
cx :: BoolExpr -> Expr -> Expr -> ConditionalExpr
cx = ConditionalExpr

ct' :: Expr -> Expr -> Expr -> Expr -> Expr
ct' lhs rhs then' else' = ExprConditionalType $ ConditionalType lhs rhs then' else'

ct :: Expr -> Expr -> Expr -> Expr -> NTConditionalType
ct = ConditionalType

-- | Single element tuple
t1 :: Expr -> Expr
t1 a = Tuple [ListValue Nothing a]

mkIdent :: String -> Expr
mkIdent = ExprIdent . Ident

mkString :: String -> Expr
mkString = Literal . LString

lv :: Expr -> NTListValue
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

-- Smart constructors }}}

-- PrettyTypescript {{{

-- PrettyTypescript }}}
