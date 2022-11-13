{-# LANGUAGE OverloadedStrings #-}

module Newtype.Syntax.Typescript (fromIR) where

import Newtype.Syntax.IntermediateRepresentation
import Newtype.Syntax.Internal
import Prettyprinter
import Text.Regex.TDFA ((=~))

fromIR :: Program -> Doc ann
fromIR = pp

class PrettyTypescript a where
  pp :: a -> Doc ann
  ppl :: [a] -> Doc ann
  ppl l = vcat $ pp <$> l

instance PrettyTypescript PrimitiveType where
  pp = \case
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

instance PrettyTypescript Ident where
  pp (Ident s) = pretty s

instance PrettyTypescript Program where
  pp (Program statements) = ppl statements

instance PrettyTypescript Statement where
  pp s = case s of
    ImportDeclaration {..} ->
      "import" <+> pp importClause <+> "from" <+> dquotes (pretty fromClause) <> semi
    TypeDefinition {..} ->
      group head <> group (nest 2 body') <> semi
      where
        head = "type" <+> pretty name <> ppl params
        body' = line <> "=" <+> pp body
    ExportStatement s ->
      "export" <+> (braces . hsep . punctuate comma . map pp) s <> semi
    InterfaceDefinition {..} ->
      head <+> cblock body
      where
        head = group "interface" <+> pretty name <> ppl params
        body = align . vsep . map statement $ props

  ppl = vsep . punctuate line . map pp

instance PrettyTypescript ImportClause where
  pp (ImportClauseDefault binding) = pretty binding
  pp (ImportClauseNS binding) = "* as " <> pretty binding
  pp (ImportClauseNamed namedBindings) = ppl namedBindings
  pp ImportClauseDefaultAndNS {..} = pretty defaultBinding <+> pretty namespaceBinding
  pp ImportClauseDefaultAndNamed {..} = pretty defaultBinding <+> ppl namedBindings

instance PrettyTypescript ImportSpecifier where
  ppl = braces . hsep . punctuate comma . map pp
  pp (ImportedBinding binding) = pretty binding
  pp ImportedAlias {..} = pretty from <+> "as" <+> pretty to

instance PrettyTypescript IRMappedType where
  pp = \case
    MappedType {asExpr = Just (Literal (LString as)), key, ..}
      | as == key ->
          pp MappedType {asExpr = Nothing, ..}
    MappedType {..} ->
      braces (lhs <+> pp value)
      where
        as = case asExpr of
          Nothing -> emptyDoc
          (Just expr) -> space <> "as" <+> pp expr
        index = pretty key <+> "in" <+> pp source <> as
        lhs = prettyReadonly isReadonly <> (brackets index <> prettyOptional isOptional <> colon)

instance PrettyTypescript Expr where
  pp = \case
    Literal a -> pp a
    PrimitiveType a -> pp a
    ExprMappedType a -> pp a
    -- If asExpr and key are equal
    Keyof a -> group ("keyof" <+> pp a)
    Readonly a -> group ("readonly" <+> pp a)
    Typeof a -> group ("typeof" <+> pp a)
    Access a b -> pp a <> "[" <> pp b <> "]"
    DotAccess a b -> pp a <> "." <> pp b
    ExprGenericApplication a -> pp a
    ExprIdent id -> pp id
    ExprInferIdent (Ident id) -> group "infer" <+> pretty id
    ExprInferIdentConstraint (Ident id) constraint -> group "infer" <+> pretty id <+> "extends" <+> pp constraint
    Array expr ->
      case expr of
        ExprInferIdent {} -> p
        ExprInferIdentConstraint {} -> p
        _ -> pp expr <> "[]"
      where
        p = (parens . pp $ expr) <> "[]"
    Tuple [] -> "[]"
    Tuple l -> (brackets . hsep) (punctuate comma (map pp l))
    Intersection left right ->
      fmt left <> softline <> "&" <> softline <> fmt right
      where
        fmt (Union a b) = prettyOpList . pp $ Union a b
        fmt a = pp a
    Union left right ->
      fmt left <> softline <> "|" <+> fmt right
      where
        fmt (Intersection a b) = prettyOpList . pp $ Intersection a b
        fmt a = pp a
    ExprConditionalType a -> pp a
    TemplateLiteral [] -> "``"
    TemplateLiteral a -> "`" <> cat (map pp a) <> "`"

instance PrettyTypescript IRTypeParam where
  pp (TypeParam name defaultValue constraint) =
    pretty name
      <> maybe emptyDoc (\d -> " extends " <> pp d) constraint
      <> maybe emptyDoc (\d -> " = " <> pp d) defaultValue
  ppl [] = emptyDoc
  ppl l = angles . hsep . punctuate comma . map pp $ l

instance PrettyTypescript IRListValue where
  pp (ListValue Nothing a) = pp a
  pp (ListRest Nothing a) = "..." <> pp a
  pp (ListValue (Just l) a) = pretty l <> ":" <+> pp a
  pp (ListRest (Just l) a) = pretty l <> ": ..." <> pp a

instance PrettyTypescript IRLiteral where
  pp LFunction {..} = doc
    where
      doc :: Doc ann
      doc =
        maybe
          emptyDoc
          prettyTypeParams
          typeParams
          <> parens
            ( prettyParams params
                <+> prettyRest rest
                <+> "=>"
                <+> pp returnType
            )
      prettyTypeParams l = "<" <> hsep (punctuate comma (map pretty l)) <> ">"
      prettyParams params = hsep $ punctuate comma $ map prettyParam params
      prettyParam (name, type') = pretty name <> ":" <+> pp type'
      prettyRest Nothing = emptyDoc
      prettyRest (Just t) = "..." <> prettyParam t
  pp (LNumberInteger value) = pretty value
  pp (LNumberDouble value) = pretty value
  pp (LBoolean True) = "true"
  pp (LBoolean False) = "false"
  pp (LString value) = dquotes . pretty $ value
  pp (LObject []) = "{}"
  pp (LObject props) =
    group
      ( encloseSep
          (flatAlt "{ " "{")
          (flatAlt " }" "}")
          ", "
          (map pp props)
      )

instance PrettyTypescript IRTemplateString where
  pp (TemplateRaw s) = pretty s
  pp (TemplateSubstitution e) = "${" <> pp e <> "}"

instance PrettyTypescript IRGenericApplication where
  pp (GenericApplication ident []) = pp ident
  pp (GenericApplication typeName params) =
    pp typeName <> (angles . hsep . punctuate comma . map pp $ params)

instance PrettyTypescript IRConditionalType where
  pp (ConditionalType lhs rhs then' else') =
    group . parens $ doc
    where
      doc = condition <> nest 2 body

      condition = pp lhs <+> "extends" <+> pp rhs

      body = thenDoc <> elseDoc

      thenDoc = line <> "?" <+> pp then'

      elseDoc = line <> ":" <+> pp else'

instance PrettyTypescript IRProperty where
  pp IndexSignature {..} =
    doc
    where
      doc = lhs <+> pp value
      lhs = group readonly <> brackets keyDoc <> colon
      keyDoc = pretty key <> colon <+> pp keySource
      readonly = prettyReadonly isReadonly
  pp DataProperty {..} =
    doc
    where
      doc = lhs <+> pp value
      readonly = prettyReadonly isReadonly
      optional = prettyOptional isOptional
      lhs =
        group readonly
          <> pretty key
          <> optional
          <> ":"

-- Helpers {{{
vbraces :: Doc ann -> Doc ann
vbraces a = vsep [lbrace, a, rbrace]

-- | C style block
cblock :: Doc ann -> Doc ann
cblock = vbraces . indent 2 . align

statement :: (PrettyTypescript a) => a -> Doc ann
statement = (<> semi) . pp

prettyReadonly :: Maybe Bool -> Doc ann
prettyReadonly Nothing = emptyDoc
prettyReadonly (Just False) = "-readonly" <> space
prettyReadonly (Just True) = "readonly" <> space

prettyOptional :: Maybe Bool -> Doc ann
prettyOptional Nothing = emptyDoc
prettyOptional (Just False) = "-?"
prettyOptional (Just True) = "?"

prettyOpList :: Doc ann -> Doc ann
prettyOpList a =
  group $ align $ enclose (flatAlt "( " "(") (flatAlt " )" ")") a

-- Test if a string is a valid Javascript identifier name
isIdentifierName :: String -> Bool
isIdentifierName str = str =~ ("^[a-zA-Z_$][a-zA-Z0-9_$]*$" :: String)

-- Helpers }}}
