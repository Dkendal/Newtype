{-# LANGUAGE OverloadedStrings #-}

module Newtype.Syntax.Typescript where

import Newtype.Syntax.IntermediateRepresentation
import Newtype.Syntax.Internal
import Prettyprinter
import Text.Regex.TDFA ((=~))

-- Pretty Instances {{{
class PrettyTypescript a where
  prettyTypescript :: a -> Doc ann
  prettyTypescriptList :: [a] -> Doc ann
  prettyTypescriptList l = vcat $ prettyTypescript <$> l

instance PrettyTypescript PrimitiveType where
  prettyTypescript a = case a of
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
  prettyTypescript (Ident s) = pretty s

instance PrettyTypescript Program where
  prettyTypescript (Program statements) = prettyTypescriptList statements

instance PrettyTypescript Statement where
  prettyTypescript s = case s of
    ImportDeclaration {..} ->
      "import" <+> prettyTypescript importClause <+> "from" <+> dquotes (pretty fromClause) <> semi
    TypeDefinition {..} ->
      group head <> group (nest 2 body') <> semi
      where
        head = "type" <+> pretty name <> prettyTypescriptList params
        body' = line <> "=" <+> prettyTypescript body
    (ExportStatement s) ->
      "export" <+> (braces . hsep . punctuate comma . map prettyTypescript) s <> semi
    InterfaceDefinition {..} ->
      head <+> vsep [lbrace, body, rbrace]
      where
        head = group "interface" <+> pretty name <> prettyTypescriptList params
        body = indent 2 (align (vsep (map ((<> semi) . prettyTypescript) props)))

  prettyTypescriptList statements = vsep (punctuate line (map prettyTypescript statements))

instance PrettyTypescript ImportClause where
  prettyTypescript (ImportClauseDefault binding) = pretty binding
  prettyTypescript (ImportClauseNS binding) = "* as " <> pretty binding
  prettyTypescript (ImportClauseNamed namedBindings) = prettyTypescriptList namedBindings
  prettyTypescript ImportClauseDefaultAndNS {..} = pretty defaultBinding <+> pretty namespaceBinding
  prettyTypescript ImportClauseDefaultAndNamed {..} = pretty defaultBinding <+> prettyTypescriptList namedBindings

instance PrettyTypescript ImportSpecifier where
  prettyTypescriptList = braces . hsep . punctuate comma . map prettyTypescript
  prettyTypescript (ImportedBinding binding) = pretty binding
  prettyTypescript ImportedAlias {..} = pretty from <+> "as" <+> pretty to

instance PrettyTypescript IRMappedType where
  prettyTypescript = \case
    MappedType {asExpr = Just (Literal (LString as)), key, ..}
      | as == key ->
          prettyTypescript MappedType {asExpr = Nothing, ..}
    MappedType {..} ->
      braces (lhs <+> prettyTypescript value)
      where
        as = case asExpr of
          Nothing -> emptyDoc
          (Just expr) -> space <> "as" <+> prettyTypescript expr
        index = pretty key <+> "in" <+> prettyTypescript source <> as
        lhs = prettyReadonly isReadonly <> (brackets index <> prettyOptional isOptional <> colon)

instance PrettyTypescript Expr where
  prettyTypescript = \case
    Literal a -> prettyTypescript a
    PrimitiveType a -> prettyTypescript a
    ExprMappedType a -> prettyTypescript a
    -- If asExpr and key are equal
    Keyof a -> group ("keyof" <+> prettyTypescript a)
    Readonly a -> group ("readonly" <+> prettyTypescript a)
    Typeof a -> group ("typeof" <+> prettyTypescript a)
    Access a b -> prettyTypescript a <> "[" <> prettyTypescript b <> "]"
    DotAccess a b -> prettyTypescript a <> "." <> prettyTypescript b
    ExprGenericApplication a -> prettyTypescript a
    ExprIdent id -> prettyTypescript id
    ExprInferIdent (Ident id) -> group "infer" <+> pretty id
    ExprInferIdentConstraint (Ident id) constraint -> group "infer" <+> pretty id <+> "extends" <+> prettyTypescript constraint
    Array expr ->
      case expr of
        ExprInferIdent {} -> p
        ExprInferIdentConstraint {} -> p
        _ -> prettyTypescript expr <> "[]"
      where
        p = (parens . prettyTypescript $ expr) <> "[]"
    Tuple [] -> "[]"
    Tuple l -> (brackets . hsep) (punctuate comma (map prettyTypescript l))
    Intersection left right ->
      fmt left <> softline <> "&" <> softline <> fmt right
      where
        fmt (Union a b) = prettyOpList . prettyTypescript $ Union a b
        fmt a = prettyTypescript a
    Union left right ->
      fmt left <> softline <> "|" <+> fmt right
      where
        fmt (Intersection a b) = prettyOpList . prettyTypescript $ Intersection a b
        fmt a = prettyTypescript a
    ExprConditionalType a -> prettyTypescript a
    TemplateLiteral [] -> "``"
    TemplateLiteral a -> "`" <> cat (map prettyTypescript a) <> "`"

instance PrettyTypescript IRTypeParam where
  prettyTypescript (TypeParam name defaultValue constraint) =
    pretty name
      <> maybe emptyDoc (\d -> " extends " <> prettyTypescript d) constraint
      <> maybe emptyDoc (\d -> " = " <> prettyTypescript d) defaultValue
  prettyTypescriptList [] = emptyDoc
  prettyTypescriptList l = angles . hsep . punctuate comma . map prettyTypescript $ l

instance PrettyTypescript IRListValue where
  prettyTypescript (ListValue Nothing a) = prettyTypescript a
  prettyTypescript (ListRest Nothing a) = "..." <> prettyTypescript a
  prettyTypescript (ListValue (Just l) a) = pretty l <> ":" <+> prettyTypescript a
  prettyTypescript (ListRest (Just l) a) = pretty l <> ": ..." <> prettyTypescript a

instance PrettyTypescript IRLiteral where
  prettyTypescript LFunction {..} = doc
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
                <+> prettyTypescript returnType
            )
      prettyTypeParams l = "<" <> hsep (punctuate comma (map pretty l)) <> ">"
      prettyParams params = hsep $ punctuate comma $ map prettyParam params
      prettyParam (name, type') = pretty name <> ":" <+> prettyTypescript type'
      prettyRest Nothing = emptyDoc
      prettyRest (Just t) = "..." <> prettyParam t
  prettyTypescript (LNumberInteger value) = pretty value
  prettyTypescript (LNumberDouble value) = pretty value
  prettyTypescript (LBoolean True) = "true"
  prettyTypescript (LBoolean False) = "false"
  prettyTypescript (LString value) = dquotes . pretty $ value
  prettyTypescript (LObject []) = "{}"
  prettyTypescript (LObject props) =
    group
      ( encloseSep
          (flatAlt "{ " "{")
          (flatAlt " }" "}")
          ", "
          (map prettyTypescript props)
      )

instance PrettyTypescript IRTemplateString where
  prettyTypescript (TemplateRaw s) = pretty s
  prettyTypescript (TemplateSubstitution e) = "${" <> prettyTypescript e <> "}"

instance PrettyTypescript IRGenericApplication where
  prettyTypescript (GenericApplication ident []) = prettyTypescript ident
  prettyTypescript (GenericApplication typeName params) =
    prettyTypescript typeName <> (angles . hsep . punctuate comma . map prettyTypescript $ params)

instance PrettyTypescript IRConditionalType where
  prettyTypescript (ConditionalType lhs rhs then' else') =
    group . parens $ doc
    where
      doc = condition <> nest 2 body

      condition = prettyTypescript lhs <+> "extends" <+> prettyTypescript rhs

      body = thenDoc <> elseDoc

      thenDoc = line <> "?" <+> prettyTypescript then'

      elseDoc = line <> ":" <+> prettyTypescript else'

instance PrettyTypescript IRProperty where
  prettyTypescript IndexSignature {..} =
    doc
    where
      doc = lhs <+> prettyTypescript value
      lhs = group readonly <> brackets keyDoc <> colon
      keyDoc = pretty key <> colon <+> prettyTypescript keySource
      readonly = prettyReadonly isReadonly
  prettyTypescript DataProperty {..} =
    doc
    where
      doc = lhs <+> prettyTypescript value
      readonly = prettyReadonly isReadonly
      optional = prettyOptional isOptional
      lhs =
        group readonly
          <> pretty key
          <> optional
          <> ":"

-- Pretty Instances }}}

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
