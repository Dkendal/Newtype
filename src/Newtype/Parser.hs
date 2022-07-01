{-# LANGUAGE OverloadedStrings #-}

module Newtype.Parser where

import Control.Applicative hiding (many, some)
import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Functor
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text, pack, unpack)
import Data.Void
import Debug.Trace
import Newtype.Syntax
import Newtype.Syntax.Conditionals
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

type Parser = Parsec Void Text

type FormalParamMap = Map.Map String Expr

-- list of reserved words
reservedWords :: [String]
reservedWords =
  [ "and",
    "any",
    "as",
    "async",
    "await",
    "boolean",
    "case",
    "class",
    "const",
    "delete",
    "do",
    "defaults",
    "else",
    "enum",
    "extends",
    "false",
    "for",
    "from",
    "function",
    "get",
    "goto",
    "if",
    "implements",
    "import",
    "interface",
    "keyof",
    "let",
    "member",
    "never",
    "new",
    "not",
    "null",
    "number",
    "object",
    "of",
    "or",
    "readonly",
    "require",
    "return",
    "set",
    "string",
    "symbol",
    "then",
    "true",
    "type",
    "typeof",
    "undefined",
    "var",
    "void",
    "while",
    "where",
    "when",
    "with",
    "yield"
  ]

-- list of reserved prefix operators
builtins :: [Text]
builtins = ["keyof", "typeof"]

-------------------------------------------------------------------------------
-- Lexing support                                                            --
-------------------------------------------------------------------------------

lineComment = L.skipLineComment "--"

blockComment = L.skipBlockComment "{-" "-}"

sc :: Parser ()
sc = L.space (void $ char ' ' <|> char '\t') lineComment blockComment

scn :: Parser ()
scn = L.space space1 lineComment blockComment

nonIndented :: Parser a -> Parser a
nonIndented = L.nonIndented scn

indentGuard :: Ordering -> Pos -> Parser Pos
indentGuard = L.indentGuard scn

indentBlock :: Parser (L.IndentOpt Parser a b) -> Parser a
indentBlock = L.indentBlock scn

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

lexeme' :: Parser a -> Parser a
lexeme' = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol scn

symbol' :: Text -> Parser Text
symbol' = L.symbol sc

keyword :: Text -> Parser Text
keyword txt = lexeme (string txt <* notFollowedBy identifierTail)

stringLiteral :: Parser String
stringLiteral = char '\"' *> L.charLiteral `manyTill` char '\"'

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

-- | Left associative binary operator.
binary name f = InfixL (f <$ symbol name)

prefix name f = Prefix (f <$ symbol name)

postfix name f = Postfix (f <$ symbol name)

-- | Parse an operator, using backtracking to ensure that the operator is not a
-- prefix of a valid identifier.
op n = (lexeme . try) (string n <* notFollowedBy punctuationChar)

-------------------------------------------------------------------------------
-- Symbols                                                                   --
-------------------------------------------------------------------------------

lparen :: Parser Text
lparen = symbol "("
{-# INLINE lparen #-}

rparen :: Parser Text
rparen = symbol ")"
{-# INLINE rparen #-}

lbrace :: Parser Text
lbrace = symbol "{"
{-# INLINE lbrace #-}

rbrace :: Parser Text
rbrace = symbol "}"
{-# INLINE rbrace #-}

langle :: Parser Text
langle = symbol "<"
{-# INLINE langle #-}

rangle :: Parser Text
rangle = symbol ">"
{-# INLINE rangle #-}

lbracket :: Parser Text
lbracket = symbol "["
{-# INLINE lbracket #-}

rbracket :: Parser Text
rbracket = symbol "]"
{-# INLINE rbracket #-}

semicolon :: Parser Text
semicolon = symbol ";"
{-# INLINE semicolon #-}

larrow :: Parser Text
larrow = symbol "<-"
{-# INLINE larrow #-}

rarrow :: Parser Text
rarrow = symbol "->"
{-# INLINE rarrow #-}

pipe :: Parser Text
pipe = symbol "|"
{-# INLINE pipe #-}

amp :: Parser Text
amp = symbol "&"
{-# INLINE amp #-}

bang :: Parser Text
bang = symbol "!"
{-# INLINE bang #-}

comma :: Parser Text
comma = symbol ","
{-# INLINE comma #-}

colon :: Parser Text
colon = symbol ":"
{-# INLINE colon #-}

qmark :: Parser Text
qmark = symbol "?"
{-# INLINE qmark #-}

pound :: Parser Text
pound = symbol "#"
{-# INLINE pound #-}

caret :: Parser Text
caret = symbol "^"
{-# INLINE caret #-}

dot :: Parser Text
dot = symbol "."
{-# INLINE dot #-}

equals :: Parser Text
equals = symbol "="
{-# INLINE equals #-}

inferSym :: Parser Text
inferSym = qmark
{-# INLINE inferSym #-}

underscore :: Parser (Token Text)
underscore = char '_'
{-# INLINE underscore #-}

dollar :: Parser (Token Text)
dollar = char '$'
{-# INLINE dollar #-}

-------------------------------------------------------------------------------
-- Pairs                                                                     --
-------------------------------------------------------------------------------

parens :: Parser a -> Parser a
parens = between lparen rparen
{-# INLINE parens #-}

braces :: Parser a -> Parser a
braces = between lbrace rbrace
{-# INLINE braces #-}

angles :: Parser Text -> Parser Text
angles = between langle rangle
{-# INLINE angles #-}

brackets :: Parser a -> Parser a
brackets = between lbracket rbracket
{-# INLINE brackets #-}

-- | First character of a valid identifier.
identifierHead :: Parser Char
identifierHead = letterChar <|> underscore <|> dollar

-- | Rest of a valid identifier.
identifierTail :: Parser Char
identifierTail = alphaNumChar <|> underscore <|> dollar

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> identifierHead <*> many identifierTail
    check x =
      if x `elem` reservedWords
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

-------------------------------------------------------------------------------
-- Production rules                                                          --
-------------------------------------------------------------------------------

pProgram :: Parser Program
pProgram =
  do
    scn
    statements <- pStatementList
    void eof
    return (Program statements)
  where
    pStatementList = sepEndBy pStatement scn

pImportClause :: Parser ImportClause
pImportClause =
  ImportClauseNamed <$> parens (pSpecifier `sepBy` comma)
  where
    pSpecifier = do
      binding <- identifier
      alias <- optional $ do
        void $ keyword "as"
        identifier
      case alias of
        Just importedBinding -> return (ImportedAlias binding importedBinding)
        Nothing -> return (ImportedBinding binding)

pStatement :: Parser Statement
pStatement =
  choice
    [ -- pExport,
      pImport,
      pTypeDefinition,
      pInterfaceDefintion
    ]

-- pExport :: Parser Statement
-- pExport = ExportStatement <$ string "export"

pImport :: Parser Statement
pImport = do
  void $ keyword "import"
  fromClause <- lexeme stringLiteral <?> "package name"
  importClause <- pImportClause
  return ImportDeclaration {..}

pTypeDefinition :: Parser Statement
pTypeDefinition = do
  void $ keyword "type"
  name <- identifier
  params <- pFormalParameters
  equals
  body <- pExpr
  return TypeDefinition {..}

pInterfaceDefintion :: Parser Statement
pInterfaceDefintion = do
  keyword "interface"
  name <- identifier <?> "interface name"
  params <- pFormalParameters
  extends <- optional extendsClause <?> "interface extends clause"
  props <- whereClause
  return InterfaceDefinition {..}
  where
    whereClause = do
      keyword "where"
      some pMember <?> "interface properties"

    -- Example input:
    --  extends Foo
    --  extends Foo A B C
    --  extends (Foo A B C)
    extendsClause :: Parser Extensible
    extendsClause = keyword "extends" *> generic <|> id
    generic = parens generic <|> ExtendGeneric <$> pGenericApplication
    id = ExtendIdent <$> pIdent

    -- Example input: "member x = 1"
    pMember = do
      isReadonly <- pReadonly
      key <- identifier
      isOptional <- pOptional
      equals
      value <- pExpr
      let accessor = Nothing
      return $ DataProperty {..}

pFormalParameters :: Parser [TypeParam]
pFormalParameters = do
  makeParams <$> many identifier <*> pWhenClause <*> pDefaultsClause

pDefaultsClause :: Parser FormalParamMap
pDefaultsClause = do
  t <- optional $ do
    keyword "defaults"
    some $ do
      Ident name <- pIdent
      equals
      value <- pExpr
      return (name, value)
  return $ maybe Map.empty Map.fromList t

pWhenClause :: Parser FormalParamMap
pWhenClause = do
  t <- optional $ do
    keyword "when"
    some $ do
      Ident lhs <- pIdent
      symbol "<:"
      rhs <- pExpr
      return (lhs, rhs)
  return $ maybe Map.empty Map.fromList t

-- Same as expression, but with recursive terms removed
pTerm :: Parser Expr
pTerm =
  choice
    [ try (PrimitiveType <$> pPrimitive) <?> "type primitive",
      try (ExprGenericApplication <$> pGenericApplication) <?> "generic type application",
      try pTuple <?> "tuple",
      try pMappedType,
      pExprConditionalType <?> "conditional type",
      pNumberIntegerLiteral <?> "number",
      pNumberDoubleLiteral <?> "number",
      (expandCaseStatement <$> pCaseStatement) <?> "case statement",
      pBooleanLiteral <?> "boolean literal",
      pStringLiteral <?> "string literal",
      pIdent' <?> "identifier",
      -- Not actually valid outside of the extends expression
      -- but make my life a lot easier
      hidden pInferIdent,
      pObjectLiteral <?> "object literal",
      hidden . parens $ pExpr
    ]

pPrimitive :: Parser PrimitiveType
pPrimitive =
  choice
    [ keyword "never" $> PrimitiveNever,
      keyword "any" $> PrimitiveAny,
      keyword "unknown" $> PrimitiveUnknown,
      keyword "number" $> PrimitiveNumber,
      keyword "bigint" $> PrimitiveBigInt,
      keyword "string" $> PrimitiveString,
      keyword "boolean" $> PrimitiveBoolean,
      keyword "null" $> PrimitiveNull,
      keyword "undefined" $> PrimitiveUndefined,
      keyword "void" $> PrimitiveVoid,
      keyword "object" $> PrimitiveObject,
      keyword "symbol" $> PrimitiveSymbol
    ]

pHole :: Parser Expr
pHole = Hole <$ symbol "_" <* notFollowedBy identifierTail

pExprConditionalType :: Parser Expr
pExprConditionalType =
  ExprConditionalType . expandConditional <$> pConditionalExpr

pConditionalExpr :: Parser ConditionalExpr
pConditionalExpr = do
  keyword "if"
  condition <- pBoolExpr
  keyword "then"
  thenExpr <- pExpr
  elseExpr <- (keyword "else" >> pExpr) <|> return never
  return (ConditionalExpr {..})

pBoolExpr :: Parser BoolExpr
pBoolExpr =
  expr
  where
    expr =
      ( makeExprParser
          term
          [ [prefix "not" Not],
            [ binary "and" And,
              binary "or" Or
            ]
          ]
          <?> "boolean expression"
      )
        <|> term

    term = (try comparison <?> "type comparison") <|> parens expr

    comparison =
      do
        lhs <- pExpr
        choice
          [ ExtendsLeft lhs <$ keyword "<:" <*> pExpr,
            ExtendsRight lhs <$ keyword ":>" <*> pExpr,
            Equals lhs <$ keyword "==" <*> pExpr,
            NotEquals lhs <$ keyword "!=" <*> pExpr
          ]

-- Example input:
--    { V : K <- keyof T }
pMappedType :: Parser Expr
pMappedType =
  braces p
  where
    p = do
      value <- pExpr <?> "mapped type property value"
      symbol ":"
      isReadonly <- pReadonly
      key <- pExpr <?> "mapped type property key"
      isOptional <- pOptional
      asExpr <- optional $ keyword "as" *> pExpr
      larrow
      source <- pExpr <?> "mapped type source"
      return MappedType {..}

pMappedType' :: Parser Expr
pMappedType' =
  do
    value <- pIdent' <|> parens pExpr
    keyword "for"
    key <- pIdent'
    keyword "in"
    source <- pIdent'
    withAs <- optional $ do
      keyword "as"
      isReadonly <- pReadonly
      asExpr <- Just <$> pIdent'
      isOptional <- pOptional
      return (MappedType {..})
    return $
      fromMaybe
        MappedType
          { asExpr = Nothing,
            isReadonly = Nothing,
            isOptional = Nothing,
            ..
          }
        withAs

pCaseStatement :: Parser CaseStatement
pCaseStatement =
  do
    pos <- L.indentLevel
    keyword "case"
    value <- pExpr
    keyword "of"
    branches <- some $ do
      indentGuard GT pos <|> fail "case branch must be indented"
      notFollowedBy pHole <|> fail "default branch must be last"
      lhs <- pExpr
      keyword "->"
      Case lhs <$> pExpr
    branches <-
      do
        indentGuard GT pos <|> fail "case branch must be indented"
        lhs <- pHole
        keyword "->"
        case' <- Case lhs <$> pExpr
        return (branches ++ [case'])
        <|> return branches
    return (CaseStatement value branches)

pExpr :: Parser Expr
pExpr = choice [pTypeOp, pTerm]

-- | Parse an expression with a type operator. A type operator may be a builtin
-- prefix operator like `typeof` or `keyof`, or a prefix operator: like `!` for
-- access, or type union or intersection, `|` or `&` respectively.
pTypeOp :: Parser Expr
pTypeOp =
  makeExprParser
    pTerm
    [ [ InfixL $ DotAccess <$ (dot <?> "dot access"),
        InfixL $ Access <$ (lexeme . try) (string "!" <* notFollowedBy "=")
      ],
      [ Prefix $ do
          kw <- choice (map keyword builtins) <?> "builtin type function"
          let name = unpack kw
          return (Builtin name)
      ],
      [ InfixL $ Intersection <$ (amp <?> "type intersection"),
        InfixL $ Union <$ (pipe <?> "type union")
      ]
    ]

pInferIdent :: Parser Expr
pInferIdent = inferSym >> ExprInferIdent <$> pIdent <?> "infered identifier"

pNumberIntegerLiteral :: Parser Expr
pNumberIntegerLiteral = NumberIntegerLiteral <$> integer

pNumberDoubleLiteral :: Parser Expr
pNumberDoubleLiteral = NumberDoubleLiteral <$> float

pBooleanLiteral :: Parser Expr
pBooleanLiteral = BooleanLiteral <$> choice [True <$ keyword "true", False <$ keyword "false"]

pStringLiteral :: Parser Expr
pStringLiteral = StringLiteral <$> stringLiteral

pTuple :: Parser Expr
pTuple = Tuple <$> brackets (pExpr `sepBy` comma)

pIdent :: Parser Ident
pIdent = Ident <$> identifier <?> "identifier"

pIdent' :: Parser Expr
pIdent' = ExprIdent <$> pIdent <?> "identifier"

pObjectLiteral :: Parser Expr
pObjectLiteral =
  ObjectLiteral <$> braces (pProperty `sepBy` comma)

pGenericApplication :: Parser GenericApplication
pGenericApplication = do
  pos <- L.indentLevel
  id <- pIdent
  params <-
    some . choice $
      [ indentGuard GT pos *> pIdent',
        indentGuard GT pos *> try pTerm
      ]
  return $ GenericApplication id params

pProperty :: Parser Property
pProperty = do
  isReadonly <- pReadonly
  key <- identifier
  isOptional <- pOptional
  void colon
  value <- pExpr
  let accessor = Nothing
  return (DataProperty {..})

-- | readonly keyword of an object literal property
--
-- Example source:
-- * `readonly foo: bar`
-- * `-readonly foo: bar`
pReadonly =
  optional . choice $
    [ True <$ keyword "readonly",
      False <$ keyword "-readonly"
    ]

-- | optional keyword of an object literal property
--
-- Example source:
-- * `foo?: bar`
-- * `foo-?: bar`
pOptional =
  p <?> "optional postfix key modifier"
  where
    p =
      optional . choice $
        [ True <$ qmark,
          False <$ keyword "-?"
        ]

makeParams :: [String] -> FormalParamMap -> FormalParamMap -> [TypeParam]
makeParams paramNames constraints defaults =
  [ TypeParam
      { name = paramName,
        defaultValue = Map.lookup paramName defaults,
        constraint = Map.lookup paramName constraints
      }
    | let defaultValue = Nothing,
      paramName <- paramNames
  ]
