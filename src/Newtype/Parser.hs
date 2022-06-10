{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Newtype.Parser where

import Control.Applicative hiding (many, some)
import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text, pack, unpack)
import Data.Void
import Newtype.Syntax
import Newtype.Syntax.Conditionals
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

type Parser = Parsec Void Text

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
keyword txt = lexeme (string txt <* notFollowedBy alphaNumChar)

binary name f = InfixL (f <$ symbol name)

prefix name f = Prefix (f <$ symbol name)

postfix name f = Postfix (f <$ symbol name)

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

lparen :: Parser Text
lparen = symbol "("

rparen :: Parser Text
rparen = symbol ")"

parens :: Parser a -> Parser a
parens = between lparen rparen

lbrace :: Parser Text
lbrace = symbol "{"

rbrace :: Parser Text
rbrace = symbol "}"

braces :: Parser a -> Parser a
braces = between lbrace rbrace

langle :: Parser Text
langle = symbol "<"

rangle :: Parser Text
rangle = symbol ">"

angles :: Parser Text -> Parser Text
angles = between langle rangle

lbracket :: Parser Text
lbracket = symbol "["

rbracket :: Parser Text
rbracket = symbol "]"

brackets :: Parser a -> Parser a
brackets = between lbracket rbracket

semicolon :: Parser Text
semicolon = symbol ";"

arrow :: Parser Text
arrow = symbol "->"

leftArrow :: Parser Text
leftArrow = symbol "<-"

rightArrow :: Parser Text
rightArrow = symbol "->"

pipe :: Parser Text
pipe = symbol "|"

amp :: Parser Text
amp = symbol "&"

bang :: Parser Text
bang = symbol "!"

comma :: Parser Text
comma = symbol ","

colon :: Parser Text
colon = symbol ":"

qmark :: Parser Text
qmark = symbol "?"

pound :: Parser Text
pound = symbol "#"

caret :: Parser Text
caret = symbol "^"

dot :: Parser Text
dot = symbol "."

equals :: Parser Text
equals = symbol "="

inferSym :: Parser Text
inferSym = qmark

op n = (lexeme . try) (string n <* notFollowedBy punctuationChar)

-- list of reserved words
reservedWords :: [String]
reservedWords =
  [ "and",
    "as",
    "async",
    "await",
    "case",
    "do",
    "else",
    "for",
    "from",
    "goto",
    "if",
    "import",
    "keyof",
    "not",
    "of",
    "or",
    "readonly",
    "require",
    "then",
    "typeof",
    "while",
    "yield",
    "type",
    "interface",
    "class",
    "enum",
    "extends",
    "implements",
    "let",
    "const",
    "var",
    "function",
    "return"
  ]

builtins :: [Text]
builtins = ["keyof", "typeof"]

identifierHead :: Parser Char
identifierHead = letterChar <|> underscore <|> dollar

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

underscore :: Parser (Token Text)
underscore = char '_'
{-# INLINE underscore #-}

dollar :: Parser (Token Text)
dollar = char '$'
{-# INLINE dollar #-}

pProgram :: Parser Program
pProgram =
  do
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
    [ pExport,
      pImport,
      pTypeDefinition,
      pInterfaceDefintion
    ]

pExport :: Parser Statement
pExport = ExportStatement <$ string "export"

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
  paramNames <- many identifier
  void equals
  body <- pExpr
  let params = [TypeParam {name} | name <- paramNames]
  return TypeDefinition {..}

pInterfaceDefintion :: Parser Statement
pInterfaceDefintion = do
  void $ keyword "interface"
  name <- identifier
  void $ keyword "where"
  void newline
  props <- many $ pObjectLiteralProperty <* space
  return InterfaceDefinition {..}
  where
    extends = []
    params = []

-- Same as expression, but with recursive terms removed
pTerm :: Parser Expr
pTerm =
  choice
    [ try pGenericApplication <?> "generic type application",
      try pTuple <?> "tuple",
      try pMappedType <?> "mapped type",
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

pHole :: Parser Expr
pHole = Hole <$ symbol "_" <* notFollowedBy identifierTail

pExprConditionalType :: Parser Expr
pExprConditionalType = do
  -- NOTE: I'm not sure about doing online expansion here
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

pMappedType :: Parser Expr
pMappedType =
  do
    -- TODO: I don't know if there's a way to disambiguate this statement without parens
    value <- pIdent' <|> parens pExpr
    keyword "for"
    propertyKey <- pIdent'
    keyword "in"
    propertyKeySource <- pIdent'
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

pTypeOp :: Parser Expr
pTypeOp =
  makeExprParser
    pTerm
    [ [ Prefix $ do
          kw <- choice (map keyword builtins) <?> "builtin type function"
          let name = unpack kw
          return (Builtin name)
      ],
      [InfixL $ DotAccess <$ (dot <?> "dot access")],
      [InfixL $ Access <$ (lexeme . try) (string "!" <* notFollowedBy "=")],
      [InfixL $ Intersection <$ (amp <?> "type intersection")],
      [InfixL $ Union <$ (pipe <?> "type union")]
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
  ObjectLiteral <$> braces (pObjectLiteralProperty `sepBy` comma)

pGenericApplication :: Parser Expr
pGenericApplication = do
  pos <- L.indentLevel
  id <- pIdent
  params <-
    some . choice $
      [ indentGuard GT pos *> pIdent',
        indentGuard GT pos *> try pTerm
      ]
  return (GenericApplication id params)

pObjectLiteralProperty :: Parser ObjectLiteralProperty
pObjectLiteralProperty = do
  isReadonly <- pReadonly
  key <- identifier
  isOptional <- pOptional
  void colon
  value <- pExpr
  return (KeyValue {..})

pReadonly =
  optional . choice $
    [ True <$ keyword "readonly",
      False <$ keyword "-readonly"
    ]

pOptional =
  optional . choice $
    [ True <$ qmark,
      False <$ keyword "-?"
    ]
