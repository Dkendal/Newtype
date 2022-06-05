{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Newtype.Parser
  ( pExpr,
    pMappedType,
    pStatement,
    pProgram,
    pImport,
    pConditionalType,
    pBoolExpr,
  )
where

import Control.Applicative hiding (many, some)
import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text, pack, unpack)
import Data.Void
import Newtype.Syntax
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

symbol :: Text -> Parser Text
symbol = L.symbol scn

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
    "yield"
  ]

builtins :: [Text]
builtins = ["keyof", "typeof"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> (letterChar <|> underscore <|> dollar) <*> many (alphaNumChar <|> underscore <|> dollar)
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

bool :: Parser Bool
bool = choice [True <$ keyword "true", False <$ keyword "false"]

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
    [ try pTypeApplication <?> "type application",
      try (parens pTypeApplication) <?> "quoted type application",
      try pTuple <?> "tuple",
      try pMappedType,
      parens pTypeOp,
      -- pConditionalType,
      pNumberIntegerLiteral,
      pNumberDoubleLiteral,
      pCaseStatement <?> "case statement",
      pBooleanLiteral,
      pStringLiteral,
      pId,
      -- Not actually valid outside of the extends expression
      -- but make my life a lot easier
      pInferID,
      pObjectLiteral
    ]

pConditionalType :: Parser Expr
pConditionalType = do
  ConditionalType . expandConditional <$> pConditionalExpr

pConditionalExpr :: Parser ConditionalExpr
pConditionalExpr = do
  keyword "if"
  condition <- pBoolExpr
  keyword "then"
  thenExpr <- pExpr
  keyword "else"
  elseExpr <- pExpr
  return (ConditionalExpr {..})

{- Parse boolean logic expresions for conditional types
 -
 - Example of valid boolean expressions:
 -
 -     A <: B
 -     > ExtendsLeft A B
 -
 -     A :> B
 -     > ExtendsRight A B
 -
 -     A == B
 -     > Equals A B
 -
 -     A != B
 -     > NotEquals A B
 -
 -     A <: B and B <: C
 -     > (And (ExtendsLeft A B) (ExtendsLeft B C))
 -
 -     A <: B or B <: C
 -     > (Or (ExtendsLeft A B) (ExtendsLeft B C))
 -
 -     not A <: B
 -     > (Not (ExtendsLeft A B))
 -
 -     not (A <: B and (B <: C or C <: D))
 -     > (Not (And (ExtendsLeft A B) (Or (ExtendsLeft B C) (ExtendsLeft C D))))
 -}
pBoolExpr :: Parser BoolExpr
pBoolExpr =
  expr
  where
    not = do
      void $ keyword "not"
      Not <$> expr

    expr =
      choice
        [ do
            left <- term
            expr' left,
          not
        ]

    -- Remove the left recursion to expr
    expr' :: BoolExpr -> Parser BoolExpr
    expr' left =
      choice
        [ do
            void $ keyword "and"
            And left <$> expr,
          do
            void $ keyword "or"
            Or left <$> expr,
          return left
        ]
    term =
      choice
        [ try extendsLeft,
          try extendsRight,
          try equals,
          try notEquals,
          parens expr
        ]
    -- Parses the expression "A <: B"
    extendsLeft = do
      a <- pTerm
      void $ keyword "<:"
      ExtendsLeft a <$> pTerm

    extendsRight = do
      a <- pTerm
      void $ keyword ":>"
      ExtendsRight a <$> pTerm
    -- Parses expressions like "A == B"
    equals = do
      left <- pTerm
      void $ keyword "=="
      Equals left <$> pTerm

    notEquals = do
      left <- pTerm
      void $ keyword "!="
      NotEquals left <$> pTerm

-- -- Parses the expression "A <: B and C <: D"
-- pAnd = do
--   left <- pExport
--   void $ keyword "and"
--   And left <$> pExtendsLeft

-- pAnd' = do
--   left <- pBoolExpr
--   void $ keyword "and"
--   And left <$> pExtendsLeft

-- pOr = do
--   left <- pBoolExpr
--   void $ keyword "or"
--   Or left <$> pBoolExpr

pMappedType :: Parser Expr
pMappedType =
  do
    -- TODO: I don't know if there's a way to disambiguate this statement without parens
    value <- pId <|> parens pExpr
    keyword "for"
    propertyKey <- pId
    keyword "in"
    propertyKeySource <- pId
    withAs <- optional $ do
      keyword "as"
      isReadonly <- pReadonly
      asExpr <- Just <$> pId
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

pCaseStatement :: Parser Expr
pCaseStatement =
  do
    keyword "case"
    value <- pExpr
    keyword "of"
    cases <- some pCase
    return (CaseStatement value cases)
  where
    pCase :: Parser Case
    pCase = do
      lhs <- pExpr <?> "left hand side of case"
      keyword "->"
      rhs <- pExpr <?> "right hand side of case"
      return (Case lhs rhs)

pExpr :: Parser Expr
pExpr = choice [pTypeOp, pTerm]

pTypeOp :: Parser Expr
pTypeOp = makeExprParser pTerm typeOpTable

typeOpTable :: [[Operator Parser Expr]]
typeOpTable =
  [ [ Prefix $ do
        kw <- choice (map keyword builtins)
        let name = unpack kw
        return (Builtin name)
    ],
    [InfixL $ DotAccess <$ dot],
    [InfixL $ Access <$ bang],
    [InfixL $ Intersection <$ amp],
    [InfixL $ Union <$ pipe]
  ]

pInferID :: Parser Expr
pInferID = inferSym >> InferID <$> identifier <?> "identifier"

pNumberIntegerLiteral :: Parser Expr
pNumberIntegerLiteral = NumberIntegerLiteral <$> integer

pNumberDoubleLiteral :: Parser Expr
pNumberDoubleLiteral = NumberDoubleLiteral <$> float

pBooleanLiteral :: Parser Expr
pBooleanLiteral = BooleanLiteral <$> bool

pStringLiteral :: Parser Expr
pStringLiteral = StringLiteral <$> stringLiteral

pTuple :: Parser Expr
pTuple = Tuple <$> brackets (pExpr `sepBy` comma)

pId :: Parser Expr
pId = ID <$> identifier <?> "identifier"

pObjectLiteral :: Parser Expr
pObjectLiteral =
  ObjectLiteral <$> braces (pObjectLiteralProperty `sepBy` comma)

pTypeApplication :: Parser Expr
pTypeApplication =
  do
    name <- identifier
    params <- some . choice $ [pId, try pTerm]
    return (TypeApplication name params)

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
