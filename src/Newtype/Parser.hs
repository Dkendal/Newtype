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
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

type Parser = Parsec Void Text

lineComment = L.skipLineComment "//"

blockComment = L.skipBlockComment "{-" "-}"

sc :: Parser ()
sc = L.space (void $ char ' ' <|> char '\t') lineComment blockComment

scn :: Parser ()
scn = L.space space1 lineComment blockComment

indentGuard :: Ordering -> Pos -> Parser ()
indentGuard ord pos =
  void $ L.indentGuard scn ord pos

indentBlock :: Parser (L.IndentOpt Parser a b) -> Parser a
indentBlock = L.indentBlock scn

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

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
  [ "as",
    "async",
    "await",
    "case",
    "do",
    "else",
    "for",
    "from",
    "from",
    "goto",
    "if",
    "import",
    "keyof",
    "of",
    "readonly",
    "require",
    "then",
    "typeof",
    "while",
    "yield"
  ]

builtins :: [Text]
builtins = ["keyof", "typeof"]

keyword :: Text -> Parser Text
keyword txt = lexeme (string txt <* notFollowedBy alphaNumChar)

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
    void scn
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
  L.nonIndented
    scn
    ( choice
        [ pExport,
          pImport,
          pTypeDefinition,
          pInterfaceDefintion
        ]
    )

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
  void $ indentGuard EQ pos1
  void $ keyword "type"
  name <- identifier
  paramNames <- many identifier
  void $ indentGuard GT pos1
  void equals
  void $ indentGuard GT pos1
  body <- pExpr
  let params = [TypeParam {name} | name <- paramNames]
  return TypeDefinition {..}

pInterfaceDefintion :: Parser Statement
pInterfaceDefintion = do
  void $ indentGuard EQ pos1
  void $ keyword "interface"
  name <- identifier
  void $ keyword "where"
  void newline
  void $ indentGuard GT pos1
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
      parens pSetOperator,
      pExtendsExpr,
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

pMappedType :: Parser Expr
pMappedType =
    braces p
    where
    p =
      do
        value <- pId
        pipe
        propertyKey <- pId
        leftArrow
        propertyKeySource <- pId
        let asExpr = Nothing
        let isReadonly = Nothing
        let isOptional = Nothing
        return (MappedType {..})

pCaseStatement :: Parser Expr
pCaseStatement =
  indentBlock p
  where
    p = do
      keyword "case"
      value <- pExpr
      keyword "of"
      return (L.IndentSome Nothing (return . CaseStatement value) pCase)

    pCase :: Parser Case
    pCase = do
      ref <- L.indentLevel
      lhs <- pExpr <?> "left hand side of case"
      keyword "->"
      indentGuard GT ref
      rhs <- pExpr <?> "right hand side of case"
      return (Case lhs rhs)

pExpr :: Parser Expr
pExpr = choice [pSetOperator, pTerm]

pSetOperator :: Parser Expr
pSetOperator = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
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

pExtendsExpr :: Parser Expr
pExtendsExpr = do
  void $ keyword "if"
  negate <-
    fmap isJust (optional (keyword "not"))
  lhs <- pExpr
  op <-
    choice
      [ ExtendsLeft <$ keyword "<:",
        ExtendsRight <$ keyword ":>",
        NotEquals <$ keyword "!=",
        Equals <$ keyword "=="
      ]
  rhs <- pExpr
  void $ keyword "then"
  ifBody <- pExpr
  elseBody <- do optional (keyword "else" >> pExpr)
  return
    ( ExtendsExpr
        { elseBody = fromMaybe never elseBody,
          ..
        }
    )
  where
    never = ID "never"

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
  isReadonly <-
    optional . choice $
      [ True <$ keyword "readonly",
        False <$ keyword "-readonly"
      ]

  key <- identifier
  isOptional <-
    optional . choice $
      [ True <$ qmark,
        False <$ keyword "-?"
      ]
  void colon
  value <- pExpr
  return (KeyValue {..})
