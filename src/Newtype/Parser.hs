{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Newtype.Parser where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Void
import Newtype.Syntax
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

main :: IO ()
main = putStrLn "main"

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

parens = between (symbol "(") (symbol ")")

braces = between (symbol "{") (symbol "}")

angles = between (symbol "<") (symbol ">")

brackets = between (symbol "[") (symbol "]")

semicolon = symbol ";"

comma = symbol ","

colon = symbol ":"

qmark = symbol "?"

dot = symbol "."

equals = symbol "="

-- list of reserved words
reservedWords :: [String]
reservedWords =
  [ "from",
    "if",
    "else",
    "while",
    "for",
    "goto",
    "require",
    "import",
    "from",
    "as",
    "do",
    "yield",
    "await",
    "async",
    "readonly"
  ]

keyword :: Text -> Parser Text
keyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` reservedWords
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

bool :: Parser Bool
bool = choice [True <$ keyword "true", False <$ keyword "false"]

pProgram :: Parser Program
pProgram =
  do
    statements <- many pStatement <* eof
    return (Program statements)

pImportClause :: Parser ImportClause
pImportClause =
  ImportClauseNamed <$> parens (pSpecifier `sepBy` comma)
  where
    pSpecifier = do
      id <- identifier
      alias <- optional $ do
        keyword "as"
        identifier
      case alias of
        Just importedBinding -> return (ImportedAlias id importedBinding)
        Nothing -> return (ImportedBinding id)

    pBinding = ImportedBinding <$> identifier
    pAlias = do
      from <- identifier
      keyword "as"
      to <- identifier
      return ImportedAlias {..}

pStatement :: Parser Statement
pStatement =
  choice
    [ pExport,
      pImport,
      pTypeDefinition
    ]
  where
    pExport = ExportStatement <$ string "export"
    pImport = do
      keyword "import"
      fromClause <- lexeme stringLiteral <?> "from clause"
      importClause <- pImportClause
      return ImportDeclaration {..}
    pTypeDefinition = do
      keyword "type"
      name <- identifier
      equals
      body <- pExpression
      return TypeDefinition {..}
      where
        params = Nothing

pExpression :: Parser Expression
pExpression =
  choice
    [ NumberIntegerLiteral <$> integer,
      NumberDoubleLiteral <$> float,
      BooleanLiteral <$> bool,
      StringLiteral <$> stringLiteral,
      pTypeApplication,
      Identifier <$> identifier,
      pObjectLiteral,
      -- Allow expressions to have an arbitrary number of parens surrounding them
      parens pExpression
    ]
  where
    pObjectLiteral =
      ObjectLiteral <$> braces (pObjectLiteralProperty `sepBy` comma)

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
      colon
      value <- pExpression
      return (KeyValue {..})

    pTypeApplication = do
      typeName <- identifier
      -- Give Identifier a higher precedence when it's nested in an existing
      -- expression
      params <- many . choice $ [pIdentifier, pExpression]
      return (TypeApplication typeName params)

pIdentifier :: Parser Expression
pIdentifier = Identifier <$> identifier
