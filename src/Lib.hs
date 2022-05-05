{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( main,
  )
where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
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

dot = symbol "."

newtype Program = Program {programStatements :: [Statement]}
  deriving (Eq, Show)

data Statement
  = ImportDeclaration
      { importClause :: ImportClause,
        fromClause :: String
      }
  | ExportStatement
  deriving (Eq, Show)

data ImportClause
  = ImportClauseDefault String
  | ImportClauseNS String
  | ImportClauseNamed NamedImports
  | ImportClauseDefaultAndNS
      { defaultBinding :: String,
        namespace :: String
      }
  | ImportClauseDefaultAndNamed
      { defaultBinding :: String,
        named :: NamedImports
      }
  deriving (Eq, Show)

data NamedImports = NamedImports [Either Alias String]
  deriving (Eq, Show)

data Alias = Alias {aliasFrom :: String, aliasTo :: String}
  deriving (Eq, Show)

data Declaration
  = Var String
  | Const String
  | Let String
  | Function String
  deriving (Eq, Show)

data Definition
  = Type String
  | Interface String
  deriving (Eq, Show)

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
    "async"
  ]

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

pIdentifier :: Parser String
pIdentifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` reservedWords
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

pProgram :: Parser Program
pProgram =
  do
    statements <- many pStatement <* eof
    return (Program statements)

pStatement :: Parser Statement
pStatement =
  choice
    [ pExport,
      pImportDeclaration
    ]
  where
    pImportDeclaration = do
      pKeyword "import"
      binding <- pIdentifier <?> "default binding"
      pKeyword "from"
      from <- lexeme stringLiteral <?> "from clause"
      return (ImportDeclaration (ImportClauseDefault binding) from)
    pExport = ExportStatement <$ string "export"
