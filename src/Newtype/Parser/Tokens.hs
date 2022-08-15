{-# LANGUAGE OverloadedStrings #-}

module Newtype.Parser.Tokens where

import Control.Applicative hiding (many)
import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Maybe
import Data.Text
import Data.Void
import Debug.Trace
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.State

type Parser = ParsecT Void Text (State Pos)

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
    "forall",
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

keyword' :: Text -> Parser Text
keyword' txt = lexeme' (string txt <* notFollowedBy identifierTail)

stringLiteral :: Parser String
stringLiteral = lexeme $ char '\"' *> L.charLiteral `manyTill` char '\"'

integer :: Parser Integer
integer = lexeme L.decimal

integer' :: Parser Integer
integer' = lexeme' L.decimal

float :: Parser Double
float = lexeme L.float

float' :: Parser Double
float' = lexeme' L.float

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

period :: Parser Text
period = symbol "."
{-# INLINE period #-}

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

-- | An identifier that may be exported from a module. A type, an interface, or
-- a class. Must begin with an upper-case letter.
moduleIdent :: Parser String
moduleIdent = checkLower <|> (lexeme . try) (p >>= check)
  where
    p = (:) <$> head <*> many tail
    checkLower =
      lookAhead lowerChar
        >> fail "top level identifiers must start with an upper case character"
    check x =
      if x `elem` reservedWords
        then fail $ "keyword " ++ show x ++ " cannot be a module identifier"
        else return x
    head = upperChar <|> rest
    tail = alphaNumChar <|> rest
    rest = underscore <|> dollar

-- | An identifier representing a variable. Must begin with a lower-case
varIdent :: Parser String
varIdent = checkUpper <|> (lexeme . try) (p >>= check)
  where
    checkUpper =
      lookAhead upperChar
        >> fail "variable names must start with a lower case character"
    p = (:) <$> head <*> many tail
    check x =
      if x `elem` reservedWords
        then fail $ "keyword " ++ show x ++ " cannot be a variable identifier"
        else return x
    head = lowerChar <|> rest
    tail = alphaNumChar <|> rest
    rest = underscore <|> dollar
