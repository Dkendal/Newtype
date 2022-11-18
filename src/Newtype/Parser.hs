{-# LANGUAGE OverloadedStrings #-}

module Newtype.Parser (module Newtype.Parser, module Newtype.Parser.Tokens) where

import Control.Applicative hiding (many, some)
import Control.Monad
import Control.Monad.Combinators.Expr
import Control.Monad.State (evalState)
import Data.Functor
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as Text
import Data.Void (Void)
import Newtype.Parser.Tokens
import Newtype.Syntax.Newtype
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug (dbg)

type FormalParamMap = Map.Map String Expr

type CompilerError = ParseErrorBundle Text Void

type ParserResult a = Either CompilerError a

runNewTypeParser :: Parser a -> String -> Text -> ParserResult a
runNewTypeParser parser filename source =
  evalState stateAction initialState
  where
    stateAction = runParserT parser filename source
    initialState = pos1

pProgram :: Parser Program
pProgram =
  do
    scn
    statements <- pStatementList
    void eof
    return (Program statements)
  where
    pStatementList = sepEndBy pStatement scn

pStatement :: Parser Statement
pStatement =
  nonIndented $
    choice
      [ pTestDefinition
      , pImport
      , pTypeDefinition
      , pInterfaceDefintion
      ]

pTestDefinition :: Parser Statement
pTestDefinition =
  do
    pos <- L.indentLevel
    keyword "test"
    indentGuard GT pos
    name <- stringLiteral
    indentGuard GT pos
    keyword "where"
    indentGuard GT pos
    assertion <- keyword "assertAssignable" $> AssertAssignable <*> pExpr <*> pExpr
    return $ STestDefinition TestDefinition {..}

pImport :: Parser Statement
pImport = do
  keyword "import"
  fromClause <- stringLiteral <?> "package name"
  importClause <- pImportClause
  return ImportDeclaration {..}
  where
    pImportClause =
      parens $ do
        members <- pSpecifier `sepBy` comma
        return $ ImportClauseNamed members

    pSpecifier = do
      binding <- identifier
      alias <- optional $ do
        void $ keyword "as"
        identifier
      return $ maybe (ImportedBinding binding) (ImportedAlias binding) alias

pTypeDefinition :: Parser Statement
pTypeDefinition = do
  pos <- L.indentLevel
  name <- moduleIdent
  indentGuard GT pos
  params <- pFormalTypeParams
  equals
  body <- pExpr
  return TypeDefinition {..}

pInterfaceDefintion :: Parser Statement
pInterfaceDefintion = do
  pos <- L.indentLevel
  keyword "interface"
  name <- moduleIdent <?> "interface name"
  params <- pFormalTypeParams
  extends <- optional extendsClause <?> "interface extends clause"
  props <- fromMaybe [] <$> (optional . whereClause $ pos)
  return InterfaceDefinition {..}
  where
    whereClause :: Pos -> Parser [NTProperty]
    whereClause pos = do
      keyword "where"
      some $ do
        indentGuard GT pos <|> fail "properties must be indented"
        pProperty <?> "interface properties"

    -- Example input:
    --  extends Foo
    --  extends Foo A B C
    --  extends (Foo A B C)
    extendsClause :: Parser Extensible
    extendsClause = keyword "extends" *> generic <|> id
    generic = parens generic <|> ExtendGeneric <$> pGenericApplication
    id = ExtendIdent <$> pIdent

-- | Formal type parameters to a type or interface definition
pFormalTypeParams :: Parser [NTTypeParam]
pFormalTypeParams = do
  many (pNakedIdent <|> pIdent)
  where
    -- Example input:
    -- a
    -- (a : Type)
    -- (a : Type = Int)
    pNakedIdent = do
      name <- varIdent
      return (TypeParam name Nothing Nothing)

    pIdent =
      parens $ do
        name <- varIdent
        constraint <- optional $ do
          symbol "<:"
          pExpr
        defaultValue <- optional $ do
          equals
          pExpr
        return (TypeParam name defaultValue constraint)

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
    [ try (PrimitiveType <$> pPrimitive) <?> "type primitive"
    , try (ExprGenericApplication <$> pGenericApplication) <?> "generic type application"
    , try pTuple <?> "tuple"
    , try pArray <?> "array"
    , try pDictionaryComprehension <?> "mapped type"
    , pExprConditionalType <?> "conditional type"
    , (expandCaseStatement <$> pCaseStatement) <?> "case statement"
    , try pFunctionLiteral <?> "function literal"
    , pNumberIntegerLiteral <?> "number"
    , pNumberDoubleLiteral <?> "number"
    , pBooleanLiteral <?> "boolean literal"
    , (TemplateLiteral <$> pTemplateStrings) <?> "template literal"
    , pStringLiteral <?> "string literal"
    , pIdent' <?> "identifier"
    , -- Not actually valid outside of the extends expression
      -- but make my life a lot easier
      hidden pInferIdent
    , pObjectLiteral <?> "object literal"
    , hidden . parens $ pExpr
    ]

-- Parse typescript template strings:
-- `foo ${bar} baz`
pTemplateStrings :: Parser [NTTemplateString]
pTemplateStrings = do
  symbol "`"
  s <- manyTill p $ symbol "`"
  return . catMaybes . concat $ s
  where
    p = do
      s <- Just . TemplateRaw <$> manyTill L.charLiteral (lookAhead (choice [symbol "${", symbol "`"]))

      e <- optional $ do
        string "${"
        e <- pExpr
        string "}"
        return $ TemplateSubstitution e

      return [s, e]

{- | Parse a function literal.
 Example input:
  () => void
  (n: number) => number
  (head: number, ...tail: Array number) => number
-}
pFunctionLiteral :: Parser Expr
pFunctionLiteral =
  do
    typeParams <- optional $ do
      keyword "forall"
      params <- varIdent `sepBy1` comma
      period
      return params
    lparen
    params <- pParams <?> "function parameters"
    rest <- pRest <?> "function rest parameter"
    rparen
    keyword "=>"
    returnType <- pExpr <?> "function return type"
    return $ Literal LFunction {..}
  where
    pParams = sepEndBy pParam comma
    pRest = optional $ do
      symbol "..."
      pParam
    pParam = do
      name <- identifier
      colon
      type_ <- pExpr
      return (name, type_)

pPrimitive :: Parser PrimitiveType
pPrimitive =
  choice
    [ keyword "never" $> PrimitiveNever
    , keyword "any" $> PrimitiveAny
    , keyword "unknown" $> PrimitiveUnknown
    , keyword "number" $> PrimitiveNumber
    , keyword "bigint" $> PrimitiveBigInt
    , keyword "string" $> PrimitiveString
    , keyword "boolean" $> PrimitiveBoolean
    , keyword "null" $> PrimitiveNull
    , keyword "undefined" $> PrimitiveUndefined
    , keyword "void" $> PrimitiveVoid
    , keyword "object" $> PrimitiveObject
    , keyword "symbol" $> PrimitiveSymbol
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
          [ [prefix "not" Not]
          ,
            [ binary "and" And
            , binary "or" Or
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
          [ ExtendsLeft lhs <$ keyword "<:" <*> pExpr
          , ExtendsRight lhs <$ keyword ":>" <*> pExpr
          , Equals lhs <$ keyword "==" <*> pExpr
          , NotEquals lhs <$ keyword "!=" <*> pExpr
          ]

pDictionaryComprehension :: Parser Expr
pDictionaryComprehension = do
  braces $ do
    isReadonly <- pReadonly
    key' <- pExpr
    isOptional <- pOptional
    colon
    value <- pExpr <?> "mapped type value"
    keyword "for"
    key <- identifier <?> "mapped type key"
    symbol "in"
    source <- pExpr <?> "mapped type source"
    let asExpr = if key' == mkIdent key then Nothing else Just key'
    return (ExprMappedType (MappedType {..}))

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
pExpr =
  choice
    [ pLet
    , pTypeOp
    , pTerm
    ]

{- | Parse a let expression
 Example input:

   let x = 1 in x

   let x = 1
       y = 2
   in [x, y]
-}
pLet :: Parser Expr
pLet =
  do
    keyword "let"
    bindings <- pBinding
    keyword "in"
    Let [bindings] <$> pExpr
  where
    pBinding =
      do
        name <- identifier
        symbol "="
        expr <- pExpr
        return Binding {name = name, value = expr}

{- | Parse an expression with a type operator. A type operator may be a builtin
 prefix operator like `typeof` or `keyof`, or a prefix operator: like `!` for
 access, or type union or intersection, `|` or `&` respectively.
 |
-}
pTypeOp :: Parser Expr
pTypeOp =
  makeExprParser
    pTerm
    [
      [ InfixL $ DotAccess <$ (period <?> "dot access")
      , InfixL $ Access <$ (lexeme . try) (string "!" <* notFollowedBy (string "="))
      ]
    ,
      [ prefix "keyof" Keyof
      , prefix "readonly" Readonly
      , prefix "typeof" Typeof
      , prefix "quote" Quote
      , prefix "unquote" Unquote
      ]
    , [binary "&" Intersection, binary "|" Union]
    ]

pInferIdent :: Parser Expr
pInferIdent =
  do
    inferSym
    ident <- pIdent <?> "infered identifier"
    constraint <- optional $ do
      symbol "<:"
      pExpr
    return $ case constraint of
      Nothing -> ExprInferIdent ident
      Just constraint -> ExprInferIdentConstraint ident constraint

pNumberIntegerLiteral :: Parser Expr
pNumberIntegerLiteral = Literal . LNumberInteger <$> integer

pNumberDoubleLiteral :: Parser Expr
pNumberDoubleLiteral = Literal . LNumberDouble <$> float

pBooleanLiteral :: Parser Expr
pBooleanLiteral = Literal . LBoolean <$> choice [True <$ keyword "true", False <$ keyword "false"]

pStringLiteral :: Parser Expr
pStringLiteral = Literal . LString <$> stringLiteral

pTuple :: Parser Expr
pTuple = do
  members <- brackets $ pListValue `sepBy` comma
  return $ Tuple members

pArray :: Parser Expr
pArray = do
  pound
  choice
    [ do
        expr <- brackets pExpr
        return . Array $ expr
    , do
        expr <- parens pExpr
        return . Readonly . Array $ expr
    ]

-- Parse members of a list or tuple, e.g:
--  [1, 2, 3]
--  [a, ...b]
--  [hd: a, tl: ...b]
--  [a: 1, b: 2]
pListValue :: Parser NTListValue
pListValue =
  do
    label <- optional $ try $ identifier <* colon
    rest <- optional $ keyword "..."
    value <- pExpr
    return $ case rest of
      Nothing -> ListValue {label, value}
      Just rest -> ListRest {label, value}

-- return ListValue {..}

{- | Parse a rest parameter, e.g. `...T`
 pRest :: Parser Expr
 pRest = Rest <$> (lexeme . try) (string "..." *> pExpr)
-}
pIdent :: Parser Ident
pIdent = Ident <$> identifier <?> "identifier"

pModuleIdent :: Parser Ident
pModuleIdent = Ident <$> moduleIdent <?> "top level identifier"

pVarIdent :: Parser Ident
pVarIdent = Ident <$> varIdent <?> "variable identifier"

pIdent' :: Parser Expr
pIdent' = ExprIdent <$> pIdent <?> "identifier"

pObjectLiteral :: Parser Expr
pObjectLiteral =
  Literal . LObject <$> braces (pProperty `sepBy` comma)

pGenericApplication :: Parser NTGenericApplication
pGenericApplication = do
  pos <- L.indentLevel
  id <- pModuleIdent
  params <-
    some . choice $
      [ indentGuard GT pos *> pIdent'
      , indentGuard GT pos *> try pTerm
      ]
  return $ GenericApplication id params

pProperty :: Parser NTProperty
pProperty = p
  where
    p = do
      isReadonly <- pReadonly
      isIndex <- optional (keyword "index")
      case isIndex of
        Just _ ->
          do
            keySource <- pExpr
            colon
            value <- pExpr
            let key = "key"
            return (IndexSignature {..})
        Nothing ->
          choice
            [ do
                keyT <- knownSymbols
                let key = keyT
                isOptional <- pOptional
                colon
                value <- pExpr
                return (ComputedProperty {..})
            , do
                key <- identifier
                isOptional <- pOptional
                colon
                value <- pExpr
                return (DataProperty {..})
            ]

    -- This could be changed to return a Data type instead, but it's more convenient
    -- to just return the string values of the computed type and sort it out later.
    -- This is no real need for the extra type safety because they are all treated
    -- the same way.
    knownSymbols :: Parser String
    knownSymbols =
      stripPrefix . unpack =<< choice (keyword . pack . addPrefix <$> names)
      where
        stripPrefix ('\'' : xs) = return xs
        addPrefix xs = "'" ++ xs
        names =
          [ "hasInstance"
          , "isConcatSpreadable"
          , "iterator"
          , "match"
          , "replace"
          , "search"
          , "species"
          , "split"
          , "toPrimitive"
          , "toStringTag"
          , "unscopables"
          ]

{- | readonly keyword of an object literal property

 Example source:
 * `readonly foo: bar`
 * `-readonly foo: bar`
-}
pReadonly =
  optional . choice $
    [ True <$ keyword "readonly"
    , False <$ keyword "-readonly"
    ]

{- | optional keyword of an object literal property

 Example source:
 * `foo?: bar`
 * `foo-?: bar`
-}
pOptional =
  p <?> "optional postfix key modifier"
  where
    p =
      optional . choice $
        [ True <$ qmark
        , False <$ keyword "-?"
        ]

makeParams :: [String] -> FormalParamMap -> FormalParamMap -> [NTTypeParam]
makeParams paramNames constraints defaults =
  [ TypeParam
    { name = paramName
    , defaultValue = Map.lookup paramName defaults
    , constraint = Map.lookup paramName constraints
    }
  | let defaultValue = Nothing
  , paramName <- paramNames
  ]
