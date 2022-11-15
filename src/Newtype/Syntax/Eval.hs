module Newtype.Syntax.Eval where

import Data.Char qualified as Char
import Data.Generics.Uniplate.Data qualified as U
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.String.Here.Interpolated (i)
import Debug qualified
import Newtype.Syntax.Internal
import Newtype.Syntax.Newtype hiding (any)
import Prelude hiding (any)

data Symbol
  = -- | A callable function that needs to have the parameters resolved
    SymbolFunc {params :: [NTTypeParam], expr :: Expr}
  | -- | A literal expression that can be inlined
    SymbolLit {expr :: Expr}
  deriving (Eq, Show)

type SymbolTable = Map.Map String Symbol

collectDefinitions :: Program -> SymbolTable
collectDefinitions (Program stmts) =
  Map.fromList (Maybe.mapMaybe f stmts)
  where
    f TypeDefinition {params = [], ..} =
      Just (name, SymbolLit body)
    f TypeDefinition {..} =
      Just (name, SymbolFunc params body)
    f _ = Nothing

simplify :: SymbolTable -> Expr -> Expr
simplify tbl = \case
  Quote a -> a
  expr@(ExprGenericApplication a) ->
    -- If the symbol table has a definition for this type, then expand it
    -- Otherwise, just leave it as is
    Maybe.fromMaybe
      expr
      (callSymbol <$> Map.lookup a.name.getIdent tbl <*> pure a.args)
  expr@(ExprIdent (Ident id)) ->
    case id `Map.lookup` tbl of
      -- Inline the literal value
      Just (SymbolLit b) -> simplify' b
      -- TODO: callSymbol with default arguments if arity 0 is valid otherwise, error
      Just (SymbolFunc _ b) -> simplify' b
      -- No match, leave as is and recursee
      Nothing -> descend expr
  -- Special case for:
  --     if any <: ... then ...
  -- `any` both satisfies the constraint, but also doesn't ?? because
  -- it could be anything so it returns both branches.
  -- https://github.com/microsoft/TypeScript/issues/40049
  ExprConditionalType ConditionalType {..}
    | lhs == any ->
        simplify' . union $ [then', else']
  ExprConditionalType ConditionalType {..} ->
    -- transform the return value and recurse
    simplify' $
      if lhs `isAssignable` rhs
        then then'
        else else'
  Keyof (Literal (LObject props)) ->
    -- Convert each property to a string literal, pack them into a union
    union [mkString key | DataProperty {key} <- props]
  expr ->
    -- Nothing to do here, just recurse
    descend expr
  where
    simplify' = simplify tbl
    descend = U.descend simplify'

-- | Call a symbol with the given arguments
callSymbol :: Symbol -> [Expr] -> Expr
callSymbol sym args =
  -- Replace all instances of the parameter with the argument value (expression)
  U.transform f sym.expr
  where
    -- TODO make sure a variable isn't used if it's shadowed via infer
    f = \case
      ExprIdent (Ident id) | Map.member id argMap -> argMap Map.! id
      a -> a
    argMap =
      -- TODO handle default values
      -- TODO handle constraints ... maybe
      Map.fromList [(p.name, a) | p <- sym.params, a <- args]

-- type TestResult = Expr

-- newtype SymbolTable = SymbolTable {symMap :: FM.Map String Symbol}
--   deriving (Show)

-- evalProgram :: Program -> Program
-- evalProgram p@Program {..} =
--   Program statements'
--   where
--     -- Collect all type definitions to a symbol table
--     definitions = Maybe.catMaybes [mkSymFunc x | x <- statements, isTypeDefLike x]
--     symbolTable = mkSymbolTable definitions
--     -- Bottom- up eval symbols with sources from the symbol table
--     transform = Uniplate.transformBi (evalSymbols symbolTable)
--     statements' = transform <$> statements

-- evalSymbols :: SymbolTable -> Expr -> Expr
-- descend expr = f
--   where
--     f =
--       case expr of
--         -- Reduce template literal but merging all literal strings together
--         (TemplateLiteral list) ->
--           -- Replace the template literal with a string literal if possible
--           case foldr red [] list of
--             [] -> Literal . LString $ ""
--             [TemplateRaw x] -> Literal . LString $ x
--             xs -> TemplateLiteral xs
--           where
--             red (TemplateSubstitution (Literal (LString s))) ((TemplateRaw hd) : tl) =
--               red' s hd tl
--             red (TemplateRaw s) ((TemplateSubstitution (Literal (LString hd))) : tl) =
--               red' s hd tl
--             red (TemplateRaw s) ((TemplateRaw hd) : tl) =
--               red' s hd tl
--             red x acc = x : acc
--             red' a b l = TemplateRaw (a ++ b) : l

--         -- Special case for builtin string functions
--         ( ExprGenericApplication
--             ( GenericApplication
--                 (Ident id)
--                 [Literal (LString str)]
--               )
--           )
--             | id == "Uppercase" -> f Data.Char.toUpper str
--             | id == "Lowercase" -> f Data.Char.toLower str
--             | id == "Capitalize" -> mapFirstChar Data.Char.toUpper str
--             | id == "Uncapitalize" -> mapFirstChar Data.Char.toLower str
--             where
--               f ab = string . map ab
--               string = Literal . LString
--               empty = string ""
--               mapFirstChar f (x : xs) = string $ f x : xs
--               mapFirstChar _ [] = string []
--         (ExprGenericApplication (GenericApplication id args)) ->
--           -- If the function is not defined, return the original expression
--           -- Find the definition of the function
--           -- Apply the arguments to the definition of the function
--           maybe expr f (get id)
--           where
--             f = applyFunction scope args
--         (ExprIdent id@Ident {}) -> maybe expr symbolValue (get id)
--         MappedType {..} ->
--           literal props
--           where
--             props = [f item | item <- toList source]
--             f item = prop
--               where
--                 scope' = bind key (string item) scope
--                 prop =
--                   DataProperty
--                     { isReadonly
--                     , isOptional
--                     , key = case asExpr of
--                         Nothing -> item
--                         Just as ->
--                           let s = transform scope' as
--                            in case s of
--                                 Literal (LString x) -> x
--                                 _ -> error [i|Expected string literal, got #{s}|]
--                     , value = transform scope' value
--                     }
--         x -> x
--     get id = getSym id scope
--     literal = Literal . LObject
--     string = Literal . LString

-- -- | Bottom-up traveral, replace all symbols with their values
-- transform :: SymbolTable -> Expr -> Expr
-- transform symbolTable = Uniplate.transform (evalSymbols symbolTable)

-- -- | Resolve an expression like `Id 1` (or in TS `Id<1>`) to a concrete value.
-- applyFunction :: SymbolTable -> [Expr] -> Symbol -> Expr
-- applyFunction parentScope args sym =
--   case sym of
--     SymbolVal {..} -> symbolValue
--     SymbolFunc {..} ->
--       transform symbolTable symbolValue
--       where
--         symbolTable = SymbolTable map
--         parentMap = symMap parentScope
--         map = FM.unionWith onConflict parentMap argumentSymbols
--         argumentSymbols = FM.fromList list
--         -- Arguments and TypeParams should be of the same length
--         -- Arguments may be shorter if they are optional
--         list = [(name, SymbolVal name expr) | (name, expr) <- zipped]
--         zipped = zip [name | TypeParam {..} <- params] args
--         -- Merge the argument symbols with the parent scope
--         onConflict _ _ = error "variable shadowed"

-- toList :: Expr -> [String]
-- toList (Union a b) = concatMap toList [a, b]
-- toList (Literal (LString a)) = [a]
-- toList (Literal (LNumberInteger a)) = [show a]
-- toList other = error $ "RuntimeError: " <> nicify (show other) <> " must extend `string | number | symbol`"

-- data Symbol
--   = SymbolFunc {name :: String, params :: [NTTypeParam], symbolValue :: Expr}
--   | SymbolVal {name :: String, symbolValue :: Expr}
--   deriving (Show, D.Data, D.Typeable)

-- mkSymFunc :: Statement -> Maybe Symbol
-- mkSymFunc (TypeDefinition name params body) = Just $ SymbolFunc name params body
-- mkSymFunc (InterfaceDefinition name params extends props) = Just $ error "TODO"
-- mkSymFunc _ = Nothing

-- -- | Lookup a symbol in the symbol table
-- getSym :: Ident -> SymbolTable -> Maybe Symbol
-- getSym (Ident n) (SymbolTable st) = FM.lookup n st

-- getSymValue :: Ident -> SymbolTable -> Maybe Expr
-- getSymValue (Ident n) (SymbolTable st) = symbolValue <$> FM.lookup n st

-- -- | Bind a value to a name in the symbol table
-- bind :: String -> Expr -> SymbolTable -> SymbolTable
-- bind name symbolValue (SymbolTable st) =
--   SymbolTable $ FM.insert name (SymbolVal {name, symbolValue}) st

-- mkSymbolTable :: [Symbol] -> SymbolTable
-- mkSymbolTable stmnts = SymbolTable fm
--   where
--     fm = FM.fromList list
--     list = [(toSym s, s) | s <- stmnts]
--     toSym SymbolFunc {..} = name

-- isTypeDefLike :: Statement -> Bool
-- isTypeDefLike TypeDefinition {} = True
-- isTypeDefLike InterfaceDefinition {} = True
-- isTypeDefLike _ = False

-- evalTestSuite :: Program -> [TestResult]
-- evalTestSuite Program {statements} =
--   [evalTest name body | TestDefinition {..} <- statements]

-- evalTest :: String -> Expr -> TestResult
-- evalTest name expr = expr

-- evalExpr :: Expr -> Expr
-- -- Special case where `any` distributes over both branches
-- -- https://stackoverflow.com/questions/68754652/why-any-extends-x-a-b-give-a-b-in-typescript
-- evalExpr = \case
--   (ExprConditionalType ConditionalType {..})
--     | lhs == any ->
--         union [then', else']
--   (ExprConditionalType ConditionalType {..}) ->
--     if lhs `isAssignable` rhs
--       then evalExpr then'
--       else evalExpr else'
--   e -> e

-- {-
--  Note: Implementation assumes that strictNullChecks is enabled is by the end user.

--  The following table summarizes assignability between some abstract types. Rows
--  indicate what each is assignable to, columns indicate what is assignable to
--  them. A "o" indicates a combination that is compatible only when
--  strictNullChecks is off.

--  source: https://www.typescriptlang.org/docs/handbook/type-compatibility.html#any-unknown-object-void-undefined-null-and-never-assignability
--   |             | any | unknown | object | void | undefined | null | never |
--   | any         |     | ✓       | ✓      | ✓    | ✓         | ✓    | ✕     |
--   | unknown     | ✓   |         | ✕      | ✕    | ✕         | ✕    | ✕     |
--   | object      | ✓   | ✓       |        | ✕    | ✕         | ✕    | ✕     |
--   | void        | ✓   | ✓       | ✕      |      | ✕         | ✕    | ✕     |
--   | undefined   | ✓   | ✓       | o      | ✓    |           | o    | ✕     |
--   | null        | ✓   | ✓       | o      | o    | o         |      | ✕     |
--   | never       | ✓   | ✓       | ✓      | ✓    | ✓         | ✓    |       |
--  -}
isAssignable :: Expr -> Expr -> Bool
-- Trivial equality
isAssignable left right | left == right = True
-- A bottom type is assignable to anything
isAssignable left right | isBottomType left = True
-- But nothing is assignable to a bottom type
isAssignable left right | isBottomType right = False
-- Anything may be assigned to a top type
isAssignable left right | isTopType right = True
-- `unknown` is only assignable to top types
isAssignable (PrimitiveType PrimitiveUnknown) _ = False
-- `any` differs from `unknown` in that it is assignable to anything
isAssignable (PrimitiveType PrimitiveAny) _ = True
-- `undefined` is assignable to `void`
isAssignable (PrimitiveType PrimitiveUndefined) (PrimitiveType PrimitiveVoid) = True
-- Find type from literal
isAssignable left (PrimitiveType PrimitiveNumber) | isNumberLike left = True
-- Default, not assignable
isAssignable _ _ = False

isNumberLike :: Expr -> Bool
isNumberLike (Literal (LNumberInteger _)) = True
isNumberLike (Literal (LNumberDouble _)) = True
isNumberLike (PrimitiveType PrimitiveNumber) = True
isNumberLike _ = False

isTopType :: Expr -> Bool
isTopType (PrimitiveType PrimitiveAny) = True
isTopType (PrimitiveType PrimitiveUnknown) = True
isTopType _ = False

isBottomType :: Expr -> Bool
isBottomType (PrimitiveType PrimitiveNever) = True
isBottomType _ = False

-- fromExprIdent = \case
--   ExprIdent (Ident id) -> id
--   x -> error $ "Expected ExprIdent, got " ++ show x

never :: Expr
never = PrimitiveType PrimitiveNever

any :: Expr
any = PrimitiveType PrimitiveAny
