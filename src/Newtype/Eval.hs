{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module Newtype.Eval where

import Control.Applicative ((<|>))
import Control.Monad
import qualified Data.Data as D
import Data.Dynamic
import Data.Functor
import qualified Data.Generics.Uniplate.Data as Uniplate
import qualified Data.List as List
import qualified Data.Map.Strict as FM
import qualified Data.Maybe as Maybe
import Data.String.Here.Interpolated
import Debug
import Debug.Trace
import Newtype.Syntax
import Text.Nicify
import Prelude hiding (any)

type TestResult = Expr

newtype SymbolTable = SymbolTable {symMap :: FM.Map String Symbol}
  deriving (Show)

evalProgram :: Program -> Program
evalProgram p@Program {..} =
  Program statements'
  where
    -- Collect all type definitions to a symbol table
    definitions = Maybe.catMaybes [mkSymFunc x | x <- statements, isTypeDefLike x]
    symbolTable = mkSymbolTable definitions
    -- Bottom- up eval symbols with sources from the symbol table
    transform = Uniplate.transformBi (evalSymbols symbolTable)
    statements' = transform <$> statements

evalSymbols :: SymbolTable -> Expr -> Expr
evalSymbols scope expr = f
  where
    f =
      case expr of
        -- Special case for:
        --     if any <: ... then ...
        -- `any` both satisfies the constraint, but also doesn't ?? because
        -- it could be anything so it returns both branches.
        -- https://github.com/microsoft/TypeScript/issues/40049
        (ExprConditionalType ConditionalType {..})
          | lhs == any ->
            union [thenExpr, elseExpr]
        (ExprConditionalType ConditionalType {..}) ->
          if lhs `isAssignable` rhs
            then evalSymbols scope thenExpr
            else evalSymbols scope elseExpr
        (Keyof (Literal (ObjectLiteral props))) ->
          -- Convert each property to a string literal, pack them into a union
          union [mkString key | DataProperty {key} <- props]
        (ExprGenericApplication (GenericApplication id args)) ->
          -- If the function is not defined, return the original expression
          -- Find the definition of the function
          -- Apply the arguments to the definition of the function
          maybe expr f (get id)
          where
            f sym =
              let result = applyFunction scope args sym
               in trace (prettyPrint expr ++ " == " ++ prettyPrint result) result
        (ExprIdent id@Ident {}) -> maybe expr symbolValue (get id)
        MappedType {..} ->
          literal props
          where
            props = [f item | item <- toList source]
            f item = prop
              where
                scope' =
                  bind (fromExprIdent key) (string item) scope
                prop =
                  DataProperty
                    { isReadonly
                    , isOptional
                    , isIndex = False
                    , accessor = Nothing
                    , key = item
                    , value = transform scope' value
                    }
        x -> x
    get id = getSym id scope
    literal = Literal . ObjectLiteral
    string = Literal . StringLiteral

-- | Bottom-up traveral, replace all symbols with their values
transform :: SymbolTable -> Expr -> Expr
transform symbolTable = Uniplate.transform (evalSymbols symbolTable)

-- | Resolve an expression like `Id 1` (or in TS `Id<1>`) to a concrete value.
applyFunction :: SymbolTable -> [Expr] -> Symbol -> Expr
applyFunction parentScope args sym =
  case sym of
    SymbolVal {..} -> symbolValue
    SymbolFunc {..} ->
      transform symbolTable symbolValue
      where
        symbolTable = SymbolTable map
        parentMap = symMap parentScope
        map = FM.unionWith onConflict parentMap argumentSymbols
        argumentSymbols = FM.fromList list
        -- Arguments and TypeParams should be of the same length
        -- Arguments may be shorter if they are optional
        list = [(name, SymbolVal name expr) | (name, expr) <- zipped]
        zipped = zip [name | TypeParam {..} <- params] args
        -- Merge the argument symbols with the parent scope
        onConflict _ _ = error "variable shadowed"

toList :: Expr -> [String]
toList (Union a b) = concatMap toList [a, b]
toList (Literal (StringLiteral a)) = [a]
toList (Literal (NumberIntegerLiteral a)) = [show a]
toList other = error $ "RuntimeError: " <> nicify (show other) <> " must extend `string | number | symbol`"

data Symbol
  = SymbolFunc {name :: String, params :: [TypeParam], symbolValue :: Expr}
  | SymbolVal {name :: String, symbolValue :: Expr}
  deriving (Show, D.Data, D.Typeable)

mkSymFunc :: Statement -> Maybe Symbol
mkSymFunc (TypeDefinition name params body) = Just $ SymbolFunc name params body
mkSymFunc (InterfaceDefinition name params extends props) = Just $ error "TODO"
mkSymFunc _ = Nothing

-- | Lookup a symbol in the symbol table
getSym :: Ident -> SymbolTable -> Maybe Symbol
getSym (Ident n) (SymbolTable st) = FM.lookup n st

getSymValue :: Ident -> SymbolTable -> Maybe Expr
getSymValue (Ident n) (SymbolTable st) = symbolValue <$> FM.lookup n st

-- | Bind a value to a name in the symbol table
bind :: String -> Expr -> SymbolTable -> SymbolTable
bind name symbolValue (SymbolTable st) =
  SymbolTable $ FM.insert name (SymbolVal {name, symbolValue}) st

mkSymbolTable :: [Symbol] -> SymbolTable
mkSymbolTable stmnts = SymbolTable fm
  where
    fm = FM.fromList list
    list = [(toSym s, s) | s <- stmnts]
    toSym SymbolFunc {..} = name

isTypeDefLike :: Statement -> Bool
isTypeDefLike TypeDefinition {} = True
isTypeDefLike InterfaceDefinition {} = True
isTypeDefLike _ = False

evalTestSuite :: Program -> [TestResult]
evalTestSuite Program {statements} =
  [evalTest name body | TestDefinition {..} <- statements]

evalTest :: String -> Expr -> TestResult
evalTest name expr = expr

evalExpr :: Expr -> Expr
-- Special case where `any` distributes over both branches
-- https://stackoverflow.com/questions/68754652/why-any-extends-x-a-b-give-a-b-in-typescript
evalExpr
  (ExprConditionalType ConditionalType {..})
    | lhs == any =
      union [thenExpr, elseExpr]
evalExpr
  (ExprConditionalType ConditionalType {..}) =
    if lhs `isAssignable` rhs
      then evalExpr thenExpr
      else evalExpr elseExpr
evalExpr e = e

{-
 Note: Implementation assumes that strictNullChecks is enabled is by the end user.

 The following table summarizes assignability between some abstract types. Rows
 indicate what each is assignable to, columns indicate what is assignable to
 them. A "o" indicates a combination that is compatible only when
 strictNullChecks is off.

 source: https://www.typescriptlang.org/docs/handbook/type-compatibility.html#any-unknown-object-void-undefined-null-and-never-assignability
  |             | any | unknown | object | void | undefined | null | never |
  | any         |     | ✓       | ✓      | ✓    | ✓         | ✓    | ✕     |
  | unknown     | ✓   |         | ✕      | ✕    | ✕         | ✕    | ✕     |
  | object      | ✓   | ✓       |        | ✕    | ✕         | ✕    | ✕     |
  | void        | ✓   | ✓       | ✕      |      | ✕         | ✕    | ✕     |
  | undefined   | ✓   | ✓       | o      | ✓    |           | o    | ✕     |
  | null        | ✓   | ✓       | o      | o    | o         |      | ✕     |
  | never       | ✓   | ✓       | ✓      | ✓    | ✓         | ✓    |       |
 -}
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
isNumberLike (Literal (NumberIntegerLiteral _)) = True
isNumberLike (Literal (NumberDoubleLiteral _)) = True
isNumberLike (PrimitiveType PrimitiveNumber) = True
isNumberLike _ = False

isTopType :: Expr -> Bool
isTopType (PrimitiveType PrimitiveAny) = True
isTopType (PrimitiveType PrimitiveUnknown) = True
isTopType _ = False

isBottomType :: Expr -> Bool
isBottomType (PrimitiveType PrimitiveNever) = True
isBottomType _ = False

fromExprIdent = \case
  ExprIdent (Ident id) -> id
  x -> error $ "Expected ExprIdent, got " ++ show x
