{-# LANGUAGE DeriveDataTypeable #-}

module Newtype.Eval where

import Control.Applicative ((<|>))
import Control.Monad
import qualified Data.Data as D
import Data.Dynamic
import Data.Functor
import Data.Generics.Uniplate.Data as U
import qualified Data.List as List
import qualified Data.Map.Strict as FM
import Data.Maybe (catMaybes, fromMaybe)
import Debug.Trace
import Newtype.Syntax
import Text.Nicify
import Prelude hiding (any)

type TestResult = Expr

newtype SymbolTable = SymbolTable {symMap :: FM.Map String Symbol}
  deriving (Show)

pp label a = trace (label ++ (nicify . show) a)

evalProgram :: Program -> Program
evalProgram p@Program {..} =
  Program statements'
  where
    -- Collect all type definitions to a symbol table
    definitions = catMaybes [mkSymFunc x | x <- statements, isTypeDefLike x]
    symbolTable = mkSymbolTable definitions
    -- For lack of a better name, evaluate the expression to a concrete value
    optimizeS = U.transformBi (resolveExpr symbolTable)
    statements' = optimizeS <$> statements

resolveExpr :: SymbolTable -> Expr -> Expr
resolveExpr scope a@(Literal _) = a
resolveExpr scope expr@(ExprGenericApplication (GenericApplication name args)) =
  -- If the function is not defined, return the original expression
  expr `fromMaybe` do
    -- Find the definition of the function
    -- FIXME: check if this is just a value firse
    td <- getSym name scope
    -- Apply the arguments to the definition of the function
    return $ callFunc scope td args
resolveExpr scope (Access ex ex') = error "NI: Access"
resolveExpr scope (DotAccess ex ex') = error "NI: DotAccess"
resolveExpr scope (PrimitiveType pt) = error "NI: PrimitiveType"
resolveExpr scope (Builtin s ex) = error "NI: Builtin"
resolveExpr scope a@(ExprIdent id) =
  fromMaybe a (getSymValue id scope)
resolveExpr scope (ExprInferIdent id) = error "NI: ExprInferIdent"
resolveExpr scope (Tuple exs) = error "NI: Tuple"
resolveExpr scope (ExprConditionalType ConditionalType {..})
  | lhs == any =
    union [thenExpr, elseExpr]
resolveExpr scope (ExprConditionalType ConditionalType {..}) =
  if lhs `isAssignable` rhs
    then resolveExpr scope thenExpr
    else resolveExpr scope elseExpr
resolveExpr scope (MappedType ex ex' ex3 m_ex m_b ma) = error "NI: MappedType"
resolveExpr scope (Union ex ex') = error "NI: Union"
resolveExpr scope (Intersection ex ex') = error "NI: Intersection"
resolveExpr scope Hole = error "NI: Hole"

data Symbol
  = SymbolFunc {name :: String, params :: [TypeParam], symbolValue :: Expr}
  | SymbolVal {name :: String, symbolValue :: Expr}
  deriving (Show, D.Data, D.Typeable)

-- Resolve an expression like `Id 1` (or in TS `Id<1>`) to a concrete value.
callFunc :: SymbolTable -> Symbol -> [Expr] -> Expr
callFunc parentScope td@SymbolFunc {..} args =
  resolveExpr scope symbolValue
  where
    argumentSymbols = FM.fromList list
      where
        -- Arguments and TypeParams should be of the same length
        -- Arguments may be shorter if they are optional
        zipped = zip [name | TypeParam {..} <- params] args
        list = [(name, SymbolVal name expr) | (name, expr) <- zipped]
    -- Merge the argument symbols with the parent scope
    scope =
      SymbolTable $
        FM.unionWith
          -- Don't allow overwriting of existing symbols
          (\_ _ -> error "variable shadowed")
          (symMap parentScope)
          argumentSymbols
callFunc _ SymbolVal {..} _ = symbolValue

mkSymFunc :: Statement -> Maybe Symbol
mkSymFunc (TypeDefinition name params body) = Just $ SymbolFunc name params body
mkSymFunc (InterfaceDefinition name params extends props) = Just $ error "TODO"
mkSymFunc _ = Nothing

getSym :: Ident -> SymbolTable -> Maybe Symbol
getSym (Ident n) (SymbolTable st) = FM.lookup n st

getSymValue :: Ident -> SymbolTable -> Maybe Expr
getSymValue (Ident n) (SymbolTable st) = symbolValue <$> FM.lookup n st

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
