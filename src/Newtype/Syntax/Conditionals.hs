module Newtype.Syntax.Conditionals where

import Newtype.Syntax

data ConditionalExpr = ConditionalExpr
  { condition :: BoolExpr
  , thenExpr :: Expr
  , elseExpr :: Expr
  }
  deriving (Show, Eq)

data BoolExpr
  = And BoolExpr BoolExpr
  | Or BoolExpr BoolExpr
  | Not BoolExpr
  | ExtendsLeft Expr Expr
  | ExtendsRight Expr Expr
  | Equals Expr Expr
  | NotEquals Expr Expr
  deriving (Show, Eq)

data Case
  = Case Expr Expr
  deriving (Eq, Show)

data CaseStatement = CaseStatement Expr [Case]
  deriving (Eq, Show)

expandCaseStatement :: CaseStatement -> Expr
expandCaseStatement (CaseStatement lhs [Case rhs then', Case Hole else']) =
  ct' lhs rhs then' else'
expandCaseStatement (CaseStatement lhs [Case rhs then']) =
  ct' lhs rhs then' never
expandCaseStatement (CaseStatement lhs (Case rhs then' : tl)) =
  ct' lhs rhs then' (expandCaseStatement (CaseStatement lhs tl))
expandCaseStatement (CaseStatement lhs []) = never

expandConditional' :: ConditionalExpr -> Expr
expandConditional' = ExprConditionalType . expandConditional

expandConditional :: ConditionalExpr -> ConditionalType
expandConditional (ConditionalExpr (ExtendsLeft a b) then' else') =
  ct a b then' else'
expandConditional (ConditionalExpr (ExtendsRight b a) then' else') =
  ct a b then' else'
expandConditional (ConditionalExpr (Not con) then' else') =
  expandConditional (cx con else' then')
expandConditional (ConditionalExpr (Equals a b) then' else') =
  ct (t1 a) (t1 b) then' else'
expandConditional (ConditionalExpr (NotEquals a b) then' else') =
  expandConditional (cx (Not (Equals a b)) then' else')
expandConditional (ConditionalExpr (And a b) then' else') =
  let outer then'' = cx a then'' else'
      inner = cx b then' else'
   in expandConditional (outer (expandConditional' inner))
expandConditional (ConditionalExpr (Or a b) then' else') =
  let outer else'' = cx a then' else''
      inner = cx b then' else'
   in expandConditional (outer (expandConditional' inner))

cx :: BoolExpr -> Expr -> Expr -> ConditionalExpr
cx = ConditionalExpr

ct' :: Expr -> Expr -> Expr -> Expr -> Expr
ct' lhs rhs then' else' = ExprConditionalType $ ConditionalType lhs rhs then' else'

ct :: Expr -> Expr -> Expr -> Expr -> ConditionalType
ct = ConditionalType

-- | Single element tuple
t1 a = Tuple [ListValue Nothing a]
