use crate::extends_result::ExtendsResult;

use super::*;
use cond_expr::CondExpr;
use if_expr::IfExpr;
use let_expr::LetExpr;
use match_expr::MatchExpr;
use pest::Span;
use std::collections::HashMap;


pub type Bindings<'a> = HashMap<String, Ast<'a>>;

