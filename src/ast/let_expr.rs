use pretty::RcDoc as D;
use serde::Serialize;

use super::*;

use std::collections::HashMap;

#[derive(Derivative, Clone, Eq, Serialize)] #[derivative(PartialEq)] #[derivative(Debug)]
#[serde(rename_all = "kebab-case")]
pub struct LetExpr<'a> {
    #[serde(skip)]
    pub span: Span<'a>,
    pub bindings: Bindings<'a>,
    pub body: Rc<Ast<'a>>,
}

impl<'a> LetExpr<'a> {
    pub fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast<'a>) -> Ast<'a>,
    {
        let mut expr = self.clone();
        expr.body = f(&self.body).into();
        expr
    }
    /// Replace all identifiers in the body of the let expression with their corresponding
    /// values
    pub fn simplify(&self) -> Ast<'a> {
        let mut bindings = self.bindings.clone();
        // simplifiy all bindings first
        for (ident, value) in &self.bindings {
            let new_value = value.simplify();
            bindings.insert(ident.clone(), new_value);
        }

        let (tree, _) = self.body.prewalk(bindings, &|ast, bindings| match ast {
            Ast::Ident(ref id) => {
                let new_value = bindings.get(&id.name).unwrap_or(&ast).clone();
                (new_value, bindings)
            }
            _ => (ast, bindings),
        });

        tree
    }
}
