use pretty::RcDoc as D;
use serde::Serialize;

use super::*;

use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
pub struct Expr<'a> {
    pub bindings: HashMap<Identifier, Node<'a>>,
    pub body: Node<'a>,
}

impl<'a> Expr<'a> {
    /// Replace all identifiers in the body of the let expression with their corresponding
    /// values
    pub(crate) fn simplify(&self) -> super::node::Node<'a> {
        let mut bindings = self.bindings.clone();
        // simplifiy all bindings first
        for (ident, value) in &self.bindings {
            let new_value = value.simplify();
            bindings.insert(ident.clone(), new_value);
        }

        let (tree, _) = self
            .body
            .prewalk(bindings, &|node, bindings| match &*node.value {
                Ast::Ident(id) => {
                    let new_value = bindings.get(id).unwrap_or(&node).clone();
                    (new_value, bindings)
                }
                _ => (node, bindings),
            });

        tree
    }
}

impl PrettySexpr for Expr<'_> {
    fn pretty_sexpr(&self) -> D<()> {
        let mut bindings = vec![];

        for (ident, value) in &self.bindings {
            bindings.push(ident.pretty_sexpr().append(":"));
            bindings.push(value.pretty_sexpr());
        }

        Ast::sexpr(vec![
            D::text("let"),
            Ast::sexpr(bindings),
            self.body.pretty_sexpr(),
        ])
    }
}

