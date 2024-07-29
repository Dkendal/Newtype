use crate::extends_result::ExtendsResult;

use super::*;
use cond_expr::CondExpr;
use if_expr::IfExpr;
use let_expr::LetExpr;
use match_expr::MatchExpr;
use pest::Span;
use std::collections::HashMap;

#[derive(Debug, Eq, Clone)]
pub struct Node<'a> {
    pub span: Span<'a>,
    pub value: Box<Ast<'a>>,
}

impl<'a> From<Ast<'a>> for Node<'a> {
    fn from(value: Ast<'a>) -> Self {
        Node {
            span: value.as_span(),
            value: Box::new(value.clone()),
        }
    }
}

impl<'a> From<&Ast<'a>> for Node<'a> {
    fn from(value: &Ast<'a>) -> Self {
        Node {
            span: value.as_span(),
            value: Box::new(value.clone()),
        }
    }
}

impl<'a> From<&Rc<Ast<'a>>> for Node<'a> {
    fn from(value: &Rc<Ast<'a>>) -> Self {
        Node {
            span: value.as_span(),
            value: value.into(),
        }
    }
}

impl<'a> PartialEq for Node<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<'a> serde::Serialize for Node<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.value.serialize(serializer)
    }
}

impl<'a> Node<'a> {
    pub fn from_pair<R>(pair: &pest::iterators::Pair<'a, R>, value: Ast<'a>) -> Self
    where
        R: pest::RuleType,
    {
        Self {
            span: pair.clone().as_span(),
            value: Box::new(value),
        }
    }

    pub fn as_span(&self) -> Span<'a> {
        self.span
    }

    pub fn to_sexp(&self) -> serde_lexpr::Result<serde_lexpr::Value> {
        serde_lexpr::to_value(self)
    }

    pub fn from_span(span: Span<'a>, value: Ast<'a>) -> Self {
        Self {
            span,
            value: Box::new(value),
        }
    }

    /// Transform the value of the node with a function that takes a reference to the value
    pub fn map(&self, f: impl Fn(&Ast<'a>) -> Ast<'a>) -> Self {
        Self {
            span: self.span,
            value: Box::new(f(&self.value)),
        }
    }

    /// Replace the value of the node with a new value, creating a new node
    /// with the same span.
    pub fn replace(self, value: Ast<'a>) -> Self {
        Self {
            span: self.span,
            value: Box::new(value),
        }
    }

    pub fn new(span: Span<'a>, value: Ast<'a>) -> Self {
        Self {
            span,
            value: Box::new(value),
        }
    }

    pub fn set_span(&mut self, span: Span<'a>) {
        self.span = span;
    }

    pub fn set_value(&mut self, value: Box<Ast<'a>>) {
        self.value = value;
    }

    pub fn prewalk<Context, F>(&self, ctx: Context, pre: &F) -> (Self, Context)
    where
        Context: Clone,
        F: Fn(Self, Context) -> (Self, Context),
    {
        self.traverse(ctx, pre, &|n, c| (n, c))
    }

    pub fn postwalk<Context, F>(&self, ctx: Context, post: &F) -> (Self, Context)
    where
        Context: Clone,
        F: Fn(Self, Context) -> (Self, Context),
    {
        self.traverse(ctx, &|n, c| (n, c), post)
    }

    pub fn traverse<Context, Pre, Post>(
        &self,
        ctx: Context,
        pre: &Pre,
        post: &Post,
    ) -> (Self, Context)
    where
        Context: Clone,
        Pre: Fn(Self, Context) -> (Self, Context),
        Post: Fn(Self, Context) -> (Self, Context),
    {
        let node = self.clone();

        let (node, ctx) = pre(node, ctx);

        let ast = node
            .value
            .map(|node| node.traverse(ctx.clone(), pre, post).0);

        let node = Node::new(node.span, ast);

        let (node, acc) = post(node, ctx);

        (node, acc)
    }

    pub fn simplify(&self) -> Self {
        let bindings: Bindings = Default::default();

        let identity = |node, ctx| (node, ctx);

        let (tree, _) = self.traverse(bindings, &identity, &|node, ctx| {
            let span = node.as_span();
            let value = node.value.as_ref();

            match value {
                Ast::IfExpr(if_expr) => (if_expr.simplify().into(), ctx),
                Ast::MatchExpr(match_expr) => (match_expr.simplify().into(), ctx),
                Ast::CondExpr(cond_expr) => (cond_expr.simplify().into(), ctx),
                Ast::LetExpr(let_expr) => (let_expr.simplify().into(), ctx),
                Ast::Path(path) => (path.simplify().into(), ctx),
                Ast::UnionType(UnionType { types, .. }) => match types.as_slice() {
                    // Flatten nested union types (both)
                    [Ast::UnionType(UnionType {
                        types: lhs_types, ..
                    }), Ast::UnionType(UnionType {
                        types: rhs_types, ..
                    })] => {
                        let types = lhs_types
                            .clone()
                            .into_iter()
                            .chain(rhs_types.clone())
                            .collect();

                        let ast = Ast::UnionType(UnionType { types, span });

                        let node = Node::new(node.span, ast);

                        (node, ctx)
                    }
                    // Flatten nested union types (rhs)
                    [lhs, Ast::UnionType(UnionType {
                        types: rhs_types, ..
                    })] => {
                        let mut types = rhs_types.clone();

                        types.push(lhs.clone());

                        let ast = Ast::UnionType(UnionType { types, span });

                        let node = Node::new(node.span, ast);

                        (node, ctx)
                    }
                    // Flatten nested union types (lhs)
                    [Ast::UnionType(UnionType {
                        types: lhs_types, ..
                    }), rhs] => {
                        let mut types = lhs_types.clone();

                        types.push(rhs.clone());

                        let ast = Ast::UnionType(UnionType { types, span });

                        let node = Node::new(node.span, ast);

                        (node, ctx)
                    }
                    // Move all intersection to the right
                    types => {
                        let types = types
                            .iter()
                            .sorted_by(|a, b| a.is_intersection().cmp(&b.is_intersection()))
                            .cloned()
                            .collect_vec();

                        let ast = Ast::UnionType(UnionType { types, span });

                        let node = Node::new(node.span, ast);

                        (node, ctx)
                    }
                },
                _ => (node, ctx),
            }
        });
        tree
    }

    pub fn eval(&self) -> Self {
        let (tree, _) = self.prewalk((), &|node, ctx| match &*node.value {
            Ast::MacroCall(value) => (value.eval().into(), ctx),
            _ => (node, ctx),
        });

        tree
    }

    pub fn is_subtype(&self, other: &Self) -> ExtendsResult {
        self.value.as_ref().is_subtype(&other.value)
    }

    pub fn is_super_type(&self, other: &Self) -> ExtendsResult {
        other.is_subtype(self)
    }

    pub fn is_set_op(&self) -> bool {
        self.value.is_set_op()
    }
}

pub type Bindings<'a> = HashMap<String, Ast<'a>>;

impl<'a> typescript::Pretty for Node<'a> {
    fn to_ts(&self) -> pretty::RcDoc<()> {
        self.value.to_ts()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parser::Rule, test_support::*};
    use pretty_assertions::assert_eq;

    #[test]
    fn is_subtype() {
        assert_eq!(ast!("1").is_subtype(&ast!("number")), ExtendsResult::True)
    }
}
