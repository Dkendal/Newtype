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
        // Reducer that maps over a list of nodes
        let red_items = move |node: &Nodes<'a>, ctx: Context| {
            node.iter()
                .map(|item| item.traverse(ctx.clone(), pre, post).0)
                .collect_vec()
        };

        // Returns a closure that takes a node
        let fn_red_pick_node = move |ctx| move |node: &Node<'a>| (node.traverse(ctx, pre, post).0);

        let node = self.clone();

        let (node, ctx) = pre(node, ctx);

        let (node, ctx) = match &*node.value {
            Ast::Access(expr) => {
                let expr = expr.map(|node| node.traverse(ctx.clone(), pre, post).0);
                let ast = Ast::Access(expr);
                (self.clone().replace(ast), ctx)
            }
            Ast::ApplyGeneric(expr) => {
                let expr = expr.map(|node| node.traverse(ctx.clone(), pre, post).0);
                let ast = Ast::ApplyGeneric(expr);
                (self.clone().replace(ast), ctx)
            }

            Ast::Array(expr) => {
                let (expr, _) = expr.traverse(ctx.clone(), pre, post);
                let ast = Ast::Array(expr);
                (self.clone().replace(ast), ctx)
            }

            Ast::Builtin(expr) => {
                let expr = expr.map(|node| node.traverse(ctx.clone(), pre, post).0);
                let ast = Ast::Builtin(expr);
                (self.clone().replace(ast), ctx)
            }

            Ast::CondExpr(expr) => {
                let expr = expr.map(|node| node.traverse(ctx.clone(), pre, post).0);
                let ast = Ast::CondExpr(expr);
                (self.clone().replace(ast), ctx)
            }

            Ast::ExtendsInfixOp(expr) => {
                let expr = expr.map(|node| node.traverse(ctx.clone(), pre, post).0);
                let ast = Ast::ExtendsInfixOp(expr);
                (self.clone().replace(ast), ctx)
            }

            Ast::ExtendsExpr(ExtendsExpr {
                lhs,
                rhs,
                then_branch,
                else_branch,
                span,
            }) => {
                let lhs = lhs.traverse(ctx.clone(), pre, post).0;
                let rhs = rhs.traverse(ctx.clone(), pre, post).0;
                let then_branch = then_branch.traverse(ctx.clone(), pre, post).0;
                let else_branch = else_branch.traverse(ctx.clone(), pre, post).0;

                let ast = Ast::ExtendsExpr(ExtendsExpr {
                    lhs,
                    rhs,
                    then_branch,
                    else_branch,
                    span: *span,
                });
                (self.clone().replace(ast), ctx)
            }

            Ast::ExtendsPrefixOp(ExtendsPrefixOp { op, value, span }) => {
                let value = value.traverse(ctx.clone(), pre, post).0;

                let ast = Ast::ExtendsPrefixOp(ExtendsPrefixOp {
                    op: op.clone(),
                    value,
                    span: *span,
                });

                (self.clone().replace(ast), ctx)
            }

            Ast::IfExpr(IfExpr {
                condition,
                then_branch,
                else_branch,
                span,
            }) => {
                let (condition, _) = condition.traverse(ctx.clone(), pre, post);
                let (then_branch, _) = then_branch.traverse(ctx.clone(), pre, post);
                let else_branch = else_branch.as_ref().map(fn_red_pick_node(ctx.clone()));

                let ast = Ast::IfExpr(IfExpr {
                    condition,
                    then_branch,
                    else_branch,
                    span: *span,
                });

                (self.clone().replace(ast), ctx)
            }

            Ast::ImportStatement(ImportStatement {
                import_clause,
                module,
                span,
            }) => {
                let ast = Ast::ImportStatement(ImportStatement {
                    import_clause: import_clause.clone(),
                    module: module.clone(),
                    span: *span,
                });

                (self.clone().replace(ast), ctx)
            }
            Ast::LetExpr(expr) => {
                let expr = expr.map(|node| node.traverse(ctx.clone(), pre, post).0);
                let ast = Ast::LetExpr(expr);
                (self.clone().replace(ast), ctx)
            }
            Ast::MappedType(expr) => {
                let expr = expr.map(|node| node.traverse(ctx.clone(), pre, post).0);
                let ast = Ast::MappedType(expr);
                (self.clone().replace(ast), ctx)
            }
            Ast::MatchExpr(expr) => {
                let expr = expr.map(|node| node.traverse(ctx.clone(), pre, post).0);
                let ast = Ast::MatchExpr(expr);
                (self.clone().replace(ast), ctx)
            }
            Ast::Path(Path { segments, span }) => {
                let segments = segments
                    .iter()
                    .map(|node| node.traverse(ctx.clone(), pre, post).0)
                    .collect_vec();

                let ast = Ast::Path(Path {
                    segments,
                    span: *span,
                });

                (self.clone().replace(ast), ctx)
            }
            Ast::TypeLiteral(ty) => {
                let properties = ty
                    .properties
                    .iter()
                    .map(|prop| {
                        let mut prop = prop.clone();
                        prop.value = prop.value.traverse(ctx.clone(), pre, post).0;
                        prop
                    })
                    .collect_vec();

                let ast = Ast::TypeLiteral(ObjectLiteral {
                    properties,
                    span: ty.span,
                });

                (self.clone().replace(ast), ctx)
            }
            Ast::Program(program) => {
                let mut ctx = ctx.clone();
                let mut statements = program.statements.clone();

                for statement in &mut statements {
                    (*statement, ctx) = statement.traverse(ctx.clone(), pre, post);
                }

                let ast = Ast::Program(Program { statements });

                (self.clone().replace(ast), ctx)
            }
            Ast::Statement(inner) => {
                // Statement MAY propagate context to siblings
                let (inner, ctx) = inner.traverse(ctx.clone(), pre, post);
                let ast = Ast::Statement(inner);
                (self.clone().replace(ast), ctx)
            }
            Ast::TemplateString(_) => (node, ctx),
            Ast::Tuple(Tuple { items, span }) => {
                let items = items
                    .iter()
                    .map(|item| item.traverse(ctx.clone(), pre, post).0)
                    .collect_vec();

                let ast = Ast::Tuple(Tuple { items, span: *span });

                (self.clone().replace(ast), ctx)
            }
            Ast::TypeAlias(TypeAlias {
                export,
                name,
                params,
                body,
                span,
            }) => {
                let (body, _) = body.traverse(ctx.clone(), pre, post);

                let params = params
                    .iter()
                    .map(
                        |TypeParameter {
                             name,
                             constraint,
                             default,
                             rest,
                             span,
                         }| {
                            let constraint = constraint.as_ref().map(fn_red_pick_node(ctx.clone()));
                            let default = default.as_ref().map(fn_red_pick_node(ctx.clone()));

                            TypeParameter {
                                name: name.clone(),
                                constraint,
                                default,
                                rest: *rest,
                                span: *span,
                            }
                        },
                    )
                    .collect_vec();

                let ast = Ast::TypeAlias(TypeAlias {
                    export: *export,
                    name: name.clone(),
                    params,
                    body,
                    span: *span,
                });

                (self.clone().replace(ast), ctx)
            }

            Ast::UnionType(UnionType { types, span }) => {
                let types = types
                    .iter()
                    .map(|ty| ty.traverse(ctx.clone(), pre, post).0)
                    .collect_vec();

                let ast = Ast::UnionType(UnionType { types, span: *span });

                (self.clone().replace(ast), ctx)
            }

            Ast::IntersectionType(IntersectionType { types, span }) => {
                let types = types
                    .iter()
                    .map(|ty| ty.traverse(ctx.clone(), pre, post).0)
                    .collect_vec();

                let ast = Ast::IntersectionType(IntersectionType { types, span: *span });

                (self.clone().replace(ast), ctx)
            }

            _ => (node, ctx),
        };

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
                Ast::IfExpr(if_expr) => (if_expr.simplify(), ctx),
                Ast::MatchExpr(match_expr) => (match_expr.simplify(), ctx),
                Ast::CondExpr(cond_expr) => (cond_expr.simplify(), ctx),
                Ast::LetExpr(let_expr) => (let_expr.simplify(), ctx),
                Ast::Path(path) => (path.simplify(span), ctx),
                Ast::UnionType(UnionType { types, .. }) => match types.as_slice() {
                    // Flatten nested union types (both)
                    [Node {
                        span: _,
                        value:
                            box Ast::UnionType(UnionType {
                                types: lhs_types, ..
                            }),
                    }, Node {
                        span: _,
                        value:
                            box Ast::UnionType(UnionType {
                                types: rhs_types, ..
                            }),
                    }] => {
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
                    [lhs, Node {
                        value:
                            box Ast::UnionType(UnionType {
                                types: rhs_types, ..
                            }),
                        ..
                    }] => {
                        let mut types = rhs_types.clone();

                        types.push(lhs.clone());

                        let ast = Ast::UnionType(UnionType { types, span });

                        let node = Node::new(node.span, ast);

                        (node, ctx)
                    }
                    // Flatten nested union types (lhs)
                    [Node {
                        span: _,
                        value:
                            box Ast::UnionType(UnionType {
                                types: lhs_types, ..
                            }),
                    }, rhs] => {
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
                            .sorted_by(|a, b| {
                                a.value.is_intersection().cmp(&b.value.is_intersection())
                            })
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
            Ast::MacroCall(value) => (value.eval(), ctx),
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

pub type Nodes<'a> = Vec<Node<'a>>;

pub type Bindings<'a> = HashMap<String, Node<'a>>;

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
