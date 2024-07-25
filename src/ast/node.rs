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
    /// Generated nodes have no span
    pub span: Option<Span<'a>>,
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

impl<'a> From<Ast<'a>> for Node<'a> {
    fn from(v: Ast<'a>) -> Self {
        Self::generate(Box::new(v))
    }
}

impl<'a> Node<'a> {
    pub fn from_pair<R>(pair: &pest::iterators::Pair<'a, R>, value: Ast<'a>) -> Self
    where
        R: pest::RuleType,
    {
        Self {
            span: Some(pair.clone().as_span()),
            value: Box::new(value),
        }
    }

    pub fn as_span(&self) -> Span<'a> {
        self.span.unwrap()
    }

    pub fn to_sexp(&self) -> serde_lexpr::Result<serde_lexpr::Value> {
        serde_lexpr::to_value(self)
    }

    pub fn from_span(span: Span<'a>, value: Ast<'a>) -> Self {
        Self {
            span: Some(span),
            value: Box::new(value),
        }
    }

    pub fn generate(value: Box<Ast<'a>>) -> Self {
        Self { span: None, value }
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

    pub fn new(span: Option<Span<'a>>, value: Ast<'a>) -> Self {
        Self {
            span,
            value: Box::new(value),
        }
    }

    pub fn set_span(&mut self, span: Option<Span<'a>>) {
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
        /// Extract the node from the tuple
        pub fn pick_node<T>((node, _ctx): (Node, T)) -> Node {
            node
        }

        // Produce a new node and context
        let result = |node: Ast<'a>, ctx: Context| (self.clone().replace(node), ctx);

        // Reducer
        let red = move |node: &Node<'a>, ctx| node.traverse(ctx, pre, post);

        // Reducer only returns the node, drops the context
        let red_pick_node = move |node: &Node<'a>, ctx| pick_node(node.traverse(ctx, pre, post));

        // Reducer that maps over a list of nodes
        let red_items = move |node: &Nodes<'a>, ctx: Context| {
            node.iter()
                .map(|item| red_pick_node(item, ctx.clone()))
                .collect_vec()
        };

        // Returns a closure that takes a node
        let fn_red_pick_node =
            move |ctx| move |node: &Node<'a>| pick_node(node.traverse(ctx, pre, post));

        let node = self.clone();

        let (node, ctx) = pre(node, ctx);

        let (node, ctx) = match &*node.value {
            Ast::Access(Access { lhs, rhs, is_dot }) => {
                let (lhs, _) = red(lhs, ctx.clone());
                let (rhs, _) = red(rhs, ctx.clone());

                let ast = Ast::Access(Access {
                    lhs,
                    rhs,
                    is_dot: *is_dot,
                });

                (self.clone().replace(ast), ctx)
            }
            Ast::ApplyGeneric(ApplyGeneric {
                receiver: name,
                args,
                span,
            }) => {
                let ast = Ast::ApplyGeneric(ApplyGeneric {
                    receiver: name.clone(),
                    args: red_items(args, ctx.clone()),
                    span: *span,
                });

                result(ast, ctx)
            }

            Ast::Array(inner) => {
                let (inner, _) = red(inner, ctx.clone());
                let ast = Ast::Array(inner);
                result(ast, ctx)
            }

            Ast::Builtin { name, argument } => {
                let (argument, _) = red(argument, ctx.clone());

                let ast = Ast::Builtin {
                    name: name.clone(),
                    argument,
                };

                result(ast, ctx)
            }

            Ast::CondExpr(CondExpr {
                arms,
                else_arm,
                span,
            }) => {
                let arms = arms
                    .iter()
                    .map(|arm| {
                        let mut arm = arm.clone();
                        arm.condition = red_pick_node(&arm.condition, ctx.clone());
                        arm.body = red_pick_node(&arm.body, ctx.clone());
                        arm
                    })
                    .collect_vec();

                let else_arm = red_pick_node(else_arm, ctx.clone());

                let ast = Ast::CondExpr(CondExpr {
                    arms,
                    else_arm,
                    span: *span,
                });

                result(ast, ctx)
            }

            Ast::ExtendsInfixOp { lhs, op, rhs } => {
                let (lhs, _) = red(lhs, ctx.clone());
                let (rhs, _) = red(rhs, ctx.clone());

                let ast = Ast::ExtendsInfixOp {
                    lhs,
                    op: op.clone(),
                    rhs,
                };

                (self.clone().replace(ast), ctx)
            }

            Ast::ExtendsExpr(ExtendsExpr {
                lhs,
                rhs,
                then_branch,
                else_branch,
                span,
            }) => {
                let lhs = red_pick_node(lhs, ctx.clone());
                let rhs = red_pick_node(rhs, ctx.clone());
                let then_branch = red_pick_node(then_branch, ctx.clone());
                let else_branch = red_pick_node(else_branch, ctx.clone());

                let ast = Ast::ExtendsExpr(ExtendsExpr {
                    lhs,
                    rhs,
                    then_branch,
                    else_branch,
                    span: *span,
                });
                result(ast, ctx)
            }

            Ast::ExtendsPrefixOp { op, value } => {
                let value = red_pick_node(value, ctx.clone());

                let ast = Ast::ExtendsPrefixOp {
                    op: op.clone(),
                    value,
                };

                result(ast, ctx)
            }

            Ast::IfExpr(IfExpr {
                condition,
                then_branch,
                else_branch,
                span,
            }) => {
                let (condition, _) = red(condition, ctx.clone());
                let (then_branch, _) = red(then_branch, ctx.clone());
                let else_branch = else_branch.as_ref().map(fn_red_pick_node(ctx.clone()));

                let ast = Ast::IfExpr(IfExpr {
                    condition,
                    then_branch,
                    else_branch,
                    span: *span,
                });

                (self.clone().replace(ast), ctx)
            }

            Ast::ImportStatement {
                import_clause,
                module,
            } => {
                let ast = Ast::ImportStatement {
                    import_clause: import_clause.clone(),
                    module: module.clone(),
                };

                result(ast, ctx)
            }
            Ast::LetExpr(let_expr) => {
                let (body, _) = red(&let_expr.body, ctx.clone());

                let ast = Ast::LetExpr(LetExpr {
                    bindings: let_expr.bindings.clone(),
                    body,
                    span: let_expr.span,
                });

                result(ast, ctx)
            }
            Ast::MappedType(MappedType {
                index,
                iterable,
                remapped_as,
                readonly_mod,
                optional_mod,
                body,
                span,
            }) => {
                let (iterable, _) = red(iterable, ctx.clone());
                let remapped_as = remapped_as.as_ref().map(fn_red_pick_node(ctx.clone()));
                let body = red_pick_node(body, ctx.clone());

                let ast = Ast::MappedType(MappedType {
                    index: index.clone(),
                    iterable,
                    remapped_as,
                    readonly_mod: readonly_mod.clone(),
                    optional_mod: optional_mod.clone(),
                    body,
                    span: *span,
                });

                result(ast, ctx)
            }
            Ast::MatchExpr(MatchExpr {
                value,
                arms,
                else_arm,
                span,
            }) => {
                let (value, _) = red(value, ctx.clone());

                let arms = arms
                    .iter()
                    .map(|arm| {
                        let mut arm = arm.clone();
                        arm.pattern = red_pick_node(&arm.pattern, ctx.clone());
                        arm.body = red_pick_node(&arm.body, ctx.clone());
                        arm
                    })
                    .collect_vec();

                let else_arm = red_pick_node(else_arm, ctx.clone());

                let ast = Ast::MatchExpr(MatchExpr {
                    value,
                    arms,
                    else_arm,
                    span: *span,
                });

                result(ast, ctx)
            }
            Ast::Path(Path { segments, span }) => {
                let segments = segments
                    .iter()
                    .map(|node| red_pick_node(node, ctx.clone()))
                    .collect_vec();

                let ast = Ast::Path(Path {
                    segments,
                    span: *span,
                });

                result(ast, ctx)
            }
            Ast::TypeLiteral(ty) => {
                let properties = ty
                    .properties
                    .iter()
                    .map(|prop| {
                        let mut prop = prop.clone();
                        prop.value = red_pick_node(&prop.value, ctx.clone());
                        prop
                    })
                    .collect_vec();

                let ast = Ast::TypeLiteral(ObjectLiteral {
                    properties,
                    span: ty.span,
                });

                result(ast, ctx)
            }
            Ast::Program(statements) => {
                let mut ctx = ctx.clone();
                let mut statements = statements.clone();

                for statement in &mut statements {
                    (*statement, ctx) = red(statement, ctx.clone());
                }

                let ast = Ast::Program(statements);

                result(ast, ctx)
            }
            Ast::Statement(inner) => {
                // Statement MAY propagate context to siblings
                let (inner, ctx) = red(inner, ctx.clone());
                let ast = Ast::Statement(inner);
                result(ast, ctx)
            }
            node @ Ast::TemplateString(_) => result(node.clone(), ctx.clone()),
            Ast::Tuple(Tuple { items, span }) => {
                let items = items
                    .iter()
                    .map(|item| red_pick_node(item, ctx.clone()))
                    .collect_vec();

                let ast = Ast::Tuple(Tuple { items, span: *span });

                result(ast, ctx)
            }
            Ast::TypeAlias {
                export,
                name,
                params,
                body,
            } => {
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

                let ast = Ast::TypeAlias {
                    export: *export,
                    name: name.clone(),
                    params,
                    body,
                };

                result(ast, ctx)
            }

            Ast::UnionType { types } => {
                let types = types
                    .iter()
                    .map(|ty| red_pick_node(ty, ctx.clone()))
                    .collect_vec();

                let ast = Ast::UnionType { types };

                (self.clone().replace(ast), ctx)
            }

            Ast::IntersectionType(IntersectionType { types }) => {
                let types = types
                    .iter()
                    .map(|ty| red_pick_node(ty, ctx.clone()))
                    .collect_vec();

                let ast = Ast::IntersectionType(IntersectionType { types });

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

        let (tree, _) = self.traverse(
            bindings,
            &identity,
            &|node, ctx| match node.value.as_ref() {
                Ast::IfExpr(if_expr) => (if_expr.simplify(), ctx),
                Ast::MatchExpr(match_expr) => (match_expr.simplify(), ctx),
                Ast::CondExpr(cond_expr) => (cond_expr.simplify(), ctx),
                Ast::LetExpr(let_expr) => (let_expr.simplify(), ctx),
                Ast::Path(path) => (path.simplify(node.span.unwrap()), ctx),
                Ast::UnionType { types } => match types.as_slice() {
                    // Flatten nested union types (both)
                    [Node {
                        span: _,
                        value: box Ast::UnionType { types: lhs_types },
                    }, Node {
                        span: _,
                        value: box Ast::UnionType { types: rhs_types },
                    }] => {
                        let types = lhs_types
                            .clone()
                            .into_iter()
                            .chain(rhs_types.clone())
                            .collect();
                        let ast = Ast::UnionType { types };
                        let node = Node::new(node.span, ast);

                        (node, ctx)
                    }
                    // Flatten nested union types (rhs)
                    [lhs, Node {
                        span: _,
                        value: box Ast::UnionType { types: rhs_types },
                    }] => {
                        let mut types = rhs_types.clone();
                        types.push(lhs.clone());
                        let ast = Ast::UnionType { types };
                        let node = Node::new(node.span, ast);

                        (node, ctx)
                    }
                    // Flatten nested union types (lhs)
                    [Node {
                        span: _,
                        value: box Ast::UnionType { types: lhs_types },
                    }, rhs] => {
                        let mut types = lhs_types.clone();
                        types.push(rhs.clone());
                        let ast = Ast::UnionType { types };
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

                        let node = Node::new(node.span, Ast::UnionType { types });

                        (node, ctx)
                    }
                },
                _ => (node, ctx),
            },
        );
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

pub type Bindings<'a> = HashMap<Identifier, Node<'a>>;

impl<'a> Default for Node<'a> {
    fn default() -> Self {
        Node {
            span: None,
            value: Box::new(Ast::NoOp),
        }
    }
}

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
