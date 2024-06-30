use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use pest::Span;
use pretty::RcDoc;

use crate::{
    parser::ParserError,
    pretty::{parens, string_literal},
    typescript,
};

pub(crate) mod macros;

pub(crate) trait PrettySexpr {
    fn to_sexpr(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.pretty_sexpr().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }

    fn pretty_sexpr(&self) -> RcDoc;

    fn sexpr(items: Vec<RcDoc>) -> RcDoc {
        let sep = RcDoc::line();

        RcDoc::text("(")
            .append(RcDoc::intersperse(items, sep).nest(4))
            .append(RcDoc::text(")"))
            .group()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Node<'a, T> {
    /// Generated nodes have no span
    pub span: Option<Span<'a>>,
    pub value: Box<T>,
}

impl<'a, T> From<T> for Node<'a, T> {
    fn from(v: T) -> Self {
        Self::generate(Box::new(v))
    }
}

impl<'a, T> Node<'a, T> {
    pub fn from_pair<R>(pair: &pest::iterators::Pair<'a, R>, value: T) -> Self
    where
        R: pest::RuleType,
    {
        Self {
            span: Some(pair.clone().as_span()),
            value: Box::new(value),
        }
    }

    pub fn from_span(span: Span<'a>, value: T) -> Self {
        Self {
            span: Some(span),
            value: Box::new(value),
        }
    }

    pub fn generate(value: Box<T>) -> Self {
        Self { span: None, value }
    }

    /// Transform the value of the node with a function that takes a reference to the value
    pub fn map(&self, f: impl Fn(&T) -> T) -> Self {
        Self {
            span: self.span,
            value: Box::new(f(&self.value)),
        }
    }

    /// Replace the value of the node with a new value, creating a new node
    /// with the same span.
    pub fn replace(self, value: T) -> Self {
        Self {
            span: self.span,
            value: Box::new(value),
        }
    }

    pub(crate) fn new(span: Option<Span<'a>>, value: T) -> Self {
        Self {
            span,
            value: Box::new(value),
        }
    }
}

pub(crate) type AstNode<'a> = Node<'a, Ast<'a>>;

pub(crate) type AstNodes<'a> = Vec<Node<'a, Ast<'a>>>;

type Bindings<'a> = HashMap<Identifier, AstNode<'a>>;

impl<'a> AstNode<'a> {
    pub fn traverse<Context, Pre, Post>(
        &self,
        ctx: Context,
        pre: &Pre,
        post: &Post,
    ) -> (Self, Context)
    where
        Context: Clone,
        Pre: Fn((Self, Context)) -> (Self, Context),
        Post: Fn((Self, Context)) -> (Self, Context),
    {
        /// Extract the node from the tuple
        fn pick_node<T>((node, _ctx): (AstNode, T)) -> AstNode {
            node
        }

        // Produce a new node and context
        let result = |node: Ast<'a>, ctx: Context| (self.clone().replace(node), ctx);

        // Reducer
        let red = move |node: &AstNode<'a>, ctx| node.traverse(ctx, pre, post);

        // Reducer only returns the node, drops the context
        let red_pick_node = move |node: &AstNode<'a>, ctx| pick_node(node.traverse(ctx, pre, post));

        // Reducer that maps over a list of nodes
        let red_items = move |node: &AstNodes<'a>, ctx: Context| {
            node.iter()
                .map(|item| red_pick_node(item, ctx.clone()))
                .collect_vec()
        };

        // Returns a closure that takes a node
        let fn_red_pick_node =
            move |ctx| move |node: &AstNode<'a>| pick_node(node.traverse(ctx, pre, post));

        let (node, ctx) = pre((self.clone(), ctx));

        let (node, ctx) = match &*self.value {
            Ast::Access { lhs, rhs, is_dot } => {
                let (lhs, _) = red(lhs, ctx.clone());
                let (rhs, _) = red(rhs, ctx.clone());

                let ast = Ast::Access {
                    lhs,
                    rhs,
                    is_dot: *is_dot,
                };

                (self.clone().replace(ast), ctx)
            }
            Ast::Application(Application { name, args }) => {
                let ast = Ast::Application(Application {
                    name: name.clone(),
                    args: red_items(args, ctx.clone()),
                });

                result(ast, ctx)
            }

            Ast::Array(inner) => {
                let (inner, _) = red(inner, ctx.clone());
                let ast = Ast::Array(inner);
                result(ast, ctx)
            }

            Ast::InfixOp { lhs, op, rhs } => {
                let ast = Ast::InfixOp {
                    lhs: red_pick_node(lhs, ctx.clone()),
                    op: op.clone(),
                    rhs: red_pick_node(rhs, ctx.clone()),
                };

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

            Ast::CondExpr(cond_expr::Expr { arms, else_arm }) => {
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

                let ast = Ast::CondExpr(cond_expr::Expr { arms, else_arm });

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

            Ast::ExtendsExpr(_) => (self.clone(), ctx.clone()),

            Ast::ExtendsPrefixOp { op, value } => {
                let value = red_pick_node(value, ctx.clone());

                let ast = Ast::ExtendsPrefixOp {
                    op: op.clone(),
                    value,
                };

                result(ast, ctx)
            }

            Ast::IfExpr(if_expr::Expr {
                condition,
                then_branch,
                else_branch,
            }) => {
                let (condition, _) = red(condition, ctx.clone());
                let (then_branch, _) = red(then_branch, ctx.clone());
                let else_branch = else_branch.as_ref().map(fn_red_pick_node(ctx.clone()));

                let ast = Ast::IfExpr(if_expr::Expr {
                    condition,
                    then_branch,
                    else_branch,
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
                });

                result(ast, ctx)
            }
            Ast::MatchExpr(match_expr::Expr {
                value,
                arms,
                else_arm,
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

                let ast = Ast::MatchExpr(match_expr::Expr {
                    value,
                    arms,
                    else_arm,
                });

                result(ast, ctx)
            }
            Ast::NamespaceAccess(access) => {
                let lhs = red_pick_node(&access.lhs, ctx.clone());
                let rhs = red_pick_node(&access.rhs, ctx.clone());

                let ast = Ast::NamespaceAccess(NamespaceAccess { lhs, rhs });

                result(ast, ctx)
            }
            Ast::ObjectLiteral(object) => {
                let properties = object
                    .properties
                    .iter()
                    .map(|prop| {
                        let mut prop = prop.clone();
                        prop.value = red_pick_node(&prop.value, ctx.clone());
                        prop
                    })
                    .collect_vec();

                let ast = Ast::ObjectLiteral(ObjectLiteral { properties });

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
            Ast::Tuple(Tuple { items }) => {
                let items = items
                    .iter()
                    .map(|item| red_pick_node(item, ctx.clone()))
                    .collect_vec();

                let ast = Ast::Tuple(Tuple { items });

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
                             name: param_name,
                             constraint,
                             default,
                             rest,
                         }| {
                            let constraint = constraint.as_ref().map(fn_red_pick_node(ctx.clone()));
                            let default = default.as_ref().map(fn_red_pick_node(ctx.clone()));

                            TypeParameter {
                                name: param_name.clone(),
                                constraint,
                                default,
                                rest: *rest,
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
            _ => (node, ctx),
        };

        let (node, acc) = post((node, ctx));

        (node, acc)
    }

    pub fn simplify(&self) -> Self {
        let bindings: Bindings = Default::default();
        let (tree, _) = self.traverse(
            bindings,
            &|(node, ctx)| {
                // dbg!(&node);
                (node, ctx)
            },
            &|(node, ctx)| match &*node.value {
                Ast::IfExpr(if_expr) => (if_expr.simplify(), ctx),
                Ast::MatchExpr(match_expr) => (match_expr.simplify(), ctx),
                Ast::CondExpr(cond_expr) => (cond_expr.simplify(), ctx),
                _ast => (node, ctx),
            },
        );
        tree
    }
}

#[cfg(test)]
mod tests {
    use crate::{parser::Rule, test_support::parse};

    use super::*;

    #[test]
    #[ignore]
    fn traverse() {
        let tree = parse!(
            Rule::program,
            r#"
            type A as 1

            type B as
                if a <: b then
                    c
                else
                    d
                end

            type C(a) as
                match a do
                    number => a,
                    string => b,
                    else => c
                end

            type D(a) as
                cond do
                    a <: 1 => a,
                    else => b
                end
            "#
        );
        let bindings: Bindings = Default::default();
        let (tree, _) = tree.traverse(
            bindings,
            &|(node, ctx)| {
                // dbg!(&node);
                (node, ctx)
            },
            &|(node, ctx)| match &*node.value {
                Ast::IfExpr(if_expr) => (if_expr.simplify(), ctx),
                Ast::MatchExpr(match_expr) => (match_expr.simplify(), ctx),
                Ast::CondExpr(cond_expr) => (cond_expr.simplify(), ctx),
                _ast => (node, ctx),
            },
        );
        // dbg!(&tree);
        assert_eq!("", tree.to_sexpr(80));
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct NamespaceAccess<'a> {
    pub lhs: AstNode<'a>,
    pub rhs: AstNode<'a>,
}

impl NamespaceAccess<'_> {
    fn pretty_sexpr(&self) -> RcDoc<()> {
        Ast::sexpr(vec![
            RcDoc::text("::"),
            self.lhs.pretty_sexpr(),
            self.rhs.pretty_sexpr(),
        ])
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExtendsExpr<'a> {
    pub lhs: AstNode<'a>,
    pub rhs: AstNode<'a>,
    pub then_branch: AstNode<'a>,
    pub else_branch: AstNode<'a>,
}

impl<'a> ExtendsExpr<'a> {
    pub fn new(
        lhs: AstNode<'a>,
        rhs: AstNode<'a>,
        then_branch: AstNode<'a>,
        else_branch: AstNode<'a>,
    ) -> Self {
        if !lhs.value.is_typescript_feature() {
            dbg!(&lhs);
            unreachable!("value must be desugared before this point");
        }
        if !rhs.value.is_typescript_feature() {
            dbg!(&rhs);
            unreachable!("value must be desugared before this point");
        }
        if !then_branch.value.is_typescript_feature() {
            dbg!(&then_branch);
            unreachable!("value must be desugared before this point");
        }
        if !else_branch.value.is_typescript_feature() {
            dbg!(&else_branch);
            unreachable!("value must be desugared before this point");
        }
        Self {
            lhs,
            rhs,
            then_branch,
            else_branch,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LetExpr<'a> {
    pub bindings: HashMap<Identifier, AstNode<'a>>,
    pub body: AstNode<'a>,
}

impl<'a> LetExpr<'a> {
    fn resolve_bindings(&self) -> AstNode<'a> {
        dbg!(self.to_sexpr(80));
        // let bindings = self.bindings.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
        // let f = resolve_let_bindings(&bindings);
        //
        // self.body.transform(&f)
        todo!()
    }
}

impl PrettySexpr for LetExpr<'_> {
    fn pretty_sexpr(&self) -> RcDoc<()> {
        let mut bindings = vec![];

        for (ident, value) in &self.bindings {
            bindings.push(ident.pretty_sexpr().append(":"));
            bindings.push(value.pretty_sexpr());
        }

        Ast::sexpr(vec![
            RcDoc::text("let"),
            Ast::sexpr(bindings),
            self.body.pretty_sexpr(),
        ])
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Tuple<'a> {
    pub items: AstNodes<'a>,
}

impl PrettySexpr for Tuple<'_> {
    fn pretty_sexpr(&self) -> RcDoc<()> {
        Ast::sexpr(self.items.iter().map(|item| item.pretty_sexpr()).collect())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Application<'a> {
    pub name: String,
    pub args: AstNodes<'a>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MappedType<'a> {
    pub index: String,
    pub iterable: AstNode<'a>,
    pub remapped_as: Option<AstNode<'a>>,
    pub readonly_mod: Option<MappingModifier>,
    pub optional_mod: Option<MappingModifier>,
    pub body: AstNode<'a>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjectLiteral<'a> {
    pub properties: Vec<ObjectProperty<'a>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Ast<'a> {
    Access {
        lhs: AstNode<'a>,
        rhs: AstNode<'a>,
        is_dot: bool,
    },
    Any,
    Application(Application<'a>),
    Array(AstNode<'a>),
    InfixOp {
        lhs: AstNode<'a>,
        op: Op,
        rhs: AstNode<'a>,
    },
    Builtin {
        name: BuiltInKeyword,
        argument: AstNode<'a>,
    },
    CondExpr(cond_expr::Expr<'a>),
    ExtendsInfixOp {
        lhs: AstNode<'a>,
        op: InfixOp,
        rhs: AstNode<'a>,
    },
    ExtendsExpr(ExtendsExpr<'a>),
    ExtendsPrefixOp {
        op: PrefixOp,
        value: AstNode<'a>,
    },
    False,
    Ident(String),
    IfExpr(if_expr::Expr<'a>),
    ImportStatement {
        import_clause: ImportClause,
        module: String,
    },
    LetExpr(LetExpr<'a>),
    MappedType(MappedType<'a>),
    MatchExpr(match_expr::Expr<'a>),
    NamespaceAccess(NamespaceAccess<'a>),
    Never,
    NoOp,
    Null,
    Number(String),
    ObjectLiteral(ObjectLiteral<'a>),
    Primitive(PrimitiveType),
    Program(AstNodes<'a>),
    Statement(AstNode<'a>),
    String(String),
    TemplateString(String),
    True,
    Tuple(Tuple<'a>),
    TypeAlias {
        export: bool,
        name: String,
        params: Vec<TypeParameter<'a>>,
        body: AstNode<'a>,
    },
    Undefined,
    Unknown,
}

impl<'a> From<if_expr::Expr<'a>> for Ast<'a> {
    fn from(v: if_expr::Expr<'a>) -> Self {
        Self::IfExpr(v)
    }
}

impl<'a> From<match_expr::Expr<'a>> for Ast<'a> {
    fn from(v: match_expr::Expr<'a>) -> Self {
        Self::MatchExpr(v)
    }
}

impl<'a> From<cond_expr::Expr<'a>> for Ast<'a> {
    fn from(v: cond_expr::Expr<'a>) -> Self {
        Self::CondExpr(v)
    }
}

impl<'a> Ast<'a> {
    /// Operators that the `not` prefix operator can be applied to.
    pub fn is_compatible_with_not_prefix_op(&self) -> bool {
        match self {
            Ast::ExtendsPrefixOp { op, .. } if op.is_not() => true,
            Ast::ExtendsInfixOp { .. } => true,
            _ => false,
        }
    }
    /// Anything that returns true is a feature that has a direct equivalent in TypeScript.
    /// Anything that's false is a feature that needs to be desugared.
    pub fn is_typescript_feature(&self) -> bool {
        match self {
            Ast::Access { .. } => true,
            Ast::Any => true,
            Ast::Application(Application { name: _, args: _ }) => true,
            Ast::Array(_) => true,
            Ast::InfixOp { .. } => true,
            Ast::Builtin { .. } => true,
            Ast::CondExpr(cond_expr::Expr { .. }) => false,
            Ast::ExtendsInfixOp { .. } => false,
            Ast::ExtendsExpr(_) => true,
            Ast::ExtendsPrefixOp { .. } => false,
            Ast::False => true,
            Ast::Ident(_) => true,
            Ast::IfExpr { .. } => false,
            Ast::ImportStatement { .. } => true,
            Ast::LetExpr(LetExpr { .. }) => false,
            Ast::MappedType(MappedType { .. }) => true,
            Ast::MatchExpr(match_expr::Expr { .. }) => false,
            Ast::NamespaceAccess(_) => true,
            Ast::Never => true,
            Ast::NoOp => false,
            Ast::Null => true,
            Ast::Number(_) => true,
            Ast::ObjectLiteral(ObjectLiteral { properties: _ }) => true,
            Ast::Primitive(_) => true,
            Ast::Program(_) => true,
            Ast::Statement(_) => true,
            Ast::String(_) => true,
            Ast::TemplateString(_) => true,
            Ast::True => true,
            Ast::Tuple(Tuple { items: _ }) => true,
            Ast::TypeAlias { .. } => false,
            Ast::Undefined => true,
            Ast::Unknown => true,
        }
    }

    pub fn as_ident(&self) -> Option<&String> {
        if let Self::Ident(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the node is [`ExtendsPrefixOp`].
    ///
    /// [`ExtendsPrefixOp`]: ASTNode<'a>::ExtendsPrefixOp
    #[must_use]
    pub fn is_extends_prefix_op(&self) -> bool {
        matches!(self, Self::ExtendsPrefixOp { .. })
    }

    /// Returns `true` if the ast is [`ExtendsInfixOp`].
    ///
    /// [`ExtendsInfixOp`]: Ast::ExtendsInfixOp
    #[must_use]
    pub fn is_extends_infix_op(&self) -> bool {
        matches!(self, Self::ExtendsInfixOp { .. })
    }

    /// Returns `true` if the ast is [`InfixOp`].
    ///
    /// [`InfixOp`]: Ast::InfixOp
    #[must_use]
    pub fn is_infix_op(&self) -> bool {
        matches!(self, Self::InfixOp { .. })
    }
}

impl<'a> From<ExtendsExpr<'a>> for Ast<'a> {
    fn from(v: ExtendsExpr<'a>) -> Self {
        Ast::ExtendsExpr(v)
    }
}

impl<'a, T> PrettySexpr for Node<'a, T>
where
    T: PrettySexpr,
{
    fn pretty_sexpr(&self) -> RcDoc {
        self.value.pretty_sexpr()
    }
}

impl<'a> PrettySexpr for Ast<'a> {
    fn pretty_sexpr(&self) -> RcDoc {
        match self {
            Ast::Access { lhs, rhs, is_dot } => {
                let op = if *is_dot { "." } else { ".[]" };

                Ast::sexpr(vec![
                    RcDoc::text(op),
                    lhs.pretty_sexpr(),
                    rhs.pretty_sexpr(),
                ])
            }
            Ast::Any => RcDoc::text("any"),
            Ast::Application(Application { name, args }) => Ast::sexpr(
                vec![RcDoc::text(name)]
                    .into_iter()
                    .chain(args.iter().map(|n| n.pretty_sexpr()))
                    .collect_vec(),
            ),
            Ast::Array(value) => Ast::sexpr(vec![RcDoc::text("[]"), value.pretty_sexpr()]),
            Ast::InfixOp { lhs, op, rhs } => Ast::sexpr(vec![
                RcDoc::text(match op {
                    Op::Union => "&",
                    Op::Intersection => "|",
                }),
                lhs.pretty_sexpr(),
                rhs.pretty_sexpr(),
            ]),

            Ast::Builtin { name, argument } => {
                let name = match name {
                    BuiltInKeyword::Keyof => "keyof",
                };

                Ast::sexpr(vec![RcDoc::text(name), argument.pretty_sexpr()])
            }
            Ast::CondExpr(cond_expr) => cond_expr.pretty_sexpr(),
            Ast::ExtendsInfixOp { lhs, op, rhs } => {
                let op = match op {
                    InfixOp::Extends => "<:",
                    InfixOp::NotExtends => "</",
                    InfixOp::Equals => "==",
                    InfixOp::NotEquals => "!=",
                    InfixOp::StrictEquals => "===",
                    InfixOp::StrictNotEquals => "!==",
                    InfixOp::And => "and",
                    InfixOp::Or => "or",
                };

                Ast::sexpr(vec![
                    RcDoc::text(op),
                    lhs.pretty_sexpr(),
                    rhs.pretty_sexpr(),
                ])
            }
            Ast::ExtendsExpr(ExtendsExpr {
                lhs,
                rhs,
                then_branch,
                else_branch,
            }) => Ast::sexpr(vec![
                RcDoc::text("extends"),
                lhs.pretty_sexpr(),
                rhs.pretty_sexpr(),
                then_branch.pretty_sexpr(),
                else_branch.pretty_sexpr(),
            ]),
            Ast::ExtendsPrefixOp { op, value } => {
                let op = match op {
                    PrefixOp::Not => "not",
                    PrefixOp::Infer => "infer",
                };

                Ast::sexpr(vec![RcDoc::text(op), value.pretty_sexpr()])
            }
            Ast::False => todo!(),
            Ast::Ident(ident) => RcDoc::text(ident),
            Ast::IfExpr(if_expr) => if_expr.pretty_sexpr(),
            Ast::ImportStatement { .. } => todo!(),
            Ast::LetExpr(let_expr) => let_expr.pretty_sexpr(),
            Ast::MappedType(MappedType { .. }) => todo!(),
            Ast::MatchExpr(match_expr::Expr { .. }) => todo!(),
            Ast::NamespaceAccess(namespace_access) => namespace_access.pretty_sexpr(),
            Ast::Never => todo!(),
            Ast::NoOp => todo!(),
            Ast::Null => todo!(),
            Ast::Number(number) => RcDoc::text(number),
            Ast::ObjectLiteral(ObjectLiteral { properties: _ }) => todo!(),
            Ast::Primitive(primitive) => primitive.pretty_sexpr(),
            Ast::Program(statements) => {
                let mut vec = vec![RcDoc::text("program")];

                vec.extend(statements.iter().map(|s| s.pretty_sexpr()));

                Ast::sexpr(vec)
            }
            Ast::Statement(inner) => inner.pretty_sexpr(),
            Ast::String(_) => todo!(),
            Ast::TemplateString(_) => todo!(),
            Ast::True => todo!(),
            Ast::Tuple(tuple) => tuple.pretty_sexpr(),
            Ast::TypeAlias {
                export,
                name,
                params,
                body,
            } => {
                let mut vec = vec![RcDoc::text("type"), RcDoc::text(name)];

                if *export {
                    vec.push(RcDoc::text(":export"));
                }

                if !params.is_empty() {
                    vec.push(Ast::sexpr(
                        params.iter().map(|p| p.pretty_sexpr()).collect(),
                    ));
                }

                vec.push(RcDoc::text("as:"));

                vec.push(body.pretty_sexpr());

                Ast::sexpr(vec)
            }
            Ast::Undefined => todo!(),
            Ast::Unknown => todo!(),
        }
    }
}

#[cfg(test)]
mod simplify_tests {

    use super::*;
    use crate::{
        ast::{
            self,
            Ast::{self, *},
        },
        parser::Rule::expr,
        pest::Parser,
        test_support::parse,
    };
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn simplify_basic() {
        assert_eq!(
            parse!(expr, "if a <: b then c else d end")
                .simplify()
                .to_sexpr(80),
            "(extends a b c d)"
        )
    }

    #[test]
    fn simplify_not() {
        assert_eq!(
            parse!(
                expr,
                r#"
                if a <: b then
                    d
                else
                    c
                end
                "#
            )
            .simplify()
            .to_sexpr(80),
            parse!(
                expr,
                r#"
                if not (a <: b) then
                    c
                else
                    d
                end
                "#
            )
            .simplify()
            .to_sexpr(80),
        )
    }

    #[test]
    fn simplify_and() {
        assert_eq!(
            parse!(
                expr,
                r#"
                if a <: b and c <: d then
                    e
                else
                    f
                end
                "#
            )
            .simplify()
            .to_sexpr(80),
            parse!(
                expr,
                r#"
                if a <: b then
                    if c <: d then
                        e
                    else
                        f
                    end
                else
                    f
                end
                "#
            )
            .simplify()
            .to_sexpr(80),
        )
    }

    #[test]
    fn simplify_or() {
        assert_eq!(
            parse!(
                expr,
                r#"
                if a <: b or c <: d
                then e
                else f
                end
                "#
            )
            .simplify()
            .to_sexpr(80),
            parse!(
                expr,
                r#"
                if a <: b then e
                else
                    if c <: d then e
                    else f
                    end
                end
                "#
            )
            .simplify()
            .to_sexpr(80),
        )
    }

    #[test]
    fn simplify_match_expr() {
        assert_eq!(
            parse!(
                expr,
                r#"
                match A do
                    number => 1,
                    string => 2,
                    else => 3
                end
                "#
            )
            .simplify()
            .to_sexpr(80),
            parse!(
                expr,
                r#"
                if A <: number then 1
                else
                    if A <: string then 2
                    else 3
                    end
                end
                "#
            )
            .simplify()
            .to_sexpr(80)
        )
    }
}

impl<'a, T> typescript::Pretty for Node<'a, T>
where
    T: typescript::Pretty,
{
    fn to_ts(&self) -> pretty::RcDoc<()> {
        self.value.to_ts()
    }
}

impl<'a> typescript::Pretty for Ast<'a> {
    fn to_ts(&self) -> RcDoc<()> {
        match self {
            Ast::Program(stmnts) => {
                let mut doc = RcDoc::nil();
                for stmnt in stmnts {
                    doc = doc
                        .append(stmnt.to_ts())
                        .append(RcDoc::hardline())
                        .append(RcDoc::hardline());
                }
                doc
            }
            Ast::TypeAlias {
                export,
                name,
                params,
                body,
            } => {
                let body = (*body).to_ts();

                let doc = if *export {
                    RcDoc::text("export").append(RcDoc::space())
                } else {
                    RcDoc::nil()
                };

                let params_doc = match params {
                    list if list.is_empty() => RcDoc::nil(),
                    list => {
                        let seperator = RcDoc::text(",").append(RcDoc::line());

                        let body = RcDoc::intersperse(
                            list.iter().map(|param| param.to_ts().group()),
                            seperator,
                        );

                        RcDoc::text("<")
                            .append(RcDoc::line_().append(body).append(RcDoc::line_()).nest(4))
                            .append(RcDoc::text(">"))
                            .group()
                    }
                };

                doc.append("type")
                    .append(RcDoc::space())
                    .append(name)
                    .append(params_doc)
                    .append(RcDoc::space())
                    .append("=")
                    .append(RcDoc::line().append(body).nest(4))
                    .group()
            }
            Ast::Ident(ident) => RcDoc::text(ident),
            Ast::Number(number) => RcDoc::text(number),
            Ast::Primitive(primitive) => RcDoc::text(match primitive {
                PrimitiveType::Boolean => "boolean",
                PrimitiveType::Number => "number",
                PrimitiveType::String => "string",
            }),
            Ast::String(string) => string_literal(string),
            Ast::TemplateString(string) => RcDoc::text(string),
            Ast::IfExpr(if_expr::Expr { .. }) => {
                unreachable!("IfExpr should be desugared before this point");
            }
            Ast::Access {
                lhs,
                rhs,
                is_dot: true,
            } => {
                let rhs = rhs
                    .value
                    .as_ident()
                    .expect("rhs of dot access should be an ident");

                lhs.to_ts()
                    .append(RcDoc::text("["))
                    .append(string_literal(rhs))
                    .append(RcDoc::text("]"))
                    .group()
            }
            Ast::Access { lhs, rhs, .. } => lhs
                .to_ts()
                .append(RcDoc::text("["))
                .append(rhs.to_ts())
                .append(RcDoc::text("]"))
                .group(),
            Ast::ObjectLiteral(ObjectLiteral { properties: props }) => {
                let sep = RcDoc::text(",").append(RcDoc::space());

                let props = RcDoc::intersperse(props.iter().map(|prop| prop.to_ts()), sep);

                RcDoc::text("{").append(props).append(RcDoc::text("}"))
            }
            Ast::Application(Application {
                name: ident,
                args: params,
            }) => {
                let sep = RcDoc::text(",").append(RcDoc::space());

                let params = RcDoc::intersperse(params.iter().map(|param| param.to_ts()), sep);

                RcDoc::text(ident).append(RcDoc::text("<").append(params).append(RcDoc::text(">")))
            }
            Ast::Tuple(Tuple { items }) => {
                let sep = RcDoc::text(",").append(RcDoc::space());

                let items = RcDoc::intersperse(items.iter().map(|item| item.to_ts()), sep);

                RcDoc::text("[").append(items).append(RcDoc::text("]"))
            }
            Ast::Array(node) => {
                let doc = if node.value.is_infix_op() {
                    parens(node.to_ts())
                } else {
                    node.to_ts()
                };

                doc.append(RcDoc::text("[]"))
            }
            Ast::Null => RcDoc::text("null"),
            Ast::Undefined => RcDoc::text("undefined"),
            Ast::Never => RcDoc::text("never"),
            Ast::Any => RcDoc::text("any"),
            Ast::Unknown => RcDoc::text("unknown"),
            Ast::True => RcDoc::text("true"),
            Ast::False => RcDoc::text("false"),

            Ast::InfixOp { lhs, op, rhs } => {
                fn fmt<'a>(v: &'a AstNode<'a>) -> RcDoc<'a, ()> {
                    match &*v.value {
                        Ast::InfixOp { .. } => parens(v.to_ts()),
                        _ => v.to_ts(),
                    }
                }

                let lhs = fmt(lhs);
                let rhs = fmt(rhs);

                let op = match op {
                    Op::Union => RcDoc::text("|"),
                    Op::Intersection => RcDoc::text("&"),
                };

                RcDoc::nil()
                    .append(lhs)
                    .append(RcDoc::space())
                    .append(op)
                    .append(RcDoc::space())
                    .append(rhs)
            }

            Ast::Builtin { name, argument } => name.to_ts().append(" ").append(argument.to_ts()),

            Ast::ExtendsExpr(ExtendsExpr {
                lhs,
                rhs,
                then_branch: then,
                else_branch: els,
            }) => {
                let condition_doc = lhs
                    .to_ts()
                    .append(RcDoc::space())
                    .append("extends")
                    .append(RcDoc::space())
                    .append(rhs.to_ts());

                let then_doc = RcDoc::line()
                    .append("?")
                    .append(RcDoc::space())
                    .append(then.to_ts())
                    .nest(4);

                let else_doc = RcDoc::line()
                    .append(":")
                    .append(RcDoc::space())
                    .append(els.to_ts())
                    .nest(4);

                condition_doc.append(then_doc).append(else_doc)
            }
            Ast::ExtendsInfixOp {
                lhs,
                op: InfixOp::Extends,
                rhs,
            } => lhs
                .to_ts()
                .append(RcDoc::space())
                .append("extends")
                .append(RcDoc::space())
                .append(rhs.to_ts())
                .group(),
            Ast::Statement(stmnt) => stmnt.to_ts().append(RcDoc::text(";")),
            Ast::MappedType(MappedType {
                index: key,
                iterable,
                remapped_as,
                readonly_mod,
                optional_mod,
                body,
            }) => {
                let remapped_as_doc = match remapped_as {
                    Some(remapped_as) => RcDoc::space()
                        .append("as")
                        .append(RcDoc::space())
                        .append(remapped_as.to_ts()),
                    None => RcDoc::nil(),
                };

                let lhs_doc = RcDoc::nil()
                    .append(key)
                    .append(RcDoc::space())
                    .append("in")
                    .append(RcDoc::space())
                    .append(iterable.to_ts())
                    .append(remapped_as_doc)
                    .group();

                let rhs_doc = body.to_ts();

                let rhs_doc = RcDoc::line().append(rhs_doc).nest(4).group();

                let readonly_doc = match readonly_mod {
                    Some(MappingModifier::Add) => RcDoc::text("readonly"),
                    Some(MappingModifier::Remove) => RcDoc::text("-readonly"),
                    None => RcDoc::nil(),
                };

                let optional_doc = match optional_mod {
                    Some(MappingModifier::Add) => RcDoc::text("?"),
                    Some(MappingModifier::Remove) => RcDoc::text("-?"),
                    None => RcDoc::nil(),
                };

                let inner_doc = RcDoc::line()
                    .append(readonly_doc)
                    .append("[")
                    .append(lhs_doc)
                    .append("]")
                    .append(optional_doc)
                    .append(":")
                    .append(rhs_doc)
                    .append(RcDoc::line())
                    .nest(4)
                    .group();

                RcDoc::nil()
                    .append("{")
                    .append(inner_doc)
                    .append("}")
                    .group()
            }
            Ast::LetExpr(LetExpr { .. }) => {
                unreachable!("LetExpr should be desugared before this point")
            }
            Ast::ImportStatement {
                import_clause,
                module,
            } => {
                let import_clause = import_clause.to_ts();

                RcDoc::text("import type")
                    .append(RcDoc::space())
                    .append(import_clause)
                    .append(RcDoc::space())
                    .append("from")
                    .append(RcDoc::space())
                    .append(string_literal(module))
            }
            Ast::NamespaceAccess(NamespaceAccess { lhs, rhs }) => {
                lhs.to_ts().append(".").append(rhs.to_ts())
            }
            Ast::NoOp => todo!(),
            node @ (Ast::ExtendsPrefixOp { .. }
            | Ast::MatchExpr(match_expr::Expr { .. })
            | Ast::CondExpr(cond_expr::Expr { .. })
            | Ast::ExtendsInfixOp { .. }) => {
                unreachable!(
                    "ASTNode<'a> should be desugared before this point {:#?}",
                    node
                )
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ImportClause {
    Named(Vec<ImportSpecifier>),
    Namespace { alias: Identifier },
}

impl typescript::Pretty for ImportClause {
    fn to_ts(&self) -> RcDoc<()> {
        match self {
            ImportClause::Named(specifiers) => {
                let sep = RcDoc::text(",").append(RcDoc::line());

                let specifiers =
                    RcDoc::intersperse(specifiers.iter().map(typescript::Pretty::to_ts), sep);

                RcDoc::text("{")
                    .append(
                        RcDoc::nil()
                            .append(RcDoc::line())
                            .append(specifiers)
                            .append(RcDoc::line())
                            .nest(4),
                    )
                    .append(RcDoc::text("}"))
                    .group()
            }
            ImportClause::Namespace { alias } => RcDoc::text("*")
                .append(RcDoc::space())
                .append("as")
                .append(RcDoc::space())
                .append(alias.to_ts()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ImportSpecifier {
    pub module_export_name: Identifier,
    pub alias: Option<Identifier>,
}

impl typescript::Pretty for ImportSpecifier {
    fn to_ts(&self) -> RcDoc<()> {
        let alias_doc = match &self.alias {
            Some(alias) => RcDoc::space()
                .append("as")
                .append(RcDoc::space())
                .append(alias.to_ts()),

            None => RcDoc::nil(),
        };

        self.module_export_name.to_ts().append(alias_doc)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BuiltInKeyword {
    Keyof,
}

impl typescript::Pretty for BuiltInKeyword {
    fn to_ts(&self) -> RcDoc<()> {
        match self {
            BuiltInKeyword::Keyof => RcDoc::text("keyof"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Op {
    Union,
    Intersection,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PrimitiveType {
    Boolean,
    Number,
    String,
}

impl PrettySexpr for PrimitiveType {
    fn pretty_sexpr(&self) -> RcDoc<()> {
        match self {
            PrimitiveType::Boolean => RcDoc::text("boolean"),
            PrimitiveType::Number => RcDoc::text("number"),
            PrimitiveType::String => RcDoc::text("string"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PrefixOp {
    Infer,
    Not,
}

impl PrefixOp {
    /// Returns `true` if the prefix op is [`Not`].
    ///
    /// [`Not`]: PrefixOp::Not
    #[must_use]
    pub fn is_not(&self) -> bool {
        matches!(self, Self::Not)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InfixOp {
    Extends,
    NotExtends,
    Equals,
    NotEquals,
    StrictEquals,
    StrictNotEquals,
    And,
    Or,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MappingModifier {
    Add,
    Remove,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjectProperty<'a> {
    pub readonly: bool,
    pub optional: bool,
    pub key: String,
    pub value: AstNode<'a>,
}

impl<'a> typescript::Pretty for ObjectProperty<'a> {
    fn to_ts(&self) -> RcDoc<()> {
        let readonly = if self.readonly {
            RcDoc::text("readonly").append(RcDoc::space())
        } else {
            RcDoc::nil()
        };

        let optional = if self.optional {
            RcDoc::text("?")
        } else {
            RcDoc::nil()
        };

        let doc = RcDoc::nil();

        doc.append(readonly)
            .append(&self.key)
            .append(optional)
            .append(RcDoc::text(":"))
            .append(RcDoc::space())
            .append(self.value.to_ts())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Identifier(pub String);
impl Identifier {
    fn pretty_sexpr(&self) -> RcDoc<()> {
        RcDoc::text(&self.0)
    }
}

impl typescript::Pretty for Identifier {
    fn to_ts(&self) -> RcDoc<()> {
        RcDoc::text(self.0.clone())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeParameter<'a> {
    pub name: String,
    pub constraint: Option<AstNode<'a>>,
    pub default: Option<AstNode<'a>>,
    pub rest: bool,
}

impl<'a> TypeParameter<'a> {
    pub fn new(
        name: String,
        constraint: Option<AstNode<'a>>,
        default: Option<AstNode<'a>>,
        rest: bool,
    ) -> Self {
        Self {
            name,
            constraint,
            default,
            rest,
        }
    }

    fn pretty_sexpr(&self) -> RcDoc<()> {
        Ast::sexpr(vec![
            RcDoc::text("type-parameter"),
            RcDoc::text(self.name.clone()),
            self.constraint
                .as_ref()
                .map_or_else(RcDoc::nil, |v| v.pretty_sexpr()),
            self.default
                .as_ref()
                .map_or_else(RcDoc::nil, |v| v.pretty_sexpr()),
        ])
    }
}

impl<'a> typescript::Pretty for TypeParameter<'a> {
    fn to_ts(&self) -> RcDoc<()> {
        let rest = if self.rest {
            RcDoc::text("...")
        } else {
            RcDoc::nil()
        };

        let constraint = match &self.constraint {
            Some(constraint) => RcDoc::space()
                .append("extends")
                .append(RcDoc::space())
                .append(constraint.to_ts()),
            None => RcDoc::nil(),
        };

        let default_value = match &self.default {
            Some(value) => RcDoc::space()
                .append("=")
                .append(RcDoc::space())
                .append(value.to_ts()),
            None => RcDoc::nil(),
        };

        RcDoc::nil()
            .append(rest)
            .append(self.name.clone())
            .append(constraint)
            .append(default_value)
    }
}

pub(crate) mod if_expr {
    use pretty::RcDoc;

    use super::{Ast, AstNode, ExtendsExpr, InfixOp, Node, PrefixOp, PrettySexpr};

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub struct Expr<'a> {
        pub condition: AstNode<'a>,
        pub then_branch: AstNode<'a>,
        pub else_branch: Option<AstNode<'a>>,
    }

    impl<'a> Expr<'a> {
        pub(crate) fn simplify(&self) -> AstNode<'a> {
            let else_branch = self
                .else_branch
                .as_ref()
                .map_or_else(|| Node::from(Ast::Never), |v| v.clone());

            expand_to_extends(&self.condition, &self.then_branch, &else_branch)
        }
    }

    impl<'a> PrettySexpr for Expr<'a> {
        fn pretty_sexpr(&self) -> RcDoc<()> {
            let mut vec = vec![
                RcDoc::text("if"),
                self.condition.pretty_sexpr(),
                RcDoc::text("then:"),
                self.then_branch.pretty_sexpr(),
            ];

            if let Some(else_branch) = &self.else_branch {
                vec.push(RcDoc::text("else:"));
                vec.push(else_branch.pretty_sexpr());
            }

            Ast::sexpr(vec)
        }
    }

    /// Expands an if expression into a series of nested ternary expressions
    pub(crate) fn expand_to_extends<'a>(
        condition: &AstNode<'a>,
        then: &AstNode<'a>,
        else_arm: &AstNode<'a>,
    ) -> AstNode<'a> {
        // Recursive operations
        let out: Option<AstNode<'a>> = match &*condition.value {
            // Unary operators
            Ast::ExtendsPrefixOp { op, value } => {
                match op {
                    // Swap `then` and `else` branches
                    PrefixOp::Not if value.value.is_compatible_with_not_prefix_op() => {
                        Some(expand_to_extends(value, else_arm, then))
                    }
                    PrefixOp::Infer => todo!(),
                    _ => {
                        unreachable!(
                            "Expected `not` or `infer` prefix operator, found {condition:#?}"
                        )
                    }
                }
            }
            Ast::ExtendsInfixOp { lhs, op, rhs } => match op {
                InfixOp::And => {
                    let then = expand_to_extends(rhs, then, else_arm);
                    Some(expand_to_extends(lhs, &then, else_arm))
                }
                InfixOp::Or => {
                    let else_arm = expand_to_extends(rhs, then, else_arm);
                    Some(expand_to_extends(lhs, then, &else_arm))
                }
                _ => None,
            },
            _ => panic!("Expected extends operator, found {condition:#?}"),
        };

        if let Some(v) = out {
            return v;
        }

        // Terminal nodes
        match &*condition.value {
            // Binary operators
            Ast::ExtendsInfixOp { lhs, op, rhs } => {
                // TODO report a syntax error here
                // need to include spans in ASTNode<'a>
                if !lhs.value.is_typescript_feature() {
                    dbg!(&lhs);
                    unreachable!("value must be desugared before this point");
                }

                if !rhs.value.is_typescript_feature() {
                    dbg!(rhs.value.to_sexpr(80));
                    unreachable!("value must be desugared before this point");
                }

                match op {
                    // Equivalent to `lhs extends rhs ? then : else`
                    InfixOp::Extends => {
                        // FIXME missing span
                        Ast::from(ExtendsExpr::new(
                            lhs.clone(),
                            rhs.clone(),
                            then.clone(),
                            else_arm.clone(),
                        ))
                        .into()
                    }
                    InfixOp::NotExtends => {
                        // FIXME missing span
                        Ast::from(ExtendsExpr::new(
                            lhs.clone(),
                            rhs.clone(),
                            else_arm.clone(),
                            then.clone(),
                        ))
                        .into()
                    }
                    InfixOp::Equals => todo!("equals"),
                    InfixOp::NotEquals => todo!("not equals"),
                    InfixOp::StrictEquals => todo!("strict equals"),
                    InfixOp::StrictNotEquals => todo!("strict not equals"),
                    _ => unreachable!(),
                }
            }
            _ => panic!("Expected extends operator, found {condition:#?}"),
        }
    }
}

pub(crate) mod match_expr {
    use pest::Span;

    use super::{Ast, AstNode, ExtendsExpr, Node};

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub struct Expr<'a> {
        pub value: AstNode<'a>,
        pub arms: Vec<Arm<'a>>,
        pub else_arm: AstNode<'a>,
    }

    impl<'a> Expr<'a> {
        pub(crate) fn simplify(&self) -> AstNode<'a> {
            // Convert match arms to a series of extends expressions.
            // Allows for a single wildcard pattern ("_") to be used as the default case.
            let Expr {
                value,
                arms,
                else_arm,
            } = self;

            arms.iter()
                .rev()
                .fold(else_arm.clone(), |acc: AstNode, arm: &Arm| -> AstNode {
                    let Arm { pattern, body } = arm;

                    // FIXME missing span
                    Node {
                        span: None,
                        value: Box::new(Ast::from(ExtendsExpr::new(
                            value.clone(),
                            pattern.clone(),
                            body.clone(),
                            acc,
                        ))),
                    }
                })
        }
    }

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub struct Arm<'a> {
        pub pattern: AstNode<'a>,
        pub body: AstNode<'a>,
    }
}

pub(crate) mod cond_expr {
    use super::{if_expr, AstNode, PrettySexpr};

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub struct Expr<'a> {
        pub arms: Vec<Arm<'a>>,
        /// Unlike match and if expressions, the else arm is *not* optional
        pub else_arm: AstNode<'a>,
    }

    impl<'a> Expr<'a> {
        pub(crate) fn simplify(&self) -> AstNode<'a> {
            // Convert a CondExpr to a series of nested ternary expressions
            let Expr { arms, else_arm } = self;

            let init_else: AstNode<'a> = (else_arm).clone();

            let acc: AstNode<'a> = arms.iter().rev().fold(init_else, |else_arm, arm| {
                let Arm {
                    condition,
                    body: then,
                } = arm;

                if_expr::expand_to_extends(condition, then, &else_arm)
            });

            acc
        }
    }

    impl<'a> PrettySexpr for Expr<'a> {
        fn pretty_sexpr(&self) -> pretty::RcDoc<()> {
            let mut vec = vec![pretty::RcDoc::text("cond")];

            for arm in &self.arms {
                vec.push(arm.pretty_sexpr());
            }

            vec.push(self.else_arm.pretty_sexpr());

            super::Ast::sexpr(vec)
        }
    }

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub struct Arm<'a> {
        pub condition: AstNode<'a>,
        pub body: AstNode<'a>,
    }

    impl<'a> PrettySexpr for Arm<'a> {
        fn pretty_sexpr(&self) -> pretty::RcDoc<()> {
            super::Ast::sexpr(vec![
                pretty::RcDoc::text("when"),
                self.condition.pretty_sexpr(),
            ])
        }
    }
}

// fn resolve_let_bindings<'a>(
//     bindings: &'a HashMap<Identifier, ASTNode<'a>>,
// ) -> impl Fn(ASTNode) -> ASTNode + 'a {
//     move |node| match &*node.value {
//         Ast::Ident(name) => {
//             let ident = Identifier(name.clone());
//
//             if let Some(value) = bindings.get(&ident) {
//                 value.to_owned()
//             } else {
//                 node.clone()
//             }
//         }
//         Ast::LetExpr {
//             bindings: new_bindings,
//             body,
//         } => {
//             // let nested_bindings: HashMap<Identifier, _> = bindings
//             //     .iter()
//             //     .chain(new_bindings.iter())
//             //     .map(|(k, v)| (k.clone(), v.clone()))
//             //     .collect();
//             //
//             // // let f = resolve_let_bindings(&nested_bindings);
//             //
//             // body.transform(_)
//             todo!()
//         }
//         _ => node.clone(),
//     }
// }

