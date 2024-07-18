use itertools::Itertools;
use node::{Node, Nodes};
use pest::Span;
use pretty::RcDoc as D;

use crate::{
    parser::{Pair, ParserError},
    pretty::{parens, string_literal, surround},
    runtime::{self, builtin},
    typescript,
};

pub(crate) mod macros;

pub(crate) trait PrettySexpr {
    fn to_sexpr(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.pretty_sexpr().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }

    fn pretty_sexpr(&self) -> D;

    fn sexpr(items: Vec<D>) -> D {
        let sep = D::line();

        D::text("(")
            .append(D::intersperse(items, sep).nest(4))
            .append(D::text(")"))
            .group()
    }
}

pub(crate) mod node {
    use super::*;
    use pest::Span;
    use std::collections::HashMap;

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub struct Node<'a> {
        /// Generated nodes have no span
        pub span: Option<Span<'a>>,
        pub value: Box<Ast<'a>>,
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

        pub(crate) fn new(span: Option<Span<'a>>, value: Ast<'a>) -> Self {
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
            pub(crate) fn pick_node<T>((node, _ctx): (Node, T)) -> Node {
                node
            }

            // Produce a new node and context
            let result = |node: Ast<'a>, ctx: Context| (self.clone().replace(node), ctx);

            // Reducer
            let red = move |node: &Node<'a>, ctx| node.traverse(ctx, pre, post);

            // Reducer only returns the node, drops the context
            let red_pick_node =
                move |node: &Node<'a>, ctx| pick_node(node.traverse(ctx, pre, post));

            // Reducer that maps over a list of nodes
            let red_items = move |node: &Nodes<'a>, ctx: Context| {
                node.iter()
                    .map(|item| red_pick_node(item, ctx.clone()))
                    .collect_vec()
            };

            // Returns a closure that takes a node
            let fn_red_pick_node =
                move |ctx| move |node: &Node<'a>| pick_node(node.traverse(ctx, pre, post));

            let (node, ctx) = pre(self.clone(), ctx);

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

                Ast::ExtendsExpr(ExtendsExpr {
                    lhs,
                    rhs,
                    then_branch,
                    else_branch,
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

                    let ast = Ast::LetExpr(let_expr::Expr {
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
                                let constraint =
                                    constraint.as_ref().map(fn_red_pick_node(ctx.clone()));
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

            let (node, acc) = post(node, ctx);

            (node, acc)
        }

        pub fn simplify(&self) -> Self {
            let bindings: Bindings = Default::default();
            let (tree, _) =
                self.traverse(
                    bindings,
                    &|node, ctx| (node, ctx),
                    &|node, ctx| match &*node.value {
                        Ast::IfExpr(if_expr) => (if_expr.simplify(), ctx),
                        Ast::MatchExpr(match_expr) => (match_expr.simplify(), ctx),
                        Ast::CondExpr(cond_expr) => (cond_expr.simplify(), ctx),
                        Ast::LetExpr(let_expr) => (let_expr.simplify(), ctx),
                        _ast => (node, ctx),
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
    }

    pub(crate) type Nodes<'a> = Vec<Node<'a>>;

    pub(crate) type Bindings<'a> = HashMap<Identifier, Node<'a>>;

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

    impl<'a> PrettySexpr for Node<'a> {
        fn pretty_sexpr(&self) -> D {
            self.value.pretty_sexpr()
        }
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
        let bindings: node::Bindings = Default::default();
        let (tree, _) = tree.traverse(
            bindings,
            &|node, ctx| (node, ctx),
            &|node, ctx| match &*node.value {
                Ast::IfExpr(if_expr) => (if_expr.simplify(), ctx),
                Ast::MatchExpr(match_expr) => (match_expr.simplify(), ctx),
                Ast::CondExpr(cond_expr) => (cond_expr.simplify(), ctx),
                _ast => (node, ctx),
            },
        );
        assert_eq!("", tree.to_sexpr(80));
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct NamespaceAccess<'a> {
    pub lhs: Node<'a>,
    pub rhs: Node<'a>,
}

impl NamespaceAccess<'_> {
    fn pretty_sexpr(&self) -> D<()> {
        Ast::sexpr(vec![
            D::text("::"),
            self.lhs.pretty_sexpr(),
            self.rhs.pretty_sexpr(),
        ])
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExtendsExpr<'a> {
    pub lhs: Node<'a>,
    pub rhs: Node<'a>,
    pub then_branch: Node<'a>,
    pub else_branch: Node<'a>,
}

impl<'a> ExtendsExpr<'a> {
    pub fn new(lhs: Node<'a>, rhs: Node<'a>, then_branch: Node<'a>, else_branch: Node<'a>) -> Self {
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
pub struct Tuple<'a> {
    pub items: Nodes<'a>,
}

impl PrettySexpr for Tuple<'_> {
    fn pretty_sexpr(&self) -> D<()> {
        Ast::sexpr(self.items.iter().map(|item| item.pretty_sexpr()).collect())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Application<'a> {
    pub name: Identifier,
    pub args: Nodes<'a>,
}

impl<'a> typescript::Pretty for Application<'a> {
    fn to_ts(&self) -> D<()> {
        let sep = D::text(",").append(D::space());

        let generic_inner = D::intersperse(self.args.iter().map(|param| param.to_ts()), sep);

        let generic_params = D::text("<").append(generic_inner).append(D::text(">"));

        self.name.pretty().append(generic_params)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MappedType<'a> {
    pub index: String,
    pub iterable: Node<'a>,
    pub remapped_as: Option<Node<'a>>,
    pub readonly_mod: Option<MappingModifier>,
    pub optional_mod: Option<MappingModifier>,
    pub body: Node<'a>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjectLiteral<'a> {
    pub properties: Vec<ObjectProperty<'a>>,
}

impl<'a> typescript::Pretty for ObjectLiteral<'a> {
    fn to_ts(&self) -> D<()> {
        let props = &self.properties;

        let sep = D::text(",").append(D::line());

        let props = D::intersperse(props.iter().map(|prop| prop.to_ts()), sep);

        D::nil()
            .append("{")
            .append(D::line_())
            .append(props.nest(4))
            .append(D::line_())
            .append(D::text("}"))
            .group()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Interface<'a> {
    pub export: bool,
    pub name: String,
    pub extends: Option<String>,
    pub params: Vec<TypeParameter<'a>>,
    pub definition: Vec<ObjectProperty<'a>>,
}

impl<'a> typescript::Pretty for Interface<'a> {
    fn to_ts(&self) -> D<()> {
        let Interface {
            export,
            name,
            extends,
            params,
            definition,
        } = self;

        let doc = if *export {
            D::text("export").append(D::space())
        } else {
            D::nil()
        };

        let extends = match extends {
            Some(extends) => D::space()
                .append("extends")
                .append(D::space())
                .append(extends)
                .append(D::space()),
            None => D::nil(),
        };

        let params_doc = match params {
            list if list.is_empty() => D::nil(),
            list => {
                let seperator = D::text(",").append(D::line());

                let params_body =
                    D::intersperse(list.iter().map(|param| param.to_ts().group()), seperator);

                D::text("<")
                    .append(D::line_().append(params_body).append(D::line_()).nest(4))
                    .append(D::text(">"))
                    .group()
            }
        };

        let body = if definition.is_empty() {
            D::text("{}")
        } else {
            let body = definition.iter().map(|p| p.to_ts());

            let body = D::intersperse(body, D::text(";").append(D::hardline())).append(";");

            let body = D::hardline().append(body).nest(4);

            D::nil()
                .append("{")
                .append(body)
                .append(D::hardline())
                .append("}")
        };

        doc.append("interface")
            .append(D::space())
            .append(name)
            .append(params_doc)
            .append(extends)
            .append(D::space())
            .append(body)
            .group()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UnitTest<'a> {
    pub name: String,
    pub body: Vec<Node<'a>>,
}

impl PrettySexpr for UnitTest<'_> {
    fn pretty_sexpr(&self) -> D<()> {
        Ast::sexpr(
            [
                vec![
                    D::text("unit-test"),
                    D::text(self.name.clone()),
                    D::text(":do"),
                ],
                self.body.iter().map(|node| node.pretty_sexpr()).collect(),
            ]
            .concat(),
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MacroCall<'a> {
    pub name: String,
    pub args: Vec<Node<'a>>,
}

impl<'a> MacroCall<'a> {
    fn eval(&self) -> Node<'a> {
        match self.name.as_str() {
            "unquote!" => match self.args.as_slice() {
                [node] => builtin::unquote(node.to_owned()),
                _ => panic!("unquote! expects exactly one argument"),
            },
            id => unimplemented!("macro {} not implemented", id),
        }
    }
}

impl<'a> PrettySexpr for MacroCall<'a> {
    fn pretty_sexpr(&self) -> D<()> {
        Ast::sexpr(
            vec![D::text(self.name.clone())]
                .into_iter()
                .chain(self.args.iter().map(|n| n.pretty_sexpr()))
                .collect_vec(),
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Ast<'a> {
    Access {
        lhs: Node<'a>,
        rhs: Node<'a>,
        is_dot: bool,
    },
    Any,
    MacroCall(MacroCall<'a>),
    Application(Application<'a>),
    Array(Node<'a>),
    InfixOp {
        lhs: Node<'a>,
        op: Op,
        rhs: Node<'a>,
    },
    Builtin {
        name: BuiltInKeyword,
        argument: Node<'a>,
    },
    CondExpr(cond_expr::Expr<'a>),
    ExtendsInfixOp {
        lhs: Node<'a>,
        op: InfixOp,
        rhs: Node<'a>,
    },
    ExtendsExpr(ExtendsExpr<'a>),
    ExtendsPrefixOp {
        op: PrefixOp,
        value: Node<'a>,
    },
    False,
    Ident(Identifier),
    IfExpr(if_expr::Expr<'a>),
    ImportStatement {
        import_clause: ImportClause,
        module: String,
    },
    LetExpr(let_expr::Expr<'a>),
    MappedType(MappedType<'a>),
    MatchExpr(match_expr::Expr<'a>),
    NamespaceAccess(NamespaceAccess<'a>),
    Never,
    NoOp,
    Null,
    Number(String),
    ObjectLiteral(ObjectLiteral<'a>),
    Primitive(PrimitiveType),
    Program(Nodes<'a>),
    Statement(Node<'a>),
    UnitTest(UnitTest<'a>),
    String(String),
    TemplateString(String),
    True,
    Tuple(Tuple<'a>),
    TypeAlias {
        export: bool,
        name: Identifier,
        params: Vec<TypeParameter<'a>>,
        body: Node<'a>,
    },
    Undefined,
    Unknown,
    Interface(Interface<'a>),
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
    pub fn to_node(&self, pair: Pair<'a>) -> Node<'a> {
        Node::from_pair(&pair, self.clone())
    }

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
            Ast::LetExpr(let_expr::Expr { .. }) => false,
            Ast::MappedType(MappedType { .. }) => true,
            Ast::MatchExpr(match_expr::Expr { .. }) => false,
            Ast::NamespaceAccess(_) => true,
            Ast::Never => true,
            Ast::NoOp => false,
            Ast::Null => true,
            Ast::Number(_) => true,
            Ast::ObjectLiteral(_) => true,
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
            Ast::Interface(Interface { .. }) => todo!(),
            Ast::UnitTest(_) => todo!(),
            Ast::MacroCall(_) => todo!(),
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

    pub fn as_ident(&self) -> Option<&Identifier> {
        if let Self::Ident(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl<'a> typescript::Pretty for Ast<'a> {
    fn to_ts(&self) -> D<()> {
        match self {
            Ast::Program(stmnts) => {
                let mut doc = D::nil();
                for stmnt in stmnts {
                    doc = doc
                        .append(stmnt.to_ts())
                        .append(D::hardline())
                        .append(D::hardline());
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
                    D::text("export").append(D::space())
                } else {
                    D::nil()
                };

                let params_doc = match params {
                    list if list.is_empty() => D::nil(),
                    list => {
                        let seperator = D::text(",").append(D::line());

                        let body = D::intersperse(
                            list.iter().map(|param| param.to_ts().group()),
                            seperator,
                        );

                        D::text("<")
                            .append(D::line_().append(body).append(D::line_()).nest(4))
                            .append(D::text(">"))
                            .group()
                    }
                };

                doc.append("type")
                    .append(D::space())
                    .append(name.pretty())
                    .append(params_doc)
                    .append(D::space())
                    .append("=")
                    .append(D::line().append(body).nest(4))
                    .group()
            }
            Ast::Ident(identifier) => identifier.pretty(),
            Ast::Number(number) => D::text(number),
            Ast::Primitive(primitive) => D::text(match primitive {
                PrimitiveType::Boolean => "boolean",
                PrimitiveType::Number => "number",
                PrimitiveType::String => "string",
            }),
            Ast::String(string) => string_literal(string),
            Ast::TemplateString(string) => D::text(string),
            Ast::IfExpr(..) => {
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
                    .append(D::text("["))
                    .append(string_literal(rhs.0.as_str()))
                    .append(D::text("]"))
                    .group()
            }
            Ast::Access { lhs, rhs, .. } => lhs
                .to_ts()
                .append(D::text("["))
                .append(rhs.to_ts())
                .append(D::text("]"))
                .group(),
            Ast::ObjectLiteral(value) => value.to_ts(),
            Ast::Application(value) => value.to_ts(),
            Ast::Tuple(Tuple { items }) => {
                let sep = D::text(",").append(D::space());

                let items = D::intersperse(items.iter().map(|item| item.to_ts()), sep);

                D::text("[").append(items).append(D::text("]"))
            }
            Ast::Array(node) => {
                let doc = if node.value.is_infix_op() {
                    parens(node.to_ts())
                } else {
                    node.to_ts()
                };

                doc.append(D::text("[]"))
            }
            Ast::Null => D::text("null"),
            Ast::Undefined => D::text("undefined"),
            Ast::Never => D::text("never"),
            Ast::Any => D::text("any"),
            Ast::Unknown => D::text("unknown"),
            Ast::True => D::text("true"),
            Ast::False => D::text("false"),

            Ast::InfixOp { lhs, op, rhs } => {
                fn fmt<'a>(v: &'a Node<'a>) -> D<'a, ()> {
                    match &*v.value {
                        Ast::InfixOp { .. } => parens(v.to_ts()),
                        _ => v.to_ts(),
                    }
                }

                let lhs = fmt(lhs);
                let rhs = fmt(rhs);

                let op = match op {
                    Op::Union => D::text("|"),
                    Op::Intersection => D::text("&"),
                };

                D::nil()
                    .append(lhs)
                    .append(D::space())
                    .append(op)
                    .append(D::space())
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
                    .append(D::space())
                    .append("extends")
                    .append(D::space())
                    .append(rhs.to_ts());

                let then_doc = D::line()
                    .append("?")
                    .append(D::space())
                    .append(then.to_ts())
                    .nest(4);

                let else_doc = D::line()
                    .append(":")
                    .append(D::space())
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
                .append(D::space())
                .append("extends")
                .append(D::space())
                .append(rhs.to_ts())
                .group(),
            Ast::Statement(stmnt) => stmnt.to_ts().append(D::text(";")),
            Ast::MappedType(MappedType {
                index: key,
                iterable,
                remapped_as,
                readonly_mod,
                optional_mod,
                body,
            }) => {
                let remapped_as_doc = match remapped_as {
                    Some(remapped_as) => D::space()
                        .append("as")
                        .append(D::space())
                        .append(remapped_as.to_ts()),
                    None => D::nil(),
                };

                let lhs_doc = D::nil()
                    .append(key)
                    .append(D::space())
                    .append("in")
                    .append(D::space())
                    .append(iterable.to_ts())
                    .append(remapped_as_doc)
                    .group();

                let rhs_doc = body.to_ts();

                let rhs_doc = D::line().append(rhs_doc).nest(4).group();

                let readonly_doc = match readonly_mod {
                    Some(MappingModifier::Add) => D::text("readonly"),
                    Some(MappingModifier::Remove) => D::text("-readonly"),
                    None => D::nil(),
                };

                let optional_doc = match optional_mod {
                    Some(MappingModifier::Add) => D::text("?"),
                    Some(MappingModifier::Remove) => D::text("-?"),
                    None => D::nil(),
                };

                let inner_doc = D::line()
                    .append(readonly_doc)
                    .append("[")
                    .append(lhs_doc)
                    .append("]")
                    .append(optional_doc)
                    .append(":")
                    .append(rhs_doc)
                    .append(D::line())
                    .nest(4)
                    .group();

                D::nil().append("{").append(inner_doc).append("}").group()
            }
            Ast::LetExpr(..) => {
                unreachable!("LetExpr should be desugared before this point")
            }
            Ast::ImportStatement {
                import_clause,
                module,
            } => {
                let import_clause = import_clause.to_ts();

                D::text("import type")
                    .append(D::space())
                    .append(import_clause)
                    .append(D::space())
                    .append("from")
                    .append(D::space())
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
            Ast::Interface(value) => {
                return value.to_ts();
            }
            Ast::UnitTest(_) => D::nil(),
            Ast::MacroCall(_) => unreachable!("MacroCall should be desugared before this point"),
        }
    }
}

impl<'a> From<ExtendsExpr<'a>> for Ast<'a> {
    fn from(v: ExtendsExpr<'a>) -> Self {
        Ast::ExtendsExpr(v)
    }
}

impl<'a> PrettySexpr for Ast<'a> {
    fn pretty_sexpr(&self) -> D {
        match self {
            Ast::Access { lhs, rhs, is_dot } => {
                let op = if *is_dot { "." } else { ".[]" };

                Ast::sexpr(vec![D::text(op), lhs.pretty_sexpr(), rhs.pretty_sexpr()])
            }
            Ast::Any => D::text("any"),
            Ast::Application(Application { name, args }) => Ast::sexpr(
                vec![name.pretty()]
                    .into_iter()
                    .chain(args.iter().map(|n| n.pretty_sexpr()))
                    .collect_vec(),
            ),
            Ast::Array(value) => Ast::sexpr(vec![D::text("[]"), value.pretty_sexpr()]),
            Ast::InfixOp { lhs, op, rhs } => Ast::sexpr(vec![
                D::text(match op {
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

                Ast::sexpr(vec![D::text(name), argument.pretty_sexpr()])
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

                Ast::sexpr(vec![D::text(op), lhs.pretty_sexpr(), rhs.pretty_sexpr()])
            }
            Ast::ExtendsExpr(ExtendsExpr {
                lhs,
                rhs,
                then_branch,
                else_branch,
            }) => Ast::sexpr(vec![
                D::text("extends"),
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

                Ast::sexpr(vec![D::text(op), value.pretty_sexpr()])
            }
            Ast::False => todo!(),
            Ast::Ident(ident) => ident.pretty_sexpr(),
            Ast::IfExpr(if_expr) => if_expr.pretty_sexpr(),
            Ast::ImportStatement { .. } => todo!(),
            Ast::LetExpr(let_expr) => let_expr.pretty_sexpr(),
            Ast::MappedType(MappedType { .. }) => todo!(),
            Ast::MatchExpr(match_expr::Expr { .. }) => todo!(),
            Ast::NamespaceAccess(namespace_access) => namespace_access.pretty_sexpr(),
            Ast::Never => todo!(),
            Ast::NoOp => todo!(),
            Ast::Null => todo!(),
            Ast::Number(number) => D::text(number),
            Ast::ObjectLiteral(..) => todo!(),
            Ast::Primitive(primitive) => primitive.pretty_sexpr(),
            Ast::Program(statements) => {
                let mut vec = vec![D::text("program")];

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
                let mut vec = vec![D::text("type"), name.pretty()];

                if *export {
                    vec.push(D::text(":export"));
                }

                if !params.is_empty() {
                    vec.push(Ast::sexpr(
                        params.iter().map(|p| p.pretty_sexpr()).collect(),
                    ));
                }

                vec.push(D::text("as:"));

                vec.push(body.pretty_sexpr());

                Ast::sexpr(vec)
            }
            Ast::Undefined => todo!(),
            Ast::Unknown => todo!(),
            Ast::Interface(Interface { .. }) => todo!(),
            Ast::UnitTest(x) => x.pretty_sexpr(),
            Ast::MacroCall(x) => x.pretty_sexpr(),
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ImportClause {
    Named(Vec<ImportSpecifier>),
    Namespace { alias: Identifier },
}

impl typescript::Pretty for ImportClause {
    fn to_ts(&self) -> D<()> {
        match self {
            ImportClause::Named(specifiers) => {
                let sep = D::text(",").append(D::line());

                let specifiers =
                    D::intersperse(specifiers.iter().map(typescript::Pretty::to_ts), sep);

                D::text("{")
                    .append(
                        D::nil()
                            .append(D::line())
                            .append(specifiers)
                            .append(D::line())
                            .nest(4),
                    )
                    .append(D::text("}"))
                    .group()
            }
            ImportClause::Namespace { alias } => D::text("*")
                .append(D::space())
                .append("as")
                .append(D::space())
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
    fn to_ts(&self) -> D<()> {
        let alias_doc = match &self.alias {
            Some(alias) => D::space()
                .append("as")
                .append(D::space())
                .append(alias.to_ts()),

            None => D::nil(),
        };

        self.module_export_name.to_ts().append(alias_doc)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BuiltInKeyword {
    Keyof,
}

impl typescript::Pretty for BuiltInKeyword {
    fn to_ts(&self) -> D<()> {
        match self {
            BuiltInKeyword::Keyof => D::text("keyof"),
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
    fn pretty_sexpr(&self) -> D<()> {
        match self {
            PrimitiveType::Boolean => D::text("boolean"),
            PrimitiveType::Number => D::text("number"),
            PrimitiveType::String => D::text("string"),
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
pub enum ObjectPropertyKey<'a> {
    Index(PropertyKeyIndex<'a>),
    Key(String),
    Computed(Identifier),
}

impl<'a> typescript::Pretty for ObjectPropertyKey<'a> {
    fn to_ts(&self) -> D<()> {
        match self {
            ObjectPropertyKey::Index(index) => surround(index.to_ts(), "[", "]").group(),
            ObjectPropertyKey::Key(key) => D::text(key.clone()),
            ObjectPropertyKey::Computed(id) => surround(id.to_ts(), "[", "]").group(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PropertyKeyIndex<'a> {
    pub key: String,
    pub iterable: Node<'a>,
    pub remapped_as: Option<Node<'a>>,
}

impl<'a> typescript::Pretty for PropertyKeyIndex<'a> {
    fn to_ts(&self) -> D<()> {
        let PropertyKeyIndex {
            key,
            iterable,
            remapped_as,
        } = self;

        let remapped_as = match remapped_as {
            Some(remapped_as) => D::space()
                .append("as")
                .append(D::space())
                .append(remapped_as.to_ts()),
            None => D::nil(),
        };

        D::nil()
            .append(key)
            .append(D::space())
            .append("in")
            .append(D::space())
            .append(iterable.to_ts())
            .append(remapped_as)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjectProperty<'a> {
    pub readonly: bool,
    pub optional: bool,
    pub key: ObjectPropertyKey<'a>,
    pub value: Node<'a>,
}

impl<'a> typescript::Pretty for ObjectProperty<'a> {
    fn to_ts(&self) -> D<()> {
        let readonly = if self.readonly {
            D::text("readonly").append(D::space())
        } else {
            D::nil()
        };

        let optional = if self.optional {
            D::text("?")
        } else {
            D::nil()
        };

        let doc = D::nil();

        doc.append(readonly)
            .append(self.key.to_ts())
            .append(optional)
            .append(D::text(":"))
            .append(D::space())
            .append(self.value.to_ts())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Identifier(pub String);

impl From<String> for Identifier {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Self(value.to_string())
    }
}

impl PrettySexpr for Identifier {
    fn pretty_sexpr(&self) -> D<()> {
        self.pretty()
    }
}

impl Identifier {
    fn pretty(&self) -> D<()> {
        D::text(&self.0)
    }
}

impl typescript::Pretty for Identifier {
    fn to_ts(&self) -> D<()> {
        D::text(self.0.clone())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeParameter<'a> {
    pub name: String,
    pub constraint: Option<Node<'a>>,
    pub default: Option<Node<'a>>,
    pub rest: bool,
}

impl<'a> TypeParameter<'a> {
    pub fn new(
        name: String,
        constraint: Option<Node<'a>>,
        default: Option<Node<'a>>,
        rest: bool,
    ) -> Self {
        Self {
            name,
            constraint,
            default,
            rest,
        }
    }

    fn pretty_sexpr(&self) -> D<()> {
        Ast::sexpr(vec![
            D::text("type-parameter"),
            D::text(self.name.clone()),
            self.constraint
                .as_ref()
                .map_or_else(D::nil, |v| v.pretty_sexpr()),
            self.default
                .as_ref()
                .map_or_else(D::nil, |v| v.pretty_sexpr()),
        ])
    }
}

impl<'a> typescript::Pretty for TypeParameter<'a> {
    fn to_ts(&self) -> D<()> {
        let rest = if self.rest { D::text("...") } else { D::nil() };

        let constraint = match &self.constraint {
            Some(constraint) => D::space()
                .append("extends")
                .append(D::space())
                .append(constraint.to_ts()),
            None => D::nil(),
        };

        let default_value = match &self.default {
            Some(value) => D::space()
                .append("=")
                .append(D::space())
                .append(value.to_ts()),
            None => D::nil(),
        };

        D::nil()
            .append(rest)
            .append(self.name.clone())
            .append(constraint)
            .append(default_value)
    }
}

pub(crate) mod if_expr {
    use pretty::RcDoc as D;

    use super::{
        node::{self, Node, Nodes},
        Ast, ExtendsExpr, InfixOp, PrefixOp, PrettySexpr,
    };

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub struct Expr<'a> {
        pub condition: Node<'a>,
        pub then_branch: Node<'a>,
        pub else_branch: Option<Node<'a>>,
    }

    impl<'a> Expr<'a> {
        pub(crate) fn simplify(&self) -> Node<'a> {
            let else_branch = self
                .else_branch
                .as_ref()
                .map_or_else(|| node::Node::from(Ast::Never), |v| v.clone());

            expand_to_extends(&self.condition, &self.then_branch, &else_branch)
        }
    }

    impl<'a> PrettySexpr for Expr<'a> {
        fn pretty_sexpr(&self) -> pretty::RcDoc<()> {
            let mut vec = vec![
                D::text("if"),
                self.condition.pretty_sexpr(),
                D::text("then:"),
                self.then_branch.pretty_sexpr(),
            ];

            if let Some(else_branch) = &self.else_branch {
                vec.push(D::text("else:"));
                vec.push(else_branch.pretty_sexpr());
            }

            Ast::sexpr(vec)
        }
    }

    /// Expands an if expression into a series of nested ternary expressions
    pub(crate) fn expand_to_extends<'a>(
        condition: &Node<'a>,
        then: &Node<'a>,
        else_arm: &Node<'a>,
    ) -> Node<'a> {
        // Recursive operations
        let out: Option<Node<'a>> = match &*condition.value {
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

    use super::{node::Node, Ast, ExtendsExpr};

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub struct Expr<'a> {
        pub value: Node<'a>,
        pub arms: Vec<Arm<'a>>,
        pub else_arm: Node<'a>,
    }

    impl<'a> Expr<'a> {
        pub(crate) fn simplify(&self) -> Node<'a> {
            // Convert match arms to a series of extends expressions.
            // Allows for a single wildcard pattern ("_") to be used as the default case.
            let Expr {
                value,
                arms,
                else_arm,
            } = self;

            arms.iter()
                .rev()
                .fold(else_arm.clone(), |acc: Node, arm: &Arm| -> Node {
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
        pub pattern: Node<'a>,
        pub body: Node<'a>,
    }
}

pub(crate) mod cond_expr {
    use super::{if_expr, Node, PrettySexpr};

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub struct Expr<'a> {
        pub arms: Vec<Arm<'a>>,
        /// Unlike match and if expressions, the else arm is *not* optional
        pub else_arm: Node<'a>,
    }

    impl<'a> Expr<'a> {
        pub(crate) fn simplify(&self) -> Node<'a> {
            // Convert a CondExpr to a series of nested ternary expressions
            let Expr { arms, else_arm } = self;

            let init_else: Node<'a> = (else_arm).clone();

            let acc: Node<'a> = arms.iter().rev().fold(init_else, |else_arm, arm| {
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
        pub condition: Node<'a>,
        pub body: Node<'a>,
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

pub(crate) mod let_expr {
    use pretty::RcDoc as D;

    use super::{
        node::{self, Node},
        Ast, Identifier, PrettySexpr,
    };

    use std::collections::HashMap;

    #[derive(Debug, PartialEq, Eq, Clone)]
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
}
