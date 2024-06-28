use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use pretty::RcDoc;

use crate::{
    parser::ParserError,
    pretty::{parens, string_literal},
    typescript,
};

pub(crate) mod macros;

pub(crate) trait Sexpr {
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
pub struct NamespaceAccess {
    pub lhs: Box<AST>,
    pub rhs: Box<AST>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExtendsExpr {
    pub lhs: Box<AST>,
    pub rhs: Box<AST>,
    pub then_branch: Box<AST>,
    pub else_branch: Box<AST>,
}

impl ExtendsExpr {
    pub fn new(
        lhs: Box<AST>,
        rhs: Box<AST>,
        then_branch: Box<AST>,
        else_branch: Box<AST>,
    ) -> Self {
        if !lhs.is_typescript_feature() {
            dbg!(&lhs);
            unreachable!("value must be desugared before this point");
        }
        if !rhs.is_typescript_feature() {
            dbg!(&rhs);
            unreachable!("value must be desugared before this point");
        }
        if !then_branch.is_typescript_feature() {
            dbg!(&then_branch);
            unreachable!("value must be desugared before this point");
        }
        if !else_branch.is_typescript_feature() {
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
pub struct IfExpr {
    pub condition: Box<AST>,
    pub then_branch: Box<AST>,
    pub else_branch: Option<Box<AST>>,
}

impl IfExpr {
    fn simplify(&self) -> AST {
        let else_branch = match &self.else_branch {
            Some(v) => v.clone(),
            None => Box::new(AST::Never),
        };

        expand_if_expr(&self.condition, &self.then_branch, &else_branch)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AST {
    Access {
        lhs: Box<AST>,
        rhs: Box<AST>,
        is_dot: bool,
    },
    Any,
    Application(String, Vec<AST>),
    Array(Box<AST>),
    BinOp {
        lhs: Box<AST>,
        op: Op,
        rhs: Box<AST>,
    },
    Builtin {
        name: BuiltInKeyword,
        argument: Box<AST>,
    },
    CondExpr {
        arms: Vec<CondArm>,
        else_: Box<AST>,
    },
    ExtendsBinOp {
        lhs: Box<AST>,
        op: InfixOp,
        rhs: Box<AST>,
    },
    ExtendsExpr(ExtendsExpr),
    ExtendsPrefixOp {
        op: PrefixOp,
        value: Box<AST>,
    },
    False,
    Ident(String),
    IfExpr(IfExpr),
    ImportStatement {
        import_clause: ImportClause,
        module: String,
    },
    LetExpr {
        bindings: HashMap<Identifier, AST>,
        body: Box<AST>,
    },
    MappedType {
        index: String,
        iterable: Box<AST>,
        remapped_as: Option<Box<AST>>,
        readonly_mod: Option<MappingModifier>,
        optional_mod: Option<MappingModifier>,
        body: Box<AST>,
    },
    MatchExpr {
        value: Box<AST>,
        arms: Vec<MatchArm>,
        else_: Box<AST>,
    },
    NamespaceAccess(NamespaceAccess),
    Never,
    NoOp,
    Null,
    Number(String),
    ObjectLiteral(Vec<ObjectProperty>),
    Primitive(PrimitiveType),
    Program(Vec<AST>),
    Statement(Box<AST>),
    String(String),
    TemplateString(String),
    True,
    Tuple(Vec<AST>),
    TypeAlias {
        export: bool,
        name: String,
        params: Vec<TypeParameter>,
        body: Box<AST>,
    },
    Undefined,
    Unknown,
}

impl From<IfExpr> for AST {
    fn from(v: IfExpr) -> Self {
        Self::IfExpr(v)
    }
}

impl AST {
    /// Operators that the `not` prefix operator can be applied to.
    pub fn is_compatible_with_not_prefix_op(&self) -> bool {
        match self {
            AST::ExtendsPrefixOp { op, .. } if op.is_not() => true,
            AST::ExtendsBinOp { .. } => true,
            _ => false,
        }
    }
    /// Anything that returns true is a feature that has a direct equivalent in TypeScript.
    /// Anything that's false is a feature that needs to be desugared.
    pub fn is_typescript_feature(&self) -> bool {
        match self {
            AST::Access { .. } => true,
            AST::Any => true,
            AST::Application(_, _) => true,
            AST::Array(_) => true,
            AST::BinOp { .. } => false,
            AST::Builtin { .. } => true,
            AST::CondExpr { .. } => false,
            AST::ExtendsBinOp { .. } => false,
            AST::ExtendsExpr(_) => true,
            AST::ExtendsPrefixOp { .. } => false,
            AST::False => true,
            AST::Ident(_) => true,
            AST::IfExpr(IfExpr { .. }) => false,
            AST::ImportStatement { .. } => true,
            AST::LetExpr { .. } => false,
            AST::MappedType { .. } => true,
            AST::MatchExpr { .. } => false,
            AST::NamespaceAccess(_) => true,
            AST::Never => true,
            AST::NoOp => false,
            AST::Null => true,
            AST::Number(_) => true,
            AST::ObjectLiteral(_) => true,
            AST::Primitive(_) => true,
            AST::Program(_) => true,
            AST::Statement(_) => true,
            AST::String(_) => true,
            AST::TemplateString(_) => true,
            AST::True => true,
            AST::Tuple(_) => true,
            AST::TypeAlias { .. } => false,
            AST::Undefined => true,
            AST::Unknown => true,
        }
    }

    pub fn is_bin_op(&self) -> bool {
        matches!(self, AST::BinOp { .. })
    }

    pub fn as_ident(&self) -> Option<&String> {
        if let Self::Ident(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /**
     * Recursively transform a node and all of its children. There is no generalize
     * tree walk method in Rust to deal with ADTs, so we have to implement it manually.
     */
    pub fn transform(&self, f: &impl Fn(&Self) -> Self) -> Self {
        let transform = |node: &AST| node.transform(f);

        let transform_and_box = |node: &AST| Box::new(node.transform(f));

        let transform_each = |nodes: &Vec<AST>| nodes.iter().map(transform).collect::<Vec<_>>();

        // For all nodes that are not a leaf node,
        // we need to recursively simplify
        let out = match self {
            // Leaf nodes are not transformed
            AST::Never
            | AST::Any
            | AST::Unknown
            | AST::Null
            | AST::Undefined
            | AST::False
            | AST::True
            | AST::Ident { .. }
            | AST::Number { .. }
            | AST::Primitive { .. }
            | AST::String { .. }
            | AST::ImportStatement { .. }
            | AST::TemplateString { .. } => self.clone(),

            // For all other nodes, we recursively transform
            AST::Program(vec) => AST::Program(transform_each(vec)),
            AST::TypeAlias {
                export,
                name,
                params,
                body,
            } => {
                let params = params
                    .iter()
                    .map(|param| {
                        let mut param = param.clone();
                        param.default = param.default.map(|d| Box::new(d.transform(f)));
                        param.constraint = param.constraint.map(|d| Box::new(d.transform(f)));
                        param
                    })
                    .collect_vec();

                AST::TypeAlias {
                    export: *export,
                    name: name.clone(),
                    params,
                    body: Box::new(body.transform(f)),
                }
            }
            AST::Tuple(vec) => AST::Tuple(transform_each(vec)),
            AST::Array(vec) => AST::Array(transform_and_box(vec)),
            AST::Access { lhs, rhs, is_dot } => AST::Access {
                lhs: transform_and_box(lhs),
                rhs: transform_and_box(rhs),
                is_dot: *is_dot,
            },
            AST::IfExpr(IfExpr {
                condition: cond,
                then_branch: then,
                else_branch: els,
            }) => AST::from(IfExpr {
                condition: transform_and_box(cond),
                then_branch: transform_and_box(then),
                else_branch: els.as_ref().map(|v| transform_and_box(v)),
            }),
            AST::BinOp { lhs, op, rhs } => AST::BinOp {
                lhs: transform_and_box(lhs),
                op: op.clone(),
                rhs: transform_and_box(rhs),
            },
            AST::ExtendsBinOp { lhs, op, rhs } => AST::ExtendsBinOp {
                lhs: transform_and_box(lhs),
                op: op.clone(),
                rhs: transform_and_box(rhs),
            },

            AST::ExtendsPrefixOp { op, value } => AST::ExtendsPrefixOp {
                op: op.clone(),
                value: transform_and_box(value),
            },

            AST::ExtendsExpr(ExtendsExpr {
                lhs,
                rhs,
                then_branch: then,
                else_branch: els,
            }) => AST::from(ExtendsExpr::new(
                transform_and_box(lhs),
                transform_and_box(rhs),
                transform_and_box(then),
                transform_and_box(els),
            )),
            AST::ObjectLiteral(props) => AST::ObjectLiteral(
                props
                    .iter()
                    .map(|prop| {
                        let mut p = prop.clone();
                        p.value = transform(&prop.value);
                        p
                    })
                    .collect(),
            ),
            AST::Application(name, args) => AST::Application(name.clone(), transform_each(args)),

            AST::MatchExpr { value, arms, else_ } => {
                let value = transform_and_box(value);

                let arms = arms
                    .iter()
                    .map(|arm| {
                        let mut a = arm.clone();
                        a.pattern = transform(&arm.pattern);
                        a.body = transform(&arm.body);
                        a
                    })
                    .collect();

                let else_ = Box::new(transform(else_));

                AST::MatchExpr { value, arms, else_ }
            }

            AST::CondExpr { arms, else_ } => {
                let arms = arms
                    .iter()
                    .map(|arm| {
                        let mut a = arm.clone();
                        a.condition = transform(&arm.condition);
                        a.body = transform(&arm.body);
                        a
                    })
                    .collect();

                let else_ = Box::new(transform(else_));

                AST::CondExpr { arms, else_ }
            }

            AST::Builtin { name, argument } => AST::Builtin {
                name: name.clone(),
                argument: transform_and_box(argument),
            },
            AST::Statement(node) => AST::Statement(Box::new(node.transform(f))),

            AST::MappedType {
                index,
                iterable,
                remapped_as,
                readonly_mod,
                optional_mod,
                body,
            } => AST::MappedType {
                index: index.clone(),
                iterable: Box::new(iterable.transform(f)),
                remapped_as: remapped_as.clone(),
                readonly_mod: readonly_mod.clone(),
                optional_mod: optional_mod.clone(),
                body: Box::new(body.transform(f)),
            },
            AST::LetExpr { bindings, body } => (**body)
                .clone()
                .transform(&resolve_let_bindings(bindings))
                .transform(f),
            AST::NamespaceAccess(_) => self.clone(),
            AST::NoOp => todo!(),
        };

        f(&out)
    }

    pub fn simplify(&self) -> Self {
        self.transform(&|node| match node {
            // Replace all instances of `IfExpr` with `ExtendsExpr`
            AST::IfExpr(if_expr) => if_expr.simplify(),
            AST::MatchExpr { .. } => node.simplify_match_expr(),
            AST::CondExpr { .. } => node.simplify_cond_expr(),
            _ => node.clone(),
        })
    }

    // Convert match arms to a series of extends expressions.
    // Allows for a single wildcard pattern ("_") to be used as the default case.
    fn simplify_match_expr(&self) -> AST {
        let AST::MatchExpr { value, arms, else_ } = self else {
            panic!("Expected MatchExpr, found {self:#?}");
        };

        let init_else: AST = (**else_).clone();

        let out: AST = arms.iter().rev().fold(init_else, |acc, arm| -> AST {
            let MatchArm { pattern, body } = arm;

            AST::from(ExtendsExpr::new(
                value.clone(),
                Box::new(pattern.clone()),
                Box::new(body.clone()),
                Box::new(acc),
            ))
        });

        out
    }

    fn simplify_cond_expr(&self) -> AST {
        // Convert a CondExpr to a series of nested ternary expressions
        let AST::CondExpr { arms, else_ } = self else {
            panic!("Expected CondExpr, found {self:#?}");
        };

        let init_else: AST = (**else_).clone();

        let acc: AST = arms.iter().rev().fold(init_else, |else_, arm| {
            let CondArm {
                condition,
                body: then,
            } = arm;
            expand_if_expr(condition, then, &else_)
        });

        acc
    }

    /// Returns `true` if the node is [`ExtendsPrefixOp`].
    ///
    /// [`ExtendsPrefixOp`]: Node::ExtendsPrefixOp
    #[must_use]
    pub fn is_extends_prefix_op(&self) -> bool {
        matches!(self, Self::ExtendsPrefixOp { .. })
    }
}

impl From<ExtendsExpr> for AST {
    fn from(v: ExtendsExpr) -> Self {
        Self::ExtendsExpr(v)
    }
}

impl Sexpr for AST {
    fn pretty_sexpr(&self) -> RcDoc {
        match self {
            AST::Access { lhs, rhs, is_dot } => {
                let op = if *is_dot { "." } else { ".[]" };

                AST::sexpr(vec![
                    RcDoc::text(op),
                    lhs.pretty_sexpr(),
                    rhs.pretty_sexpr(),
                ])
            }
            AST::Any => RcDoc::text("any"),
            AST::Application(name, args) => AST::sexpr(
                vec![RcDoc::text(name)]
                    .into_iter()
                    .chain(args.iter().map(AST::pretty_sexpr))
                    .collect_vec(),
            ),
            AST::Array(value) => AST::sexpr(vec![RcDoc::text("[]"), value.pretty_sexpr()]),
            AST::BinOp { lhs, op, rhs } => AST::sexpr(vec![
                RcDoc::text(match op {
                    Op::Union => "&",
                    Op::Intersection => "|",
                }),
                lhs.pretty_sexpr(),
                rhs.pretty_sexpr(),
            ]),

            AST::Builtin { name, argument } => {
                let name = match name {
                    BuiltInKeyword::Keyof => "keyof",
                };

                AST::sexpr(vec![RcDoc::text(name), argument.pretty_sexpr()])
            }
            AST::CondExpr { arms, else_ } => todo!(),
            AST::ExtendsBinOp { lhs, op, rhs } => {
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

                AST::sexpr(vec![
                    RcDoc::text(op),
                    lhs.pretty_sexpr(),
                    rhs.pretty_sexpr(),
                ])
            }
            AST::ExtendsExpr(ExtendsExpr {
                lhs: _,
                rhs: _,
                then_branch: _,
                else_branch: _,
            }) => todo!(),
            AST::ExtendsPrefixOp { op, value } => {
                let op = match op {
                    PrefixOp::Not => "not",
                    PrefixOp::Infer => "infer",
                };

                AST::sexpr(vec![RcDoc::text(op), value.pretty_sexpr()])
            }
            AST::False => todo!(),
            AST::Ident(ident) => RcDoc::text(ident),
            AST::IfExpr(IfExpr {
                condition,
                then_branch,
                else_branch,
            }) => todo!(),
            AST::ImportStatement {
                import_clause,
                module,
            } => todo!(),
            AST::LetExpr { bindings, body } => todo!(),
            AST::MappedType {
                index,
                iterable,
                remapped_as,
                readonly_mod,
                optional_mod,
                body,
            } => todo!(),
            AST::MatchExpr { value, arms, else_ } => todo!(),
            AST::NamespaceAccess(_) => todo!(),
            AST::Never => todo!(),
            AST::NoOp => todo!(),
            AST::Null => todo!(),
            AST::Number(_) => todo!(),
            AST::ObjectLiteral(_) => todo!(),
            AST::Primitive(_) => todo!(),
            AST::Program(_) => todo!(),
            AST::Statement(_) => todo!(),
            AST::String(_) => todo!(),
            AST::TemplateString(_) => todo!(),
            AST::True => todo!(),
            AST::Tuple(_) => todo!(),
            AST::TypeAlias {
                export,
                name,
                params,
                body,
            } => todo!(),
            AST::Undefined => todo!(),
            AST::Unknown => todo!(),
        }
    }
}

/// Expands an if expression into a series of nested ternary expressions
fn expand_if_expr(condition: &AST, then: &AST, else_: &AST) -> AST {
    // Recursive operations
    let out = match condition {
        // Unary operators
        AST::ExtendsPrefixOp { op, value } => {
            match op {
                // Swap `then` and `else` branches
                PrefixOp::Not if value.is_compatible_with_not_prefix_op() => {
                    Some(expand_if_expr(value, else_, then))
                }
                PrefixOp::Infer => todo!(),
                _ => {
                    unreachable!("Expected `not` or `infer` prefix operator, found {condition:#?}")
                }
            }
        }
        AST::ExtendsBinOp { lhs, op, rhs } => match op {
            InfixOp::And => {
                let then = expand_if_expr(rhs, then, else_);
                Some(expand_if_expr(lhs, &then, else_))
            }
            InfixOp::Or => {
                let else_ = expand_if_expr(rhs, then, else_);
                Some(expand_if_expr(lhs, then, &else_))
            }
            _ => None,
        },
        _ => panic!("Expected extends operator, found {condition:#?}"),
    };

    if let Some(v) = out {
        return v;
    }

    // Terminal nodes
    match condition {
        // Binary operators
        AST::ExtendsBinOp { lhs, op, rhs } => {
            // TODO report a syntax error here
            // need to include spans in Node
            if !lhs.is_typescript_feature() {
                dbg!(&lhs);
                unreachable!("value must be desugared before this point");
            }

            if !rhs.is_typescript_feature() {
                dbg!(&rhs);
                unreachable!("value must be desugared before this point");
            }

            match op {
                // Equivalent to `lhs extends rhs ? then : else`
                InfixOp::Extends => AST::from(ExtendsExpr::new(
                    lhs.clone(),
                    rhs.clone(),
                    Box::new(then.clone()),
                    Box::new(else_.clone()),
                )),
                InfixOp::NotExtends => AST::from(ExtendsExpr::new(
                    lhs.clone(),
                    rhs.clone(),
                    Box::new(else_.clone()),
                    Box::new(then.clone()),
                )),
                InfixOp::Equals => todo!(),
                InfixOp::NotEquals => todo!(),
                InfixOp::StrictEquals => todo!(),
                InfixOp::StrictNotEquals => todo!(),
                _ => unreachable!(),
            }
        }
        _ => panic!("Expected extends operator, found {condition:#?}"),
    }
}

#[cfg(test)]
mod simplify_tests {

    use super::*;
    use crate::{
        ast::{
            self,
            AST::{self, *},
        },
        parser::Rule::expr,
        pest::Parser,
        test_support::parse,
    };
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn simplify_basic() {
        assert_eq!(
            parse!(expr, "if a <: b then c else d end").simplify(),
            AST::from(ast::ExtendsExpr::new(
                Box::new(Ident("a".to_string())),
                Box::new(Ident("b".to_string())),
                Box::new(Ident("c".to_string())),
                Box::new(Ident("d".to_string()))
            ))
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
            .simplify(),
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
            .simplify(),
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
            .simplify(),
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
            .simplify(),
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
            .simplify(),
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
            .simplify(),
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
            .simplify(),
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
        )
    }
}

fn resolve_let_bindings(bindings: &HashMap<Identifier, AST>) -> impl Fn(&AST) -> AST + '_ {
    |node: &AST| -> AST {
        match node {
            AST::Ident(name) => {
                if let Some(value) = bindings.get(&Identifier(name.clone())) {
                    value.clone()
                } else {
                    node.clone()
                }
            }
            AST::LetExpr {
                bindings: new_bindings,
                body,
            } => {
                let nested_bindings: HashMap<Identifier, AST> = bindings
                    .iter()
                    .chain(new_bindings.iter())
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();

                dbg!(&nested_bindings);

                let f = resolve_let_bindings(&nested_bindings);

                body.transform(&f)
            }
            _ => node.clone(),
        }
    }
}

impl typescript::Pretty for AST {
    fn to_ts<'a>(&self) -> RcDoc<()> {
        match self {
            node @ (AST::ExtendsPrefixOp { .. }
            | AST::MatchExpr { .. }
            | AST::CondExpr { .. }
            | AST::ExtendsBinOp { .. }) => {
                unreachable!("Node should be desugared before this point {:#?}", node)
            }

            AST::Program(stmnts) => {
                let mut doc = RcDoc::nil();
                for stmnt in stmnts {
                    doc = doc
                        .append(stmnt.to_ts())
                        .append(RcDoc::hardline())
                        .append(RcDoc::hardline());
                }
                doc
            }
            AST::TypeAlias {
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
            AST::Ident(ident) => RcDoc::text(ident),
            AST::Number(number) => RcDoc::text(number),
            AST::Primitive(primitive) => RcDoc::text(match primitive {
                PrimitiveType::Boolean => "boolean",
                PrimitiveType::Number => "number",
                PrimitiveType::String => "string",
            }),
            AST::String(string) => string_literal(string),
            AST::TemplateString(string) => RcDoc::text(string),
            AST::IfExpr(IfExpr { .. }) => {
                unreachable!("IfExpr should be desugared before this point");
            }
            AST::Access {
                lhs,
                rhs,
                is_dot: true,
            } => {
                let rhs = rhs
                    .as_ident()
                    .expect("rhs of dot access should be an ident");

                lhs.to_ts()
                    .append(RcDoc::text("["))
                    .append(string_literal(rhs))
                    .append(RcDoc::text("]"))
                    .group()
            }
            AST::Access { lhs, rhs, .. } => lhs
                .to_ts()
                .append(RcDoc::text("["))
                .append(rhs.to_ts())
                .append(RcDoc::text("]"))
                .group(),
            AST::ObjectLiteral(props) => {
                let sep = RcDoc::text(",").append(RcDoc::space());

                let props = RcDoc::intersperse(props.iter().map(|prop| prop.to_ts()), sep);

                RcDoc::text("{").append(props).append(RcDoc::text("}"))
            }
            AST::Application(ident, params) => {
                let sep = RcDoc::text(",").append(RcDoc::space());

                let params = RcDoc::intersperse(params.iter().map(|param| param.to_ts()), sep);

                RcDoc::text(ident).append(RcDoc::text("<").append(params).append(RcDoc::text(">")))
            }
            AST::Tuple(items) => {
                let sep = RcDoc::text(",").append(RcDoc::space());

                let items = RcDoc::intersperse(items.iter().map(|item| item.to_ts()), sep);

                RcDoc::text("[").append(items).append(RcDoc::text("]"))
            }
            AST::Array(value) => {
                let doc = if value.is_bin_op() {
                    parens(value.to_ts())
                } else {
                    value.to_ts()
                };

                doc.append(RcDoc::text("[]"))
            }
            AST::Null => RcDoc::text("null"),
            AST::Undefined => RcDoc::text("undefined"),
            AST::Never => RcDoc::text("never"),
            AST::Any => RcDoc::text("any"),
            AST::Unknown => RcDoc::text("unknown"),
            AST::True => RcDoc::text("true"),
            AST::False => RcDoc::text("false"),

            AST::BinOp { lhs, op, rhs } => {
                fn fmt(v: &AST) -> RcDoc<()> {
                    match v {
                        AST::BinOp { .. } => parens(v.to_ts()),
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

            AST::Builtin { name, argument } => name.to_ts().append(" ").append(argument.to_ts()),

            AST::ExtendsExpr(ExtendsExpr {
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
            AST::ExtendsBinOp {
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
            AST::Statement(stmnt) => stmnt.to_ts().append(RcDoc::text(";")),
            AST::MappedType {
                index: key,
                iterable,
                remapped_as,
                readonly_mod,
                optional_mod,
                body,
            } => {
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
            AST::LetExpr { .. } => {
                unreachable!("LetExpr should be desugared before this point")
            }
            AST::ImportStatement {
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
            AST::NamespaceAccess(NamespaceAccess { lhs, rhs }) => {
                lhs.to_ts().append(".").append(rhs.to_ts())
            }
            AST::NoOp => todo!(),
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
pub enum Expr {
    Value(Box<AST>),
    BinOp {
        lhs: Box<Expr>,
        op: Op,
        rhs: Box<Expr>,
    },
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
pub struct MatchArm {
    pub pattern: AST,
    pub body: AST,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CondArm {
    pub condition: AST,
    pub body: AST,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjectProperty {
    pub readonly: bool,
    pub optional: bool,
    pub key: String,
    pub value: AST,
}

impl typescript::Pretty for ObjectProperty {
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

impl typescript::Pretty for Identifier {
    fn to_ts(&self) -> RcDoc<()> {
        RcDoc::text(self.0.clone())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeParameter {
    pub name: String,
    pub constraint: Option<Box<AST>>,
    pub default: Option<Box<AST>>,
    pub rest: bool,
}

impl TypeParameter {
    pub fn new(
        name: String,
        constraint: Option<Box<AST>>,
        default: Option<Box<AST>>,
        rest: bool,
    ) -> Self {
        Self {
            name,
            constraint,
            default,
            rest,
        }
    }
}

impl typescript::Pretty for TypeParameter {
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
