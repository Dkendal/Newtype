use itertools::Itertools;
use node::{Node, Nodes};
use pest::Span;
use pretty::RcDoc as D;
use serde_derive::Serialize;

use crate::{
    extends_result::ExtendsResult,
    parser::{Pair, ParserError},
    pretty::{parens, string_literal, surround},
    runtime::{self, builtin},
    typescript,
};

pub(crate) mod errors;
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

pub(crate) mod node;

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
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

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
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

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
pub struct Tuple<'a> {
    pub items: Nodes<'a>,
}

impl PrettySexpr for Tuple<'_> {
    fn pretty_sexpr(&self) -> D<()> {
        Ast::sexpr(self.items.iter().map(|item| item.pretty_sexpr()).collect())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
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

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
pub struct MappedType<'a> {
    pub index: String,
    pub iterable: Node<'a>,
    pub remapped_as: Option<Node<'a>>,
    pub readonly_mod: Option<MappingModifier>,
    pub optional_mod: Option<MappingModifier>,
    pub body: Node<'a>,
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
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

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
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

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
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

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
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

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
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
            Ast::False => D::text("false"),
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
            Ast::String(str) => string_literal(str),
            Ast::TemplateString(_) => todo!(),
            Ast::True => D::text("true"),
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

    pub fn is_top_type(&self) -> bool {
        matches!(self, Ast::Any | Ast::Unknown)
    }

    pub fn is_empty_object(&self) -> bool {
        matches!(self, Ast::ObjectLiteral(ObjectLiteral { properties }) if properties.is_empty())
    }

    pub fn is_nullish(&self) -> bool {
        matches!(self, Ast::Null | Ast::Undefined)
    }

    pub fn is_bottom_type(&self) -> bool {
        matches!(self, Ast::Never)
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

    fn is_subtype(&self, other: &Ast<'a>) -> ExtendsResult {
        type A<'a> = Ast<'a>;
        type T = ExtendsResult;

        // comparisons with never always result in never
        if self.is_bottom_type() {
            return T::Never;
        }

        // everything extends both top types
        if other.is_top_type() {
            return T::True;
        }

        match self {
            x if !x.is_typescript_feature() => {
                unreachable!()
            }

            A::Any => T::Both,

            A::Unknown => T::False,

            A::True => match other {
                A::True => T::True,
                A::Primitive(PrimitiveType::Boolean) => T::True,
                _ => T::False,
            },

            A::False => match other {
                A::False => T::True,
                A::Primitive(PrimitiveType::Boolean) => T::True,
                _ => T::False,
            },

            A::Access { .. } => todo!(),

            A::Application(_) => todo!(),

            A::Array(_) => todo!(),

            A::InfixOp { .. } => todo!(),

            A::Builtin { .. } => todo!(),

            A::Ident(_) => todo!(),

            A::NamespaceAccess(_) => todo!(),

            A::Number(a) => match other {
                A::Number(b) => Into::into(a == b),
                A::Primitive(PrimitiveType::Number) => T::True,
                _ => T::False,
            },

            A::ObjectLiteral(_) => todo!(),

            A::Primitive(a) => match other {
                A::Primitive(b) => Into::into(a == b),
                _ => T::False,
            },

            A::String(a) => match other {
                A::String(b) => Into::into(a == b),
                A::Primitive(PrimitiveType::String) => T::True,
                _ => T::False,
            },

            A::TemplateString(a) => match other {
                A::TemplateString(b) => Into::into(a == b),
                A::Primitive(PrimitiveType::String) => T::True,
                _ => T::False,
            },

            A::Tuple(_) => todo!(),

            A::Undefined => match other {
                A::Undefined => T::True,
                _ => T::False,
            },

            A::Null => match other {
                A::Null => T::True,
                _ => T::False,
            },

            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{parser::Rule, test_support::*};
    static TRUE: ExtendsResult = ExtendsResult::True;
    static FALSE: ExtendsResult = ExtendsResult::False;
    static BOTH: ExtendsResult = ExtendsResult::Both;
    static NEVER: ExtendsResult = ExtendsResult::Never;

    use super::*;

    mod is_subtype {
        use super::*;

        mod never {
            use super::*;

            // #[test]
            // fn never() {
            //     assert_eq!(ast!("never").is_subtype(&ast!("never")), NEVER);
            // }
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

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
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

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
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

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
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

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
pub enum Op {
    Union,
    Intersection,
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
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

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
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

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
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

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
pub enum MappingModifier {
    Add,
    Remove,
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
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

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
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

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
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

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize)]
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

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
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

pub(crate) mod if_expr;

pub(crate) mod match_expr;

pub(crate) mod cond_expr;

pub(crate) mod let_expr;
