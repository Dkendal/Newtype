use std::fmt::Display;

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
pub(crate) mod node;

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Path<'a> {
    pub segments: Nodes<'a>,
}

impl<'a> Path<'a> {
    fn simplify(&self, span: Option<Span<'a>>) -> Node<'a> {
        let mut acc = vec![];

        for seg in self.segments.iter() {
            if let Ast::Path(path) = seg.value.as_ref() {
                acc.extend(path.segments.clone());
            } else {
                acc.push(seg.clone());
            }
        }

        let ast = Ast::Path(Path { segments: acc });

        Node::new(span, ast)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
#[serde(rename_all = "kebab-case")]
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
#[serde(rename_all = "kebab-case")]
pub struct Tuple<'a> {
    pub items: Nodes<'a>,
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct ApplyGeneric<'a> {
    pub receiver: Node<'a>,
    pub args: Nodes<'a>,
}

impl<'a> typescript::Pretty for ApplyGeneric<'a> {
    fn to_ts(&self) -> D<()> {
        let sep = D::text(",").append(D::space());

        let generic_inner = D::intersperse(self.args.iter().map(|param| param.to_ts()), sep);

        let generic_params = D::text("<").append(generic_inner).append(D::text(">"));

        self.receiver.to_ts().append(generic_params)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct MappedType<'a> {
    pub index: String,
    pub iterable: Node<'a>,
    pub remapped_as: Option<Node<'a>>,
    pub readonly_mod: Option<MappingModifier>,
    pub optional_mod: Option<MappingModifier>,
    pub body: Node<'a>,
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct ObjectLiteral<'a> {
    pub properties: Vec<ObjectProperty<'a>>,
}

impl<'a> ObjectLiteral<'a> {
    pub fn is_empty(&self) -> bool {
        self.properties.is_empty()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, ObjectProperty<'a>> {
        self.properties.iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, ObjectProperty<'a>> {
        self.properties.iter_mut()
    }
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
#[serde(rename_all = "kebab-case")]
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
#[serde(rename_all = "kebab-case")]
pub struct UnitTest<'a> {
    pub name: String,
    pub body: Vec<Node<'a>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct MacroCall<'a> {
    pub name: String,
    pub args: Vec<Node<'a>>,
}

impl<'a> MacroCall<'a> {
    fn eval(&self) -> Node<'a> {
        let name = self.name.strip_suffix("!").unwrap();

        match name {
            "dbg!" => match self.args.as_slice() {
                [node] => builtin::dbg(node.to_owned()),
                _ => panic!("dbg! expects exactly one argument"),
            },
            "assert_equal" => match self.args.as_slice() {
                [lhs, rhs] => builtin::assert_equal(lhs.to_owned(), rhs.to_owned()),
                _ => panic!("assert_equal! expects exactly two arguments"),
            },
            "unquote" => match self.args.as_slice() {
                [node] => builtin::unquote(node.to_owned()),
                _ => panic!("unquote! expects exactly one argument"),
            },
            id => unimplemented!("macro {} not implemented", id),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum Ast<'a> {
    #[serde(rename(serialize = "."))]
    Access {
        lhs: Node<'a>,
        rhs: Node<'a>,
        is_dot: bool,
    },
    Boolean(bool),
    Any,
    #[serde(rename(serialize = "macro"))]
    MacroCall(MacroCall<'a>),
    #[serde(rename(serialize = "apply"))]
    ApplyGeneric(ApplyGeneric<'a>),
    Array(Node<'a>),
    #[serde(rename(serialize = "|"))]
    UnionType {
        types: Vec<Node<'a>>,
    },
    #[serde(rename(serialize = "&"))]
    IntersectionType {
        types: Vec<Node<'a>>,
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
    Infer(Node<'a>),
    ExtendsPrefixOp {
        op: PrefixOp,
        value: Node<'a>,
    },
    Ident(Identifier),
    #[serde(rename(serialize = "if"))]
    IfExpr(if_expr::IfExpr<'a>),
    #[serde(rename(serialize = "import"))]
    ImportStatement {
        import_clause: ImportClause,
        module: String,
    },
    LetExpr(let_expr::LetExpr<'a>),
    MappedType(MappedType<'a>),
    MatchExpr(match_expr::MatchExpr<'a>),
    #[serde(rename(serialize = "::"))]
    Path(Path<'a>),
    Never,
    NoOp,
    Number(String),
    TypeLiteral(ObjectLiteral<'a>),
    Primitive(PrimitiveType),
    Program(Nodes<'a>),
    Statement(Node<'a>),
    UnitTest(UnitTest<'a>),
    String(String),
    TemplateString(String),
    Tuple(Tuple<'a>),
    #[serde(rename(serialize = "type"))]
    TypeAlias {
        export: bool,
        name: Identifier,
        params: Vec<TypeParameter<'a>>,
        body: Node<'a>,
    },
    Unknown,
    Interface(Interface<'a>),
}

impl<'a> From<if_expr::IfExpr<'a>> for Ast<'a> {
    fn from(v: if_expr::IfExpr<'a>) -> Self {
        Self::IfExpr(v)
    }
}

impl<'a> From<match_expr::MatchExpr<'a>> for Ast<'a> {
    fn from(v: match_expr::MatchExpr<'a>) -> Self {
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
            Ast::Primitive(primitive) => D::text(primitive.to_string()),
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
            Ast::TypeLiteral(value) => value.to_ts(),
            Ast::ApplyGeneric(value) => value.to_ts(),
            Ast::Tuple(Tuple { items }) => {
                let sep = D::text(",").append(D::space());

                let items = D::intersperse(items.iter().map(|item| item.to_ts()), sep);

                D::text("[").append(items).append(D::text("]"))
            }
            Ast::Array(node) => {
                let doc = if node.value.is_set_op() {
                    parens(node.to_ts())
                } else {
                    node.to_ts()
                };

                doc.append(D::text("[]"))
            }
            Ast::Never => D::text("never"),
            Ast::Any => D::text("any"),
            Ast::Unknown => D::text("unknown"),
            Ast::Boolean(value) => D::text(value.to_string()),
            Ast::Infer(value) => D::text("infer").append(D::space()).append(value.to_ts()),

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
            Ast::Path(Path { segments }) => {
                let sep = D::text(".");

                let segments = D::intersperse(segments.iter().map(|seg| seg.to_ts()), sep);

                segments
            }
            Ast::Interface(value) => {
                return value.to_ts();
            }
            Ast::UnitTest(_) => D::nil(),
            Ast::MacroCall(_) => unreachable!("MacroCall should be desugared before this point"),
            Ast::UnionType { types } => {
                let sep = D::line().append(D::text("|")).append(D::space());
                D::intersperse(
                    types.iter().map(|t| match t.value.as_ref() {
                        Ast::IntersectionType { .. } => surround(t.to_ts(), "(", ")"),
                        _ => t.to_ts(),
                    }),
                    sep,
                )
                .group()
            }
            Ast::IntersectionType { types } => {
                let sep = D::line().append(D::text("&")).append(D::space());
                D::intersperse(types.iter().map(|t| t.to_ts()), sep).group()
            }
            Ast::NoOp => unimplemented!(),
            node @ (Ast::ExtendsPrefixOp { .. }
            | Ast::MatchExpr(match_expr::MatchExpr { .. })
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

impl<'a> From<ExtendsExpr<'a>> for Ast<'a> {
    fn from(v: ExtendsExpr<'a>) -> Self {
        Ast::ExtendsExpr(v)
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
        matches!(self, Ast::TypeLiteral(ObjectLiteral { properties }) if properties.is_empty())
    }

    pub fn is_nullish(&self) -> bool {
        matches!(
            self,
            Ast::Primitive(PrimitiveType::Undefined | PrimitiveType::Null | PrimitiveType::Void)
        )
    }

    fn is_non_nullish(&self) -> bool {
        !self.is_nullish()
    }

    fn is_non_primitive(&self) -> bool {
        !self.is_primitive()
    }

    fn is_object_interface(&self) -> bool {
        self.has_identifier("Object")
    }

    fn has_identifier(&self, name: &str) -> bool {
        matches!(self, Ast::Ident(Identifier(value)) if value == name)
    }

    fn is_primitive(&self) -> bool {
        matches!(self, Ast::Primitive(_))
    }

    pub fn is_bottom_type(&self) -> bool {
        matches!(self, Ast::Never)
    }

    pub fn get_primitive_type(&self) -> Option<PrimitiveType> {
        type P = PrimitiveType;

        let value = match self {
            Ast::String(_) => P::String,

            Ast::Number(_) => P::Number,

            Ast::Boolean(_) => P::Boolean,

            Ast::TypeLiteral(_) => P::Object,

            Ast::Ident(Identifier(name)) => match name.as_str() {
                "String" => P::String,
                "Number" => P::Number,
                "Boolean" => P::Boolean,
                "Object" => P::Object,
                "Symbol" => P::Symbol,
                "BigInt" => P::BigInt,
                _ => return None,
            },
            _ => return None,
        };

        Some(value)
    }

    pub fn is_object_wrapper(&self) -> bool {
        matches!(self, Ast::Ident(Identifier(name)) if matches!(name.as_str(),
                "Boolean" | "Number" | "String" | "Object" | "Symbol" | "BigInt"
        ))
    }

    pub fn is_object_wrapper_for(&self, object_wrapper_name: &str) -> bool {
        matches!(self, Ast::Ident(Identifier(name)) if name.as_str() == object_wrapper_name)
    }

    pub fn is_string_object_wrapper(&self) -> bool {
        self.is_object_wrapper_for("String")
    }

    pub fn is_number_object_wrapper(&self) -> bool {
        self.is_object_wrapper_for("Number")
    }

    pub fn is_boolean_object_wrapper(&self) -> bool {
        self.is_object_wrapper_for("Boolean")
    }

    pub fn is_object_object_wrapper(&self) -> bool {
        self.is_object_wrapper_for("Object")
    }

    pub fn is_symbol_object_wrapper(&self) -> bool {
        self.is_object_wrapper_for("Symbol")
    }

    pub fn is_bigint_object_wrapper(&self) -> bool {
        self.is_object_wrapper_for("BigInt")
    }

    pub fn get_object_wrapper(&self) -> Option<Ast> {
        let name = match self {
            Ast::Primitive(p) => {
                type P = PrimitiveType;

                match p {
                    P::Boolean => "Boolean",
                    P::Number => "Number",
                    P::String => "String",
                    P::Object => "Object",
                    P::Symbol => "Symbol",
                    P::BigInt => "BigInt",
                    P::Void | P::Undefined | P::Null => return None,
                }
            }
            ast if ast.is_object_interface() => return None,
            _ => "Object",
        };
        Some(Ast::Ident(Identifier(name.to_string())))
    }

    /// Anything that returns true is a feature that has a direct equivalent in TypeScript.
    /// Anything that's false is a feature that needs to be desugared.
    pub fn is_typescript_feature(&self) -> bool {
        match self {
            Ast::ExtendsPrefixOp { .. }
            | Ast::IfExpr { .. }
            | Ast::LetExpr(_)
            | Ast::MatchExpr(_)
            | Ast::NoOp
            | Ast::TypeAlias { .. }
            | Ast::UnitTest(_)
            | Ast::MacroCall(_)
            | Ast::CondExpr(_)
            | Ast::ExtendsInfixOp { .. } => false,
            Ast::IntersectionType { .. }
            | Ast::ExtendsExpr(_)
            | Ast::Ident(_)
            | Ast::Infer { .. }
            | Ast::ImportStatement { .. }
            | Ast::MappedType(_)
            | Ast::Path(_)
            | Ast::Never
            | Ast::Number(_)
            | Ast::TypeLiteral(_)
            | Ast::Primitive(_)
            | Ast::Program(_)
            | Ast::Statement(_)
            | Ast::String(_)
            | Ast::TemplateString(_)
            | Ast::Tuple(_)
            | Ast::Unknown
            | Ast::Interface(_)
            | Ast::Boolean(_)
            | Ast::UnionType { .. }
            | Ast::Access { .. }
            | Ast::Any
            | Ast::ApplyGeneric(_)
            | Ast::Array(_)
            | Ast::Builtin { .. } => true,
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
    pub fn is_set_op(&self) -> bool {
        matches!(self, Self::UnionType { .. } | Self::IntersectionType { .. })
    }

    #[must_use]
    pub fn is_union(&self) -> bool {
        matches!(self, Self::UnionType { .. })
    }

    #[must_use]
    pub fn is_intersection(&self) -> bool {
        matches!(self, Self::IntersectionType { .. })
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

        if let Ast::Primitive(other) = other {
            if let Some(value) = self.get_primitive_type() {
                if value == *other {
                    return T::True;
                }
            }
        }

        match (self, other) {
            (x, _) if !x.is_typescript_feature() => {
                unreachable!()
            }

            (A::Never, _) => T::Never,

            (lhs, rhs) if lhs == rhs => T::True,

            (_, rhs) if rhs.is_top_type() => T::True,

            (A::Any, _) => T::Both,

            (A::Unknown, _) => T::False,

            (_, A::Never) => T::False,

            (A::Access { .. }, _) => todo!(),

            (A::ApplyGeneric(_), _) => todo!(),

            (A::Array(_), _) => todo!(),

            (A::Builtin { .. }, _) => todo!(),

            (A::Path(_), _) => todo!(),

            (lhs, rhs) if lhs.is_non_nullish() && rhs.is_object_object_wrapper() => T::True,

            (A::TypeLiteral(lhs), _) if lhs.is_empty() => match other {
                Ast::Primitive(PrimitiveType::Object) => T::True,
                ast if ast.is_object_interface() => T::True,
                _ => T::False,
            },

            (A::TypeLiteral(_), _) => todo!(),

            (A::Primitive(lhs), A::Primitive(rhs)) => Into::into(lhs == rhs),

            (A::TemplateString(_) | A::String(_), A::Primitive(PrimitiveType::String)) => T::True,

            (A::Primitive(PrimitiveType::String) | A::TemplateString(_) | A::String(_), rhs)
                if rhs.is_string_object_wrapper() =>
            {
                T::True
            }

            (A::Number(_), A::Primitive(PrimitiveType::Number)) => T::True,

            // Object wrappers are equivalent to their primitive types in
            // this context.
            (lhs, rhs) if lhs.is_object_wrapper() => {
                let primitive_type = lhs.get_primitive_type().unwrap();
                Ast::Primitive(primitive_type).is_subtype(rhs)
            }

            (A::Ident(_), _) => {
                todo!()
            }

            (A::Tuple(_), _) => todo!(),

            (a, b) => {
                dbg!(a, b);
                T::False
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{parser::Rule, test_support::*};
    use rstest::rstest;
    static TRUE: ExtendsResult = ExtendsResult::True;
    static FALSE: ExtendsResult = ExtendsResult::False;
    static BOTH: ExtendsResult = ExtendsResult::Both;
    static NEVER: ExtendsResult = ExtendsResult::Never;

    use super::*;

    #[rstest]
    // never
    #[case("never", "any", NEVER)]
    #[case("never", "unknown", NEVER)]
    #[case("never", "{}", NEVER)]
    #[case("never", "[]", NEVER)]
    #[case("never", "string", NEVER)]
    #[case("never", "number", NEVER)]
    #[case("never", "object", NEVER)]
    #[case("never", "boolean", NEVER)]
    #[case("never", "Object", NEVER)]
    #[case("never", "1", NEVER)]
    #[case("never", "'string'", NEVER)]
    #[case("never", "true", NEVER)]
    #[case("never", "false", NEVER)]
    #[case("never", "null", NEVER)]
    #[case("never", "undefined", NEVER)]
    #[case("never", "never", NEVER)]
    // any
    #[case("any", "any", TRUE)]
    #[case("any", "unknown", TRUE)]
    #[case("any", "{}", BOTH)]
    #[case("any", "[]", BOTH)]
    #[case("any", "string", BOTH)]
    #[case("any", "number", BOTH)]
    #[case("any", "boolean", BOTH)]
    #[case("any", "object", BOTH)]
    #[case("any", "Object", BOTH)]
    #[case("any", "1", BOTH)]
    #[case("any", "'string'", BOTH)]
    #[case("any", "true", BOTH)]
    #[case("any", "false", BOTH)]
    #[case("any", "null", BOTH)]
    #[case("any", "undefined", BOTH)]
    #[case("any", "never", BOTH)]
    // unknown
    #[case("unknown", "any", TRUE)]
    #[case("unknown", "unknown", TRUE)]
    #[case("unknown", "{}", FALSE)]
    #[case("unknown", "[]", FALSE)]
    #[case("unknown", "string", FALSE)]
    #[case("unknown", "number", FALSE)]
    #[case("unknown", "boolean", FALSE)]
    #[case("unknown", "object", FALSE)]
    #[case("unknown", "Object", FALSE)]
    #[case("unknown", "1", FALSE)]
    #[case("unknown", "'string'", FALSE)]
    #[case("unknown", "true", FALSE)]
    #[case("unknown", "false", FALSE)]
    #[case("unknown", "null", FALSE)]
    #[case("unknown", "undefined", FALSE)]
    #[case("unknown", "never", FALSE)]
    // {}
    #[case("{}", "any", TRUE)]
    #[case("{}", "unknown", TRUE)]
    #[case("{}", "{}", TRUE)]
    #[case("{}", "{ x: string }", FALSE)]
    #[case("{}", "[]", FALSE)]
    #[case("{}", "[{}]", FALSE)]
    #[case("{}", "{}[]", FALSE)]
    #[case("{}", "string", FALSE)]
    #[case("{}", "number", FALSE)]
    #[case("{}", "boolean", FALSE)]
    #[case("{}", "bitint", FALSE)]
    #[case("{}", "symbol", FALSE)]
    #[case("{}", "object", TRUE)]
    #[case("{}", "Object", TRUE)]
    #[case("{}", "Function", FALSE)]
    #[case("{}", "String", FALSE)]
    #[case("{}", "1", FALSE)]
    #[case("{}", "'string'", FALSE)]
    #[case("{}", "`string${var}`", FALSE)]
    #[case("{}", "true", FALSE)]
    #[case("{}", "false", FALSE)]
    #[case("{}", "null", FALSE)]
    #[case("{}", "undefined", FALSE)]
    #[case("{}", "never", FALSE)]
    // string
    #[case("string", "any", TRUE)]
    #[case("string", "unknown", TRUE)]
    #[case("string", "{}", FALSE)]
    #[case("string", "[]", FALSE)]
    #[case("string", "string", TRUE)]
    #[case("string", "number", FALSE)]
    #[case("string", "boolean", FALSE)]
    #[case("string", "Object", TRUE)]
    #[case("string", "Function", FALSE)]
    #[case("string", "String", TRUE)]
    #[case("string", "1", FALSE)]
    #[case("string", "'string'", FALSE)]
    #[case("string", "true", FALSE)]
    #[case("string", "false", FALSE)]
    #[case("string", "null", FALSE)]
    #[case("string", "undefined", FALSE)]
    #[case("string", "never", FALSE)]
    // String
    #[case("String", "any", TRUE)]
    #[case("String", "unknown", TRUE)]
    #[case("String", "{}", FALSE)]
    #[case("String", "[]", FALSE)]
    #[case("String", "string", TRUE)]
    #[case("String", "number", FALSE)]
    #[case("String", "boolean", FALSE)]
    #[case("String", "Object", TRUE)]
    #[case("String", "Function", FALSE)]
    #[case("String", "String", TRUE)]
    #[case("String", "1", FALSE)]
    #[case("String", "'string'", FALSE)]
    #[case("String", "true", FALSE)]
    #[case("String", "false", FALSE)]
    #[case("String", "null", FALSE)]
    #[case("String", "undefined", FALSE)]
    #[case("String", "never", FALSE)]
    #[trace]
    fn is_subtype(#[case] a: &str, #[case] b: &str, #[case] expected: ExtendsResult) {
        assert_eq!(ast!(a).is_subtype(&ast!(b)), expected);
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
    use lexpr::sexp;
    use pretty_assertions::{assert_eq, assert_ne};
    use rstest::rstest;

    #[rstest]
    #[case(
        "A::B::C::D",
        sexp!((:: (segments (ident . "A") (ident . "B") (ident . "C") (ident . "D"))))
    )]
    fn test_simplify_expr(#[case] input: &str, #[case] expected: lexpr::Value) {
        use crate::parser::{self, Rule};

        let result = parser::NewtypeParser::parse(Rule::expr, input);

        match result {
            Ok(pairs) => {
                let actual = parser::parse_expr(pairs).simplify();

                pretty_assertions::assert_eq!(
                    actual.to_sexp().unwrap().to_string(),
                    expected.to_string()
                );
            }
            Err(err) => {
                panic!("{}", err);
            }
        }
    }

    #[test]
    fn simplify_basic() {
        assert_eq!(
            parse!(expr, "if a <: b then c else d end")
                .simplify()
                .to_sexp()
                .unwrap()
                .to_string(),
            lexpr::sexp!(
                (#"extends-expr"
                    (lhs ident . "a")
                    (rhs ident . "b")
                    (#"then-branch" ident . "c")
                    (#"else-branch" ident . "d")))
            .to_string()
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
            .to_sexp()
            .unwrap(),
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
            .to_sexp()
            .unwrap(),
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
            .to_sexp()
            .unwrap(),
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
            .to_sexp()
            .unwrap(),
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
            .to_sexp()
            .unwrap(),
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
            .to_sexp()
            .unwrap(),
        )
    }

    #[test]
    fn simplify_match_expr() {
        assert_eq!(
            parse!(
                expr,
                r#"
                match A do
                    number -> 1,
                    string -> 2,
                    else -> 3
                end
                "#
            )
            .simplify()
            .to_sexp()
            .unwrap(),
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
            .to_sexp()
            .unwrap()
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
#[serde(rename_all = "kebab-case")]
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
#[serde(rename_all = "kebab-case")]
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
#[serde(rename_all = "kebab-case")]
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
#[serde(rename_all = "kebab-case")]
pub enum PrimitiveType {
    Boolean,
    Number,
    String,
    Object,
    Symbol,
    BigInt,
    Void,
    Undefined,
    Null,
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimitiveType::Boolean => write!(f, "boolean"),
            PrimitiveType::Number => write!(f, "number"),
            PrimitiveType::String => write!(f, "string"),
            PrimitiveType::Object => write!(f, "object"),
            PrimitiveType::Symbol => write!(f, "symbol"),
            PrimitiveType::BigInt => write!(f, "bigint"),
            PrimitiveType::Void => write!(f, "void"),
            PrimitiveType::Undefined => write!(f, "undefined"),
            PrimitiveType::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
#[serde(rename_all = "kebab-case")]
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
#[serde(rename_all = "kebab-case")]
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
#[serde(rename_all = "kebab-case")]
pub enum MappingModifier {
    Add,
    Remove,
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
#[serde(rename_all = "kebab-case")]
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
#[serde(rename_all = "kebab-case")]
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
#[serde(rename_all = "kebab-case")]
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
#[serde(rename_all = "kebab-case")]
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
#[serde(rename_all = "kebab-case")]
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
