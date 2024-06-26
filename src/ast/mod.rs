use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use pretty::RcDoc;

use crate::{
    parser::ParserError,
    pretty::{parens, string_literal},
    ToTypescript,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Node {
    Program(Vec<Node>),
    Statement(Box<Node>),
    ImportStatement {
        import_clause: ImportClause,
        module: String,
    },
    TypeAlias {
        export: bool,
        name: String,
        params: Vec<TypeParameter>,
        body: Box<Node>,
    },
    BinOp {
        lhs: Box<Node>,
        op: Op,
        rhs: Box<Node>,
    },
    Ident(String),
    Number(String),
    Primitive(PrimitiveType),
    String(String),
    TemplateString(String),
    IfExpr(
        Box<Node>,         // condition, must be ExtendsBinOp
        Box<Node>,         // then
        Option<Box<Node>>, // else
    ),
    MappedType {
        index: String,
        iterable: Box<Node>,
        remapped_as: Option<Box<Node>>,
        readonly_mod: Option<MappingModifier>,
        optional_mod: Option<MappingModifier>,
        body: Box<Node>,
    },
    MatchExpr {
        value: Box<Node>,
        arms: Vec<MatchArm>,
        else_: Box<Node>,
    },
    CondExpr {
        arms: Vec<CondArm>,
        else_: Box<Node>,
    },
    LetExpr {
        bindings: HashMap<Identifier, Node>,
        body: Box<Node>,
    },
    ExtendsPrefixOp {
        op: PrefixOp,
        value: Box<Node>,
    },
    ExtendsBinOp {
        lhs: Box<Node>,
        op: InfixOp,
        rhs: Box<Node>,
    },
    ExtendsExpr(
        Box<Node>, // lhs
        Box<Node>, // rhs
        Box<Node>, // then
        Box<Node>, // else
    ),
    Builtin {
        name: BuiltInKeyword,
        argument: Box<Node>,
    },
    Error(ParserError),
    ObjectLiteral(Vec<ObjectProperty>),
    Application(String, Vec<Node>),
    Never,
    Any,
    Unknown,
    Tuple(Vec<Node>),
    Array(Box<Node>),
    Access {
        lhs: Box<Node>,
        rhs: Box<Node>,
        is_dot: bool,
    },
    Null,
    Undefined,
    False,
    True,
}

impl Node {
    /**
     * Recursively transform a node and all of its children. There is no generalize
     * tree walk method in Rust to deal with ADTs, so we have to implement it manually.
     */
    pub fn transform(&self, f: &impl Fn(&Self) -> Self) -> Self {
        let transform = |node: &Node| node.transform(f);

        let transform_and_box = |node: &Node| Box::new(node.transform(f));

        let transform_each =
            |nodes: &Vec<Node>| nodes.into_iter().map(transform).collect::<Vec<_>>();

        // For all nodes that are not a leaf node,
        // we need to recursively simplify
        let out = match self {
            // Leaf nodes are not transformed
            Node::Error { .. }
            | Node::Never
            | Node::Any
            | Node::Unknown
            | Node::Null
            | Node::Undefined
            | Node::False
            | Node::True
            | Node::Ident { .. }
            | Node::Number { .. }
            | Node::Primitive { .. }
            | Node::String { .. }
            | Node::ImportStatement { .. }
            | Node::TemplateString { .. } => self.clone(),

            // For all other nodes, we recursively transform
            Node::Program(vec) => Node::Program(transform_each(vec)),
            Node::TypeAlias {
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

                Node::TypeAlias {
                    export: *export,
                    name: name.clone(),
                    params,
                    body: Box::new(body.transform(f)),
                }
            }
            Node::Tuple(vec) => Node::Tuple(transform_each(vec)),
            Node::Array(vec) => Node::Array(transform_and_box(vec)),
            Node::Access { lhs, rhs, is_dot } => Node::Access {
                lhs: transform_and_box(lhs),
                rhs: transform_and_box(rhs),
                is_dot: *is_dot,
            },
            Node::IfExpr(cond, then, els) => Node::IfExpr(
                transform_and_box(cond),
                transform_and_box(then),
                els.as_ref().map(|v| transform_and_box(v)),
            ),
            Node::BinOp { lhs, op, rhs } => Node::BinOp {
                lhs: transform_and_box(lhs),
                op: op.clone(),
                rhs: transform_and_box(rhs),
            },
            Node::ExtendsBinOp { lhs, op, rhs } => Node::ExtendsBinOp {
                lhs: transform_and_box(lhs),
                op: op.clone(),
                rhs: transform_and_box(rhs),
            },

            Node::ExtendsPrefixOp { op, value } => Node::ExtendsPrefixOp {
                op: op.clone(),
                value: transform_and_box(value),
            },

            Node::ExtendsExpr(lhs, rhs, then, els) => Node::ExtendsExpr(
                transform_and_box(lhs),
                transform_and_box(rhs),
                transform_and_box(then),
                transform_and_box(els),
            ),
            Node::ObjectLiteral(props) => Node::ObjectLiteral(
                props
                    .into_iter()
                    .map(|prop| {
                        let mut p = prop.clone();
                        p.value = transform(&prop.value);
                        p
                    })
                    .collect(),
            ),
            Node::Application(name, args) => Node::Application(name.clone(), transform_each(args)),

            Node::MatchExpr { value, arms, else_ } => {
                let value = transform_and_box(value);

                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        let mut a = arm.clone();
                        a.pattern = transform(&arm.pattern);
                        a.body = transform(&arm.body);
                        a
                    })
                    .collect();

                let else_ = Box::new(transform(else_));

                Node::MatchExpr { value, arms, else_ }
            }

            Node::CondExpr { arms, else_ } => {
                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        let mut a = arm.clone();
                        a.condition = transform(&arm.condition);
                        a.body = transform(&arm.body);
                        a
                    })
                    .collect();

                let else_ = Box::new(transform(else_));

                Node::CondExpr { arms, else_ }
            }

            Node::Builtin { name, argument } => Node::Builtin {
                name: name.clone(),
                argument: transform_and_box(argument),
            },
            Node::Statement(node) => Node::Statement(Box::new(node.transform(f))),

            Node::MappedType {
                index,
                iterable,
                remapped_as,
                readonly_mod,
                optional_mod,
                body,
            } => Node::MappedType {
                index: index.clone(),
                iterable: Box::new(iterable.transform(f)),
                remapped_as: remapped_as.clone(),
                readonly_mod: readonly_mod.clone(),
                optional_mod: optional_mod.clone(),
                body: Box::new(body.transform(f)),
            },
            Node::LetExpr { bindings, body } => (**body)
                .clone()
                .transform(&resolve_let_bindings(bindings))
                .transform(f),
        };

        f(&out)
    }
}

fn resolve_let_bindings<'a>(
    bindings: &'a HashMap<Identifier, Node>,
) -> impl Fn(&Node) -> Node + 'a {
    |node: &Node| -> Node {
        match node {
            Node::Ident(name) => {
                if let Some(value) = bindings.get(&Identifier(name.clone())) {
                    value.clone()
                } else {
                    node.clone()
                }
            }
            Node::LetExpr {
                bindings: new_bindings,
                body,
            } => {
                let nested_bindings: HashMap<Identifier, Node> = bindings
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

impl ToTypescript for Node {
    fn to_ts<'a>(&self) -> RcDoc<()> {
        match self {
            Node::Program(stmnts) => {
                let mut doc = RcDoc::nil();
                for stmnt in stmnts {
                    doc = doc.append(stmnt.to_ts()).append(RcDoc::hardline());
                }
                doc
            }
            Node::TypeAlias {
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
                    list if list.len() == 0 => RcDoc::nil(),
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
            Node::Ident(ident) => RcDoc::text(ident),
            Node::Number(number) => RcDoc::text(number),
            Node::Primitive(primitive) => RcDoc::text(match primitive {
                PrimitiveType::Boolean => "boolean",
                PrimitiveType::Number => "number",
                PrimitiveType::String => "string",
            }),
            Node::String(string) => string_literal(string),
            Node::TemplateString(string) => RcDoc::text(string),
            Node::IfExpr(_cond, _then, _els) => {
                unreachable!("IfExpr should be desugared before this point");
            }
            Node::Access {
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
            Node::Access { lhs, rhs, .. } => lhs
                .to_ts()
                .append(RcDoc::text("["))
                .append(rhs.to_ts())
                .append(RcDoc::text("]"))
                .group(),
            Node::Error(_) => unreachable!("Error should be handled before this point"),
            Node::ObjectLiteral(props) => {
                let sep = RcDoc::text(",").append(RcDoc::space());

                let props = RcDoc::intersperse(props.iter().map(|prop| prop.to_ts()), sep);

                RcDoc::text("{").append(props).append(RcDoc::text("}"))
            }
            Node::Application(ident, params) => {
                let sep = RcDoc::text(",").append(RcDoc::space());

                let params = RcDoc::intersperse(params.iter().map(|param| param.to_ts()), sep);

                RcDoc::text(ident).append(RcDoc::text("<").append(params).append(RcDoc::text(">")))
            }
            Node::Tuple(items) => {
                let sep = RcDoc::text(",").append(RcDoc::space());

                let items = RcDoc::intersperse(items.iter().map(|item| item.to_ts()), sep);

                RcDoc::text("[").append(items).append(RcDoc::text("]"))
            }
            Node::Array(value) => {
                let doc = if value.is_bin_op() {
                    parens(value.to_ts())
                } else {
                    value.to_ts()
                };

                doc.append(RcDoc::text("[]"))
            }
            Node::Null => RcDoc::text("null"),
            Node::Undefined => RcDoc::text("undefined"),
            Node::Never => RcDoc::text("never"),
            Node::Any => RcDoc::text("any"),
            Node::Unknown => RcDoc::text("unknown"),
            Node::True => RcDoc::text("true"),
            Node::False => RcDoc::text("false"),

            Node::BinOp { lhs, op, rhs } => {
                fn fmt(v: &Node) -> RcDoc<()> {
                    match v {
                        Node::BinOp { .. } => parens(v.to_ts()),
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

            Node::Builtin { name, argument } => name.to_ts().append(" ").append(argument.to_ts()),

            Node::ExtendsExpr(lhs, rhs, then, els) => {
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
            Node::ExtendsBinOp {
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

            Node::ExtendsBinOp { .. } => {
                unreachable!("ExtendsBinOp should be desugared before this point")
            }
            Node::ExtendsPrefixOp { .. } => {
                unreachable!("ExtendsPrefixOp should be desugared before this point")
            }
            Node::MatchExpr { .. } => {
                unreachable!("MatchExpr should be desugared before this point")
            }
            Node::CondExpr { .. } => {
                unreachable!("CondExpr should be desugared before this point")
            }
            Node::Statement(stmnt) => stmnt.to_ts().append(RcDoc::text(";")),
            Node::MappedType {
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
            Node::LetExpr { .. } => {
                unreachable!("LetExpr should be desugared before this point")
            }
            Node::ImportStatement {
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
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ImportClause {
    Named(Vec<ImportSpecifier>),
    Namespace { alias: Identifier },
}

impl ToTypescript for ImportClause {
    fn to_ts(&self) -> RcDoc<()> {
        match self {
            ImportClause::Named(specifiers) => {
                let sep = RcDoc::text(",").append(RcDoc::line());

                let specifiers =
                    RcDoc::intersperse(specifiers.iter().map(ToTypescript::to_ts), sep);

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

impl ToTypescript for ImportSpecifier {
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

impl ToTypescript for BuiltInKeyword {
    fn to_ts(&self) -> RcDoc<()> {
        match self {
            BuiltInKeyword::Keyof => RcDoc::text("keyof"),
        }
    }
}

impl Node {
    pub fn is_bin_op(&self) -> bool {
        match self {
            Node::BinOp { .. } => true,
            _ => false,
        }
    }

    pub fn as_ident(&self) -> Option<&String> {
        if let Self::Ident(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Value(Box<Node>),
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
    pub pattern: Node,
    pub body: Node,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CondArm {
    pub condition: Node,
    pub body: Node,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjectProperty {
    pub readonly: bool,
    pub optional: bool,
    pub key: String,
    pub value: Node,
}

impl ToTypescript for ObjectProperty {
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

impl ToTypescript for Identifier {
    fn to_ts(&self) -> RcDoc<()> {
        RcDoc::text(self.0.clone())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeParameter {
    pub name: String,
    pub constraint: Option<Box<Node>>,
    pub default: Option<Box<Node>>,
    pub rest: bool,
}

impl TypeParameter {
    pub fn new(
        name: String,
        constraint: Option<Box<Node>>,
        default: Option<Box<Node>>,
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

impl ToTypescript for TypeParameter {
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
