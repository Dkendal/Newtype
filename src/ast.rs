use std::collections::{HashMap, HashSet};

use crate::parser::ParserError;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Node {
    Program(Vec<Node>),
    Statement(Box<Node>),
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
        body: Box<Node>
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BuiltInKeyword {
    Keyof,
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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Identifier(pub String);

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
