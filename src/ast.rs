use crate::parser::ParserError;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Node {
    Program(Vec<Node>),
    TypeAlias(
        String,    // Ident
        Vec<Node>, // TypeParams
        Box<Node>, // Expression
    ),
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
    MatchExpr{
        value: Box<Node>,
        arms: Vec<MatchArm>,
    },
    CondExpr{
        arms: Vec<CondArm>,
        else_: Box<Node>,
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
    Error(ParserError),
    ObjectLiteral(Vec<ObjectProperty>),
    Application(String, Vec<Node>),
    Never,
    Any,
    Unknown,
    Tuple(Vec<Node>),
    Array(Box<Node>),
    Null,
    Undefined,
    False,
    True,
}

impl Node {
    pub fn is_bin_op(&self) -> bool {
        match self {
            Node::BinOp { .. } => true,
            _ => false,
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
    Not
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
