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
    Primitive(Primitive),
    String(String),
    TemplateString(String),
    IfExpr(
        Box<Node>,         // condition, must be ExtendsBinOp
        Box<Node>,         // then
        Option<Box<Node>>, // else
    ),
    Infer(String),
    ExtendsBinOp {
        lhs: Box<Node>,
        op: ExtendsInfixOp,
        rhs: Box<Node>,
    },
    ExtendsExpr(
        Box<Node>, // lhs
        Box<Node>, // rhs
        Box<Node>, // then
        Box<Node>, // else
    ),
    None,
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
pub enum Primitive {
    Boolean,
    Number,
    String,
}

// #[derive(Debug, PartialEq, Eq, Clone)]
// pub enum ExtendsExpr {
//     Value(Box<Node>),
//     Infer(String),
//     ExtendsBinOp {
//         lhs: Box<ExtendsExpr>,
//         op: ExtendsInfixOp,
//         rhs: Box<ExtendsExpr>,
//     },
// }

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExtendsPrefixOp {
    Infer(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExtendsInfixOp {
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
pub struct ObjectProperty {
    pub readonly: bool,
    pub optional: bool,
    pub key: String,
    pub value: Node,
}
