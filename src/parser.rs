use pest::error::{Error, ErrorVariant};
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::PrattParser;
use pest::Parser;
use pretty::RcDoc;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct NewtypeParser;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Node {
    Program(Vec<Node>),
    TypeAlias(String, Vec<Node>, Box<Node>),
    Expr(Expr),
    Ident(String),
    Number(String),
    Primitive(Primitive),
    String(String),
    TemplateString(String),
    IfExpr(
        Box<ExtendsExpr>,  // condition
        Box<Node>,         // then
        Option<Box<Node>>, // else
    ),
    None,
    Error(Error<Rule>),
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
    fn is_bin_op(&self) -> bool {
        match self {
            Node::Expr(Expr::BinOp { .. }) => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjectProperty {
    pub readonly: bool,
    pub optional: bool,
    pub key: String,
    pub value: Node,
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExtendsExpr {
    Value(Box<Node>),
    Infer(String),
    BinOp {
        lhs: Box<ExtendsExpr>,
        op: ExtendsInfixOp,
        rhs: Box<ExtendsExpr>,
    },
}

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

fn surround<'a, T>(doc: RcDoc<'a, ()>, left: T, right: T) -> RcDoc<()>
where
    T: Into<std::borrow::Cow<'a, str>>,
{
    RcDoc::text(left).append(doc).append(RcDoc::text(right))
}

fn parens(doc: RcDoc<()>) -> RcDoc<()> {
    surround(doc, "(", ")")
}

pub trait ToTypescript {
    fn to_pretty_ts(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_ts().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }

    fn to_ts(&self) -> RcDoc<()>;
}

impl ToTypescript for Node {
    fn to_ts(&self) -> RcDoc<()> {
        match self {
            Node::Program(stmnts) => {
                let mut doc = RcDoc::nil();
                for stmnt in stmnts {
                    doc = doc
                        .append(stmnt.to_ts())
                        .append(";")
                        .append(RcDoc::hardline());
                }
                doc
            }
            Node::TypeAlias(name, params, body) => {
                let body = (*body).to_ts();

                RcDoc::text("type")
                    .append(RcDoc::space())
                    .append(name)
                    .append(match params {
                        list if list.len() == 0 => RcDoc::nil(),
                        list => {
                            let seperator = RcDoc::text(",").append(RcDoc::space());

                            let body = RcDoc::intersperse(
                                list.iter().map(|param| param.to_ts()),
                                seperator,
                            );

                            RcDoc::text("<").append(body).append(RcDoc::text(">"))
                        }
                    })
                    .append(RcDoc::space())
                    .append("=")
                    .append(RcDoc::space())
                    .append(body)
            }
            Node::Ident(ident) => RcDoc::text(ident),
            Node::Number(number) => RcDoc::text(number),
            Node::Primitive(primitive) => RcDoc::text(match primitive {
                Primitive::Boolean => "boolean",
                Primitive::Number => "number",
                Primitive::String => "string",
            }),
            Node::String(string) => RcDoc::text(string),
            Node::TemplateString(_) => todo!(),
            Node::IfExpr(cond, then, els) => (**cond)
                .to_ts()
                .append(RcDoc::space())
                .append("?")
                .append(RcDoc::space())
                .append(then.to_ts())
                .append(RcDoc::space())
                .append(":")
                .append(RcDoc::space())
                .append(match els {
                    Some(els) => els.to_ts(),
                    None => RcDoc::text("never"),
                }),
            Node::None => todo!(),
            Node::Error(_) => todo!(),
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
            Node::Expr(body) => (*body).to_ts(),
        }
    }
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

impl ToTypescript for Expr {
    fn to_ts(&self) -> RcDoc<()> {
        match self {
            Expr::Value(node) => node.to_ts(),

            Expr::BinOp { lhs, op, rhs } => {
                fn fmt(v: &Expr) -> RcDoc<()> {
                    match v {
                        Expr::Value(value) => match **value {
                            Node::Expr(_) => parens(v.to_ts()),
                            _ => value.to_ts(),
                        },
                        Expr::BinOp { .. } => parens(v.to_ts()),
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
        }
    }
}

impl ToTypescript for ExtendsExpr {
    fn to_ts(&self) -> RcDoc {
        match self {
            ExtendsExpr::Value(value) => value.to_ts(),
            ExtendsExpr::Infer(ident) => RcDoc::text("infer").append(RcDoc::space()).append(ident),
            ExtendsExpr::BinOp { lhs, op, rhs } => match op {
                ExtendsInfixOp::Extends => lhs
                    .to_ts()
                    .append(RcDoc::space())
                    .append(RcDoc::text("extends"))
                    .append(RcDoc::space())
                    .append(rhs.to_ts()),
                ExtendsInfixOp::NotExtends => todo!(),
                ExtendsInfixOp::Equals => todo!(),
                ExtendsInfixOp::NotEquals => todo!(),
                ExtendsInfixOp::StrictEquals => todo!(),
                ExtendsInfixOp::StrictNotEquals => todo!(),
                ExtendsInfixOp::And => todo!(),
                ExtendsInfixOp::Or => todo!(),
            },
        }
    }
}

lazy_static::lazy_static! {
    static ref EXPR_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
            .op(Op::infix(union, Right) | Op::infix(intersection, Right))
    };

    static ref EXTENDS_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
            .op(
                Op::infix(or, Left)
                | Op::infix(and, Left)
            )
            .op(
                Op::infix(extends, Left)
                | Op::infix(not_extends, Left)
                | Op::infix(equals, Left)
                | Op::infix(not_equals, Left)
                | Op::infix(strict_equals, Left)
                | Op::infix(strict_not_equals, Left)
            )
            .op(Op::prefix(infer))
    };
}

pub fn expr(pairs: Pairs<Rule>) -> Expr {
    EXPR_PARSER
        .map_primary(|primary| Expr::Value(Box::new(node(primary))))
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::union => Op::Union,
                Rule::intersection => Op::Intersection,
                rule => unreachable!("Expr::parse expected infix operation, found {:?}", rule),
            };
            Expr::BinOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        })
        .parse(pairs)
}

pub fn parse_newtype(source: &str) -> Result<Node, Error<Rule>> {
    let pair = NewtypeParser::parse(Rule::program, source)?.next().unwrap();

    Ok(node(pair))
}

fn new_error(message: String, pair: Pair<Rule>) -> Node {
    return Node::Error(Error::new_from_span(
        ErrorVariant::CustomError { message },
        pair.as_span(),
    ));
}

fn parse_extends_condition(pairs: Pairs<Rule>) -> ExtendsExpr {
    fn wrap_error(error: Node) -> ExtendsExpr {
        return ExtendsExpr::Value(Box::new(error));
    }

    EXTENDS_PARSER
        .map_primary(|primary| match primary.as_rule() {
            _ => {
                let value = Box::new(node(primary));
                ExtendsExpr::Value(value)
            }
        })
        .map_prefix(|op, primary| match op.as_rule() {
            Rule::infer => match primary {
                ExtendsExpr::Value(value) => match value.as_ref() {
                    Node::Ident(ident) => ExtendsExpr::Infer(ident.to_string()),
                    _ => {
                        unreachable!()
                    }
                },
                ExtendsExpr::Infer(_) => unreachable!(),
                ExtendsExpr::BinOp { .. } => wrap_error(new_error(
                    "Only identifiers may be inferred".to_string(),
                    op,
                )),
            },
            _ => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::extends => ExtendsInfixOp::Extends,
                Rule::not_extends => ExtendsInfixOp::NotExtends,
                Rule::equals => ExtendsInfixOp::Equals,
                Rule::not_equals => ExtendsInfixOp::NotEquals,
                Rule::strict_equals => ExtendsInfixOp::StrictEquals,
                Rule::strict_not_equals => ExtendsInfixOp::StrictNotEquals,
                Rule::and => ExtendsInfixOp::And,
                rule => unreachable!(
                    "ExtendsExpr::parse expected infix operation, found {:?}",
                    rule
                ),
            };
            ExtendsExpr::BinOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        })
        .parse(pairs)
}

fn node(pair: Pair<Rule>) -> Node {
    match pair.as_rule() {
        // Rule::program => Node::Program(pair.into_inner().map(parse_node).collect()),
        Rule::program => {
            let children: Vec<_> = pair
                .into_inner()
                .filter(|pair| pair.as_rule() != Rule::EOI) // Remove the end of input token
                .map(node)
                .collect();

            Node::Program(children)
        }
        Rule::type_alias => type_alias(pair),
        Rule::if_expr => {
            let mut inner = pair.into_inner();

            let condition = inner
                .find_first_tagged("condition")
                .map(|p| Box::new(parse_extends_condition(p.into_inner())))
                .unwrap();

            let then = inner
                .find_first_tagged("then")
                .map(node)
                .map(Box::new)
                .unwrap();

            let els = inner.find_first_tagged("else").map(node).map(Box::new);
            //
            Node::IfExpr(condition, then, els)
        }
        Rule::object_literal => object_literal(pair),
        Rule::primitive => {
            println!("{:#?}", pair);
            let value = pair.into_inner().next().unwrap();
            let primitive = match value.as_rule() {
                Rule::type_string => Primitive::String,
                Rule::type_number => Primitive::Number,
                Rule::type_boolean => Primitive::Boolean,
                _ => unreachable!(
                    "unexpected rule while parsing primitive: {:?}",
                    value.as_rule()
                ),
            };
            Node::Primitive(primitive)
        }
        Rule::number => Node::Number(text(pair)),
        Rule::string => Node::String(text(pair)),
        Rule::template_string => Node::TemplateString(text(pair)),
        Rule::ident => Node::Ident(text(pair)),
        Rule::never => Node::Never,
        Rule::any => Node::Any,
        Rule::unknown => Node::Unknown,
        Rule::literal_true => Node::True,
        Rule::literal_false => Node::False,
        Rule::null => Node::Null,
        Rule::undefined => Node::Undefined,
        Rule::tuple => tuple(pair),
        Rule::application => {
            let mut inner = pair.into_inner();

            let ident = inner.next().map(text).unwrap();

            let arguments = inner.map(node).collect();

            Node::Application(ident, arguments)
        }
        Rule::EOI => unreachable!("unexpected end of input"),
        Rule::expr => Node::Expr(expr(pair.into_inner())),
        Rule::array => {
            println!("{:#?}", pair);
            let mut inner = pair.into_inner();
            let mut value = inner.next().map(node).unwrap();

            // To avoid a left hand recursive rule in the grammar
            // The rule allows for one or more array modifiers to be applied,
            // which we nest here.
            while let Some(_) = inner.next() {
                value = Node::Array(Box::new(value));
            }

            value
        }
        Rule::infer => new_error(
            "Infer operator must be used in an extends expression".to_string(),
            pair,
        ),
        _ => {
            unreachable!("unexpected rule while parsing expr: {:?}", pair.as_rule())
        } // rule => unreachable!("unexpected rule: {:?}", rule),
    }
}

fn tuple(pair: Pair<Rule>) -> Node {
    let items = pair.into_inner().map(node).collect();

    Node::Tuple(items)
}

fn object_literal(pair: Pair<Rule>) -> Node {
    let mut inner_rules = pair.into_inner();
    let mut properties = Vec::new();

    while let Some(property) = inner_rules.next() {
        let inner = property.into_inner();
        let readonly = inner.find_first_tagged("readonly").is_some();
        let key = inner.find_first_tagged("key").map(text).unwrap();
        let optional = inner.find_first_tagged("optional").is_some();
        let value = inner.find_first_tagged("value").map(node).unwrap();

        properties.push(ObjectProperty {
            readonly,
            optional,
            key,
            value,
        });
    }

    Node::ObjectLiteral(properties)
}

fn type_alias(pair: Pair<Rule>) -> Node {
    let mut inner = pair.into_inner();

    let pair = inner.next().unwrap();
    assert_eq!(Rule::ident, pair.as_rule());
    let name = pair.as_str().to_string();

    let pair = inner.next().unwrap();
    assert_eq!(Rule::type_parameters, pair.as_rule());
    let type_parameters = pair.into_inner().map(node).collect();

    let body = Box::new(inner.next().map(node).unwrap());

    Node::TypeAlias(name, type_parameters, body)
}

fn text(pair: Pair<Rule>) -> String {
    pair.as_str().to_string()
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use pretty_assertions::{assert_eq, assert_ne};
    use quickcheck::TestResult;

    use super::*;

    macro_rules! join {
        ($($e:expr),*) => {
            vec![$($e.to_string()),*].join("\n").as_str()
        };
    }

    macro_rules! assert_typescript {
        ($expected:expr, $source:expr) => {
            let result = parse($source.to_string());
            println!("{:#?}", result);
            assert_eq!($expected, result.to_pretty_ts(usize::MAX));
        };
    }

    macro_rules! assert_parse_failure {
        ($source:expr) => {
            let result = parse_newtype($source.to_string())
            println!("{:#?}", result);
            assert!(result.is_err());
        };
    }

    macro_rules! inspect {
        ($source:expr) => {
            println!("{:#?}", parse($source.to_string()));
            assert!(false)
        };
    }

    fn s(input: &str) -> String {
        input.to_string()
    }

    fn parse(source: String) -> Node {
        parse_newtype(&source).unwrap_or_else(|e| panic!("ERROR {}", e))
    }

    #[test]
    fn test_parse_primitives() {
        assert_typescript!("type A = string;\n", "type A = string");
        assert_typescript!("type A = number;\n", "type A = number");
        assert_typescript!("type A = boolean;\n", "type A = boolean");
        assert_typescript!("type A = never;\n", "type A = never");
        assert_typescript!("type A = any;\n", "type A = any");
        assert_typescript!("type A = unknown;\n", "type A = unknown");
    }

    #[test]
    fn test_parse_number_literals() {
        assert_typescript!("type A = 1;\n", "type A = 1");
        assert_typescript!("type A = -1;\n", "type A = -1");
        assert_typescript!("type A = - 1;\n", "type A = - 1");
        assert_typescript!("type A = 100;\n", "type A = 100");
        assert_typescript!("type A = 1_000;\n", "type A = 1_000");
        assert_typescript!("type A = 100.;\n", "type A = 100.");
        assert_typescript!("type A = 100.0;\n", "type A = 100.0");
        assert_typescript!("type A = 100.000_000;\n", "type A = 100.000_000");
    }

    #[test]
    fn test_parse_string_literals() {
        assert_typescript!("type A = \"1\";\n", "type A = \"1\"");
        assert_typescript!("type A = '1';\n", "type A = '1'");
    }

    fn test_parse_template_string_literals() {
        assert_typescript!("type A = `1`;\n", "type A = `1`");
    }

    #[test]
    fn test_parse_literals() {
        assert_typescript!("type A = true;\n", "type A = true");
        assert_typescript!("type A = false;\n", "type A = false");
        assert_typescript!("type A = null;\n", "type A = null");
        assert_typescript!("type A = undefined;\n", "type A = undefined");
    }

    #[test]
    fn test_parse_bin_ops() {
        assert_typescript!("type A = 1 | 2;\n", "type A = 1 | 2");
        assert_typescript!("type A = 1 & 2;\n", "type A = 1 & 2");
        assert_typescript!("type A = 1 | (2 & 3);\n", "type A = 1 | 2 & 3");
        assert_typescript!("type A = 1 & (2 | 3);\n", "type A = 1 & 2 | 3");
        assert_typescript!("type A = (1 | 2) & 3;\n", "type A = (1 | 2) & 3");
    }

    #[test]
    fn test_parse_statment_indent_sensitive() {
        assert_matches!(parse_newtype(" type A = 1"), Err(_));
        // assert_matches!(parse_newtype(join!("type A =", "1")), Err(_));
    }

    #[test]
    fn test_parse_if_expr() {
        assert_typescript!(
            "type A = 1 extends number ? 1 : 0;\n",
            "type A = if 1 <: number then 1 else 0"
        );

        assert_typescript!(
            "type A = 1 extends number ? 1 : never;\n",
            "type A = if 1 <: number then 1"
        );

        assert_typescript!(
            "type A = 1 extends number ? 1 : 0;\n",
            join!("type A = if 1 <: number", "then 1", "else 0")
        );

        // let result = parse_newtype("type A extends");
        //
        // let msg = "expected if";
        // match result {
        //     Ok(x) => assert!(
        //         false,
        //         "assertion failed: expected error, got:\n{}",
        //         x.to_pretty_ts(usize::MAX)
        //     ),
        //     Err(e) => assert!(
        //         e.to_string().contains(msg),
        //         "assertion failed: expected {} in {}",
        //         msg,
        //         e
        //     ),
        // }
    }

    #[test]
    fn test_parse_object_literals() {
        assert_typescript!("type A = {};\n", "type A = {}");

        assert_typescript!(
            "type A = {x: 1, y: 2, z: 3};\n",
            "type A = {x: 1, y: 2, z: 3}"
        );

        assert_typescript!("type A = {x: 1};\n", "type A = {x: 1}");
        assert_typescript!("type A = {readonly x: 1};\n", "type A = {readonly x: 1}");
        assert_typescript!("type A = {x?: 1};\n", "type A = {x?: 1}");
    }

    #[test]
    fn test_parse_tuples() {
        assert_typescript!("type A = [];\n", "type A = []");
        assert_typescript!("type A = [1];\n", "type A = [1]");
        assert_typescript!("type A = [1, 2, 3];\n", "type A = [1, 2, 3]");
        assert_typescript!("type A = [\"sup\"];\n", "type A = [\"sup\"]");
    }

    #[test]
    fn test_parse_arrays() {
        assert_typescript!("type A = number[];\n", "type A = number[]");
        assert_typescript!("type A = number[];\n", "type A = (number)[]");
        assert_typescript!("type A = number[][][];\n", "type A = number[][][]");
        assert_typescript!(
            "type A = (number | string)[];\n",
            "type A = (number | string)[]"
        );
    }

    #[test]
    fn test_parse_generics() {
        assert_typescript!("type A<x> = x;\n", "type A x = x");
        assert_typescript!("type A<x, y, z> = 1;\n", "type A x y z = 1");
    }

    #[test]
    fn test_parse_application() {
        assert_typescript!("type B = A<1>;\n", "type B = A 1");
        assert_typescript!("type B = A<1, 2, 3>;\n", "type B = A 1 2 3");
        assert_typescript!("type B = A<1, []>;\n", "type B = A 1 []");
        assert_typescript!("type B = A<[], [], []>;\n", "type B = A [] [] []");
        assert_typescript!("type B = A<B<1>>;\n", "type B = A (B 1)");
        assert_typescript!("type B = A<B, 1>;\n", "type B = A B 1");
    }

    //
    //     #[quickcheck]
    //     fn prop_parse_number_float(n: f64) -> bool {
    //         let Ok(("", Number(out))) = number(&n.to_string())
    //         else {
    //             return false
    //         };
    //         n.to_string() == out.to_string()
    //     }
}
