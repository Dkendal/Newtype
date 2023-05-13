use pest::error::{Error, ErrorVariant};
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::PrattParser;
use pest::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct NewtypeParser;

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Value(Node),
    BinOp {
        lhs: Box<Expr>,
        op: Op,
        rhs: Box<Expr>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Op {
    Union,
    Intersection,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Primitive {
    Boolean,
    Number,
    String,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Node {
    Program(Vec<Node>),
    TypeAlias(String, Vec<Node>, Box<Expr>),
    Ident(String),
    Number(String),
    Primitive(Primitive),
    String(String),
    TemplateString(String),
    IfExpr(
        Box<Node>,         // then
        Box<ExtendsExpr>,  // condition
        Option<Box<Node>>, // else
    ),
    None,
    Error(Error<Rule>),
    ObjectLiteral(Vec<(String, Node)>),
    GenericCall(String, Vec<Node>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExtendsExpr {
    Value(Box<Node>),
    Infer(String),
    BinOp {
        lhs: Box<ExtendsExpr>,
        op: ExtendsInfixOp,
        rhs: Box<ExtendsExpr>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExtendsPrefixOp {
    Infer(String),
}

#[derive(Debug, PartialEq, Eq)]
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

lazy_static::lazy_static! {
    static ref EXPR_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
            .op(Op::infix(union, Left) | Op::infix(intersection, Left))
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
        .map_primary(|primary| Expr::Value(node(primary)))
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

fn parse_newtype(source: &str) -> Result<Node, Error<Rule>> {
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
                _ => {
                    unreachable!()
                }
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
            let mut inner_rules = pair.into_inner();

            let condition = inner_rules
                .next()
                .map(|pair| pair.into_inner())
                .map(parse_extends_condition)
                .map(Box::new)
                .unwrap();

            let then = inner_rules.next().map(node).map(Box::new).unwrap();
            let else_ = inner_rules.next().map(node).map(Box::new);
            Node::IfExpr(then, condition, else_)
        }
        Rule::object_literal => {
            let mut inner_rules = pair.into_inner();
            let mut properties = Vec::new();
            while let Some(property) = inner_rules.next() {
                let mut property_rules = property.into_inner();
                let key = property_rules.next().unwrap().as_str().to_string();
                let value = property_rules.next().map(node).unwrap();
                properties.push((key, value));
            }
            Node::ObjectLiteral(properties)
        }
        Rule::primitive => {
            let primitive = match pair.as_rule() {
                Rule::type_string => Primitive::String,
                Rule::type_number => Primitive::Number,
                Rule::type_boolean => Primitive::Boolean,
                _ => unreachable!(
                    "unexpected rule while parsing primitive: {:?}",
                    pair.as_rule()
                ),
            };
            Node::Primitive(primitive)
        }
        Rule::number => Node::Number(pair.as_str().to_string()),
        Rule::string => Node::String(pair.as_str().to_string()),
        Rule::template_string => Node::TemplateString(pair.as_str().to_string()),
        Rule::ident => Node::Ident(pair.as_str().to_string()),
        Rule::generic_call => {
            let mut inner = pair.into_inner();

            let ident = inner.next().unwrap().as_str().to_string();

            let arguments = inner.next().unwrap().into_inner().map(node).collect();

            Node::GenericCall(ident, arguments)
        }
        Rule::EOI => unreachable!("unexpected end of input"),
        Rule::infer => new_error(
            "Infer operator must be used in an extends expression".to_string(),
            pair,
        ),
        _ => {
            unreachable!("unexpected rule while parsing expr: {:?}", pair.as_rule())
        } // rule => unreachable!("unexpected rule: {:?}", rule),
    }
}

fn type_alias(pair: Pair<Rule>) -> Node {
    let mut inner = pair.into_inner();

    let pair = inner.next().unwrap();
    assert_eq!(Rule::ident, pair.as_rule());
    let name = pair.as_str().to_string();

    let pair = inner.next().unwrap();
    assert_eq!(Rule::type_parameters, pair.as_rule());
    let type_parameters = pair.into_inner().map(node).collect();

    let body = Box::new(expr(inner));

    Node::TypeAlias(name, type_parameters, body)
}

#[cfg(test)]
mod tests {
    use pretty_assertions::{assert_eq, assert_ne};
    use quickcheck::TestResult;

    use super::*;

    fn s(input: &str) -> String {
        input.to_string()
    }

    #[test]
    fn test_parse() {
        let source = vec!["type A = 1 | 2"].join("\n");

        let result = parse_newtype(&source).unwrap_or_else(|e| panic!("ERROR {:#?}", e));

        println!("RESULT: {:#?}", result);

        assert_eq!(false)
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
