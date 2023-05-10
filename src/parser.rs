use pest::error::{Error, ErrorVariant};
use pest::iterators::Pairs;
use pest::pratt_parser::PrattParser;
use pest::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct NewtypeParser;

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

#[derive(Debug)]
pub enum Expr {
    Rule(Rule),
    BinOp {
        lhs: Box<Expr>,
        op: Op,
        rhs: Box<Expr>,
    },
}

#[derive(Debug)]
pub enum Op {
    Union,
    Intersection,
}

pub fn parse_expr(pairs: Pairs<Rule>) -> Expr {
    EXPR_PARSER
        .map_primary(|primary| match primary.as_rule() {
            rule @ Rule::expr => Expr::Rule(rule),
            rule => unreachable!("Expr::parse expected atom, found {:?}", rule),
        })
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

#[derive(Debug)]
pub enum Node {
    Program(Vec<Node>),
    TypeAlias(String, Box<Node>),
    Ident(String),
    Number(String),
    String(String),
    TemplateString(String),
    IfExpr(
        Box<Node>,         // then
        Box<ExtendsExpr>,  // condition
        Option<Box<Node>>, // else
    ),
    None,
    Error(Error<Rule>),
}

#[derive(Debug)]
pub enum ExtendsExpr {
    Value(Box<Node>),
    Infer(String),
    BinOp {
        lhs: Box<ExtendsExpr>,
        op: ExtendsInfixOp,
        rhs: Box<ExtendsExpr>,
    },
}

#[derive(Debug)]
pub enum ExtendsPrefixOp {
    Infer(String),
}

#[derive(Debug)]
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

fn parse_newtype(source: &str) -> Result<Node, Error<Rule>> {
    let pair = NewtypeParser::parse(Rule::program, source)?.next().unwrap();

    use pest::iterators::Pair;

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
                    let value = Box::new(parse_node(primary));
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

    fn parse_node(pair: Pair<Rule>) -> Node {
        match pair.as_rule() {
            // Rule::program => Node::Program(pair.into_inner().map(parse_node).collect()),
            Rule::program => {
                let inner: Vec<_> = pair
                    .into_inner()
                    .filter(|pair| pair.as_rule() != Rule::EOI)
                    .map(parse_node)
                    .collect();

                println!("{:?}", inner);

                Node::Program(vec![])
            }
            Rule::type_alias => {
                let mut inner_rules = pair.into_inner();
                let name = inner_rules.next().unwrap().as_str().to_string();
                let body = Box::new(inner_rules.next().map(parse_node).unwrap());
                Node::TypeAlias(name, body)
            }
            Rule::if_expr => {
                let mut inner_rules = pair.into_inner();

                let condition = inner_rules
                    .next()
                    .map(|pair| pair.into_inner())
                    .map(parse_extends_condition)
                    .map(Box::new)
                    .unwrap();

                let then = inner_rules.next().map(parse_node).map(Box::new).unwrap();
                let else_ = inner_rules.next().map(parse_node).map(Box::new);
                Node::IfExpr(then, condition, else_)
            }
            Rule::number => Node::Number(pair.as_str().to_string()),
            Rule::string => Node::String(pair.as_str().to_string()),
            Rule::template_string => Node::TemplateString(pair.as_str().to_string()),
            Rule::ident => Node::Ident(pair.as_str().to_string()),
            Rule::EOI => unreachable!("unexpected end of input"),
            Rule::infer => new_error(
                "Infer operator must be used in an extends expression".to_string(),
                pair,
            ),
            Rule::union
            | Rule::extends_expr
            | Rule::intersection
            | Rule::expr
            | Rule::term
            | Rule::statement
            | Rule::infix
            | Rule::WHITESPACE => {
                unreachable!("unexpected rule while parsing expr: {:?}", pair.as_rule())
            }
            rule => unreachable!("unexpected rule: {:?}", rule),
        }
    }

    Ok(parse_node(pair))
}

#[cfg(test)]
mod tests {
    use quickcheck::TestResult;

    use super::*;

    fn s(input: &str) -> String {
        input.to_string()
    }

    #[test]
    fn test_parse() {
        let source = vec!["type C = if (a <: b) && (b <: c) then b else c"].join("\n");

        // let successful_parse = NewtypeParser::parse(Rule::program, source);
        match parse_newtype(&source) {
            Ok(node) => println!("{:?}", node),
            Err(e) => println!("{}", e),
        }

        assert_eq!(0, 1);
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
