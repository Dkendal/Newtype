use crate::ast::*;
use crate::simplify::*;
use crate::to_typescript::ToTypescript;
use pest::error::{Error, ErrorVariant};
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::PrattParser;
use pest::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct NewtypeParser;

pub type ParserError = pest::error::Error<Rule>;

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
            .op(Op::prefix(not))
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

pub fn parse_expr(pairs: Pairs<Rule>) -> Node {
    EXPR_PARSER
        .map_primary(parse_node)
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::union => Op::Union,
                Rule::intersection => Op::Intersection,
                rule => unreachable!("Expr::parse expected infix operation, found {:?}", rule),
            };
            Node::BinOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        })
        .parse(pairs)
}

pub fn parse_newtype(source: &str) -> Result<Node, Error<Rule>> {
    let pair = NewtypeParser::parse(Rule::program, source)?.next().unwrap();

    Ok(parse_node(pair))
}

fn new_error(message: String, pair: Pair<Rule>) -> Node {
    return Node::Error(Error::new_from_span(
        ErrorVariant::CustomError { message },
        pair.as_span(),
    ));
}

fn parse_extends_condition(pairs: Pairs<Rule>) -> Node {
    EXTENDS_PARSER
        .map_primary(parse_node)
        .map_prefix(|op, primary_node| {
            let op = match op.as_rule() {
                Rule::infer => PrefixOp::Infer,
                Rule::not => PrefixOp::Not,
                rule => unreachable!("parse expected prefix operation, found {:?}", rule),
            };

            Node::ExtendsPrefixOp {
                op,
                value: Box::new(primary_node),
            }
        })
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::extends => InfixOp::Extends,
                Rule::not_extends => InfixOp::NotExtends,
                Rule::equals => InfixOp::Equals,
                Rule::not_equals => InfixOp::NotEquals,
                Rule::strict_equals => InfixOp::StrictEquals,
                Rule::strict_not_equals => InfixOp::StrictNotEquals,
                Rule::and => InfixOp::And,
                Rule::or => InfixOp::Or,
                rule => unreachable!("Expected infix operation, found {:?}", rule),
            };
            Node::ExtendsBinOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        })
        .parse(pairs)
}

pub(crate) fn parse_node(pair: Pair<Rule>) -> Node {
    let rule = pair.as_rule();
    match rule {
        Rule::program => {
            let children: Vec<_> = pair
                .into_inner()
                .filter(|pair| pair.as_rule() != Rule::EOI) // Remove the end of input token
                .map(parse_node)
                .collect();

            Node::Program(children)
        }
        Rule::type_alias => parse_type_alias(pair),
        Rule::if_expr => parse_if_expr(pair),
        Rule::object_literal => parse_object_literal(pair),
        Rule::primitive => {
            let value = pair.into_inner().next().unwrap();
            let primitive = match value.as_rule() {
                Rule::type_string => PrimitiveType::String,
                Rule::type_number => PrimitiveType::Number,
                Rule::type_boolean => PrimitiveType::Boolean,
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

            let identifier = inner.next().map(text).unwrap();

            let arguments_pair = inner
                .find_first_tagged("arguments")
                .expect("application missing arguments");

            let arguments = arguments_pair.into_inner().map(parse_node).collect();

            Node::Application(identifier, arguments)
        }
        Rule::EOI => unreachable!("unexpected end of input"),
        Rule::expr => parse_expr(pair.into_inner()),
        Rule::array => {
            let mut inner = pair.into_inner();
            let mut value = inner.next().map(parse_node).unwrap();

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
        Rule::match_expr => {
            let inner = pair.into_inner();
            let value = inner
                .find_first_tagged("value")
                .map(parse_node)
                .map(Box::new)
                .unwrap();

            let arms: Vec<MatchArm> = inner
                .find_tagged("arm")
                .map(|arm| {
                    let inner = arm.into_inner();
                    let pattern = inner.find_first_tagged("pattern").map(parse_node).unwrap();
                    let body = inner.find_first_tagged("body").map(parse_node).unwrap();
                    MatchArm { pattern, body }
                })
                .collect();
            Node::MatchExpr { value, arms }
        }
        Rule::match_arm => new_error(
            "Match arms must be used in a match expression".to_string(),
            pair,
        ),
        Rule::cond_expr => {
            let inner = pair.into_inner();

            let else_ = inner
                .find_first_tagged("else")
                .and_then(|p| p.into_inner().find_first_tagged("body"))
                .map(parse_node)
                .map(Box::new)
                .unwrap_or(Box::new(Node::Never));

            let arms: Vec<CondArm> = inner
                .find_tagged("arm")
                .map(|arm| {
                    let inner = arm.into_inner();
                    let condition = inner
                        .find_first_tagged("condition")
                        .map(|p| p.into_inner())
                        .map(parse_extends_condition)
                        .unwrap();
                    let body = inner.find_first_tagged("body").map(parse_node).unwrap();
                    CondArm { condition, body }
                })
                .collect();

            Node::CondExpr { arms, else_ }
        }
        Rule::cond_arm => new_error(
            "Condition arms must be used in a condition expression".to_string(),
            pair,
        ),
        Rule::statement
        | Rule::and
        | Rule::argument_list
        | Rule::array_modifier
        | Rule::bottom_type
        | Rule::double_quote_string
        | Rule::else_arm
        | Rule::equals
        | Rule::expr1
        | Rule::extends
        | Rule::extends_condition
        | Rule::extends_expr
        | Rule::extends_infix
        | Rule::extends_prefix
        | Rule::extends_primary
        | Rule::infix
        | Rule::intersection
        | Rule::neg
        | Rule::not
        | Rule::not_equals
        | Rule::not_extends
        | Rule::object_property
        | Rule::optional_modifier
        | Rule::or
        | Rule::prefix
        | Rule::primary
        | Rule::readonly_modifier
        | Rule::single_quote_string
        | Rule::strict_equals
        | Rule::strict_not_equals
        | Rule::sub_expr
        | Rule::term
        | Rule::top_type
        | Rule::type_boolean
        | Rule::type_number
        | Rule::type_parameters
        | Rule::type_string
        | Rule::union
        | Rule::COMMENT
        | Rule::WHITESPACE
        | Rule::keyword => {
            let positives: Vec<Rule> = vec![];
            let negatives: Vec<Rule> = vec![pair.as_rule()];
            let error = Error::new_from_span(
                ErrorVariant::ParsingError {
                    positives,
                    negatives,
                },
                pair.as_span(),
            );

            panic!("{error}")
        }
    }
}

fn parse_if_expr(pair: Pair<Rule>) -> Node {
    let inner = pair.into_inner();

    let condition = inner
        .find_first_tagged("condition")
        .map(|p| Box::new(parse_extends_condition(p.into_inner())))
        .unwrap();

    let then = inner
        .find_first_tagged("then")
        .map(parse_node)
        .map(Box::new)
        .unwrap();

    let else_ = inner
        .find_first_tagged("else")
        .map(parse_node)
        .map(Box::new)
        .unwrap_or(Box::new(Node::Never));

    match *condition {
        // If the condition is a binary operation, we can desugar it into an extends expression
        Node::ExtendsBinOp {
            op: InfixOp::Extends,
            lhs,
            rhs,
        } => Node::ExtendsExpr(lhs, rhs, then, else_),

        // Other conditions are desugared later in the simplification step
        Node::ExtendsBinOp { .. } | Node::ExtendsPrefixOp { .. } => {
            Node::IfExpr(condition, then, Some(else_))
        }
        _ => unreachable!(),
    }
}

fn tuple(pair: Pair<Rule>) -> Node {
    let items = pair.into_inner().map(parse_node).collect();

    Node::Tuple(items)
}

fn parse_object_literal(pair: Pair<Rule>) -> Node {
    let mut object_property_rules = pair.into_inner();
    let mut properties = Vec::new();

    while let Some(prop_pair) = object_property_rules.next() {
        match prop_pair.as_rule() {
            Rule::object_property => {
                let inner = prop_pair.into_inner();

                let readonly = inner.find_first_tagged("readonly").is_some();

                let key = inner
                    .find_first_tagged("key")
                    .expect("object property missing key")
                    .as_str()
                    .to_string();

                let optional = inner.find_first_tagged("optional").is_some();

                let value = inner
                    .find_first_tagged("value")
                    .map(parse_node)
                    .expect("object property missing value");

                properties.push(ObjectProperty {
                    readonly,
                    optional,
                    key,
                    value,
                });
            }
            _ => unreachable!(
                "unexpected rule while parsing object literal: {:?}",
                prop_pair.as_rule()
            ),
        }
    }

    Node::ObjectLiteral(properties)
}

fn parse_type_alias(pair: Pair<Rule>) -> Node {
    let inner = pair.into_inner();

    let name = inner
        .find_first_tagged("name")
        .expect("type alias missing name")
        .as_str()
        .to_string();

    let type_parameters = inner
        .find_first_tagged("parameters")
        .map(|p| p.into_inner().map(parse_node).collect())
        .unwrap_or_else(Vec::new);

    let body = inner
        .find_first_tagged("body")
        .map(parse_node)
        .map(Box::new)
        .unwrap();

    Node::TypeAlias(name, type_parameters, body)
}

fn text(pair: Pair<Rule>) -> String {
    pair.as_str().to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::*;
    use pretty_assertions::{assert_eq, assert_ne};
    use quickcheck::TestResult;
    use std::assert_matches::assert_matches;

    fn s(input: &str) -> String {
        input.to_string()
    }

    // #[test]
    // fn infer() {
    //     assert_typescript!("", "type A = ?x");
    // }

    #[test]
    fn primitive_string() {
        assert_typescript!("type A = string;", "type A = string");
    }

    #[test]
    fn primitive_number() {
        assert_typescript!("type A = number;", "type A = number");
    }

    #[test]
    fn primitive_boolean() {
        assert_typescript!("type A = boolean;", "type A = boolean");
    }

    #[test]
    fn primitive_never() {
        assert_typescript!("type A = never;", "type A = never");
    }

    #[test]
    fn primitive_any() {
        assert_typescript!("type A = any;", "type A = any");
    }

    #[test]
    fn primitive_unknown() {
        assert_typescript!("type A = unknown;", "type A = unknown");
    }

    #[test]
    fn number_literal_positive_integer() {
        assert_typescript!("type A = 1;", "type A = 1");
    }

    #[test]
    fn number_literal_negative_integer() {
        assert_typescript!("type A = -1;", "type A = -1");
    }

    #[test]
    fn number_literal_negative_integer_with_space() {
        assert_typescript!("type A = - 1;", "type A = - 1");
    }

    #[test]
    fn number_literal_large_integer() {
        assert_typescript!("type A = 100;", "type A = 100");
    }

    #[test]
    fn number_literal_integer_with_underscore() {
        assert_typescript!("type A = 1_000;", "type A = 1_000");
    }

    #[test]
    fn number_literal_decimal_without_fraction() {
        assert_typescript!("type A = 100.;", "type A = 100.");
    }

    #[test]
    fn number_literal_decimal_with_fraction() {
        assert_typescript!("type A = 100.0;", "type A = 100.0");
    }

    #[test]
    fn number_literal_decimal_with_fraction_and_underscore() {
        assert_typescript!("type A = 100.000_000;", "type A = 100.000_000");
    }

    #[test]
    fn string_literals() {
        assert_typescript!("type A = \"1\";", "type A = \"1\"");
        assert_typescript!("type A = '1';", "type A = '1'");
    }

    #[test]
    fn template_string_literals() {
        assert_typescript!("type A = `1`;", "type A = `1`");
    }

    #[test]
    fn literal_true() {
        assert_typescript!("type A = true;", "type A = true");
    }

    #[test]
    fn literal_false() {
        assert_typescript!("type A = false;", "type A = false");
    }

    #[test]
    fn literal_null() {
        assert_typescript!("type A = null;", "type A = null");
    }

    #[test]
    fn literal_undefined() {
        assert_typescript!("type A = undefined;", "type A = undefined");
    }

    #[test]
    fn bin_ops_union() {
        assert_typescript!("type A = 1 | 2;", "type A = 1 | 2");
    }

    #[test]
    fn bin_ops_intersection() {
        assert_typescript!("type A = 1 & 2;", "type A = 1 & 2");
    }

    #[test]
    fn bin_ops_union_with_intersection() {
        assert_typescript!("type A = 1 | (2 & 3);", "type A = 1 | 2 & 3");
    }

    #[test]
    fn bin_ops_intersection_with_union() {
        assert_typescript!("type A = 1 & (2 | 3);", "type A = 1 & 2 | 3");
    }

    #[test]
    fn bin_ops_grouped_union_and_intersection() {
        assert_typescript!("type A = (1 | 2) & 3;", "type A = (1 | 2) & 3");
    }

    // If case

    #[test]
    fn if_expr_simple_if_else() {
        assert_typescript!(
            "type A = 1 extends number ? 1 : 0;",
            "type A = if 1 <: number then 1 else 0"
        );
    }

    #[test]
    fn if_expr_if_without_else() {
        assert_typescript!(
            "type A = 1 extends number ? 1 : never;",
            "type A = if 1 <: number then 1"
        );
    }

    #[test]
    fn if_expr_nested_if() {
        assert_typescript!(
            "type A = 1 extends number ? 2 extends number ? 3 : never : never;",
            "type A = if 1 <: number then if 2 <: number then 3"
        );
    }

    #[test]
    fn if_not_extends() {
        assert_typescript!(
            "type A = a extends b ? d : c;",
            "type A = if not a <: b then c else d"
        );
    }

    #[test]
    fn if_and() {
        assert_typescript!(
            "type A = a extends b ? c extends d ? e : f : f;",
            "type A = if a <: b and c <: d then e else f"
        );
    }

    #[test]
    fn if_expr_joined_conditions() {
        assert_typescript!(
            "type A = 1 extends number ? 1 : 0;",
            join!("type A = if 1 <: number", "then 1", "else 0")
        );
    }

    #[test]
    fn object_object_literal_empty() {
        assert_typescript!("type A = {};", "type A = {}");
    }

    #[test]
    fn object_literal_one_key() {
        assert_typescript!("type A = {x: 1};", "type A = {x: 1}");
    }

    #[test]
    fn object_literal_many_keys() {
        assert_typescript!(
            "type A = {x: 1, y: 2, z: 3};",
            "type A = {x: 1, y: 2, z: 3}"
        );
    }

    #[test]
    fn object_literal_readonly_modifier() {
        assert_typescript!("type A = {readonly x: 1};", "type A = {readonly x: 1}");
    }

    #[test]
    fn object_literal_optional_modifier() {
        assert_typescript!("type A = {x?: 1};", "type A = {x?: 1}");
    }

    #[test]
    fn tuple_empty() {
        assert_typescript!("type A = [];", "type A = []");
    }

    #[test]
    fn tuple_single_element() {
        assert_typescript!("type A = [1];", "type A = [1]");
    }

    #[test]
    fn tuple_multiple_elements() {
        assert_typescript!("type A = [1, 2, 3];", "type A = [1, 2, 3]");
    }

    #[test]
    fn tuple_single_string_element() {
        assert_typescript!(r#"type A = ["sup"];"#, r#"type A = ["sup"]"#);
    }

    #[test]
    fn array_of_numbers() {
        assert_typescript!("type A = number[];", "type A = number[]");
    }

    #[test]
    fn array_of_numbers_with_parentheses() {
        assert_typescript!("type A = number[];", "type A = (number)[]");
    }

    #[test]
    fn multidimensional_array_of_numbers() {
        assert_typescript!("type A = number[][][];", "type A = number[][][]");
    }

    #[test]
    fn array_of_union_types() {
        assert_typescript!(
            "type A = (number | string)[];",
            "type A = (number | string)[]"
        );
    }

    #[test]
    fn generics_one_argument() {
        assert_typescript!("type A<x> = x;", "type A(x) = x");
    }

    #[test]
    fn generics_many_arguments() {
        assert_typescript!("type A<x, y, z> = 1;", "type A(x, y, z) = 1");
    }

    #[test]
    fn application_single_type_argument() {
        assert_typescript!("type B = A<1>;", "type B = A(1)");
    }

    #[test]
    fn application_multiple_type_arguments() {
        assert_typescript!("type B = A<1, 2, 3>;", "type B = A(1, 2, 3)");
    }

    #[test]
    fn application_type_argument_with_array() {
        assert_typescript!("type B = A<1, []>;", "type B = A(1, [])");
    }

    #[test]
    fn application_multiple_array_type_arguments() {
        assert_typescript!("type B = A<[], [], []>;", "type B = A([], [], [])");
    }

    #[test]
    fn application_nested_type_argument() {
        assert_typescript!("type B = A<B<1>>;", "type B = A(B(1))");
    }

    #[test]
    fn application_mixed_type_arguments() {
        assert_typescript!("type B = A<B, 1>;", "type B = A(B, 1)");
    }

    #[test]
    fn case_expr() {
        assert_typescript!(
            r#"
            type A<x> = x extends number ? 1 : x extends string ? 2 : 3;
            "#,
            r#"
            type A(x) = match x do
                number => 1,
                string => 2,
                _ => 3
            end
            "#
        );
    }

    #[test]
    fn cond_expr() {
        assert_typescript!(
            r#"
            type A<x> = x extends number ? 1 : x extends {} ? x extends {a: 1} ? 2 : never : never;
            "#,
            r#"
            type A(x) = cond do
                x <: number => 1,
                x <: {} and x <: {a: 1} => 2,
            end
            "#
        );
    }

    #[test]
    fn cond_expr_with_else() {
        assert_typescript!(
            r#"
            type A<x> = x extends number ? 1 : x extends {} ? x extends {a: 1} ? 2 : 3 : 3;
            "#,
            r#"
            type A(x) = cond do
                x <: number => 1,
                x <: {} and x <: {a: 1} => 2,
                else => 3
            end
            "#
        );
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
