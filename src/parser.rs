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

pub fn expr(pairs: Pairs<Rule>) -> Node {
    EXPR_PARSER
        .map_primary(node)
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

// fn explode_if_expr(cond: &ExtendsExpr, then: &Node, els: &Option<Box<Node>>) -> Node {
//     Node::ExtendsExpr(
//         Box::new(Node::Number("1".to_string())),
//         Box::new(Node::Number("2".to_string())),
//         Box::new(Node::Number("3".to_string())),
//         Box::new(Node::Number("4".to_string())),
//         // Box::new(then.clone()),
//         // Box::new(els.clone()),
//         // Box::new(then.clone()),
//         // Box::new(els.clone()),
//     )
// }

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

fn parse_extends_condition(pairs: Pairs<Rule>) -> Node {
    EXTENDS_PARSER
        .map_primary(node)
        .map_prefix(|op, primary| match op.as_rule() {
            Rule::infer => match primary {
                Node::Ident(ident) => Node::Infer(ident.to_string()),
                _ => new_error("Only identifiers may be inferred".to_string(), op),
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
            Node::ExtendsBinOp {
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
            let inner = pair.into_inner();

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
        Rule::expr => expr(pair.into_inner()),
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
                    .map(node)
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
            assert_eq!($expected, result.simplify().to_pretty_ts(usize::MAX));
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
    fn primitive_string() {
        assert_typescript!("type A = string;\n", "type A = string");
    }

    #[test]
    fn primitive_number() {
        assert_typescript!("type A = number;\n", "type A = number");
    }

    #[test]
    fn primitive_boolean() {
        assert_typescript!("type A = boolean;\n", "type A = boolean");
    }

    #[test]
    fn primitive_never() {
        assert_typescript!("type A = never;\n", "type A = never");
    }

    #[test]
    fn primitive_any() {
        assert_typescript!("type A = any;\n", "type A = any");
    }

    #[test]
    fn primitive_unknown() {
        assert_typescript!("type A = unknown;\n", "type A = unknown");
    }

    #[test]
    fn number_literal_positive_integer() {
        assert_typescript!("type A = 1;\n", "type A = 1");
    }

    #[test]
    fn number_literal_negative_integer() {
        assert_typescript!("type A = -1;\n", "type A = -1");
    }

    #[test]
    fn number_literal_negative_integer_with_space() {
        assert_typescript!("type A = - 1;\n", "type A = - 1");
    }

    #[test]
    fn number_literal_large_integer() {
        assert_typescript!("type A = 100;\n", "type A = 100");
    }

    #[test]
    fn number_literal_integer_with_underscore() {
        assert_typescript!("type A = 1_000;\n", "type A = 1_000");
    }

    #[test]
    fn number_literal_decimal_without_fraction() {
        assert_typescript!("type A = 100.;\n", "type A = 100.");
    }

    #[test]
    fn number_literal_decimal_with_fraction() {
        assert_typescript!("type A = 100.0;\n", "type A = 100.0");
    }

    #[test]
    fn number_literal_decimal_with_fraction_and_underscore() {
        assert_typescript!("type A = 100.000_000;\n", "type A = 100.000_000");
    }

    #[test]
    fn string_literals() {
        assert_typescript!("type A = \"1\";\n", "type A = \"1\"");
        assert_typescript!("type A = '1';\n", "type A = '1'");
    }

    #[test]
    fn template_string_literals() {
        assert_typescript!("type A = `1`;\n", "type A = `1`");
    }

    #[test]
    fn literal_true() {
        assert_typescript!("type A = true;\n", "type A = true");
    }

    #[test]
    fn literal_false() {
        assert_typescript!("type A = false;\n", "type A = false");
    }

    #[test]
    fn literal_null() {
        assert_typescript!("type A = null;\n", "type A = null");
    }

    #[test]
    fn literal_undefined() {
        assert_typescript!("type A = undefined;\n", "type A = undefined");
    }

    #[test]
    fn bin_ops_union() {
        assert_typescript!("type A = 1 | 2;\n", "type A = 1 | 2");
    }

    #[test]
    fn bin_ops_intersection() {
        assert_typescript!("type A = 1 & 2;\n", "type A = 1 & 2");
    }

    #[test]
    fn bin_ops_union_with_intersection() {
        assert_typescript!("type A = 1 | (2 & 3);\n", "type A = 1 | 2 & 3");
    }

    #[test]
    fn bin_ops_intersection_with_union() {
        assert_typescript!("type A = 1 & (2 | 3);\n", "type A = 1 & 2 | 3");
    }

    #[test]
    fn bin_ops_grouped_union_and_intersection() {
        assert_typescript!("type A = (1 | 2) & 3;\n", "type A = (1 | 2) & 3");
    }

    #[test]
    fn statment_indent_sensitive() {
        assert_matches!(parse_newtype(" type A = 1"), Err(_));
        // assert_matches!(parse_newtype(join!("type A =", "1")), Err(_));
    }

    // If case

    #[test]
    fn if_expr_simple_if_else() {
        assert_typescript!(
            "type A = 1 extends number ? 1 : 0;\n",
            "type A = if 1 <: number then 1 else 0"
        );
    }

    #[test]
    fn if_expr_if_without_else() {
        assert_typescript!(
            "type A = 1 extends number ? 1 : never;\n",
            "type A = if 1 <: number then 1"
        );
    }

    #[test]
    fn if_expr_nested_if() {
        assert_typescript!(
            "type A = 1 extends number ? 2 extends number ? 3 : never : never;\n",
            "type A = if 1 <: number then if 2 <: number then 3"
        );
    }

    #[test]
    fn if_expr_if_with_and_condition() {
        assert_typescript!(
            "type A = 1 extends number ? 2 extends number ? 3 : never : never;\n",
            "type A = if 1 <: number && 2 <:3 then 3"
        );
    }

    #[test]
    fn if_expr_joined_conditions() {
        assert_typescript!(
            "type A = 1 extends number ? 1 : 0;\n",
            join!("type A = if 1 <: number", "then 1", "else 0")
        );
    }

    #[test]
    fn object_object_literal_empty() {
        assert_typescript!("type A = {};\n", "type A = {}");
    }

    #[test]
    fn object_literal_one_key() {
        assert_typescript!("type A = {x: 1};\n", "type A = {x: 1}");
    }

    #[test]
    fn object_literal_many_keys() {
        assert_typescript!(
            "type A = {x: 1, y: 2, z: 3};\n",
            "type A = {x: 1, y: 2, z: 3}"
        );
    }

    #[test]
    fn object_literal_readonly_modifier() {
        assert_typescript!("type A = {readonly x: 1};\n", "type A = {readonly x: 1}");
    }

    #[test]
    fn object_literal_optional_modifier() {
        assert_typescript!("type A = {x?: 1};\n", "type A = {x?: 1}");
    }

    #[test]
    fn tuple_empty() {
        assert_typescript!("type A = [];\n", "type A = []");
    }

    #[test]
    fn tuple_single_element() {
        assert_typescript!("type A = [1];\n", "type A = [1]");
    }

    #[test]
    fn tuple_multiple_elements() {
        assert_typescript!("type A = [1, 2, 3];\n", "type A = [1, 2, 3]");
    }

    #[test]
    fn tuple_single_string_element() {
        assert_typescript!("type A = [\"sup\"];\n", "type A = [\"sup\"]");
    }

    #[test]
    fn array_of_numbers() {
        assert_typescript!("type A = number[];\n", "type A = number[]");
    }

    #[test]
    fn array_of_numbers_with_parentheses() {
        assert_typescript!("type A = number[];\n", "type A = (number)[]");
    }

    #[test]
    fn multidimensional_array_of_numbers() {
        assert_typescript!("type A = number[][][];\n", "type A = number[][][]");
    }

    #[test]
    fn array_of_union_types() {
        assert_typescript!(
            "type A = (number | string)[];\n",
            "type A = (number | string)[]"
        );
    }

    #[test]
    fn generics() {
        assert_typescript!("type A<x> = x;\n", "type A x = x");
        assert_typescript!("type A<x, y, z> = 1;\n", "type A x y z = 1");
    }

    #[test]
    fn application_single_type_argument() {
        assert_typescript!("type B = A<1>;\n", "type B = A 1");
    }

    #[test]
    fn application_multiple_type_arguments() {
        assert_typescript!("type B = A<1, 2, 3>;\n", "type B = A 1 2 3");
    }

    #[test]
    fn application_type_argument_with_array() {
        assert_typescript!("type B = A<1, []>;\n", "type B = A 1 []");
    }

    #[test]
    fn application_multiple_array_type_arguments() {
        assert_typescript!("type B = A<[], [], []>;\n", "type B = A [] [] []");
    }

    #[test]
    fn application_nested_type_argument() {
        assert_typescript!("type B = A<B<1>>;\n", "type B = A (B 1)");
    }

    #[test]
    fn application_mixed_type_arguments() {
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
