use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::default;
use std::iter::FilterMap;

use crate::ast::*;
use crate::simplify::*;
use crate::to_typescript::ToTypescript;
use itertools::Itertools;
use pest::error::{Error, ErrorVariant};
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::PrattParser;
use pest::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct NewtypeParser;

pub type ParserError = pest::error::Error<Rule>;

macro_rules! parse_error {
    ($pair:expr, $message:expr) => {{
        let error = Error::<Rule>::new_from_span(
            ErrorVariant::CustomError { message: $message },
            $pair.as_span(),
        );

        panic!("{error}");
    }};

    ($pair:expr, $positives:expr, $negatives:expr) => {{
        let error = Error::<Rule>::new_from_span(
            ErrorVariant::ParsingError {
                positives: $positives,
                negatives: $negatives,
            },
            $pair.as_span(),
        );
        panic!("{error}");
    }};
}

lazy_static::lazy_static! {
    static ref EXPR_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
            .op(Op::infix(union, Right) | Op::infix(intersection, Right))
            .op(Op::infix(pipe, Left))
            .op(Op::postfix(array_modifier))
            .op(Op::postfix(dot_access))
            .op(Op::postfix(indexed_access))
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
    use Rule::*;
    EXPR_PARSER
        .map_primary(parse_node)
        .map_prefix(|op, _rhs| match op.as_rule() {
            _ => unreachable!("Expected prefix operator, found {:?}", op.as_rule()),
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            indexed_access => {
                let inner = op.into_inner().next().unwrap();
                let index = parse_node(inner);
                Node::Access {
                    lhs: boxed(lhs),
                    rhs: boxed(index),
                    is_dot: false,
                }
            }
            array_modifier => Node::Array(boxed(lhs)),
            dot_access => {
                let index = op.into_inner().next().map(parse_node).unwrap();
                Node::Access {
                    lhs: boxed(lhs),
                    rhs: boxed(index),
                    is_dot: true,
                }
            }
            rule => {
                let error = Error::new_from_span(
                    ErrorVariant::ParsingError {
                        positives: vec![indexed_access, array_modifier, dot_access],
                        negatives: vec![rule],
                    },
                    op.as_span(),
                );

                panic!("{error}")
            }
        })
        .map_infix(|lhs, op, rhs| {
            if op.as_rule() == pipe {
                return match rhs {
                    Node::Ident(rhs_name) => Node::Application(rhs_name.clone(), vec![lhs]),
                    Node::Application(name, params) => {
                        let mut params = params.clone();
                        params.insert(0, lhs);
                        Node::Application(name.clone(), params)
                    }
                    _ => {
                        let error = Error::new_from_span(
                            ErrorVariant::ParsingError {
                                positives: vec![ident, application],
                                // FIXME: don't know what the input rule was
                                negatives: vec![],
                            },
                            op.as_span(),
                        );
                        panic!("{}", error);
                    }
                };
            }

            let op = match op.as_rule() {
                union => Op::Union,
                intersection => Op::Intersection,
                rule => unreachable!("Expected infix operator, found {:?}", rule),
            };

            Node::BinOp {
                lhs: boxed(lhs),
                op,
                rhs: boxed(rhs),
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
        .map_postfix(|_lhs, op| match op.as_rule() {
            rule => unreachable!("Expected postfix operation, found {:?}", rule),
        })
        .map_prefix(|op, primary_node| {
            let op = match op.as_rule() {
                Rule::infer => PrefixOp::Infer,
                Rule::not => PrefixOp::Not,
                rule => unreachable!("Expected prefix operation, found {:?}", rule),
            };

            Node::ExtendsPrefixOp {
                op,
                value: boxed(primary_node),
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
                lhs: boxed(lhs),
                op,
                rhs: boxed(rhs),
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
        Rule::statement => {
            let inner = pair.into_inner().next().unwrap();
            let inner = parse_node(inner);
            Node::Statement(boxed(inner))
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
        Rule::string => {
            let content = match pair.clone().into_inner().next().unwrap().as_rule() {
                Rule::atom_string => pair.as_str().trim_start_matches(':').to_string(),
                Rule::double_quote_string => pair.as_str().trim_matches('"').to_string(),
                Rule::single_quote_string => pair.as_str().trim_matches('\'').to_string(),
                _ => unreachable!(),
            };

            Node::String(content)
        }
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
                .find(tag_eq("arguments"))
                .expect("application missing arguments");

            let arguments = arguments_pair.into_inner().map(parse_node).collect();

            Node::Application(identifier, arguments)
        }
        Rule::builtin => {
            let mut inner = pair.into_inner();

            let name = inner.find(tag_eq("name")).unwrap();

            let name = match name.as_rule() {
                Rule::keyof => BuiltInKeyword::Keyof,
                _ => unreachable!(
                    "unexpected rule while parsing builtin: {:?}",
                    name.as_rule()
                ),
            };

            let argument = inner.find(tag_eq("argument")).map(parse_node).unwrap();

            Node::Builtin {
                name,
                argument: boxed(argument),
            }
        }
        Rule::expr => parse_expr(pair.into_inner()),
        Rule::infer => new_error(
            "Infer operator must be used in an extends expression".to_string(),
            pair,
        ),
        Rule::match_expr => {
            let mut inner = pair.into_inner();

            let value = inner
                .find(tag_eq("value"))
                .map(parse_node)
                .map(boxed)
                .unwrap();

            let arms: Vec<MatchArm> = inner
                .clone()
                .filter(tag_eq("arm"))
                .map(|arm| {
                    let mut inner = arm.into_inner();
                    let pattern = inner.find(tag_eq("pattern")).map(parse_node).unwrap();
                    let body = inner.find(tag_eq("body")).map(parse_node).unwrap();
                    MatchArm { pattern, body }
                })
                .collect();

            let else_ = inner
                .find(tag_eq("else"))
                .and_then(|p| p.into_inner().find(tag_eq("body")))
                .map(parse_node)
                .map(boxed)
                .unwrap_or(boxed(Node::Never));

            Node::MatchExpr { value, arms, else_ }
        }
        Rule::cond_expr => {
            let inner = pair.into_inner();

            let else_ = inner
                .clone()
                .find(tag_eq("else"))
                .and_then(|p| p.into_inner().find(tag_eq("body")))
                .map(parse_node)
                .map(boxed)
                .unwrap_or(boxed(Node::Never));

            let arms: Vec<CondArm> = inner
                .clone()
                .filter(tag_eq("arm"))
                .map(|arm| {
                    let mut inner = arm.into_inner();

                    let condition = inner
                        .find(tag_eq("condition"))
                        .map(|p| p.into_inner())
                        .map(parse_extends_condition)
                        .unwrap();

                    let body = inner.find(tag_eq("body")).map(parse_node).unwrap();

                    CondArm { condition, body }
                })
                .collect();

            Node::CondExpr { arms, else_ }
        }
        Rule::for_expr => {
            let mut inner = pair.into_inner();

            let index = inner.find(tag_eq("index")).unwrap().as_str().to_string();

            let iterable = inner
                .find(tag_eq("iterable"))
                .map(parse_node)
                .map(boxed)
                .unwrap();

            let body = inner
                .find(tag_eq("body"))
                .map(parse_node)
                .map(boxed)
                .unwrap();

            Node::MappedType {
                index,
                iterable,
                body,
                remapped_as: None,
                readonly_mod: None,
                optional_mod: None,
            }
        }
        Rule::let_expr => {
            let bindings: HashMap<_, _> = pair
                .clone()
                .into_inner()
                .filter(tag_eq("binding"))
                .map(|pair| {
                    let mut inner = pair.into_inner();
                    let name = inner.next().unwrap();
                    assert_eq!(name.as_rule(), Rule::ident);

                    let name = name.as_str().to_string();
                    let value = inner.next().unwrap();
                    assert_eq!(value.as_rule(), Rule::expr);
                    let value = parse_node(value);

                    (Identifier(name), value)
                })
                .collect();

            let body: Box<Node> = pair
                .clone()
                .into_inner()
                .find(tag_eq("body"))
                .map(parse_node)
                .map(boxed)
                .unwrap();

            Node::LetExpr { bindings, body }
        }
        Rule::EOI => {
            parse_error!(pair, format!("Unexpected end of input"));
        }
        _ => {
            parse_error!(pair, format!("Unexpected rule: {:?}", rule));
        }
    }
}

fn parse_if_expr(pair: Pair<Rule>) -> Node {
    let mut inner = pair.into_inner();

    let condition = inner
        .find(tag_eq("condition"))
        .map(|p| boxed(parse_extends_condition(p.into_inner())))
        .unwrap();

    let then = inner
        .find(tag_eq("then"))
        .map(parse_node)
        .map(boxed)
        .unwrap();

    let else_ = inner
        .find(tag_eq("else"))
        .map(parse_node)
        .unwrap_or_else(|| Node::Never);

    let else_ = boxed(else_);

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

fn boxed(lhs: Node) -> Box<Node> {
    Box::new(lhs)
}

fn parse_object_literal(pair: Pair<Rule>) -> Node {
    let mut object_property_rules = pair.into_inner();
    let mut properties = Vec::new();

    while let Some(prop_pair) = object_property_rules.next() {
        match prop_pair.as_rule() {
            Rule::object_property => {
                let mut inner = prop_pair.into_inner();

                let readonly = inner.clone().find(tag_eq("readonly")).is_some();

                let key = inner
                    .find(tag_eq("key"))
                    .expect("object property missing key")
                    .as_str()
                    .to_string();

                let optional = inner.clone().find(tag_eq("optional")).is_some();

                let value = inner
                    .find(tag_eq("value"))
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
    let inner = pair.clone().into_inner();

    let export = inner
        .peek()
        .map(|p| p.as_rule() == Rule::export)
        .unwrap_or(false);

    let name = inner
        .clone()
        .find(tag_eq("name"))
        .unwrap()
        .as_str()
        .to_string();

    let body = inner
        .clone()
        .find(tag_eq("body"))
        .map(parse_node)
        .map(boxed)
        .unwrap();

    // Track the order of the inserted parametes
    let mut ordered_params: Vec<&str> = Default::default();

    let mut params: HashMap<&str, TypeParameter> = inner
        .clone()
        .find(tag_eq("parameters"))
        .map(|p| -> HashMap<&str, TypeParameter> {
            p.into_inner()
                .map(|param| {
                    ordered_params.push(param.as_str());

                    (
                        param.as_str(),
                        TypeParameter::new(param.as_str().to_string(), None, None, false),
                    )
                })
                .collect()
        })
        .unwrap_or_default();

    if let Some(where_clause) = inner.clone().find(tag_eq("where")) {
        where_clause
            .into_inner()
            .filter(tag_eq("constraint"))
            .for_each(|pair| {
                let mut inner = pair.clone().into_inner();
                let name = inner.next().unwrap();
                assert_eq!(name.as_node_tag(), Some("constraint_name"));
                assert_eq!(name.as_rule(), Rule::ident);
                let name = name.as_str();

                assert_eq!(inner.next().unwrap().as_rule(), Rule::extends);

                let body = inner.next().unwrap();
                assert_eq!(body.as_node_tag(), Some("constraint_body"));
                assert_eq!(body.as_rule(), Rule::expr);
                let body = parse_node(body);

                let param = match params.get_mut(name) {
                    Some(param) => param,
                    None => {
                        let error = Error::<Rule>::new_from_span(
                            ErrorVariant::CustomError {
                                message: format!(
                                    r#"Type parameter "{}", is missing from signature"#,
                                    name
                                ),
                            },
                            pair.clone().as_span(),
                        );

                        panic!("{error}")
                    }
                };

                param.constraint = Some(boxed(body));
            });
    }

    if let Some(defaults_clause) = inner.clone().find(tag_eq("defaults")) {
        defaults_clause
            .into_inner()
            .filter(tag_eq("default"))
            .for_each(|pair| {
                let mut inner = pair.clone().into_inner();
                let name = inner.next().unwrap();
                assert_eq!(name.as_node_tag(), Some("name"));
                assert_eq!(name.as_rule(), Rule::ident);
                let name = name.as_str();

                let body = inner.next().unwrap();
                assert_eq!(body.as_node_tag(), Some("value"));
                assert_eq!(body.as_rule(), Rule::expr);
                let body = parse_node(body);

                let param = match params.get_mut(name) {
                    Some(param) => param,
                    None => {
                        let error = Error::<Rule>::new_from_span(
                            ErrorVariant::CustomError {
                                message: format!(
                                    r#"Type parameter "{}", is missing from signature"#,
                                    name
                                ),
                            },
                            pair.clone().as_span(),
                        );

                        panic!("{error}")
                    }
                };

                param.default = Some(boxed(body));
            });
    };

    let params = ordered_params
        .iter()
        .map(|name| params.get(name).unwrap().clone())
        .collect();

    Node::TypeAlias {
        export,
        name,
        params,
        body,
    }
}

fn text(pair: Pair<Rule>) -> String {
    pair.as_str().to_string()
}

/**
* Returns a predicate that matches a pair with the given tag.
*/
fn tag_eq<'a>(tag: &'a str) -> impl FnMut(&Pair<'a, Rule>) -> bool {
    |pair: &Pair<Rule>| pair.as_node_tag() == Some(tag)
}

// Generate tests for all test cases in tests/pest/foo/ and all subdirectories. Since
// `lazy_static = true`, a single `PestTester` is created and used by all tests; otherwise a new
// `PestTester` would be created for each test.
#[cfg(test)]
mod parser_tests {
    use super::*;
    use crate::test_support::*;
    use pest::consumes_to;
    use pest::parses_to;
    use pretty_assertions::{assert_eq, assert_ne};
    use std::assert_matches::assert_matches;
    use textwrap_macros::dedent;
    use Rule::*;

    #[test]
    fn array_access_single_quote() {
        assert_typescript!(expr, "A['field']", "A['field']");
    }

    #[test]
    fn array_access_double_quote() {
        assert_typescript!(expr, r#"A['field']"#, r#"A["field"]"#);
    }

    #[test]
    fn array_access_template_string() {
        assert_typescript!(expr, r#"A[`${x}`]"#, r#"A[`${x}`]"#);
    }

    #[test]
    fn dot_access() {
        assert_typescript!(expr, r#"A["x"]"#, r#"A.x"#);
    }

    fn chained_dot_access() {
        assert_typescript!(expr, r#"A["x"]["y"]["z"]"#, r#"A.x.y.z"#);
    }

    #[test]
    fn chained_array_access() {
        assert_typescript!(expr, r#"A['x']['y']['z']"#, r#"A['x']['y']['z']"#);
    }

    #[test]
    fn array_access_indexer() {
        assert_typescript!(expr, r#"A[string]"#, r#"A[string]"#);
    }

    #[test]
    fn primitive_number() {
        assert_typescript!("type A = number;", "type A as number");
    }

    #[test]
    fn primitive_boolean() {
        assert_typescript!("type A = boolean;", "type A as boolean");
    }

    #[test]
    fn primitive_never() {
        assert_typescript!("type A = never;", "type A as never");
    }

    #[test]
    fn primitive_any() {
        assert_typescript!("type A = any;", "type A as any");
    }

    #[test]
    fn primitive_unknown() {
        assert_typescript!("type A = unknown;", "type A as unknown");
    }

    #[test]
    fn number_literal_positive_integer() {
        assert_typescript!("type A = 1;", "type A as 1");
    }

    #[test]
    fn number_literal_negative_integer() {
        assert_typescript!("type A = -1;", "type A as -1");
    }

    #[test]
    fn number_literal_negative_integer_with_space() {
        assert_typescript!("type A = - 1;", "type A as - 1");
    }

    #[test]
    fn number_literal_large_integer() {
        assert_typescript!("type A = 100;", "type A as 100");
    }

    #[test]
    fn number_literal_integer_with_underscore() {
        assert_typescript!("type A = 1_000;", "type A as 1_000");
    }

    #[test]
    fn number_literal_decimal_without_fraction() {
        assert_typescript!("type A = 100.;", "type A as 100.");
    }

    #[test]
    fn number_literal_decimal_with_fraction() {
        assert_typescript!("type A = 100.0;", "type A as 100.0");
    }

    #[test]
    fn number_literal_decimal_with_fraction_and_underscore() {
        assert_typescript!("type A = 100.000_000;", "type A as 100.000_000");
    }

    #[test]
    fn string_atom_literals() {
        assert_typescript!(expr, r#"'x'"#, ":x");
    }

    #[test]
    fn string_atom_literals_space() {
        assert_typescript!(expr, r#"['$x', '--y__']"#, "[:$x, :--y__]");
    }

    #[test]
    fn string_double_quoted_literals() {
        assert_typescript!(expr, r#"'1'"#, r#""1""#);
    }

    #[test]
    fn string_single_quoted_literals() {
        assert_typescript!(expr, "'1'", "'1'");
    }

    #[test]
    fn template_string_literals() {
        assert_typescript!(expr, "`1`", "`1`");
    }

    #[test]
    fn literal_true() {
        assert_typescript!("type A = true;", "type A as true");
    }

    #[test]
    fn literal_false() {
        assert_typescript!("type A = false;", "type A as false");
    }

    #[test]
    fn literal_null() {
        assert_typescript!("type A = null;", "type A as null");
    }

    #[test]
    fn literal_undefined() {
        assert_typescript!("type A = undefined;", "type A as undefined");
    }

    #[test]
    fn bin_ops_pipe_into_identifier() {
        assert_typescript!(Rule::program, r#"type A = Y<X>;"#, r#"type A as X |> Y"#);
    }

    #[test]
    fn bin_ops_pipe_literal() {
        assert_typescript!(Rule::program, r#"type A = Y<1>;"#, r#"type A as 1 |> Y"#);
    }

    #[test]
    fn bin_ops_pipe_into_call_with_args() {
        assert_typescript!(
            Rule::program,
            r#"type A = Y<X, 1>;"#,
            r#"type A as X |> Y(1)"#
        );
    }

    #[test]
    fn bin_ops_pipe_chained() {
        assert_typescript!(
            Rule::program,
            r#"type A = D<C<B<A, 1>>, 2, 3, 4>;"#,
            r#"type A as A |> B(1) |> C |> D(2, 3, 4)"#
        );
    }

    #[test]
    fn bin_ops_union() {
        assert_typescript!("type A = 1 | 2;", "type A as 1 | 2");
    }

    #[test]
    fn bin_ops_intersection() {
        assert_typescript!("type A = 1 & 2;", "type A as 1 & 2");
    }

    #[test]
    fn bin_ops_union_with_intersection() {
        assert_typescript!("type A = 1 | (2 & 3);", "type A as 1 | 2 & 3");
    }

    #[test]
    fn bin_ops_intersection_with_union() {
        assert_typescript!("type A = 1 & (2 | 3);", "type A as 1 & 2 | 3");
    }

    #[test]
    fn bin_ops_grouped_union_and_intersection() {
        assert_typescript!("type A = (1 | 2) & 3;", "type A as (1 | 2) & 3");
    }

    // If case

    #[test]
    fn if_expr_simple_if_else() {
        assert_typescript!(
            "type A = 1 extends number ? 1 : 0;",
            "type A as if 1 <: number then 1 else 0 end"
        );
    }

    #[test]
    fn if_expr_if_without_else() {
        assert_typescript!(
            "type A = 1 extends number ? 1 : never;",
            "type A as if 1 <: number then 1 end"
        );
    }

    #[test]
    fn if_expr_nested_if() {
        assert_typescript!(
            expr,
            r#"
            1 extends number
                ? 2 extends number
                    ? 3
                    : never
                : never
            "#,
            r#"
            if 1 <: number then
                if 2 <: number then 3 end
            end
            "#
        );
    }

    #[test]
    fn if_expr_nested_if_else() {
        assert_typescript!(
            expr,
            r#"
            a extends b
                ? c extends d
                    ? x
                    : never
                : never
            "#,
            r#"
            if a <: b then
                if c <: d then
                    x
                end
            end
            "#
        );

        assert_typescript!(
            expr,
            r#"
            a extends b
                ? c extends d
                    ? x
                    : y
                : never
            "#,
            r#"
            if a <: b then
                if c <: d then
                    x
                else
                    y
                end
            end
            "#
        );

        assert_typescript!(
            expr,
            r#"
            a extends b
                ? c extends d
                    ? x
                    : never
                : z
            "#,
            r#"
            if a <: b then
                if c <: d then
                    x
                end
            else
                z
            end
            "#
        );

        assert_typescript!(
            expr,
            r#"
            a extends b
                ? c extends d
                    ? x
                    : y
                : z
            "#,
            r#"
            if a <: b then
                if c <: d then
                    x
                else
                    y
                end
            else
                z
            end
            "#
        );
    }

    #[test]
    fn if_not_extends() {
        assert_typescript!(
            "type A = a extends b ? d : c;",
            r#"
            type A as
                if not a <: b then
                    c
                else
                    d
                end
            "#
        );
    }

    #[test]
    fn if_and() {
        assert_typescript!(
            "type A = a extends b ? c extends d ? e : f : f;",
            "type A as if a <: b and c <: d then e else f end"
        );
    }

    #[test]
    fn if_expr_joined_conditions() {
        assert_typescript!(
            "type A = 1 extends number ? 1 : 0;",
            r#"type A as if 1 <: number then 1 else 0 end"#
        );
    }

    #[test]
    fn if_keyof() {
        assert_typescript!(
            expr,
            r#"
            x extends keyof y
                ? 1
                : never
            "#,
            "if x <: keyof(y) then 1 end"
        );
    }

    #[test]
    fn for_in() {
        assert_typescript!(
            expr,
            r#"
            { [k in t]: 1 }
            "#,
            r#"
            for k in t do 1 end
            "#
        );
    }

    #[test]
    fn object_object_literal_empty() {
        assert_typescript!("type A = {};", "type A as {}");
    }

    #[test]
    fn object_literal_one_key() {
        assert_typescript!("type A = {x: 1};", "type A as {x: 1}");
    }

    #[test]
    fn object_literal_many_keys() {
        assert_typescript!(
            "type A = {x: 1, y: 2, z: 3};",
            "type A as {x: 1, y: 2, z: 3}"
        );
    }

    #[test]
    fn object_literal_readonly_modifier() {
        assert_typescript!("type A = {readonly x: 1};", "type A as {readonly x: 1}");
    }

    #[test]
    fn object_literal_optional_modifier() {
        assert_typescript!("type A = {x?: 1};", "type A as {x?: 1}");
    }

    #[test]
    fn tuple_empty() {
        assert_typescript!("type A = [];", "type A as []");
    }

    #[test]
    fn tuple_single_element() {
        assert_typescript!("type A = [1];", "type A as [1]");
    }

    #[test]
    fn tuple_multiple_elements() {
        assert_typescript!("type A = [1, 2, 3];", "type A as [1, 2, 3]");
    }

    #[test]
    fn tuple_single_string_element() {
        assert_typescript!(r#"type A = ['sup'];"#, r#"type A as [:sup]"#);
    }

    #[test]
    fn array_2d() {
        assert_typescript!(expr, "number[]", "number[]");
    }
    #[test]
    fn array_4d() {
        assert_typescript!(expr, "number[][][]", "number[][][]");
    }

    #[test]
    fn array_of_numbers_with_parentheses() {
        assert_typescript!("type A = number[];", "type A as (number)[]");
    }

    #[test]
    fn array_of_union_types() {
        assert_typescript!(
            "type A = (number | string)[];",
            "type A as (number | string)[]"
        );
    }

    #[test]
    fn generics_one_argument() {
        assert_typescript!("type A<x> = x;", "type A(x) as x");
    }

    #[test]
    fn generics_many_arguments() {
        assert_typescript!("type A<x, y, z> = 1;", "type A(x, y, z) as 1");
    }

    #[test]
    fn generic_with_guard() {
        assert_typescript!("type A<x, y, z> = 1;", "type A(x, y, z) as 1");
    }

    #[test]
    fn exported_type() {
        assert_typescript!("export type A = 1;", "export type A as 1");
    }

    #[test]
    fn type_where_clause() {
        assert_typescript!(
            r#"type A<x extends number> = x;"#,
            r#"type A(x) where x <: number as x"#
        );
    }

    #[test]
    fn type_defaults_clause() {
        assert_typescript!(
            r#"type A<x = number> = x;"#,
            r#"type A(x) defaults x = number as x"#
        );
    }

    #[test]
    fn type_where_and_defaults_clause() {
        assert_typescript!(
            r#"type A<x extends number = number> = x;"#,
            r#"type A(x) defaults x = number where x <: number as x"#
        );
    }

    #[test]
    fn application_single_type_argument() {
        assert_typescript!(expr, "A<1>", "A(1)");
    }

    #[test]
    fn application_multiple_type_arguments() {
        assert_typescript!("type B = A<1, 2, 3>;", "type B as A(1, 2, 3)");
    }

    #[test]
    fn application_type_argument_with_array() {
        assert_typescript!("type B = A<1, []>;", "type B as A(1, [])");
    }

    #[test]
    fn application_multiple_array_type_arguments() {
        assert_typescript!("type B = A<[], [], []>;", "type B as A([], [], [])");
    }

    #[test]
    fn application_nested_type_argument() {
        assert_typescript!("type B = A<B<1>>;", "type B as A(B(1))");
    }

    #[test]
    fn application_mixed_type_arguments() {
        assert_typescript!("type B = A<B, 1>;", "type B as A(B, 1)");
    }

    #[test]
    fn keyof() {
        assert_typescript!(expr, "keyof A", "keyof(A)");
    }

    #[test]
    fn keyof_twice() {
        assert_typescript!(expr, "keyof keyof A", "keyof(keyof(A))");
    }

    #[test]
    fn match_expr() {
        assert_typescript!(
            r#"
            type A<x> = x extends number ? 1 : x extends string ? 2 : 3;
            "#,
            r#"
            type A(x) as match x do
                number => 1,
                string => 2,
                else => 3
            end
            "#
        );
    }

    #[test]
    fn cond_expr() {
        assert_typescript!(
            r#"
            type A<x> =
                x extends number
                    ? 1
                    : x extends {}
                        ? x extends {a: 1}
                            ? 2
                            : never
                        : never;
            "#,
            r#"
            type A(x) as cond do
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
            type A(x) as cond do
                x <: number => 1,
                x <: {} and x <: {a: 1} => 2,
                else => 3
            end
            "#
        );
    }

    #[test]
    fn let_expr() {
        assert_typescript!(
            r#"
            type A = 1;
            "#,
            r#"
            type A as let a = 1 in a
            "#
        );
    }

    #[test]
    fn let_expr_multi_definitions() {
        assert_typescript!(
            r#"
            type A = [1, 2];
            "#,
            r#"
            type A as
                let a = 1,
                    b = 2,
                in [a, b]
            "#
        );
    }

    #[test]
    fn let_expr_no_self_reference() {
        assert_typescript!(
            r#"
            type A = [1, a];
            "#,
            r#"
            type A as
                let a = 1,
                    b = a,
                in [a, b]
            "#
        );
    }

    #[test]
    fn let_expr_nested() {
        assert_typescript!(
            r#"
            type A = [[1, 1], [1, 1]];
            "#,
            r#"
            type A as
                let a = 1 in
                let b = [a, a] in
                let c = [b, b] in
                c
            "#
        );
    }

    #[test]
    fn ts_toolbelt_any_at() {
        assert_typescript!(
            statement,
            r#"
            export type At<A extends any, K extends Key> =
                A extends List
                    ? number extends A["length"]
                        ? K extends number | `${number}`
                            ? A[never] | undefined
                            : undefined
                        : K extends keyof A
                            ? A[K]
                            : undefined
                    : unknown extends A
                        ? unknown
                        : K extends keyof A
                            ? A[K]
                            : undefined;
            "#,
            r#"
            export type At(A, K)
                where
                    A <: any,
                    K <: Key
                as
                    let a_k =
                        if K <: keyof(A) then
                            A[K]
                        else
                            undefined
                        end
                    in

                    cond do
                        A <: List =>
                            if number <: A.length then
                                if K <: number | `${number}` then
                                    A[never] | undefined
                                else
                                    undefined
                                end
                            else
                                a_k
                            end,

                        unknown <: A => unknown,

                        else => a_k
                    end

            "#
        );
    }

    #[test]
    fn ts_toolbelt_function_exact() {
        assert_typescript!(
            statement,
            r#"
            type Exact<A, W> =
                W extends unknown
                    ? A extends W
                        ? A extends Narrowable
                            ? A
                            : {
                                [K in keyof A]:
                                    K extends keyof W
                                        ? Exact<A[K], W[K]>
                                        : never
                            }
                        : W
                    : never;
            "#,
            r#"
            type Exact(A, W) as
                if W <: unknown then
                    if A <: W then
                        if A <: Narrowable then
                            A
                        else
                            for K in keyof(A) do
                                if K <: keyof(W) then
                                    Exact(A[K], W[K])
                                end
                            end
                        end
                    else
                        W
                    end
                end
            "#
        );
    }

    #[test]
    fn ts_toolbelt_list_assign() {
        assert_typescript!(
            statement,
            r#"
            export type Assign<
                L extends List,
                Ls extends List<List>,
                depth extends Depth = 'flat',
                ignore extends object = BuiltIn,
                fill extends any = never
            > =
                Cast<OAssign<L, Ls, depth, ignore, fill>, List>;
            "#,
            r#"
            export type Assign(L, Ls, depth, ignore, fill)
                defaults
                    depth = :flat,
                    ignore = BuiltIn,
                    fill = never,
                where
                    L <: List,
                    Ls <: List(List),
                    depth <: Depth,
                    ignore <: object,
                    fill <: any
                as
                    L
                    |> OAssign(Ls, depth, ignore, fill)
                    |> Cast(List)
            "#
        );
    }
}
