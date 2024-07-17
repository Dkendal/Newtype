pub(crate) mod pratt;

use std::{
    collections::{BTreeSet, HashMap},
    default,
    iter::FilterMap,
};

use crate::{
    ast::{macros::*, *},
    typescript,
};

use itertools::Itertools;

use pest::{
    error::{Error, ErrorVariant},
    pratt_parser::PrattParser,
    Parser,
};

use pratt::{EXPR_PARSER, EXTENDS_PARSER};
use Interface;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct NewtypeParser;

pub type ParserError = Error<Rule>;

pub type Pair<'i> = pest::iterators::Pair<'i, Rule>;
pub type Pairs<'i> = pest::iterators::Pairs<'i, Rule>;

pub(crate) fn parse_expr(pairs: Pairs) -> AstNode {
    use Rule::*;
    EXPR_PARSER
        .map_primary(parse_node)
        .map_prefix(|op, _rhs| unreachable!("Expected prefix operator, found {:?}", op.as_rule()))
        .map_postfix(|lhs, op| match op.as_rule() {
            indexed_access => {
                let inner = op.into_inner().next().unwrap();
                let index = parse_node(inner);
                // FIXME missing span
                Ast::Access {
                    lhs,
                    rhs: index,
                    is_dot: false,
                }
                .into()
            }
            array_modifier => Node::from_pair(&op, Ast::Array(lhs)),
            dot_access => {
                let index = op.into_inner().next().map(parse_node).unwrap();
                // FIXME missing span
                Ast::Access {
                    lhs,
                    rhs: (index),
                    is_dot: true,
                }
                .into()
            }
            namespace_access => {
                let rhs = op.into_inner().next().map(parse_node).unwrap();
                // FIXME missing span
                Ast::NamespaceAccess(NamespaceAccess { lhs, rhs }).into()
            }
            rule => {
                parse_error!(
                    op,
                    vec![namespace_access, indexed_access, array_modifier, dot_access],
                    vec![rule]
                );
            }
        })
        .map_infix(|lhs, op, rhs| {
            if op.as_rule() == pipe {
                return replace_pipe_with_type_application(rhs, lhs, op);
            }

            let op = match op.as_rule() {
                union => Op::Union,
                intersection => Op::Intersection,
                rule => unreachable!("Expected infix operator, found {:?}", rule),
            };

            // FIXME missing span
            Ast::InfixOp { lhs, op, rhs }.into()
        })
        .parse(pairs)
}

/// Replace the pipe operator macro with a type application
/// ```
/// A |> B
/// # B(A)
/// ```
fn replace_pipe_with_type_application<'a>(
    rhs: AstNode<'a>,
    lhs: AstNode<'a>,
    op: Pair<'a>,
) -> AstNode<'a> {
    match &*rhs.value {
        Ast::Ident(rhs_name) => {
            // FIXME missing span
            Node::from_pair(
                &op,
                Ast::Application(Application {
                    name: rhs_name.clone(),
                    args: vec![lhs],
                }),
            )
        }
        Ast::Application(Application { name, args: params }) => {
            let mut params = params.clone();
            params.insert(0, lhs);
            // FIXME missing span
            Node::from_pair(
                &op,
                Ast::Application(Application {
                    name: name.clone(),
                    args: params,
                }),
            )
        }
        _ => {
            let error = Error::new_from_span(
                ErrorVariant::ParsingError {
                    positives: vec![Rule::ident, Rule::application],
                    // FIXME: don't know what the input rule was
                    negatives: vec![],
                },
                op.as_span(),
            );
            panic!("{}", error);
        }
    }
}

pub(crate) fn parse_newtype_program(source: &str) -> Result<AstNode<'_>, Box<Error<Rule>>> {
    let pair = NewtypeParser::parse(Rule::program, source)?.next().unwrap();

    Ok(parse_node(pair))
}

pub(crate) fn parse_extends_expr(pairs: Pairs) -> AstNode {
    EXTENDS_PARSER
        .map_primary(|pair| match pair.as_rule() {
            Rule::expr => parse_expr(pair.into_inner()),
            Rule::extends_expr => parse_extends_expr(pair.into_inner()),
            rule => parse_error!(pair, vec![Rule::expr, Rule::extends_expr], vec![rule]),
        })
        .map_postfix(|_lhs, op| {
            unreachable!("Expected postfix operation, found {:?}", op.as_rule())
        })
        .map_prefix(|op, primary_node| {
            if op.as_rule() == Rule::not && !primary_node.value.is_extends_infix_op() {
                let error_not = Error::<Rule>::new_from_span(
                    ErrorVariant::CustomError {
                        message: "`not` may only be used with an extends expression".to_string(),
                    },
                    op.as_span(),
                );

                let error_expr = if let Some(span) = primary_node.span {
                    // FIXME need rule for the primary node for better reporting
                    let error = Error::<Rule>::new_from_span(
                        ErrorVariant::ParsingError {
                            positives: vec![Rule::extends_expr],
                            negatives: vec![],
                        },
                        span,
                    );

                    format!("{error}")
                } else {
                    String::new()
                };

                panic!("{error_not}\n{error_expr}\nHint: You might have forgotten to wrap the expression in parentheses, `not` has higher precedence than other operators.");
            }

            let op = match op.as_rule() {
                Rule::infer => PrefixOp::Infer,
                Rule::not => PrefixOp::Not,
                rule => parse_error!(op, vec![Rule::infer, Rule::not], vec![rule]),
            };

            Node::new(
                primary_node.span,
                Ast::ExtendsPrefixOp {
                    op,
                    value: primary_node,
                },
            )
        })
        .map_infix(|lhs: Node<Ast>, op: Pair, rhs: Node<Ast>| {
            let op = match op.as_rule() {
                Rule::extends => InfixOp::Extends,
                Rule::not_extends => InfixOp::NotExtends,
                Rule::equals => InfixOp::Equals,
                Rule::not_equals => InfixOp::NotEquals,
                Rule::strict_equals => InfixOp::StrictEquals,
                Rule::strict_not_equals => InfixOp::StrictNotEquals,
                Rule::and => InfixOp::And,
                Rule::or => InfixOp::Or,
                rule => parse_error!(
                    op,
                    vec![
                        Rule::extends,
                        Rule::not_extends,
                        Rule::equals,
                        Rule::not_equals,
                        Rule::strict_equals,
                        Rule::strict_not_equals,
                        Rule::and,
                        Rule::or
                    ],
                    vec![rule]
                ),
            };
            // FIXME missing span
            Ast::ExtendsInfixOp { lhs, op, rhs }.into()
        })
        .parse(pairs)
}

pub(crate) fn parse_node(pair: Pair) -> AstNode {
    let rule = pair.clone().as_rule();
    let span = pair.clone().as_span();
    let new = |ast| Node::from_span(span, ast);

    // TODO: just return AST, remove calls to new/1 wrap result
    match rule {
        Rule::program => {
            let children: Vec<_> = pair
                .clone()
                .into_inner()
                .filter(|pair| pair.as_rule() != Rule::EOI) // Remove the end of input token
                .map(parse_node)
                .collect();

            new(Ast::Program(children))
        }
        Rule::statement => {
            let inner = pair.into_inner().next().unwrap();
            let inner = parse_node(inner);
            new(Ast::Statement(inner))
        }
        Rule::type_alias => parse_type_alias(pair),
        Rule::interface => parse_interface(pair),
        Rule::import_statement => parse_import_statement(pair),
        Rule::if_expr => parse_if_expr(pair),
        Rule::object_literal => new(Ast::ObjectLiteral(parse_object_literal(pair))),
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
            new(Ast::Primitive(primitive))
        }
        Rule::number => new(Ast::Number(node_as_string(pair))),
        Rule::string => parse_string(pair),
        Rule::template_string => new(Ast::TemplateString(node_as_string(pair))),
        Rule::ident => new(Ast::Ident(pair.as_str().into())),
        Rule::never => new(Ast::Never),
        Rule::any => new(Ast::Any),
        Rule::unknown => new(Ast::Unknown),
        Rule::literal_true => new(Ast::True),
        Rule::literal_false => new(Ast::False),
        Rule::null => new(Ast::Null),
        Rule::undefined => new(Ast::Undefined),
        Rule::tuple => parse_tuple(pair),
        Rule::application => {
            let mut inner = pair.into_inner();

            let identifier = inner.next().map(node_as_string).unwrap().into();

            let arguments_pair = inner
                .find(match_tag("arguments"))
                .expect("application missing arguments");

            let arguments = arguments_pair.into_inner().map(parse_node).collect();

            new(Ast::Application(Application {
                name: identifier,
                args: arguments,
            }))
        }
        Rule::builtin => {
            let mut inner = pair.into_inner();

            let name = inner.find(match_tag("name")).unwrap();

            let name = match name.as_rule() {
                Rule::keyof => BuiltInKeyword::Keyof,
                _ => unreachable!(
                    "unexpected rule while parsing builtin: {:?}",
                    name.as_rule()
                ),
            };

            let argument = inner.find(match_tag("argument")).map(parse_node).unwrap();

            new(Ast::Builtin {
                name,
                argument: (argument),
            })
        }
        Rule::expr => parse_expr(pair.into_inner()),
        Rule::infer => parse_error!(
            pair,
            format!("Infer operator must be used in an extends expression")
        ),
        Rule::match_expr => {
            let mut inner = pair.into_inner();

            let value = inner.find(match_tag("value")).map(parse_node).unwrap();

            let arms: Vec<match_expr::Arm> = inner
                .clone()
                .filter(match_tag("arm"))
                .map(|arm| {
                    let mut inner = arm.into_inner();
                    let pattern = inner.find(match_tag("pattern")).map(parse_node).unwrap();
                    let body = inner.find(match_tag("body")).map(parse_node).unwrap();
                    match_expr::Arm { pattern, body }
                })
                .collect();

            let else_arm = inner
                .find(match_tag("else"))
                .and_then(|p| p.into_inner().find(match_tag("body")))
                .map(parse_node)
                .unwrap_or_else(|| Ast::Never.into());

            new(Ast::MatchExpr(match_expr::Expr {
                value,
                arms,
                else_arm,
            }))
        }
        Rule::cond_expr => {
            let inner = pair.into_inner();

            let else_arm = inner
                .clone()
                .find(match_tag("else"))
                .and_then(|p| p.into_inner().find(match_tag("body")))
                .map(parse_node)
                .unwrap_or_else(|| Ast::Never.into());

            let arms: Vec<cond_expr::Arm> = inner
                .clone()
                .filter(match_tag("arm"))
                .map(|arm| {
                    let mut inner = arm.into_inner();

                    let condition = inner
                        .find(match_tag("condition"))
                        .map(|p| p.into_inner())
                        .map(parse_extends_expr)
                        .unwrap();

                    let body = inner.find(match_tag("body")).map(parse_node).unwrap();

                    cond_expr::Arm { condition, body }
                })
                .collect();

            new(Ast::CondExpr(cond_expr::Expr { arms, else_arm }))
        }
        Rule::for_expr => {
            let mut inner = pair.into_inner();

            let ipk = next_pair!(inner, Rule::index_property_key);

            let body = inner.find(match_tag("body")).map(parse_node).unwrap();

            let mut inner = ipk.into_inner();

            let index = inner
                .next()
                .and_then(filter_rule(Rule::ident))
                .unwrap()
                .as_str()
                .to_string();

            let iterable = inner.find(match_tag("iterable")).map(parse_node).unwrap();

            new(Ast::MappedType(MappedType {
                index,
                iterable,
                body,
                remapped_as: None,
                readonly_mod: None,
                optional_mod: None,
            }))
        }
        Rule::let_expr => {
            let bindings: HashMap<_, _> = pair
                .clone()
                .into_inner()
                .filter(match_tag("binding"))
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

            let body = pair
                .clone()
                .into_inner()
                .find(match_tag("body"))
                .map(parse_node)
                .unwrap();

            new(Ast::LetExpr(let_expr::Expr { bindings, body }))
        }
        Rule::EOI => {
            parse_error!(pair, format!("Unexpected end of input"));
        }

        rule => {
            parse_error!(pair, format!("Unexpected rule: {:?}", rule));
        }
    }
}

fn parse_interface(pair: Pair) -> Node<Ast> {
    let inner = pair.clone().into_inner();

    let export = inner
        .peek()
        .map(|p| p.as_rule() == Rule::export)
        .unwrap_or(false);

    let name = inner
        .clone()
        .find(match_tag("name"))
        .unwrap()
        .as_str()
        .into();

    let body = inner.clone().find(match_tag("body")).unwrap();

    let body = parse_object_literal(body);

    let definition = body.properties;

    let params = parse_definition_options(inner);

    dbg!(&params);

    let extends = None;

    Node::from_pair(
        &pair,
        Ast::Interface(Interface {
            export,
            extends,
            name,
            params,
            definition,
        }),
    )
}

fn parse_definition_options(inner: pest::iterators::Pairs<Rule>) -> Vec<TypeParameter> {
    // Track the order of the inserted parametes
    let mut ordered_params: Vec<&str> = Default::default();

    let mut params: HashMap<&str, TypeParameter> = inner
        .clone()
        .find(match_tag("parameters"))
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

    if let Some(where_clause) = inner.clone().find(match_tag("where")) {
        where_clause
            .into_inner()
            .filter(match_tag("constraint"))
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

                param.constraint = Some(body);
            });
    }

    if let Some(defaults_clause) = inner.clone().find(match_tag("defaults")) {
        defaults_clause
            .into_inner()
            .filter(match_tag("default"))
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

                param.default = Some(body);
            });
    };

    let params = ordered_params
        .iter()
        .map(|name| params.get(name).unwrap().clone())
        .collect();
    params
}

fn pair_as_identifier(pair: Pair) -> Identifier {
    assert_ast!(pair, Rule::ident);
    Identifier(pair.as_str().to_string())
}

fn pair_as_string_literal(pair: Pair) -> String {
    assert_ast!(pair, Rule::string);

    match pair.clone().into_inner().next().unwrap().as_rule() {
        Rule::atom_string => pair.as_str().trim_start_matches(':').to_string(),
        Rule::double_quote_string => pair.as_str().trim_matches('"').to_string(),
        Rule::single_quote_string => pair.as_str().trim_matches('\'').to_string(),
        _ => unreachable!(),
    }
}

fn parse_string(pair: Pair) -> AstNode {
    Node::from_pair(&pair.clone(), Ast::String(pair_as_string_literal(pair)))
}

fn parse_import_statement(pair: Pair) -> AstNode {
    let mut inner = pair.clone().into_inner();

    let import_clause = inner.next().unwrap();

    let import_clause = match import_clause.as_rule() {
        Rule::named_import => {
            let specs = import_clause
                .into_inner()
                .find_tagged("import_specifier")
                .map(|pair| {
                    let mut inner = pair.into_inner();
                    let name = inner.next().unwrap();
                    let name = pair_as_identifier(name);
                    let alias = inner.next().map(pair_as_identifier);

                    ImportSpecifier {
                        module_export_name: name,
                        alias,
                    }
                })
                .collect_vec();

            ImportClause::Named(specs)
        }
        Rule::namespace_import => {
            todo!()
        }
        _ => parse_error!(pair),
    };

    let module = inner.next().map(pair_as_string_literal).unwrap();

    Node::from_pair(
        &pair,
        Ast::ImportStatement {
            import_clause,
            module,
        },
    )
}

fn parse_if_expr(pair: Pair) -> AstNode {
    let mut inner = pair.clone().into_inner();

    let condition = inner
        .find(match_tag("condition"))
        .map(|p| (parse_extends_expr(p.into_inner())))
        .unwrap();

    let then_branch = inner.find(match_tag("then")).map(parse_node).unwrap();

    let else_branch = inner
        .find(match_tag("else"))
        .map(parse_node)
        .unwrap_or_else(|| Ast::Never.into());

    let else_branch = else_branch;

    match &*condition.value {
        // Other conditions are desugared later in the simplification step
        Ast::ExtendsInfixOp { .. } | Ast::ExtendsPrefixOp { .. } => Node::from_pair(
            &pair,
            Ast::IfExpr(if_expr::Expr {
                condition,
                then_branch,
                else_branch: Some(else_branch),
            }),
        ),
        _ => unreachable!(),
    }
}

fn parse_tuple(pair: Pair) -> AstNode {
    let items = pair.clone().into_inner().map(parse_node).collect();

    Node::from_pair(&pair, Ast::Tuple(Tuple { items }))
}

fn parse_object_literal(pair: Pair) -> ObjectLiteral {
    let object_property_rules = pair.clone().into_inner();
    let mut properties = Vec::new();

    for prop_pair in object_property_rules {
        match prop_pair.as_rule() {
            Rule::object_property => {
                let mut inner = prop_pair.into_inner();

                let key = next_pair!(inner, Rule::property_key);

                let value = inner
                    .find(match_tag("value"))
                    .map(parse_node)
                    .expect("object property missing value");

                let mut inner = key.into_inner();

                let readonly = find_tag(inner.clone(), "readonly").is_some();
                let optional = find_tag(inner.clone(), "optional").is_some();

                let key = find_tag(inner.clone(), "key").unwrap();

                let key = parse_property_key_inner(key);

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

    return ObjectLiteral { properties };
}

fn parse_property_key_inner(key: Pair) -> ObjectPropertyKey {
    match key.as_rule() {
        Rule::ident => ObjectPropertyKey::Key(key.as_str().to_string()),
        Rule::index_property_key => parse_index_property_key(key),
        Rule::computed_property_key => {
            let inner = key.into_inner().next().unwrap();
            let id = pair_as_identifier(inner);
            ObjectPropertyKey::Computed(id)
        }
        _ => unreachable!(),
    }
}

fn parse_index_property_key(key: Pair) -> ObjectPropertyKey {
    let inner = key.into_inner();

    let [index, iterable, remap_clause] = take_tags!(inner, ["index", "iterable", "remap_clause"]);

    let key = index.unwrap().as_str().to_string();
    let iterable = parse_node(iterable.unwrap());
    let remapped_as = remap_clause.map(parse_node);

    ObjectPropertyKey::Index(PropertyKeyIndex {
        key,
        iterable,
        remapped_as,
    })
}

fn parse_type_alias(pair: Pair) -> AstNode {
    let inner = pair.clone().into_inner();

    let export = inner
        .peek()
        .map(|p| p.as_rule() == Rule::export)
        .unwrap_or(false);

    let name = inner
        .clone()
        .find(match_tag("name"))
        .unwrap()
        .as_str()
        .into();

    let body = inner
        .clone()
        .find(match_tag("body"))
        .map(parse_node)
        .unwrap();

    let params = parse_definition_options(inner);

    Node::from_pair(
        &pair,
        Ast::TypeAlias {
            export,
            name,
            params,
            body,
        },
    )
}

fn node_as_string(pair: Pair) -> String {
    return pair.as_str().to_string();
}

fn match_tag<'a>(tag: &'a str) -> impl FnMut(&Pair<'a>) -> bool {
    return |pair| pair.as_node_tag() == Some(tag);
}

fn filter_tag<'a>(tag: &'a str) -> impl FnMut(Pair<'a>) -> Option<Pair<'a>> {
    return move |pair| {
        if pair.as_node_tag() == Some(tag) {
            Some(pair)
        } else {
            None
        }
    };
}

fn match_rule<'a>(rule: Rule) -> impl Fn(&Pair<'a>) -> bool {
    return move |pair| pair.as_rule() == rule;
}

fn filter_rule<'a>(rule: Rule) -> impl Fn(Pair<'a>) -> Option<Pair<'a>> {
    return move |pair| {
        if pair.as_rule() == rule {
            Some(pair)
        } else {
            None
        }
    };
}

fn find_tag<'a>(mut pairs: Pairs<'a>, tag: &'a str) -> Option<Pair<'a>> {
    return pairs.find(|p| p.as_node_tag() == Some(tag));
}

// Generate tests for all test cases in tests/pest/foo/ and all subdirectories. Since
// `lazy_static = true`, a single `PestTester` is created and used by all tests; otherwise a new
// `PestTester` would be created for each test.
#[cfg(test)]
mod parser_tests {
    use super::*;
    use crate::test_support::*;
    use crate::typescript::Pretty;
    use pest::consumes_to;
    use pest::fails_with;
    use pest::parses_to;
    use pretty_assertions::{assert_eq, assert_ne};
    use std::assert_matches::assert_matches;
    use textwrap_macros::dedent;
    use Rule::*;

    #[test]
    fn parses_to_ident() {
        parses_to! {
            parser: NewtypeParser,
            input: "x",
            rule: Rule::ident,
            tokens: [ident(0, 1)]
        };
    }

    #[test]
    fn fails_with_else() {
        fails_with! {
            parser: NewtypeParser,
            input: "else",
            rule: Rule::ident,
            positives: [ident],
            negatives: [],
            pos: 0
        };
    }

    #[test]
    fn extends_expr_parser_extends() {
        assert_sexpr!(
            Rule::extends_expr,
            crate::parser::parse_extends_expr,
            "A <: B",
            "(<: A B)"
        );
    }

    #[test]
    fn extends_expr_parser_extends_parens() {
        assert_sexpr!(
            Rule::extends_expr,
            crate::parser::parse_extends_expr,
            "(A <: B)",
            "(<: A B)"
        );
    }

    #[test]
    fn extends_expr_parser_extends_multiple_parens() {
        assert_sexpr!(
            Rule::extends_expr,
            crate::parser::parse_extends_expr,
            "((A <: B))",
            "(<: A B)"
        );
    }

    #[test]
    fn extends_expr_parser_not_with_parens_extends() {
        assert_sexpr!(
            Rule::extends_expr,
            crate::parser::parse_extends_expr,
            "not (A <: B)",
            "(not (<: A B))"
        );
    }

    #[test]
    fn extends_expr_parser_and() {
        assert_sexpr!(
            Rule::extends_expr,
            crate::parser::parse_extends_expr,
            "A <: B and C <: D",
            "(and (<: A B) (<: C D))"
        );
    }

    #[test]
    fn extends_expr_parser_not_and() {
        assert_sexpr!(
            Rule::extends_expr,
            crate::parser::parse_extends_expr,
            "not (A <: B) and C <: D",
            "(and (not (<: A B)) (<: C D))"
        );

        assert_sexpr!(
            Rule::extends_expr,
            crate::parser::parse_extends_expr,
            "A <: B and (not (C <: D))",
            "(and (<: A B) (not (<: C D)))"
        );

        assert_sexpr!(
            Rule::extends_expr,
            crate::parser::parse_extends_expr,
            "not (A <: B) and (not (C <: D))",
            "(and (not (<: A B)) (not (<: C D)))"
        );
    }

    #[test]
    fn import_statement_named_imports() {
        assert_typescript!(
            r#"import type { A, B, C, D as D1 } from 'a';"#,
            r#"import { A, B, C, D as D1 } from :a"#
        );
    }

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
        assert_typescript!(expr, r#"A['x']"#, r#"A.x"#);
    }

    fn chained_dot_access() {
        assert_typescript!(expr, r#"A['x']['y']['z']"#, r#"A.x.y.z"#);
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

    mod if_expr {
        const R: Rule = object_literal;
        use super::*;

        #[test]
        fn simple_if_else() {
            assert_typescript!(
                if_expr,
                r#"
                1 extends number
                    ? 1
                    : 0
                "#,
                r#"
                if 1 <: number then
                    1
                else
                    0
                end
                "#
            );
        }

        #[test]
        fn if_without_else() {
            assert_typescript!(
                if_expr,
                r#"
                1 extends number
                    ? 1
                    : never
                "#,
                r#"
                if 1 <: number then
                    1
                end
                "#
            );
        }

        #[test]
        fn nested_if() {
            assert_typescript!(
                if_expr,
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
        }

        #[test]
        fn nested_if_else() {
            assert_typescript!(
                if_expr,
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
                if_expr,
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
                if_expr,
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
                if_expr,
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
        fn not_extends() {
            assert_typescript!(
                if_expr,
                r#"
                a extends b
                    ? never
                    : c
                "#,
                r#"
                if not (a <: b) then
                    c
                end
                "#
            );
        }

        #[test]
        fn not_extends_else() {
            assert_typescript!(
                if_expr,
                r#"
                a extends b
                    ? d
                    : c
                "#,
                r#"
                if not (a <: b) then
                    c
                else
                    d
                end
                "#
            );
        }

        #[test]
        fn not_extends_nested() {
            assert_typescript!(
                if_expr,
                r#"
                a extends b
                    ? c extends d
                        ? never
                        : x
                    : never
                "#,
                r#"
                if a <: b then
                    if not (c <: d) then
                        x
                    end
                end
                "#
            );

            assert_typescript!(
                if_expr,
                r#"
                a extends b
                    ? c extends d
                        ? y
                        : x
                    : never
                "#,
                r#"
                if a <: b then
                    if not (c <: d) then
                        x
                    else
                        y
                    end
                end
                "#
            );

            assert_typescript!(
                if_expr,
                r#"
                a extends b
                    ? never
                    : c extends d
                        ? x
                        : never
                "#,
                r#"
                if not (a <: b) then
                    if c <: d then
                        x
                    end
                end
                "#
            );

            assert_typescript!(
                if_expr,
                r#"
                a extends b
                    ? never
                    : c extends d
                        ? never
                        : x
                "#,
                r#"
                if not (a <: b) then
                    if not (c <: d) then
                        x
                    end
                end
                "#
            );
        }

        #[test]
        fn and_not() {
            assert_typescript!(
                if_expr,
                r#"
                a extends b
                    ? never
                    : c extends d
                        ? x
                        : never
                "#,
                r#"
                if not (a <: b) and c <: d then
                    x
                end
                "#
            );

            assert_typescript!(
                if_expr,
                r#"
                a extends b
                    ? c extends d
                        ? never
                        : x
                    : never
                "#,
                r#"
                if a <: b and (not (c <: d)) then
                    x
                end
                "#
            );
        }

        #[test]
        fn and() {
            assert_typescript!(
                "type A = a extends b ? c extends d ? e : f : f;",
                "type A as if a <: b and c <: d then e else f end"
            );
        }

        #[test]
        fn joined_conditions() {
            assert_typescript!(
                "type A = 1 extends number ? 1 : 0;",
                r#"type A as if 1 <: number then 1 else 0 end"#
            );
        }

        #[test]
        fn keyof() {
            assert_typescript!(
                if_expr,
                r#"
                x extends keyof y
                    ? 1
                    : never
                "#,
                "if x <: keyof(y) then 1 end"
            );
        }
    }

    #[test]
    fn for_in() {
        assert_typescript!(
            for_expr,
            r#"
            { [k in t]: 1 }
            "#,
            r#"
            for k in t do 1 end
            "#
        );
    }

    mod object_literal {
        const R: Rule = object_literal;
        use super::*;

        #[test]
        fn index() {
            assert_typescript!(R, "{[k in K]: value}", "{ [k in K]: value }");
        }

        #[test]
        fn readonly_modifier() {
            assert_typescript!(R, "{readonly x: 1}", "{ readonly x: 1 }");
        }

        #[test]
        fn optional_modifier() {
            assert_typescript!(R, "{x?: 1}", "{ ?x: 1 }");
        }

        #[test]
        fn empty() {
            assert_typescript!(R, "{}", "{}");
        }

        #[test]
        fn one_key() {
            assert_typescript!(R, "{x: 1}", "{x: 1}");
        }

        #[test]
        fn many_keys() {
            assert_typescript!(R, "{x: 1, y: 2, z: 3}", "{x: 1, y: 2, z: 3}");
        }

        #[test]
        fn computed_property() {
            assert_typescript!(R, "{[K]: T}", "{[K]: T}");
        }
    }

    mod tuple {
        const R: Rule = Rule::tuple;
        use super::*;

        #[test]
        fn empty() {
            assert_typescript!(R, "[]", "[]");
        }

        #[test]
        fn single_element() {
            assert_typescript!(R, "[1]", "[1]");
        }

        #[test]
        fn multiple_elements() {
            assert_typescript!(R, "[1, 2, 3]", "[1, 2, 3]");
        }

        #[test]
        fn single_string_element() {
            assert_typescript!(R, r#"['sup']"#, r#"[:sup]"#);
        }
    }

    mod array {
        const R: Rule = Rule::expr;
        use super::*;

        #[test]
        fn two_d() {
            assert_typescript!(R, "number[]", "number[]");
        }
        #[test]
        fn four_d() {
            assert_typescript!(R, "number[][][]", "number[][][]");
        }

        #[test]
        fn of_numbers_with_parentheses() {
            assert_typescript!(R, "number[]", "(number)[]");
        }

        #[test]
        fn of_union_types() {
            assert_typescript!(R, "(number | string)[]", "(number | string)[]");
        }
    }

    mod type_alias {
        const R: Rule = Rule::type_alias;
        use super::*;

        #[test]
        fn generics_one_argument() {
            assert_typescript!(R, "type A<x> = x", "type A(x) as x");
        }

        #[test]
        fn generics_many_arguments() {
            assert_typescript!(R, "type A<x, y, z> = 1", "type A(x, y, z) as 1");
        }

        #[test]
        fn generic_with_guard() {
            assert_typescript!(R, "type A<x, y, z> = 1", "type A(x, y, z) as 1");
        }

        #[test]
        fn where_clause() {
            assert_typescript!(
                R,
                r#"type A<x extends number> = x"#,
                r#"type A(x) where x <: number as x"#
            );
        }

        #[test]
        fn defaults_clause() {
            assert_typescript!(
                R,
                r#"type A<x = number> = x"#,
                r#"type A(x) defaults x = number as x"#
            );
        }

        #[test]
        fn where_and_defaults_clause() {
            assert_typescript!(
                R,
                r#"type A<x extends number = number> = x"#,
                r#"type A(x) defaults x = number where x <: number as x"#
            );
        }

        #[test]
        fn exported() {
            assert_typescript!(R, "export type A = 1", "export type A as 1");
        }
    }

    mod interface {
        const R: Rule = Rule::interface;
        use super::*;

        #[test]
        fn interface() {
            assert_typescript!(
                R,
                r#"
                interface Foo {}
                "#,
                r#"
                interface Foo {}
                "#
            );
        }

        #[test]
        fn params() {
            assert_typescript!(
                R,
                r#"
                interface Foo<A> {}
                "#,
                r#"
                interface Foo(A) {}
                "#
            );
        }

        #[test]
        fn params_defaults() {
            assert_typescript!(
                R,
                r#"
                interface Foo<A = any> {}
                "#,
                r#"
                interface Foo(A)
                defaults A = any
                {}
                "#
            );
        }

        #[test]
        fn readonly_property() {
            assert_typescript!(
                R,
                r#"
                interface Foo {
                    readonly x: number;
                }
                "#,
                r#"
                interface Foo {
                    readonly x: number,
                }
                "#
            );
        }

        #[test]
        fn optional_property() {
            assert_typescript!(
                R,
                r#"
                interface Foo {
                    x?: number;
                }
                "#,
                r#"
                interface Foo {
                    ?x: number,
                }
                "#
            );
        }

        #[test]
        fn index_property() {
            assert_typescript!(
                R,
                r#"
                interface Foo {
                    [K in T]: number;
                }
                "#,
                r#"
                interface Foo {
                    [K in T]: number,
                }
                "#
            );
        }

        #[test]
        fn readonly_index_property() {
            assert_typescript!(
                R,
                r#"
                interface Foo {
                    readonly [K in T]: number;
                }
                "#,
                r#"
                interface Foo {
                    readonly [K in T]: number,
                }
                "#
            );
        }

        #[test]
        fn optional_index_property() {
            assert_typescript!(
                R,
                r#"
                interface Foo {
                    [K in T]?: number;
                }
                "#,
                r#"
                interface Foo {
                    ?[K in T]: number,
                }
                "#
            );
        }

        #[test]
        fn readonly_optional_index_property() {
            assert_typescript!(
                R,
                r#"
                interface Foo {
                    readonly [K in T]?: number;
                }
                "#,
                r#"
                interface Foo {
                    readonly ?[K in T]: number,
                }
                "#
            );
        }
    }

    mod application {
        const R: Rule = Rule::expr;
        use super::*;

        #[test]
        fn single_type_argument() {
            assert_typescript!(R, "A<1>", "A(1)");
        }

        #[test]
        fn multiple_type_arguments() {
            assert_typescript!(R, "A<1, 2, 3>", "A(1, 2, 3)");
        }

        #[test]
        fn type_argument_with_array() {
            assert_typescript!(R, "A<1, []>", "A(1, [])");
        }

        #[test]
        fn multiple_array_type_arguments() {
            assert_typescript!(R, "A<[], [], []>", "A([], [], [])");
        }

        #[test]
        fn nested_type_argument() {
            assert_typescript!(R, "A<B<1>>", "A(B(1))");
        }

        #[test]
        fn mixed_type_arguments() {
            assert_typescript!(R, "A<B, 1>", "A(B, 1)");
        }
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
    fn let_expr_shadowed() {
        assert_typescript!(
            r#"
            type A = [1, 2, 3];
            "#,
            r#"
            type A as
                let a = 1 in
                let b = a in
                let a = 2 in
                [b, a, let a = 3 in a]
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
                    ? number extends A['length']
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
    #[ignore]
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

    #[test]
    fn match_ts() {
        assert_typescript!(
            r#"
            import type { A, B, I, M, Union } from 'ts-toolbelt';

            import type { __ } from '.';

            import type {
                __capture__,
                __kind__,
                any_,
                bigint_,
                boolean_,
                function_,
                number_,
                object_,
                rest_,
                string_,
                symbol_
            } from './const';

            type True = 1;

            type ExtractSubcapture<T> =
                T extends M.Primitive | M.BuiltIn
                    ? T extends object
                        ? T[Exclude<keyof T, keyof [] | keyof {}>]
                        : never
                    : never;

            type PartialAssignment<K, V> =
                V extends never
                    ? never
                    : K extends string
                        ? { [k in K]: v }
                        : never;

            type EmptyToNever<T> = {} extends T ? never : T;

            export interface Hole<Type = any, Label = any> {
                T: Type;
                readonly [__kind__]: Label;
            };
            "#,
            r#"
            import { A, B, I, M, Union } from :ts-toolbelt

            import { __ } from :.

            import {
              __capture__,
              __kind__,
              any_,
              bigint_,
              boolean_,
              function_,
              number_,
              object_,
              rest_,
              string_,
              symbol_,
            } from :./const

            type True as 1

            type ExtractSubcapture(T) as
                if T <: M::Primitive | M::BuiltIn and T <: object then
                    T[ Exclude(keyof(T), keyof([]) | keyof({})) ]
                end

            type PartialAssignment(K, V) as
                if not (V <: never) and K <: string then
                    for k in K do
                        v
                    end
                end


            type EmptyToNever(T) as
                if {} </: T then
                    T
                end

            export interface Hole(Type, Label)
            defaults
                Type = any,
                Label = any,
            {
                T: Type,
                readonly [__kind__]: Label,
            }

            "#
        );
    }
}
