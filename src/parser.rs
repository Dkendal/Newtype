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

use cond_expr::CondExpr;
use if_expr::IfExpr;
use itertools::Itertools;

use let_expr::LetExpr;
use match_expr::MatchExpr;
use node::Node;
use pest::{
    error::{Error, ErrorVariant},
    pratt_parser::PrattParser,
    Parser, Span,
};

use pratt::{EXPR_PARSER, EXTENDS_PARSER};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct NewtypeParser;

pub type ParserError = Error<Rule>;

pub type Pair<'i> = pest::iterators::Pair<'i, Rule>;
pub type Pairs<'i> = pest::iterators::Pairs<'i, Rule>;

pub(crate) fn parse_expr(pairs: Pairs) -> Node {
    use Rule::*;
    EXPR_PARSER
        .map_primary(parse)
        .map_prefix(|op, child| match op.as_rule() {
            infer => Node::from_pair(&op, Ast::Infer(child)),
            rule => {
                parse_error!(op, vec![infer], vec![rule]);
            }
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            indexed_access => {
                let span = Span::new(
                    op.as_span().get_input(),
                    lhs.as_span().start(),
                    op.as_span().end(),
                )
                .unwrap();

                let inner = op.into_inner().next().unwrap();
                let index = parse(inner);

                Node::new(
                    span,
                    Ast::Access(Access {
                        lhs,
                        rhs: index,
                        is_dot: false,
                        span,
                    }),
                )
            }
            array_modifier => Node::from_pair(&op, Ast::Array(lhs)),
            application => {
                let span = Span::new(
                    op.as_span().get_input(),
                    lhs.as_span().start(),
                    op.as_span().end(),
                )
                .unwrap();

                let args = next_pair!(op.clone().into_inner(), Rule::argument_list);

                Node::from_pair(
                    &op,
                    Ast::ApplyGeneric(ApplyGeneric {
                        span,
                        receiver: lhs,
                        args: args.into_inner().map(parse).collect(),
                    }),
                )
            }
            rule => {
                parse_error!(op, vec![indexed_access, array_modifier], vec![rule]);
            }
        })
        .map_infix(|lhs, op, rhs| {
            let span = Span::new(
                op.as_span().get_input(),
                lhs.as_span().start(),
                rhs.as_span().end(),
            )
            .unwrap();

            if op.as_rule() == pipe {
                return replace_pipe_with_type_application(rhs, lhs, op);
            }

            let ast = match op.as_rule() {
                union => Ast::UnionType(UnionType {
                    types: vec![lhs, rhs],
                    span,
                }),

                intersection => Ast::IntersectionType(IntersectionType {
                    types: vec![lhs, rhs],
                    span,
                }),

                colon2 => {
                    let mut acc = vec![];

                    match lhs.value.as_ref() {
                        Ast::Path(Path { segments, .. }) => acc.extend(segments.to_owned()),
                        Ast::Ident(_) => acc.push(lhs),
                        _ => parse_error!(
                            lhs,
                            format!(
                                "Expected path or identifier, found {:?}",
                                lhs.value.as_ref()
                            )
                        ),
                    };

                    match rhs.value.as_ref() {
                        Ast::Path(Path { segments, .. }) => acc.extend(segments.to_owned()),
                        Ast::Ident(_) => acc.push(rhs),
                        _ => parse_error!(
                            rhs,
                            format!(
                                "Expected path or identifier, found {:?}",
                                rhs.value.as_ref()
                            )
                        ),
                    };

                    Ast::Path(Path {
                        span,
                        segments: acc,
                    })
                }

                dot_op => Ast::Access(Access {
                    lhs,
                    rhs,
                    is_dot: true,
                    span,
                }),

                rule => unreachable!("Expected infix operator, found {:?}", rule),
            };

            Node::new(span, ast)
        })
        .parse(pairs)
}

/// Replace the pipe operator macro with a type application
/// ```
/// A |> B
/// # B(A)
/// ```
fn replace_pipe_with_type_application<'a>(rhs: Node<'a>, lhs: Node<'a>, op: Pair<'a>) -> Node<'a> {
    let span = Span::new(
        op.as_span().get_input(),
        lhs.as_span().start(),
        rhs.as_span().end(),
    )
    .unwrap();

    match &*rhs.value {
        Ast::Ident(_) => {
            // FIXME missing span
            Node::from_pair(
                &op,
                Ast::ApplyGeneric(ApplyGeneric {
                    span,
                    receiver: rhs.clone(),
                    args: vec![lhs],
                }),
            )
        }
        Ast::ApplyGeneric(ApplyGeneric {
            receiver: name,
            args: params,
            ..
        }) => {
            let mut params = params.clone();
            params.insert(0, lhs);
            // FIXME missing span
            Node::from_pair(
                &op,
                Ast::ApplyGeneric(ApplyGeneric {
                    span,
                    receiver: name.clone(),
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

pub(crate) fn parse_newtype_program(source: &str) -> Result<Node<'_>, Box<Error<Rule>>> {
    let pair = NewtypeParser::parse(Rule::program, source)?.next().unwrap();

    Ok(parse(pair))
}

pub(crate) fn parse_extends_expr(pairs: Pairs) -> Node {
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
            let span = Span::new(
                op.as_span().get_input(),
                op.as_span().start(),
                primary_node.span.end(),
            ).unwrap();

            if op.as_rule() == Rule::not && !primary_node.value.is_extends_infix_op() {
                let error_not = Error::<Rule>::new_from_span(
                    ErrorVariant::CustomError {
                        message: "`not` may only be used with an extends expression".to_string(),
                    },
                    op.as_span(),
                );

                    // FIXME need rule for the primary node for better reporting
                    let error = Error::<Rule>::new_from_span(
                        ErrorVariant::ParsingError {
                            positives: vec![Rule::extends_expr],
                            negatives: vec![],
                        },
                        span,
                    );

                let error_expr = format!("{error}");

                panic!("{error_not}\n{error_expr}\nHint: You might have forgotten to wrap the expression in parentheses, `not` has higher precedence than other operators.");
            }

            let op = match op.as_rule() {
                Rule::not => PrefixOp::Not,
                rule => parse_error!(op, vec![Rule::not], vec![rule]),
            };

           Node::new(
                span,
                Ast::ExtendsPrefixOp(ExtendsPrefixOp {
                    op,
                    value: primary_node,
                    span
                }),
            )
        })
        .map_infix(|lhs: Node, op: Pair, rhs: Node| {
            let span = Span::new(
                lhs.span.get_input(),
                lhs.span.start(),
                rhs.span.end(),
            ).unwrap();

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

            let ast = Ast::ExtendsInfixOp(ExtendsInfixOp { lhs, op, rhs, span });

            Node::new(span, ast)
        })
        .parse(pairs)
}

pub(crate) fn parse(pair: Pair) -> Node {
    let rule = pair.clone().as_rule();
    let span = pair.clone().as_span();
    let new = |ast| Node::from_span(span, ast);

    // TODO: just return AST, remove calls to new/1 wrap result
    match rule {
        Rule::program => new(parse_program(pair)),
        Rule::statement => new(parse_statement(pair)),
        Rule::type_alias => new(parse_type_alias(pair)),
        Rule::unittest => new(Ast::UnitTest(parse_unittest(pair))),
        Rule::interface => parse_interface(pair),
        Rule::import_statement => parse_import_statement(pair),
        Rule::if_expr => parse_if_expr(pair),
        Rule::object_literal => new(Ast::TypeLiteral(parse_object_literal(pair))),
        Rule::primitive => {
            let value = pair.into_inner().next().unwrap();

            let primitive = match value.as_rule() {
                Rule::primitive_string => PrimitiveType::String,
                Rule::primitive_number => PrimitiveType::Number,
                Rule::primitive_boolean => PrimitiveType::Boolean,
                Rule::primitive_bigint => PrimitiveType::BigInt,
                Rule::primitive_symbol => PrimitiveType::Symbol,
                Rule::primitive_object => PrimitiveType::Object,
                Rule::primitive_null => PrimitiveType::Null,
                Rule::primitive_void => PrimitiveType::Void,
                Rule::primitive_undefined => PrimitiveType::Undefined,
                _ => unimplemented!("{:?}", rule),
            };
            new(Ast::Primitive(primitive))
        }
        Rule::number => new(Ast::Number(node_as_string(pair))),
        Rule::string => parse_string(pair),
        Rule::template_string => new(Ast::TemplateString(node_as_string(pair))),
        Rule::ident => new(Ast::Ident(parse_ident(pair))),
        Rule::never => new(Ast::NeverKeyword(span)),
        Rule::any => new(Ast::AnyKeyword(span)),
        Rule::unknown => new(Ast::UnknownKeyword(span)),
        Rule::boolean => {
            let value = pair.into_inner().next().unwrap();

            match value.as_rule() {
                Rule::literal_true => new(Ast::TrueKeyword(span)),
                Rule::literal_false => new(Ast::FalseKeyword(span)),
                _ => unreachable!(),
            }
        }
        Rule::tuple => parse_tuple(pair),
        Rule::macro_call => new(Ast::MacroCall(parse_macro_call(pair))),
        Rule::builtin => new(parse_builtin(pair)),
        Rule::expr => parse_expr(pair.into_inner()),
        Rule::match_expr => new(Ast::MatchExpr(parse_match_expr(pair))),
        Rule::cond_expr => new(Ast::CondExpr(parse_cond_expr(pair))),
        Rule::map_expr => new(Ast::MappedType(parse_map_expr(pair))),
        Rule::let_expr => new(Ast::LetExpr(parse_let_expr(pair))),
        Rule::function_type => new(Ast::FunctionType(parse_function_type(pair))),

        Rule::EOI => {
            parse_error!(pair, format!("Unexpected end of input"));
        }

        rule => {
            parse_error!(pair, format!("Unexpected rule: {:?}", rule));
        }
    }
}

fn parse_builtin(pair: Pair) -> Ast {
    let span = pair.as_span();
    let mut inner = pair.into_inner();

    let name = inner.find(match_tag("name")).unwrap();

    let name = match name.as_rule() {
        Rule::keyof => BuiltinKeyword::Keyof,
        _ => unreachable!(
            "unexpected rule while parsing builtin: {:?}",
            name.as_rule()
        ),
    };

    let argument = inner.find(match_tag("argument")).map(parse).unwrap();

    Ast::Builtin(Builtin {
        name,
        argument,
        span,
    })
}

fn parse_match_expr(pair: Pair) -> MatchExpr {
    use match_expr::Arm;

    let span = pair.as_span();

    let mut inner = pair.into_inner();

    let value = inner.find(match_tag("value")).map(parse).unwrap();

    let arms: Vec<match_expr::Arm> = inner
        .clone()
        .filter(match_tag("arm"))
        .map(|pair| {
            let span = pair.as_span();
            let mut inner = pair.into_inner();
            let pattern = inner.find(match_tag("pattern")).map(parse).unwrap();
            let body = inner.find(match_tag("body")).map(parse).unwrap();

            Arm {
                span,
                pattern,
                body,
            }
        })
        .collect();

    let else_arm = inner
        .find(match_tag("else"))
        .and_then(|p| p.into_inner().find(match_tag("body")))
        .map(parse)
        .unwrap_or_else(|| Node::new(span, Ast::NeverKeyword(span)));

    MatchExpr {
        span,
        value,
        arms,
        else_arm,
    }
}

fn parse_cond_expr(pair: Pair) -> CondExpr {
    let span = pair.as_span();
    let inner = pair.into_inner();

    let else_arm = inner
        .clone()
        .find(match_tag("else"))
        .and_then(|p| p.into_inner().find(match_tag("body")))
        .map(parse)
        .unwrap_or_else(|| Node::new(span, Ast::NeverKeyword(span)));

    let arms: Vec<cond_expr::Arm> = inner
        .clone()
        .filter(match_tag("arm"))
        .map(|arm| {
            use cond_expr::Arm;
            let span = arm.as_span();
            let mut inner = arm.into_inner();

            let condition = inner
                .find(match_tag("condition"))
                .map(|p| p.into_inner())
                .map(parse_extends_expr)
                .unwrap();

            let body = inner.find(match_tag("body")).map(parse).unwrap();

            Arm {
                span,
                condition,
                body,
            }
        })
        .collect();

    CondExpr {
        span,
        arms,
        else_arm,
    }
}

fn parse_map_expr(pair: Pair) -> MappedType {
    let span = pair.as_span();

    let mut inner = pair.into_inner();

    let ipk = next_pair!(inner, Rule::index_property_key);

    let body = inner.find(match_tag("body")).map(parse).unwrap();

    let mut inner = ipk.into_inner();

    let index = inner
        .next()
        .and_then(filter_rule(Rule::ident))
        .unwrap()
        .as_str()
        .to_string();

    let iterable = inner.find(match_tag("iterable")).map(parse).unwrap();

    MappedType {
        span,
        index,
        iterable,
        body,
        remapped_as: None,
        readonly_mod: None,
        optional_mod: None,
    }
}

fn parse_let_expr(pair: Pair) -> LetExpr {
    let span = pair.as_span();

    let bindings: HashMap<_, _> = pair
        .clone()
        .into_inner()
        .filter(match_tag("binding"))
        .map(|pair| {
            let span = pair.as_span();
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap();
            assert_eq!(name.as_rule(), Rule::ident);

            let name = name.as_str().to_string();
            let value = inner.next().unwrap();
            assert_eq!(value.as_rule(), Rule::expr);
            let value = parse(value);

            (Ident { name, span }, value)
        })
        .collect();

    let body = pair
        .clone()
        .into_inner()
        .find(match_tag("body"))
        .map(parse)
        .unwrap();

    LetExpr {
        span,
        bindings,
        body,
    }
}

fn parse_function_type(pair: Pair) -> FunctionType {
    let span = pair.as_span();

    let mut inner = pair.into_inner();

    let next = next_pair!(inner, Rule::parameters);

    let next = next.into_inner().next();

    let params = match next.as_ref().map(|p| p.as_rule()) {
        Some(Rule::unnamed_parameters) => next
            .unwrap()
            .into_inner()
            .enumerate()
            .map(|(idx, pair)| {
                assert_eq!(pair.as_rule(), Rule::unnamed_parameter);

                let binding = pair
                    .clone()
                    .into_inner()
                    .map(|p| (p.as_rule(), p))
                    .collect_vec();

                let (ellipsis, name, kind) = match binding.as_slice() {
                    [(Rule::ellipsis_token, _), (_, kind)] => (true, "rest".to_string(), kind),
                    [(_, kind)] => (false, format!("arg{}", idx), kind),
                    _ => unreachable!(),
                };

                Parameter {
                    span: pair.as_span(),
                    ellipsis,
                    name,
                    kind: parse(kind.to_owned()),
                }
            })
            .collect_vec(),

        Some(Rule::named_parameters) => next
            .unwrap()
            .into_inner()
            .map(|pair| {
                assert_eq!(pair.as_rule(), Rule::named_parameter);

                let binding = pair
                    .clone()
                    .into_inner()
                    .map(|p| (p.as_rule(), p))
                    .collect_vec();

                let (ellipsis, name, kind) = match binding.as_slice() {
                    [(Rule::ellipsis_token, _), (_, name), (_, kind)] => (true, name, kind),
                    [(_, name), (_, kind)] => (false, name, kind),
                    _ => unreachable!(),
                };

                Parameter {
                    span: pair.as_span(),
                    ellipsis,
                    name: name.as_str().to_string(),
                    kind: parse(kind.to_owned()),
                }
            })
            .collect_vec(),

        Some(_) => unreachable!(),

        None => vec![],
    };

    let return_type = next_pair!(inner, Rule::expr);

    let return_type = parse(return_type);

    FunctionType {
        span,
        params,
        return_type,
    }
}

fn parse_macro_call(pair: Pair) -> MacroCall {
    let span = pair.as_span();
    let mut inner = pair.into_inner();
    let name = next_pair!(inner, Rule::macro_ident);
    let args = next_pair!(inner, Rule::argument_list);
    let name = name.as_str().to_string();
    let inner = args.into_inner();
    let args = inner.map(parse).collect_vec();
    MacroCall { span, name, args }
}

fn parse_unittest(pair: Pair) -> UnitTest {
    let span = pair.as_span();
    let mut inner = pair.into_inner();
    let name = next_pair!(inner, Rule::string);
    let name = name.as_str().to_string();
    let body = inner.map(parse).collect_vec();

    UnitTest { span, name, body }
}

fn parse_type_alias(pair: Pair) -> Ast {
    let span = pair.as_span();
    let inner = pair.clone().into_inner();

    let export = inner
        .peek()
        .map(|p| p.as_rule() == Rule::export)
        .unwrap_or(false);

    let name = inner.clone().find(match_tag("name")).unwrap().as_str();

    let name = Ident {
        name: name.to_string(),
        span,
    };

    let body = inner.clone().find(match_tag("body")).map(parse).unwrap();

    let params = parse_definition_options(inner);

    Ast::TypeAlias(TypeAlias {
        export,
        name,
        params,
        body,
        span,
    })
}

fn parse_statement(pair: Pair) -> Ast {
    let inner = pair.into_inner().next().unwrap();
    let inner = parse(inner);
    Ast::Statement(inner)
}

fn parse_program(pair: Pair) -> Ast {
    let children: Vec<_> = pair
        .clone()
        .into_inner()
        .filter(|pair| pair.as_rule() != Rule::EOI) // Remove the end of input token
        .map(parse)
        .collect();

    Ast::Program(children)
}

fn parse_interface(pair: Pair) -> Node {
    let span = pair.as_span();
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

    let extends = None;

    Node::from_pair(
        &pair,
        Ast::Interface(Interface {
            span,
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
                .map(|pair| {
                    let span = pair.as_span();
                    let str = pair.as_str();
                    let name = str.to_string();

                    ordered_params.push(str);

                    (
                        str,
                        TypeParameter {
                            span,
                            name,
                            constraint: None,
                            default: None,
                            rest: false,
                        },
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
                let body = parse(body);

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
                let body = parse(body);

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

fn parse_ident(pair: Pair) -> Ident {
    assert_ast!(pair, Rule::ident);
    Ident {
        name: pair.as_str().to_string(),
        span: pair.as_span(),
    }
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

fn parse_string(pair: Pair) -> Node {
    Node::from_pair(&pair.clone(), Ast::String(pair_as_string_literal(pair)))
}

fn parse_import_statement(pair: Pair) -> Node {
    let span = pair.as_span();
    let mut inner = pair.clone().into_inner();

    let import_clause = inner.next().unwrap();

    let import_clause = match import_clause.as_rule() {
        Rule::named_import => {
            let specs = import_clause
                .into_inner()
                .find_tagged("import_specifier")
                .map(|pair| {
                    let span = pair.as_span();
                    let mut inner = pair.into_inner();
                    let name = inner.next().unwrap();
                    let name = parse_ident(name);
                    let alias = inner.next().map(parse_ident);

                    ImportSpecifier {
                        span,
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
        Ast::ImportStatement(ImportStatement {
            import_clause,
            module,
            span,
        }),
    )
}

fn parse_if_expr(pair: Pair) -> Node {
    let span = pair.as_span();
    let mut inner = pair.clone().into_inner();

    let condition = inner
        .find(match_tag("condition"))
        .map(|p| (parse_extends_expr(p.into_inner())))
        .unwrap();

    let then_branch = inner.find(match_tag("then")).map(parse).unwrap();

    let else_branch = inner
        .find(match_tag("else"))
        .map(parse)
        .unwrap_or_else(|| Node::new(span, Ast::NeverKeyword(span)));

    let else_branch = else_branch;

    match &*condition.value {
        // Other conditions are desugared later in the simplification step
        Ast::ExtendsInfixOp(ExtendsInfixOp { .. })
        | Ast::ExtendsPrefixOp(ExtendsPrefixOp { .. }) => Node::from_pair(
            &pair,
            Ast::IfExpr(IfExpr {
                span: pair.as_span(),
                condition,
                then_branch,
                else_branch: Some(else_branch),
            }),
        ),
        _ => unreachable!(),
    }
}

fn parse_tuple(pair: Pair) -> Node {
    let items = pair.clone().into_inner().map(parse).collect();

    Node::from_pair(
        &pair,
        Ast::Tuple(Tuple {
            span: pair.as_span(),
            items,
        }),
    )
}

fn parse_object_literal(pair: Pair) -> ObjectLiteral {
    let span = pair.as_span();
    let object_property_rules = pair.clone().into_inner();
    let mut properties = Vec::new();

    for prop_pair in object_property_rules {
        let span = prop_pair.as_span();

        match prop_pair.as_rule() {
            Rule::object_property => {
                let mut inner = prop_pair.into_inner();

                let key = next_pair!(inner, Rule::property_key);

                let value = inner
                    .find(match_tag("value"))
                    .map(parse)
                    .expect("object property missing value");

                let inner = key.into_inner();

                let readonly = find_tag(inner.clone(), "readonly").is_some();
                let optional = find_tag(inner.clone(), "optional").is_some();

                let key = find_tag(inner.clone(), "key").unwrap();

                let key = parse_property_key_inner(key);

                properties.push(ObjectProperty {
                    span,
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

    ObjectLiteral { properties, span }
}

fn parse_property_key_inner(key: Pair) -> ObjectPropertyKey {
    match key.as_rule() {
        Rule::ident => ObjectPropertyKey::Key(key.as_str().to_string()),
        Rule::index_property_key => parse_index_property_key(key),
        Rule::computed_property_key => {
            let inner = key.into_inner().next().unwrap();
            let id = parse_ident(inner);
            ObjectPropertyKey::Computed(id)
        }
        _ => unreachable!(),
    }
}

fn parse_index_property_key(key: Pair) -> ObjectPropertyKey {
    let span = key.as_span();
    let inner = key.into_inner();

    let [index, iterable, remap_clause] = take_tags!(inner, ["index", "iterable", "remap_clause"]);

    let key = index.unwrap().as_str().to_string();
    let iterable = parse(iterable.unwrap());
    let remapped_as = remap_clause.map(parse);

    ObjectPropertyKey::Index(PropertyKeyIndex {
        span,
        key,
        iterable,
        remapped_as,
    })
}

fn node_as_string(pair: Pair) -> String {
    pair.as_str().to_string()
}

fn match_tag<'a>(tag: &'a str) -> impl FnMut(&Pair<'a>) -> bool {
    |pair| pair.as_node_tag() == Some(tag)
}

fn filter_tag<'a>(tag: &'a str) -> impl FnMut(Pair<'a>) -> Option<Pair<'a>> {
    move |pair| {
        if pair.as_node_tag() == Some(tag) {
            Some(pair)
        } else {
            None
        }
    }
}

fn match_rule<'a>(rule: Rule) -> impl Fn(&Pair<'a>) -> bool {
    move |pair| pair.as_rule() == rule
}

fn filter_rule<'a>(rule: Rule) -> impl Fn(Pair<'a>) -> Option<Pair<'a>> {
    move |pair| {
        if pair.as_rule() == rule {
            Some(pair)
        } else {
            None
        }
    }
}

fn find_tag<'a>(mut pairs: Pairs<'a>, tag: &'a str) -> Option<Pair<'a>> {
    return pairs.find(|p| p.as_node_tag() == Some(tag));
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    use crate::test_support::*;
    use crate::typescript::Pretty;
    use lexpr::sexp;
    use pest::consumes_to;
    use pest::fails_with;
    use pest::parses_to;
    use pretty_assertions::{assert_eq, assert_ne};
    use rstest::rstest;
    use std::assert_matches::assert_matches;
    use textwrap_macros::dedent;
    use Rule::*;

    #[rstest]
    #[case(
        "Equals(T, any)",
        sexp!((apply (receiver ident . "Equals") (args (ident . "T") any))))
    ]
    #[case(
        "A::Equals(T, any)",
        sexp!(
            (apply
                (receiver :: (segments (ident . "A") (ident . "Equals")))
                (args (ident . "T") any))
        )
    )]
    fn test_parse_expr_sexp_repr(#[case] input: &str, #[case] expected: lexpr::Value) {
        use crate::parser;

        let result = parser::NewtypeParser::parse(Rule::expr, input);

        match result {
            Ok(pairs) => {
                let actual = parser::parse_expr(pairs);

                pretty_assertions::assert_eq!(
                    actual.to_sexp().unwrap().to_string(),
                    expected.to_string()
                );
            }
            Err(err) => {
                panic!("{}", err);
            }
        }
    }

    #[rstest]
    // literals
    #[case("1", "1")]
    #[case("true", "true")]
    #[case("false", "false")]
    #[case("{}", "{}")]
    // function type
    #[case("() => void", "() => void")]
    #[case("(any) => void", "(arg0: any) => void")]
    #[case("(x: any) => void", "(x: any) => void")]
    #[case("(x: any, y: any, z: any) => void", "(x: any, y: any, z: any) => void")]
    #[case("(...args: any[]) => void", "(...args: any[]) => void")]
    #[case("(...any[]) => void", "(...rest: any[]) => void")]
    fn test_parse_expr_typescript_repr(#[case] input: &str, #[case] expected: &str) {
        use crate::parser;

        let result = parser::NewtypeParser::parse(Rule::expr, input);

        match result {
            Ok(pairs) => {
                let actual = parser::parse_expr(pairs).simplify().render_pretty_ts(80);

                pretty_assertions::assert_eq!(actual, expected);
            }
            Err(err) => {
                panic!("{}", err);
            }
        }
    }

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
            lexpr::sexp!((#"extends-infix-op" (lhs #"ident" . "A") (op . #"extends") (rhs #"ident" . "B")))
        );
    }

    #[test]
    fn extends_expr_parser_extends_parens() {
        assert_sexpr!(
            Rule::extends_expr,
            crate::parser::parse_extends_expr,
            "(A <: B)",
            lexpr::sexp!((#"extends-infix-op" (lhs #"ident" . "A") (op . #"extends") (rhs #"ident" . "B")))
        );
    }

    #[test]
    fn extends_expr_parser_extends_multiple_parens() {
        assert_sexpr!(
            Rule::extends_expr,
            crate::parser::parse_extends_expr,
            "((A <: B))",
            lexpr::sexp!((#"extends-infix-op" (lhs #"ident" . "A") (op . #"extends") (rhs #"ident" . "B")))
        );
    }

    #[test]
    fn extends_expr_parser_not_with_parens_extends() {
        assert_sexpr!(
            Rule::extends_expr,
            crate::parser::parse_extends_expr,
            "not (A <: B)",
            lexpr::sexp!(
                (#"extends-prefix-op"
                    (op . not)
                    (value #"extends-infix-op"
                        (lhs #"ident" . "A")
                        (op . extends)
                        (rhs #"ident" . "B")))
            )
        );
    }

    #[test]
    fn extends_expr_parser_and() {
        assert_sexpr!(
            Rule::extends_expr,
            crate::parser::parse_extends_expr,
            "A <: B and C <: D",
            lexpr::sexp!(
                (#"extends-infix-op"
                    (lhs #"extends-infix-op"
                        (lhs ident . "A")
                        (op . extends)
                        (rhs ident . "B"))
                (op . and)
                (rhs #"extends-infix-op"
                    (lhs ident . "C")
                    (op . extends)
                    (rhs ident . "D")))
            )
        );
    }

    #[test]
    fn extends_expr_parser_not_and() {
        assert_sexpr!(
            Rule::extends_expr,
            crate::parser::parse_extends_expr,
            "not (A <: B) and C <: D",
            lexpr::sexp!(
                (#"extends-infix-op"
                    (lhs #"extends-prefix-op"
                        (op . not)
                        (value #"extends-infix-op"
                            (lhs ident . "A")
                            (op . extends)
                            (rhs ident . "B")))
                    (op . and)
                    (rhs #"extends-infix-op"
                        (lhs ident . "C")
                        (op . extends)
                        (rhs ident . "D")))
            )
        );

        assert_sexpr!(
            Rule::extends_expr,
            crate::parser::parse_extends_expr,
            "A <: B and (not (C <: D))",
            lexpr::sexp!(
                (#"extends-infix-op"
                    (lhs #"extends-infix-op"
                        (lhs ident . "A")
                        (op . extends)
                        (rhs ident . "B"))
                    (op . and)
                    (rhs #"extends-prefix-op"
                        (op . not)
                        (value #"extends-infix-op"
                            (lhs ident . "C")
                            (op . extends)
                            (rhs ident . "D"))))
            )
        );

        assert_sexpr!(
            Rule::extends_expr,
            crate::parser::parse_extends_expr,
            "not (A <: B) and (not (C <: D))",
            lexpr::sexp!(
                (#"extends-infix-op"
                    (lhs #"extends-prefix-op"
                        (op . not)
                        (value #"extends-infix-op"
                            (lhs ident . "A")
                            (op . extends)
                            (rhs ident . "B")))
                    (op . and)
                    (rhs #"extends-prefix-op"
                        (op . not)
                        (value #"extends-infix-op"
                            (lhs ident . "C")
                            (op . extends)
                            (rhs ident . "D"))))
            )
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

    mod primitives {
        const R: Rule = Rule::expr;
        use super::*;

        #[test]
        fn number() {
            assert_typescript!("type A = number;", "type A as number");
        }

        #[test]
        fn boolean() {
            assert_typescript!("type A = boolean;", "type A as boolean");
        }

        #[test]
        fn never() {
            assert_typescript!("type A = never;", "type A as never");
        }

        #[test]
        fn any() {
            assert_typescript!("type A = any;", "type A as any");
        }

        #[test]
        fn unknown() {
            assert_typescript!("type A = unknown;", "type A as unknown");
        }
    }

    mod literals {
        const R: Rule = Rule::expr;
        use super::*;

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
    }

    mod bin_ops {
        const R: Rule = Rule::expr;
        use rstest::rstest;

        use super::*;

        #[test]
        fn pipe_into_identifier() {
            assert_typescript!(Rule::program, r#"type A = Y<X>;"#, r#"type A as X |> Y"#);
        }

        #[test]
        fn pipe_literal() {
            assert_typescript!(Rule::program, r#"type A = Y<1>;"#, r#"type A as 1 |> Y"#);
        }

        #[test]
        fn pipe_into_call_with_args() {
            assert_typescript!(
                Rule::program,
                r#"type A = Y<X, 1>;"#,
                r#"type A as X |> Y(1)"#
            );
        }

        #[test]
        fn pipe_chained() {
            assert_typescript!(
                Rule::program,
                r#"type A = D<C<B<A, 1>>, 2, 3, 4>;"#,
                r#"type A as A |> B(1) |> C |> D(2, 3, 4)"#
            );
        }

        fn union() {
            assert_typescript!("type A = 1 | 2;", "type A as 1 | 2");
        }

        #[test]
        fn intersection() {
            assert_typescript!("type A = 1 & 2;", "type A as 1 & 2");
        }

        #[rstest]
        #[case("A | B", "A | B")]
        #[case("A & B", "A & B")]
        #[case("A | B | C", "A | B | C")]
        #[case("A | (B & C)", "A | B & C")]
        #[case("C | (A & B)", "A & B | C")]
        #[case("C | (A & B)", "(A & B) | C")]
        #[case("A | (B & C)", "A | (B & C)")]
        #[case("(A & B) | (C & D)", "(A & B) | (C & D)")]
        #[case("A | (B & C & D)", "A | (B & C & D)")]
        #[case("A | B | (C & D)", "A | B | C & D")]
        fn infix_op(#[case] ts: &str, #[case] nt: &str) {
            let source = nt.trim();
            let expected = ts.trim();
            let pairs = parse!(crate::parser::Rule::expr, source);
            let simplified = pairs.simplify();
            let actual = simplified.render_pretty_ts(80);
            pretty_assertions::assert_eq!(expected, actual.trim());
        }
    }

    mod if_expr {
        const R: Rule = if_expr;
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
            map_expr,
            r#"
            { [k in t]: 1 }
            "#,
            r#"
            map k in t do 1 end
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

    mod unittest_statement {
        const R: Rule = Rule::program;
        use super::*;
        use pretty_assertions::assert_eq;

        #[ignore = "whitespace issues"]
        #[test]
        fn typescript_no_output() {
            assert_typescript!(
                R,
                "",
                r#"
                unittest "test" do
                    1
                end
                "#
            );
        }
    }

    mod unquote {
        const R: Rule = Rule::expr;
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn parsing() {
            assert_sexpr!(
                R,
                crate::parser::parse_expr,
                "unquote!(1)",
                lexpr::sexp!((macro (name . "unquote!") (args (number . "1"))))
            );
        }

        #[ignore]
        #[test]
        fn evaluates_expression() {
            assert_typescript!(
                R,
                "1",
                r#"
                unquote!(
                    if 1 <: number then
                        1
                    else
                        0
                    end
                )
                "#
            );
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
                number -> 1,
                string -> 2,
                else -> 3
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
                x <: number -> 1,
                x <: {} and x <: {a: 1} -> 2,
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
                x <: number -> 1,
                x <: {} and x <: {a: 1} -> 2,
                else -> 3
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
}
