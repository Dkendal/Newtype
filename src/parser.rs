pub(crate) mod pratt;

use std::{
    collections::{BTreeSet, HashMap},
    default,
    iter::FilterMap,
    rc::Rc,
    result,
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
use pest::{
    error::{Error, ErrorVariant},
    pratt_parser::PrattParser,
    Parser,
};

use pratt::{EXPR_PARSER, EXTENDS_PARSER};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct NewtypeParser;

pub type ParserError = Error<Rule>;

pub type Pair<'i> = pest::iterators::Pair<'i, Rule>;
pub type Pairs<'i> = pest::iterators::Pairs<'i, Rule>;

pub fn parse_expr(pairs: Pairs) -> Ast {
    use Rule::*;

    EXPR_PARSER
        .map_primary(parse)
        .map_prefix(|op, child| match op.as_rule() {
            infer => Ast::Infer(child.into()),
            rule => {
                parse_error!(op, vec![infer], vec![rule]);
            }
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            indexed_access => {
                let span: Span = lhs.as_span().merge(&Span::from(&op));

                let inner = op.into_inner().next().unwrap();
                let lhs = lhs.into();
                let rhs = parse(inner).into();

                Ast::Access(Access {
                    lhs,
                    rhs,
                    is_dot: false,
                    span,
                })
            }
            array_modifier => Ast::Array(lhs.into()),
            application => {
                let span: Span = lhs.as_span().merge(&Span::from(&op));

                let args = next_pair!(op.clone().into_inner(), Rule::argument_list);

                Ast::ApplyGeneric(ApplyGeneric {
                    span,
                    receiver: lhs.into(),
                    args: args.into_inner().map(parse).collect(),
                })
            }
            rule => {
                parse_error!(op, vec![indexed_access, array_modifier], vec![rule]);
            }
        })
        .map_infix(|lhs, op, rhs| {
            let span: Span = lhs.as_span().merge(&rhs.as_span());

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

                    match lhs {
                        Ast::Path(Path { segments, .. }) => acc.extend(segments.to_owned()),
                        Ast::Ident(_) => acc.push(lhs),
                        ast => {
                            let error = ast.as_span().as_parsing_error(
                                op.get_input(),
                                vec![Rule::ident],
                                vec![],
                            );
                            panic!("{error}");
                        }
                    };

                    match rhs {
                        Ast::Path(Path { segments, .. }) => acc.extend(segments.to_owned()),
                        Ast::Ident(_) => acc.push(rhs),
                        ast => {
                            let error = ast.as_span().as_parsing_error(
                                op.get_input(),
                                vec![Rule::ident],
                                vec![],
                            );
                            panic!("{error}");
                        }
                    };

                    Ast::Path(Path {
                        span,
                        segments: acc,
                    })
                }

                dot_op => Ast::Access(Access {
                    lhs: lhs.into(),
                    rhs: rhs.into(),
                    is_dot: true,
                    span,
                }),

                rule => unreachable!("Expected infix operator, found {:?}", rule),
            };

            ast
        })
        .parse(pairs)
}

/// Replace the pipe operator macro with a type application
/// ```text
/// A |> B
/// # B(A)
/// ```
fn replace_pipe_with_type_application(rhs: Ast, lhs: Ast, op: Pair) -> Ast {
    let span: Span = lhs.as_span().merge(&rhs.as_span());

    match rhs {
        Ast::Ident(_) => {
            // FIXME missing span
            Ast::ApplyGeneric(ApplyGeneric {
                span,
                receiver: rhs.into(),
                args: vec![lhs],
            })
        }
        Ast::ApplyGeneric(ApplyGeneric {
            receiver: name,
            args: params,
            ..
        }) => {
            let mut params = params.clone();
            params.insert(0, lhs);
            Ast::ApplyGeneric(ApplyGeneric {
                span,
                receiver: name.clone(),
                args: params,
            })
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

pub fn parse_newtype_program(source: &str) -> result::Result<Ast, Box<Error<Rule>>> {
    let pair = NewtypeParser::parse(Rule::program, source)?.next().unwrap();

    Ok(parse(pair))
}

pub fn parse_extends_expr(pairs: Pairs) -> Ast {
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
            let span: Span = primary_node.as_span().merge(&op.as_span().into());

            if op.as_rule() == Rule::not && !primary_node.is_extends_infix_op() {
                let error_not = Error::<Rule>::new_from_span(
                    ErrorVariant::CustomError {
                        message: "`not` may only be used with an extends expression".to_string(),
                    },
                    op.as_span(),
                );

                let error = span.as_parsing_error(
                    op.get_input(),
                    vec![Rule::extends_expr],
                    vec![],
                );

                let error_expr = format!("{error}");

                panic!("{error_not}\n{error_expr}\nHint: You might have forgotten to wrap the expression in parentheses, `not` has higher precedence than other operators.");
            }

            let op = match op.as_rule() {
                Rule::not => PrefixOp::Not,
                rule => parse_error!(op, vec![Rule::not], vec![rule]),
            };

           let value = primary_node.into();

        Ast::ExtendsPrefixOp(ExtendsPrefixOp {
            op,
            value,
            span
        })
        })
        .map_infix(|lhs: Ast, op: Pair, rhs: Ast| {
            let span: Span = lhs.as_span().merge(&rhs.as_span());

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

            let lhs = lhs.into();
            let rhs = rhs.into();
            let extends_infix_op = ExtendsInfixOp { lhs, op, rhs, span };
            

            Ast::ExtendsInfixOp(extends_infix_op)
        })
        .parse(pairs)
}

pub fn parse(pair: Pair) -> Ast {
    let rule = pair.clone().as_rule();
    let span: Span = pair.as_span().into();

    // TODO: just return AST, remove calls to new/1 wrap result
    match rule {
        Rule::program => parse_program(pair),
        Rule::statement => parse_statement(pair),
        Rule::type_alias => parse_type_alias(pair),
        Rule::unittest => Ast::UnitTest(parse_unittest(pair)),
        Rule::interface => parse_interface(pair),
        Rule::import_statement => parse_import_statement(pair),
        Rule::if_expr => parse_if_expr(pair),
        Rule::object_literal => Ast::TypeLiteral(parse_object_literal(pair)),
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
            Ast::Primitive(primitive, span)
        }
        Rule::number => Ast!(TypeNumber {
            ty: pair.as_str().to_string(),
            span,
        }),
        Rule::string => parse_type_string(pair),
        Rule::template_string => Ast!(TemplateString {
            ty: pair.as_str().to_string(),
            span,
        }),
        Rule::ident => Ast::Ident(parse_ident(pair)),
        Rule::never => Ast::NeverKeyword(span),
        Rule::any => Ast::AnyKeyword(span),
        Rule::unknown => Ast::UnknownKeyword(span),
        Rule::boolean => {
            let value = pair.into_inner().next().unwrap();

            match value.as_rule() {
                Rule::literal_true => Ast::TrueKeyword(span),
                Rule::literal_false => Ast::FalseKeyword(span),
                _ => unreachable!(),
            }
        }
        Rule::tuple => parse_tuple(pair),
        Rule::macro_call => Ast::MacroCall(parse_macro_call(pair)),
        Rule::builtin => parse_builtin(pair),
        Rule::expr => parse_expr(pair.into_inner()),
        Rule::match_expr => Ast::MatchExpr(parse_match_expr(pair)),
        Rule::cond_expr => Ast::CondExpr(parse_cond_expr(pair)),
        Rule::map_expr => Ast::MappedType(parse_map_expr(pair)),
        Rule::let_expr => Ast::LetExpr(parse_let_expr(pair)),
        Rule::function_type => Ast::FunctionType(parse_function_type(pair)),

        Rule::EOI => {
            parse_error!(pair, format!("Unexpected end of input"));
        }

        rule => {
            parse_error!(pair, format!("Unexpected rule: {:?}", rule));
        }
    }
}

fn parse_builtin(pair: Pair) -> Ast {
    let span: Span = (&pair).into();
    let mut inner = pair.into_inner();

    let name = inner.find(match_tag("name")).unwrap();

    let name = match name.as_rule() {
        Rule::keyof => BuiltinKeyword::Keyof,
        _ => unreachable!(
            "unexpected rule while parsing builtin: {:?}",
            name.as_rule()
        ),
    };

    let argument = inner.find(match_tag("argument")).map(parse).unwrap().into();

    Ast::Builtin(Builtin {
        name,
        argument,
        span,
    })
}

fn parse_match_expr(pair: Pair) -> MatchExpr {
    use match_expr::Arm;

    let span: Span = (&pair).into();

    let mut inner = pair.into_inner();

    let value = inner.find(match_tag("value")).map(parse).unwrap().into();

    let arms: Vec<match_expr::Arm> = inner
        .clone()
        .filter(match_tag("arm"))
        .map(|pair| {
            let span: Span = (&pair).into();
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
        .unwrap_or(Ast::NeverKeyword(span))
        .into();

    MatchExpr {
        span,
        value,
        arms,
        else_arm,
    }
}

fn parse_cond_expr(pair: Pair) -> CondExpr {
    let span = Span::from(&pair);
    let inner = pair.into_inner();

    let else_arm = inner
        .clone()
        .find(match_tag("else"))
        .and_then(|p| p.into_inner().find(match_tag("body")))
        .map(parse)
        .unwrap_or(Ast::NeverKeyword(span))
        .into();

    let arms: Vec<cond_expr::Arm> = inner
        .clone()
        .filter(match_tag("arm"))
        .map(|arm| {
            use cond_expr::Arm;
            let span = Span::from(&arm);
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
    let span: Span = (&pair).into();

    let mut inner = pair.into_inner();

    let ipk = next_pair!(inner, Rule::index_property_key);

    let body = inner.find(match_tag("body")).map(parse).unwrap().into();

    let mut inner = ipk.into_inner();

    let index = inner
        .next()
        .and_then(filter_rule(Rule::ident))
        .unwrap()
        .as_str()
        .to_string();

    let iterable = inner.find(match_tag("iterable")).map(parse).unwrap().into();

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
    let span: Span = (&pair).into();

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
            let value = parse(value);

            (name, value)
        })
        .collect();

    let body = pair
        .clone()
        .into_inner()
        .find(match_tag("body"))
        .map(parse)
        .unwrap()
        .into();

    LetExpr {
        span,
        bindings,
        body,
    }
}

fn parse_function_type(pair: Pair) -> FunctionType {
    let span: Span = (&pair).into();

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
                    span: pair.as_span().into(),
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
                    span: pair.as_span().into(),
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

    let return_type = parse(return_type).into();

    FunctionType {
        span,
        params,
        return_type,
    }
}

fn parse_macro_call(pair: Pair) -> MacroCall {
    let span: Span = (&pair).into();
    let mut inner = pair.into_inner();
    let name = next_pair!(inner, Rule::macro_ident);
    let args = next_pair!(inner, Rule::argument_list);
    let name = name.as_str().to_string();
    let inner = args.into_inner();
    let args = inner.map(parse).collect_vec();
    MacroCall { span, name, args }
}

fn parse_unittest(pair: Pair) -> UnitTest {
    let span: Span = (&pair).into();
    let mut inner = pair.into_inner();
    let name = next_pair!(inner, Rule::string);
    let name = name.as_str().to_string();
    let body = inner.map(parse).collect_vec();

    UnitTest { span, name, body }
}

fn parse_type_alias(pair: Pair) -> Ast {
    let span: Span = (&pair).into();
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

    let body = Rc::new(inner.clone().find(match_tag("body")).map(parse).unwrap());

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
    Ast::Statement(inner.into())
}

fn parse_program(pair: Pair) -> Ast {
    let span: Span = (&pair).into();

    let statements: Vec<_> = pair
        .clone()
        .into_inner()
        .filter(|pair| pair.as_rule() != Rule::EOI) // Remove the end of input token
        .map(parse)
        .collect();

    Ast::Program(Program { statements, span })
}

fn parse_interface(pair: Pair) -> Ast {
    let span: Span = (&pair).into();
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

    Ast::Interface(Interface {
        span,
        export,
        extends,
        name,
        params,
        definition,
    })
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
                    let span: Span = (&pair).into();
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

    ordered_params
        .iter()
        .map(|name| params.get(name).unwrap().clone())
        .collect()
}

fn parse_ident(pair: Pair) -> Ident {
    assert_ast!(pair, Rule::ident);
    Ident {
        name: pair.as_str().to_string(),
        span: pair.as_span().into(),
    }
}

/// Returns a `String` with the contents of a string literal (without the quotes).
fn parse_string_literal(pair: Pair) -> String {
    assert_ast!(pair, Rule::string);

    match pair.clone().into_inner().next().unwrap().as_rule() {
        Rule::atom_string => pair.as_str().trim_start_matches(':').to_string(),
        Rule::double_quote_string => pair.as_str().trim_matches('"').to_string(),
        Rule::single_quote_string => pair.as_str().trim_matches('\'').to_string(),
        _ => unreachable!(),
    }
}

fn parse_type_string(pair: Pair) -> Ast {
    Ast!(TypeString {
        span: (&pair).into(),
        ty: parse_string_literal(pair.clone()),
    })
}

fn parse_import_statement(pair: Pair) -> Ast {
    let span: Span = (&pair).into();
    let mut inner = pair.clone().into_inner();

    let import_clause = inner.next().unwrap();

    let import_clause = match import_clause.as_rule() {
        Rule::named_import => {
            let specs = import_clause
                .into_inner()
                .find_tagged("import_specifier")
                .map(|pair| {
                    let span: Span = (&pair).into();
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

    let module = inner.next().map(parse_string_literal).unwrap();

    Ast::ImportStatement(ImportStatement {
        import_clause,
        module,
        span,
    })
}

fn parse_if_expr(pair: Pair) -> Ast {
    let span: Span = (&pair).into();
    let mut inner = pair.clone().into_inner();

    let condition = inner
        .find(match_tag("condition"))
        .map(|p| parse_extends_expr(p.into_inner()) )
        .unwrap();

    let then_branch = inner.find(match_tag("then")).map(parse).unwrap().into();

    let else_branch = inner
        .find(match_tag("else"))
        .map(parse)
        .unwrap_or_else(|| Ast::NeverKeyword(span))
        .into();

    match condition {
        // Other conditions are desugared later in the simplification step
        Ast::ExtendsInfixOp(ExtendsInfixOp { .. })
        | Ast::ExtendsPrefixOp(ExtendsPrefixOp { .. }) => Ast::IfExpr(IfExpr {
            span,
            condition: Rc::new(condition),
            then_branch,
            else_branch: Some(else_branch),
        }),
        _ => unreachable!(),
    }
}

fn parse_tuple(pair: Pair) -> Ast {
    let span: Span = (&pair).into();
    let items = pair.clone().into_inner().map(parse).collect();

    Ast::Tuple(Tuple { span, items })
}

fn parse_object_literal(pair: Pair) -> TypeLiteral {
    let span: Span = (&pair).into();
    let object_property_rules = pair.clone().into_inner();
    let mut properties = Vec::new();

    for prop_pair in object_property_rules {
        let span: Span = (&pair).into();

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

    TypeLiteral { properties, span }
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
    let span: Span = key.clone().into();
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

fn node_as_string(pair: Pair<'_>) -> String {
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

fn match_rule(rule: Rule) -> impl Fn(&Pair) -> bool {
    move |pair| pair.as_rule() == rule
}

fn filter_rule(rule: Rule) -> impl Fn(Pair) -> Option<Pair> {
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

