use crate::{ast::Ast, parser::parse_newtype_program};

macro_rules! assert_sexpr {
    ($rule:expr, $processor:expr, $input:expr, $expected:expr) => {{
        let pairs = crate::parser::NewtypeParser::parse($rule, $input).unwrap();
        let actual = $processor(pairs);
        pretty_assertions::assert_eq!(actual.to_sexpr(80), $expected);
    }};
}

pub(crate) use assert_sexpr;

macro_rules! parse {
    ($rule:expr, $source:expr) => {{
        use crate::pest::Parser;

        let result = crate::parser::NewtypeParser::parse($rule, $source);

        let pair = result.unwrap_or_else(|e| panic!("{}", e)).next().unwrap();

        pretty_assertions::assert_eq!(
            pair.as_span().as_str(),
            $source,
            "Rule did not consume entire input"
        );

        let out = crate::parser::parse(pair);

        out
    }};
    ($source:expr) => {
        parse!(crate::parser::Rule::program, $source)
    };
}

pub(crate) use parse;

macro_rules! assert_typescript {
    ($rule:expr, $expected:expr, $source:expr) => {
        let source = dedent!($source).trim();
        let expected = dedent!($expected).trim();

        let pairs = parse!($rule, source);

        let simplified = pairs.simplify();

        let actual = simplified.render_pretty_ts(80);

        pretty_assertions::assert_eq!(expected, actual.trim());
    };

    ($expected:expr, $source:expr) => {
        assert_typescript!(crate::parser::Rule::program, $expected, $source);
    };
}

pub(crate) use assert_typescript;

macro_rules! assert_parse_failure {
    ($source:expr) => {
        let node = parse_newtype($source.to_string())
        println!("{:#?}", node);
        assert!(node.is_err());
    };
}
