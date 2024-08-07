use crate::{ast::Ast, parser::parse_newtype_program};

macro_rules! assert_sexpr {
    ($rule:expr, $processor:expr, $input:expr, $expected:expr) => {{
        let pairs = crate::parser::NewtypeParser::parse($rule, $input).unwrap();
        let actual = $processor(pairs);
        pretty_assertions::assert_eq!(actual.to_sexp().unwrap().to_string(), $expected.to_string());
    }};
}

pub(crate) use assert_sexpr;

macro_rules! ast {
    ($input:expr) => {{
        use crate::parser;
        use crate::pest::Parser;

        let pair = parser::NewtypeParser::parse(parser::Rule::expr, $input)
            .unwrap()
            .next()
            .unwrap();

        parser::parse(pair)
    }};
}

pub(crate) use ast;

macro_rules! sexpr {
    ($rule:expr, $input:expr) => {{
        use crate::parser;
        use crate::pest::Parser;

        let pair = parser::NewtypeParser::parse($rule, $input)
            .unwrap()
            .next()
            .unwrap();

        let ast = parser::parse(pair);

        serde_lexpr::to_value(ast)
    }};

    ($input:expr) => {
        sexpr!(crate::parser::Rule::expr, $input)
    };
}

pub(crate) use sexpr;

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
