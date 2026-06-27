#[macro_export]
macro_rules! ast {
    ($input:expr) => {{
        use newtype::parser;
        use pest::Parser;

        let pair = parser::NewtypeParser::parse(parser::Rule::expr, $input)
            .unwrap()
            .next()
            .unwrap();

        parser::parse(pair)
    }};
}

#[macro_export]
macro_rules! parse {
    ($rule:expr, $source:expr) => {{
        use pest::Parser;

        let result = newtype::parser::NewtypeParser::parse($rule, $source);

        let pair = result.unwrap_or_else(|e| panic!("{}", e)).next().unwrap();

        pretty_assertions::assert_eq!(
            pair.as_span().as_str(),
            $source,
            "Rule did not consume entire input"
        );

        newtype::parser::parse(pair)
    }};
    ($source:expr) => {
        parse!(newtype::parser::Rule::program, $source)
    };
}

#[macro_export]
macro_rules! assert_typescript {
    ($rule:expr, $expected:expr, $source:expr) => {
        let source = ::textwrap_macros::dedent!($source).trim();
        let expected = ::textwrap_macros::dedent!($expected).trim();

        let pairs = parse!($rule, source);

        let simplified = pairs.simplify();

        let actual = <_ as newtype::typescript::Pretty>::render_pretty_ts(&simplified, 80);

        pretty_assertions::assert_eq!(expected, actual.trim());
    };

    ($expected:expr, $source:expr) => {
        assert_typescript!(newtype::parser::Rule::program, $expected, $source);
    };
}
