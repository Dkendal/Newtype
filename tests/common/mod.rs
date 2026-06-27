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
