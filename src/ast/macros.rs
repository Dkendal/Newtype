macro_rules! assert_ast {
    ($pair:expr, $rule:expr) => {
        assert_eq!($pair.clone().as_rule(), $rule, "Rule mismatch");
    };

    ($pair:expr, $tag:expr => $rule:expr) => {
        assert_eq!($pair.clone().as_rule(), $rule, "Rule mismatch");
        assert_eq!($pair.clone().as_node_tag(), Some($tag), "Node tag mismatch")
    };
}

pub(crate) use assert_ast;

macro_rules! parse_error {
    ($pair:expr) => {{
        parse_error!($pair, vec![], vec![$pair.clone().as_rule()]);
    }};

    ($pair:expr, $message:expr) => {{
        let error = Error::<Rule>::new_from_span(
            ErrorVariant::CustomError { message: $message },
            $pair.clone().as_span(),
        );

        panic!("{error}");
    }};

    ($pair:expr, $positives:expr, $negatives:expr) => {{
        let error = Error::<Rule>::new_from_span(
            ErrorVariant::ParsingError {
                positives: $positives,
                negatives: $negatives,
            },
            $pair.clone().as_span(),
        );
        panic!("{error}");
    }};
}

pub(crate) use parse_error;
