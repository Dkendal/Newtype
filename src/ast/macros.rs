macro_rules! node {
    ($value: pat) => {
        $crate::ast::Node {
            span: _,
            value: box $value,
        }
    };
}

pub(crate) use node;

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

macro_rules! next_pair {
    ($pairs:expr, $rule:expr) => {
        if let Some(pair) = $pairs.next() {
            if pair.as_rule() == $rule {
                pair
            } else {
                panic!("Expected {:#?} got {:#?}", $rule, pair.as_rule())
            }
        } else {
            panic!("Expected {:#?} got None", $rule)
        }
    };
}

pub(crate) use next_pair;

macro_rules! take_pairs {
    ($pairs:expr, $($rule:expr),+) => {{
        let mut pairs = $pairs;
        (
            $(
                if let Some(pair) = pairs.next() {
                    if pair.as_rule() == $rule {
                        pair
                    } else {
                        panic!("Expected {:#?} got {:#?}", $rule, pair.as_rule())
                    }
                } else {
                    panic!("Expected {:#?} got None", $rule)
                }
            ),+
        )
    }};
}

pub(crate) use take_pairs;

macro_rules! take_tags {
    ($pairs:expr, [$($tag:expr),+]) => {{
        let tags = [$($tag),+];
        let mut init: [Option<Pair>; ${count($tag)}] = Default::default();

        for pair in $pairs {
            if let Some(tag) = pair.as_node_tag() {
                if let Some(index) = tags.iter().position(|&t| t == tag) {
                    init[index] = Some(pair);
                }
            }
        }

        init
    }};
}

pub(crate) use take_tags;

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
