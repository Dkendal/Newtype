// #![feature(never_type)]
#![feature(assert_matches)]
#![feature(box_patterns)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_macros)]

extern crate alloc;
extern crate pest;

#[macro_use]
extern crate pest_derive;

#[cfg(test)]
extern crate quickcheck;

#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

mod ast;
mod parser;
mod simplify;
mod to_typescript;
mod transform;

use crate::simplify::Simplify;
use crate::to_typescript::ToTypescript;
use clap::Parser;
use std::io::Read;

#[derive(Debug, Parser)]
#[clap(name = "newtype compiler")]
struct Args {
    #[clap(short, long, value_name = "FILE")]
    input: Option<String>,
    #[clap(short, long, value_name = "FILE")]
    output: Option<String>,
}

fn main() {
    let args = Args::parse();

    let input_source = if let Some(input_filename) = args.input {
        std::fs::read_to_string(input_filename).unwrap()
    } else {
        let mut input = String::new();
        std::io::stdin().read_to_string(&mut input).unwrap();
        input
    };

    let result = parser::parse_newtype(&input_source);

    match result {
        Ok(result) => {
            let simplified = result.simplify();

            let out = simplified.to_pretty_ts(120);

            if let Some(output_filename) = args.output {
                std::fs::write(output_filename, out).unwrap();
            } else {
                println!("{}", out);
            }
        }
        Err(error) => {
            eprintln!("{}", error);
        }
    }
}

#[cfg(test)]
pub mod test_support {
    use crate::{ast::Node, parser::parse_newtype};

    macro_rules! parse {
        ($rule:expr, $source:expr) => {{
            let pair = crate::parser::NewtypeParser::parse($rule, $source)
                .unwrap_or_else(|e| panic!("{}", e))
                .next()
                .unwrap_or_else(|| panic!("No parse result"));

            assert_eq!(
                pair.as_span().as_str(),
                $source,
                "Rule did not consume entire input"
            );

            let out = crate::parser::parse_node(pair);

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

            let actual = simplified.to_pretty_ts(80);
            assert_eq!(expected, actual.trim());
        };

        ($expected:expr, $source:expr) => {
            assert_typescript!(crate::parser::Rule::statement, $expected, $source);
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
}
