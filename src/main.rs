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
            let out = result.to_pretty_ts(120);

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

    macro_rules! join {
        ($($e:expr),*) => {
            vec![$($e.to_string()),*].join("\n").as_str()
        };
    }
    pub(crate) use join;

    macro_rules! parse {
        ($source:expr) => {
            crate::parser::parse_node(
                crate::parser::NewtypeParser::parse(
                    crate::parser::Rule::program,
                    &$source.to_string(),
                )
                .unwrap()
                .next()
                .unwrap(),
            )
        };
        ($rule:expr, $source:expr) => {
            crate::parser::parse_node(
                crate::parser::NewtypeParser::parse($rule, &$source.to_string())
                    .unwrap()
                    .next()
                    .unwrap(),
            )
        };
    }

    pub(crate) use parse;

    macro_rules! assert_typescript {
        ($expected:expr, $source:expr) => {
            let result = parse!($source);
            println!("INITIAL AST: {result:#?}");
            let simplified = result.simplify();
            println!("SIMPLIFIED AST: {:#?}", simplified);
            let actual = simplified.to_pretty_ts(usize::MAX);
            assert_eq!($expected.trim(), actual.trim());
        };
    }

    pub(crate) use assert_typescript;

    macro_rules! assert_parse_failure {
        ($source:expr) => {
            let result = parse_newtype($source.to_string())
            println!("{:#?}", result);
            assert!(result.is_err());
        };
    }

    // pub(crate) fn parse(source: String) -> Node {
    //     parse_newtype(&source).unwrap_or_else(|e| panic!("Failed to parse: {}", e))
    // }
}
