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
mod pretty;
mod typescript;

#[cfg(test)]
mod test_support;

use clap::Parser;
use std::io::Read;
use typescript::Pretty;

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

    let result = parser::parse_newtype_program(&input_source);

    match result {
        Ok(result) => {
            let simplified = result.simplify();

            let out = simplified.render_pretty_ts(120);

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
