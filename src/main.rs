#![feature(never_type)]
#![allow(dead_code)]

extern crate alloc;
extern crate pest;

#[macro_use]
extern crate pest_derive;

#[cfg(test)]
extern crate quickcheck;

#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

mod parser;

use clap::Parser;
use std::io::Read;
// use parser::ToTypescript;


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

    // let (_, program) = ast::program(&input_source).unwrap();
    //
    // // Generate TypeScript code
    // let ts_code = program.to_ts();
    //
    // if let Some(output_filename) = args.output {
    //     std::fs::write(output_filename, ts_code).unwrap();
    // } else {
    //     println!("{}", ts_code);
    // }
}
