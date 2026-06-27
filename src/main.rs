use clap::Parser;
use newtype::typescript::Pretty;
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

    let input = input_source.as_str();

    let result = newtype::parser::parse_newtype_program(input);

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
