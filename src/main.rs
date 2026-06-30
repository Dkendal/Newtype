use clap::Parser;
use newtype::test_codegen;
use newtype::test_harness;
use newtype::typescript::Pretty;
use std::io::Read;

#[derive(Debug, Parser)]
#[clap(name = "newtype compiler")]
struct Args {
    #[clap(short, long, value_name = "FILE")]
    input: Option<String>,
    #[clap(short, long, value_name = "FILE")]
    output: Option<String>,
    /// Stop evaluating `unittest` assertions at the first failure.
    #[clap(long)]
    fail_fast: bool,
    /// Emit TypeScript type-level assertions for each `unittest` assert, prefixed
    /// with the helper types they need.
    #[clap(long)]
    generate_tests: bool,
    /// With `--generate-tests`, annotate each generated `type` alias with a
    /// `/** @newtype line:N */` comment giving the source line of its `assert`.
    #[clap(long, requires = "generate_tests")]
    source_comments: bool,
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

            // Evaluate `unittest` assertions after simplification but before
            // rendering. Failures are reported to stderr; rendering still
            // proceeds so the emitted TypeScript is always produced.
            let report = test_harness::run(
                &simplified,
                input,
                test_harness::Config {
                    fail_fast: args.fail_fast,
                },
                &mut std::io::stderr(),
            )
            .expect("writing the test report to stderr failed");

            let out = if args.generate_tests {
                let expansion = test_codegen::expand(&simplified, input);
                let header = test_codegen::render_helpers(&expansion.helpers);
                let mut body = expansion.ast.render_pretty_ts(120);
                if args.source_comments {
                    body = test_codegen::attach_comments(&body, &expansion.comments);
                }
                if header.is_empty() {
                    body
                } else {
                    format!("{header}\n{body}")
                }
            } else {
                simplified.render_pretty_ts(120)
            };

            if let Some(output_filename) = args.output {
                std::fs::write(output_filename, out).unwrap();
            } else {
                println!("{}", out);
            }

            // Non-zero exit on any assertion failure, after rendering completes.
            if report.has_failures() {
                std::process::exit(1);
            }
        }
        Err(error) => {
            eprintln!("{}", error);
            std::process::exit(1);
        }
    }
}
