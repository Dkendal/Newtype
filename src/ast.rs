use nom::character::complete::{char, one_of, satisfy};
use nom::combinator::{map, map_res, recognize};
use nom::multi::{many0, many1};
use nom::number::complete::float;
use nom::sequence::{preceded, separated_pair, terminated};
use nom::IResult;

#[derive(Debug)]
pub enum Program {
    Statements(Vec<Statement>),
}

#[derive(Debug)]
pub enum Statement {
    TypeAlias(String),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Number(String);

pub trait ToTypescript {
    fn to_ts(&self) -> String;
}

impl ToTypescript for Program {
    fn to_ts(&self) -> String {
        match self {
            Program::Statements(statements) => {
                let mut ts_code = String::new();
                for statement in statements {
                    ts_code.push_str(&statement.to_ts());
                    ts_code.push_str("\n");
                }
                ts_code
            }
        }
    }
}

impl ToTypescript for Statement {
    fn to_ts(&self) -> String {
        match self {
            Statement::TypeAlias(name) => format!("type {} = ?;", name),
        }
    }
}

pub fn program(input: &str) -> IResult<&str, Program> {
    let (input, statements) = many0(statement)(input)?;
    Ok((input, Program::Statements(statements)))
}

fn statement(input: &str) -> IResult<&str, Statement> {
    type_alias(input)
}

fn type_alias(input: &str) -> IResult<&str, Statement> {
    let (input, (id, _number)) = separated_pair(top_level_identifier, char('='), number)(input)?;
    Ok((input, Statement::TypeAlias(id.to_string())))
}

fn number(input: &str) -> IResult<&str, Number> {
    map(integer, |s| Number(s.to_owned()))(input)
}

fn integer(input: &str) -> IResult<&str, &str> {
    recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(input)
}

//BNF: <identifier> ::= [a-zA-Z_$][a-zA-Z0-9_$]*
fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(preceded(
        satisfy(|c| c == '_' || c == '$' || c.is_alphabetic()),
        many0(satisfy(|c| {
            c == '_' || c == '$' || c.is_ascii_alphanumeric()
        })),
    ))(input)
}

//BNF: <top_level_identifier> ::= [A-Z_$][a-zA-Z0-9_$]*
fn top_level_identifier(input: &str) -> IResult<&str, &str> {
    recognize(preceded(
        satisfy(|c| c == '_' || c == '$' || c.is_ascii_uppercase()),
        many0(satisfy(|c| {
            c == '_' || c == '$' || c.is_ascii_alphanumeric()
        })),
    ))(input)
}

#[cfg(test)]
mod tests {
    use quickcheck::TestResult;

    use super::*;

    fn s(input: &str) -> String {
        input.to_string()
    }

    #[quickcheck]
    fn prop_top_level_identifier(name: String) -> TestResult {
        if name.starts_with(|c: char| c.is_ascii_uppercase())
            && name
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '$')
        {
            let result = top_level_identifier(&name);
            TestResult::from_bool(matches!(result, Ok(("", n)) if n == name))
        } else {
            TestResult::discard()
        }
    }

    #[quickcheck]
    fn prop_top_level_identifier_refute(name: String) -> TestResult {
        if name.starts_with(|c: char| c.is_ascii_uppercase())
            && name
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '$')
        {
            return TestResult::discard();
        }

        let result = top_level_identifier(&name);

        match result {
            Err(_) => TestResult::passed(),
            // Fail if the input is fully consumed
            Ok(("", _)) => TestResult::failed(),
            // Ignore cases where the input is partially consumed
            Ok((_, _)) => TestResult::discard(),
        }
    }

    #[test]
    fn test_number() {
        assert_eq!(number("1"), Ok(("", Number(s("1")))));
        assert_eq!(number("-1"), Ok(("", Number(s("-1")))));
        assert_eq!(number("1.0"), Ok(("", Number(s("1")))));
        assert_eq!(number("-1.0"), Ok(("", Number(s("-1")))));
        // Free of floating point rouding errors
        assert_eq!(number("16777217"), Ok(("", Number(s("16777217")))));
        assert_eq!(number("16777217.0"), Ok(("", Number(s("16777217.0")))));
    }

    #[quickcheck]
    fn prop_number_integer(n: u64) -> bool {
        let Ok(("", Number(out))) = number(&n.to_string())
        else {
            return false
        };
        n.to_string() == out.to_string()
    }

    #[quickcheck]
    fn prop_parse_number_float(n: f64) -> bool {
        let Ok(("", Number(out))) = number(&n.to_string())
        else {
            return false
        };
        n.to_string() == out.to_string()
    }
}
