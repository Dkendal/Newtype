use std::iter::Peekable;

use logos::{Logos, Span, SpannedIter};

// a LL content sensitive parser. Indentation is significant.

#[derive(Logos, Debug, PartialEq)]
enum Token {
    #[regex("[ \t]+", logos::skip)]
    #[regex("\n", logos::skip)]
    Indent,

    #[token("type")]
    Type,

    #[token("interface")]
    Interface,

    #[token("import")]
    Import,

    #[token("export")]
    Export,

    #[token("from")]
    From,

    #[token("extends")]
    Extends,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[token("=")]
    Equals,

    #[token(":")]
    Colon,

    #[token(";")]
    Semi,

    // Allow numbers to have underscores in them for readability
    #[regex("-?[0-9][0-9_]*(\\.([0-9][0-9_]*)?)?")]
    Number,

    #[regex("[a-zA-Z$_][a-zA-Z0-9_$_]*")]
    Ident,

    #[regex(r#""([^"\\]|\\.)*""#)]
    String,
}

type Scanner<'a> = logos::Lexer<'a, Token>;

// ebnf grammar
// program = { statement }
// statement = Type Ident Equals expression
// expression = Number | String

struct Node {
    name: String,
    span: Span,
    children: Vec<Node>,
}

impl Node {
    fn new(name: String, span: Span) -> Node {
        Node {
            name,
            span,
            children: vec![],
        }
    }

    fn root() -> Node {
        Node {
            name: "root".to_string(),
            span: Span { start: 0, end: 0 },
            children: vec![],
        }
    }
}

fn parse(source: &str) -> Result<Node, ()> {
    let mut lex = Token::lexer(source);

    parse_program(&mut lex);

    Err(())
}

type ParserResult = Result<Node, ()>;

fn parse_program(lex: &mut Scanner) -> ParserResult {
    todo!()
}

#[cfg(test)]
mod tests {
    use quickcheck::TestResult;

    use super::*;

    fn s(input: &str) -> String {
        input.to_string()
    }

    #[test]
    fn test_parse() {
        let source = "type A = 1";

        parse(source);

        assert_eq!(0, 1);
    }

    #[test]
    fn test_lex() {
        let mut lex = Token::lexer("1");
        assert_eq!(lex.next(), Some(Ok(Token::Number)));
        assert_eq!(lex.slice(), "1");

        let mut lex = Token::lexer("-1");
        assert_eq!(lex.next(), Some(Ok(Token::Number)));
        assert_eq!(lex.slice(), "-1");

        let mut lex = Token::lexer("-1.");
        assert_eq!(lex.next(), Some(Ok(Token::Number)));
        assert_eq!(lex.slice(), "-1.");

        let mut lex = Token::lexer("-1.0");
        assert_eq!(lex.next(), Some(Ok(Token::Number)));
        assert_eq!(lex.slice(), "-1.0");

        let mut lex = Token::lexer("1");
        assert_eq!(lex.next(), Some(Ok(Token::Number)));
        assert_eq!(lex.slice(), "1");

        let mut lex = Token::lexer("1.");
        assert_eq!(lex.next(), Some(Ok(Token::Number)));
        assert_eq!(lex.slice(), "1.");

        let mut lex = Token::lexer("1_000");
        assert_eq!(lex.next(), Some(Ok(Token::Number)));
        assert_eq!(lex.slice(), "1_000");

        let mut lex = Token::lexer("1_000.000_000");
        assert_eq!(lex.next(), Some(Ok(Token::Number)));
        assert_eq!(lex.slice(), "1_000.000_000");

        let source = "type A = 1";
        let mut lex = Token::lexer(source);

        assert_eq!(lex.next(), Some(Ok(Token::Type)));
        assert_eq!(lex.next(), Some(Ok(Token::Ident)));
        assert_eq!(lex.next(), Some(Ok(Token::Equals)));
        assert_eq!(lex.next(), Some(Ok(Token::Number)));
    }
    //
    //     #[quickcheck]
    //     fn prop_parse_number_float(n: f64) -> bool {
    //         let Ok(("", Number(out))) = number(&n.to_string())
    //         else {
    //             return false
    //         };
    //         n.to_string() == out.to_string()
    //     }
}
