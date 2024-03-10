use std::fmt::{self};

use logos::Logos;

use crate::Gate;

type Lexer<'a> = logos::Lexer<'a, Token>;
type Error<'a> = ParseErr<'a>;
type Result<'a, T, E = Error<'a>> = std::result::Result<T, E>;

#[derive(Debug, PartialEq, Eq, Clone, Default, thiserror::Error)]
#[error("Invalid Token")]
pub struct TokenError;

#[derive(Logos, Debug, Clone, PartialEq, Eq)]
#[logos(skip "[ \t\r\n]+")]
#[logos(error = TokenError)]
pub enum Token {
    #[regex(r"[A-Za-z]", |s| s.slice().as_bytes()[0].to_ascii_uppercase() - b'A')]
    Ident(u8),
    #[token("!")]
    Not,
    #[token("+")]
    Or,
    #[token(".")]
    And,
    #[token("^")]
    Xor,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token("1", |_| true)]
    #[token("0", |_| false)]
    Literal(bool),

    Eof,
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum ParseErr<'a> {
    #[error("{0}")]
    TokenError(#[from] TokenError),
    #[error("Remaining: {remainder:?}")]
    RemainingTokens { parsed: Gate, remainder: &'a str },
    #[error("Expected `{expected}` Got `{got}`")]
    UnexpectedToken { expected: &'static str, got: Token },

    #[error("Missing token")]
    MissingToken,
}

pub fn parse(input: &str) -> Result<Gate> {
    Parser::new(Token::lexer(input)).parse()
}

struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub const fn new(lexer: Lexer<'a>) -> Self {
        Self { lexer }
    }

    pub fn parse(&mut self) -> Result<'a, Gate> {
        let parsed = self.parse_root()?;
        match self.lexer.remainder().trim() {
            "" => Ok(parsed),
            remainder => Err(ParseErr::RemainingTokens { parsed, remainder }),
        }
    }

    fn peek(&mut self) -> std::result::Result<Option<Token>, TokenError> {
        self.lexer.clone().next().transpose()
    }

    fn parse_root(&mut self) -> Result<'a, Gate> {
        let initial = self.parse_xor()?;
        let mut remainder = vec![];
        loop {
            match self.peek()? {
                Some(Token::Or) => {
                    self.lexer.next();
                    remainder.push(self.parse_xor()?);
                }
                Some(token @ (Token::Ident(_) | Token::OpenParen)) => {
                    return Err(ParseErr::UnexpectedToken { expected: "Op", got: token })
                }
                _ => break,
            };
        }
        Ok(remainder.into_iter().fold(initial, |acc, gate| Gate::Or(Box::new((acc, gate)))))
    }

    fn parse_xor(&mut self) -> Result<'a, Gate> {
        let initial = self.parse_and()?;
        let mut remainder = vec![];
        loop {
            match self.peek()? {
                Some(Token::Xor) => {
                    self.lexer.next();
                    remainder.push(self.parse_and()?);
                }
                Some(token @ (Token::Ident(_) | Token::OpenParen)) => {
                    return Err(ParseErr::UnexpectedToken { expected: "Op", got: token })
                }
                _ => break,
            };
        }
        Ok(remainder.into_iter().fold(initial, |acc, gate| Gate::Xor(Box::new((acc, gate)))))
    }

    fn parse_and(&mut self) -> Result<'a, Gate> {
        let initial = self.parse_atom()?;
        let mut remainder = vec![];
        loop {
            match self.peek()? {
                Some(Token::And) => {
                    self.lexer.next();
                    remainder.push(self.parse_atom()?);
                }
                Some(token @ (Token::Ident(_) | Token::OpenParen)) => {
                    return Err(ParseErr::UnexpectedToken { expected: "Op", got: token })
                }
                _ => break,
            };
        }
        Ok(remainder.into_iter().fold(initial, |acc, gate| Gate::And(Box::new((acc, gate)))))
    }

    fn parse_atom(&mut self) -> Result<'a, Gate> {
        let first = self.lexer.next().ok_or(ParseErr::MissingToken)??;

        Ok(match first {
            Token::Literal(bool) => Gate::Literal(bool),
            Token::Ident(ident) => Gate::Is(ident),
            Token::OpenParen => self.parse_parens()?,
            Token::Not => self.parse_atom().map(|gate| Gate::Not(Box::new(gate)))?,
            token => return Err(ParseErr::UnexpectedToken { expected: "Expression", got: token }),
        })
    }

    fn parse_parens(&mut self) -> Result<'a, Gate> {
        let gate = self.parse_root()?;

        match self.lexer.next().transpose()? {
            Some(Token::CloseParen) => Ok(gate),
            Some(token) => Err(ParseErr::UnexpectedToken { expected: ")", got: token }),
            None => Err(ParseErr::UnexpectedToken { expected: ")", got: Token::Eof }),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            Self::Not => "!",
            Self::And => ".",
            Self::Or => "+",
            Self::Xor => "^",
            Self::OpenParen => "(",
            Self::CloseParen => ")",
            Self::Literal(bool) => return write!(f, "{}", *bool as u8),
            Self::Ident(ident) => return write!(f, "{}", (b'A' + ident) as char),
            Self::Eof => "EOF",
        };
        f.write_str(str)
    }
}

#[cfg(test)]
mod tests {
    use super::parse;
    use crate::gates::consts::*;
    #[test]
    #[allow(clippy::precedence)]
    fn test_precedence() {
        assert_eq!(parse("A+B+C"), Ok(A | B | C));
        assert_eq!(parse("A+B.C"), Ok(A | B & C));
        assert_eq!(parse("A.B+C"), Ok(A & B | C));
        assert_eq!(parse("A.(B+C)"), Ok(A & (B | C)));
        assert_eq!(parse("(A.B)+C"), Ok((A & B) | C));
        assert_eq!(parse("A+B.C^D"), Ok(A | B & C ^ D));
        assert_eq!(parse("A.B^C+D"), Ok(A & B ^ C | D));
        assert_eq!(parse("A^B+C.D"), Ok(A ^ B | C & D));
    }
}
