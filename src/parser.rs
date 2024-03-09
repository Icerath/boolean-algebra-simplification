use std::fmt;

use logos::Logos;

use crate::Gate;

type Lexer<'a> = logos::Lexer<'a, Token>;
type Error<'a> = ParseErr<'a>;
type Result<'a, T, E = Error<'a>> = std::result::Result<T, E>;

#[derive(Debug, PartialEq, Eq, Clone, Default, thiserror::Error)]
#[error("Token Error")]
pub struct TokenError;

#[derive(Logos, Debug, Clone)]
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
}

#[derive(Debug, thiserror::Error)]
pub enum ParseErr<'a> {
    #[error("Token Error")]
    TokenError(#[from] TokenError),
    #[error("Remaining: {0:?}")]
    RemainingTokens(&'a str),
    #[error("Expected `{expected}` Got `{got}`")]
    UnexpectedToken { expected: &'static str, got: Token },
    #[error("Expected Token: `{0}`")]
    ExpectedToken(Token),
    #[error("Missing token")]
    MissingToken,
}

pub fn parse(input: &str) -> Result<Gate> {
    let mut parser = Parser::new(Token::lexer(input));
    let output = parser.parse()?;
    match parser.lexer.remainder().trim() {
        "" => Ok(output),
        remainder => Err(ParseErr::RemainingTokens(remainder)),
    }
}

struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub const fn new(lexer: Lexer<'a>) -> Self {
        Self { lexer }
    }

    pub fn parse(&mut self) -> Result<'a, Gate> {
        let first = self.parse_atom()?;

        let next = self.lexer.clone().next().transpose()?;
        Ok(match next {
            Some(op @ (Token::Or | Token::And | Token::Xor)) => {
                self.lexer.next();
                match op {
                    Token::Or => Gate::Or(Box::new((first, self.parse_atom()?))),
                    Token::And => Gate::And(Box::new((first, self.parse_atom()?))),
                    Token::Xor => Gate::Xor(Box::new((first, self.parse_atom()?))),
                    _ => unreachable!(),
                }
            }
            Some(Token::CloseParen) | None => first,
            Some(token) => return Err(ParseErr::UnexpectedToken { expected: "Op", got: token }),
        })
    }

    fn parse_atom(&mut self) -> Result<'a, Gate> {
        let first = self.lexer.next().ok_or(ParseErr::MissingToken)??;

        Ok(match first {
            Token::Ident(ident) => Gate::Is(ident),
            Token::OpenParen => self.parse_parens()?,
            Token::Not => self.parse_atom().map(|gate| Gate::Not(Box::new(gate)))?,
            token => return Err(ParseErr::UnexpectedToken { expected: "Expression", got: token }),
        })
    }

    fn parse_parens(&mut self) -> Result<'a, Gate> {
        let gate = self.parse()?;

        match self.lexer.next().transpose()? {
            Some(Token::CloseParen) => Ok(gate),
            Some(token) => Err(ParseErr::UnexpectedToken { expected: ")", got: token }),
            None => Err(ParseErr::ExpectedToken(Token::CloseParen)),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            Self::Not => "!",
            Self::And => ".",
            Self::Or => "|",
            Self::Xor => "^",
            Self::OpenParen => "(",
            Self::CloseParen => ")",
            Self::Ident(ident) => return write!(f, "`{}`", (b'A' + ident) as char),
        };
        f.write_str(str)
    }
}
