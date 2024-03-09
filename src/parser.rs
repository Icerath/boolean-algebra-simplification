use std::fmt;

use logos::Logos;

use crate::Gate;

type Lexer<'a> = logos::Lexer<'a, Token<'a>>;
type Error<'a> = ParseErr<'a>;
type Result<'a, T, E = Error<'a>> = std::result::Result<T, E>;

#[derive(Debug, PartialEq, Eq, Clone, Default, thiserror::Error)]
#[error("Token Error")]
pub struct TokenError;

#[derive(Logos, Debug, Clone)]
#[logos(skip "[ \t\r\n]+")]
#[logos(error = TokenError)]
pub enum Token<'a> {
    #[regex(r"\w")]
    Ident(&'a str),
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
    UnexpectedToken { expected: &'static str, got: Token<'a> },
    #[error("Expected Token: `{0}`")]
    ExpectedToken(Token<'a>),
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
    idents: Vec<&'a str>,
}

impl<'a> Parser<'a> {
    pub const fn new(lexer: Lexer<'a>) -> Self {
        Self { lexer, idents: vec![] }
    }

    pub fn parse(&mut self) -> Result<'a, Gate> {
        let first = self.parse_atom()?;

        Ok(match self.lexer.next().transpose()? {
            Some(Token::Or) => Gate::Or(Box::new((first, self.parse_atom()?))),
            Some(Token::And) => Gate::And(Box::new((first, self.parse_atom()?))),
            Some(Token::Xor) => Gate::Xor(Box::new((first, self.parse_atom()?))),
            Some(_) | None => first,
        })
    }

    fn parse_atom(&mut self) -> Result<'a, Gate> {
        let first = self.lexer.next().ok_or(ParseErr::MissingToken)??;

        Ok(match first {
            Token::Ident(ident) => {
                if let Some(index) = self.idents.iter().position(|i| &ident == i) {
                    Gate::Is(index as u32)
                } else {
                    self.idents.push(ident);
                    Gate::Is(self.idents.len() as u32 - 1)
                }
            }
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

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            Self::Not => "!",
            Self::And => ".",
            Self::Or => "|",
            Self::Xor => "^",
            Self::OpenParen => "(",
            Self::CloseParen => ")",
            Self::Ident(ident) => return write!(f, "Ident: `{ident}`"),
        };
        f.write_str(str)
    }
}
