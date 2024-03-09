use logos::Logos;

use crate::Gate;

type Error = ParseErr;
type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, PartialEq, Clone, Default, thiserror::Error)]
#[error("Token Error")]
pub struct TokenError;

#[derive(Logos, Debug, Clone)]
#[logos(skip "[ \t\r\n]+")]
#[logos(error = TokenError)]
pub enum Token {
    #[regex(r"\w", |str| { Ok::<Box::<str>, TokenError>(From::from(str.slice())) })]
    Ident(Box<str>),
    #[token("!")]
    Not,
    #[token(r"+")]
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
pub enum ParseErr {
    #[error("Token Error")]
    TokenError(#[from] TokenError),
    #[error("Remaining tokens: `{0:?}`")]
    RemainingTokens(Vec<Token>),
    #[error("Unexpected Token: `{0:?}`")]
    UnexpectedToken(Token),
    #[error("Expected Token: `{0:?}`")]
    ExpectedToken(Token),
    #[error("Missing token")]
    MissingToken,
}

pub fn parse(input: &str) -> Result<Gate> {
    let tokens = Token::lexer(input).collect::<Result<Vec<Token>, TokenError>>()?;
    let mut parser = Parser::new(&tokens);
    let output = parser.parse()?;
    if !parser.tokens.is_empty() {
        return Err(ParseErr::RemainingTokens(parser.tokens.to_vec()));
    }
    Ok(output)
}

struct Parser<'a> {
    tokens: &'a [Token],
    idents: Vec<&'a str>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            idents: vec![],
        }
    }

    pub fn parse(&mut self) -> Result<Gate> {
        let first = self.parse_atom()?;

        Ok(match self.bump() {
            Some(Token::Or) => Gate::Or(Box::new((first, self.parse_atom()?))),
            Some(Token::And) => Gate::And(Box::new((first, self.parse_atom()?))),
            Some(Token::Xor) => Gate::Xor(Box::new((first, self.parse_atom()?))),
            Some(_) | None => first,
        })
    }

    fn bump(&mut self) -> Option<&'a Token> {
        let token = self.tokens.first()?;
        self.tokens = &self.tokens[1..];
        Some(token)
    }

    fn parse_atom(&mut self) -> Result<Gate> {
        let first = self.bump().ok_or(ParseErr::MissingToken)?;

        Ok(match first {
            Token::Ident(ident) => {
                if let Some(index) = self.idents.iter().position(|&i| &**ident == i) {
                    Gate::Is(index as u32)
                } else {
                    self.idents.push(ident);
                    Gate::Is(self.idents.len() as u32 - 1)
                }
            }
            Token::OpenParen => self.parse_parens()?,
            Token::Not => self.parse_atom().map(|gate| Gate::Not(Box::new(gate)))?,
            token => return Err(ParseErr::UnexpectedToken(token.clone())),
        })
    }
    fn parse_parens(&mut self) -> Result<Gate> {
        let gate = self.parse()?;

        match self.bump() {
            Some(Token::CloseParen) => Ok(gate),
            Some(token) => Err(ParseErr::UnexpectedToken(token.clone())),
            None => Err(ParseErr::ExpectedToken(Token::CloseParen)),
        }
    }
}
