pub mod ast;
mod expr;
mod item;

mod lexer;
use std::ops::Range;

use ast::*;
use lexer::Token;
use logos::Logos;
use thiserror::Error;

use self::expr::parse_expression;
use self::item::parse_item;

struct Tokens<'a> {
    tokens: logos::Lexer<'a, Token>,
    peeked: Option<(Token, SourceLocation)>,
}

impl<'a> Tokens<'a> {
    fn new(source: &str) -> Tokens<'_> {
        let tokens = Token::lexer(source);
        Tokens {
            tokens,
            peeked: None,
        }
    }
    fn peek_token_with_pos(&mut self) -> Option<(Token, SourceLocation)> {
        if self.peeked.is_none() {
            if let Some(token) = self.tokens.next() {
                return Some(
                    self.peeked.insert((token, SourceLocation::range(self.tokens.span()))).clone()
                );
            } else {
                return None;
            }
        }
        self.peeked.clone()
    }
    fn next_token_with_pos(&mut self) -> Option<(Token, SourceLocation)> {
        let token = self.peeked.take();
        if token.is_none() {
            self.tokens.next().map(|token| {
                (token, SourceLocation::range(self.tokens.span()))
            })
        } else {
            token
        }
    }
    /// Expects the next token to be {token} and errors otherwise;
    fn expect_token(&mut self, token: Token) -> Result<&str, ParseError> {
        // thank you, copilot
        let t = match self.next_token_with_pos() {
            Some((Token::Error, _)) => Err(ParseError::LexError),
            Some((t, _)) if t == token => Ok(self.tokens.slice()),
            Some((t,pos)) => Err(ParseError::UnexpectedToken{token: t, at: pos}),
            None => Err(ParseError::UnexpectedEOF),
        };
        t
    }
    fn slice_token(&self) -> &str {
        self.tokens.slice()
    }
    // fn is(&mut self, func: impl FnOnce(Token) -> bool) -> Result<(Token, &str), ParseError> {
    //     match self.next() {
    //         Some(Token::Error) => Err(ParseError::LexError),
    //         Some(t) if func(t) => Ok((t, self.tokens.slice())),
    //         Some(t) => Err(ParseError::UnexpectedToken(t)),
    //         None => Err(ParseError::UnexpectedEOF),
    //     }
    // }
}

#[derive(Debug, Clone)]
pub struct SourceLocation {
    range: Range<usize>,
}
impl SourceLocation {
    fn range(range: Range<usize>) -> SourceLocation {
        SourceLocation { range }
    }
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Unknown error")]
    Unknown,
    #[error("Lex error")]
    LexError,
    #[error("Unexpected EOF")]
    UnexpectedEOF,
    #[error("Unexpected token {token:?} at {at:?}")]
    UnexpectedToken{token: Token, at: SourceLocation},
    #[error("{0}")]
    Message(String),
}

struct Parser<'a> {
    node_id_counter: usize,
    tokens: Tokens<'a>,
}

impl<'a> Parser<'a> {
    fn next_token(&mut self) -> Option<Token> {
        self.tokens.next_token_with_pos().map(|(t, _)| t)
    }
    fn next_token_with_pos(&mut self) -> Option<(Token, SourceLocation)> {
        self.tokens.next_token_with_pos()
    }
    fn peek_token(&mut self) -> Option<Token> {
        self.tokens.peek_token_with_pos().map(|(t, _)| t)
    }
    fn peek_token_with_pos(&mut self) -> Option<(Token, SourceLocation)> {
        self.tokens.peek_token_with_pos()
    }
    fn slice_token(&self) -> &str {
        self.tokens.slice_token()
    }
    fn expect_token(&mut self, token: Token) -> Result<&str, ParseError> {
        self.tokens.expect_token(token)
    }
    fn new(source: &'a str) -> Parser<'a> {
        Parser {
            node_id_counter: 0,
            tokens: Tokens::new(source),
        }
    }
    /// Gives a unique node id
    fn node_id(&mut self) -> usize {
        let id = self.node_id_counter;
        self.node_id_counter += 1;
        id
    }
}

fn parse_fn_call_body(parser: &mut Parser) -> Result<Vec<Expression>, ParseError> {
    parser.expect_token(Token::LParen)?;
    let mut args = Vec::new();
    loop {
        match parser.peek_token() {
            Some(Token::RParen) => {
                break;
            }
            Some(Token::Error) => return Err(ParseError::LexError),
            Some(_) => {
                args.push(parse_expression(parser)?);
                match parser.peek_token_with_pos() {
                    Some((Token::Comma, _)) => {
                        parser.next_token();
                    }
                    Some((Token::RParen, _)) => {
                        break;
                    }
                    Some((t,pos)) => return Err(ParseError::UnexpectedToken{token: t, at: pos}),
                    None => return Err(ParseError::UnexpectedEOF),
                };
            }
            None => return Err(ParseError::UnexpectedEOF),
        }
    }
    parser.expect_token(Token::RParen)?;
    Ok(args)
}

#[allow(unused)]
fn parse_macro_invocation(parser: &mut Parser) -> Result<Vec<Token>, ParseError> {
    // very simple. all parentheses must be closed, all brackets must be closed, and all braces must be closed
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum OpenToken {
        Brace,
        Paren,
        Bracket,
    }
    impl From<Token> for OpenToken {
        fn from(t: Token) -> Self {
            match t {
                Token::LBrace | Token::RBrace => OpenToken::Brace,
                Token::LParen | Token::RParen => OpenToken::Paren,
                Token::LBracket | Token::RBracket => OpenToken::Bracket,
                _ => panic!("Invalid token for OpenToken"),
            }
        }
    }
    let mut stack: Vec<Token> = Vec::new();
    let mut matched: Vec<Token> = Vec::new();
    loop {
        let (tok, pos) = parser.next_token_with_pos().ok_or(ParseError::UnexpectedEOF)?;
        match tok {
            Token::LParen | Token::LBracket | Token::LBrace => {
                stack.push(tok);
                stack.push(tok);
            }
            Token::RParen | Token::RBracket | Token::RBrace => {
                match stack.pop() {
                    Some(top) => {
                        if OpenToken::from(top) != OpenToken::from(tok) {
                            return Err(ParseError::UnexpectedToken{token: tok, at: pos});
                        }
                    }
                    None => return Err(ParseError::UnexpectedToken{token: tok, at: pos}),
                }
            }
            Token::Error => return Err(ParseError::LexError),
            _ => {}
        }
        matched.push(tok);
        if stack.is_empty() {
            break;
        }
    }
    Ok(matched)
}

fn parse_let_declaration(parser: &mut Parser) -> Result<LetDeclaration, ParseError> {
    parser.expect_token(Token::Let)?;
    let pat = parse_pattern(parser)?;

    if parser.peek_token() == Some(Token::Assign) {
        parser.expect_token(Token::Assign)?;
        let value = parse_expression(parser)?;
        if !value.terminating() {
            parser.expect_token(Token::Semicolon)?;
        }
        Ok(LetDeclaration {
            pat,
            value: Some(value),
            node_id: parser.node_id(),
        })
    } else {
        parser.expect_token(Token::Semicolon)?;
        Ok(LetDeclaration { pat, value: None, node_id: parser.node_id() })
    }
}

fn parse_pattern(parser: &mut Parser) -> Result<Pattern, ParseError> {
    let ident = parser.expect_token(Token::Identifier)?;
    Ok(Pattern::Ident(ident.to_owned()))
}

fn parse_type(parser: &mut Parser) -> Result<TypeExpr, ParseError> {
    match parser.next_token_with_pos() {
        Some((Token::Identifier, _)) => Ok(TypeExpr::Identifier(parser.slice_token().to_owned())),
        Some((Token::LParen, _)) => {
            if parser.peek_token() == Some(Token::RParen) {
                parser.next_token();
                return Ok(TypeExpr::Unit);
            }
            let mut types = Vec::new();
            types.push(parse_type(parser)?);
            while parser.peek_token() == Some(Token::Comma) {
                parser.next_token();
                types.push(parse_type(parser)?);
            }
            parser.expect_token(Token::RParen)?;
            Ok(TypeExpr::Tuple(types))
        }
        Some((Token::Error, _)) => Err(ParseError::LexError),
        Some((t,pos)) => Err(ParseError::UnexpectedToken{token: t, at: pos}),
        None => Err(ParseError::UnexpectedEOF),
    }
}

fn parse_program(parser: &mut Parser) -> Result<Program, ParseError> {
    let node_id = parser.node_id();
    let mut items = Vec::new();
    while parser.peek_token().is_some() {
        items.push(parse_item(parser)?);
    }
    println!("finished parsing");
    Ok(Program { items, node_id })
}

pub fn parse(source: &str) -> Result<Program, ParseError> {
    let mut parser = Parser::new(source);
    parse_program(&mut parser)
}
