pub mod ast;
mod expr;
mod item;

mod lexer;
use ast::*;
use lexer::Token;
use logos::Logos;

use self::expr::parse_expression;
use self::item::parse_item;

struct Tokens<'a> {
    tokens: logos::Lexer<'a, Token>,
    peeked: Option<Token>,
}

impl<'a> Tokens<'a> {
    fn new<'source>(source: &'source str) -> Tokens<'source> {
        let tokens = Token::lexer(source);
        Tokens {
            tokens,
            peeked: None,
        }
    }
    fn peek_token(&mut self) -> Option<Token> {
        if self.peeked == None {
            if let Some(token) = self.tokens.next() {
                // weird
                return Some(*self.peeked.insert(token));
            }
        }
        self.peeked
    }
    fn next_token(&mut self) -> Option<Token> {
        let token = self.peeked.take();
        if token == None {
            self.tokens.next()
        } else {
            token
        }
    }
    /// Expects the next token to be {token} and errors otherwise;
    fn expect_token(&mut self, token: Token) -> Result<&str, ParseError> {
        // thank you, copilot
        let t = match self.next_token() {
            Some(Token::Error) => Err(ParseError::LexError),
            Some(t) if t == token => Ok(self.tokens.slice()),
            Some(t) => Err(ParseError::UnexpectedToken(t)),
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

#[derive(Debug)]
pub enum ParseError {
    Unknown,
    LexError,
    UnexpectedEOF,
    UnexpectedToken(Token),
    Message(String),
}

struct Parser<'a> {
    node_id_counter: usize,
    tokens: Tokens<'a>,
}

impl<'a> Parser<'a> {
    fn next_token(&mut self) -> Option<Token> {
        self.tokens.next_token()
    }
    fn peek_token(&mut self) -> Option<Token> {
        self.tokens.peek_token()
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
            Some(t) => {
                args.push(parse_expression(parser)?);
                match parser.peek_token() {
                    Some(Token::Comma) => {
                        parser.next_token();
                    }
                    Some(Token::RParen) => {
                        break;
                    }
                    Some(t) => return Err(ParseError::UnexpectedToken(t)),
                    None => return Err(ParseError::UnexpectedEOF),
                };
            }
            None => return Err(ParseError::UnexpectedEOF),
        }
    }
    parser.expect_token(Token::RParen)?;
    Ok(args)
}

fn parse_macro_invocation(parser: &mut Parser) -> Result<Vec<Token>, ParseError> {
    // very simple. all parentheses must be closed, all brackets must be closed, and all braces must be closed
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
        let tok = parser.next_token().ok_or(ParseError::UnexpectedEOF)?;
        match tok {
            Token::LParen | Token::LBracket | Token::LBrace => {
                stack.push(tok.into());
            }
            Token::RParen | Token::RBracket | Token::RBrace => {
                let open = stack.pop().ok_or(ParseError::UnexpectedToken(tok))?;
                if open != tok.into() {
                    return Err(ParseError::UnexpectedToken(tok));
                }
            }
            Token::Error => return Err(ParseError::LexError),
            _ => {}
        }
        matched.push(tok);
        if stack.len() == 0 {
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
        })
    } else {
        parser.expect_token(Token::Semicolon)?;
        Ok(LetDeclaration { pat, value: None })
    }
}

fn parse_pattern(parser: &mut Parser) -> Result<Pattern, ParseError> {
    let ident = parser.expect_token(Token::Identifier)?;
    Ok(Pattern::Ident(ident.to_owned()))
}

fn parse_type(parser: &mut Parser) -> Result<TypeExpr, ParseError> {
    match parser.next_token() {
        Some(Token::Identifier) => Ok(TypeExpr::Identifier(parser.slice_token().to_owned())),
        Some(Token::LParen) => {
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
        Some(Token::Error) => Err(ParseError::LexError),
        Some(t) => Err(ParseError::UnexpectedToken(t)),
        None => Err(ParseError::UnexpectedEOF),
    }
}

fn parse_program(parser: &mut Parser) -> Result<Program, ParseError> {
    let node_id = parser.node_id();
    let mut items = Vec::new();
    while let Some(_) = parser.peek_token() {
        items.push(parse_item(parser)?);
    }
    println!("finished parsing");
    Ok(Program { items, node_id })
}

pub fn parse(source: &str) -> Result<Program, ParseError> {
    let mut parser = Parser::new(source);
    parse_program(&mut parser)
}
