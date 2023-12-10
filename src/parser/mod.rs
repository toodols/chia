pub mod ast;
mod expr;
mod item;

mod lexer;
use std::collections::HashMap;
use std::ops::Range;
use std::sync::Arc;

use ast::*;
use lexer::Token;
use logos::Logos;
use std::fmt::Debug;
use thiserror::Error;

struct Tokens<'a> {
    tokens: logos::Lexer<'a, Token>,
    peeked: Option<(Token, SourceLocation)>,
}

impl<'a> Tokens<'a> {
    fn new<'s>(source: &'s str) -> Tokens<'s> {
        let tokens = Token::lexer(source);
        Tokens {
            tokens,
            peeked: None,
        }
    }
    fn peek_token_with_pos(&mut self) -> Option<(Token, SourceLocation)> {
        if self.peeked.is_none() {
            return match self.tokens.next() {
                Some(Ok(token)) => Some(
                    self.peeked
                        .insert((token, SourceLocation::range(self.tokens.span())))
                        .clone(),
                ),
                _ => None,
            };
        }
        self.peeked.clone()
    }
    fn next_token_with_pos(&mut self) -> Option<(Token, SourceLocation)> {
        let token = self.peeked.take();
        if token.is_none() {
            match self.tokens.next() {
                Some(Ok(token)) => Some((token, SourceLocation::range(self.tokens.span()))),
                _ => None,
            }
        } else {
            token
        }
    }
    /// Expects the next token to be {token} and errors otherwise;
    fn expect_token(&mut self, token: Token) -> Result<&str, ParseError> {
        // thank you, copilot
        let t = match self.next_token_with_pos() {
            Some((t, _)) if t == token => Ok(self.tokens.slice()),
            Some((t, pos)) => Err(ParseError::UnexpectedToken { token: t, at: pos }),
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
    UnexpectedToken { token: Token, at: SourceLocation },
    #[error("{0}")]
    Message(String),
}

pub trait Sources {
    fn get(&self, path: &std::path::Path) -> Option<&str>;
    fn child(&self, path: &std::path::Path, child: &str) -> Option<&std::path::Path>;
}

impl Sources for &str {
    fn get(&self, path: &std::path::Path) -> Option<&str> {
        if path == std::path::Path::new("") {
            Some(self)
        } else {
            None
        }
    }
    fn child(&self, path: &std::path::Path, child: &str) -> Option<&std::path::Path> {
        None
    }
}

pub(crate) struct Parser<'a> {
    node_id_counter: usize,
    tokens: Tokens<'a>,
    path: &'a std::path::Path,
    sources: Arc<&'a dyn Sources>,
}

impl<'a> Parser<'a> {
    /// Constructs a unique node from a given value
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

    pub(crate) fn new<'s, S: Sources>(sources: &'s S) -> Parser<'s> {
        let sources: Arc<&dyn Sources> = Arc::new(sources);
        let s = sources.get(std::path::Path::new("")).unwrap();
        Parser {
            node_id_counter: 0,
            path: std::path::Path::new(""),
            tokens: Tokens::new(s),
            sources,
        }
    }
    fn parse_child(&self, name: &str) -> Result<Program, ParseError> {
        let child_path = self
            .sources
            .child(self.path, name)
            .ok_or_else(|| ParseError::Unknown)?;

        // trait T {

        // }
        // struct S {
        //     s: Arc<dyn T>
        // }
        // fn f(&S) {

        // }

        let mut parser = Parser {
            node_id_counter: 0,
            path: child_path,
            sources: self.sources.clone(),
            tokens: Tokens::new(self.sources.get(child_path).unwrap()),
        };

        parser.parse_program()
    }
    /// Gives a unique node id
    fn node_id(&mut self) -> usize {
        let id = self.node_id_counter;
        self.node_id_counter += 1;
        id
    }
}

impl Parser<'_> {
    fn parse_fn_call_body(&mut self) -> Result<Vec<Expression>, ParseError> {
        self.expect_token(Token::LParen)?;
        let mut args = Vec::new();
        loop {
            match self.peek_token() {
                Some(Token::RParen) => {
                    break;
                }
                Some(_) => {
                    args.push(self.parse_expression()?);
                    match self.peek_token_with_pos() {
                        Some((Token::Comma, _)) => {
                            self.next_token();
                        }
                        Some((Token::RParen, _)) => {
                            break;
                        }
                        Some((t, pos)) => {
                            return Err(ParseError::UnexpectedToken { token: t, at: pos })
                        }
                        None => return Err(ParseError::UnexpectedEOF),
                    };
                }
                None => return Err(ParseError::UnexpectedEOF),
            }
        }
        self.expect_token(Token::RParen)?;
        Ok(args)
    }

    fn parse_let_declaration(&mut self) -> Result<LetDeclaration, ParseError> {
        self.expect_token(Token::Let)?;
        let pat = self.parse_pattern()?;

        if self.peek_token() == Some(Token::Assign) {
            self.expect_token(Token::Assign)?;
            let value = self.parse_expression()?;
            if !value.terminating() {
                self.expect_token(Token::Semicolon)?;
            }
            Ok(LetDeclaration {
                pat,
                value: Some(value),
                id: self.node_id(),
            })
        } else {
            self.expect_token(Token::Semicolon)?;
            Ok(LetDeclaration {
                pat,
                value: None,
                id: self.node_id(),
            })
        }
    }
    fn parse_ty_path(&mut self) -> Result<TyPath, ParseError> {
        let mut path = Vec::new();
        if self.peek_token() == Some(Token::Identifier) {
            self.next_token();
            path.push(self.slice_token().to_owned());
        }
        while self.peek_token() == Some(Token::Scope) {
            self.next_token();
            self.expect_token(Token::Identifier)?;
            path.push(self.slice_token().to_owned());
        }
        Ok(TyPath {
            path: path
                .into_iter()
                .map(|s| Span::from(s))
                .collect::<Vec<Span>>(),
            id: self.node_id(),
        })
    }
    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        let path = self.parse_expr_path()?;
        Ok(Pattern::Path(path))
    }

    fn parse_type(&mut self) -> Result<TypeExpr, ParseError> {
        match self.next_token_with_pos() {
            Some((Token::Identifier, _)) => Ok(TypeExpr::Identifier(self.slice_token().to_owned())),
            Some((Token::LParen, _)) => {
                if self.peek_token() == Some(Token::RParen) {
                    self.next_token();
                    return Ok(TypeExpr::Unit);
                }
                let mut types = Vec::new();
                types.push(self.parse_type()?);
                while self.peek_token() == Some(Token::Comma) {
                    self.next_token();
                    types.push(self.parse_type()?);
                }
                self.expect_token(Token::RParen)?;
                Ok(TypeExpr::Tuple(types))
            }
            Some((t, pos)) => Err(ParseError::UnexpectedToken { token: t, at: pos }),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    pub(crate) fn parse_program(&mut self) -> Result<Program, ParseError> {
        let node_id = self.node_id();
        let mut items = Vec::new();
        while self.peek_token().is_some() {
            items.push(self.parse_item()?);
        }
        Ok(Program { items, id: node_id })
    }
}
// don't even bother trying to include macros in this shit language
// #[allow(unused)]
// fn parse_macro_invocation(parser: &mut Parser) -> Result<Vec<Token>, ParseError> {
//     // very simple. all parentheses must be closed, all brackets must be closed, and all braces must be closed
//     #[derive(Debug, Clone, Copy, PartialEq, Eq)]
//     enum OpenToken {
//         Brace,
//         Paren,
//         Bracket,
//     }
//     impl From<Token> for OpenToken {
//         fn from(t: Token) -> Self {
//             match t {
//                 Token::LBrace | Token::RBrace => OpenToken::Brace,
//                 Token::LParen | Token::RParen => OpenToken::Paren,
//                 Token::LBracket | Token::RBracket => OpenToken::Bracket,
//                 _ => panic!("Invalid token for OpenToken"),
//             }
//         }
//     }
//     let mut stack: Vec<Token> = Vec::new();
//     let mut matched: Vec<Token> = Vec::new();
//     loop {
//         let (tok, pos) = parser
//             .next_token_with_pos()
//             .ok_or(ParseError::UnexpectedEOF)?;
//         match tok {
//             Token::LParen | Token::LBracket | Token::LBrace => {
//                 stack.push(tok);
//                 stack.push(tok);
//             }
//             Token::RParen | Token::RBracket | Token::RBrace => match stack.pop() {
//                 Some(top) => {
//                     if OpenToken::from(top) != OpenToken::from(tok) {
//                         return Err(ParseError::UnexpectedToken {
//                             token: tok,
//                             at: pos,
//                         });
//                     }
//                 }
//                 None => {
//                     return Err(ParseError::UnexpectedToken {
//                         token: tok,
//                         at: pos,
//                     })
//                 }
//             },
//             Token::Error => return Err(ParseError::LexError),
//             _ => {}
//         }
//         matched.push(tok);
//         if stack.is_empty() {
//             break;
//         }
//     }
//     Ok(matched)
// }

pub fn parse<'a>(source: &'a str) -> Result<Program, ParseError> {
    // struct SingleSource<'a>(&'a str);

    // impl<'a> Sources for SingleSource<'a> {
    //     fn get(&self, path: &std::path::Path) -> Option<&str> {
    //         None
    //     }
    //     fn child(&self, path: &std::path::Path, child: &str) -> Option<&std::path::Path> {
    //         None
    //     }
    // }
    // let binding = SingleSource(source);
    // let mut parser = Parser::<'a, SingleSource<'a>>::new(&binding);
    let mut parser = Parser::new(&source);
    parser.parse_program()
}

#[test]
fn test_block_is_expr() {
    let mut parser = Parser::new(&"{}");
    let program = parser.parse_expression().unwrap();
}
