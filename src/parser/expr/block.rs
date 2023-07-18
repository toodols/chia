use super::parse_expression;
use crate::parser::{
    ast::{Block, Statement},
    lexer::Token,
    parse_let_declaration, ParseError, Parser,
};

pub(in crate::parser) fn parse_block(parser: &mut Parser) -> Result<Block, ParseError> {
    parser.expect_token(Token::LBrace)?;
    let mut statements = Vec::new();
    let mut does_return = false;
    let node_id = parser.node_id();
    struct IterRes {
        statement: Statement,
        can_cont: bool,
        did_close: bool,
    }
    if parser.peek_token() != Some(Token::RBrace) {
        loop {
            // cont is whether another expression is allowed
            /*
                if a {

                }
                // this is allowed because cont is true
                1
            */
            /*
                1
                2 // not allowed; cont is false
            */
            /*
            1;
            2 // allowed; cont is true
            */
            let IterRes {
                statement,
                can_cont,
                did_close,
            } = match parser.peek_token() {
                Some(Token::Let) => Ok(IterRes {
                    statement: Statement::LetDeclaration(parse_let_declaration(parser)?),
                    can_cont: true,
                    did_close: true,
                }),
                Some(Token::Semicolon) => {
                    parser.next_token();
                    Ok(IterRes {
                        statement: Statement::Empty,
                        can_cont: true,
                        did_close: true,
                    })
                }
                Some(Token::Error) => Err(ParseError::LexError),
                Some(_) => {
                    let expr = parse_expression(parser)?;
                    let mut terminating = expr.terminating();
                    let mut did_close = false;
                    if parser.peek_token() == Some(Token::Semicolon) {
                        did_close = true;
                        terminating = true;
                        parser.next_token();
                    }
                    Ok(IterRes {
                        statement: Statement::Expression(expr),
                        can_cont: terminating,
                        did_close,
                    })
                }
                None => Err(ParseError::UnexpectedEOF),
            }?;

            statements.push(statement);
            if !did_close {
                does_return = true;
            }

            if parser.peek_token() == Some(Token::RBrace) {
                break;
            } else if !can_cont {
                return Err(ParseError::Message("Expected Semicolon".to_owned()));
            }
        }
    }
    parser.expect_token(Token::RBrace)?;
    Ok(Block {
        statements,
        does_return,
        node_id,
    })
}
