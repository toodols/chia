use crate::parser::{
    ast::{Literal, Node},
    lexer::Token,
    ParseError, Parser,
};

use super::parse_expression;

pub(in crate::parser) fn parse_literal(parser: &mut Parser) -> Result<Node<Literal>, ParseError> {
    let (token, pos) = parser
        .next_token_with_pos()
        .ok_or(ParseError::UnexpectedEOF)?; // TODO: Better error handling (e.g. UnexpectedEOF
    match token {
        Token::Number => Ok(parser.node(Literal::Number(parser.slice_token().parse().unwrap()))),
        Token::True => Ok(parser.node(Literal::Boolean(true))),
        Token::False => Ok(parser.node(Literal::Boolean(false))),
        Token::String => {
            // tokens.slice() will have quotes
            let text = parser.slice_token();
            Ok(parser.node(Literal::String(text[1..text.len() - 1].to_string())))
        }
        Token::Error => Err(ParseError::LexError),
        Token::LBracket => {
            let initial = parse_expression(parser)?;
            match parser.next_token() {
                Some(Token::Semicolon) => {
                    let len = parser.expect_token(Token::Number)?.parse().unwrap();
                    parser.expect_token(Token::RBracket)?;
                    Ok(parser.node(Literal::ArraySized(Box::new(initial), len)))
                }
                Some(Token::Comma) => {
                    let mut t = vec![initial, parse_expression(parser)?];
                    while parser.peek_token() == Some(Token::Comma) {
                        parser.next_token();
                        t.push(parse_expression(parser)?);
                    }
                    parser.expect_token(Token::RBracket)?;
                    Ok(parser.node(Literal::Array(t)))
                }
                Some(Token::RBracket) => Ok(parser.node(Literal::Array(vec![initial]))),
                Some(t) => Err(ParseError::UnexpectedToken { token: t, at: pos }),
                None => Err(ParseError::UnexpectedEOF),
            }
        }
        t => Err(ParseError::UnexpectedToken { token: t, at: pos }),
    }
}
