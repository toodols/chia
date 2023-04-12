use crate::parser::{Parser, ast::{Expression, ForLoop}, ParseError, parse_pattern, lexer::Token};

use super::{block::parse_block, parse_expression};

pub(super) fn parse_loops(parser: &mut Parser) -> Result<Expression, ParseError> {
    match parser.peek_token() {
        // Some(Token::While) => Ok(Loop::While(self.parse_while_loop()?)),
        Some(Token::For) => Ok(Expression::ForLoop(parse_for_loop(parser)?)),
        _ => Err(ParseError::Message("Expected While or For".to_owned())),
    }
}

fn parse_for_loop(parser: &mut Parser) -> Result<ForLoop, ParseError> {
    parser.expect_token(Token::For)?;
    let pat = parse_pattern(parser)?;
    parser.expect_token(Token::In)?;
    let iter = Box::new(parse_expression(parser)?);
    let block = parse_block(parser)?;
    Ok(ForLoop {
        pat,
        iter,
        body: block,
    })
}