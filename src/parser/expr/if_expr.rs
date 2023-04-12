use crate::parser::{
    ast::{Expression, IfExpression},
    lexer::Token,
    ParseError, Parser,
};

use super::{block::parse_block, parse_expression};

pub(super) fn parse_if_expr(parser: &mut Parser) -> Result<IfExpression, ParseError> {
    parser.expect_token(Token::If)?;
    let condition = parse_expression(parser)?;
    let body = Box::new(Expression::Block(parse_block(parser)?));
    let else_body = if parser.peek_token() == Some(Token::Else) {
        parser.next_token();
        if parser.peek_token() == Some(Token::If) {
            Some(Box::new(Expression::IfExpression(parse_if_expr(parser)?)))
        } else {
            Some(Box::new(Expression::Block(parse_block(parser)?)))
        }
    } else {
        None
    };
    Ok(IfExpression {
        condition: Box::new(condition),
        body,
        else_body,
    })
}
