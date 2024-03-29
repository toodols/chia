use crate::parser::{
    ast::{Expression, IfExpression},
    lexer::Token,
    ParseError, Parser, Sources,
};

impl Parser<'_> {
    pub(super) fn parse_if_expr(&mut self) -> Result<IfExpression, ParseError> {
        self.expect_token(Token::If)?;
        let condition = self.parse_expression()?;
        let body = Box::new(Expression::Block(self.parse_block()?));
        let else_body = if self.peek_token() == Some(Token::Else) {
            self.next_token();
            if self.peek_token() == Some(Token::If) {
                Some(Box::new(Expression::IfExpression(self.parse_if_expr()?)))
            } else {
                Some(Box::new(Expression::Block(self.parse_block()?)))
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
}
