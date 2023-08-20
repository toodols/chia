use crate::parser::{
    ast::{Expression, ForLoop, Node},
    lexer::Token,
    ParseError, Parser, Sources,
};
impl<T: Sources> Parser<'_, T> {
    pub(super) fn parse_loops(&mut self) -> Result<Expression, ParseError> {
        match self.peek_token() {
            // Some(Token::While) => Ok(Loop::While(self.parse_while_loop()?)),
            Some(Token::For) => Ok(Expression::ForLoop(self.parse_for_loop()?)),
            _ => Err(ParseError::Message("Expected While or For".to_owned())),
        }
    }

    fn parse_for_loop(&mut self) -> Result<Node<ForLoop>, ParseError> {
        self.expect_token(Token::For)?;
        let pat = self.parse_pattern()?;
        self.expect_token(Token::In)?;
        let iter = Box::new(self.parse_expression()?);
        let block = self.parse_block()?;
        Ok(self.node(ForLoop {
            pat,
            iter,
            body: block,
        }))
    }
}
