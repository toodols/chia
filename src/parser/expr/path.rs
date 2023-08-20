use crate::parser::{
    ast::{Node, Path, SymbolName},
    lexer::Token,
    ParseError, Parser, Sources,
};

impl<T: Sources> Parser<'_, T> {
    pub(in crate::parser) fn parse_expr_path(&mut self) -> Result<Node<Path>, ParseError> {
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
        Ok(self.node(Path(path.into_iter().map(SymbolName::External).collect())))
    }
}
