use crate::parser::{
    ast::{Path, Span},
    lexer::Token,
    ParseError, Parser,
};

impl Parser<'_> {
    pub(in crate::parser) fn parse_expr_path(&mut self) -> Result<Path, ParseError> {
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
        Ok(Path {
            path: path
                .into_iter()
                .map(|s| Span::from(s))
                .collect::<Vec<Span>>(),
            id: self.node_id(),
        })
    }
}
