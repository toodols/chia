use super::{
    ast::{Item, Mod, Node},
    lexer::Token,
    ParseError, Parser, Sources,
};
mod fn_decl;
mod struct_decl;

impl<T: Sources> Parser<'_, T> {
    pub(in crate::parser) fn parse_item(&mut self) -> Result<Item, ParseError> {
        match self.peek_token_with_pos() {
            Some((Token::Fn, _)) => Ok(Item::FunctionDeclaration(self.parse_fn_declaration()?)),
            Some((Token::Struct, _)) => Ok(self.parse_struct_declaration()?),
            Some((Token::Mod, _)) => {
                self.next_token();
                self.expect_token(Token::Identifier)?;
                let t = self.slice_token();
                let child = Item::Mod(self.node(Mod {
                    name: t.to_owned(),
                    body: self.parse_child(t)?,
                }));
                self.expect_token(Token::Semicolon)?;
                Ok(child)
            }
            Some((t, pos)) => Err(ParseError::UnexpectedToken { token: t, at: pos }),

            None => Err(ParseError::UnexpectedEOF),
        }
    }
}
