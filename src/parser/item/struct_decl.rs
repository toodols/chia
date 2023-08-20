use crate::parser::{
    ast::{Item, SymbolName, TupleStructDeclaration},
    lexer::Token,
    ParseError, Parser, Sources,
};

impl<T: Sources> Parser<'_, T> {
    pub(super) fn parse_struct_declaration(&mut self) -> Result<Item, ParseError> {
        self.expect_token(Token::Struct)?;
        let name = SymbolName::External(self.expect_token(Token::Identifier)?.to_owned());
        if self.peek_token() == Some(Token::LBrace) {
            todo!()
        } else if self.peek_token() == Some(Token::LParen) {
            self.next_token();
            let mut fields = Vec::new();
            fields.push(self.parse_type()?);
            while self.peek_token() == Some(Token::Comma) {
                self.next_token();
                fields.push(self.parse_type()?);
            }
            self.expect_token(Token::RParen)?;
            self.expect_token(Token::Semicolon)?;
            Ok(Item::TupleStructDeclaration(
                self.node(TupleStructDeclaration { name, fields }),
            ))
        } else {
            Err(ParseError::Unknown)
        }
    }
}
