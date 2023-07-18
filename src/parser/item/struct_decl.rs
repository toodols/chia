use crate::parser::{
    ast::{Item, SymbolName, TupleStructDeclaration},
    lexer::Token,
    parse_type, ParseError, Parser,
};

pub(super) fn parse_struct_declaration(parser: &mut Parser) -> Result<Item, ParseError> {
    parser.expect_token(Token::Struct)?;
    let name = SymbolName::External(parser.expect_token(Token::Identifier)?.to_owned());
    if parser.peek_token() == Some(Token::LBrace) {
        todo!()
    } else if parser.peek_token() == Some(Token::LParen) {
        let mut fields = Vec::new();
        fields.push(parse_type(parser)?);
        while parser.peek_token() == Some(Token::Comma) {
            parser.next_token();
            fields.push(parse_type(parser)?);
        }
        Ok(Item::TupleStructDeclaration(TupleStructDeclaration {
            name,
            fields,
        }))
    } else {
        Err(ParseError::Unknown)
    }
}
