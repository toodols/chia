use crate::{
    parser::{
        ast::{Parameters, StructDeclaration, Item, TupleStructDeclaration, SymbolName},
        lexer::Token,
        parse_pattern, parse_type, ParseError, Parser,
    },
};

pub(super) fn parse_struct_declaration(parser: &mut Parser) -> Result<Item, ParseError> {
	parser.expect_token(Token::Struct)?;
	let name = SymbolName::External(parser.expect_token(Token::Identifier)?.to_owned());
	if parser.peek_token() == Some(Token::LBrace) {
		todo!()
	} else if parser.peek_token() == Some(Token::LParen) {
		parser.next_token();
		let mut fields = Vec::new();
		fields.push(parse_type(parser)?);
		while parser.peek_token() == Some(Token::Comma) {
			parser.next_token();
			fields.push(parse_type(parser)?);
		}
		parser.expect_token(Token::RParen)?;
		parser.expect_token(Token::Semicolon)?;
		Ok(Item::TupleStructDeclaration(TupleStructDeclaration{
			name,
			fields,
		}))
	} else {
		Err(ParseError::Unknown)
	}
}
