use self::{fn_decl::parse_fn_declaration, struct_decl::parse_struct_declaration};

use super::{
    ast::{Item, Node},
    lexer::Token,
    ParseError, Parser,
};
mod fn_decl;
mod struct_decl;

pub(in crate::parser) fn parse_item(parser: &mut Parser) -> Result<Item, ParseError> {
    match parser.peek_token_with_pos() {
        Some((Token::Fn, _)) => Ok(Item::FunctionDeclaration(parse_fn_declaration(parser)?)),
        Some((Token::Struct, _)) => Ok(parse_struct_declaration(parser)?),
        Some((Token::Error, _)) => Err(ParseError::LexError),
        Some((t, pos)) => Err(ParseError::UnexpectedToken { token: t, at: pos }),
        None => Err(ParseError::UnexpectedEOF),
    }
}
