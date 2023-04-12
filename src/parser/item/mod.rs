use self::fn_decl::parse_fn_declaration;

use super::{ast::Item, lexer::Token, ParseError, Parser};
mod fn_decl;

pub(in crate::parser) fn parse_item(parser: &mut Parser) -> Result<Item, ParseError> {
    match parser.peek_token() {
        Some(Token::Fn) => Ok(Item::FunctionDeclaration(parse_fn_declaration(parser)?)),
        Some(Token::Error) => Err(ParseError::LexError),
        Some(t) => Err(ParseError::UnexpectedToken(t)),
        None => Err(ParseError::UnexpectedEOF),
    }
}
