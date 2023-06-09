use crate::parser::{Parser, ast::{Path, SymbolName}, ParseError, lexer::Token};

pub(in crate::parser) fn parse_expr_path(parser: &mut Parser) -> Result<Path, ParseError> {
    let mut path = Vec::new();
    if parser.peek_token() == Some(Token::Identifier) {
        parser.next_token();
        path.push(parser.slice_token().to_owned());
    }
    while parser.peek_token() == Some(Token::Scope) {
        parser.next_token();
        parser.expect_token(Token::Identifier)?;
        path.push(parser.slice_token().to_owned());
    }
    Ok(Path(path.into_iter().map(|s| SymbolName::External(s)).collect()))
}