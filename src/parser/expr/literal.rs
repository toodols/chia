use crate::{
	parser::{ast::Literal, lexer::Token, ParseError, Parser, SymbolName},
};

use super::parse_expression;

pub(in crate::parser) fn parse_literal(parser: &mut Parser) -> Result<Literal, ParseError> {
	match parser.next_token() {
		Some(Token::Number) => Ok(Literal::Number(parser.slice_token().parse().unwrap())),
		Some(Token::True) => Ok(Literal::Boolean(true)),
		Some(Token::False) => Ok(Literal::Boolean(false)),
		Some(Token::String) => {
			// tokens.slice() will have quotes
			let text = parser.slice_token();
			Ok(Literal::String(text[1..text.len() - 1].to_string()))
		}
		Some(Token::Identifier) => Ok(Literal::Identifier(SymbolName::External(
			parser.slice_token().to_owned(),
		))),
		Some(Token::Error) => Err(ParseError::LexError),
		Some(Token::LBracket) => {
			let initial = parse_expression(parser)?;
			match parser.next_token() {
				Some(Token::Semicolon) => {
					let len = parser.expect_token(Token::Number)?.parse().unwrap();
					parser.expect_token(Token::RBracket)?;
					Ok(Literal::ArraySized(Box::new(initial), len))
				}
				Some(Token::Comma) => {
					let mut t = vec![initial, parse_expression(parser)?];
					while parser.peek_token() == Some(Token::Comma) {
						parser.next_token();
						t.push(parse_expression(parser)?);
					}
					parser.expect_token(Token::RBracket)?;
					Ok(Literal::Array(t))
				}
				Some(Token::RBracket) => Ok(Literal::Array(vec![initial])),
				Some(t) => Err(ParseError::UnexpectedToken(t)),
				None => Err(ParseError::UnexpectedEOF),
			}
		}
		Some(t) => Err(ParseError::UnexpectedToken(t)),
		None => Err(ParseError::UnexpectedEOF),
	}
}
