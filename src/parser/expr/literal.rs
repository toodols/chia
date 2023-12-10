use crate::parser::{ast::Literal, lexer::Token, ParseError, Parser, Sources};

impl Parser<'_> {
    pub(in crate::parser) fn parse_literal(&mut self) -> Result<Literal, ParseError> {
        let (token, pos) = self
            .next_token_with_pos()
            .ok_or(ParseError::UnexpectedEOF)?; // TODO: Better error handling (e.g. UnexpectedEOF
        match token {
            Token::Number => Ok(Literal::Number(self.slice_token().parse().unwrap())),
            Token::True => Ok(Literal::Boolean(true)),
            Token::False => Ok(Literal::Boolean(false)),
            Token::String => {
                // tokens.slice() will have quotes
                let text = self.slice_token();
                Ok(Literal::String(text[1..text.len() - 1].to_string()))
            }
            Token::LBracket => {
                let initial = self.parse_expression()?;
                match self.next_token() {
                    Some(Token::Semicolon) => {
                        let len = self.expect_token(Token::Number)?.parse().unwrap();
                        self.expect_token(Token::RBracket)?;
                        Ok(Literal::ArraySized(Box::new(initial), len))
                    }
                    Some(Token::Comma) => {
                        let mut t = vec![initial, self.parse_expression()?];
                        while self.peek_token() == Some(Token::Comma) {
                            self.next_token();
                            t.push(self.parse_expression()?);
                        }
                        self.expect_token(Token::RBracket)?;
                        Ok(Literal::Array(t))
                    }
                    Some(Token::RBracket) => Ok(Literal::Array(vec![initial])),
                    Some(t) => Err(ParseError::UnexpectedToken { token: t, at: pos }),
                    None => Err(ParseError::UnexpectedEOF),
                }
            }
            t => Err(ParseError::UnexpectedToken { token: t, at: pos }),
        }
    }
}
