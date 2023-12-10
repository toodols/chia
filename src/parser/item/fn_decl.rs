use crate::parser::{
    ast::{FunctionDeclaration, Parameters, TypeExpr},
    lexer::Token,
    ParseError, Parser,
};

impl Parser<'_> {
    fn parse_fn_params(&mut self) -> Result<Parameters, ParseError> {
        self.expect_token(Token::LParen)?;
        let mut params = Parameters::new();
        while self.peek_token() != Some(Token::RParen) {
            let pat = self.parse_pattern()?;
            self.expect_token(Token::Colon)?;
            let param_type = self.parse_type()?;
            params.0.push((pat, param_type));
            if self.peek_token() == Some(Token::Comma) {
                self.next_token();
            }
        }
        self.expect_token(Token::RParen)?;
        Ok(params)
    }

    pub(super) fn parse_fn_declaration(&mut self) -> Result<FunctionDeclaration, ParseError> {
        self.expect_token(Token::Fn)?;
        let name = self.expect_token(Token::Identifier)?.to_owned();

        let parameters = match self.peek_token() {
            Some(Token::LParen) => self.parse_fn_params()?,

            // Allows `fn foo {}` syntax in contrast to `fn foo() {}` always required
            // Some(Token::Arrow) => vec![],
            // Some(Token::LBrace) => vec![],
            _ => {
                return Err(ParseError::Unknown);
            }
        };
        let return_type = match self.peek_token() {
            Some(Token::Arrow) => {
                self.expect_token(Token::Arrow)?;
                self.parse_type()?
            }
            Some(Token::LBrace) => TypeExpr::Unit,
            _ => {
                return Err(ParseError::Unknown);
            }
        };

        let body = self.parse_block()?;
        Ok(FunctionDeclaration {
            id: self.node_id(),
            span: name.into(),
            parameters,
            body,
            return_type,
        })
    }
}
