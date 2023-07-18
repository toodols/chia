use crate::parser::{
    ast::{FunctionDeclaration, Parameters, TypeExpr},
    expr::block::parse_block,
    lexer::Token,
    parse_pattern, parse_type, ParseError, Parser, SymbolName,
};

fn parse_fn_params(parser: &mut Parser) -> Result<Parameters, ParseError> {
    parser.expect_token(Token::LParen)?;
    let mut params = Vec::new();
    while parser.peek_token() != Some(Token::RParen) {
        let pat = parse_pattern(parser)?;
        parser.expect_token(Token::Colon)?;
        let param_type = parse_type(parser)?;
        params.push((pat, param_type));
        if parser.peek_token() == Some(Token::Comma) {
            parser.next_token();
        }
    }
    parser.expect_token(Token::RParen)?;
    Ok(params)
}

pub(super) fn parse_fn_declaration(parser: &mut Parser) -> Result<FunctionDeclaration, ParseError> {
    parser.expect_token(Token::Fn)?;
    let name = parser.expect_token(Token::Identifier)?.to_owned();

    let params = match parser.peek_token() {
        Some(Token::LParen) => parse_fn_params(parser)?,

        // Allows `fn foo {}` syntax in contrast to `fn foo() {}` always required
        // Some(Token::Arrow) => vec![],
        // Some(Token::LBrace) => vec![],
        _ => {
            return Err(ParseError::Unknown);
        }
    };

    let return_type: TypeExpr = match parser.peek_token() {
        Some(Token::Arrow) => {
            parser.expect_token(Token::Arrow)?;
            parse_type(parser)?
        }
        Some(Token::LBrace) => TypeExpr::Unit,
        _ => {
            return Err(ParseError::Unknown);
        }
    };

    let body = parse_block(parser)?;
    Ok(FunctionDeclaration {
        name: SymbolName::External(name),
        parameters: params,
        body,
        return_type,
    })
}
