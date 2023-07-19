use crate::parser::{
    ast::{FunctionDeclaration, Parameters, TypeExpr, Node},
    expr::block::parse_block,
    lexer::Token,
    parse_pattern, parse_type, ParseError, Parser, SymbolName,
};

fn parse_fn_params(parser: &mut Parser) -> Result<Parameters, ParseError> {
    parser.expect_token(Token::LParen)?;
    let mut params = Parameters::new();
    while parser.peek_token() != Some(Token::RParen) {
        let pat = parse_pattern(parser)?;
        parser.expect_token(Token::Colon)?;
        let param_type = parse_type(parser)?;
        params.0.push((pat, param_type));
        if parser.peek_token() == Some(Token::Comma) {
            parser.next_token();
        }
    }
    parser.expect_token(Token::RParen)?;
    Ok(params)
}

pub(super) fn parse_fn_declaration(parser: &mut Parser) -> Result<Node<FunctionDeclaration>, ParseError> {
    parser.expect_token(Token::Fn)?;
    let name = parser.expect_token(Token::Identifier)?.to_owned();

    let parameters = match parser.peek_token() {
        Some(Token::LParen) => parse_fn_params(parser)?,

        // Allows `fn foo {}` syntax in contrast to `fn foo() {}` always required
        // Some(Token::Arrow) => vec![],
        // Some(Token::LBrace) => vec![],
        _ => {
            return Err(ParseError::Unknown);
        }
    };
    let parameters = parser.node(parameters);

    let return_type = match parser.peek_token() {
        Some(Token::Arrow) => {
            parser.expect_token(Token::Arrow)?;
            parse_type(parser)?
        }
        Some(Token::LBrace) => parser.node(TypeExpr::Unit),
        _ => {
            return Err(ParseError::Unknown);
        }
    };

    let body = parse_block(parser)?;
    Ok(parser.node(FunctionDeclaration {
        name: SymbolName::External(name),
        parameters,
        body,
        return_type,
    }))
}
