use self::if_expr::parse_if_expr;
use self::path::parse_expr_path;
use self::{literal::parse_literal, loops::parse_loops};

use super::ast::Node;
use super::{
    ast::{
        Associativity, BinaryOperation, BinaryOperator, Expression, FunctionCall, Index, Literal,
        UnaryOperation, UnaryOperator,
    },
    lexer::Token,
    parse_fn_call_body, ParseError, Parser,
};

pub mod block;
mod if_expr;
mod literal;
mod loops;
pub mod path;

// Basically expressions that don't rely on operators / precedence and are generally wrapped together nicely
fn parse_atomic_expression(parser: &mut Parser) -> Result<Expression, ParseError> {
    match parser.peek_token_with_pos() {
        Some((t, pos)) => match t {
            Token::For | Token::While | Token::Loop => parse_loops(parser),
            t @ (Token::Break | Token::Return) => {
                parser.next_token();
                // if self.peek_token() == Some(Token::Semicolon) {
                //     // self.next_token();
                //     return Ok(Expression::Return(Box::new(Expression::Literal(
                //         Literal::Unit
                //     ))));
                // }

                let inner = Box::new(match parser.peek_token() {
                    Some(t) if t.is_expression_start() => {
                        let expr = parse_expression(parser)?;
                        expr
                    }
                    _ => Expression::Literal(parser.node(Literal::Unit)),
                });
                match t {
                    Token::Break => Ok(Expression::Break(inner)),
                    Token::Return => Ok(Expression::Return(inner)),
                    _ => unreachable!(),
                }
            }
            Token::If => Ok(Expression::IfExpression(parse_if_expr(parser)?)),
            Token::Identifier => Ok(Expression::Path(parse_expr_path(parser)?)),
            Token::Number | Token::True | Token::False | Token::String | Token::LBracket => {
                let literal = parse_literal(parser)?;
                Ok(Expression::Literal(literal))
            }
            Token::LParen => {
                // a * (b + c)
                // b + c is evaluated first
                parser.expect_token(Token::LParen)?;
                let expression = parse_expression(parser)?;
                parser.expect_token(Token::RParen)?;
                Ok(expression)
            }
            _ => Err(ParseError::UnexpectedToken { token: t, at: pos }),
        },
        None => Err(ParseError::UnexpectedEOF),
    }
}

pub(in crate::parser) fn parse_expression(parser: &mut Parser) -> Result<Expression, ParseError> {
    let mut expressions: Vec<Expression> = vec![];
    let mut operators: Vec<BinaryOperator> = vec![];

    expressions.push(parse_expression_prefix(parser)?);
    loop {
        let token = parser.peek_token();
        if let Some(token) = token {
            if token.is_binary_operator() {
                let op: BinaryOperator = parser.next_token().unwrap().into();
                operators.push(op);
                expressions.push(parse_expression_prefix(parser)?);
            } else {
                break;
            }
        } else {
            break;
        }
    }

    while !operators.is_empty() {
        let mut op_index = 0usize;
        for (i, op) in operators.iter().enumerate() {
            // wow that looks inefficient as hell
            let n_precedence = op.precedence();
            let o_precedence = operators[op_index].precedence();
            if n_precedence.1 < o_precedence.1
                || (n_precedence == o_precedence && n_precedence.0 == Associativity::RightToLeft)
            {
                op_index = i;
            }
        }
        let left = expressions.remove(op_index);
        let right = expressions.remove(op_index);
        expressions.insert(
            op_index,
            Expression::BinaryOperation(parser.node(BinaryOperation {
                operator: operators.remove(op_index),
                left: Box::new(left),
                right: Box::new(right),
            })),
        );
    }
    Ok(expressions.pop().unwrap())
}

fn parse_expression_postfix(parser: &mut Parser) -> Result<Expression, ParseError> {
    let mut expr = parse_atomic_expression(parser)?;
    loop {
        match parser.peek_token() {
            Some(Token::LParen) => {
                let arguments = parse_fn_call_body(parser)?;
                expr = Expression::FunctionCall(parser.node(FunctionCall {
                    value: Box::new(expr),
                    arguments,
                }));
            }
            Some(Token::LBracket) => {
                parser.next_token();
                let index_expr = parse_expression(parser)?;
                expr = Expression::Index(parser.node(Index {
                    value: Box::new(expr),
                    index: Box::new(index_expr),
                }));
                parser.expect_token(Token::RBracket)?;
            }
            Some(Token::Error) => return Err(ParseError::LexError),
            Some(_) => break,
            None => break,
        };
    }
    Ok(expr)
}

fn parse_expression_prefix(parser: &mut Parser) -> Result<Expression, ParseError> {
    match parser.peek_token() {
        Some(Token::Sub) => {
            parser.next_token();
            let inner_expr = parse_expression_prefix(parser)?;
            Ok(Expression::UnaryOperation(parser.node(UnaryOperation {
                operator: UnaryOperator::Negate,
                value: Box::new(inner_expr),
            })))
        }
        Some(_) => parse_expression_postfix(parser),
        None => Err(ParseError::UnexpectedEOF),
    }
}
