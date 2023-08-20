use super::{
    ast::{
        Associativity, BinaryOperation, BinaryOperator, Expression, FunctionCall, Index, Literal,
        UnaryOperation, UnaryOperator,
    },
    lexer::Token,
    ParseError, Parser, Sources,
};

pub mod block;
mod if_expr;
mod literal;
mod loops;
pub mod path;

impl<T: Sources> Parser<'_, T> {
    // Basically expressions that don't rely on operators / precedence and are generally wrapped together nicely
    fn parse_atomic_expression(&mut self) -> Result<Expression, ParseError> {
        match self.peek_token_with_pos() {
            Some((t, pos)) => match t {
                Token::For | Token::While | Token::Loop => self.parse_loops(),
                t @ (Token::Break | Token::Return) => {
                    self.next_token();
                    // if self.peek_token() == Some(Token::Semicolon) {
                    //     // self.next_token();
                    //     return Ok(Expression::Return(Box::new(Expression::Literal(
                    //         Literal::Unit
                    //     ))));
                    // }

                    let inner = Box::new(match self.peek_token() {
                        Some(t) if t.is_expression_start() => {
                            let expr = self.parse_expression()?;
                            expr
                        }
                        _ => Expression::Literal(self.node(Literal::Unit)),
                    });
                    match t {
                        Token::Break => Ok(Expression::Break(inner)),
                        Token::Return => Ok(Expression::Return(inner)),
                        _ => unreachable!(),
                    }
                }
                Token::If => Ok(Expression::IfExpression(self.parse_if_expr()?)),
                Token::Identifier => Ok(Expression::Path(self.parse_expr_path()?)),
                Token::Number | Token::True | Token::False | Token::String | Token::LBracket => {
                    let literal = self.parse_literal()?;
                    Ok(Expression::Literal(literal))
                }
                Token::LParen => {
                    // a * (b + c)
                    // b + c is evaluated first
                    self.expect_token(Token::LParen)?;
                    let expression = self.parse_expression()?;
                    self.expect_token(Token::RParen)?;
                    Ok(expression)
                }
                _ => Err(ParseError::UnexpectedToken { token: t, at: pos }),
            },
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    pub(in crate::parser) fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        let mut expressions: Vec<Expression> = vec![];
        let mut operators: Vec<BinaryOperator> = vec![];

        expressions.push(self.parse_expression_prefix()?);
        loop {
            let token = self.peek_token();
            if let Some(token) = token {
                if token.is_binary_operator() {
                    let op: BinaryOperator = self.next_token().unwrap().into();
                    operators.push(op);
                    expressions.push(self.parse_expression_prefix()?);
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
                    || (n_precedence == o_precedence
                        && n_precedence.0 == Associativity::RightToLeft)
                {
                    op_index = i;
                }
            }
            let left = expressions.remove(op_index);
            let right = expressions.remove(op_index);
            expressions.insert(
                op_index,
                Expression::BinaryOperation(self.node(BinaryOperation {
                    operator: operators.remove(op_index),
                    left: Box::new(left),
                    right: Box::new(right),
                })),
            );
        }
        Ok(expressions.pop().unwrap())
    }

    fn parse_expression_postfix(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_atomic_expression()?;
        loop {
            match self.peek_token() {
                Some(Token::LParen) => {
                    let arguments = self.parse_fn_call_body()?;
                    expr = Expression::FunctionCall(self.node(FunctionCall {
                        value: Box::new(expr),
                        arguments,
                    }));
                }
                Some(Token::LBracket) => {
                    self.next_token();
                    let index_expr = self.parse_expression()?;
                    expr = Expression::Index(self.node(Index {
                        value: Box::new(expr),
                        index: Box::new(index_expr),
                    }));
                    self.expect_token(Token::RBracket)?;
                }
                Some(_) => break,
                None => break,
            };
        }
        Ok(expr)
    }

    fn parse_expression_prefix(&mut self) -> Result<Expression, ParseError> {
        match self.peek_token() {
            Some(Token::Sub) => {
                self.next_token();
                let inner_expr = self.parse_expression_prefix()?;
                Ok(Expression::UnaryOperation(self.node(UnaryOperation {
                    operator: UnaryOperator::Negate,
                    value: Box::new(inner_expr),
                })))
            }
            Some(_) => self.parse_expression_postfix(),
            None => Err(ParseError::UnexpectedEOF),
        }
    }
}
