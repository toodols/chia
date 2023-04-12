pub mod ast;
mod lexer;
use ast::*;
use lexer::Token;
use logos::Logos;

use crate::typecheck::SymbolName;

struct Tokens<'a> {
    tokens: logos::Lexer<'a, Token>,
    peeked: Option<Token>,
}

impl<'a> Tokens<'a> {
    fn new<'source>(source: &'source str) -> Tokens<'source> {
        let tokens = Token::lexer(source);
        Tokens {
            tokens,
            peeked: None,
        }
    }
    fn peek_token(&mut self) -> Option<Token> {
        if self.peeked == None {
            if let Some(token) = self.tokens.next() {
                // weird
                return Some(*self.peeked.insert(token));
            }
        }
        self.peeked
    }
    fn next_token(&mut self) -> Option<Token> {
        let token = self.peeked.take();
        if token == None {
            self.tokens.next()
        } else {
            token
        }
    }
    /// Expects the next token to be {token} and errors otherwise;
    fn expect_token(&mut self, token: Token) -> Result<&str, ParseError> {
        // thank you, copilot
        let t = match self.next_token() {
            Some(Token::Error) => Err(ParseError::LexError),
            Some(t) if t == token => Ok(self.tokens.slice()),
            Some(t) => Err(ParseError::UnexpectedToken(t)),
            None => Err(ParseError::UnexpectedEOF),
        };
        t
    }
    fn slice_token(&self) -> &str {
        self.tokens.slice()
    }
    // fn is(&mut self, func: impl FnOnce(Token) -> bool) -> Result<(Token, &str), ParseError> {
    //     match self.next() {
    //         Some(Token::Error) => Err(ParseError::LexError),
    //         Some(t) if func(t) => Ok((t, self.tokens.slice())),
    //         Some(t) => Err(ParseError::UnexpectedToken(t)),
    //         None => Err(ParseError::UnexpectedEOF),
    //     }
    // }
}

#[derive(Debug)]
pub enum ParseError {
    Unknown,
    LexError,
    UnexpectedEOF,
    UnexpectedToken(Token),
    Message(String),
}

struct Parser<'a> {
    node_id_counter: usize,
    tokens: Tokens<'a>,
}

impl<'a> Parser<'a> {
    fn next_token(&mut self) -> Option<Token> {
        self.tokens.next_token()
    }
    fn peek_token(&mut self) -> Option<Token> {
        self.tokens.peek_token()
    }
    fn slice_token(&self) -> &str {
        self.tokens.slice_token()
    }
    fn expect_token(&mut self, token: Token) -> Result<&str, ParseError> {
        self.tokens.expect_token(token)
    }
    fn new(source: &'a str) -> Parser<'a> {
        Parser {
            node_id_counter: 0,
            tokens: Tokens::new(source),
        }
    }
    fn node_id(&mut self) -> usize {
        let id = self.node_id_counter;
        self.node_id_counter += 1;
        id
    }
    fn parse_literal(&mut self) -> Result<Literal, ParseError> {
        match self.next_token() {
            Some(Token::Number) => Ok(Literal::Number(self.slice_token().parse().unwrap())),
            Some(Token::True) => Ok(Literal::Boolean(true)),
            Some(Token::False) => Ok(Literal::Boolean(false)),
            Some(Token::String) => {
                // tokens.slice() will have quotes
                let text = self.slice_token();
                Ok(Literal::String(text[1..text.len() - 1].to_string()))
            }
            Some(Token::Identifier) => Ok(Literal::Identifier(SymbolName::External(self.slice_token().to_owned()))),
            Some(Token::Error) => Err(ParseError::LexError),
            Some(Token::LBracket) => {
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
                    Some(t) => Err(ParseError::UnexpectedToken(t)),
                    None => Err(ParseError::UnexpectedEOF),
                }
            }
            Some(t) => Err(ParseError::UnexpectedToken(t)),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn parse_fn_call_body(&mut self) -> Result<Vec<Expression>, ParseError> {
        self.expect_token(Token::LParen)?;
        let mut args = Vec::new();
        loop {
            match self.peek_token() {
                Some(Token::RParen) => {
                    break;
                }
                Some(Token::Error) => return Err(ParseError::LexError),
                Some(t) => {
                    args.push(self.parse_expression()?);
                    match self.peek_token() {
                        Some(Token::Comma) => {
                            self.next_token();
                        }
                        Some(Token::RParen) => {
                            break;
                        }
                        Some(t) => return Err(ParseError::UnexpectedToken(t)),
                        None => return Err(ParseError::UnexpectedEOF),
                    };
                }
                None => return Err(ParseError::UnexpectedEOF),
            }
        }
        self.expect_token(Token::RParen)?;
        Ok(args)
    }

    fn parse_expression_postfix(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_atomic_expression()?;
        loop {
            match self.peek_token() {
                Some(Token::LParen) => {
                    expr = Expression::FunctionCall(FunctionCall {
                        value: Box::new(expr),
                        arguments: self.parse_fn_call_body()?,
                    });
                }
                Some(Token::LBracket) => {
                    self.next_token();
                    expr = Expression::Index(Index {
                        value: Box::new(expr),
                        index: Box::new(self.parse_expression()?),
                    });
                    self.expect_token(Token::RBracket)?;
                }
                Some(Token::Error) => return Err(ParseError::LexError),
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
                Ok(Expression::UnaryOperation(UnaryOperation {
                    operator: UnaryOperator::Negate,
                    value: Box::new(self.parse_expression_prefix()?),
                }))
            }
            Some(_) => self.parse_expression_postfix(),
            None => Err(ParseError::UnexpectedEOF),
        }
    }
    fn parse_path(&mut self) -> Result<Path, ParseError> {
        let mut path = Vec::new();
        if self.peek_token() == Some(Token::Identifier) {
            self.next_token();
            path.push(self.slice_token().to_owned());
        }
        while self.peek_token() == Some(Token::Scope) {
            self.next_token();
            self.expect_token(Token::Identifier)?;
            path.push(self.slice_token().to_owned());
        }
        Ok(Path(path))
    }
    fn parse_macro_invocation(&mut self) -> Result<Vec<Token>, ParseError> {
        // very simple. all parentheses must be closed, all brackets must be closed, and all braces must be closed
        enum OpenToken {
            Brace,
            Paren,
            Bracket
        }
        impl From<Token> for OpenToken {
            fn from(t: Token) -> Self {
                match t {
                    Token::LBrace | Token::RBrace => OpenToken::Brace,
                    Token::LParen | Token::RParen => OpenToken::Paren,
                    Token::LBracket | Token::RBracket => OpenToken::Bracket,
                    _ => panic!("Invalid token for OpenToken")
                }
            }
        }
        let mut stack: Vec<Token> = Vec::new();
        let mut matched: Vec<Token> = Vec::new();
        loop {
            let tok = self.next_token().ok_or(ParseError::UnexpectedEOF)?;
            match tok {
                Token::LParen | Token::LBracket | Token::LBrace => {
                    stack.push(tok.into());
                }
                Token::RParen | Token::RBracket | Token::RBrace => {
                    let open = stack.pop().ok_or(ParseError::UnexpectedToken(tok))?;
                    if open != tok.into() {
                        return Err(ParseError::UnexpectedToken(tok));
                    }
                }
                Token::Error => return Err(ParseError::LexError),
                _ => {}
            }
            matched.push(tok);
            if stack.len() == 0 {
                break;
            }
        }
        Ok(matched)
    }

    // Basically expressions that don't rely on operators / precedence and are generally wrapped together nicely
    fn parse_atomic_expression(&mut self) -> Result<Expression, ParseError> {
        match self.peek_token() {
            Some(t) => match t {
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
                        _ => Expression::Literal(Literal::Unit),
                    });
                    match t {
                        Token::Break => Ok(Expression::Break(inner)),
                        Token::Return => Ok(Expression::Return(inner)),
                        _ => unreachable!(),
                    }
                }
                Token::If => Ok(Expression::IfExpression(self.parse_if_expr()?)),
                Token::Number
                | Token::True
                | Token::False
                | Token::String
                | Token::Identifier
                | Token::LBracket => {
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
                _ => Err(ParseError::UnexpectedToken(t)),
            },
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
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

        while operators.len() > 0 {
            let mut op_index = 0usize;
            for (i, op) in operators.iter().enumerate() {
                // wow that looks inefficient as hell
                let n_precedence = op.precedence();
                let o_precedence = operators[op_index].precedence();
                if n_precedence.1 < o_precedence.1 {
                    op_index = i;
                } else if n_precedence == o_precedence
                    && n_precedence.0 == Associativity::RightToLeft
                {
                    op_index = i;
                }
            }
            let left = expressions.remove(op_index);
            let right = expressions.remove(op_index);
            expressions.insert(
                op_index,
                Expression::BinaryOperation(BinaryOperation {
                    operator: operators.remove(op_index),
                    left: Box::new(left),
                    right: Box::new(right),
                }),
            );
        }
        Ok(expressions.pop().unwrap())
    }

    fn parse_if_expr(&mut self) -> Result<IfExpression, ParseError> {
        self.expect_token(Token::If)?;
        let condition = self.parse_expression()?;
        let body = Box::new(Expression::Block(self.parse_block()?));
        let else_body = if self.peek_token() == Some(Token::Else) {
            self.next_token();
            if self.peek_token() == Some(Token::If) {
                Some(Box::new(Expression::IfExpression(self.parse_if_expr()?)))
            } else {
                Some(Box::new(Expression::Block(self.parse_block()?)))
            }
        } else {
            None
        };
        Ok(IfExpression {
            condition: Box::new(condition),
            body,
            else_body,
        })
    }

    fn parse_let_declaration(&mut self) -> Result<LetDeclaration, ParseError> {
        self.expect_token(Token::Let)?;
        let pat = self.parse_pattern()?;

        if self.peek_token() == Some(Token::Assign) {
            self.expect_token(Token::Assign)?;
            let value = self.parse_expression()?;
            if !value.terminating() {
                self.expect_token(Token::Semicolon)?;
            }
            Ok(LetDeclaration {
                pat,
                value: Some(value),
            })
        } else {
            self.expect_token(Token::Semicolon)?;
            Ok(LetDeclaration { pat, value: None })
        }
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        let ident = self.expect_token(Token::Identifier)?;
        Ok(Pattern::Ident(ident.to_owned()))
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        self.expect_token(Token::LBrace)?;
        let mut statements = Vec::new();
        let mut does_return = false;
        let node_id = self.node_id();
        struct IterRes {
            statement: Statement,
            can_cont: bool,
            did_close: bool,
        }
        if self.peek_token() != Some(Token::RBrace) {
            loop {
                // cont is whether another expression is allowed
                /*
                    if a {

                    }
                    // this is allowed because cont is true
                    1
                */
                /*
                    1
                    2 // not allowed; cont is false
                */
                /*
                1;
                2 // allowed; cont is true
                */
                let IterRes {
                    statement,
                    can_cont,
                    did_close,
                } = match self.peek_token() {
                    Some(Token::Let) => Ok(IterRes {
                        statement: Statement::LetDeclaration(self.parse_let_declaration()?),
                        can_cont: true,
                        did_close: true,
                    }),
                    Some(Token::Semicolon) => {
                        self.next_token();
                        Ok(IterRes {
                            statement: Statement::Empty,
                            can_cont: true,
                            did_close: true,
                        })
                    }
                    Some(Token::Error) => Err(ParseError::LexError),
                    Some(t) => {
                        let expr = self.parse_expression()?;
                        let mut terminating = expr.terminating();
                        let mut did_close = false;
                        if self.peek_token() == Some(Token::Semicolon) {
                            did_close = true;
                            terminating = true;
                            self.next_token();
                        }
                        Ok(IterRes {
                            statement: Statement::Expression(expr),
                            can_cont: terminating,
                            did_close,
                        })
                    }
                    None => Err(ParseError::UnexpectedEOF),
                }?;

                statements.push(statement);
                if !did_close {
                    does_return = true;
                }

                if self.peek_token() == Some(Token::RBrace) {
                    break;
                } else if !can_cont {
                    return Err(ParseError::Message("Expected Semicolon".to_owned()));
                }
            }
        }
        self.expect_token(Token::RBrace)?;
        Ok(Block {
            statements,
            does_return,
            node_id,
        })
    }

    fn parse_fn_params(&mut self) -> Result<Parameters, ParseError> {
        self.expect_token(Token::LParen)?;
        let mut params = Vec::new();
        while self.peek_token() != Some(Token::RParen) {
            let pat = self.parse_pattern()?;
            self.expect_token(Token::Colon)?;
            let param_type = self.parse_type()?;
            params.push((pat, param_type));
            if self.peek_token() == Some(Token::Comma) {
                self.next_token();
            }
        }
        self.expect_token(Token::RParen)?;
        Ok(params)
    }

    fn parse_loops(&mut self) -> Result<Expression, ParseError> {
        match self.peek_token() {
            // Some(Token::While) => Ok(Loop::While(self.parse_while_loop()?)),
            Some(Token::For) => Ok(Expression::ForLoop(self.parse_for_loop()?)),
            _ => Err(ParseError::Message("Expected While or For".to_owned())),
        }
    }

    fn parse_for_loop(&mut self) -> Result<ForLoop, ParseError> {
        self.expect_token(Token::For)?;
        let pat = self.parse_pattern()?;
        self.expect_token(Token::In)?;
        let iter = Box::new(self.parse_expression()?);
        let block = self.parse_block()?;
        Ok(ForLoop {
            pat,
            iter,
            body: block,
        })
    }

    fn parse_type(&mut self) -> Result<TypeExpr, ParseError> {
        match self.next_token() {
            Some(Token::Identifier) => Ok(TypeExpr::Identifier(self.slice_token().to_owned())),
            Some(Token::LParen) => {
                if self.peek_token() == Some(Token::RParen) {
                    self.next_token();
                    return Ok(TypeExpr::Unit);
                }
                let mut types = Vec::new();
                types.push(self.parse_type()?);
                while self.peek_token() == Some(Token::Comma) {
                    self.next_token();
                    types.push(self.parse_type()?);
                }
                self.expect_token(Token::RParen)?;
                Ok(TypeExpr::Tuple(types))
            }
            Some(Token::Error) => Err(ParseError::LexError),
            Some(t) => Err(ParseError::UnexpectedToken(t)),
            None => Err(ParseError::UnexpectedEOF),
        }
    }
    fn parse_item(&mut self) -> Result<Item, ParseError> {
        match self.peek_token() {
            Some(Token::Fn) => Ok(Item::FunctionDeclaration(self.parse_fn_declaration()?)),
            Some(Token::Error) => Err(ParseError::LexError),
            Some(t) => Err(ParseError::UnexpectedToken(t)),
            None => Err(ParseError::UnexpectedEOF),
        }
    }
    fn parse_fn_declaration(&mut self) -> Result<FunctionDeclaration, ParseError> {
        self.expect_token(Token::Fn)?;
        let name = self.expect_token(Token::Identifier)?.to_owned();

        let params = match self.peek_token() {
            Some(Token::LParen) => self.parse_fn_params()?,
            // Some(Token::Arrow) => vec![], // Allows `fn foo {}` syntax in contrast to `fn foo() {}` always required
            Some(Token::LBrace) => vec![],
            _ => {
                return Err(ParseError::Unknown);
            }
        };

        let return_type: TypeExpr = match self.peek_token() {
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
            name: SymbolName::External(name),
            parameters: params,
            body,
            return_type,
        })
    }

    fn parse_program(&mut self) -> Result<Program, ParseError> {
        let node_id = self.node_id();
        let mut items = Vec::new();
        while let Some(token) = self.peek_token() {
            items.push(self.parse_item()?);
        }
        println!("finished parsing");
        Ok(Program { items, node_id })
    }
}

pub fn parse(source: &str) -> Result<Program, ParseError> {
    let mut parser = Parser::new(source);
    parser.parse_program()
}
