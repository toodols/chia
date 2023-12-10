use crate::parser::{
    ast::{Block, Statement},
    lexer::Token,
    ParseError, Parser, Sources,
};

impl Parser<'_> {
    pub(in crate::parser) fn parse_block(&mut self) -> Result<Block, ParseError> {
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
                    Some(_) => {
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
                if !did_close && can_cont {
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
            id: self.node_id(),
            statements,
            does_return,
        })
    }
}
