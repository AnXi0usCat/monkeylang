use std::{io, mem};
use crate::lexer::Lexer;
use crate::token::Token;
use crate::ast::{Program, Statement};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            cur_token: Token::Illegal,
            peek_token: Token::Illegal
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    fn next_token(&mut self) {
        self.cur_token = mem::replace(
            &mut self.peek_token, self.lexer.next_token()
        )
    }

    fn parse_program(&mut self) -> Program {
        let mut statements = vec![];

        while self.cur_token != Token::Eof {
            match self.parse_statement() {
                Ok(res) => statements.push(res),
                Err(err) => panic!("failed to parse statement {}", err)
            }
            self.next_token();
        }

        Program { statements }
    }
    
    fn parse_statement(&mut self) -> Result<Statement, E> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            _ => Err("")
        }
    }

    fn parse_let_statement(&mut self) -> Statement::Let {

    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[text]
    fn let_statement() {

    }
}