use std::mem;
use std::error::Error;
use crate::lexer::Lexer;
use crate::token::Token;
use crate::ast::{Program, Statement, Expression};

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
    
    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            _ => Err("".to_string())
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        let name;

        if let Token::Ident(ident) = self.peek_token.clone() {
            name = ident;
            self.next_token();
        } else {
            return Err(format!("Unexpected token. Expected identifier, received {} instead.",
                                 self.peek_token.to_string()));
        }

        if self.peek_token != Token::Assign {
            return Err(format!("Unexpected token. Expected {}, received {} instead.",
                        Token::Assign, self.peek_token.to_string()));
        }

        self.next_token();

        // skip the Expression part foe now
        let value = Expression::Identifier("".to_string());

        while self.peek_token != Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Let(name, value))
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn let_statement() {
        let input = "
            let x = 5;
            let y = 1023;
            let foobar = x + y;
        ";

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        println!("{}", program);
    }
}