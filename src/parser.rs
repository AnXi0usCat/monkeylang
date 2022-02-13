use crate::ast::{Expression, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use std::mem;

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         //+
    Product,     //*
    Prefix,      //-Xor!X
    Call,        // myFunction(X)
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            cur_token: Token::Illegal,
            peek_token: Token::Illegal,
            errors: vec![],
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    fn next_token(&mut self) {
        self.cur_token = mem::replace(&mut self.peek_token, self.lexer.next_token())
    }

    fn parse_program(&mut self) -> Program {
        let mut statements = vec![];

        while self.cur_token != Token::Eof {
            match self.parse_statement() {
                Ok(res) => statements.push(res),
                Err(err) => self.errors.push(err),
            }
            self.next_token();
        }

        Program { statements }
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        let name;

        // current token is whatever comes after 'let'
        if let Token::Ident(ident) = self.peek_token.clone() {
            name = ident;
            self.next_token();
        } else {
            return Err(format!(
                "Unexpected token. Expected identifier, received {} instead.",
                self.peek_token.to_string()
            ));
        }
        // current token is '='
        self.expect_peek(Token::Assign)?;

        self.next_token();
        // current token is expression after '='
        // skip the Expression part for now
        let value = Expression::Identifier("PLACEHOLDER\n".to_string());

        while self.peek_token == Token::Semicolon {
            // current token is ';'
            self.next_token();
        }

        Ok(Statement::Let(name, value))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.next_token();

        if self.cur_token == Token::Semicolon {
            return Ok(Statement::Return(None));
        }

        // skip the Expression part for now
        let value = Expression::Identifier("PLACEHOLDER\n".to_string());
        while self.peek_token == Token::Semicolon {
            // current token is ';'
            self.next_token();
        }
        Ok(Statement::Return(Some(value)))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        while self.peek_token == Token::Semicolon {
            // current token is ';'
            self.next_token();
        }
        Ok(Statement::Expression(expression))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, String> {
        let infix = self.prefix_parse_fn();
        infix
    }

    fn parse_identifier(&mut self) -> Result<Expression, String> {
        if let Token::Ident(ident) = self.cur_token.clone() {
            Ok(Expression::Identifier(ident))
        } else {
            Err(format!("Expected identifier, found {}", self.cur_token))
        }
    }

    fn prefix_parse_fn(&mut self) -> Result<Expression, String> {
        match self.cur_token {
            Token::Ident(_) => self.parse_identifier(),
            _ => Err(format!("Expected a prefix token, got: {}", self.cur_token)),
        }
    }

    fn expect_peek(&mut self, expected: Token) -> Result<(), String> {
        if self.peek_token != expected {
            return Err(format!(
                "Expected {}, got {} instead",
                expected, self.peek_token
            ));
        }
        self.next_token();
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use std::vec;

    #[test]
    fn let_statement() {
        let input = "
            let x = 5;
            let y = 1023;
            let foobar = x + y;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(
            program.statements,
            vec![
                Statement::Let(
                    String::from("x"),
                    Expression::Identifier(String::from("PLACEHOLDER\n"))
                ),
                Statement::Let(
                    String::from("y"),
                    Expression::Identifier(String::from("PLACEHOLDER\n"))
                ),
                Statement::Let(
                    String::from("foobar"),
                    Expression::Identifier(String::from("PLACEHOLDER\n"))
                ),
            ]
        );
        assert_eq!(parser.errors, Vec::<String>::new());
    }

    #[test]
    fn return_statement() {
        let input = " 
            return;
            return;
            return;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(
            program.statements,
            vec![
                Statement::Return(None),
                Statement::Return(None),
                Statement::Return(None)
            ]
        );
        assert_eq!(parser.errors, Vec::<String>::new());
    }

    #[test]
    fn identifier_expression() {
        // GIVEN
        let input = "foobar;";

        // WHEN
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        // THEN
        assert_eq!(parser.errors, Vec::<String>::new());
    }
}
