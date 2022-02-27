use crate::ast::Expression::PrefixExpression;
use crate::ast::{Expression, Prefix, Program, Statement};
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

        while self.cur_token != Token::Semicolon {
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
        while self.cur_token != Token::Semicolon {
            // current token is ';'
            self.next_token();
        }
        Ok(Statement::Return(Some(value)))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        while self.cur_token != Token::Semicolon {
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

    fn parse_integer_literal(&mut self) -> Result<Expression, String> {
        if let Token::Int(int_str) = self.cur_token.clone() {
            let int = int_str.to_owned().parse::<i32>().unwrap();
            Ok(Expression::IntegerLiteral(int))
        } else {
            Err(format!("Expected an integer, found {}", self.cur_token))
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, String> {
        // current token should be a prefix (Minus or Bang)
        let token = match self.cur_token {
            Token::Minus => Ok(Prefix::Minus),
            Token::Bang => Ok(Prefix::Bang),
            _ => Err(format!(
                "Expected a `-` or `!` token, got: {} instead",
                self.cur_token
            )),
        }?;
        self.next_token();
        // current token should be the first token of the expression
        let expression = self.parse_expression(Precedence::Prefix)?;

        Ok(PrefixExpression(token, Box::new(expression)))
    }

    fn prefix_parse_fn(&mut self) -> Result<Expression, String> {
        match self.cur_token {
            Token::Ident(_) => self.parse_identifier(),
            Token::Int(_) => self.parse_integer_literal(),
            Token::Minus => self.parse_prefix_expression(),
            Token::Bang => self.parse_prefix_expression(),
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
    use crate::ast::Expression::{Identifier, InfixExpression, IntegerLiteral, PrefixExpression};
    use crate::ast::{Expression, Infix, Prefix, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use std::vec;

    #[test]
    fn let_statement() {
        let input = "
            let x = 5;
            let y = 1023;
            let foobar = x + z;
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
        assert_eq!(
            program.statements,
            vec![Statement::Expression(Expression::Identifier(String::from(
                "foobar"
            )))]
        );
        assert_eq!(parser.errors, Vec::<String>::new());
    }

    #[test]
    fn integer_literal_expression() {
        // GIVEN
        let input = "5;";

        // WHEN
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        // THEN
        assert_eq!(
            program.statements,
            vec![Statement::Expression(Expression::IntegerLiteral(5))]
        );
        assert_eq!(parser.errors, Vec::<String>::new());
    }

    #[test]
    fn prefix_expression() {
        // GIVEN
        let tests = vec![
            (
                "!5;",
                Statement::Expression(PrefixExpression(
                    Prefix::Bang,
                    Box::new(Expression::IntegerLiteral(5)),
                )),
            ),
            (
                "-15;",
                Statement::Expression(PrefixExpression(
                    Prefix::Minus,
                    Box::new(Expression::IntegerLiteral(15)),
                )),
            ),
        ];

        // WHEN
        for (input, pref) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            // THEN
            assert_eq!(program.statements, vec![pref]);
            assert_eq!(parser.errors, Vec::<String>::new());
        }
    }

    #[test]
    fn infix_expression_integer() {
        // GIVEN
        let tests = vec![
            ("5 + 5;", 5, Infix::Plus, 5),
            ("5 - 5;", 5, Infix::Minus, 5),
            ("5 * 5;", 5, Infix::Asterisk, 5),
            ("5 / 5;", 5, Infix::Slash, 5),
            ("5 > 5;", 5, Infix::Gthen, 5),
            ("5 < 5;", 5, Infix::Lthen, 5),
            ("5 == 5;", 5, Infix::Equals, 5),
            ("5 != 5;", 5, Infix::Nequals, 5),
        ];

        for (input, exp1, infix, exp2) in tests {
            // WHEN
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            // THEN
            assert_eq!(
                program.statements,
                vec![Statement::Expression(InfixExpression(
                    Box::new(IntegerLiteral(exp1)),
                    infix,
                    Box::new(IntegerLiteral(exp2))
                ))]
            );
        }
    }
}
