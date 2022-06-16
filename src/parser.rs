use crate::ast::BlockStatement;
use crate::ast::{Expression, Infix, Prefix, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use std::mem;

type PrefixParseFn<'a> = fn(&mut Parser<'a>) -> Result<Expression, String>;
type InfixParseFn<'a> = fn(&mut Parser<'a>, Expression) -> Result<Expression, String>;

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         //+
    Product,     //*
    Prefix,      //-Xor!X
    Call,        // myFunction(X)
    Index,
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
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

    pub fn parse_program(&mut self) -> Program {
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
        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
            // cur_token: ;
        }

        Ok(Statement::Let(name, value))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.next_token();

        if self.cur_token == Token::Semicolon {
            return Ok(Statement::Return(None));
        }
        let value = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token == Token::Semicolon {
            self.next_token();
            // cur_token: ;
        }
        Ok(Statement::Return(Some(value)))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
            // cur_token: ;
        }
        Ok(Statement::Expression(expression))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, String> {
        let mut statements = vec![];
        self.next_token();
        while self.cur_token != Token::Rbrace && self.cur_token != Token::Eof {
            statements.push(self.parse_statement()?);
            self.next_token();
        }
        return Ok(BlockStatement { statements });
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, String> {
        let prefix = self
            .prefix_parse_fn()
            .ok_or_else(|| format!("Expected a prefix token, got: {}", self.peek_token))?;
        let mut left_expr = prefix(self)?;

        while self.peek_token != Token::Semicolon
            && precedence < self.infix_token(&self.peek_token).0
        {
            if let Some(infix) = self.infix_parse_fn() {
                self.next_token();
                left_expr = infix(self, left_expr)?;
            } else {
                return Ok(left_expr);
            };
        }
        Ok(left_expr)
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, String> {
        self.next_token();
        let expression = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(Token::Rparen)?;
        Ok(expression)
    }

    fn parse_if_expression(&mut self) -> Result<Expression, String> {
        self.expect_peek(Token::Lparen)?;
        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(Token::Rparen)?;
        self.expect_peek(Token::Lbrace)?;

        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token == Token::Else {
            self.next_token();
            self.expect_peek(Token::Lbrace)?;
            Some(self.parse_block_statement()?)
        } else {
            None
        };
        Ok(Expression::If(
            Box::new(condition),
            consequence,
            alternative,
        ))
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
            let int = int_str.to_owned().parse::<i64>().unwrap();
            Ok(Expression::IntegerLiteral(int))
        } else {
            Err(format!(
                "Expected an integer, found {} instead",
                self.cur_token
            ))
        }
    }

    fn parse_boolean(&mut self) -> Result<Expression, String> {
        match self.cur_token {
            Token::True => Ok(Expression::Boolean(true)),
            Token::False => Ok(Expression::Boolean(false)),
            _ => Err(format!(
                "Expected a Boolean, got {} instead",
                self.cur_token
            )),
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

        Ok(Expression::PrefixExpression(token, Box::new(expression)))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, String> {
        let (precedence, infix) = self.infix_token(&self.cur_token);
        let infix = infix
            .ok_or_else(|| format!("Excepted an Infix token got {} instead", self.cur_token))?;
        self.next_token();
        let right = self.parse_expression(precedence)?;
        Ok(Expression::InfixExpression(
            Box::new(left),
            infix,
            Box::new(right),
        ))
    }

    fn parse_function_literal(&mut self) -> Result<Expression, String> {
        self.expect_peek(Token::Lparen)?;
        let parameters = self.parse_function_parameters()?;
        self.expect_peek(Token::Lbrace)?;
        let body = self.parse_block_statement()?;
        return Ok(Expression::FunctionLiteral(parameters, body));
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, String> {
        let args = self.parse_expression_list(Token::Rparen)?;
        Ok(Expression::Call(Box::new(function), args))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<String>, String> {
        let mut identifiers: Vec<String> = vec![];

        if self.peek_token == Token::Rparen {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();
        identifiers.push(self.parse_identifier()?.to_string());

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            identifiers.push(self.parse_identifier()?.to_string());
        }

        self.expect_peek(Token::Rparen)?;
        Ok(identifiers)
    }

    fn parse_expression_list(&mut self, closing_token: Token) -> Result<Vec<Expression>, String> {
        let mut args: Vec<Expression> = Vec::new();

        if self.peek_token == closing_token {
            self.next_token();
            return Ok(args);
        }
        self.next_token();
        args.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::Lowest)?);
        }
        self.expect_peek(closing_token)?;
        Ok(args)
    }

    fn parse_string_literal(&mut self) -> Result<Expression, String> {
        if let Token::String(string) = self.cur_token.clone() {
            return Ok(Expression::StringLiteral(string));
        }
        Err(format!(
            "Expected a Boolean, got {} instead",
            self.cur_token
        ))
    }

    fn parse_array_literal(&mut self) -> Result<Expression, String> {
        let args = self.parse_expression_list(Token::Rbracket)?;
        Ok(Expression::Array(args))
    }

    fn parse_index_expression(&mut self, array: Expression) -> Result<Expression, String> {
        self.next_token();

        let index = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(Token::Rbracket)?;

        Ok(Expression::Index(Box::new(array), Box::new(index)))
    }

    fn parse_hash_literal(&mut self) -> Result<Expression, String> {
        let mut hash_map: Vec<(Expression, Expression)> = Vec::new();

        while self.peek_token != Token::Rbrace {
            self.next_token();
            let key = self.parse_expression(Precedence::Lowest)?;
            self.expect_peek(Token::Colon)?;
            self.next_token();
            let value = self.parse_expression(Precedence::Lowest)?;
            hash_map.push((key, value));

            if self.peek_token != Token::Rbrace {
                self.expect_peek(Token::Comma)?;
            }

            self.expect_peek(Token::Rbrace)?;
        }

        Ok(Expression::HashLiteral(hash_map))
    }

    fn prefix_parse_fn(&self) -> Option<PrefixParseFn<'a>> {
        match self.cur_token {
            Token::Ident(_) => Some(Self::parse_identifier),
            Token::Int(_) => Some(Self::parse_integer_literal),
            Token::Minus => Some(Self::parse_prefix_expression),
            Token::Bang => Some(Self::parse_prefix_expression),
            Token::True => Some(Self::parse_boolean),
            Token::False => Some(Self::parse_boolean),
            Token::Lparen => Some(Self::parse_grouped_expression),
            Token::If => Some(Self::parse_if_expression),
            Token::Function => Some(Self::parse_function_literal),
            Token::String(_) => Some(Self::parse_string_literal),
            Token::Lbracket => Some(Self::parse_array_literal),
            Token::Lbrace => Some(Parser::parse_hash_literal),
            _ => None,
        }
    }

    fn infix_parse_fn(&self) -> Option<InfixParseFn<'a>> {
        match self.peek_token {
            Token::Ident(_) => Some(Self::parse_infix_expression),
            Token::Int(_) => Some(Self::parse_infix_expression),
            Token::Minus => Some(Self::parse_infix_expression),
            Token::Plus => Some(Self::parse_infix_expression),
            Token::Asterisk => Some(Self::parse_infix_expression),
            Token::Slash => Some(Self::parse_infix_expression),
            Token::Nequals => Some(Self::parse_infix_expression),
            Token::Equals => Some(Self::parse_infix_expression),
            Token::Gthen => Some(Self::parse_infix_expression),
            Token::Lthen => Some(Self::parse_infix_expression),
            Token::Lparen => Some(Self::parse_call_expression),
            Token::Lbracket => Some(Self::parse_index_expression),
            _ => None,
        }
    }

    fn infix_token(&self, token: &Token) -> (Precedence, Option<Infix>) {
        match token {
            Token::Equals => (Precedence::Equals, Some(Infix::Equals)),
            Token::Nequals => (Precedence::Equals, Some(Infix::Nequals)),
            Token::Lthen => (Precedence::LessGreater, Some(Infix::Lthen)),
            Token::Gthen => (Precedence::LessGreater, Some(Infix::Gthen)),
            Token::Plus => (Precedence::Sum, Some(Infix::Plus)),
            Token::Minus => (Precedence::Sum, Some(Infix::Minus)),
            Token::Slash => (Precedence::Product, Some(Infix::Slash)),
            Token::Asterisk => (Precedence::Product, Some(Infix::Asterisk)),
            Token::Lparen => (Precedence::Call, None),
            Token::Lbracket => (Precedence::Index, None),
            _ => (Precedence::Lowest, None),
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

    pub fn get_errors(&self) -> Option<&[String]> {
        match !self.errors.is_empty() {
            true => Some(&self.errors),
            false => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Expression::{
        Boolean, Identifier, InfixExpression, IntegerLiteral, PrefixExpression,
    };
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
                Statement::Let(String::from("x"), Expression::IntegerLiteral(5)),
                Statement::Let(String::from("y"), Expression::IntegerLiteral(1023)),
                Statement::Let(
                    String::from("foobar"),
                    Expression::InfixExpression(
                        Box::new(Identifier(String::from("x"))),
                        Infix::Plus,
                        Box::new(Identifier(String::from("z")))
                    )
                ),
            ]
        );
        assert_eq!(parser.errors, Vec::<String>::new());
    }

    #[test]
    fn return_statement() {
        let input = " 
            return 12;
            return 1204;
            return;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(
            program.statements,
            vec![
                Statement::Return(Some(IntegerLiteral(12))),
                Statement::Return(Some(IntegerLiteral(1204))),
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

    #[test]
    fn boolean_expression() {
        // GIVEN
        let tests = vec![
            ("true == true", true, Infix::Equals, true),
            ("true != false", true, Infix::Nequals, false),
            ("false == false", false, Infix::Equals, false),
        ];
        // WHEN
        for (input, exp1, infix, exp2) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            // THEN
            assert_eq!(
                program.statements,
                vec![Statement::Expression(InfixExpression(
                    Box::new(Boolean(exp1)),
                    infix,
                    Box::new(Boolean(exp2))
                ))]
            );
        }
    }

    #[test]
    fn boolean_expression_precedence() {
        // GIVEN
        let tests = vec![
            ("true", "true;"),
            ("false", "false;"),
            ("3 > 5 == false", "((3 > 5) == false);"),
            ("3 < 5 == true", "((3 < 5) == true);"),
        ];
        // WHEN
        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            // THEN
            assert_eq!(program.to_string(), expected);
        }
    }

    #[test]
    fn operator_precedence() {
        // GIVEN
        let tests = vec![
            ("-a * b", "((-a) * b);"),
            ("!-a", "(!(-a));"),
            ("a + b + c", "((a + b) + c);"),
            ("a + b - c", "((a + b) - c);"),
            ("a * b * c", "((a * b) * c);"),
            ("a * b / c", "((a * b) / c);"),
            ("a + b / c", "(a + (b / c));"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f);"),
            ("3 + 4; -5 * 5", "(3 + 4);((-5) * 5);"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4));"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4));"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
            ),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4);"),
            ("(5 + 5) * 2", "((5 + 5) * 2);"),
            ("2 / (5 + 5)", "(2 / (5 + 5));"),
            ("-(5 + 5)", "(-(5 + 5));"),
            ("!(true == true)", "(!(true == true));"),
            ("return x", "return x;"),
            ("return x return 2 * 3", "return x;return (2 * 3);"),
            ("return 2 * 4 + 5;", "return ((2 * 4) + 5);"),
            ("fn() { 3 * 9; }", "fn() { (3 * 9); };"),
            ("fn(x) { x * 9; }", "fn(x) { (x * 9); };"),
            ("fn(x, y) { x + y; }", "fn(x, y) { (x + y); };"),
            ("call()", "call();"),
            ("add(1, 2 * 3, 4 + 5)", "add(1, (2 * 3), (4 + 5));"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d);"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g));",
            ),
            ("fn(x, y) { x + y; }(3, 4)", "fn(x, y) { (x + y); }(3, 4);"),
            ("let x = 3", "let x = 3;"),
            ("let x = 3 + f * 8;", "let x = (3 + (f * 8));"),
            ("\"hello world\"", "\"hello world\";"),
            ("let s = \"hello world\"", "let s = \"hello world\";"),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d);",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])));",
            ),
        ];
        // WHEN
        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            // THEN
            assert_eq!(program.to_string(), expected);
        }
    }

    #[test]
    fn if_expression() {
        // GIVEN
        let tests = vec![
            ("if (x < y) { x }", "if (x < y) { x; };"),
            (
                "if (x < y) { x } else { y }",
                "if (x < y) { x; } else { y; };",
            ),
        ];
        // WHEN
        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            // THEN
            assert_eq!(program.to_string(), expected);
        }
    }

    #[test]
    fn string_literal() {
        // GIVEN
        let tests = vec![("\"Hello World\"", "\"Hello World\";")];
        // WHEN
        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            // THEN
            assert_eq!(program.to_string(), expected);
        }
    }

    #[test]
    fn hash_literal() {
        // GIVEN
        let tests = vec![
            ("{}", "{};"),
            ("{1: 2, 2: 3}", "{1: 2, 2: 3};"),
            ("{true: 3}", "{true: 3};"),
            (r#"{"one": 2, "two": 3}"#, r#"{"one": 2, "two": 3};"#),
        ];
        // WHEN
        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            // THEN
            assert_eq!(program.to_string(), expected);
        }
    }

    #[test]
    fn array_expression() {}

    #[test]
    fn index_expression() {}
}
