use std::iter::Peekable;
use std::str::Chars;
use crate::token::{Token, lookup_identifier};

pub struct Lexer<'a>{
    input: &'a str,
    position: usize,
    ch: char,
    chars: Peekable<Chars<'a>>
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            ch: '\u{0}',
            chars: input.chars().peekable()
        };
        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch {
            '=' => {
                if Some(&'=') == self.chars.peek() {
                    self.read_char();
                    Token::Equals
                } else {
                    Token::Assign
                }
            },
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' => {
                if Some(&'=') == self.chars.peek() {
                    self.read_char();
                    Token::Nequals
                } else {
                    Token::Bang
                }
            },
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '>' => {
                if Some(&'=') == self.chars.peek() {
                    self.read_char();
                    Token::Gequals
                } else {
                    Token::Gthen
                }
            },
            '<' => {
                if Some(&'=') == self.chars.peek() {
                    self.read_char();
                     Token::Lequals
                } else {
                    Token::Lthen
                }
            }
            ';' => Token::Semicolon,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            ',' => Token::Comma,
            '\u{0}' => Token::Eof,
            _ => {
               return if self.ch.is_alphabetic() {
                   let ident = self.read_identifier();
                   lookup_identifier(ident)
                } else if self.ch.is_numeric() {
                   let digit = self.read_number();
                     Token::Int(String::from(digit))
                } else {
                     Token::Illegal
                };
            }
        };
        self.read_char();
        tok
    }

    fn read_char(&mut self) {
        self.position += if self.ch == '\u{0}' {
            0
        } else {
            self.ch.len_utf8()
        };
        self.ch = self.chars.next().unwrap_or('\u{0}');
    }

    fn read_identifier(&mut self) -> &str {
        let position = self.position;

        while self.ch.is_alphanumeric() {
            self.read_char();
        }
        &self.input[position..self.position]
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }

    fn read_number(&mut self) -> &str {
        let position = self.position;

        while self.ch.is_numeric() {
            self.read_char();
        }
        &self.input[position..self.position]
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn next_token() {
        let input = "
        let five = 5;

        let ten = 10;
           let add = fn(x, y) {
             x + y;
        };

        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;
        if (5 < 10) {
            return true;
        } else {
            return false;
        }
        10 == 10;
        10 != 9;";

        let mut lexer = Lexer::new(input);

        let tests = [
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int("10".to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::Lparen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::Rparen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Int("5".to_string()),
            Token::Lthen,
            Token::Int("10".to_string()),
            Token::Gthen,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::If,
            Token::Lparen,
            Token::Int("5".to_string()),
            Token::Lthen,
            Token::Int("10".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::Rbrace,
            Token::Else,
            Token::Lbrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::Rbrace,
            Token::Int("10".to_string()),
            Token::Equals,
            Token::Int("10".to_string()),
            Token::Semicolon,
            Token::Int("10".to_string()),
            Token::Nequals,
            Token::Int("9".to_string()),
            Token::Semicolon,
        ];

        for (i, test) in tests.iter().enumerate() {
            let token = lexer.next_token();
            assert_eq!(&token, test, "tests[{}] - token", i)
        }
    }

}