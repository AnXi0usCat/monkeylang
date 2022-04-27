use crate::token::{lookup_identifier, Token};
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    ch: char,
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            ch: '\u{0}',
            chars: input.chars().peekable(),
        };
        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch {
            '=' => self.two_ch_token(Token::Equals, Token::Assign, '='),
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' => self.two_ch_token(Token::Nequals, Token::Bang, '='),
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '>' => self.two_ch_token(Token::Gequals, Token::Gthen, '='),
            '<' => self.two_ch_token(Token::Lequals, Token::Lthen, '='),
            ';' => Token::Semicolon,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            ',' => Token::Comma,
            '"' => Token::String(self.read_string().to_string()),
            '\u{0}' => Token::Eof,
            _ => {
                return if self.ch.is_alphabetic() {
                    let ident = self.read_identifier();
                    lookup_identifier(ident)
                } else if self.ch.is_numeric() {
                    let integer_part = self.read_number().to_string();
                    if self.ch == '.' && self.chars.peek().unwrap_or(&'\u{0}').is_numeric() {
                        self.read_char();
                        let decimal_part = self.read_number();
                        Token::Float(format!("{}.{}", integer_part, decimal_part))
                    } else {
                        Token::Int(String::from(integer_part))
                    }
                } else {
                    self.read_char();
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

    fn two_ch_token(&mut self, token_if: Token, token_else: Token, test_ch: char) -> Token {
        if Some(&test_ch) == self.chars.peek() {
            self.read_char();
            token_if
        } else {
            token_else
        }
    }

    fn read_number(&mut self) -> &str {
        let position = self.position;

        while self.ch.is_numeric() {
            self.read_char();
        }
        &self.input[position..self.position]
    }

    fn read_string(&mut self) -> &str {
        let position = self.position + 1;
        loop {
            self.read_char();
            if self.ch == '"' || self.ch == '\u{0}' {
                break;
            }
        }
        &self.input[position..self.position]
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Token::Eof => None,
            token => Some(token),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn next_token() {
        let input = r#"
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
        10 != 9;
        12.345
        0.12;
        "foobar"
        "foo bar"
        "#;

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
            Token::Float("12.345".to_string()),
            Token::Float("0.12".to_string()),
            Token::Semicolon,
            Token::String("foobar".to_string()),
            Token::String("foo bar".to_string()),
        ];

        for (i, test) in tests.iter().enumerate() {
            let token = lexer.next_token();
            assert_eq!(&token, test, "tests[{}] - token", i)
        }
    }
}
