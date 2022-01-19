use std::iter::Peekable;
use std::str::Chars;
use crate::token::Token;

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
        let tok = match self.ch {
            '=' => Token::Assign,
            '+' => Token::Plus,
            ';' => Token::Semicolon,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            ',' => Token::Comma,
            '\u{0}' => Token::Eof,
            _ => Token::Illegal
        };
        self.read_char();
        tok
    }

    pub fn read_char(&mut self) {
        self.position += if self.ch == '\u{0}' {
            0
        } else {
            self.ch.len_utf8()
        };
        self.ch = self.chars.next().unwrap_or('\u{0}');
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn next_token() {
        let input = "=+(){},;";
        let mut lexer = Lexer::new(input);

        let tests = [
            Token::Assign,
            Token::Plus,
            Token::Lparen,
            Token::Rparen,
            Token::Lbrace,
            Token::Rbrace
        ];

        for (i, test) in tests.iter().enumerate() {
            let token = lexer.next_token();
            assert_eq!(&token, test, "tests[{}] - token", i)
        }
    }

}