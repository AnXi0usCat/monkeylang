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
        Lexer {
            input,
            position: 0,
            ch: '\u{0}',
            chars: input.chars().peekable()
        }
    }

    pub fn next_token(&self) -> Token {
        Token::Lparen
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn next_token() {
        let input = "=+(){},;";
        let lexer = Lexer::new(input);

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