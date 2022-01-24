use std::{io, process};
use std::io::BufRead;
use crate::lexer::Lexer;
use crate::token::Token;

pub fn start() {

    let stdin = io::stdin();

    loop {
        let mut input =  String::new();
        print!("{}", ">>");

        stdin.lock().read_line(&mut input).unwrap_or_else(|err| {
            eprintln!("Problem reading input: {:?}", (err));
            process::exit(1);
        });

        let mut lexer = Lexer::new(&input);

        while let token = lexer.next_token() {
            if token == Token::Eof {
                process::exit(0);
            }
            print!("{}", token);
        }

    }
}