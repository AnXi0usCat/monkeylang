use std::{io, process};
use std::io::BufRead;
use crate::lexer::Lexer;

pub fn start() {

    let stdin = io::stdin();
    println!("Hello! This is the 🐒 programming language!");

    loop {
        let mut input =  String::new();
        print!("{}", ">> ");

        stdin.lock().read_line(&mut input).unwrap_or_else(|err| {
            eprintln!("Problem reading input: {:?}", (err));
            process::exit(1);
        });

        let lexer = Lexer::new(&input);
        for token in lexer {
            println!("{}", token);
        }
    }
}