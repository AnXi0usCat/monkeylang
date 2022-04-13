use crate::evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::BufRead;
use std::io::{self, Write};
use std::process;

pub fn start() -> io::Result<()> {
    let stdin = io::stdin();
    println!("Hello! This is the 🐒 programming language!");

    loop {
        let mut input = String::new();
        print!("{}", ">> ");
        std::io::stdout().flush().unwrap();

        stdin.lock().read_line(&mut input).unwrap_or_else(|err| {
            eprintln!("Problem reading input: {:?}", (err));
            process::exit(1);
        });

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if parser.get_errors().is_some() {
            eprintln!("parser errors:");
            for err in parser.get_errors().unwrap() {
                eprintln!("\t{}", err)
            }
            continue;
        }

        let eval = evaluator::eval(&program);
        println!("{}", eval.unwrap().to_string());
    }
}
