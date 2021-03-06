use crate::environment::Environment;
use crate::evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::cell::RefCell;
use std::io::BufRead;
use std::io::{self, Write};
use std::process;
use std::rc::Rc;

pub fn start() -> io::Result<()> {
    let stdin = io::stdin();
    let env = Rc::new(RefCell::new(Environment::new()));
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

        let result = evaluator::eval(&program, Rc::clone(&env));
        match result {
            Ok(object) => println!("{}", object.to_string()),
            Err(error) => println!("{}", error.to_string()),
        }
    }
}
