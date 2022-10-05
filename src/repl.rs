use rustyline::error::ReadlineError;
use rustyline::Editor;
use crate::environment::Environment;
use crate::evaluator;
use crate::object::Object;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::cell::RefCell;
use std::rc::Rc;


pub fn start() {
    let mut rl = Editor::<()>::new().unwrap();
    let env = Rc::new(RefCell::new(Environment::new()));
    println!("Hello! This is the 🐒 programming language!");

    loop {
        let readline = rl.readline(">> ");
        
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
 
                let lexer = Lexer::new(&line);
                let mut parser = Parser::new(lexer);
                let program = parser.parse_program();

                if parser.get_errors().is_some() {
                    eprintln!("parser errors:");
                    for err in parser.get_errors().unwrap() {
                        eprintln!("\t{}", err);
                    }
                    continue;
                }
                
                let result = evaluator::eval(&program, Rc::clone(&env));
                match result {
                    Ok(Object::Null) => (),
                    Ok(object) => println!("{}", object),
                    Err(error) => println!("{}", error),
                }
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }

}
