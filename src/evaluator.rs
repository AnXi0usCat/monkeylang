use crate::ast::{Expression, Program, Statement};
use crate::object::Object;

pub fn eval(program: &Program) -> Result<Object, String> {
    let mut result = Ok(Object::Null);
    for statement in &program.statements {
        result = match statement {
            Statement::Expression(val) => eval_expression(val),
            Statement::Return(Some(expr)) => Ok(Object::Null),
            Statement::Return(None) => Ok(Object::Null),
            Statement::Let(value, expr) => Ok(Object::Null),
        };
    }
    result
}

fn eval_expression(expr: &Expression) -> Result<Object, String> {
    match expr {
        Expression::IntegerLiteral(value) => Ok(Object::Integer(*value)),
        _ => Ok(Object::Null),
    }
}

#[cfg(test)]
mod tests {
    use crate::evaluator::eval;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;

    fn test_eval(input: &str) -> Result<Object, String> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        eval(&program)
    }

    #[test]
    fn eval_integer() {
        // GIVEN
        let tests = vec![("5", 5), ("10", 10)];

        // WHEN
        for (input, expected) in tests {
            let result = test_eval(input);

            // THEN
            assert_eq!(result.unwrap().to_string(), expected.to_string());
        }
    }
}
