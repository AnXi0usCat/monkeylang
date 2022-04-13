use crate::ast::{Expression, Prefix, Program, Statement};
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
        Expression::Boolean(value) => Ok(Object::Boolean(*value)),
        Expression::PrefixExpression(prefix, expr) => eval_prefix_expression(prefix, expr.as_ref()),
        _ => Ok(Object::Null),
    }
}

fn eval_prefix_expression(prefix: &Prefix, expr: &Expression) -> Result<Object, String> {
    let obj = eval_expression(expr)?;
    match prefix {
        Prefix::Bang => eval_bang_operator(&obj),
        _ => Ok(Object::Null),
    }
}

fn eval_bang_operator(right: &Object) -> Result<Object, String> {
    match right {
        Object::Boolean(true) => Ok(Object::Boolean(false)),
        Object::Boolean(false) => Ok(Object::Boolean(true)),
        Object::Null => Ok(Object::Boolean(true)),
        _ => Ok(Object::Boolean(false)),
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

    #[test]
    fn eval_boolean() {
        // GIVEN
        let tests = vec![("true", "true"), ("false", "false")];

        // WHEN
        for (input, expected) in tests {
            let result = test_eval(input);

            // THEN
            assert_eq!(result.unwrap().to_string(), expected);
        }
    }

    #[test]
    fn eval_bang_prefix() {
        // GIVEN
        let tests = vec![
            ("!true", "false"),
            ("!!true", "true"),
            ("!false", "true"),
            ("!!false", "false"),
            ("!null", "true"),
            ("!!null", "false"),
            ("!0", "false"),
            ("!3", "false"),
            ("!!3", "true"),
        ];
        // WHEN
        for (input, expected) in tests {
            let result = test_eval(input);

            // THEN
            assert_eq!(result.unwrap().to_string(), expected);
        }
    }
}
