use crate::ast::{Expression, Infix, Prefix, Program, Statement};
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
        Expression::InfixExpression(expr1, infix, expr2) => {
            let ex_obj1 = eval_expression(expr1)?;
            let ex_obj2 = eval_expression(expr2)?;
            eval_infix_expression(infix, &ex_obj1, &ex_obj2)
        }
        _ => Ok(Object::Null),
    }
}

fn eval_prefix_expression(prefix: &Prefix, expr: &Expression) -> Result<Object, String> {
    let obj = eval_expression(expr)?;
    match prefix {
        Prefix::Bang => eval_bang_operator(&obj),
        Prefix::Minus => eval_minus_operator(&obj),
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

fn eval_minus_operator(right: &Object) -> Result<Object, String> {
    match right {
        Object::Integer(int) => Ok(Object::Integer(-(*int))),
        _ => Ok(Object::Null),
    }
}

fn eval_infix_expression(infix: &Infix, left: &Object, right: &Object) -> Result<Object, String> {
    match (left, right) {
        (Object::Integer(value1), Object::Integer(value2)) => {
            eval_integer_infix_expression(infix, *value1, *value2)
        }
        _ => Ok(Object::Null),
    }
}

fn eval_integer_infix_expression(infix: &Infix, left: i64, right: i64) -> Result<Object, String> {
    match infix {
        Infix::Plus => Ok(Object::Integer(left + right)),
        Infix::Minus => Ok(Object::Integer(left - right)),
        Infix::Asterisk => Ok(Object::Integer(left * right)),
        Infix::Slash => Ok(Object::Integer(left / right)),
        Infix::Lthen => Ok(Object::Boolean(left < right)),
        Infix::Gthen => Ok(Object::Boolean(left > right)),
        Infix::Equals => Ok(Object::Boolean(left == right)),
        Infix::Nequals => Ok(Object::Boolean(left != right)),
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

    #[test]
    fn eval_integer_prefix() {
        // GIVEN
        let tests = vec![("-5", -5), ("-10", -10)];

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
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
        ];

        // WHEN
        for (input, expected) in tests {
            let result = test_eval(input);

            // THEN
            assert_eq!(result.unwrap().to_string(), expected.to_string());
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

    #[test]
    fn eval_integer_expression() {
        // GIVEN
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
        // WHEN
        for (input, expected) in tests {
            let result = test_eval(input);

            // THEN
            assert_eq!(result.unwrap().to_string(), expected.to_string());
        }
    }
}
