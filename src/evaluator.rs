use crate::ast::{BlockStatement, Expression, Infix, Prefix, Program, Statement};
use crate::environment::Environment;
use crate::object::Object;
use crate::object::Object::{Boolean, Integer, Null, Return};

pub fn eval(program: &Program, env: &mut Environment) -> Result<Object, String> {
    let mut result = Null;
    for statement in &program.statements {
        result = eval_statement(statement, env)?;

        if let Object::Return(value) = result {
            return Ok(*value);
        }
    }
    Ok(result)
}

fn eval_statement(statement: &Statement, env: &mut Environment) -> Result<Object, String> {
    match statement {
        Statement::Expression(expr) => eval_expression(expr, env),
        Statement::Return(Some(expr)) => {
            let obj = eval_expression(expr, env)?;
            Ok(Return(Box::new(obj)))
        }
        Statement::Return(None) => Ok(Null),
        Statement::Let(name, expr) => {
            let result = eval_expression(expr, env)?;
            env.set(name, result.clone());
            Ok(result)
        }
    }
}

fn eval_block_statement(block: &BlockStatement, env: &mut Environment) -> Result<Object, String> {
    let mut result = Null;
    for statement in &block.statements {
        result = eval_statement(statement, env)?;
        if let Object::Return(_) = result {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_expression(expr: &Expression, env: &mut Environment) -> Result<Object, String> {
    match expr {
        Expression::IntegerLiteral(value) => Ok(Object::Integer(*value)),
        Expression::Boolean(value) => Ok(Object::Boolean(*value)),
        Expression::PrefixExpression(prefix, expr) => {
            eval_prefix_expression(prefix, expr.as_ref(), env)
        }
        Expression::InfixExpression(expr1, infix, expr2) => {
            let ex_obj1 = eval_expression(expr1, env)?;
            let ex_obj2 = eval_expression(expr2, env)?;
            eval_infix_expression(infix, &ex_obj1, &ex_obj2)
        }
        Expression::If(condition, consequence, alternative) => {
            eval_if_expression(condition.as_ref(), consequence, alternative.as_ref(), env)
        }
        _ => Ok(Null),
    }
}

fn eval_prefix_expression(
    prefix: &Prefix,
    expr: &Expression,
    env: &mut Environment,
) -> Result<Object, String> {
    let obj = eval_expression(expr, env)?;
    match prefix {
        Prefix::Bang => eval_bang_operator(&obj),
        Prefix::Minus => eval_minus_operator(&obj),
        _ => Err(format!("unknown operator: {} {}", prefix, obj.obj_type())),
    }
}

fn eval_bang_operator(right: &Object) -> Result<Object, String> {
    match right {
        Object::Boolean(true) => Ok(Object::Boolean(false)),
        Object::Boolean(false) => Ok(Object::Boolean(true)),
        Null => Ok(Object::Boolean(true)),
        _ => Ok(Object::Boolean(false)),
    }
}

fn eval_minus_operator(right: &Object) -> Result<Object, String> {
    match right {
        Object::Integer(int) => Ok(Object::Integer(-(*int))),
        _ => Err(format!("unknown operator: -{}", right.obj_type())),
    }
}

fn eval_infix_expression(infix: &Infix, left: &Object, right: &Object) -> Result<Object, String> {
    match (left, right) {
        (Object::Integer(value1), Object::Integer(value2)) => {
            eval_integer_infix_expression(infix, *value1, *value2)
        }
        (Object::Boolean(value1), Object::Boolean(value2)) => {
            eval_boolean_infix_expression(infix, *value1, *value2)
        }
        (left, right) => Err(format!(
            "type mismatch: {} {} {}",
            left.obj_type(),
            infix,
            right.obj_type()
        )),
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
        _ => Err(format!(
            "unknown operator: {} {} {}",
            Integer(left).obj_type(),
            infix,
            Integer(right).obj_type()
        )),
    }
}

fn eval_boolean_infix_expression(infix: &Infix, left: bool, right: bool) -> Result<Object, String> {
    match infix {
        Infix::Equals => Ok(Boolean(left == right)),
        Infix::Nequals => Ok(Boolean(left != right)),
        _ => Err(format!(
            "unknown operator: {} {} {}",
            Boolean(left).obj_type(),
            infix,
            Boolean(right).obj_type()
        )),
    }
}

fn eval_if_expression(
    condition: &Expression,
    consequence: &BlockStatement,
    alternative: Option<&BlockStatement>,
    env: &mut Environment,
) -> Result<Object, String> {
    let result = eval_expression(condition, env)?;
    if result.is_truthy() {
        eval_block_statement(consequence, env)
    } else {
        alternative
            .map(|alt| eval_block_statement(alt, env))
            .unwrap_or(Ok(Null))
    }
}

#[cfg(test)]
mod tests {
    use crate::environment::Environment;
    use crate::evaluator::eval;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::object::Object::Null;
    use crate::parser::Parser;

    fn test_eval(input: &str) -> Result<Object, String> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let mut env = Environment::new();
        eval(&program, &mut env)
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

    #[test]
    fn eval_boolean_expression() {
        // GIVEN
        let tests = vec![
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];
        // WHEN
        for (input, expected) in tests {
            let result = test_eval(input);

            // THEN
            assert_eq!(result.unwrap().to_string(), expected.to_string())
        }
    }

    #[test]
    fn eval_if_else_expressions() {
        // GIVEN
        let tests = vec![
            ("if (true) { 10 }", "10"),
            ("if (false) { 10 }", "Null"),
            ("if (1) { 10 }", "10"),
            ("if (1 < 2) { 10 }", "10"),
            ("if (1 > 2) { 10 }", "Null"),
            ("if (1 > 2) { 10 } else { 20 }", "20"),
            ("if (1 < 2) { 10 } else { 20 }", "10"),
        ];
        // WHEN
        for (input, expected) in tests {
            let result = test_eval(input);

            // THEN
            assert_eq!(result.unwrap().to_string(), expected);
        }
    }

    #[test]
    fn eval_return_statement() {
        // GIVEN
        let tests = vec![
            ("return 10;", "10"),
            ("return 10; 9;", "10"),
            ("return 2 * 5; 9;", "10"),
            ("9; return 2 * 5; 9;", "10"),
        ];
        // WHEN
        for (input, expected) in tests {
            let result = test_eval(input);

            // THEN
            assert_eq!(result.unwrap().to_string(), expected);
        }
    }

    #[test]
    fn eval_nested_statements() {
        // GIVEN
        let tests = vec![
            // Nested statements
            (
                "if (10 > 1) {
                if (10 > 1) {
                    return 10;
                }
                return 1;
            }",
                "10",
            ),
        ];
        // WHEN
        for (input, expected) in tests {
            let result = test_eval(input);

            // THEN
            assert_eq!(result.unwrap().to_string(), expected);
        }
    }

    #[test]
    fn evaluate_errors() {
        // GIVEN
        let tests = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "if (10 > 1) {
                if (10 > 1) {
                  return true + false;
                }
              return 1; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar", "identifier not found: foobar"),
        ];
        // WHEN
        for (input, expected) in tests {
            let result = test_eval(input);

            // THEN
            assert_eq!(result.unwrap_err().to_string(), expected);
        }
    }

    #[test]
    fn eval_let_statements() {
        // GIVEN
        let tests = vec![
            ("let a = 5; a;", "5"),
            ("let a = 5 * 5; a;", "25"),
            ("let a = 5; let b = a; b;", "5"),
            ("let a = 5; let b = a; let c = a + b + 5; c;", "15"),
        ];
        // WHEN
        for (input, expected) in tests {
            let result = test_eval(input);

            // THEN
            assert_eq!(result.unwrap().to_string(), expected);
        }
    }
}
