use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement);
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(String, Expression),
    Return(Option<Expression>),
    Expression(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Let(name, value) => write!(f, "let {} = {};", name, value),
            Self::Return(Some(value)) => write!(f, "return {};", value),
            Self::Return(None) => write!(f, "return;"),
            Self::Expression(value) => write!(f, "{};", value),
        };
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(String),
    IntegerLiteral(i32),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Identifier(value) => write!(f, "{}", value),
            Self::IntegerLiteral(int) => write!(f, "{}", int),
        };
        Ok(())
    }
}
