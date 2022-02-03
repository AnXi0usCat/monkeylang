use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>
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
    Let(String, Expression)
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Let(name, value) => write!(f, "{} = {}", name, value),
        };
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Expression {
    Identifier(String)
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self{
            Self::Identifier(value) => write!(f, "{}", value),
        };
        Ok(())
    }
}