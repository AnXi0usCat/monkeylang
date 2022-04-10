use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement)?;
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
    PrefixExpression(Prefix, Box<Expression>),
    InfixExpression(Box<Expression>, Infix, Box<Expression>),
    Boolean(bool),
    If(Box<Expression>, BlockStatement, Option<BlockStatement>),
    FunctionLiteral(Vec<String>, BlockStatement),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Identifier(value) => write!(f, "{}", value),
            Self::IntegerLiteral(int) => write!(f, "{}", int),
            Self::PrefixExpression(operator, exp) => write!(f, "({}{})", operator, exp),
            Self::InfixExpression(exp1, operator, exp2) => {
                write!(f, "({} {} {})", exp1, operator, exp2)
            }
            Self::Boolean(value) => write!(f, "{}", value),
            Self::If(condition, consequence, alternative) => {
                write!(f, "if {} {}", condition, consequence)?;
                if alternative.is_some() {
                    write!(f, "else {}", alternative.as_ref().unwrap())?;
                }
                Ok(())
            }
            Self::FunctionLiteral(parameters, body) => {
                write!(f, "fn({}) {}", parameters.join(", "), body)
            }
        };
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Prefix {
    Minus,
    Bang,
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Minus => write!(f, "-"),
            Self::Bang => write!(f, "!"),
        };
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Infix {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Equals,
    Gequals,
    Lequals,
    Nequals,
    Gthen,
    Lthen,
}

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Asterisk => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Equals => write!(f, "=="),
            Self::Gequals => write!(f, ">="),
            Self::Lequals => write!(f, "<="),
            Self::Nequals => write!(f, "!="),
            Self::Gthen => write!(f, ">"),
            Self::Lthen => write!(f, "<"),
        };
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for statement in &self.statements {
            write!(f, "{{ {} }}", statement)?;
        }
        Ok(())
    }
}
