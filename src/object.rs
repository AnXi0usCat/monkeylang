use crate::ast::BlockStatement;
use crate::environment::Environment;
use std::fmt;
use std::fmt::{write, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Object<'a> {
    String(String),
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object<'a>>),
    Function(Vec<String>, BlockStatement, &'a Environment<'a>),
}

#[allow(unused_must_use)]
impl<'a> fmt::Display for Object<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(value) => write!(f, "{}", value),
            Self::Integer(value) => write!(f, "{}", value),
            Self::Boolean(value) => write!(f, "{}", value),
            Self::Return(value) => write!(f, "{}", *value),
            Self::Null => write!(f, "Null"),
            Self::Function(params, body, _) => {
                write!(f, "fn({}) {{ {} }}", params.join(", "), body)
            }
        };
        Ok(())
    }
}

impl<'a> Object<'a> {
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Null => false,
            Self::Boolean(true) => true,
            Self::Boolean(false) => false,
            _ => true,
        }
    }

    pub fn obj_type(&self) -> &str {
        match self {
            Self::Integer(_) => "INTEGER",
            Self::String(_) => "STRING",
            Self::Boolean(_) => "BOOLEAN",
            Self::Null => "NULL",
            Self::Return(_) => "RETURN",
            Self::Function(_, _, _) => "FUNCTION",
        }
    }
}
