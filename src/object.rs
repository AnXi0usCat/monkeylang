use std::fmt;
use std::fmt::{write, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    String(String),
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
}

#[allow(unused_must_use)]
impl fmt::Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(value) => write!(f, "{}", value),
            Self::Integer(value) => write!(f, "{}", value),
            Self::Boolean(value) => write!(f, "{}", value),
            Self::Return(value) => write!(f, "{}", *value),
            Self::Null => write!(f, "Null"),
        };
        Ok(())
    }
}

impl Object {
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
        }
    }
}
