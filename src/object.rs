use std::fmt;
use std::fmt::{write, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    String(String),
    Integer(i64),
    Boolean(bool),
    Null,
}

#[allow(unused_must_use)]
impl fmt::Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(value) => write!(f, "{}", value),
            Self::Integer(value) => write!(f, "{}", value),
            Self::Boolean(value) => write!(f, "{}", value),
            Self::Null => write!(f, "Null"),
        };
        Ok(())
    }
}
