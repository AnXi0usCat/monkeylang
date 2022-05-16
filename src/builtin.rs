use crate::object::Object;
use std::collections::HashMap;

pub const NULL_LITERAL: &str = "Null";
type BuiltInFunction<'a> = fn(Vec<String>) -> Result<Object, String>;

struct Builtin {
    pub name: &'static str,
    pub func: Object,
}

macro_rules! builtin {
    ($var_name: ident) => {
        Builtin {
            name: stringify!($var_name),
            func: Object::Builtin($var_name),
        }
    };
}

pub const BUILTINS: &[Builtin] = &[builtin!(len)];

fn len(input: Vec<Object>) -> Result<Object, String> {
    if input.len() != 1 {
        return Err(format!("Expected 1 argument, got {} instead", input.len()));
    }
    match &input[0] {
        Object::String(val) => Ok(Object::Integer(val.len() as i64)),
        _ => Err(format!("Unsupported type {}", &input[0].obj_type())),
    }
}
