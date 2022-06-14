use crate::object::Object;
use crate::object::Object::Array;

pub const NULL_LITERAL: &str = "Null";
pub type BuiltInFunction = fn(Vec<Object>) -> Result<Object, String>;

pub struct Builtin {
    pub name: &'static str,
    pub func: Object,
}

macro_rules! builtin {
    ($var_name:ident) => {
        Builtin {
            name: stringify!($var_name),
            func: Object::Builtin($var_name),
        }
    };
}

pub const BUILTINS: &[Builtin] = &[
    builtin!(len),
    builtin!(first),
    builtin!(last),
    builtin!(rest),
];

fn len(input: Vec<Object>) -> Result<Object, String> {
    if input.len() != 1 {
        return Err(format!(
            "wrong number of arguments. got={}, want=1",
            input.len()
        ));
    }
    match &input[0] {
        Object::String(val) => Ok(Object::Integer(val.len() as i64)),
        _ => Err(format!(
            "argument to `len` not supported, got {}",
            &input[0].obj_type()
        )),
    }
}

fn first(input: Vec<Object>) -> Result<Object, String> {
    if input.len() != 1 {
        return Err(format!(
            "wrong number of arguments. got={}, want=1",
            input.len()
        ));
    };
    match &input[0] {
        Object::Array(val) => {
            return if let Some(val) = val.get(0) {
                Ok(val.clone())
            } else {
                Ok(Object::Null)
            }
        }
        _ => Err(format!(
            "argument to `first` not supported, got {}",
            &input[0].obj_type()
        )),
    }
}

fn last(input: Vec<Object>) -> Result<Object, String> {
    if input.len() != 1 {
        return Err(format!(
            "wrong number of arguments. got={}, want=1",
            input.len()
        ));
    };
    match &input[0] {
        Object::Array(val) => {
            return if let Some(val) = val.last() {
                Ok(val.clone())
            } else {
                Ok(Object::Null)
            }
        }
        _ => Err(format!(
            "argument to `last` not supported, got {}",
            &input[0].obj_type()
        )),
    }
}

fn rest(input: Vec<Object>) -> Result<Object, String> {
    if input.len() != 1 {
        return Err(format!(
            "wrong number of arguments. got={}, want=1",
            input.len()
        ));
    };
    match &input[0] {
        Object::Array(val) => {
            return if val.len() > 0 {
                Ok(Object::Array(val[1..].to_vec()))
            } else {
                Ok(Object::Null)
            }
        }
        _ => Err(format!(
            "argument to `rest` not supported, got {}",
            &input[0].obj_type()
        )),
    }
}

fn push(input: Vec<Object>) -> Result<Object, String> {
    if input.len() != 2 {
        return Err(format!(
            "wrong number of arguments. got={}, want=2",
            input.len()
        ));
    };
    match &input[0] {
        Object::Array(val) => {
            let mut new_array = val.clone();
            new_array.push(input[1].clone());
            Ok(Object::Array(new_array))
        }
        _ => Err(format!(
            "argument to `push` not supported, got {}",
            &input[0].obj_type()
        )),
    }
}
