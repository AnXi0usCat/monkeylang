use crate::object::Object;
use lazy_static::lazy_static;
use std::collections::HashMap;

pub const NULL_LITERAL: &str = "Null";

type BuiltInFunction = fn(Vec<String>) -> Result<Object, String>;

fn len(input: Vec<String>) -> Result<Object, String> {
    Ok(Object::Null)
}

lazy_static! {
    static ref BUILTINS: HashMap<String, BuiltInFunction> = {
        let mut map = HashMap::new();
        map.insert(String::from("len"), len);
        map
    };
}
