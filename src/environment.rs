use crate::object::Object;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment<'a> {
    store: HashMap<String, Object<'a>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
        }
    }

    pub fn set(&mut self, k: &str, v: Object<'a>) {
        self.store.insert(k.to_string(), v);
    }

    pub fn get(&self, k: &str) -> Option<&Object<'a>> {
        self.store.get(k)
    }
}
