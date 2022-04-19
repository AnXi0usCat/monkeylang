use crate::object::Object;
use std::collections::HashMap;

pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
        }
    }

    pub fn set(&mut self, k: &str, v: Object) {
        self.store.insert(k.to_string(), v);
    }

    pub fn get(&self, k: &str) -> Option<&Object> {
        self.store.get(k)
    }
}
