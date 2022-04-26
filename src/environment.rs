use crate::object::Object;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn extend(outer: Rc<RefCell<Self>>) -> Environment {
        Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn set(&mut self, k: &str, v: Object) {
        self.store.insert(k.to_string(), v);
    }

    pub fn get(&self, k: &str) -> Option<Object> {
        match self.store.get(k) {
            Some(value) => Some(value.clone()),
            None => self
                .outer
                .as_ref()
                .and_then(|obj| obj.deref().borrow().get(k)),
        }
    }
}
