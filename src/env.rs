use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::object::Object;

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
  pub store: HashMap<String, Object>,
  pub outer: Option<Rc<RefCell<Env>>>,
}

impl Env {
  pub fn new() -> Self {
    Self {
      store: HashMap::new(),
      outer: None,
    }
  }

  pub fn from(store: HashMap<String, Object>) -> Self {
    Self {
      store,
      outer: None,
    }
  }

  pub fn with_outer(outer: Rc<RefCell<Env>>) -> Self {
    Self {
      store: HashMap::new(),
      outer: Some(outer),
    }
  }

  pub fn get(&self, name: String) -> Option<Object> {
    match self.store.get(&name) {
      Some(value) => Some(value.clone()),
      None => match self.outer {
        Some(ref outer) => outer.borrow_mut().get(name),
        None => None,
      },
    }
  }

  pub fn set(&mut self, name: String, value: &Object) {
    self.store.insert(name, value.clone());
  }
}