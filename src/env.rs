use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::object::Object;

type Objects = HashMap<String, Object>;

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
  pub objects: Objects,
  pub parent: Option<Rc<RefCell<Env>>>,
}

impl Env {
  pub fn new() -> Self {
    Self {
      objects: HashMap::new(),
      parent: None,
    }
  }

  pub fn from(objects: Objects) -> Self {
    Self {
      objects,
      parent: None,
    }
  }

  pub fn new_with_parent(parent: Rc<RefCell<Env>>) -> Self {
    Self {
      objects: HashMap::new(),
      parent: Some(parent),
    }
  }

  pub fn get(&self, name: String) -> Option<Object> {
    if let Some(object) = self.objects.get(&name) {
      return Some(object.clone());
    }

    if let Some(ref parent) = self.parent {
      return parent.borrow_mut().get(name);
    }

    None
  }

  pub fn set(&mut self, name: String, value: &Object) {
    self.objects.insert(name, value.clone());
  }

  pub fn assign(&mut self, name: String, value: &Object) {
    if let Some(_) = self.objects.get(&name) {
      self.objects.insert(name, value.clone());
    } else if let Some(ref parent) = self.parent {
      parent.borrow_mut().assign(name, value);
    } else {
      panic!("Undefined variable '{}'", name);
    }
  }
}