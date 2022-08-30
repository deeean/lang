use std::fmt;
use std::fmt::{Formatter, Write};
use colored::Colorize;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
  Null,
  Error(String),
  Number(f64),
  String(String),
  Boolean(bool),
  Native(i32, String, fn(Vec<Object>) -> Object),
}

impl fmt::Display for Object {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Object::Null => {
        f.write_str(format!("{}", "null".cyan()).as_str())
      },
      Object::Error(msg) => write!(f, "{}", msg),
      Object::Number(n) => write!(f, "{}", n),
      Object::String(s) => write!(f, "{}", s),
      Object::Boolean(b) => write!(f, "{}", b),
      Object::Native(_, name, _) => write!(f, "{}() {{ [native code] }}", name),
    }
  }
}

impl Eq for Object {

}