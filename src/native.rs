use std::collections::HashMap;
use chrono::{Local};

use crate::object::Object;

pub fn native() -> HashMap<String, Object> {
  let mut native = HashMap::new();
  native.insert(String::from("println"), Object::Native(0, "println".to_owned(), native_println));
  native.insert(String::from("time"), Object::Native(0, "time".to_owned(),native_time));
  native.insert(String::from("typeof"), Object::Native(1, "typeof".to_owned(),native_typeof));
  native
}

fn native_println(args: Vec<Object>) -> Object {
  println!("{}", args.iter().map(|arg| arg.to_string()).collect::<Vec<_>>().join(" "));

  Object::Null
}

fn native_time(_: Vec<Object>) -> Object {
  Object::Number(Local::now().timestamp_millis() as f64)
}

fn native_typeof(args: Vec<Object>) -> Object {
  match args[0] {
    Object::Null => Object::String("null".to_owned()),
    Object::Error(_) => Object::String("error".to_owned()),
    Object::Number(_) => Object::String("number".to_owned()),
    Object::String(_) => Object::String("string".to_owned()),
    Object::Boolean(_) => Object::String("boolean".to_owned()),
    Object::Native(_, _, _) => Object::String("function".to_owned()),
  }
}