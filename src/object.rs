#[derive(Debug, Clone, PartialEq)]
pub enum Object {
  Null,
  Number(f64),
  String(String),
  Boolean(bool),
}