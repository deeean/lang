#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpCode {
  Constant,
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  Pop,
  False,
  True,
  Equal,
  NotEqual,
  LessThan,
  LessThanOrEqual,
  GreaterThan,
  GreaterThanOrEqual,
  Positive,
  Negative,
  Not,
  Jump,
  Null,
  Call,
  GetGlobal,
  SetGlobal,
  GetLocal,
  SetLocal,
  GetBuiltin,
  GetFree,
}

impl OpCode {
  pub fn operand_widths(&self) -> Vec<u8> {
    match self {
      OpCode::Constant |
      OpCode::GetGlobal |
      OpCode::SetGlobal |
      OpCode::Jump => vec![2],

      OpCode::Add |
      OpCode::Sub |
      OpCode::Mul |
      OpCode::Div |
      OpCode::Mod |
      OpCode::Pop |
      OpCode::False |
      OpCode::True |
      OpCode::Equal |
      OpCode::NotEqual |
      OpCode::LessThan |
      OpCode::LessThanOrEqual |
      OpCode::GreaterThan |
      OpCode::GreaterThanOrEqual |
      OpCode::Positive |
      OpCode::Negative |
      OpCode::Not |
      OpCode::Null => vec![],

      OpCode::Call |
      OpCode::GetLocal |
      OpCode::SetLocal |
      OpCode::GetBuiltin |
      OpCode::GetFree => vec![1],
    }
  }
}