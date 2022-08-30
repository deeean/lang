use std::cell::RefCell;
use std::rc::Rc;
use crate::ast::{AssignOperator, BinaryOperator, Expression, Statement, UnaryOperator};
use crate::env::Env;
use crate::object::Object;

#[derive(Debug)]
pub struct Evaluator {
  pub env: Rc<RefCell<Env>>,
}

impl Evaluator {
  pub fn new(env: Rc<RefCell<Env>>) -> Self {
    Evaluator {
      env
    }
  }

  fn error(msg: String) -> Object {
    Object::Error(msg)
  }

  fn is_error(obj: &Object) -> bool {
    match obj {
      Object::Error(_) => true,
      _ => false,
    }
  }

  fn eval_binary_expression(&mut self, left: Object, op: BinaryOperator, right: Object) -> Option<Object> {
    match (left.clone(), right.clone()) {
      (Object::Number(left), Object::Number(right)) => {
        match op {
          BinaryOperator::Add => Some(Object::Number(left + right)),
          BinaryOperator::Subtract => Some(Object::Number(left - right)),
          BinaryOperator::Multiply => Some(Object::Number(left * right)),
          BinaryOperator::Divide => Some(Object::Number(left / right)),
          BinaryOperator::Equal => Some(Object::Boolean(left == right)),
          BinaryOperator::NotEqual => Some(Object::Boolean(left != right)),
          BinaryOperator::GreaterThan => Some(Object::Boolean(left > right)),
          BinaryOperator::LessThan => Some(Object::Boolean(left < right)),
          BinaryOperator::GreaterThanOrEqual => Some(Object::Boolean(left >= right)),
          BinaryOperator::LessThanOrEqual => Some(Object::Boolean(left <= right)),
        }
      }
      (Object::String(left), Object::String(right)) => {
        match op {
          BinaryOperator::Add => Some(Object::String(left + right.as_str())),
          BinaryOperator::Equal => Some(Object::Boolean(left == right)),
          BinaryOperator::NotEqual => Some(Object::Boolean(left != right)),
          _ => Some(Self::error(format!("Type mismatch: {} + {}", left, right))),
        }
      }
      (Object::Number(left), Object::String(right)) => {
        match op {
          BinaryOperator::Add => Some(Object::String(left.to_string() + right.as_str())),
          _ => Some(Self::error(format!("Type mismatch: {} + {}", left, right))),
        }
      }
      (Object::String(left), Object::Number(right)) => {
        match op {
          BinaryOperator::Add => Some(Object::String(left.to_string() + right.to_string().as_str())),
          _ => Some(Self::error(format!("Type mismatch: {} + {}", left, right))),
        }
      }
      (_, _) => Some(Self::error(format!("Unknown operator: {:?} {:?} {:?}", left, op, right))),
    }
  }

  fn eval_unary_expression(&mut self, op: UnaryOperator, right: Object) -> Option<Object> {
    match right {
      Object::Boolean(right) => {
        match op {
          UnaryOperator::Not => Some(Object::Boolean(!right)),
          _ => Some(Self::error(format!("Unknown operator: {:?}, {}", op, right))),
        }
      }
      Object::Number(right) => {
        match op {
          UnaryOperator::Positive => Some(Object::Number(right)),
          UnaryOperator::Negative => Some(Object::Number(-right)),
          _ => Some(Self::error(format!("Unknown operator: {:?}, {}", op, right))),
        }
      }
      _ => Some(Self::error(format!("Unknown operator: {:?}, {:?}", op, right))),
    }
  }

  fn eval_identifier(&mut self, name: String) -> Object {
    match self.env.borrow_mut().get(name.clone()) {
      Some(object) => object,
      None => Object::Error(format!("Identifier not found: {}", name)),
    }
  }

  fn eval_expression(&mut self, expression: Expression) -> Option<Object> {
    match expression {
      Expression::Identifier(name) => Some(self.eval_identifier(name)),
      Expression::Number(value) => Some(Object::Number(value)),
      Expression::String(value) => Some(Object::String(value)),
      Expression::Boolean(value) => Some(Object::Boolean(value)),
      Expression::Null => Some(Object::Null),
      Expression::Binary(left, op, right) => {
        let left = self.eval_expression(*left);
        let right = self.eval_expression(*right);

        match (left, right) {
          (Some(left), Some(right)) => self.eval_binary_expression(left, op, right),
          _ => None,
        }
      }
      Expression::Unary(op, right) => {
        let right = self.eval_expression(*right);

        match right {
          Some(right) => self.eval_unary_expression(op, right),
          _ => None,
        }
      }
      Expression::Call(callee, args) => Some(self.eval_call_expression(callee, args)),
    }
  }

  fn eval_assign_statement(&mut self, left: Object, assign_operator: AssignOperator, right: Object) -> Option<Object> {
    let binary_operator = match assign_operator {
      AssignOperator::Assign => return Some(right),
      AssignOperator::AddAssign => BinaryOperator::Add,
      AssignOperator::SubtractAssign => BinaryOperator::Subtract,
      AssignOperator::MultiplyAssign => BinaryOperator::Multiply,
      AssignOperator::DivideAssign => BinaryOperator::Divide,
    };

    self.eval_binary_expression(left, binary_operator, right)
  }

  fn eval_statement(&mut self, statement: Statement) -> Option<Object> {
    match statement {
      Statement::Let(name, expression) => {
        if let Some(_) = self.env.borrow_mut().get(name.clone()) {
          return Some(Self::error(format!("Identifier '{}' has already been declared", name)));
        }

        let value = match self.eval_expression(expression) {
          Some(value) => value,
          None => return None,
        };

        if Self::is_error(&value) {
          Some(value)
        } else {
          self.env.borrow_mut().set(name, &value);
          None
        }
      },
      Statement::Assign(name, assign_operator, expression) => {
        let left = self.eval_identifier(name.clone());
        let right = match self.eval_expression(expression) {
          Some(value) => value,
          None => return None,
        };

        let value = match self.eval_assign_statement(left, assign_operator, right) {
          Some(value) => value,
          None => return None,
        };

        if Self::is_error(&value) {
          Some(value)
        } else {
          self.env.borrow_mut().set(name, &value);
          None
        }
      },
      Statement::Expression(expr) => self.eval_expression(expr),
    }
  }

  pub fn eval(&mut self, program: Vec<Statement>) -> Option<Object> {
    let mut result = None;

    for statement in program {
      match self.eval_statement(statement) {
        Some(Object::Error(message)) => return Some(Object::Error(message)),
        obj => result = obj,
      }
    }

    return result
  }

  fn eval_call_expression(&mut self, callee: Box<Expression>, args: Vec<Expression>) -> Object {
    let args = args
      .iter()
      .map(|e| self.eval_expression(e.clone()).unwrap_or(Object::Null))
      .collect::<Vec<_>>();

    match self.eval_expression(*callee) {
      Some(Object::Native(expect_params_count, _, native)) => {
        return if expect_params_count == 0 || expect_params_count == args.len() as i32 {
          native(args)
        } else {
          Self::error(format!(
            "wrong number of arguments. got={}, want={}",
            args.len(),
            expect_params_count,
          ))
        }
      }
      Some(o) => return Self::error(format!("{:?} is not a function", o)),
      None => return Object::Null,
    }
  }
}

mod tests {
  use std::cell::RefCell;
  use std::rc::Rc;
  use crate::env::Env;
  use crate::evaluator::Evaluator;
  use crate::lexer::Lexer;
  use crate::native::native;
  use crate::object::Object;
  use crate::parser::Parser;

  fn eval(input: &str) -> Option<Object> {
    let mut evaluator = Evaluator::new(Rc::new(RefCell::new(Env::from(native()))));
    let tokens = Lexer::new(input).lex();
    let program = Parser::new(tokens).parse();
    let res = evaluator.eval(program);
    res
  }

  #[test]
  fn test() {
    let res = eval(r#"
    let a = 5;

    a = 1;

    println(a);
    "#);
  }
}