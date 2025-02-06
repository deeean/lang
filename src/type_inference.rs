use std::collections::HashMap;
use crate::ast::{Expression, Statement, Type, Literal, BinaryOperator, UnaryOperator, Param, Program};

#[derive(Debug, Clone)]
struct FunctionSignature {
    parameters: Option<Vec<Type>>,
    return_type: Type,
}

#[derive(Debug)]
pub enum TypeInferenceError {
    UndefinedFunction(String),
    UndefinedVariable(String),
    ArgumentCountMismatch { expected: usize, found: usize },
    TypeMismatch { expected: Type, found: Type },
    InvalidOperatorForType { operator: BinaryOperator, r#type: Type },
    InvalidUnaryOperatorForType { operator: UnaryOperator, r#type: Type },
}

pub struct TypeInference {
    functions: HashMap<String, FunctionSignature>,
}

impl TypeInference {
    pub fn new() -> Self {
        let mut functions = HashMap::new();
        functions.insert("printf".to_string(), FunctionSignature {
            parameters: None,
            return_type: Type::Void,
        });

        TypeInference {
            functions,
        }
    }

    pub fn type_inference_program(
        &mut self,
        program: &mut Program,
    ) -> Result<(), TypeInferenceError> {
        for stmt in program.iter() {
            if let Statement::FunctionDeclaration { name, parameters, return_type, .. } = stmt {
                let param_types = parameters.iter().map(|p| p.r#type.clone()).collect();

                self.functions.insert(name.clone(), FunctionSignature {
                    parameters: Some(param_types),
                    return_type: return_type.clone(),
                });
            }
        }

        for stmt in program.iter_mut() {
            if let Statement::FunctionDeclaration { name, parameters, return_type, body } = stmt {
                let mut env = HashMap::new();
                for param in parameters {
                    env.insert(param.name.clone(), param.r#type.clone());
                }
                self.process_statements(body, &mut env, return_type)?;
            }
        }

        Ok(())
    }

    fn process_statements(
        &mut self,
        stmts: &mut Vec<Statement>,
        env: &mut HashMap<String, Type>,
        return_type: &Type,
    ) -> Result<(), TypeInferenceError> {
        for stmt in stmts {
            self.process_statement(stmt, env, return_type)?;
        }
        Ok(())
    }

    fn process_statement(
        &mut self,
        stmt: &mut Statement,
        env: &mut HashMap<String, Type>,
        return_type: &Type,
    ) -> Result<(), TypeInferenceError> {

        match stmt {
            Statement::VariableDeclaration { name, r#type: var_type, value } => {
                let value_ty = self.infer_expression(value, var_type.as_ref(), env)?;

                if let Some(declared_ty) = var_type.clone() {
                    if value_ty == Type::Unknown {
                        *var_type = Some(declared_ty.clone());
                    } else if value_ty != declared_ty {
                        return Err(TypeInferenceError::TypeMismatch {
                            expected: declared_ty.clone(),
                            found: value_ty,
                        });
                    }
                } else {
                    *var_type = Some(value_ty.clone());
                }
                env.insert(name.clone(), value_ty);
                Ok(())
            }
            Statement::Assignment { name, value } => {
                let var_ty = env.get(name)
                    .ok_or_else(|| TypeInferenceError::UndefinedVariable(name.clone()))?;
                let value_ty = self.infer_expression(value, Some(var_ty), env)?;
                if &value_ty != var_ty {
                    return Err(TypeInferenceError::TypeMismatch {
                        expected: var_ty.clone(),
                        found: value_ty,
                    });
                }
                Ok(())
            }
            Statement::While { condition, body } => {
                let cond_ty = self.infer_expression(condition, Some(&Type::Boolean), env)?;
                if cond_ty != Type::Boolean {
                    return Err(TypeInferenceError::TypeMismatch {
                        expected: Type::Boolean,
                        found: cond_ty,
                    });
                }
                self.process_statements(body, env, return_type)
            }
            Statement::If { condition, body, else_body, else_ifs } => {
                let cond_ty = self.infer_expression(condition, Some(&Type::Boolean), env)?;
                if cond_ty != Type::Boolean {
                    return Err(TypeInferenceError::TypeMismatch {
                        expected: Type::Boolean,
                        found: cond_ty,
                    });
                }
                self.process_statements(body, env, return_type)?;
                for (cond, body) in else_ifs {
                    let cond_ty = self.infer_expression(cond, Some(&Type::Boolean), env)?;
                    if cond_ty != Type::Boolean {
                        return Err(TypeInferenceError::TypeMismatch {
                            expected: Type::Boolean,
                            found: cond_ty,
                        });
                    }
                    self.process_statements(body, env, return_type)?;
                }
                if let Some(else_body) = else_body {
                    self.process_statements(else_body, env, return_type)?;
                }
                Ok(())
            }
            Statement::Return(expr) => {
                let expr_ty = self.infer_expression(expr, Some(return_type), env)?;
                if expr_ty != *return_type {
                    return Err(TypeInferenceError::TypeMismatch {
                        expected: return_type.clone(),
                        found: expr_ty,
                    });
                }
                Ok(())
            }
            Statement::Expression(expression) => {
                self.infer_expression(expression, None, env)?;
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn infer_expression(
        &mut self,
        expr: &mut Expression,
        expected_ty: Option<&Type>,
        env: &HashMap<String, Type>,
    ) -> Result<Type, TypeInferenceError> {
        match expr {
            Expression::Literal(lit) => {
                match lit {
                    Literal::Number(Some(ty), _) => Ok(ty.clone()),
                    Literal::Number(None, s) => {
                        let temp_ty = if s.contains(".") {
                            Type::F64
                        } else {
                            Type::I32
                        };

                        let ty = expected_ty.cloned().unwrap_or(temp_ty);
                        *lit = Literal::Number(Some(ty.clone()), s.clone());
                        Ok(ty)
                    }
                    Literal::String(_) => Ok(Type::String),
                    Literal::Boolean(_) => Ok(Type::Boolean),
                }
            }
            Expression::Variable(name) => {
                env.get(name)
                    .cloned()
                    .ok_or(TypeInferenceError::UndefinedVariable(name.clone()))
            }
            Expression::FunctionCall { name, arguments } => {
                println!("name: {:?}, arguments: {:?}", name, arguments);

                let sig = self.functions.get(name).cloned()
                    .ok_or(TypeInferenceError::UndefinedFunction(name.clone()))?;

                if let Some(parameters) = &sig.parameters {
                    if arguments.len() != parameters.len() {
                        return Err(TypeInferenceError::ArgumentCountMismatch {
                            expected: parameters.len(),
                            found: arguments.len(),
                        });
                    }

                    for (i, arg) in arguments.iter_mut().enumerate() {
                        let param_ty = &parameters[i];
                        let arg_ty = self.infer_expression(arg, Some(param_ty), env)?;

                        if arg_ty != *param_ty {
                            return Err(TypeInferenceError::TypeMismatch {
                                expected: param_ty.clone(),
                                found: arg_ty,
                            });
                        }
                    }
                } else {
                    for arg in arguments {
                        self.infer_expression(arg, None, env)?;
                    }
                }

                Ok(sig.return_type.clone())
            }
            Expression::Binary { left, operator, right } => {
                let mut left_ty = self.infer_expression(&mut left.clone(), None, env)?;
                let mut right_ty = self.infer_expression(&mut right.clone(), Some(&left_ty), env)?;

                println!("left_ty: {:?}, right_ty: {:?}", left_ty, right_ty);

                if left_ty != right_ty {
                    left_ty = self.infer_expression(left, Some(&right_ty), env)?;
                    right_ty = self.infer_expression(right, None, env)?;
                } else {
                    left_ty = self.infer_expression(left, None, env)?;
                    right_ty = self.infer_expression(right, Some(&left_ty), env)?;
                }

                if left_ty != right_ty {
                    return Err(TypeInferenceError::TypeMismatch {
                        expected: left_ty.clone(),
                        found: right_ty,
                    });
                }

                if !Self::is_valid_operator(operator, &left_ty) {
                    return Err(TypeInferenceError::InvalidOperatorForType {
                        operator: operator.clone(),
                        r#type: left_ty.clone(),
                    });
                }

                let result_ty = match operator {
                    BinaryOperator::Add | BinaryOperator::Subtract | BinaryOperator::Multiply |
                    BinaryOperator::Divide | BinaryOperator::Modulo => left_ty.clone(),
                    BinaryOperator::Equal | BinaryOperator::NotEqual |
                    BinaryOperator::Less | BinaryOperator::LessEqual |
                    BinaryOperator::Greater | BinaryOperator::GreaterEqual |
                    BinaryOperator::And | BinaryOperator::Or => Type::Boolean,
                };

                if let Some(expected) = expected_ty {
                    if result_ty != *expected {
                        return Err(TypeInferenceError::TypeMismatch {
                            expected: expected.clone(),
                            found: result_ty,
                        });
                    }
                }

                Ok(result_ty)
            }
            Expression::Unary { operator, operand } => {
                let operand_ty = self.infer_expression(operand, expected_ty.cloned().as_ref(), env)?;
                match operator {
                    UnaryOperator::Negate => {
                        if !matches!(
                            operand_ty,
                            Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::U128 |
                            Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::I128 | Type::ISize |
                            Type::F32 | Type::F64
                        ) {
                            return Err(TypeInferenceError::InvalidUnaryOperatorForType {
                                operator: operator.clone(),
                                r#type: operand_ty,
                            });
                        }
                        Ok(operand_ty)
                    }
                    UnaryOperator::Not => {
                        if operand_ty != Type::Boolean {
                            return Err(TypeInferenceError::InvalidUnaryOperatorForType {
                                operator: operator.clone(),
                                r#type: operand_ty,
                            });
                        }
                        Ok(Type::Boolean)
                    }
                }
            }
            _ => Ok(Type::Unknown),
        }
    }

    fn is_valid_operator(op: &BinaryOperator, ty: &Type) -> bool {
        match op {
            BinaryOperator::Add | BinaryOperator::Subtract | BinaryOperator::Multiply |
            BinaryOperator::Divide | BinaryOperator::Modulo => {
                matches!(
                ty,
                Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::U128 |
                Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::I128 | Type::ISize |
                Type::F32 | Type::F64
            )
            }
            BinaryOperator::Equal | BinaryOperator::NotEqual => {
                !matches!(ty, Type::Unknown)
            }
            BinaryOperator::Less | BinaryOperator::LessEqual |
            BinaryOperator::Greater | BinaryOperator::GreaterEqual => {
                matches!(
                ty,
                Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::U128 |
                Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::I128 | Type::ISize |
                Type::F32 | Type::F64 | Type::String
            )
            }
            BinaryOperator::And | BinaryOperator::Or => {
                ty == &Type::Boolean
            }
        }
    }
}
