use std::collections::HashMap;
use std::path::Path;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::{AddressSpace, OptimizationLevel};
use inkwell::basic_block::BasicBlock;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetData, TargetMachine};
use inkwell::types::{BasicTypeEnum, BasicType, AnyTypeEnum, AnyType};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue};
use crate::ast::{Type, BinaryOperator, Expression, Literal, Statement, UnaryOperator};

struct LoopStack<'ctx> {
    condition_block: BasicBlock<'ctx>,
    end_block: BasicBlock<'ctx>,
}

pub struct Compiler<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    variables: HashMap<String, (AnyTypeEnum<'ctx>, PointerValue<'ctx>)>,
    loop_stack: Vec<LoopStack<'ctx>>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("main");
        let builder = context.create_builder();

        let ptr_i8_type = context.ptr_type(AddressSpace::default());
        let printf_type = context.i32_type().fn_type(&[ptr_i8_type.into()], true);
        module.add_function("printf", printf_type, Some(Linkage::External));

        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();

        Compiler {
            context,
            module,
            builder,
            variables: HashMap::new(),
            loop_stack: Vec::new(),
            execution_engine,
        }
    }

    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    fn build_string_concat(&mut self, a: BasicValueEnum<'ctx>, b: BasicValueEnum<'ctx>) -> Result<BasicValueEnum<'ctx>, String> {
        let strlen_func = self.get_function("strlen").unwrap_or_else(|| {
            let str_ptr_type = self.context.ptr_type(AddressSpace::default());
            let fn_type = self.context.i64_type().fn_type(&[str_ptr_type.into()], false);
            self.module.add_function("strlen", fn_type, Some(Linkage::External))
        });

        let malloc_func = self.get_function("malloc").unwrap_or_else(|| {
            let fn_type = self.context.ptr_type(AddressSpace::default()).fn_type(&[self.context.i64_type().into()], false);
            self.module.add_function("malloc", fn_type, Some(Linkage::External))
        });

        let strcpy_func = self.get_function("strcpy").unwrap_or_else(|| {
            let str_ptr_type = self.context.ptr_type(AddressSpace::default());
            let fn_type = str_ptr_type.fn_type(&[str_ptr_type.into(), str_ptr_type.into()], false);
            self.module.add_function("strcpy", fn_type, Some(Linkage::External))
        });

        let strcat_func = self.get_function("strcat").unwrap_or_else(|| {
            let str_ptr_type = self.context.ptr_type(AddressSpace::default());
            let fn_type = str_ptr_type.fn_type(&[str_ptr_type.into(), str_ptr_type.into()], false);
            self.module.add_function("strcat", fn_type, Some(Linkage::External))
        });

        let str1_ptr = a.into_pointer_value();
        let str2_ptr = b.into_pointer_value();

        let str1_len = self.builder.build_call(strlen_func, &[str1_ptr.into()], "str1_len").unwrap()
            .try_as_basic_value().left().unwrap().into_int_value();

        let str2_len = self.builder.build_call(strlen_func, &[str2_ptr.into()], "str2_len").unwrap()
            .try_as_basic_value().left().unwrap().into_int_value();

        let total_len = self.builder.build_int_add(str1_len, str2_len, "total_len").unwrap();
        let total_len = self.builder.build_int_add(total_len, self.context.i64_type().const_int(1, false), "total_len_plus_one").unwrap();

        let new_str_ptr = self.builder.build_call(malloc_func, &[total_len.into()], "new_str").unwrap()
            .try_as_basic_value().left().unwrap().into_pointer_value();

        self.builder.build_call(strcpy_func, &[new_str_ptr.into(), str1_ptr.into()], "call_strcpy").unwrap();

        self.builder.build_call(strcat_func, &[new_str_ptr.into(), str2_ptr.into()], "call_strcat").unwrap();

        Ok(new_str_ptr.into())
    }

    fn compile_expression(&mut self, expression: &Expression) -> Result<Option<BasicValueEnum<'ctx>>, String> {

        match expression {
            Expression::Unary { operator, operand } => {
                let operand = self.compile_expression(operand)?;

                match operator {
                    UnaryOperator::Negate => {
                        if operand.unwrap().is_int_value() {
                            Ok(Some(self.builder.build_int_neg(operand.unwrap().into_int_value(), "tmpneg").unwrap().into()))
                        } else if operand.unwrap().is_float_value() {
                            Ok(Some(self.builder.build_float_neg(operand.unwrap().into_float_value(), "tmpneg").unwrap().into()))
                        } else {
                            Err("Unsupported types for negation".to_string())
                        }
                    }
                    UnaryOperator::Not => {
                        Ok(Some(self.builder.build_not(operand.unwrap().into_int_value(), "tmpnot").unwrap().into()))
                    }
                    _ => {
                        Err(format!("Unsupported operator: {:?}", operator))
                    }
                }
            }
            Expression::Binary { left, operator, right } => {
                let lhs = self.compile_expression(left)?;
                let rhs = self.compile_expression(right)?;

                match operator {
                    BinaryOperator::Add => {
                        if lhs.unwrap().is_int_value() && rhs.unwrap().is_int_value() {
                            Ok(Some(self.builder.build_int_add(lhs.unwrap().into_int_value(), rhs.unwrap().into_int_value(), "tmpadd").unwrap().into()))
                        } else if lhs.unwrap().is_float_value() && rhs.unwrap().is_float_value() {
                            Ok(Some(self.builder.build_float_add(lhs.unwrap().into_float_value(), rhs.unwrap().into_float_value(), "tmpadd").unwrap().into()))
                        } else if lhs.unwrap().is_pointer_value() && rhs.unwrap().is_pointer_value() {
                            self.build_string_concat(lhs.unwrap(), rhs.unwrap()).map(Some)
                        } else {
                            Err("Unsupported types for addition".to_string())
                        }
                    }
                    BinaryOperator::Subtract => {
                        if lhs.unwrap().is_int_value() && rhs.unwrap().is_int_value() {
                            Ok(Some(self.builder.build_int_sub(lhs.unwrap().into_int_value(), rhs.unwrap().into_int_value(), "tmpsub").unwrap().into()))
                        } else if lhs.unwrap().is_float_value() && rhs.unwrap().is_float_value() {
                            Ok(Some(self.builder.build_float_sub(lhs.unwrap().into_float_value(), rhs.unwrap().into_float_value(), "tmpsub").unwrap().into()))
                        } else {
                            Err("Unsupported types for subtraction".to_string())
                        }
                    }
                    BinaryOperator::Multiply => {
                        if lhs.unwrap().is_int_value() && rhs.unwrap().is_int_value() {
                            Ok(Some(self.builder.build_int_mul(lhs.unwrap().into_int_value(), rhs.unwrap().into_int_value(), "tmpmul").unwrap().into()))
                        } else if lhs.unwrap().is_float_value() && rhs.unwrap().is_float_value() {
                            Ok(Some(self.builder.build_float_mul(lhs.unwrap().into_float_value(), rhs.unwrap().into_float_value(), "tmpmul").unwrap().into()))
                        } else {
                            Err("Unsupported types for multiplication".to_string())
                        }
                    }
                    BinaryOperator::Modulo => {
                        if lhs.unwrap().is_int_value() && rhs.unwrap().is_int_value() {
                            Ok(Some(self.builder.build_int_signed_rem(lhs.unwrap().into_int_value(), rhs.unwrap().into_int_value(), "tmpmod").unwrap().into()))
                        } else if lhs.unwrap().is_float_value() && rhs.unwrap().is_float_value() {
                            Ok(Some(self.builder.build_float_rem(lhs.unwrap().into_float_value(), rhs.unwrap().into_float_value(), "tmpmod").unwrap().into()))
                        } else {
                            Err("Unsupported types for modulo".to_string())
                        }
                    }
                    BinaryOperator::Divide => {
                        if lhs.unwrap().is_int_value() && rhs.unwrap().is_int_value() {
                            Ok(Some(self.builder.build_int_signed_div(lhs.unwrap().into_int_value(), rhs.unwrap().into_int_value(), "tmpdiv").unwrap().into()))
                        } else if lhs.unwrap().is_float_value() && rhs.unwrap().is_float_value() {
                            Ok(Some(self.builder.build_float_div(lhs.unwrap().into_float_value(), rhs.unwrap().into_float_value(), "tmpdiv").unwrap().into()))
                        } else {
                            Err("Unsupported types for division".to_string())
                        }
                    }
                    BinaryOperator::Greater => {
                        if lhs.unwrap().is_int_value() && rhs.unwrap().is_int_value() {
                            Ok(Some(self.builder.build_int_compare(inkwell::IntPredicate::SGT, lhs.unwrap().into_int_value(), rhs.unwrap().into_int_value(), "tmpgt").unwrap().into()))
                        } else if lhs.unwrap().is_float_value() && rhs.unwrap().is_float_value() {
                            Ok(Some(self.builder.build_float_compare(inkwell::FloatPredicate::OGT, lhs.unwrap().into_float_value(), rhs.unwrap().into_float_value(), "tmpgt").unwrap().into()))
                        } else {
                            Err("Unsupported types for greater than".to_string())
                        }
                    }
                    BinaryOperator::GreaterEqual => {
                        if lhs.unwrap().is_int_value() && rhs.unwrap().is_int_value() {
                            Ok(Some(self.builder.build_int_compare(inkwell::IntPredicate::SGE, lhs.unwrap().into_int_value(), rhs.unwrap().into_int_value(), "tmpge").unwrap().into()))
                        } else if lhs.unwrap().is_float_value() && rhs.unwrap().is_float_value() {
                            Ok(Some(self.builder.build_float_compare(inkwell::FloatPredicate::OGE, lhs.unwrap().into_float_value(), rhs.unwrap().into_float_value(), "tmpge").unwrap().into()))
                        } else {
                            Err("Unsupported types for greater than or equal".to_string())
                        }
                    }
                    BinaryOperator::Less => {
                        if lhs.unwrap().is_int_value() && rhs.unwrap().is_int_value() {
                            Ok(Some(self.builder.build_int_compare(inkwell::IntPredicate::SLT, lhs.unwrap().into_int_value(), rhs.unwrap().into_int_value(), "tmplt").unwrap().into()))
                        } else if lhs.unwrap().is_float_value() && rhs.unwrap().is_float_value() {
                            Ok(Some(self.builder.build_float_compare(inkwell::FloatPredicate::OLT, lhs.unwrap().into_float_value(), rhs.unwrap().into_float_value(), "tmplt").unwrap().into()))
                        } else {
                            Err("Unsupported types for less than".to_string())
                        }
                    }
                    BinaryOperator::LessEqual => {
                        if lhs.unwrap().is_int_value() && rhs.unwrap().is_int_value() {
                            Ok(Some(self.builder.build_int_compare(inkwell::IntPredicate::SLE, lhs.unwrap().into_int_value(), rhs.unwrap().into_int_value(), "tmple").unwrap().into()))
                        } else if lhs.unwrap().is_float_value() && rhs.unwrap().is_float_value() {
                            Ok(Some(self.builder.build_float_compare(inkwell::FloatPredicate::OLE, lhs.unwrap().into_float_value(), rhs.unwrap().into_float_value(), "tmple").unwrap().into()))
                        } else {
                            Err("Unsupported types for less than or equal".to_string())
                        }
                    }
                    BinaryOperator::Equal => {
                        if lhs.unwrap().is_int_value() && rhs.unwrap().is_int_value() {
                            Ok(Some(self.builder.build_int_compare(inkwell::IntPredicate::EQ, lhs.unwrap().into_int_value(), rhs.unwrap().into_int_value(), "tmpeq").unwrap().into()))
                        } else if lhs.unwrap().is_float_value() && rhs.unwrap().is_float_value() {
                            Ok(Some(self.builder.build_float_compare(inkwell::FloatPredicate::OEQ, lhs.unwrap().into_float_value(), rhs.unwrap().into_float_value(), "tmpeq").unwrap().into()))
                        } else {
                            Err("Unsupported types for equal".to_string())
                        }
                    }
                    BinaryOperator::And => {
                        Ok(Some(self.builder.build_and(lhs.unwrap().into_int_value(), rhs.unwrap().into_int_value(), "tmpand").unwrap().into()))
                    }
                    BinaryOperator::Or => {
                        Ok(Some(self.builder.build_or(lhs.unwrap().into_int_value(), rhs.unwrap().into_int_value(), "tmpor").unwrap().into()))
                    }
                    _ => {
                        Err(format!("Unsupported operator: {:?}", operator))
                    }
                }
            }
            Expression::FunctionCall { name, arguments } => {
                match self.get_function(&name) {
                    Some(function) => {
                        let mut compiled_args = Vec::with_capacity(arguments.len());

                        for arg in arguments {
                            compiled_args.push(self.compile_expression(arg)?);
                        }

                        let argsv: Vec<BasicMetadataValueEnum> =
                            compiled_args.iter().map(|&val| val.unwrap().into()).collect();

                        match self.builder
                            .build_call(function, &argsv, "tmp")
                            .unwrap()
                            .try_as_basic_value()
                            .left() {
                            Some(value) => {
                                Ok(Some(value))
                            }
                            None => {
                                Ok(None)
                            }
                        }

                    }
                    None => {
                        Err(format!("Function not found: {}", name))
                    }
                }
            }
            Expression::Literal(literal) => {
                match literal {
                    Literal::Boolean(value) => {
                        Ok(Some(self.context.bool_type().const_int(*value as u64, false).into()))
                    }
                    Literal::Number(r#type, value) => {
                        if let Some(r#type) = r#type {
                            match r#type {
                                Type::U8 => {
                                    Ok(Some(self.context.i8_type().const_int(value.parse().unwrap(), false).into()))
                                }
                                Type::U16 => {
                                    Ok(Some(self.context.i16_type().const_int(value.parse().unwrap(), false).into()))
                                }
                                Type::U32 => {
                                    Ok(Some(self.context.i32_type().const_int(value.parse().unwrap(), false).into()))
                                }
                                Type::U64 => {
                                    Ok(Some(self.context.i64_type().const_int(value.parse().unwrap(), false).into()))
                                }
                                Type::U128 => {
                                    Ok(Some(self.context.i128_type().const_int(value.parse().unwrap(), false).into()))
                                }
                                Type::USize => {
                                    Ok(Some(self.context.ptr_sized_int_type(self.execution_engine.get_target_data(), Some(AddressSpace::default())).const_int(value.parse().unwrap(), false).into()))
                                }
                                Type::I8 => {
                                    Ok(Some(self.context.i8_type().const_int(value.parse().unwrap(), true).into()))
                                }
                                Type::I16 => {
                                    Ok(Some(self.context.i16_type().const_int(value.parse().unwrap(), true).into()))
                                }
                                Type::I32 => {
                                    Ok(Some(self.context.i32_type().const_int(value.parse().unwrap(), true).into()))
                                }
                                Type::I64 => {
                                    Ok(Some(self.context.i64_type().const_int(value.parse().unwrap(), true).into()))
                                }
                                Type::I128 => {
                                    Ok(Some(self.context.i128_type().const_int(value.parse().unwrap(), true).into()))
                                }
                                Type::ISize => {
                                    Ok(Some(self.context.ptr_sized_int_type(self.execution_engine.get_target_data(), Some(AddressSpace::default())).const_int(value.parse().unwrap(), true).into()))
                                }
                                Type::F32 => {
                                    Ok(Some(self.context.f32_type().const_float(value.parse().unwrap()).into()))
                                }
                                Type::F64 => {
                                    Ok(Some(self.context.f64_type().const_float(value.parse().unwrap()).into()))
                                }
                                _ => {
                                    Err("Unsupported number type".to_string())
                                }
                            }
                        } else {
                            Err("Number type not specified".to_string())
                        }
                    }
                    Literal::String(value) => {
                        let string_ptr = self.builder.build_global_string_ptr(&value, "fmt").unwrap();
                        Ok(Some(string_ptr.as_pointer_value().into()))
                    }
                    _ => {
                        Err("Unsupported literal".to_string())
                    }
                }
            }
            Expression::Variable(name) => {
                if let Some((pointer_type, value)) = self.variables.get(name) {

                    let basic_type_enum = match pointer_type {
                        AnyTypeEnum::IntType(int_type) => BasicTypeEnum::IntType(*int_type),
                        AnyTypeEnum::PointerType(ptr_type) => BasicTypeEnum::PointerType(*ptr_type),
                        AnyTypeEnum::ArrayType(array_type) => BasicTypeEnum::ArrayType(*array_type),
                        AnyTypeEnum::StructType(struct_type) => BasicTypeEnum::StructType(*struct_type),
                        AnyTypeEnum::VectorType(vector_type) => BasicTypeEnum::VectorType(*vector_type),
                        AnyTypeEnum::FloatType(float_type) => BasicTypeEnum::FloatType(*float_type),
                        _ => {
                            return Err("Unsupported type".to_string());
                        }
                    };


                    Ok(self.builder.build_load(basic_type_enum, *value, &name).unwrap().into())
                } else {
                    Err(format!("Variable not found: {}", name))
                }
            }
            expr => {
                println!("{:?}", expr);
                Err("Unsupported expression".to_string())
            }
        }
    }

    fn get_value_type_by_type(&self, r#type: &Type) -> AnyTypeEnum<'ctx> {
        match r#type {
            Type::U8 => self.context.i8_type().into(),
            Type::U16 => self.context.i16_type().into(),
            Type::U32 => self.context.i32_type().into(),
            Type::U64 => self.context.i64_type().into(),
            Type::U128 => self.context.i128_type().into(),
            Type::USize => self.context.ptr_sized_int_type(self.execution_engine.get_target_data(), Some(AddressSpace::default())).into(),
            Type::I8 => self.context.i8_type().into(),
            Type::I16 => self.context.i16_type().into(),
            Type::I32 => self.context.i32_type().into(),
            Type::I64 => self.context.i64_type().into(),
            Type::I128 => self.context.i128_type().into(),
            Type::ISize => self.context.ptr_sized_int_type(self.execution_engine.get_target_data(), Some(AddressSpace::default())).into(),
            Type::F32 => self.context.f32_type().into(),
            Type::F64 => self.context.f64_type().into(),
            Type::Void => self.context.void_type().into(),
            Type::Boolean => self.context.bool_type().into(),
            Type::String => self.context.ptr_type(AddressSpace::default()).into(),
            _ => {
                panic!("Unsupported type");
            }
        }
    }

    fn any_value_type_to_basic_type_enum(&self, value_type: AnyTypeEnum<'ctx>) -> BasicTypeEnum<'ctx> {
        match value_type {
            AnyTypeEnum::IntType(int_type) => BasicTypeEnum::IntType(int_type),
            AnyTypeEnum::PointerType(ptr_type) => BasicTypeEnum::PointerType(ptr_type),
            AnyTypeEnum::ArrayType(array_type) => BasicTypeEnum::ArrayType(array_type),
            AnyTypeEnum::StructType(struct_type) => BasicTypeEnum::StructType(struct_type),
            AnyTypeEnum::VectorType(vector_type) => BasicTypeEnum::VectorType(vector_type),
            AnyTypeEnum::FloatType(float_type) => BasicTypeEnum::FloatType(float_type),
            _ => {
                panic!("Unsupported type");
            }
        }
    }

    fn compile_statement(&mut self, statement: &Statement) -> Result<(), String> {
        match statement {
            Statement::Assignment {
                name,
                value,
            } => {
                let value = self.compile_expression(&value)?.unwrap();
                let (_, alloca) = self.variables.get(name).ok_or(format!("Variable not found: {}", name))?;

                self.builder.build_store(*alloca, value).unwrap();

                Ok(())
            }
            Statement::VariableDeclaration { name, r#type, value } => {
                let value = self.compile_expression(&value)?;
                let value_type = match r#type {
                    Some(r#type) => self.get_value_type_by_type(&r#type),
                    None => value.unwrap().get_type().as_any_type_enum(),
                };
                let basic_type_enum = self.any_value_type_to_basic_type_enum(value_type);
                let alloca = self.builder.build_alloca(basic_type_enum, &name).unwrap();
                self.builder.build_store(alloca, value.unwrap()).unwrap();
                self.variables.insert(name.clone(), (value_type, alloca));

                Ok(())
            }
            Statement::If { condition, body, else_ifs, else_body } => {
                let current_function = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();

                let if_cond_block = self
                    .context
                    .append_basic_block(current_function, "if_cond");
                let if_body_block = self
                    .context
                    .append_basic_block(current_function, "if_body");
                let end_block = self
                    .context
                    .append_basic_block(current_function, "if_end");

                let else_ifs_blocks: Vec<_> = else_ifs
                    .into_iter()
                    .map(|(cond_expr, body)| {
                        let cond_block = self
                            .context
                            .append_basic_block(current_function, "else_if_cond");
                        let body_block = self
                            .context
                            .append_basic_block(current_function, "else_if_body");
                        (cond_expr, body, cond_block, body_block)
                    })
                    .collect();

                let else_body_block = else_body
                    .as_ref()
                    .map(|_| self.context.append_basic_block(current_function, "else_body"));

                self.builder
                    .build_unconditional_branch(if_cond_block)
                    .unwrap();

                self.builder.position_at_end(if_cond_block);
                let cond_value = self.compile_expression(condition)?;
                let cond_bool = self
                    .builder
                    .build_int_compare(
                        inkwell::IntPredicate::NE,
                        cond_value.unwrap().into_int_value(),
                        self.context.bool_type().const_zero(),
                        "tmp",
                    )
                    .unwrap();

                let main_else_dest = if !else_ifs_blocks.is_empty() {
                    else_ifs_blocks[0].2
                } else {
                    else_body_block.unwrap_or(end_block)
                };

                self.builder
                    .build_conditional_branch(cond_bool, if_body_block, main_else_dest)
                    .unwrap();

                self.builder.position_at_end(if_body_block);
                for stmt in body {
                    self.compile_statement(stmt)?;
                }

                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder
                        .build_unconditional_branch(end_block)
                        .unwrap();
                }

                for i in 0..else_ifs_blocks.len() {
                    let (cond_expr, body, cond_block, body_block) = &else_ifs_blocks[i];

                    self.builder.position_at_end(*cond_block);
                    let cond_value = self.compile_expression(cond_expr)?;
                    let cond_bool = self
                        .builder
                        .build_int_compare(
                            inkwell::IntPredicate::NE,
                            cond_value.unwrap().into_int_value(),
                            self.context.bool_type().const_zero(),
                            "tmp",
                        )
                        .unwrap();

                    let next_else_dest = if i < else_ifs_blocks.len() - 1 {
                        else_ifs_blocks[i + 1].2
                    } else {
                        else_body_block.unwrap_or(end_block)
                    };

                    self.builder
                        .build_conditional_branch(cond_bool, *body_block, next_else_dest)
                        .unwrap();

                    self.builder.position_at_end(*body_block);
                    for stmt in &**body {
                        self.compile_statement(stmt)?;
                    }
                    if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                        self.builder
                            .build_unconditional_branch(end_block)
                            .unwrap();
                    }
                }

                if let Some(else_body_block) = else_body_block {
                    self.builder.position_at_end(else_body_block);

                    if let Some(else_body) = else_body {
                        for stmt in else_body {
                            self.compile_statement(stmt)?;
                        }
                    }

                    if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                        self.builder
                            .build_unconditional_branch(end_block)
                            .unwrap();
                    }
                }

                self.builder.position_at_end(end_block);

                Ok(())

            }
            Statement::While { condition, body } => {
                let current_function = self.builder.get_insert_block().unwrap().get_parent().unwrap();

                let condition_block = self.context.append_basic_block(current_function, "while_condition");
                let body_block = self.context.append_basic_block(current_function, "while_body");
                let end_block = self.context.append_basic_block(current_function, "while_end");

                self.loop_stack.push(LoopStack {
                    condition_block,
                    end_block,
                });

                self.builder.build_unconditional_branch(condition_block).unwrap();

                self.builder.position_at_end(condition_block);

                let condition_value = self.compile_expression(condition)?;
                let condition_bool = self.builder.build_int_compare(inkwell::IntPredicate::NE, condition_value.unwrap().into_int_value(), self.context.bool_type().const_zero(), "tmp").unwrap();

                self.builder.build_conditional_branch(condition_bool, body_block, end_block).unwrap();

                self.builder.position_at_end(body_block);

                for stmt in body {
                    self.compile_statement(stmt)?;
                }

                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_unconditional_branch(condition_block).unwrap();
                }

                self.loop_stack.pop();

                self.builder.position_at_end(end_block);

                Ok(())
            }
            Statement::FunctionDeclaration { name, parameters, return_type, body } => {
                let params_pointer_types: Vec<_> = parameters
                    .iter()
                    .map(|param| {
                        self.any_value_type_to_basic_type_enum(self.get_value_type_by_type(&param.r#type)).into()
                    })
                    .collect();

                let return_pointer_type = self.get_value_type_by_type(&return_type);

                let fn_type = if return_type == &crate::ast::Type::Void {
                    self.context.void_type().fn_type(&params_pointer_types, false)
                } else {
                    let basic_type_enum = self.any_value_type_to_basic_type_enum(return_pointer_type);
                    basic_type_enum.fn_type(&params_pointer_types, false)
                };
                let function = self.module.add_function(&name, fn_type, None);

                let entry_block = self.context.append_basic_block(function, "entry");
                self.builder.position_at_end(entry_block);

                let old_variables = std::mem::take(&mut self.variables);

                for (i, param) in parameters.into_iter().enumerate() {
                    let pointer_type = self.get_value_type_by_type(&param.r#type);

                    let param_value = function
                        .get_nth_param(i as u32)
                        .expect("Expected parameter");

                    param_value.set_name(&param.name);

                    let alloca = self.builder
                        .build_alloca(self.any_value_type_to_basic_type_enum(pointer_type), &param.name)
                        .expect("Failed to build alloca");

                    self.builder.build_store(alloca, param_value)
                        .expect("Failed to build store");

                    self.variables.insert(param.name.clone(), (pointer_type, alloca));
                }

                for stmt in body {
                    self.compile_statement(stmt)?;
                }

                if return_type == &crate::ast::Type::Void {
                    if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                        self.builder.build_return(None).unwrap();
                    }
                } else {
                    if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                        self.builder.build_unreachable().unwrap();
                    }
                }

                self.variables = old_variables;

                Ok(())
            }
            Statement::Break => {
                if let Some(&ref loop_stack) = self.loop_stack.last() {
                    if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                        self.builder.build_unconditional_branch(loop_stack.end_block).unwrap();
                    }
                    Ok(())
                } else {
                    Err("Break statement outside of loop".to_string())
                }
            }
            Statement::Continue => {
                if let Some(&ref loop_stack) = self.loop_stack.last() {
                    if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                        self.builder.build_unconditional_branch(loop_stack.condition_block).unwrap();
                    }
                    Ok(())
                } else {
                    Err("Continue statement outside of loop".to_string())
                }
            }
            Statement::Expression(expression) => {
                self.compile_expression(expression)?;
                Ok(())
            }
            Statement::Return(expression) => {
                let value = self.compile_expression(expression)?;
                self.builder.build_return(Some(&value.unwrap())).unwrap();
                Ok(())
            }
            _ => {
                Err("Unsupported statement".to_string())
            }
        }
    }

    pub fn compile(&mut self, program: Vec<Statement>) -> Result<(), String> {
        for statement in program {
            self.compile_statement(&statement)?;
        }

        self.module.print_to_stderr();


        println!("==================== Execution ====================");

        unsafe {
            let main_ptr = self.execution_engine.get_function_address("main").unwrap();
            let main_fn: extern "C" fn() = std::mem::transmute(main_ptr);
            main_fn();
        }

        Ok(())
    }
}
