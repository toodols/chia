use crate::parser::ast::{Expression, Literal, BinaryOperator, BinaryOperation};

use super::{Context, Type, CompilerResult, CompilerError, State};

pub fn typecheck_expression<'a, 'ctx>(mut context: &'ctx mut Context<'a>, state: State, expr: &'a Expression) -> CompilerResult<(Type, &'ctx mut Context<'a>)>{
	match expr {
		Expression::Literal(Literal::Number(_)) => Ok((Type::Number, context)),
		Expression::Literal(Literal::String(_)) => Ok((Type::String, context)),
		Expression::Literal(Literal::Boolean(_)) => Ok((Type::Boolean, context)),
		Expression::Literal(Literal::Identifier(name)) => {
			context.symtab.get_variable(state.scope, name.clone()).map(|t|(t,context)).ok_or(CompilerError::VariableNotFound(name.clone()))
		}
		Expression::BinaryOperation(op @ BinaryOperation{operator: BinaryOperator::Add, .. }) => {
			let (left_ty, context) = typecheck_expression(context, state.clone(), &op.left)?;
			let (right_ty, context) = typecheck_expression(context, state, &op.right)?;
			match (left_ty, right_ty) {
				(Type::Number, Type::Number) => Ok((Type::Number, context)),
				_=> Err(CompilerError::Unknown)
			}
		}
		Expression::FunctionCall(fn_call)=>{
			let t;
			(t,context) = typecheck_expression(context, state.clone(), fn_call.value.as_ref())?;
			if let Type::Function(params, return_type) = t {
				let mut v = Vec::with_capacity(params.len());
				for arg in fn_call.arguments.iter() {
					let arg_ty;
					(arg_ty, context) = typecheck_expression(context, state.clone(), arg)?;
					v.push(arg_ty);
				}
				if v == params {
					Ok((*return_type, context))
				} else {
					Err(CompilerError::MismatchedTypes(format!("expected {:?}, got {:?}", params, v)))
				}
			} else {
				Err(CompilerError::MismatchedTypes(format!("expected function type, got {:?}", t)))			}
		}
		Expression::Return(expr) => {
			let (ty, context) = typecheck_expression(context, state.clone(), expr)?;
			if let Some(expect_ty) = state.expect_return {
				if ty == expect_ty {
					Ok((Type::Unit, context))
				} else {
					Err(CompilerError::MismatchedTypes(format!("expected {:?}, got {:?}", expect_ty, ty)))
				}
			} else {
				Err(CompilerError::AnyError(format!("Return is not expected")))
			}
			
		}
		_=>panic!("Not implemented for {:?}", expr)
	}
}