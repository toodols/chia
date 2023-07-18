pub mod block;
pub mod for_loop;
pub mod if_expr;
use self::if_expr::typecheck_if_expr;
use crate::parser::ast::{BinaryOperation, BinaryOperator, Expression, Literal};
use block::typecheck_block;
use for_loop::typecheck_for_loop;

use super::{CompilerError, CompilerResult, Context, State, Type, TypecheckOutput};

pub fn typecheck_expression<'nodes, 'ctx>(
    ctx: &'ctx mut Context<'nodes>,
    state: State,
    expr: &'nodes Expression,
) -> CompilerResult<TypecheckOutput> {
    match expr {
        Expression::Literal(Literal::Unit) => Ok(Type::Unit.into()),
        Expression::Literal(Literal::Number(_)) => Ok(Type::Number.into()),
        Expression::Literal(Literal::String(_)) => Ok(Type::String.into()),
        Expression::Literal(Literal::Boolean(_)) => Ok(Type::Boolean.into()),
        Expression::Path(path) => ctx
            .symtab
            .get_variable(state.scope, path.ident())
            .map(|t| t.into())
            .ok_or_else(|| CompilerError::VariableNotFound(path.ident().clone())),
        Expression::BinaryOperation(op) => {
            let TypecheckOutput { ty: left_ty, .. } =
                typecheck_expression(ctx, state.clone(), &op.left)?;
            let TypecheckOutput { ty: right_ty, .. } = typecheck_expression(ctx, state, &op.right)?;
            match op {
                BinaryOperation {
                    operator:
                        BinaryOperator::Add
                        | BinaryOperator::Mul
                        | BinaryOperator::Sub
                        | BinaryOperator::Div,
                    ..
                } => match (left_ty, right_ty) {
                    (Type::Number, Type::Number) => Ok(Type::Number.into()),
                    _ => Err(CompilerError::Unknown),
                },
                BinaryOperation {
                    operator: BinaryOperator::Equal,
                    ..
                } => {
                    if left_ty == right_ty {
                        Ok(Type::Boolean.into())
                    } else {
                        Err(CompilerError::AnyError(format!(
                            "Cannot compare {left_ty:?} and {right_ty:?}"
                        )))
                    }
                }
                BinaryOperation {
                    operator: BinaryOperator::Range,
                    ..
                } => {
                    if left_ty == Type::Number && right_ty == Type::Number {
                        Ok(Type::Range.into())
                    } else {
                        Err(CompilerError::AnyError(format!(
                            "Cannot create range from {left_ty:?} and {right_ty:?}"
                        )))
                    }
                }
                _ => panic!("unimplemented"),
            }
        }
        Expression::FunctionCall(fn_call) => {
            let ty;
            TypecheckOutput { ty, .. } =
                typecheck_expression(ctx, state.clone(), fn_call.value.as_ref())?;
            if let Type::Function(params, return_type) = ty {
                let mut v = Vec::with_capacity(params.len());
                for arg in fn_call.arguments.iter() {
                    let arg_ty;
                    TypecheckOutput { ty: arg_ty, .. } =
                        typecheck_expression(ctx, state.clone(), arg)?;
                    v.push(arg_ty);
                }
                if v == params {
                    Ok((*return_type).into())
                } else {
                    Err(CompilerError::MismatchedTypes(format!(
                        "expected {:?}, got {:?}",
                        params, v
                    )))
                }
            } else {
                Err(CompilerError::MismatchedTypes(format!(
                    "expected function type, got {:?}",
                    ty
                )))
            }
        }
        Expression::ForLoop(expr) => typecheck_for_loop(ctx, state, expr),
        Expression::Break(expr) => {
            if state.expect_break {
                let TypecheckOutput { ty, exit_ty } = typecheck_expression(ctx, state, expr)?;
                Ok(TypecheckOutput {
                    ty: Type::Never,
                    exit_ty: ty.union(&exit_ty)?,
                })
            } else {
                Err(CompilerError::AnyError("Break is not expected".to_owned()))
            }
        }
        Expression::Return(expr) => {
            let TypecheckOutput { ty, .. } = typecheck_expression(ctx, state.clone(), expr)?;
            if let Some(expect_ty) = state.expect_return_type {
                if ty == expect_ty {
                    Ok(TypecheckOutput {
                        ty: Type::Never,
                        exit_ty: Type::Never,
                    })
                } else {
                    Err(CompilerError::MismatchedTypes(format!(
                        "expected {:?}, got {:?}",
                        expect_ty, ty
                    )))
                }
            } else {
                Err(CompilerError::AnyError(
                    "Return is not expected".to_string(),
                ))
            }
        }
        Expression::Block(block) => typecheck_block(ctx, state, block),
        Expression::IfExpression(expr) => typecheck_if_expr(ctx, state, expr),

        _ => panic!("Not implemented for {:?}", expr),
    }
}
