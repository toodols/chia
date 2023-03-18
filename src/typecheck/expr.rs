use crate::parser::ast::{BinaryOperation, BinaryOperator, Expression, Literal};

use super::{CompilerError, CompilerResult, Context, State, Type, TypecheckOutput};

pub fn typecheck_expression<'nodes, 'ctx>(
    mut ctx: &'ctx mut Context<'nodes>,
    state: State,
    expr: &'nodes Expression,
) -> CompilerResult<TypecheckOutput> {
    match expr {
        Expression::Literal(Literal::Number(_)) => Ok(Type::Number.into()),
        Expression::Literal(Literal::String(_)) => Ok(Type::String.into()),
        Expression::Literal(Literal::Boolean(_)) => Ok(Type::Boolean.into()),
        Expression::Literal(Literal::Identifier(name)) => ctx
            .symtab
            .get_variable(state.scope, name.clone())
            .map(|t| t.into())
            .ok_or(CompilerError::VariableNotFound(name.clone())),
        Expression::BinaryOperation(
            op @ BinaryOperation {
                operator: BinaryOperator::Add,
                ..
            },
        ) => {
            let TypecheckOutput { ty: left_ty, .. } =
                typecheck_expression(ctx, state.clone(), &op.left)?;
            let TypecheckOutput { ty: right_ty, .. } = typecheck_expression(ctx, state, &op.right)?;
            match (left_ty, right_ty) {
                (Type::Number, Type::Number) => Ok(Type::Number.into()),
                _ => Err(CompilerError::Unknown),
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
        Expression::Return(expr) => {
            let TypecheckOutput { ty, exits } = typecheck_expression(ctx, state.clone(), expr)?;
            if let Some(expect_ty) = state.expect_return {
                if ty == expect_ty {
                    Ok(TypecheckOutput {
                        ty: Type::Never,
                        exits: true,
                    })
                } else {
                    Err(CompilerError::MismatchedTypes(format!(
                        "expected {:?}, got {:?}",
                        expect_ty, ty
                    )))
                }
            } else {
                Err(CompilerError::AnyError(format!("Return is not expected")))
            }
        }
        _ => panic!("Not implemented for {:?}", expr),
    }
}
