pub mod block;
pub mod for_loop;
pub mod if_expr;
use self::if_expr::typecheck_if_expr;
use crate::parser::ast::{BinaryOperation, BinaryOperator, Expression, Literal, Path};
use block::typecheck_block;
use for_loop::typecheck_for_loop;

use super::{CompilerError, CompilerResult, Context, State, Type, TypecheckOutput};

pub fn typecheck_path<'nodes, 'ctx>(
    ctx: &'ctx mut Context<'nodes>,
    state: State,
    path: &'nodes Path,
) -> CompilerResult<TypecheckOutput> {
    if path.path.len() > 1 {
        panic!()
    }
    Ok(ctx
        .symtab
        .variables
        .get_name_in_scope_chain(&ctx.symtab.parents, state.scope, &path.last())
        .and_then(|s| {
            ctx.node_id_to_symbol.insert(path.id, s.clone());
            let v = ctx.symtab.variables.get(s)?;
            Some(v.ty.clone())
        })
        .ok_or_else(|| {
            CompilerError::VariableNotFound(format!("{} at {}", path.last(), state.scope))
        })?
        .into())
}

pub fn typecheck_expression<'nodes, 'ctx>(
    ctx: &'ctx mut Context<'nodes>,
    state: State,
    expr: &'nodes Expression,
) -> CompilerResult<TypecheckOutput> {
    match expr {
        Expression::Literal(inner) => match inner {
            Literal::Unit => Ok(Type::Unit.into()),
            Literal::Number(_) => Ok(Type::Number.into()),
            Literal::String(_) => Ok(Type::String.into()),
            Literal::Boolean(_) => Ok(Type::Boolean.into()),
            _ => todo!(),
        },
        Expression::Path(path) => typecheck_path(ctx, state, path),
        Expression::BinaryOperation(inner) => {
            let TypecheckOutput {
                expr_ty: left_ty, ..
            } = typecheck_expression(ctx, state.clone(), &inner.left)?;
            let TypecheckOutput {
                expr_ty: right_ty, ..
            } = typecheck_expression(ctx, state, &inner.right)?;
            match inner {
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
        Expression::FunctionCall(inner) => {
            let ty;
            TypecheckOutput { expr_ty: ty, .. } =
                typecheck_expression(ctx, state.clone(), inner.value.as_ref())?;
            if let Type::Function(params, return_type) = ty {
                let mut v = Vec::with_capacity(params.len());
                for arg in inner.arguments.iter() {
                    let arg_ty;
                    TypecheckOutput {
                        expr_ty: arg_ty,
                        ..
                    } = typecheck_expression(ctx, state.clone(), arg)?;
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
            // Break expressions:
            // Inherit return_type
            // Use expr_ty for loop_ty
            // Inherit loop_ty

            if state.expect_break {
                let TypecheckOutput {
                    expr_ty,
                    return_ty,
                    loop_ty,
                } = typecheck_expression(ctx, state, expr)?;
                Ok(TypecheckOutput {
                    expr_ty: Type::Never,
                    return_ty,
                    loop_ty: expr_ty.union(&loop_ty)?,
                })
            } else {
                Err(CompilerError::AnyError("Break is not expected".to_owned()))
            }
        }
        Expression::Return(expr) => {
            // Return expressions:
            // Inherit return_type
            // Use expr_ty for return_type
            // Inherit loop_ty (this part i am uncertain)

            let TypecheckOutput {
                expr_ty,
                return_ty,
                loop_ty,
            } = typecheck_expression(ctx, state.clone(), expr)?;
            Ok(TypecheckOutput {
                expr_ty: Type::Never,
                return_ty: expr_ty.union(&return_ty)?,
                loop_ty,
            })
        }
        Expression::Block(block) => typecheck_block(ctx, state, block),
        Expression::IfExpression(expr) => typecheck_if_expr(ctx, state, expr),

        _ => panic!("Not implemented for {:?}", expr),
    }
}
