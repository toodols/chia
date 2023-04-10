use crate::parser::ast::{BinaryOperation, BinaryOperator, Expression, Literal};

use super::{
    typecheck_block, CompilerError, CompilerResult, Context, NodeRef, State, Symbol, Type,
    TypecheckOutput,
};

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
        Expression::Literal(Literal::Identifier(name)) => ctx
            .symtab
            .get_variable(state.scope, name.clone())
            .map(|t| t.into())
            .ok_or(CompilerError::VariableNotFound(name.clone())),
        Expression::BinaryOperation(op) => {
            let TypecheckOutput { ty: left_ty, .. } =
                typecheck_expression(ctx, state.clone(), &op.left)?;
            let TypecheckOutput { ty: right_ty, .. } = typecheck_expression(ctx, state, &op.right)?;
            match op {
                BinaryOperation {
                    operator: BinaryOperator::Add | BinaryOperator::Mul | BinaryOperator::Sub | BinaryOperator::Div,
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
        Expression::ForLoop(expr) => {
            let inner_scope = expr.body.node_id;
            let t = typecheck_expression(
                ctx,
                State {
                    scope: inner_scope,
                    ..state.clone()
                },
                &expr.iter,
            )?;

            if t.ty == Type::Never {
                return Ok(t);
            }
            let iter_item_ty = ctx
                .iter_item_ty(t.ty.clone())
                .ok_or(CompilerError::AnyError(format!(
                    "{:?} not an iterator",
                    t.ty
                )))?;
            ctx.symtab.parents.insert(inner_scope, state.scope);
            ctx.symtab.variables.insert(
                Symbol {
                    name: expr.pat.ident(),
                    scope: inner_scope,
                },
                (iter_item_ty, NodeRef::ForLoop(expr)),
            );

            let block = typecheck_block(
                ctx,
                State {
                    expect_break: true,
                    ..state
                },
                &expr.body,
            )?;
            
            // read as: "if the loop body cannot be coerced into unit type"
            if !block.ty.is_subtype(&Type::Unit) {
                return Err(CompilerError::AnyError(format!("Loop cannot return {:?}", block.ty)));
            }
            
            Ok(TypecheckOutput { ty: block.exit_ty, exit_ty: Type::Never })
        }
        Expression::Break(expr) => {
            if state.expect_break {
                let TypecheckOutput { ty, exit_ty } = typecheck_expression(ctx, state, expr)?;
                Ok(TypecheckOutput {
                    ty: Type::Never,
                    exit_ty: ty.union(&exit_ty)?,
                })
            } else {
                Err(CompilerError::AnyError(format!("Break is not expected")))
            }
        },
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
                Err(CompilerError::AnyError(format!("Return is not expected")))
            }
        }
        Expression::Block(block) => typecheck_block(ctx, state, block),
        Expression::IfExpression(expr) => {
            let TypecheckOutput { ty, .. } =
                typecheck_expression(ctx, state.clone(), expr.condition.as_ref())?;

            if !ty.is_subtype(&Type::Boolean) {
                return Err(CompilerError::MismatchedTypes(format!(
                    "expected Boolean, got {:?}",
                    ty
                )));
            }

            let TypecheckOutput {
                ty: then_ty,
                exit_ty: if_exit_ty,
            } = typecheck_expression(ctx, state.clone(), expr.body.as_ref())?;
            let TypecheckOutput {
                ty: else_ty,
                exit_ty: else_exit_ty,
            } = match &expr.else_body {
                Some(else_body) => typecheck_expression(ctx, state.clone(), else_body.as_ref())?,
                None => Type::Unit.into(),
            };

            let exit_ty = if_exit_ty.union(&else_exit_ty)?;

            match then_ty.union(&else_ty) {
                Err(_) => Err(CompilerError::MismatchedTypes(format!(
                    "cannot unify if-then-else branches {:?} and {:?} {}",
                    then_ty,
                    else_ty,
                    if expr.else_body.is_none() {
                        "missing else branch is implicitly unit type"
                    } else {
                        ""
                    }
                ))),
                Ok(ty) => Ok(TypecheckOutput {
                    ty,
                    exit_ty,
                }),
            }
        }
        _ => panic!("Not implemented for {:?}", expr),
    }
}
