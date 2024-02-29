use crate::{
    parser::ast::IfExpression,
    typecheck::{CompilerError, CompilerResult, Context, State, Type, TypecheckOutput},
};

use super::typecheck_expression;

// the type of if { expr1 } else {expr2} is expr1.union(expr2)
// the type of if { expr1 } is expr1.union({
//      expr_ty: Unit, return_ty: Never, loop_ty: Never
// })

pub fn typecheck_if_expr<'nodes, 'ctx>(
    ctx: &'ctx mut Context<'nodes>,
    state: State,
    if_expr: &'nodes IfExpression,
) -> CompilerResult<TypecheckOutput> {
    let TypecheckOutput { expr_ty: ty, .. } =
        typecheck_expression(ctx, state.clone(), if_expr.condition.as_ref())?;

    if !ty.is_subtype(&Type::Boolean) {
        return Err(CompilerError::MismatchedTypes(format!(
            "expected Boolean, got {:?}",
            ty
        )));
    }

    let if_output = typecheck_expression(ctx, state.clone(), if_expr.body.as_ref())?;
    let else_output = match &if_expr.else_body {
        Some(else_body) => typecheck_expression(ctx, state, else_body.as_ref())?,
        None => Type::Unit.into(),
    };

    let expr_ty = if_output.expr_ty.union(&else_output.expr_ty)?;
    let return_ty = if_output.return_ty.union(&else_output.return_ty)?;
    let loop_ty = if_output.loop_ty.union(&else_output.loop_ty)?;
    Ok(TypecheckOutput {
        expr_ty,
        return_ty,
        loop_ty,
    })
}
