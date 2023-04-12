use crate::{
	parser::ast::{ForLoop, IfExpression},
	typecheck::{
		typecheck_block, CompilerError, CompilerResult, Context, NodeRef, State, Symbol, Type,
		TypecheckOutput,
	},
};

use super::typecheck_expression;

pub fn typecheck_if_expr<'nodes, 'ctx>(
	ctx: &'ctx mut Context<'nodes>,
	state: State,
	expr: &'nodes IfExpression,
) -> CompilerResult<TypecheckOutput> {
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
		Ok(ty) => Ok(TypecheckOutput { ty, exit_ty }),
	}
}
