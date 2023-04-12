use crate::parser::ast::{FunctionDeclaration, SymbolName};

use super::{typecheck_block, CompilerError, CompilerResult, Context, NodeRef, State, Symbol};

pub fn typecheck_function_declaration<'nodes, 'ctx>(
	ctx: &'ctx mut Context<'nodes>,
	state: State,
	function: &'nodes FunctionDeclaration,
) -> CompilerResult<&'ctx mut Context<'nodes>> {
	let scope = state.scope;
	let body_scope = function.body.node_id;

	// parent is inserted again in typecheck_block with the same arguments
	/*
	Current implementation. `p` and `T` and `R` is considered to be part of scope 1
	<scope 0>
	<scope 1> fn a( p: T ) -> R
	<scope 1> {
		...
	}

	Alternate implementation. Possible by assigning a node_id for `FnDecl` and using that as the parent of `Block`
	But largely redundant.
	<scope 0>
	<scope 1> fn a( p: T ) -> R
	<scope 2> {
		...
	}
	 */
	ctx.symtab.parents.insert(body_scope, scope);
	for (pat, type_expr) in function.parameters.iter() {
		ctx.symtab.variables.insert(
			Symbol {
				name: pat.ident(),
				scope: body_scope,
			},
			(
				ctx.symtab.get_type(scope, type_expr),
				NodeRef::FunctionDeclaration(function),
			),
		);
	}
	let return_type = ctx.symtab.get_type(scope, &function.return_type);
	let block_output = typecheck_block(
		ctx,
		State {
			scope,
			expect_return_type: Some(return_type.clone()),
			..Default::default()
		},
		&function.body,
	)?;
	if block_output.ty.is_subtype(&return_type) {
		Ok(ctx)
	} else {
		Err(CompilerError::AnyError(format!(
			"Function {:?} has return type {:?} but returns {:?}",
			function.name, return_type, block_output.ty
		)))
	}
}
