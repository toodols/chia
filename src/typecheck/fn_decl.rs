use crate::parser::ast::FunctionDeclaration;

use super::{Symbol, Context, State, NodeRef, Type, typecheck_statement, CompilerError, CompilerResult, stmt::TypecheckStatementResult};

pub fn typecheck_function_declaration<'a, 'ctx>(mut context: &'ctx mut Context<'a>, state: State, function: &'a FunctionDeclaration) -> CompilerResult<&'ctx mut Context<'a>> {
	let parent_scope = state.scope;
	let scope = function.body.node_id;
	context.symtab.parents.insert(scope, parent_scope);
	for (name, type_expr) in function.parameters.iter() {
		context.symtab.variables.insert(Symbol{
			name: name.clone(),
			scope,
		}, (context.symtab.get_type(parent_scope, type_expr), NodeRef::FunctionDeclaration(function)));
	}
	let return_type = context.symtab.get_type(parent_scope, &function.return_type);
	
	let mut ty = Type::Unit;
	let mut exit = false;
	for stmt in function.body.statements.iter() {
		if exit {
			println!("warning: unreachable code at {:?}", function.name);
			break;
		}
		TypecheckStatementResult{ty, context, exit} = typecheck_statement(context, State{
			scope,
			expect_return: Some(return_type.clone()),
		}, stmt)?;
	}

	if function.body.does_return {
		if return_type!=ty {
			Err(CompilerError::MismatchedTypes(format!("(does_return = true) expected {return_type:?}, got {ty:?} at {:?}", function.name)))
		} else {
			Ok(context)
		}
	} else {
		if return_type == Type::Unit || exit {
			Ok(context)
		} else {
			Err(CompilerError::MismatchedTypes(format!("(does_return = false) expected {:?}, got {:?} at {:?}", return_type, Type::Unit, function.name)))
		}
	}
}