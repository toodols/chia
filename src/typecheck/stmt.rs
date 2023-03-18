use crate::parser::ast::{Statement, Expression};

use super::{Context, typecheck_expression, CompilerResult, Type, CompilerError, NodeRef, Symbol, State};

pub struct TypecheckStatementResult<'a, 'ctx> {
	pub ty: Type,
	pub context: &'ctx mut Context<'a>,
	pub exit: bool,
}
impl<'a,'ctx> From<(Type, &'ctx mut Context<'a>)> for TypecheckStatementResult<'a, 'ctx> {
	fn from((ty, context): (Type, &'ctx mut Context<'a>)) -> Self {
		Self {
			ty,
			context,
			exit: false,
		}
	}
}

pub fn typecheck_statement<'a, 'ctx>(mut context: &'ctx mut Context<'a>, state: State, stmt: &'a Statement) -> CompilerResult<TypecheckStatementResult<'a,'ctx>> {
	match stmt {
		Statement::Empty => Ok((Type::Unit, context).into()),
		Statement::Expression(ret_expr @ Expression::Return(expr)) => {
			let ty;
			(ty, context) = typecheck_expression(context, state, ret_expr)?;

			Ok(TypecheckStatementResult{
				ty,
				context,
				exit: true,
			})
		},
		Statement::Expression(expr) => Ok(typecheck_expression(context, state, expr)?.into()),
		Statement::LetDeclaration(decl) => {
			let expr = match &decl.value { Some(val)=>val, None=>Err(CompilerError::AnyError(format!("{} must be initialized", decl.name)))? };
			let ty: Type;
			(ty, context) = typecheck_expression(context, state.clone(), expr)?;
			let symbol = Symbol{
				name: decl.name.clone(),
				scope: state.scope,
			};
			context.symtab.variables.insert(symbol, (ty, NodeRef::LetDeclaration(decl)));
			Ok((Type::Unit, context).into())
		}
	}
}