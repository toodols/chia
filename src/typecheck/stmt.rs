use crate::parser::ast::{Expression, Statement};

use super::{
    typecheck_expression, CompilerError, CompilerResult, Context, NodeRef, State, Symbol, Type,
    TypecheckOutput,
};

pub fn typecheck_statement<'nodes, 'ctx>(
    ctx: &'ctx mut Context<'nodes>,
    state: State,
    stmt: &'nodes Statement,
) -> CompilerResult<TypecheckOutput> {
    match stmt {
        Statement::Empty => Ok(Type::Unit.into()),
        Statement::Expression(expr) => typecheck_expression(ctx, state, expr),
        Statement::LetDeclaration(decl) => {
            let expr = match &decl.value {
                Some(val) => val,
                None => Err(CompilerError::AnyError(format!(
                    "{:?} must be initialized",
                    decl.pat.ident()
                )))?,
            };

            let TypecheckOutput { ty, exit_ty } = typecheck_expression(ctx, state.clone(), expr)?;
            // if ty == Type::Never {
            //     Err(CompilerError::AnyError(format!(
            //         "{} cannot be initialized to Never",
            //         decl.name
            //     )))?;
            // }
            let symbol = Symbol {
                name: decl.pat.ident(),
                scope: state.scope,
            };
            ctx.symtab
                .variables
                .insert(symbol, (ty, NodeRef::LetDeclaration(decl)));
            Ok(TypecheckOutput {
                ty: Type::Unit,
                exit_ty,
            })
        }
    }
}
