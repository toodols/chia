use crate::parser::ast::Block;

use super::super::{typecheck_statement, CompilerResult, Context, State, Type, TypecheckOutput};

/*
// b should be never
fn a() -> number {
    let b = {
        return 123
    }
}

 */

pub fn typecheck_block<'nodes, 'ctx>(
    ctx: &'ctx mut Context<'nodes>,
    state: State,
    block: &'nodes Block,
) -> CompilerResult<TypecheckOutput> {
    // insert block scope into symbol table
    ctx.symtab.parents.insert(block.node_id, state.scope);
    let state = State {
        scope: block.node_id,
        ..state
    };
    let mut ty = Type::Unit;
    let mut exit_ty = Type::Never;
    let mut ty_is_never = false;
    for stmt in block.statements.iter() {
        let TypecheckOutput {
            ty: stmt_ty,
            exit_ty: stmt_exit_ty,
        } = typecheck_statement(ctx, state.clone(), stmt)?;
        if ty == Type::Never {
            ty_is_never = true;
        }
        ty = stmt_ty;
        exit_ty = exit_ty.union(&stmt_exit_ty)?;
    }
    Ok(TypecheckOutput {
        ty: if ty_is_never { Type::Never } else { ty },
        exit_ty,
    })
}
