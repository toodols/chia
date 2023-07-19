use crate::{parser::ast::{Block, Node}, typecheck::stmt::TckStmtOutput};

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
    block: &'nodes Node<Block>,
) -> CompilerResult<TypecheckOutput> {
    let block_scope = ctx.get_scope_from_node_id(block.id);
    // insert block scope into symbol table
    ctx.symtab.parents.insert(block_scope, state.scope);
    let mut state = State {
        scope: block_scope,
        ..state
    };
    let mut ty = Type::Unit;
    let mut exit_ty = Type::Never;
    let mut ty_is_never = false;
    for stmt in block.inner.statements.iter() {
        let TckStmtOutput {
            tck_output:
                TypecheckOutput {
                    ty: stmt_ty,
                    exit_ty: stmt_exit_ty,
                },
            shadowed,
        } = typecheck_statement(ctx, state.clone(), stmt)?;
        if let Some(shadowed_scope) = shadowed {
            state.scope = shadowed_scope;
        }
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
