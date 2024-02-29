use crate::{parser::ast::Block, typecheck::stmt::TckStmtOutput};

use super::super::{typecheck_statement, CompilerResult, Context, State, Type, TypecheckOutput};

/*
    I finally get what this code does now!
    loops through each statement and typechecks.
    The .ty is what it would output in normal execution
    The .exit_ty should "bubble" to the closest parent function
    The .loop_ty should "bubble" to the closest loop

    Ex: In {
        return 2;
    }, the .ty is `unit` and .exit_ty is `number`
    .exit_ty is `never` for most cases

    for `block`,
    .ty is does_return ? block.last_statement.ty : unit
    .exit_ty is union of all .exit_ty statements
*/

pub fn typecheck_block<'nodes, 'ctx>(
    ctx: &'ctx mut Context<'nodes>,
    state: State,
    block: &'nodes Block,
) -> CompilerResult<TypecheckOutput> {
    let block_scope = ctx.get_new_scope();

    ctx.scopes_by_node_id.insert(block.id, block_scope);
    ctx.symtab.parents.insert(block_scope, state.scope);

    let mut state = State {
        scope: block_scope,
        ..state
    };
    let mut expr_ty = Type::Never;
    let mut loop_ty = Type::Never;
    let mut return_ty = Type::Never;
    for (i, stmt) in block.statements.iter().enumerate() {
        let TckStmtOutput {
            tck_output:
                TypecheckOutput {
                    expr_ty: stmt_expr_ty,
                    return_ty: stmt_return_ty,
                    loop_ty: stmt_loop_ty,
                },
            shadowed,
        } = typecheck_statement(ctx, state.clone(), stmt)?;
        if let Some(shadowed_scope) = shadowed {
            state.scope = shadowed_scope;
        }
        if i == block.statements.len() - 1 {
            if block.does_return {
            } else {
                expr_ty = Type::Unit;
            }
        }
        return_ty = return_ty.union(&stmt_return_ty)?;
        loop_ty = loop_ty.union(&stmt_loop_ty)?;
    }
    Ok(TypecheckOutput {
        expr_ty,
        return_ty,
        loop_ty,
    })
}
