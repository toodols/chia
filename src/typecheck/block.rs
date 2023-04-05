use crate::parser::ast::Block;

use super::{typecheck_statement, CompilerResult, Context, State, Type, TypecheckOutput};

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
    let mut exits = false;
    for stmt in block.statements.iter() {
        if exits {
            println!("warning {stmt:?} is unreachable")
        }
        let TypecheckOutput { ty: stmt_ty, exits: stmt_exits } = typecheck_statement(ctx, state.clone(), stmt)?;
        ty = stmt_ty;
        if stmt_exits {
            exits = true;
        }
    }
    Ok(TypecheckOutput {
        ty: if exits {
            Type::Never
        } else if block.does_return {
            ty
        } else {
            Type::Unit
        },
        exits,
    })
}
