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
    mut ctx: &'ctx mut Context<'nodes>,
    state: State,
    block: &'nodes Block,
) -> CompilerResult<TypecheckOutput<'nodes, 'ctx>> {
    let mut ty = Type::Unit;
    let mut exits = false;
    for stmt in block.statements.iter() {
        if exits {
            println!("Warning: unreachable code");
            break;
        }
        TypecheckOutput { ty, ctx, exits } = typecheck_statement(ctx, state.clone(), stmt)?;
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
        ctx,
    })
}
