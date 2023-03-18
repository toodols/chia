use crate::parser::ast::FunctionDeclaration;

use super::{typecheck_block, CompilerError, CompilerResult, Context, NodeRef, State, Symbol};

pub fn typecheck_function_declaration<'a, 'ctx>(
    ctx: &'ctx mut Context<'a>,
    state: State,
    function: &'a FunctionDeclaration,
) -> CompilerResult<&'ctx mut Context<'a>> {
    let parent_scope = state.scope;
    let scope = function.body.node_id;
    ctx.symtab.parents.insert(scope, parent_scope);
    for (name, type_expr) in function.parameters.iter() {
        ctx.symtab.variables.insert(
            Symbol {
                name: name.clone(),
                scope,
            },
            (
                ctx.symtab.get_type(parent_scope, type_expr),
                NodeRef::FunctionDeclaration(function),
            ),
        );
    }
    let return_type = ctx.symtab.get_type(parent_scope, &function.return_type);

    let block_output = typecheck_block(
        ctx,
        State {
            scope,
            expect_return: Some(return_type.clone()),
        },
        &function.body,
    )?;
    if block_output.ty.extends(&return_type) {
        Ok(ctx)
    } else {
        Err(CompilerError::AnyError(format!(
            "Function {:?} has return type {:?} but returns {:?}",
            function.name, return_type, block_output.ty
        )))
    }
}
