use crate::parser::ast::FunctionDeclaration;

use super::{
    expr::block, typecheck_block, CompilerError, CompilerResult, Context, NodeRef, State, Symbol,
    Type, VarSymbolDetails,
};

pub fn typecheck_function_declaration<'nodes, 'ctx>(
    ctx: &'ctx mut Context<'nodes>,
    state: State,
    fn_decl: &'nodes FunctionDeclaration,
) -> CompilerResult<()> {
    let fn_scope = state.scope;
    let param_scope = ctx.get_new_scope();
    // ctx.scopes_by_node_id.insert(fn_decl.body.id, body_scope);

    /*
    Implementation 1: `p` and `T` and `R` is considered to be part of scope 1
    <scope 0>
    <scope 1> fn a( p: T ) -> R
    <scope 1> {
        ...
    }

    Implementation 2:
    Currently being used because dumbass me made block expr create a new scope unconditionally
    <scope 0>
    <scope 1> fn a( p: T ) -> R
    <scope 2> {
        ...
    }
     */
    ctx.symtab.parents.insert(param_scope, fn_scope);
    for (pat, type_expr) in fn_decl.parameters.0.iter() {
        for (path, ty) in pat
            .destructure(
                ctx,
                // type retrieval use fn_scope instead of param_scope because you can't define types in param_scope
                &ctx.get_type(fn_scope, type_expr)
                    .ok()
                    .flatten()
                    .ok_or(CompilerError::TypeNotFound(format!("{:?}", type_expr)))?,
            )?
            .into_iter()
        {
            assert!(path.path.len() == 1, "path length > 1");
            let symbol = ctx.get_new_symbol();
            ctx.symtab.variables.insert(
                symbol,
                path.last(),
                param_scope,
                VarSymbolDetails {
                    ty,
                    node_ref: Some(NodeRef::FunctionDeclaration(fn_decl)),
                },
            );
        }
    }
    let return_type = ctx
        .get_type(fn_scope, &fn_decl.return_type)
        .ok()
        .flatten()
        .ok_or(CompilerError::TypeNotFound(format!(
            "{:?}",
            fn_decl.return_type
        )))?;
    let block_output = typecheck_block(
        ctx,
        State {
            scope: param_scope,
            ..Default::default()
        },
        &fn_decl.body,
    )?;

    // Prioritizes `return_ty` over `expr_ty` of block.
    if if block_output.return_ty == Type::Never {
        block_output.expr_ty.is_subtype(&return_type)
    } else {
        block_output.return_ty.is_subtype(&return_type)
    } {
        Ok(())
    } else {
        Err(CompilerError::AnyError(format!(
            "Function {:?} has return type {:?} but returns {:?}",
            fn_decl.span, return_type, block_output.expr_ty
        )))
    }
}
