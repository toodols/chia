use crate::{
    parser::ast::ForLoop,
    typecheck::{
        typecheck_block, CompilerError, CompilerResult, Context, NodeRef, State, Symbol, Type,
        TypecheckOutput, VarSymbolDetails,
    },
};

use super::typecheck_expression;

pub fn typecheck_for_loop<'nodes, 'ctx>(
    ctx: &'ctx mut Context<'nodes>,
    state: State,
    for_loop: &'nodes ForLoop,
) -> CompilerResult<TypecheckOutput> {
    let loop_expr_scope = ctx.get_new_scope();
    ctx.scopes_by_node_id
        .insert(for_loop.body.id, loop_expr_scope);

    println!("loopexprscope {loop_expr_scope}");
    let t = typecheck_expression(ctx, state.clone(), &for_loop.iter)?;

    if t.ty == Type::Never {
        return Ok(t);
    }
    let iter_item_ty = ctx
        .iter_item_ty(t.ty.clone())
        .ok_or_else(|| CompilerError::AnyError(format!("{:?} not an iterator", t.ty)))?;
    ctx.symtab.parents.insert(loop_expr_scope, state.scope);

    for (path, ty) in for_loop.pat.decompose(ctx, &iter_item_ty)? {
        assert_eq!(path.path.len(), 1, "path len > 0");
        let sym = ctx.get_new_symbol();
        ctx.node_id_to_symbol.insert(path.id, sym.clone());
        ctx.symtab.variables.insert(
            sym,
            path.last(),
            loop_expr_scope,
            VarSymbolDetails {
                ty,
                node_ref: Some(NodeRef::ForLoop(for_loop)),
            },
        );
    }

    let block = typecheck_block(
        ctx,
        State {
            expect_break: true,
            scope: loop_expr_scope,
            ..state
        },
        &for_loop.body,
    )?;

    // read as: "if the loop body cannot be coerced into unit type"
    if !block.ty.is_subtype(&Type::Unit) {
        return Err(CompilerError::AnyError(format!(
            "Loop cannot return {:?}",
            block.ty
        )));
    }

    Ok(TypecheckOutput {
        ty: block.exit_ty,
        exit_ty: Type::Never,
    })
}
