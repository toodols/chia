use crate::{
    parser::ast::{ForLoop, Node},
    typecheck::{
        typecheck_block, CompilerError, CompilerResult, Context, NodeRef, State, Symbol, Type,
        TypecheckOutput,
    },
};

use super::typecheck_expression;

pub fn typecheck_for_loop<'nodes, 'ctx>(
    ctx: &'ctx mut Context<'nodes>,
    state: State,
    Node { inner, id }: &'nodes Node<ForLoop>,
) -> CompilerResult<TypecheckOutput> {
    let loop_body_scope = ctx.get_scope_from_node_id(inner.body.id);
    let t = typecheck_expression(
        ctx,
        State {
            scope: loop_body_scope,
            ..state.clone()
        },
        &inner.iter,
    )?;

    if t.ty == Type::Never {
        return Ok(t);
    }
    let iter_item_ty = ctx
        .iter_item_ty(t.ty.clone())
        .ok_or_else(|| CompilerError::AnyError(format!("{:?} not an iterator", t.ty)))?;
    ctx.symtab.parents.insert(loop_body_scope, state.scope);

    for (path_id, ty) in inner.pat.decompose(&iter_item_ty)? {
        let sym = Symbol {
            name: inner.pat.as_path_symbol(),
            scope: loop_body_scope,
            ..Default::default()
        };
        ctx.node_id_to_symbol.insert(path_id, sym.clone());
        ctx.symtab
            .variables
            .insert(sym, (ty, NodeRef::ForLoop(inner)));
    }

    let block = typecheck_block(
        ctx,
        State {
            expect_break: true,
            ..state
        },
        &inner.body,
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
