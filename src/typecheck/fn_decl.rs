use crate::parser::ast::{FunctionDeclaration, Node};

use super::{typecheck_block, CompilerError, CompilerResult, Context, NodeRef, State, Symbol};

pub fn typecheck_function_declaration<'nodes, 'ctx>(
    ctx: &'ctx mut Context<'nodes>,
    state: State,
    Node { inner, .. }: &'nodes Node<FunctionDeclaration>,
) -> CompilerResult<&'ctx mut Context<'nodes>> {
    let scope = state.scope;
    let body_scope = ctx.get_scope_from_node_id(inner.body.id);

    // parent is inserted again in typecheck_block with the same arguments
    /*
    Current implementation. `p` and `T` and `R` is considered to be part of scope 1
    <scope 0>
    <scope 1> fn a( p: T ) -> R
    <scope 1> {
        ...
    }

    Alternate implementation. Possible by assigning a node_id for `FnDecl` and using that as the parent of `Block`
    But largely redundant.
    <scope 0>
    <scope 1> fn a( p: T ) -> R
    <scope 2> {
        ...
    }
     */
    ctx.symtab.parents.insert(body_scope, scope);
    for (pat, type_expr) in inner.parameters.inner.0.iter() {
        ctx.symtab.variables.insert(
            Symbol {
                name: pat.as_path_symbol(),
                scope: body_scope,
                ..Default::default()
            },
            (
                ctx.symtab.get_type(scope, &type_expr.inner),
                NodeRef::FunctionDeclaration(inner),
            ),
        );
    }
    let return_type = ctx.symtab.get_type(scope, &inner.return_type.inner);
    let block_output = typecheck_block(
        ctx,
        State {
            scope,
            expect_return_type: Some(return_type.clone()),
            ..Default::default()
        },
        &inner.body,
    )?;
    if block_output.ty.is_subtype(&return_type) {
        Ok(ctx)
    } else {
        Err(CompilerError::AnyError(format!(
            "Function {:?} has return type {:?} but returns {:?}",
            inner.name, return_type, block_output.ty
        )))
    }
}
