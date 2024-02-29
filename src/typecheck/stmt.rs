use std::collections::HashMap;

use crate::{
    parser::ast::{Pattern, Statement},
    typecheck::VarSymbolDetails,
};

use super::{
    typecheck_expression, CompilerError, CompilerResult, Context, NodeRef, ScopeId, State, Symbol,
    Type, TypecheckOutput,
};

pub struct TckStmtOutput {
    pub tck_output: TypecheckOutput,
    pub shadowed: Option<ScopeId>,
}

impl From<TypecheckOutput> for TckStmtOutput {
    fn from(value: TypecheckOutput) -> Self {
        return TckStmtOutput {
            tck_output: value,
            shadowed: None,
        };
    }
}

impl From<Type> for TckStmtOutput {
    fn from(value: Type) -> Self {
        return TckStmtOutput {
            tck_output: value.into(),
            shadowed: None,
        };
    }
}

// Statements discard the .expr_ty field of the enclosed expr and replace it with unit
pub fn typecheck_statement<'nodes, 'ctx>(
    ctx: &'ctx mut Context<'nodes>,
    state: State,
    stmt: &'nodes Statement,
) -> CompilerResult<TckStmtOutput> {
    match stmt {
        Statement::Empty => Ok(Type::Unit.into()),
        Statement::Expression(ref expr) => {
            typecheck_expression(ctx, state, expr).map(|out| out.into())
        }
        Statement::LetDeclaration(let_decl) => {
            let expr = match &let_decl.value {
                Some(val) => val,
                None => Err(CompilerError::AnyError(format!(
                    "{:?} must be initialized",
                    let_decl.pat
                )))?,
            };

            let TypecheckOutput {
                expr_ty,
                return_ty,
                loop_ty,
            } = typecheck_expression(ctx, state.clone(), &expr)?;

            // if ty == Type::Never {
            //     Err(CompilerError::AnyError(format!(
            //         "{} cannot be initialized to Never",
            //         decl.name
            //     )))?;
            // }

            let mut shadowed = None;

            for (path, ty) in let_decl.pat.destructure(ctx, &expr_ty)? {
                assert!(path.path.len() == 1, "Cannot define variable by path");

                let symbol = ctx.get_new_symbol();
                ctx.node_id_to_symbol.insert(path.id, symbol);

                // does the name already exist in the current scope?
                let scope_to_insert_in = match (
                    shadowed,
                    ctx.symtab.variables.get_name_in_scope_chain(
                        &ctx.symtab.parents,
                        state.scope,
                        &path.last(),
                    ),
                ) {
                    // add a new scope and continue from there
                    (None, Some(_)) => {
                        let new_scope = ctx.get_new_scope();
                        ctx.symtab.parents.insert(new_scope, state.scope);
                        shadowed = Some(new_scope);
                        new_scope
                    }
                    // already shadowed; doesn't matter
                    (Some(new_scope), _) => new_scope,

                    // No conflict -> insert into current scope
                    (None, None) => state.scope,
                };

                ctx.symtab.variables.insert(
                    symbol,
                    path.last(),
                    scope_to_insert_in,
                    VarSymbolDetails {
                        ty,
                        node_ref: Some(NodeRef::LetDeclaration(let_decl)),
                    },
                );
            }

            Ok(TckStmtOutput {
                tck_output: TypecheckOutput {
                    expr_ty: Type::Unit,
                    return_ty,
                    loop_ty,
                },
                shadowed,
            })
        }
    }
}
