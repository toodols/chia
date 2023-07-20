use std::collections::HashMap;

use crate::parser::ast::{Node, Pattern, Statement};

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
        Statement::LetDeclaration(Node { inner, id }) => {
            let expr = match &inner.value {
                Some(val) => val,
                None => Err(CompilerError::AnyError(format!(
                    "{:?} must be initialized",
                    inner.pat.as_path_symbol()
                )))?,
            };

            let TypecheckOutput { ty, exit_ty } = typecheck_expression(ctx, state.clone(), &expr)?;

            // if ty == Type::Never {
            //     Err(CompilerError::AnyError(format!(
            //         "{} cannot be initialized to Never",
            //         decl.name
            //     )))?;
            // }

            let mut shadowed = None;

            for (path_id, ty) in inner.pat.decompose(&ty)? {
                let symbol = Symbol {
                    name: inner.pat.as_path_symbol(),
                    scope: state.scope,
                    ..Default::default()
                };
                ctx.node_id_to_symbol.insert(path_id, symbol.clone());
                // does the symbol already exist in the current scope?
                match (shadowed, ctx.symtab.variables.get(&symbol)) {
                    // add a new scope and continue from there
                    (None, Some(_)) => {
                        let new_scope = ctx.get_scope_from_node_id(*id);
                        ctx.symtab.parents.insert(new_scope, state.scope);
                        shadowed = Some(new_scope);
                        let Symbol { name, .. } = symbol;
                        let symbol = Symbol {
                            name,
                            scope: new_scope,
                            ..Default::default()
                        };
                        println!("insert {path_id}");
                        ctx.node_id_to_symbol.insert(path_id, symbol.clone());
                        ctx.symtab
                            .variables
                            .insert(symbol, (ty, NodeRef::LetDeclaration(inner)));
                    }
                    (Some(new_scope), Some(_)) => {
                        let Symbol { name, .. } = symbol;
                        let symbol = Symbol {
                            name,
                            scope: new_scope,
                            ..Default::default()
                        };
                        println!("insert {path_id}");
                        ctx.node_id_to_symbol.insert(path_id, symbol.clone());
                        ctx.symtab
                            .variables
                            .insert(symbol, (ty, NodeRef::LetDeclaration(inner)));
                    }
                    // add variable to scope, do nothing
                    (_, None) => {
                        ctx.symtab
                            .variables
                            .insert(symbol, (ty, NodeRef::LetDeclaration(inner)));
                    }
                };
            }

            Ok(TckStmtOutput {
                tck_output: TypecheckOutput {
                    ty: Type::Unit,
                    exit_ty,
                },
                shadowed,
            })
        }
    }
}
