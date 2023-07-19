use crate::parser::ast::{Statement, Node};

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
        Statement::Expression(ref expr) => typecheck_expression(ctx, state, expr).map(|out| out.into()),
        Statement::LetDeclaration(Node {inner, id}) => {
            let expr = match &inner.value {
                Some(val) => val,
                None => Err(CompilerError::AnyError(format!(
                    "{:?} must be initialized",
                    inner.pat.inner.ident()
                )))?,
            };

            let TypecheckOutput { ty, exit_ty } = typecheck_expression(ctx, state.clone(), &expr)?;

            // if ty == Type::Never {
            //     Err(CompilerError::AnyError(format!(
            //         "{} cannot be initialized to Never",
            //         decl.name
            //     )))?;
            // }
            let symbol = Symbol {
                name: inner.pat.inner.ident(),
                scope: state.scope,
                ..Default::default()
            };

            // does the symbol already exist in the current scope?
            let shadowed = match ctx.symtab.variables.get(&symbol) {
                // add a new scope and continue from there
                Some(_) => {
                    let new_scope = ctx.get_scope_from_node_id(*id);
                    ctx.symtab.parents.insert(new_scope, state.scope);
                    let Symbol { name, .. } = symbol;
                    ctx.symtab.variables.insert(
                        Symbol {
                            name,
                            scope: new_scope,
                            ..Default::default()
                        },
                        (ty, NodeRef::LetDeclaration(inner)),
                    );
                    Some(new_scope)
                }
                // add variable to scope, do nothing
                None => {
                    ctx.symtab
                        .variables
                        .insert(symbol, (ty, NodeRef::LetDeclaration(inner)));
                    None
                }
            };

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
