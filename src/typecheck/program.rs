use crate::parser::ast::{Item, Node, Program};

use super::{
    fn_decl::typecheck_function_declaration, CompilerResult, Context, NodeRef, State, Symbol,
    Symtab, Type,
};

pub fn typecheck_program(Node { inner, id }: &Node<Program>) -> CompilerResult<Context<'_>> {
    let mut context = Context::new();
    let global_scope = context.get_scope_from_node_id(*id);
    let state = State {
        scope: global_scope,
        ..Default::default()
    };
    {
        let mut context = &mut context;
        for item in inner.items.iter() {
            match item {
                Item::FunctionDeclaration(Node { inner, id }) => {
                    let symbol = Symbol {
                        name: inner.name.clone(),
                        scope: global_scope,
                        ..Default::default()
                    };
                    context.node_id_to_symbol.insert(*id, symbol.clone());
                    context.symtab.variables.insert(
                        symbol,
                        (
                            Type::Function(
                                inner
                                    .parameters
                                    .inner
                                    .0
                                    .iter()
                                    .map(|(_, ty_expr)| {
                                        context.symtab.get_type(global_scope, &ty_expr.inner)
                                    })
                                    .collect(),
                                Box::new(
                                    context
                                        .symtab
                                        .get_type(global_scope, &inner.return_type.inner),
                                ),
                            ),
                            NodeRef::FunctionDeclaration(inner),
                        ),
                    );
                }
                Item::TupleStructDeclaration(decl) => {
                    todo!("Typecheck struct declaration")
                }
                _ => todo!(""),
            }
        }

        for item in inner.items.iter() {
            match item {
                Item::FunctionDeclaration(func) => {
                    context = typecheck_function_declaration(context, state.clone(), func)?;
                }
                _ => todo!(),
            }
        }
    }
    Ok(context)
}
