use crate::parser::ast::{Item, Program};

use super::{
    fn_decl::typecheck_function_declaration, CompilerResult, Context, NodeRef, State, Symbol,
    Symtab, Type, VarSymbolDetails,
};

pub fn typecheck_program<'ctx, 'nodes>(
    context: &'ctx mut Context<'nodes>,
    program: &'nodes Program,
) -> CompilerResult<()> {
    // global scope is reserved for items built in.
    let global_scope = context.get_new_scope();

    let program_scope = context.get_new_scope();
    context.scopes_by_node_id.insert(program.id, program_scope);
    context.symtab.parents.insert(program_scope, global_scope);
    let state = State {
        scope: program_scope,
        ..Default::default()
    };
    {
        // let mut context = &mut context;
        for item in program.items.iter() {
            match item {
                Item::FunctionDeclaration(fn_decl) => {
                    let symbol = context.get_new_symbol();
                    // let symbol = Symbol {
                    //     name: fn_decl.span.clone(),
                    //     scope: program_scope,
                    //     ..Default::default()
                    // };
                    context.node_id_to_symbol.insert(fn_decl.id, symbol.clone());
                    context.symtab.variables.insert(
                        symbol,
                        fn_decl.span.to_string(),
                        program_scope,
                        VarSymbolDetails {
                            ty: Type::Function(
                                fn_decl
                                    .parameters
                                    .0
                                    .iter()
                                    .map(|(_, ty_expr)| {
                                        context
                                            .get_type(program_scope, &ty_expr)
                                            .ok()
                                            .flatten()
                                            .expect(
                                                format!("parameter type not found {ty_expr:?}")
                                                    .as_str(),
                                            )
                                    })
                                    .collect(),
                                Box::new(
                                    context
                                        .get_type(program_scope, &fn_decl.return_type)
                                        .ok()
                                        .flatten()
                                        .expect("return type not found"),
                                ),
                            ),
                            node_ref: Some(NodeRef::FunctionDeclaration(fn_decl)),
                        },
                    );
                }
                Item::TupleStructDeclaration(decl) => {
                    todo!("Typecheck tuple struct declaration")
                }
                Item::Mod(another_program) => {
                    typecheck_program(context, &another_program.body)?;
                }
                Item::StructDeclaration(decl) => {
                    todo!("Typecheck struct declaration")
                }
            }
        }

        for item in program.items.iter() {
            match item {
                Item::FunctionDeclaration(func) => {
                    typecheck_function_declaration(context, state.clone(), func)?;
                }
                Item::Mod(_) => {}
                _ => todo!(),
            }
        }
    }
    Ok(())
}
