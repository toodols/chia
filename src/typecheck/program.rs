use crate::parser::ast::{Item, Program};

use super::{
    fn_decl::typecheck_function_declaration, CompilerResult, Context, NodeRef, State, Symbol,
    Symtab, Type,
};

pub fn typecheck_program(program: &Program) -> CompilerResult<Context<'_>> {
    let mut context = Context::new();
    let global_scope = context.get_new_scope();
    let state = State {
        scope: global_scope,
        ..Default::default()
    };
    {
        let mut context = &mut context;
        for item in program.items.iter() {
            match item {
                Item::FunctionDeclaration(func) => {
                    context.symtab.variables.insert(
                        Symbol {
                            name: func.name.clone(),
                            scope: global_scope,
                            ..Default::default()
                        },
                        (
                            Type::Function(
                                func.parameters
                                    .iter()
                                    .map(|(_, ty_expr)| {
                                        context.symtab.get_type(global_scope, ty_expr)
                                    })
                                    .collect(),
                                Box::new(context.symtab.get_type(global_scope, &func.return_type)),
                            ),
                            NodeRef::FunctionDeclaration(func),
                        ),
                    );
                }
                Item::TupleStructDeclaration(decl) => {
                    todo!("Typecheck struct declaration")
                }
                _ => todo!(""),
            }
        }

        for item in program.items.iter() {
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
