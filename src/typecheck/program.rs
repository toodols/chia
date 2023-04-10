use crate::parser::ast::Program;

use super::{
    fn_decl::typecheck_function_declaration, CompilerResult, Context, NodeRef, State, Symbol,
    Symtab, Type,
};

pub fn typecheck_program<'nodes>(program: &'nodes Program) -> CompilerResult<Context<'nodes>> {
    let mut context = Context {
        symtab: Symtab::new(),
    };
    let state = State {
        scope: 0,
        ..Default::default()
    };
    {
        let mut context = &mut context;
        for func in program.functions.iter() {
            context.symtab.variables.insert(
                Symbol {
                    name: func.name.clone(),
                    scope: 0,
                },
                (
                    Type::Function(
                        func.parameters
                            .iter()
                            .map(|(name, ty_expr)| context.symtab.get_type(0, ty_expr))
                            .collect(),
                        Box::new(context.symtab.get_type(0, &func.return_type)),
                    ),
                    NodeRef::FunctionDeclaration(func),
                ),
            );
        }

        for func in program.functions.iter() {
            context = typecheck_function_declaration(context, state.clone(), func)?;
        }
    }
    Ok(context)
}
