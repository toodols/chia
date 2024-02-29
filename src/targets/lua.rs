use crate::{
    parser::ast::{
        BinaryOperation, BinaryOperator, Block, Expression, FunctionDeclaration, Item, Literal,
        Pattern, Program, Statement,
    },
    typecheck::{Context, ScopeId, Symbol},
};

// proof of concept for using chia for transpilation
// currently all identifiers are `{name}_{scope}`
// which is certainly helpful for avoiding naming conflict
// but also makes it impossible to reference global variables reliably

// random note: what if when writing a symbol as a string
// you check up the chain to see whether it conflicts with anything
// this has the added benefit that all global symbols will always keep their symbolname

// addendum #2: what if you just preserve names in global scope but do {name}_{scope} bullshit

fn get_name(ctx: &Context<'_>, symbol: Symbol) -> String {
    let (name, scope) = ctx.symtab.variables.get_name_and_scope(symbol).unwrap();
    if *scope == ScopeId(0) {
        name.clone()
    } else {
        format!("{name}_{scope}")
    }
}

fn transpile_expr(
    source: &mut String,
    scope: ScopeId,
    expr: &Expression,
    ctx: &Context<'_>,
) -> Result<(), ()> {
    match expr {
        Expression::Break(_) => todo!(),
        Expression::Index(_) => todo!(),
        Expression::Block(_) => todo!(),
        Expression::BinaryOperation(_) => todo!(),
        Expression::UnaryOperation(_) => todo!(),
        Expression::Literal(inner) => match inner {
            Literal::Unit => source.push_str("nil"),
            Literal::Array(t) => {
                source.push_str("{");
                let len = t.len();
                for (i, expr) in t.iter().enumerate() {
                    transpile_expr(source, scope, expr, ctx).unwrap();
                    if i != len - 1 {
                        source.push_str(", ")
                    }
                }
                source.push_str("}")
            }
            Literal::ArraySized(_, _) => todo!(),
            Literal::Number(num) => source.push_str(&num.to_string()),
            Literal::String(s) => source.push_str(&format!("{s:?}")),
            Literal::Boolean(b) => source.push_str(if *b { "true" } else { "false" }),
        },
        Expression::FunctionCall(inner) => {
            transpile_expr(source, scope, inner.value.as_ref(), ctx).unwrap();
            source.push('(');

            let len = inner.arguments.iter().len();
            for (i, arg) in inner.arguments.iter().enumerate() {
                transpile_expr(source, scope, arg, ctx).unwrap();
                if i != len - 1 {
                    source.push_str(", ")
                }
            }
            source.push_str(")\n")
        }
        Expression::IfExpression(_) => todo!(),
        Expression::Return(expr) => {
            source.push_str("return ");
            transpile_expr(source, scope, expr, ctx)?;
        }
        Expression::ForLoop(for_loop) => {
            println!("{for_loop:?}");
            source.push_str("for ");
            match &for_loop.pat {
                Pattern::Path(path) => {
                    let var = ctx.get_node_symbol(path);
                    source.push_str(&get_name(ctx, var));
                    source.push_str(" = ");
                }
                _ => todo!(),
            }
            match for_loop.iter.as_ref() {
                Expression::BinaryOperation(BinaryOperation {
                    operator: BinaryOperator::Range,
                    left,
                    right,
                }) => match (left.as_ref(), right.as_ref()) {
                    (
                        &Expression::Literal(Literal::Number(l)),
                        &Expression::Literal(Literal::Number(r)),
                    ) => {
                        source.push_str(&l.to_string());
                        source.push_str(", ");
                        source.push_str(&r.to_string());
                    }
                    (&Expression::Literal(Literal::Number(l)), &Expression::Path(ref path)) => {
                        source.push_str(&l.to_string());
                        source.push_str(", ");
                        source.push_str(&get_name(ctx, ctx.get_node_symbol(path)));
                    }
                    _ => {}
                },

                _ => todo!(),
            }
            source.push_str(" do \n");
            transpile_block(source, &for_loop.body, ctx, false).unwrap();
            source.push_str("end");
        }
        Expression::Path(path) => {
            let var = ctx.get_node_symbol(path);
            source.push_str(&get_name(ctx, var))
        }
    }
    Ok(())
}

fn transpile_block(
    source: &mut String,
    inner @ Block { id, .. }: &Block,
    ctx: &Context<'_>,
    is_function: bool,
) -> Result<(), ()> {
    let mut scope = ctx.get_scope_from_node_id(*id).unwrap();

    let len = inner.statements.len();
    for (i, stmt) in inner.statements.iter().enumerate() {
        match stmt {
            Statement::Empty => {}
            Statement::Expression(expr) => {
                if i == len - 1 && is_function && inner.does_return {
                    source.push_str("return ");
                }
                transpile_expr(source, scope, expr, ctx).unwrap()
            }
            Statement::LetDeclaration(inner) => {
                let original_scope = scope;
                if let Some(new_scope) = ctx.get_scope_from_node_id(inner.id) {
                    scope = new_scope;
                }

                match &inner.value {
                    Some(v) => {
                        let symbols = inner.pat.destructure_symbols(ctx);
                        if symbols.len() == 1 {
                            let (path, symbol) = &symbols[0];
                            source.push_str(&format!("local {} = ", get_name(ctx, symbol.clone())));
                            // Initialization expression for the LetDeclaration that is shadowing should still
                            // use the scope outside the declaration
                            transpile_expr(source, original_scope, v, ctx).unwrap();
                            source.push('\n')
                        } else {
                            panic!();
                        }
                    }
                    None => {
                        let symbols = inner.pat.destructure_symbols(ctx);
                        for (_, symbol) in symbols {
                            source.push_str(&format!("local {}", get_name(ctx, symbol)));
                            source.push('\n')
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

pub fn lua(inner: &Program, ctx: &Context<'_>) -> String {
    let mut source = String::new();
    let mut main = None;
    for p in inner.items.iter() {
        match p {
            Item::FunctionDeclaration(fn_decl @ FunctionDeclaration { id, .. }) => {
                if fn_decl.span.to_string() == "main" {
                    main = Some(fn_decl);
                    continue;
                }
                let name = get_name(ctx, ctx.get_node_symbol(fn_decl));
                source.push_str(&format!("function {}(", name)[..]);
                let len = fn_decl.parameters.0.len();
                for (i, (pat, _)) in fn_decl.parameters.0.iter().enumerate() {
                    if let Pattern::Path(path) = pat {
                        source.push_str(&path.last());
                        if i != len - 1 {
                            source.push_str(", ")
                        }
                    } else {
                        panic!()
                    }
                }
                source.push_str(")\n");
                transpile_block(&mut source, &fn_decl.body, ctx, true).unwrap();
                source.push_str("\nend\n");
            }
            Item::TupleStructDeclaration(_) => todo!(),
            Item::StructDeclaration(_) => todo!(),
            Item::Mod(module) => {
                source.push_str("-- module goes here\n");
            }
        }
    }

    transpile_block(&mut source, &main.unwrap().body, ctx, true).unwrap();

    source
}
