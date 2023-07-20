use crate::{
    parser::ast::{
        BinaryOperation, Block, Expression, Item, Literal, Node, Pattern, Program, Statement,
    },
    typecheck::{Context, ScopeId, Symbol},
};

// proof of concept for using chia for transpilation
// currently all identifiers are `{name}_{scope}`
// which is certainly helpful for avoiding naming conflict
// but also makes it impossible to reference global variables reliably

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
        Expression::Literal(Node { inner, .. }) => match inner {
            Literal::Unit => source.push_str("nil"),
            Literal::Array(_) => todo!(),
            Literal::ArraySized(_, _) => todo!(),
            Literal::Number(num) => source.push_str(&num.to_string()),
            Literal::String(s) => source.push_str(&format!("{s:?}")),
            Literal::Boolean(b) => source.push_str(if *b { "true" } else { "false" }),
        },
        Expression::FunctionCall(Node { inner, .. }) => {
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
        Expression::ForLoop(Node {
            inner: loop_inner,
            id,
        }) => {
            source.push_str("for ");
            match &loop_inner.pat {
                Pattern::Path(path) => {
                    let var = ctx.get_node_symbol(&path);
                    source.push_str(&var.ident());
                    source.push_str(" = ");
                }
                _ => todo!(),
            }
            match loop_inner.iter.as_ref() {
                Expression::BinaryOperation(Node { inner, id }) => {
                    match (inner.left.as_ref(), inner.right.as_ref()) {
                        (
                            &Expression::Literal(Node {
                                inner: Literal::Number(l),
                                ..
                            }),
                            &Expression::Literal(Node {
                                inner: Literal::Number(r),
                                ..
                            }),
                        ) => {
                            source.push_str(&l.to_string());
                            source.push_str(", ");
                            source.push_str(&r.to_string());
                        }
                        _ => todo!(),
                    }
                }
                _ => todo!(),
            }
            source.push_str(" do \n");
            transpile_block(source, &loop_inner.body, ctx).unwrap();
            source.push_str("end");
        }
        Expression::Path(path) => {
            let var = ctx.get_node_symbol(path);
            source.push_str(&var.ident())
        }
    }
    Ok(())
}

fn transpile_block(
    source: &mut String,
    Node { inner, id }: &Node<Block>,
    ctx: &Context<'_>,
) -> Result<(), ()> {
    let mut scope = ctx.get_scope_immut(*id).unwrap();

    let len = inner.statements.len();
    for (i, stmt) in inner.statements.iter().enumerate() {
        match stmt {
            Statement::Empty => {}
            Statement::Expression(expr) => {
                if i == len - 1 && inner.does_return {
                    source.push_str("return ");
                }
                transpile_expr(source, scope, expr, ctx).unwrap()
            }
            Statement::LetDeclaration(Node { inner, id }) => {
                let original_scope = scope;
                if let Some(new_scope) = ctx.get_scope_immut(*id) {
                    scope = new_scope;
                }

                match &inner.value {
                    Some(v) => {
                        let name = Symbol {
                            scope,
                            name: inner.pat.as_path_symbol(),
                            ..Default::default()
                        }
                        .ident();
                        source.push_str(&format!("local {} = ", name));
                        // Initialization expression for the LetDeclaration that is shadowing should still
                        // use the scope outside the declaration
                        transpile_expr(source, original_scope, v, ctx).unwrap();
                        source.push('\n')
                    }
                    None => {
                        let name = Symbol {
                            scope,
                            name: inner.pat.as_path_symbol(),
                            ..Default::default()
                        }
                        .ident();
                        source.push_str(&format!("local {}", name));
                        source.push('\n')
                    }
                }
            }
        }
    }
    Ok(())
}

pub fn lua(Node { inner, id }: &Node<Program>, ctx: &Context<'_>) -> String {
    let mut source = String::new();
	let mut main = None;
    for p in inner.items.iter() {
        match p {
            Item::FunctionDeclaration(node @ Node { inner: fn_decl, id }) => {
                if fn_decl.name.ident() == "main" {
					main = Some(fn_decl);
                    continue;
                }
                let name = ctx.get_node_symbol(node).ident();
                source.push_str(&format!("function {}(", name)[..]);
                let len = fn_decl.parameters.inner.0.len();
                for (i, (pat, _)) in fn_decl.parameters.inner.0.iter().enumerate() {
                    source.push_str(&pat.as_path_symbol().ident());
                    if i != len - 1 {
                        source.push_str(", ")
                    }
                }
                source.push_str(")\n");
                transpile_block(&mut source, &fn_decl.body, ctx).unwrap();
                source.push_str("\nend\n");
            }
            Item::TupleStructDeclaration(_) => todo!(),
            Item::StructDeclaration(_) => todo!(),
        }
    }
	
	transpile_block(&mut source, &main.unwrap().body, ctx).unwrap();

    source
}
