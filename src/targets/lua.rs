use crate::{typecheck::{Context, Symbol, ScopeId}, parser::ast::{Program, Item, Block, Statement, Expression, Literal, Node}};

// proof of concept for using chia for transpilation
// a glaring problem is that not enough information is preserved during typechecking
// that forces transpilation to recompute all the scopes chain and other stuff
// the correct solution may be to overhaul the AST, and allow
// the typechecker to append collected information onto each node
// sounds tedious though

fn transpile_expr(source: &mut String, scope: ScopeId, expr: &Expression, ctx: &Context<'_>) -> Result<(), ()> {
	match expr {
		Expression::Break(_) => todo!(),
		Expression::Index(_) => todo!(),
		Expression::Block(_) => todo!(),
		Expression::BinaryOperation(_) => todo!(),
		Expression::UnaryOperation(_) => todo!(),
		Expression::Literal(Node{inner, ..}) => {
			match inner {
				Literal::Unit => source.push_str("nil"),
				Literal::Array(_) => todo!(),
				Literal::ArraySized(_, _) => todo!(),
				Literal::Number(num) => source.push_str(&num.to_string()),
				Literal::String(s) => source.push_str(&format!("{s:?}")),
				Literal::Boolean(b) => source.push_str(if *b {"true"} else {"false"}),
			}
		},
		Expression::FunctionCall(Node{inner, ..}) => {
			transpile_expr(source, scope, inner.value.as_ref(), ctx).unwrap();
			source.push('(');
			
			let len = inner.arguments.iter().len();
			for (i, arg) in inner.arguments.iter().enumerate() {
				transpile_expr(source, scope, arg, ctx).unwrap();
				if i != len-1 {
					source.push_str(", ")
				}
			}
			source.push_str(")\n")
			
		},
		Expression::IfExpression(_) => todo!(),
		Expression::Return(_) => todo!(),
		Expression::ForLoop(_) => todo!(),
		Expression::Path(path) => {
			let var = ctx.symtab.get_variable_symbol(scope, path.inner.ident()).unwrap();
			source.push_str(&var.ident())
		},
	}
	Ok(())
}

fn transpile_block(source: &mut String, Node {inner, id}: &Node<Block>, ctx: &Context<'_>) -> Result<(), ()> {
	let mut scope = ctx.get_scope_immut(*id).unwrap();
	for stmt in inner.statements.iter() {
		match stmt {
			Statement::Empty => {},
			Statement::Expression(expr) => transpile_expr(source, scope, expr, ctx).unwrap(),
			Statement::LetDeclaration(Node {inner, id}) => {
				let original_scope = scope;
				if let Some(new_scope) = ctx.get_scope_immut(*id) {
					scope = new_scope;
				}

				match &inner.value {
					Some(v) => {
						let name = Symbol {scope, name: inner.pat.inner.ident(), ..Default::default()}.ident();
						source.push_str(&format!("local {} = ", name));
						// Initialization expression for the LetDeclaration that is shadowing should still
						// use the scope outside the declaration
						transpile_expr(source, original_scope, v, ctx).unwrap();
						source.push('\n')
					}
					None => {
						let name = Symbol {scope, name: inner.pat.inner.ident(), ..Default::default()}.ident();
						source.push_str(&format!("local {}", name));
						source.push('\n')
					}
				}
			},
		}
	}
	Ok(())
}

pub fn lua(Node {inner, id}: &Node<Program>, ctx: &Context<'_>) -> String {
	let mut source = String::new();
	for p in inner.items.iter() {
		match p {
			Item::FunctionDeclaration(Node {inner: fn_decl, id}) => {
				let scope = *ctx.symtab.parents.get(&ctx.get_scope_immut(fn_decl.body.id).unwrap()).unwrap();
				let name = Symbol {scope, name: fn_decl.name.clone(), ..Default::default()}.ident();
				source.push_str(&format!("function {}()\n", name)[..]);
				transpile_block(&mut source, &fn_decl.body, ctx).unwrap();
				source.push_str("\nend\n");
			},
			Item::TupleStructDeclaration(_) => todo!(),
			Item::StructDeclaration(_) => todo!(),
		}
	}
	source
}