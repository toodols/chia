use crate::{typecheck::{Context, Symbol, ScopeId}, parser::ast::{Program, Item, Block, Statement, Expression, Literal}};

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
		Expression::Literal(literal) => {
			match literal {
				Literal::Unit => source.push_str("nil"),
				Literal::Array(_) => todo!(),
				Literal::ArraySized(_, _) => todo!(),
				Literal::Number(num) => source.push_str(&num.to_string()),
				Literal::String(s) => source.push_str(&format!("{s:?}")),
				Literal::Boolean(b) => source.push_str(if *b {"true"} else {"false"}),
			}
		},
		Expression::FunctionCall(fn_call) => {
			transpile_expr(source, scope, fn_call.value.as_ref(), ctx).unwrap();
			source.push('(');
			
			let len = fn_call.arguments.iter().len();
			for (i, arg) in fn_call.arguments.iter().enumerate() {
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
			let var = ctx.symtab.get_variable_symbol(scope, path.ident()).unwrap();
			source.push_str(&var.ident())
		},
	}
	Ok(())
}

fn transpile_block(source: &mut String, block: &Block, ctx: &Context<'_>) -> Result<(), ()> {
	let mut scope = ctx.get_scope_immut(block.node_id).unwrap();
	for stmt in block.statements.iter() {
		match stmt {
			Statement::Empty => {},
			Statement::Expression(expr) => transpile_expr(source, scope, expr, ctx).unwrap(),
			Statement::LetDeclaration(let_decl) => {
				if let Some(new_scope) = ctx.get_scope_immut(let_decl.node_id) {
					scope = new_scope;
				}

				match &let_decl.value {
					Some(v) => {
						let name = Symbol {scope, name: let_decl.pat.ident(), ..Default::default()}.ident();
						source.push_str(&format!("local {} = ", name));
						transpile_expr(source, scope, v, ctx).unwrap();
						source.push('\n')
					}
					None => {
						let name = Symbol {scope, name: let_decl.pat.ident(), ..Default::default()}.ident();
						source.push_str(&format!("local {}", name));
						source.push('\n')
					}
				}
			},
		}
	}
	Ok(())
}

pub fn lua(program: &Program, ctx: &Context<'_>) -> String {
	let mut source = String::new();
	for p in program.items.iter() {
		match p {
			Item::FunctionDeclaration(fn_decl) => {
				let scope = *ctx.symtab.parents.get(&ctx.get_scope_immut(fn_decl.body.node_id).unwrap()).unwrap();
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