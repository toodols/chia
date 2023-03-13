use std::{collections::HashMap, hash::Hash, fmt::{Display, Formatter}};
use std::fmt::Debug;
use super::parser::ast::*;

#[derive(Clone, PartialEq, Eq, Hash)]
struct Symbol {
	name: String,
	scope: usize,
}
impl Debug for Symbol {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}@{}", self.name, self.scope)
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Type {
	Number,
	String,
	Boolean,
	Unit,
	Function(Vec<Type>, Box<Type>),
	Tuple(Vec<Type>),
	/// For example `struct Vector {x: number, y: number}` will insert a new type to the symtab
	Symbol(Symbol) 
}

enum NodeRef<'a> {
	FunctionDeclaration(&'a FunctionDeclaration),
	LetDeclaration(&'a LetDeclaration),
}
impl<'a> Debug for NodeRef<'a> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			NodeRef::FunctionDeclaration(func) => write!(f, "FunctionDeclaration({})", func.name),
			NodeRef::LetDeclaration(decl) => write!(f, "LetDeclaration({})", decl.name),
		}
	}
}

/*
HOW EXPRESSIONS SHOULD BE TRANSLATED

let a = 1
Symtab {variables: { a@0: (Number, LetDeclaration(a)) }}

struct Vec3 { x: number, y: number, z: number }
Symtab { types: { Vec3@0: (Symbol(Vec3@0), StructDeclaration(Vec3)) } }
*/

#[derive(Debug)]
pub struct Symtab<'a> {
	variables: HashMap<Symbol, (Type, NodeRef<'a>)>,
	types: HashMap<Symbol, (Type, NodeRef<'a>)>,
	parents: HashMap<usize, usize>,
}

impl<'a> Symtab<'a> {
	fn new<'b>() -> Symtab<'b> {
		return Symtab {
			variables: HashMap::new(),
			types: HashMap::new(),
			parents: HashMap::new(),
		}
	}
	fn get_variable(&self, mut scope: usize, name: String) -> Option<Type> {
		loop {
			if let Some((ty, _)) = self.variables.get(&Symbol{name: name.clone(), scope}) {
				return Some(ty.clone());
			} else if let Some(parent) = self.parents.get(&scope) {
				scope = *parent;
			} else {
				return None
			}
		}
	}
	fn get_type(&self, scope: usize, expr: &TypeExpr) -> Type {
		if scope == 0 {
			match expr {
				TypeExpr::Identifier(name) => {
					return match name.as_str() {
						"number" => Type::Number,
						"string" => Type::String,
						"boolean" => Type::Boolean,
						_ => panic!(),
					}
				},
				TypeExpr::Unit => return Type::Unit,
				_ => panic!()
			}
		}
		panic!()
	}
}

#[derive(Debug)]
pub enum CompilerError {
	MismatchedTypes(String),
	AnyError(String),
	VariableNotFound(String),
	Unknown,
}

type CompilerResult<T> = Result<T, CompilerError>;

#[derive(Debug)]
pub struct Context<'a> {
	pub symtab: Symtab<'a>,
}

pub fn typecheck_program<'a>(program: &'a Program) -> CompilerResult<Context<'a>> {
	let mut context = Context {
		symtab: Symtab::new(),
	};
	for func in program.functions.iter() {
		context.symtab.variables.insert(Symbol{
			name: func.name.clone(),
			scope: 0,
		}, (Type::Function(
			func.parameters.iter().map(
				|(name, ty_expr)| context.symtab.get_type(0, ty_expr)
			).collect(),
			Box::new(context.symtab.get_type(0, &func.return_type))), NodeRef::FunctionDeclaration(func)));
	}
	for func in program.functions.iter() {
		context = typecheck_function_declaration(context, 0, func)?;
	}
	Ok(context)
}

fn typecheck_function_declaration<'a>(mut context: Context<'a>, parent_scope: usize, function: &'a FunctionDeclaration) -> CompilerResult<Context<'a>> {
	let scope = function.body.node_id;
	context.symtab.parents.insert(scope, parent_scope);
	for (name, type_expr) in function.parameters.iter() {
		context.symtab.variables.insert(Symbol{
			name: name.clone(),
			scope,
		}, (context.symtab.get_type(parent_scope, type_expr), NodeRef::FunctionDeclaration(function)));
	}
	let return_type = context.symtab.get_type(parent_scope, &function.return_type);
	
	let mut ty = Type::Unit;
	for stmt in function.body.statements.iter() {
		(ty, context) = typecheck_statement(context, scope, stmt)?;
	}

	if function.body.does_return {
		if return_type!=ty {
			Err(CompilerError::MismatchedTypes(format!("expected {return_type:?}, got {ty:?} at {:?}", function.name)))
		} else {
			Ok(context)
		}
	} else {
		if return_type == Type::Unit {
			Ok(context)
		} else {
			Err(CompilerError::MismatchedTypes(format!("expected {:?}, got {:?} at {:?}", return_type, Type::Unit, function.name)))
		}
	}
}



fn typecheck_statement<'a>(mut context: Context<'a>, scope: usize, stmt: &'a Statement) -> CompilerResult<(Type, Context<'a>)> {
	match stmt {
		Statement::Empty => Ok((Type::Unit, context)),
		Statement::Expression(expr) => typecheck_expression(context, scope, expr),
		Statement::LetDeclaration(decl) => {
			let expr = match &decl.value { Some(val)=>val, None=>Err(CompilerError::AnyError(format!("{} must be initialized", decl.name)))? };
			let ty: Type;
			(ty, context) = typecheck_expression(context, scope, expr)?;
			let symbol = Symbol{
				name: decl.name.clone(),
				scope,
			};
			context.symtab.variables.insert(symbol, (ty, NodeRef::LetDeclaration(decl)));
			Ok((Type::Unit, context))
		}
	}
}

fn typecheck_expression<'a>(mut context: Context<'a>, scope: usize, expr: &'a Expression) -> CompilerResult<(Type, Context<'a>)>{
	match expr {
		Expression::Literal(Literal::Number(_)) => Ok((Type::Number, context)),
		Expression::Literal(Literal::String(_)) => Ok((Type::String, context)),
		Expression::Literal(Literal::Boolean(_)) => Ok((Type::Boolean, context)),
		Expression::Literal(Literal::Identifier(name)) => {
			context.symtab.get_variable(scope, name.clone()).map(|t|(t,context)).ok_or(CompilerError::VariableNotFound(name.clone()))
		}
		Expression::BinaryOperation(op @ BinaryOperation{operator: BinaryOperator::Add, .. }) => {
			let (left_ty, context) = typecheck_expression(context, scope, &op.left)?;
			let (right_ty, context) = typecheck_expression(context, scope, &op.right)?;
			match (left_ty, right_ty) {
				(Type::Number, Type::Number) => Ok((Type::Number, context)),
				_=> Err(CompilerError::Unknown)
			}
		}
		Expression::FunctionCall(fn_call)=>{
			let t;
			(t,context) = typecheck_expression(context, scope, fn_call.value.as_ref())?;
			if let Type::Function(params, return_type) = t {
				let mut v = Vec::with_capacity(params.len());
				for arg in fn_call.arguments.iter() {
					let arg_ty;
					(arg_ty, context) = typecheck_expression(context, scope, arg)?;
					v.push(arg_ty);
				}
				if v == params {
					Ok((*return_type, context))
				} else {
					Err(CompilerError::MismatchedTypes(format!("expected {:?}, got {:?}", params, v)))
				}
			} else {
				Err(CompilerError::MismatchedTypes(format!("expected function type, got {:?}", t)))			}
		}
		_=>panic!("Not implemented for {:?}", expr)
	}
}