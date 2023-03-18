use std::{collections::HashMap, hash::Hash, fmt::{Display, Formatter}};
use std::fmt::Debug;
use super::parser::ast::*;
pub use fn_decl::typecheck_function_declaration;
pub use program::typecheck_program;
pub use stmt::typecheck_statement;
pub use expr::typecheck_expression;
pub use block::typecheck_block;
mod fn_decl;
mod program;
mod stmt;
mod expr;
mod block;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Symbol {
	name: String,
	scope: usize,
}
impl Debug for Symbol {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}@{}", self.name, self.scope)
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
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

#[derive(Clone, Debug)]
pub struct State {
	pub scope: usize,
	pub expect_return: Option<Type>,
}