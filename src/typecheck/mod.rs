use super::parser::ast::*;
pub use expr::block::typecheck_block;
pub use expr::typecheck_expression;
pub use fn_decl::typecheck_function_declaration;
pub use program::typecheck_program;
use std::fmt::Debug;
use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    hash::Hash,
};
pub use stmt::typecheck_statement;
mod expr;
mod fn_decl;
mod program;
mod stmt;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum SymbolName {
    External(String),
    Internal(usize),
}

impl Debug for SymbolName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolName::External(name) => write!(f, "{:?}", name),
            SymbolName::Internal(id) => write!(f, "<{:?}>", id),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Symbol {
    pub name: SymbolName,
    pub scope: usize,
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Symbol { name, scope } = self;
        match name {
            SymbolName::External(name) => write!(f, "{}@{}", name, scope),
            SymbolName::Internal(id) => write!(f, "{}@{}", id, scope),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Number,
    String,
    Boolean,
    Unit,
    Range,
    Function(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
    /// For example `struct Vector {x: number, y: number}` will insert a new type to the symtab
    Symbol(Symbol),
    Never,
}
impl Type {
    fn union(&self, other: &Type) -> Result<Type, CompilerError> {
        match (self, other) {
            (Type::Never, _) => Ok(other.clone()),
            (_, Type::Never) => Ok(self.clone()),
            (left, right) if left == right => Ok(left.clone()),
            _ => Err(CompilerError::AnyError(format!(
                "Cannot unify {:?} and {:?}",
                self, other
            ))),
        }
    }
    fn is_subtype(&self, other: &Type) -> bool {
        match (self, other) {
            (t1, t2) if t1 == t2 => true,
            (Type::Never, _) => true,
            (_, Type::Never) => false,
            _ => false,
        }
    }
}

enum NodeRef<'a> {
    FunctionDeclaration(&'a FunctionDeclaration),
    LetDeclaration(&'a LetDeclaration),
    ForLoop(&'a ForLoop),
}
impl<'a> Debug for NodeRef<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            NodeRef::FunctionDeclaration(func) => write!(f, "FunctionDeclaration({:?})", func.name),
            NodeRef::LetDeclaration(decl) => write!(f, "LetDeclaration({:?})", decl.pat.ident()),
            NodeRef::ForLoop(loop_) => write!(f, "ForLoop({:?})", loop_.pat.ident()),
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
        };
    }
    fn get_variable(&self, mut scope: usize, name: SymbolName) -> Option<Type> {
        loop {
            if let Some((ty, _)) = self.variables.get(&Symbol {
                name: name.clone(),
                scope,
            }) {
                return Some(ty.clone());
            } else if let Some(parent) = self.parents.get(&scope) {
                scope = *parent;
            } else {
                return None;
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
                        t => panic!("{t}"),
                    }
                }
                TypeExpr::Unit => return Type::Unit,
                _ => panic!(),
            }
        }
        panic!()
    }
}

#[derive(Debug)]
pub enum CompilerError {
    MismatchedTypes(String),
    AnyError(String),
    VariableNotFound(SymbolName),
    Unknown,
}

type CompilerResult<T> = Result<T, CompilerError>;

/// Generic typecheck output information
#[derive(Debug)]
pub struct TypecheckOutput {
    // The type of the expression
    pub ty: Type,
    // What value break exited with
    pub exit_ty: Type,
}
impl From<Type> for TypecheckOutput {
    fn from(ty: Type) -> Self {
        Self {
            ty,
            ..Default::default()
        }
    }
}

impl Default for TypecheckOutput {
    fn default() -> Self {
        Self {
            ty: Type::Never,
            exit_ty: Type::Never,
        }
    }
}

#[derive(Debug)]
pub struct Context<'a> {
    pub symtab: Symtab<'a>,
}

impl<'a> Context<'a> {
    fn iter_item_ty(&self, ty: Type) -> Option<Type> {
        match ty {
            Type::Range => Some(Type::Number),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct State {
    pub scope: usize,
    pub expect_return_type: Option<Type>,
    pub expect_break: bool,
}

impl Default for State {
    fn default() -> Self {
        Self {
            scope: 0,
            expect_return_type: None,
            expect_break: false,
        }
    }
}
