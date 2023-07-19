use super::parser::ast::*;
pub use expr::block::typecheck_block;
pub use expr::typecheck_expression;
pub use fn_decl::typecheck_function_declaration;
pub use program::typecheck_program;
use std::fmt::{Debug, Display};
use std::{collections::HashMap, fmt::Formatter, hash::Hash};
pub use stmt::typecheck_statement;
mod expr;
mod fn_decl;
mod program;
mod stmt;

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
    pub field_of: Option<Box<Symbol>>,
    pub scope: ScopeId,
}

impl Symbol {
    pub fn ident(&self) -> String {
        format!("{}_{}", self.name.ident(), self.scope)
    }
}

impl Default for Symbol {
    fn default() -> Self {
        Self {
            name: SymbolName::Internal(0),
            field_of: None,
            scope: ScopeId(0),
        }
    }
}

/*
    struct A;
    type B = A;
    fn e(){
        let a = A; // a can be constructed with A
        let b = B; // but not b because B is a type alias
        // which indicates that they are stored differently despite both being types
        // i think this means that A will have a type entry and a struct entry??
    }
*/

impl Debug for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Symbol {
            name,
            scope,
            field_of,
        } = self;
        if let Some(field_of) = field_of {
            write!(f, "{:?}", field_of);
        }
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

pub enum NodeRef<'a> {
    FunctionDeclaration(&'a FunctionDeclaration),
    LetDeclaration(&'a LetDeclaration),
    ForLoop(&'a ForLoop),
}
impl<'a> Debug for NodeRef<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            NodeRef::FunctionDeclaration(func) => write!(f, "FunctionDeclaration({:?})", func.name),
            NodeRef::LetDeclaration(decl) => write!(f, "LetDeclaration({:?})", decl.pat.inner.ident()),
            NodeRef::ForLoop(loop_) => write!(f, "ForLoop({:?})", loop_.pat.inner.ident()),
        }
    }
}

/*

let a = 1
Symtab {variables: { a@0: (Number, LetDeclaration) }}

fn foo(param: string) {
    let f = 1;
}
Symtab {variables: {
    foo@0: (Function([String])->Unit, FnDeclaration),
    f@1: (Number, LetDeclaration),
}}

struct Vec3 { x: number, y: number, z: number }
Symtab {
    types: {
        Vec3@0: (Symbol(Vec3@0), StructDeclaration)
        Vec3::x@0: (Number, StructDeclaration),
        Vec3::y@0: (Number, StructDeclaration),
        Vec3::z@0: (Number, StructDeclaration),
    },
    structs: {
        Vec3@0: (Symbol(Vec3@0), StructDeclaration)
    }
}

type C = number;
Symtab { types: { C@0: (Number, TypeAlias) } }

let instance = S {s: 1};

*/

#[derive(Debug)]
pub struct Symtab<'a> {
    pub variables: HashMap<Symbol, (Type, NodeRef<'a>)>,
    pub types: HashMap<Symbol, (Type, NodeRef<'a>)>,
    pub structs: HashMap<Symbol, (Type, NodeRef<'a>)>,
    pub parents: HashMap<ScopeId, ScopeId>,
}

impl<'a> Symtab<'a> {
    fn new<'b>() -> Symtab<'b> {
        Symtab {
            variables: HashMap::new(),
            types: HashMap::new(),
            structs: HashMap::new(),
            parents: HashMap::new(),
        }
    }
    pub fn get_variable_symbol(&self, mut scope: ScopeId, name: &SymbolName) -> Option<Symbol> {
        loop {
            let sym = Symbol {
                name: name.clone(),
                scope,
                ..Default::default()
            };
            if self.variables.contains_key(&sym) {
                return Some(sym);
            } else if let Some(parent) = self.parents.get(&scope) {
                scope = *parent;
            } else {
                return None;
            }
        }
    }
    pub fn get_variable(&self, scope: ScopeId, name: &SymbolName) -> Option<Type> {
        self.get_variable_symbol(scope, name).map(|sym|self.variables.get(&sym).unwrap().0.clone())
    }
    pub fn get_type(&self, scope: ScopeId, expr: &TypeExpr) -> Type {
        // ScopeId(0) is the global scope
        if scope == ScopeId(0) {
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
        todo!("getting types for not global scope")
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
    pub scopes_by_node_id: HashMap<usize, ScopeId>,
    pub scope_counter: usize,
}

impl<'a> Context<'a> {
    fn new() -> Self {
        Context {
            symtab: Symtab::new(),
            scopes_by_node_id: HashMap::new(),
            scope_counter: 0,
        }
    }
    pub fn iter_item_ty(&self, ty: Type) -> Option<Type> {
        match ty {
            Type::Range => Some(Type::Number),
            _ => None,
        }
    }
    fn get_new_scope(&mut self) -> ScopeId {
        let s = self.scope_counter;
        self.scope_counter += 1;
        return ScopeId(s);
    }
    pub fn get_scope_immut(&self, node_id: usize) -> Option<ScopeId> {
        self.scopes_by_node_id.get(&node_id).map(|e|*e)
    }
    fn get_scope_from_node_id(&mut self, node_id: usize) -> ScopeId {
        self.scopes_by_node_id
            .get(&node_id)
            .map(|e| *e)
            .unwrap_or_else(|| {
                let new_scope = self.get_new_scope();
                self.scopes_by_node_id.insert(node_id, new_scope);
                new_scope
            })
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct ScopeId(usize);

impl Display for ScopeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

#[derive(Clone, Debug, Default)]
pub struct State {
    pub scope: ScopeId,
    pub expect_return_type: Option<Type>,
    pub expect_break: bool,
}
