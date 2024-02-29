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

// impl Debug for SymbolName {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         match self {
//             SymbolName::External(name) => write!(f, "{:?}", name),
//             SymbolName::Internal(id) => write!(f, "<{:?}>", id),
//         }
//     }
// }

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct Symbol(usize);

// impl Default for Symbol {
//     fn default() -> Self {
//         Self(0)
//     }
// }

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

// impl Debug for Symbol {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         let Symbol {
//             name,
//             scope,
//             field_of,
//         } = self;
//         if let Some(field_of) = field_of {
//             write!(f, "{:?}", field_of);
//         }
//         match name {
//             SymbolName::External(name) => write!(f, "{}@{}", name, scope),
//             SymbolName::Internal(id) => write!(f, "{}@{}", id, scope),
//         }
//     }
// }

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
    /// Gives the union of types
    /// Union is commutative, so if `let Some(ty) = a | b`, then `b | a = Some(ty)`
    // Advanced unions like string | number is not supported and there is minimal reason to even support it
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

impl Pattern {
    pub fn destructure_symbols(&self, ctx: &Context) -> Vec<(&Path, Symbol)> {
        let mut symbols = Vec::new();
        match self {
            Pattern::Path(path) => {
                symbols.push((path, *ctx.node_id_to_symbol.get(&path.id).unwrap()))
            }
            _ => todo!(),
        }
        symbols
    }
    /// Destructure a pattern into its symbols against a type
    /// assumes all paths are singles for now
    pub fn destructure(&self, ctx: &Context, ty: &Type) -> CompilerResult<Vec<(&Path, Type)>> {
        let mut symbols = HashMap::new();

        match self {
            Pattern::Path(path) => {
                if symbols.insert(path.last(), (path, ty.clone())).is_some() {
                    return Err(CompilerError::VariableAlreadyExists(format!(
                        "Symbol {:?} already exists in pattern",
                        ctx.get_path_ty(path).unwrap(),
                    )));
                }
            }
            Pattern::Tuple(_) => todo!(),
        }
        Ok(symbols.into_values().collect())
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
            NodeRef::FunctionDeclaration(func) => write!(f, "FunctionDeclaration({:?})", func.span),
            NodeRef::LetDeclaration(decl) => {
                write!(f, "LetDeclaration({:?})", decl.pat)
            }
            NodeRef::ForLoop(loop_) => write!(f, "ForLoop({:?})", loop_.pat),
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
pub struct VarSymbolDetails<'a> {
    ty: Type,
    node_ref: Option<NodeRef<'a>>,
}

#[derive(Debug, Clone)]
enum TypeOrModule {
    Type(Type),
    Module(()),
}

#[derive(Debug)]
pub struct TySymbolDetails<'a> {
    value: TypeOrModule,
    node_ref: Option<NodeRef<'a>>,
}

#[derive(Debug)]
pub struct Namespace<T: Debug, S: Debug + Eq + PartialEq + Hash + Copy = Symbol> {
    scopes: HashMap<ScopeId, HashMap<String, S>>,
    names: HashMap<S, (String, ScopeId)>,
    values: HashMap<S, T>,
}
impl<T: Debug, S: Debug + Eq + PartialEq + Hash + Copy> Default for Namespace<T, S> {
    fn default() -> Self {
        Namespace {
            scopes: HashMap::new(),
            values: HashMap::new(),
            names: HashMap::new(),
        }
    }
}

impl<T: Debug, S: Debug + Eq + PartialEq + Hash + Copy> Namespace<T, S> {
    /// Insert a new `symbol` with `name` at `scope` with `details`
    fn insert(&mut self, symbol: S, name: String, scope: ScopeId, details: T) {
        self.scopes
            .entry(scope)
            .or_insert_with(HashMap::new)
            .insert(name.clone(), symbol);
        self.names.insert(symbol, (name, scope));
        self.values.insert(symbol, details);
    }
    pub fn get(&self, symbol: S) -> Option<&T> {
        self.values.get(&symbol)
    }
    pub fn get_name_and_scope(&self, symbol: S) -> Option<&(String, ScopeId)> {
        self.names.get(&symbol)
    }
    fn get_name_in_scope(&self, scope: ScopeId, name: &String) -> Option<S> {
        self.scopes
            .get(&scope)
            .and_then(|e| e.get(name))
            .map(|e| *e)
    }
    fn get_name_in_scope_chain(
        &self,
        parents: &HashMap<ScopeId, ScopeId>,
        mut scope: ScopeId,
        name: &String,
    ) -> Option<S> {
        loop {
            if let Some(sym) = self.get_name_in_scope(scope, name) {
                return Some(sym);
            }
            if let Some(parent) = parents.get(&scope) {
                scope = *parent;
            } else {
                return None;
            }
        }
    }
    fn get_value_in_scope_chain(
        &self,
        parents: &HashMap<ScopeId, ScopeId>,
        mut scope: ScopeId,
        name: &String,
    ) -> Option<&T> {
        loop {
            if let Some(sym) = self.get_name_in_scope(scope, name) {
                return self.get(sym);
            }
            if let Some(parent) = parents.get(&scope) {
                scope = *parent;
            } else {
                return None;
            }
        }
    }
}

#[derive(Debug)]
pub struct Symtab<'a> {
    pub variables: Namespace<VarSymbolDetails<'a>>,
    pub types: Namespace<TySymbolDetails<'a>>,
    pub parents: HashMap<ScopeId, ScopeId>,
    symbol_counter: usize,
}

impl<'a> Symtab<'a> {
    fn new<'b>() -> Symtab<'b> {
        let mut symtab = Symtab {
            variables: Namespace::default(),
            types: Namespace::default(),
            // structs: Namespace::default(),
            symbol_counter: 0,
            parents: HashMap::new(),
        };
        symtab.insert_global_ty("number", Type::Number);
        symtab.insert_global_ty("bool", Type::Boolean);
        symtab.insert_global_ty("string", Type::String);

        return symtab;
    }

    pub fn insert_global_ty(&mut self, name: &str, value: Type) {
        let symbol = self.get_new_symbol();
        self.types.insert(
            symbol,
            name.to_owned(),
            ScopeId(0),
            TySymbolDetails {
                value: TypeOrModule::Type(value),
                node_ref: None,
            },
        );
    }
    pub fn insert_global_var(&mut self, name: &str, ty: Type) {
        let symbol = self.get_new_symbol();
        self.variables.insert(
            symbol,
            name.to_owned(),
            ScopeId(0),
            VarSymbolDetails { ty, node_ref: None },
        );
    }

    pub fn get_new_symbol(&mut self) -> Symbol {
        let s = self.symbol_counter;
        self.symbol_counter += 1;
        return Symbol(s);
    }
}

#[derive(Debug)]
pub enum CompilerError {
    MismatchedTypes(String),
    AnyError(String),
    VariableAlreadyExists(String),
    VariableNotFound(String),
    TypeNotFound(String),
    Unknown,
}

type CompilerResult<T> = Result<T, CompilerError>;

/// Generic typecheck output information
#[derive(Debug)]
pub struct TypecheckOutput {
    // The type of the expression
    pub expr_ty: Type,

    // What value return exited with
    pub return_ty: Type,

    pub loop_ty: Type,
}
impl From<Type> for TypecheckOutput {
    fn from(ty: Type) -> Self {
        Self {
            expr_ty: ty,
            ..Default::default()
        }
    }
}

impl Default for TypecheckOutput {
    fn default() -> Self {
        Self {
            expr_ty: Type::Never,
            return_ty: Type::Never,
            loop_ty: Type::Never,
        }
    }
}

#[derive(Debug)]
pub struct Context<'a> {
    pub symtab: Symtab<'a>,
    node_id_to_symbol: HashMap<usize, Symbol>,
    pub scopes_by_node_id: HashMap<usize, ScopeId>,
    pub scope_counter: usize,
}

/// Node<T: SymbolicNode>'s id has a corresponding symbol
pub trait SymbolicNode: Debug + Clone {
    fn id(&self) -> usize;
}
// impl SymbolicNode for LetDeclaration {}
impl SymbolicNode for FunctionDeclaration {
    fn id(&self) -> usize {
        self.id
    }
}
impl SymbolicNode for Path {
    fn id(&self) -> usize {
        self.id
    }
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Context {
            symtab: Symtab::new(),
            node_id_to_symbol: HashMap::new(),
            scopes_by_node_id: HashMap::new(),
            scope_counter: 0,
        }
    }
    pub fn get_type(&self, scope: ScopeId, type_expr: &TypeExpr) -> Result<Option<Type>, ()> {
        match type_expr {
            TypeExpr::Unit => Ok(Some(Type::Unit)),
            TypeExpr::Identifier(name) => {
                match self
                    .symtab
                    .types
                    .get_value_in_scope_chain(&self.symtab.parents, scope, name)
                {
                    None => Ok(None),
                    Some(TySymbolDetails {
                        value: TypeOrModule::Module(module),
                        ..
                    }) => Err(()),
                    Some(TySymbolDetails {
                        value: TypeOrModule::Type(t),
                        ..
                    }) => Ok(Some(t.clone())),
                }
            }
            TypeExpr::Tuple(_) => todo!(),
            TypeExpr::Untyped => panic!(),
        }
    }
    pub fn get_new_symbol(&mut self) -> Symbol {
        self.symtab.get_new_symbol()
    }
    pub fn get_node_symbol(&self, node: &impl SymbolicNode) -> Symbol {
        return self
            .node_id_to_symbol
            .get(&node.id())
            .expect(&format!("{node:?} not there"))
            .clone();
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
    pub fn get_scope_from_node_id(&self, node_id: usize) -> Option<ScopeId> {
        self.scopes_by_node_id.get(&node_id).map(|e| *e)
    }

    /// Gets the first variable with the name in the scope chain
    pub fn get_variable_symbol(&self, scope: ScopeId, name: &String) -> Option<Symbol> {
        self.symtab
            .variables
            .get_name_in_scope_chain(&self.symtab.parents, scope, name)
    }

    /// Gets the variable's type in the scope chain
    pub fn get_variable_ty(&self, scope: ScopeId, name: &String) -> Option<Type> {
        self.symtab
            .variables
            .get_value_in_scope_chain(&self.symtab.parents, scope, name)
            .map(|e| e.ty.clone())
    }

    fn get_path_ty(&self, path: &Path) -> Option<Type> {
        let scope = self.get_scope_from_node_id(path.id)?;
        if path.path.len() == 1 {
            Some(
                self.symtab
                    .variables
                    .get_value_in_scope_chain(
                        &self.symtab.parents,
                        scope,
                        &path.path[0].to_string(),
                    )?
                    .ty
                    .clone(),
            )
        } else {
            todo!()
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct ScopeId(pub usize);

impl Display for ScopeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

#[derive(Clone, Debug, Default)]
pub struct State {
    pub scope: ScopeId,
    pub expect_break: bool,
}
