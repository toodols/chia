use crate::{parser::ast::Program, typecheck::Context};
//TODO: i have no idea how to go about this

// Removes all occurrences of inline blocks expressions and replaces them with temp variables
// Example
// let a = {let c = 3; c + 1};
// Output:
// let _0 = 3;
// let a = _0 + 1;
pub fn remove_inline_blocks(inner: &Program, ctx: &Context<'_>) -> Program {
    todo!();
}

// fn foo() { if true {1} else {2} }
// fn foo() { let _0; if true {_0 = 1} else {_0 = 2}; return _0 }
