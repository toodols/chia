My attempt at creating a rust-like statically typed language. Obviously rust is too large of a language for me to even recreate a fraction of, much less trait solving and borrow checking. This would be useful as an intermediate language for transpilation.

### Resources
i used the [rust reference](https://doc.rust-lang.org/reference/) for rust specific kinks (like syntax)
`parser` implementation inspired by the reference and [rustc_ast](https://github.com/rust-lang/rust/blob/master/compiler/rustc_ast), rustc_parse, rustc_span, yada yada.
`typecheck` implementation takes inspiration from [rustc_hir](https://github.com/rust-lang/rust/blob/master/compiler/rustc_hir/src/def.rs)

### Todo List:
- [ ] Implement basic interpreter alongside basic global functions
- [ ] Implement struct definitions
- [ ] Implement and allow invocation of simple macros that cover use cases that are not yet implemented
- [x] Fix lua transpiler not emitting `return` for the "final operand"
- [ ] Allow declaring modules and using module-scoped definitions
- [ ] Implement useful ast transformations
- [ ] Allow items (functions) to be defined outside global scope
- [ ] support (tuple, ~~struct~~) pattern destructuring and path qualifiers

#### Maybe
- Upgrade type system (which currently covers same type cases and never + type cases) to actually do the stuff type theory does