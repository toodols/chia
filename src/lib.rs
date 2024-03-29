use std::{collections::HashMap, path::Path};

use crate::{
    parser::Sources,
    typecheck::{Context, Type},
};

pub mod parser;
pub mod targets;
pub mod transform;
pub mod typecheck;

#[test]
fn test() {
    static TEST_SRC: Dir = include_dir!("test/src");

    use include_dir::{include_dir, Dir, DirEntry};
    impl<'a> Sources for Dir<'a> {
        fn get(&self, mut path: &std::path::Path) -> Option<&str> {
            if path == TEST_SRC.path() {
                path = Path::new("main");
            }
            self.get_entry(path).and_then(|s| match s {
                DirEntry::Dir(dir) => dir.get_file("mod").and_then(|f| f.contents_utf8()),
                DirEntry::File(file) => file.contents_utf8(),
            })
        }

        fn child(&self, path: &std::path::Path, child: &str) -> Option<&std::path::Path> {
            if path == TEST_SRC.path() {
                self
            } else {
                self.get_dir(path).unwrap()
            }
            .get_entry(child)
            .map(|o| o.path())
        }
    }
    let mut parser = parser::Parser::new(&TEST_SRC);
    let program = parser.parse_program().unwrap();
    std::fs::write("test/out/ast", format!("{:#?}", program)).unwrap();
    let mut ctx = Context::new();
    ctx.symtab.insert_global_var(
        "print",
        Type::Function(vec![Type::String], Box::new(Type::Unit)),
    );
    typecheck::typecheck_program(&mut ctx, &program).unwrap();
    std::fs::write("test/out/tyck", format!("{:#?}", ctx)).unwrap();
    let out = targets::lua(&program, &ctx);
    std::fs::write("test/out/out.lua", out).unwrap();
}
