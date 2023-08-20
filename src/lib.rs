use std::{collections::HashMap, path::Path};

use crate::parser::Sources;

pub mod parser;
pub mod targets;
pub mod typecheck;
// pub mod transform;

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
    let mut parser = parser::Parser::<'_, Dir<'_>>::new(&TEST_SRC);
    let program = parser.parse_program().unwrap();

    std::fs::write("test/out/ast", format!("{:#?}", program)).unwrap();
    let e = typecheck::typecheck_program(&program).unwrap();
    std::fs::write("test/out/tyck", format!("{:#?}", e)).unwrap();
    let out = targets::lua(&program, &e);
    std::fs::write("test/out/out.lua", out).unwrap();
}
