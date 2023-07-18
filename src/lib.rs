pub mod parser;
pub mod typecheck;
pub mod targets;

#[test]
fn test() {
    let program = parser::parse(include_str!("../source")).unwrap();
    std::fs::write("ast", format!("{:#?}", program)).unwrap();
    let e = typecheck::typecheck_program(&program).unwrap();
    std::fs::write("tyck", format!("{:#?}", e)).unwrap();
    let out = targets::lua(&program, &e);
    std::fs::write("out.lua", out).unwrap();
}
