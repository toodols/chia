pub mod parser;
pub mod typecheck;

#[test]
fn test() {
    let program = parser::parse(include_str!("../source")).unwrap();
    let e = typecheck::typecheck_program(&program).unwrap();
    println!("{:#?}", e);
}
