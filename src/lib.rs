pub mod parser;
pub mod typecheck;

#[test]
fn test() {
    let program = parser::parse(include_str!("../source")).unwrap();
    std::fs::write("ast", format!("{:#?}", program)).unwrap();
    let e = typecheck::typecheck_program(&program).unwrap();
    println!("{:#?}", e);
    
}

fn condition_never() -> u32 {
	if (return 1) {
		1
	} else {
        2
    }
}