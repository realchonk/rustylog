use rustylog::frontend::parser::*;

fn main() {
	let code = "(10 * -2) + 2[10..10] / self.abc - A::X { a, b: 42, } - f(x, 42)";
	let expr = parse_expr(code);
	println!("{}", expr);

	let code = "if self.a[10..0] { self.a <= 42; } else {}";
	let stmt = parse_stmt(code);
	println!("{}", stmt);

	let code = "#[always_ff(x: posedge)] pub fn main(&mut self, a: Input<[logic; 16]>) -> [logic; 16] { }";
	let func = parse_func(code);
	println!("{}", func);

	let code = "pub struct Name { var1: logic, var2: [tri; 32], }";
	let s = parse_struct(code);
	println!("{}", s);

	let code = "impl Name { pub fn f() {} }";
	let i = parse_impl(code);
	println!("{}", i);

	let code = r#"
pub struct A {
	pub clk: Input<logic>,
	pub data_in: Input<[logic; 8]>,
	pub data_out: Output<[logic; 8]>,
}

impl A {
	#[always_ff(clk: posedge)]
	fn update_data(&mut self) {
		self.data_out <= self.data_in;
	}
}
"#;
	let program = parse_program(code);
	println!("{}", program);
}
