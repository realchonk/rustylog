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
}
