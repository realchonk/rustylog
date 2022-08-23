use rustylog::frontend::parser::*;

fn main() {
	let code = "(10 * -2) + 2[10..10] / self.abc - A::X { a, b: 42, } - f(x, 42)";
	let expr = parse_expr(code);
	println!("{}", expr);

	let code = "if self.a[10..0] { self.a <= 42; } else {}";
	let stmt = parse_stmt(code);
	println!("{}", stmt);
}
