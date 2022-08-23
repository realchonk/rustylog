use rustylog::frontend::parser::parse_expr;

fn main() {
	let code = "(10 * -2) + 2[10..10] / self.abc - A::X { a, b: 42, } - f(x, 42)";
	let expr = parse_expr(code);
	println!("{}", expr);
}
