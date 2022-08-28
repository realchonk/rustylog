use rustylog::frontend::{parser::*, ast::Program};

fn main() {
	let code = r#"
pub struct A {
	pub clk: Input<logic>,
	pub data_in: Input<[logic; 8]>,
	pub data_out: Output<[logic; 8]>,

	pub data2: Output<[logic; 8]>,
}

impl A {
	#[always_ff(clk: posedge)]
	fn update_data(&mut self) {
		self.data_out <= self.data_in;
	}

	#[always_comb]
	fn update_data2(&mut self) {
		if self.data_in[0..2] {
			self.data2 = 42;
		} else {
			self.data2 = 43;
		}
	}
}
"#;
	let program = parse::<Program>(code).unwrap();
	println!("{}", program);
}
