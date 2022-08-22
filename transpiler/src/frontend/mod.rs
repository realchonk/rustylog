use std::fmt::{Display, Formatter};
use std::rc::Rc;

pub mod input;
pub mod token;
pub mod lexer;

#[derive(Debug, Clone)]
pub struct SourceLocation {
	file: Rc<String>,
	line: usize,
	col: usize,
}

impl Display for SourceLocation {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}:{}:{}", self.file, self.line, self.col)
	}
}
