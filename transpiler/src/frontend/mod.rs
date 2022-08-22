use std::fmt::{Display, Formatter};
use std::rc::Rc;

pub mod input;
pub mod token;
pub mod lexer;
pub mod ast;

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


#[derive(Debug)]
pub struct WithPos<T: Display> {
	pub begin: SourceLocation,
	pub end: SourceLocation,
	pub inner: T,
}

impl<T: Display> Display for WithPos<T> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.inner)
	}
}

pub trait WithPosTrait
where
	Self: Sized + Display
{
	fn with_pos(self, begin: SourceLocation, end: SourceLocation) -> WithPos<Self> {
		WithPos {
			begin,
			end,
			inner: self,
		}
	}
}

impl<T: Sized + Display> WithPosTrait for T {}

type WithPosBox<T> = Box<WithPos<T>>;

