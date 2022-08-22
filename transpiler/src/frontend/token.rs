use std::fmt::{Formatter, Display};

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
	Integer(u128),
	Name(String),
	Operator(&'static str),
	Keyword(&'static str),
	EndOfFile,
}

impl Display for Token {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use Token::*;
		match self {
			Integer(x)	=> write!(f, "{}", x),
			Name(x)		=> write!(f, "{}", x),
			Operator(x)	=> write!(f, "{}", x),
			Keyword(x)	=> write!(f, "{}", x),
			EndOfFile	=> write!(f, "end-of-file"),
		}
	}
}
