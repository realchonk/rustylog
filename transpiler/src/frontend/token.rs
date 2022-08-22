use std::fmt::{Formatter, Display};
use crate::frontend::SourceLocation;

#[derive(Debug, PartialEq, Eq)]
pub enum TokenData {
	Integer(u128),
	Name(String),
	Operator(&'static str),
	Keyword(&'static str),
	EndOfFile,
}

#[derive(Debug)]
pub struct Token {
	pub begin: SourceLocation,
	pub end: SourceLocation,
	pub data: TokenData,
}

impl Display for Token {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.data)
	}
}

impl Display for TokenData {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use TokenData::*;
		match self {
			Integer(x)	=> write!(f, "{}", x),
			Name(x)		=> write!(f, "{}", x),
			Operator(x)	=> write!(f, "{}", x),
			Keyword(x)	=> write!(f, "{}", x),
			EndOfFile	=> write!(f, "end-of-file"),
		}
	}
}
