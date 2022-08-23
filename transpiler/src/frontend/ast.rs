use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum UnaryOp { Plus, Minus, Tilde, }

#[derive(Debug)]
pub enum BinaryOp { Plus, Minus, Star, Slash, }

#[derive(Debug)]
pub struct VName {
	pub is_self: bool,
	pub ident: String,
}

#[derive(Debug)]
pub enum QName {
	Name(String),
	Sub(Box<QName>, String),
}

#[derive(Debug)]
pub enum Index {
	Single(i32),
	Range(i32, i32),
}

#[derive(Debug)]
pub enum Expression {
	Int(i64),
	VName(VName),
	Call(VName, Vec<Expression>),
	Array(Vec<Expression>),
	Constructor(QName, Vec<(String, Option<Expression>)>),
	Sub(Box<Expression>),
	Unary { op: UnaryOp, expr: Box<Expression> },
	Binary { lhs: Box<Expression>, op: BinaryOp, rhs: Box<Expression> },
	Index { base: Box<Expression>, index: Index },
}

impl Display for UnaryOp {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use UnaryOp::*;
		match self {
			Plus	=> write!(f, "+"),
			Minus	=> write!(f, "-"),
			Tilde	=> write!(f, "~"),
		}
	}
}

impl Display for BinaryOp {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use BinaryOp::*;
		match self {
			Plus	=> write!(f, "+"),
			Minus	=> write!(f, "-"),
			Star	=> write!(f, "*"),
			Slash	=> write!(f, "/"),
		}
	}
}

impl Display for VName {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		if self.is_self {
			write!(f, "self.")?;
		}
		write!(f, "{}", self.ident)
	}
}

impl Display for QName {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use QName::*;
		match self {
			Name(n)		=> write!(f, "{}", n),
			Sub(ns, n)	=> write!(f, "{}::{}", ns, n),
		}
	}
}

impl Display for Index {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use Index::*;
		match self {
			Single(x)	=> write!(f, "{}", x),
			Range(x, y)	=> write!(f, "{}..{}", x, y),
		}
	}
}

impl Display for Expression {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use Expression::*;
		match self {
			Int(x)		=> write!(f, "{}", x),
			VName(x)	=> write!(f, "{}", x),
			Call(n, p)	=> {
				write!(f, "{}(", n)?;
				if !p.is_empty() {
					let mut iter = p.iter();
					write!(f, "{}", iter.next().unwrap())?;
					for x in iter {
						write!(f, ", {}", x)?;
					}
				}
				write!(f, ")")
			},
			Array(e) => {
				write!(f, "[")?;
				if !e.is_empty() {
					let mut iter = e.iter();
					write!(f, "{}", iter.next().unwrap())?;
					for x in iter {
						write!(f, ", {}", x)?;
					}
				}
				write!(f, "]")
			},
			Constructor(n, e) => {
				if e.is_empty() {
					write!(f, "{} {{}}", n)
				} else {
					writeln!(f, "{} {{", n)?;
					for x in e {
						match x {
							(n, Some(v))	=> writeln!(f, "\t{}: {},", n, v)?,
							(n, None)		=> writeln!(f, "\t{},", n)?,
						}
					}
					write!(f, "}}")
				}
			},
			Index { base, index }	=> write!(f, "{}[{}]", base, index),
			Sub(x)					=> write!(f, "({})", x),
			Unary { op, expr }		=> write!(f, "{}{}", op, expr),
			Binary { lhs, op, rhs }	=> write!(f, "{} {} {}", lhs, op, rhs),
		}
	}
}

impl From<&str> for UnaryOp {
	fn from(s: &str) -> Self {
		use UnaryOp::*;
		match s {
			"+"	=> Plus,
			"-"	=> Minus,
			"~"	=> Tilde,
			_	=> panic!("Invalid s: '{}'", s),
		}
	}
}

impl From<&str> for BinaryOp {
	fn from(s: &str) -> Self {
		use BinaryOp::*;
		match s {
			"+"	=> Plus,
			"-"	=> Minus,
			"*"	=> Star,
			"/"	=> Slash,
			_	=> panic!("Invalid s: '{}'", s),
		}
	}
}
