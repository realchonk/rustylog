use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum UnaryOp { Plus, Minus, Tilde, }

#[derive(Debug)]
pub enum BinaryOp { Plus, Minus, Star, Slash, }

#[derive(Debug)]
pub enum AssignOp { Blocking, NonBlocking, }

#[derive(Debug)]
pub enum Trigger { PosEdge, NegEdge }

#[derive(Debug)]
pub enum Visibility { Pub }

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

#[derive(Debug)]
pub struct BlockStmt(pub Vec<Statement>);

#[derive(Debug)]
pub enum Statement {
	If { cond: Expression, true_case: BlockStmt, false_case: Option<BlockStmt> },
	Assign { left: VName, op: AssignOp, right: Expression },
	Block(BlockStmt),
}

#[derive(Debug)]
pub struct SelfRef {
	pub mutable: bool,
}

#[derive(Debug)]
pub struct Function {
	pub fn_macro: Option<FnMacro>,
	pub visibility: Option<Visibility>,
	pub name: String,
	pub self_ref: Option<SelfRef>,
	pub args: Vec<(String, Type)>,
	pub ret_type: Option<Type>,
	pub body: BlockStmt,
}

#[derive(Debug)]
pub enum FnMacro {
	AlwaysFF(Vec<(String, Option<Trigger>)>),
	AlwaysComb,
}

#[derive(Debug)]
pub enum SubType {
	Array(Box<SubType>, u32),
	Logic,
	Tri,
	QName(QName),
}

#[derive(Debug)]
pub enum Type {
	Input(SubType),
	Output(SubType),
	InOut(SubType),
	Sub(SubType),
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

impl Display for AssignOp {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use AssignOp::*;
		match self {
			Blocking	=> write!(f, "="),
			NonBlocking	=> write!(f, "<="),
		}
	}
}

impl Display for Trigger {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use Trigger::*;
		match self {
			PosEdge	=> write!(f, "posedge"),
			NegEdge	=> write!(f, "negedge"),
		}
	}
}

impl Display for Visibility {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use Visibility::*;
		match self {
			Pub	=> write!(f, "pub"),
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

impl Display for BlockStmt {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		if self.0.is_empty() {
			write!(f, "{{}}")
		} else {
			writeln!(f, "{{")?;
			for stmt in &self.0 {
				let s = format!("{}", stmt);
				for s in s.split('\n') {
					writeln!(f, "\t{}", s)?;
				}
			}
			write!(f, "}}")
		}
	}
}

impl Display for Statement {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use Statement::*;
		match self {
			If { cond, true_case, false_case: Some(fc) } => write!(f, "if {} {} else {}", cond, true_case, fc),
			If { cond, true_case, false_case: None } => write!(f, "if {} {}", cond, true_case),
			Assign { left, op, right } => write!(f, "{} {} {};", left, op, right),
			Block(b) => write!(f, "{}", b),
		}
	}
}

impl Display for SelfRef {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		if self.mutable {
			write!(f, "&mut self")
		} else {
			write!(f, "&self")
		}
	}
}

impl Display for Function {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		if let Some(fn_macro) = &self.fn_macro {
			writeln!(f, "{}", fn_macro)?;
		}

		if let Some(vis) = &self.visibility {
			write!(f, "{} ", vis)?;
		}

		write!(f, "fn {}(", self.name)?;
		let mut aprefix = "";
		if let Some(self_ref) = &self.self_ref {
			write!(f, "{}", self_ref)?;
			aprefix = ", ";
		}
		if !self.args.is_empty() {
			let mut iter = self.args.iter();
			let x = iter.next().unwrap();
			write!(f, "{}{}: {}", aprefix, x.0, x.1)?;

			for x in iter {
				write!(f, ", {}: {}", x.0, x.1)?;
			}
		}
		if let Some(rt) = &self.ret_type {
			write!(f, ") -> {} {}", rt, self.body)
		} else {
			write!(f, ") {}", self.body)
		}
	}
}

impl Display for FnMacro {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use FnMacro::*;
		match self {
			AlwaysFF(params) => {
				fn write_param(f: &mut Formatter<'_>, pair: &(String, Option<Trigger>))  -> std::fmt::Result {
					match pair {
						(name, Some(trigger))	=> write!(f, "{}: {}", name, trigger),
						(name, None)			=> write!(f, "{}", name),
					}
				}
				write!(f, "#[always_ff(")?;
				let mut iter = params.iter();
				write_param(f, iter.next().unwrap())?;
				for x in iter {
					write_param(f, x)?;
				}
				write!(f, ")]")
			},
			AlwaysComb => write!(f, "#[always_comb]"),
		}
	}
}

impl Display for SubType {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use SubType::*;
		match self {
			Array(sub, num)	=> write!(f, "[{}; {}]", sub, num),
			Logic			=> write!(f, "logic"),
			Tri				=> write!(f, "tri"),
			QName(name)		=> write!(f, "{}", name),
		}
	}
}

impl Display for Type {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use Type::*;
		match self {
			Input(sub)	=> write!(f, "Input<{}>", sub),
			Output(sub)	=> write!(f, "Output<{}>", sub),
			InOut(sub)	=> write!(f, "InOut<{}>", sub),
			Sub(sub)	=> write!(f, "{}", sub),
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
			_	=> panic!("Invalid UnaryOp: {}", s),
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
			_	=> panic!("Invalid BinaryOp: {}", s),
		}
	}
}

impl From<&str> for AssignOp {
	fn from(s: &str) -> Self {
		use AssignOp::*;
		match s {
			"="  => Blocking,
			"<=" => NonBlocking,
			_	 => panic!("Invalid AssignOp: {}", s),
		}
	}
}

impl From<&str> for Trigger {
	fn from(s: &str) -> Self {
		use Trigger::*;
		match s {
			"posedge"	=> PosEdge,
			"negedge"	=> NegEdge,
			_			=> panic!("Invalid Trigger: {}", s),
		}
	}
}

impl From<&str> for Visibility {
	fn from(s: &str) -> Self {
		use Visibility::*;
		match s {
			"pub"	=> Pub,
			_		=> panic!("Invalid Visibility: {}", s),
		}
	}
}
