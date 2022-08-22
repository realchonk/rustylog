use std::fmt::{Display, Formatter};
use super::{
	WithPosBox,
	WithPos,
};

#[derive(Debug)]
pub struct VName {
	is_self: bool,
	ident: String,
}

#[derive(Debug)]
pub struct QName {
	namespaces: Vec<String>,
	ident: String,
}

#[derive(Debug)]
pub enum QVName {
	VName(WithPos<VName>),
	QName(WithPos<QName>),
}

#[derive(Debug)]
pub enum Index {
	Num(usize),
	Range(usize, usize),
}

#[derive(Debug)]
pub enum SubType {
	Logic,
	Tri,
	Array(WithPosBox<SubType>, WithPos<Index>),
}

#[derive(Debug)]
pub enum IOSpec {
	Input,
	Output,
	InOut,
}

#[derive(Debug)]
pub struct Type {
	io: Option<WithPos<IOSpec>>,
	sub: WithPos<SubType>,
}

#[derive(Debug)]
pub enum BuildExpr {
	Simple(String),
	Full(String, WithPosBox<Expression>),
}

#[derive(Debug)]
pub enum Expression {
	Name(WithPos<VName>),
	Unary(&'static str, WithPosBox<Expression>),
	Binary(WithPosBox<Expression>, &'static str, WithPosBox<Expression>),
	Build(WithPos<QName>, Vec<WithPos<BuildExpr>>),
	Call(WithPos<QName>, Vec<WithPos<Expression>>),
	IndexInto(WithPos<VName>, WithPos<Index>),
}

#[derive(Debug)]
pub struct BlockStmt(Vec<WithPos<Statement>>);

#[derive(Debug)]
pub enum Statement {
	If(WithPos<Expression>, WithPos<BlockStmt>, Option<WithPos<BlockStmt>>),
	Assignment { name: WithPos<VName>, blocking: bool, expr: WithPos<Expression> },
	Block(WithPos<BlockStmt>),
}

#[derive(Debug)]
pub enum Trigger {
	Posedge,
	Negedge,
	Pos,
	Neg,
}

#[derive(Debug)]
pub struct Sensor(WithPos<String>, Option<WithPos<Trigger>>);

#[derive(Debug)]
pub enum FnMacro {
	AlwaysFF(Vec<WithPos<Sensor>>),
	AlwaysComb,
}

#[derive(Debug)]
pub struct Declaration(String, WithPos<Type>);

#[derive(Debug)]
pub struct Function {
	fn_macro: Option<WithPos<FnMacro>>,
	is_pub: bool,
	ident: String,
	params: Vec<WithPos<Declaration>>,
	ret_val: Option<WithPos<Type>>,
	body: WithPos<BlockStmt>,
}

#[derive(Debug)]
pub struct Impl {
	ident: String,
	functions: Vec<WithPos<Function>>,
}

#[derive(Debug)]
pub struct Struct {
	is_pub: bool,
	ident: String,
	declarations: Vec<WithPos<Declaration>>,
}

#[derive(Debug)]
pub enum Element {
	Impl(WithPos<Impl>),
	Struct(WithPos<Struct>),
}

#[derive(Debug)]
pub struct Program(Vec<WithPos<Element>>);


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
		for ns in &self.namespaces {
			write!(f, "{}::", ns)?;
		}
		write!(f, "{}", self.ident)
	}
}


impl Display for QVName {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use QVName::*;
		match self {
			QName(n) => write!(f, "{}", n),
			VName(n) => write!(f, "{}", n),
		}
	}
}


impl Display for Index {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use Index::*;
		match self {
			Num(x)		=> write!(f, "{}", x),
			Range(x, y)	=> write!(f, "{}..{}", x, y),
		}
	}
}


impl Display for SubType {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use SubType::*;
		match self {
			Logic		=> write!(f, "logic"),
			Tri			=> write!(f, "tri"),
			Array(a, i)	=> write!(f, "[{}; {}]", a, i),
		}
	}
}


impl Display for IOSpec {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use IOSpec::*;
		match self {
			Input	=> write!(f, "input"),
			Output	=> write!(f, "output"),
			InOut	=> write!(f, "inout"),
		}
	}
}


impl Display for Type {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		if let Some(io) = &self.io {
			write!(f, "{}<{}>", io, self.sub)
		} else {
			write!(f, "{}", self.sub)
		}
	}
}


impl Display for BuildExpr {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use BuildExpr::*;
		match self {
			Simple(x)	=> write!(f, "{}", x),
			Full(x, e)	=> write!(f, "{}: {}", x, e),
		}
	}
}


impl Display for Expression {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use Expression::*;
		match self {
			Name(n)				=> write!(f, "{}", n),
			Unary(op, x)		=> write!(f, "{}{}", op, x),
			Binary(l, op, r)	=> write!(f, "{} {} {}", l, op, r),
			Build(n, e)			=> {
				writeln!(f, "{} {{", n)?;
				for x in e {
					let s = format!("{},", x);
					for a in s.split('\n') {
						writeln!(f, "\t{}", a)?;
					}
				}
				write!(f, "}}")
			},
			Call(n, params) => {
				write!(f, "{}(", n)?;
				if !params.is_empty() {
					let mut iter = params.iter();
					write!(f, "{}", iter.next().unwrap())?;
					for x in iter {
						write!(f, ", {}", x)?;
					}
				}
				write!(f, ")")
			},
			IndexInto(n, i) => write!(f, "{}[{}]", n, i),
		}
	}
}


impl Display for BlockStmt {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		writeln!(f, "{{")?;
		for s in &self.0 {
			let s = format!("{}", s);
			for x in s.split('\n') {
				writeln!(f, "\t{}", x)?;
			}
		}
		write!(f, "}}")
	}
}


impl Display for Statement {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use Statement::*;
		match self {
			If(cond, tc, Some(fc))	=> write!(f, "if {} {} else {}", cond, tc, fc),
			If(cond, tc, None)		=> write!(f, "if {} {}", cond, tc),
			Assignment { name, blocking: true, expr }	=> write!(f, "{} = {};", name, expr),
			Assignment { name, blocking: false, expr }	=> write!(f, "{} <= {};", name, expr),
			Block(x) => write!(f, "{}", x),
		}
	}
}

impl Display for Trigger {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use Trigger::*;
		match self {
			Posedge	=> write!(f, "posedge"),
			Negedge	=> write!(f, "negedge"),
			Pos		=> write!(f, "pos"),
			Neg		=> write!(f, "neg"),
		}
	}
}

impl Display for Sensor {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match &self.1 {
			Some(t)	=> write!(f, "{}: {}", self.0, t),
			None	=> write!(f, "{}", self.0),
		}
	}
}

impl Display for FnMacro {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use FnMacro::*;
		match self {
			AlwaysFF(sensors) => {
				let mut iter = sensors.iter();
				write!(f, "#[always_ff({}", iter.next().unwrap())?;
				for x in iter {
					write!(f, ", {}", x)?;
				}
				write!(f, ")]")
			},
			AlwaysComb => write!(f, "#[always_comb]"),
		}
	}
}

impl Display for Declaration {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}: {}", self.0, self.1)
	}
}

impl Display for Function {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		if self.fn_macro.is_some() {
			writeln!(f, "{}", self.fn_macro.as_ref().unwrap())?;
		}
		if self.is_pub {
			write!(f, "pub ")?;
		}
		write!(f, "fn {}(", self.ident)?;
		if self.params.len() != 0 {
			let mut iter = self.params.iter();
			write!(f, "{}", iter.next().unwrap())?;
			for x in iter {
				write!(f, ", {}", x)?;
			}
		}

		if let Some(rv) = &self.ret_val {
			write!(f, ") -> {} {}", rv, self.body)
		} else {
			write!(f, ") {}", self.body)
		}
	}
}

impl Display for Impl {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		writeln!(f, "impl {} {{", self.ident)?;
		for func in &self.functions {
			let s = format!("{}", func);
			for s in s.split('\n') {
				writeln!(f, "\t{}", s)?;
			}
		}
		write!(f, "}}")
	}
}

impl Display for Struct {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		if self.is_pub {
			writeln!(f, "pub ")?;
		}
		writeln!(f, "struct {} {{", self.ident)?;
		for decl in &self.declarations {
			let s = format!("{}", decl);
			for s in s.split('\n') {
				writeln!(f, "\t{}", s)?;
			}
		}
		write!(f, "}}")
	}
}

impl Display for Element {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use Element::*;
		match self {
			Impl(x)		=> write!(f, "{}", x),
			Struct(x)	=> write!(f, "{}", x),
		}
	}
}

impl Display for Program  {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		for elem in &self.0 {
			writeln!(f, "{}", elem)?;
		}
		Ok(())
	}
}
