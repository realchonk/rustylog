use super::ast::*;
use pest::{Parser, iterators::Pair};

#[derive(Parser)]
#[grammar = "frontend/rustylog.pest"]
pub struct RustylogParser;

pub trait Parse {
	const RULE: Rule;
	fn parse(pair: Pair<Rule>) -> Self;
}

pub fn parse<T: Parse>(code: &str) -> Option<T> {
	let ast = RustylogParser::parse(T::RULE, code).ok()?.next()?;
	Some(T::parse(ast))
}

impl Parse for Program {
	const RULE: Rule = Rule::Program;
	fn parse(pair: Pair<Rule>) -> Program {
		let mut p = pair.into_inner();
		let mut elements = Vec::new();
		while let Some(tk) = p.next() {
			match tk.as_rule() {
				Rule::Struct	=> elements.push(ProgramElement::Struct(Struct::parse(tk))),
				Rule::Impl		=> elements.push(ProgramElement::Impl(Impl::parse(tk))),
				Rule::EOI		=> break,
				r				=> unimplemented!("{:?}", r),
			}
		}

		Program { elements }
	}
}

impl Parse for Impl {
	const RULE: Rule = Rule::Impl;
	fn parse(pair: Pair<Rule>) -> Impl {
		let mut p = pair.into_inner();
		let name = p.next().unwrap().as_str().to_string();
		let mut functions = Vec::new();

		while let Some(tk) = p.next() {
			functions.push(Function::parse(tk));
		}

		Impl { name, functions }
	}
}

impl Parse for Struct {
	const RULE: Rule = Rule::Struct;
	fn parse(pair: Pair<Rule>) -> Struct {
		let mut p = pair.into_inner();
		let mut tk = p.next().unwrap();

		let visibility = if tk.as_rule() == Rule::Visibility {
			let x = Some(Visibility::from(tk.as_str()));
			tk = p.next().unwrap();
			x
		} else { None };

		let name = tk.as_str().to_string();
		let mut variables = Vec::new();
		while let Some(tk) = p.next() {
			let mut p = tk.into_inner();
			let mut tk = p.next().unwrap();
			let visibility = if tk.as_rule() == Rule::Visibility {
				let x = Some(Visibility::from(tk.as_str()));
				tk = p.next().unwrap();
				x
			} else { None };
			let name = tk.as_str().to_string();
			let typ = Type::parse(p.next().unwrap());
			variables.push(Variable { visibility, name, typ });
		}

		Struct { visibility, name, variables }
	}
}

impl Parse for Function {
	const RULE: Rule = Rule::Function;
	fn parse(pair: Pair<Rule>) -> Function {
		let mut p = pair.into_inner();
		let mut tk = p.next().unwrap();

		let fn_macro = if tk.as_rule() == Rule::FnMacro {
			let x = Some(FnMacro::parse(tk.into_inner().next().unwrap()));
			tk = p.next().unwrap();
			x
		} else { None };

		let visibility = if tk.as_rule() == Rule::Visibility {
			let x = Some(Visibility::from(tk.as_str()));
			tk = p.next().unwrap();
			x
		} else { None };

		let name = String::from(tk.as_str());

		let mut tk = p.next().unwrap();

		let self_ref = if tk.as_rule() == Rule::SelfRef {
			let is_mut = tk.into_inner().next().is_some();
			tk = p.next().unwrap();
			Some(SelfRef { mutable: is_mut })
		} else { None };

		let mut args = Vec::new();

		loop {
			match tk.as_rule() {
				Rule::Arg => {
					args.push(Arg::parse(tk));
					tk = p.next().unwrap();
				},
				_ => break,
			}
		}

		let ret_type = if tk.as_rule() == Rule::Type {
			let t =Type::parse(tk);
			tk = p.next().unwrap();
			Some(t)
		} else { None };

		let body = BlockStmt::parse(tk);

		Function {
			fn_macro,
			visibility,
			name,
			self_ref,
			args,
			ret_type,
			body,
		}
	}
}

impl Parse for Arg {
	const RULE: Rule = Rule::Arg;
	fn parse(pair: Pair<Rule>) -> Arg {
		let mut p = pair.into_inner();

		let name = p.next().unwrap().as_str().to_string();
		let typ = Type::parse(p.next().unwrap());

		Arg { name, typ }
	}
}

impl Parse for Type {
	const RULE: Rule = Rule::Type;
	fn parse(pair: Pair<Rule>) -> Type {
		let mut p = pair.into_inner();
		let tk = p.next().unwrap();
		if tk.as_rule() == Rule::IOType {
			let sub = SubType::parse(p.next().unwrap());
			match tk.as_str() {
				"Input"		=> Type::Input(sub),
				"Output"	=> Type::Output(sub),
				"InOut"		=> Type::InOut(sub),
				_			=> unimplemented!(),
			}
		} else {
			Type::Sub(SubType::parse(tk))
		}
	}
}

impl Parse for SubType {
	const RULE: Rule = Rule::SubType;
	fn parse(pair: Pair<Rule>) -> SubType {
		let mut p = pair.into_inner();
		let x = p.next().unwrap();
		match x.as_rule() {
			Rule::ArrayType	=> {
				let mut p = x.into_inner();
				let sub = SubType::parse(p.next().unwrap());
				let num = p.next().unwrap().as_str().parse().unwrap();
				SubType::Array(Box::new(sub), num)
			},
			Rule::LogicType => SubType::Logic,
			Rule::TriType => SubType::Tri,
			Rule::QName => panic!("{}", x.as_str()),
			r => unimplemented!("{:?}", r),
		}
	}
}

impl Parse for FnMacro {
	const RULE: Rule = Rule::FnMacro;
	fn parse(pair: Pair<Rule>) -> FnMacro {
		match pair.as_rule() {
			Rule::AlwaysComb	=> FnMacro::AlwaysComb,
			Rule::AlwaysFF		=> {
				let mut args = Vec::new();
				for arg in pair.into_inner() {
					let mut p = arg.into_inner();
					let name = p.next().unwrap().as_str().to_string();
					let trigger = match p.next() {
						Some(trig)	=> Some(Trigger::from(trig.as_str())),
						None		=> None,
					};
					args.push((name, trigger));
				}
				FnMacro::AlwaysFF(args)
			},
			r => unimplemented!("{:?}", r),
		}
	}
}


impl Parse for Statement {
	const RULE: Rule = Rule::Statement;
	fn parse(pair: Pair<Rule>) -> Statement {
		match pair.as_rule() {
			Rule::Statement => Statement::parse(pair.into_inner().next().unwrap()),
			Rule::AssignStmt => {
				let mut p = pair.into_inner();
				let left = VName::parse(p.next().unwrap());
				let op = AssignOp::from(p.next().unwrap().as_str());
				let right = Expression::parse(p.next().unwrap());

				Statement::Assign { left, op, right }
			},
			Rule::BlockStmt => Statement::Block(BlockStmt::parse(pair)),
			Rule::IfStmt => {
				let mut p = pair.into_inner();
				let cond = Expression::parse(p.next().unwrap());
				let true_case = BlockStmt::parse(p.next().unwrap());
				let false_case = match p.next() {
					Some(x)	=> Some(BlockStmt::parse(x)),
					None	=> None,
				};

				Statement::If { cond, true_case, false_case }
			},

			r => unimplemented!("{:?}", r),
		}
	}
}
impl Parse for BlockStmt {
	const RULE: Rule = Rule::BlockStmt;
	fn parse(pair: Pair<Rule>) -> BlockStmt {
		let mut p = pair.into_inner();
		let mut stmts = Vec::new();
		loop {
			match p.next() {
				Some(stmt)	=> stmts.push(Statement::parse(stmt)),
				None		=> break BlockStmt(stmts),
			}
		}
	}
}

impl Parse for Expression {
	const RULE: Rule = Rule::Expression;
	fn parse(pair: Pair<Rule>) -> Expression {
		match pair.as_rule() {
			Rule::Int => Expression::Int(pair.as_str().parse().unwrap()),
			Rule::NameExpr => Expression::VName(VName::parse(pair.into_inner().next().unwrap())),
			Rule::SubExpr => Expression::Sub(Box::new(Expression::parse(pair.into_inner().next().unwrap()))),
			Rule::AddSubExpr | Rule::MulDivExpr => {
				let mut p = pair.into_inner();
				let mut left = Expression::parse(p.next().unwrap());

				loop {
					let op = p.next();
					match op {
						Some(op) => {
							let right = Expression::parse(p.next().unwrap());
							left = Expression::Binary {
								lhs: Box::new(left),
								op: BinaryOp::from(op.as_str()),
								rhs: Box::new(right),
							};
						},
						None => break left,
					}
				}
			},
			Rule::UnaryExpr => {
				let mut p = pair.into_inner();
				let x = p.next().unwrap();
				match x.as_rule() {
					Rule::UnaryOp => {
						Expression::Unary {
							op: UnaryOp::from(x.as_str()),
							expr: Box::new(Expression::parse(p.next().unwrap())),
						}
					},
					Rule::IndexExpr => Expression::parse(x),
					r => unimplemented!("Invalid Rule '{:?}' for unary expression.", r),
				}
			},
			Rule::IndexExpr => {
				let mut p = pair.into_inner();
				let mut left = Expression::parse(p.next().unwrap());
				loop {
					let index = p.next();
					match index {
						Some(index) => {
							left = Expression::Index {
								base: Box::new(left),
								index: Index::parse(index),
							};
						},
						None => break left,
					}
				}
			},
			Rule::ConstructExpr => {
				let mut p = pair.into_inner();
				let name = QName::parse(p.next().unwrap());
				let mut entries = Vec::new();

				loop {
					match p.next() {
						Some(pair) => {
							let mut p = pair.into_inner();
							let name = p.next().unwrap().as_str().to_string();
							let expr = match p.next() {
								Some(expr)	=> Some(Expression::parse(expr)),
								None		=> None,
							};
							entries.push((name, expr));
						},
						None => break Expression::Constructor(name, entries),
					}
				}
			},
			Rule::CallExpr => {
				let mut p = pair.into_inner();
				let name = VName::parse(p.next().unwrap());
				let mut params = Vec::new();

				loop {
					match p.next() {
						Some(p) => params.push(Expression::parse(p)),
						None	=> break Expression::Call(name, params),
					}
				}

			},
			Rule::Expression => Expression::parse(pair.into_inner().next().unwrap()),
			r => unimplemented!("Unimplemented Rule '{:?}'", r),
		}
	}
}

impl Parse for Index {
	const RULE: Rule = Rule::Index;
	fn parse(pair: Pair<Rule>) -> Index {
		let mut p = pair.into_inner();
		let left = p.next().unwrap().as_str().parse().unwrap();
		match p.next() {
			Some(right)	=> Index::Range(left, right.as_str().parse().unwrap()),
			None		=> Index::Single(left),
		}
	}
}

impl Parse for VName {
	const RULE: Rule = Rule::VName;
	fn parse(pair: Pair<Rule>) -> VName {
		let is_self = pair.as_str().starts_with("self."); // FIXME
		let ident = pair.into_inner().as_str().to_string();
		VName { is_self, ident }
	}
}

impl Parse for QName {
	const RULE: Rule = Rule::QName;
	fn parse(pair: Pair<Rule>) -> QName {
		let mut p = pair.into_inner();
		let mut left = QName::Name(p.next().unwrap().as_str().to_string());

		loop {
			match p.next() {
				Some(right) => left = QName::Sub(Box::new(left), right.as_str().to_string()),
				None		=> break left,
			}
		}
	}
}
