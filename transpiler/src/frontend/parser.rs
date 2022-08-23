use super::ast::*;
use pest::{Parser, iterators::Pair};

#[derive(Parser)]
#[grammar = "frontend/rustylog.pest"]
pub struct RustylogParser;

pub fn parse_expr(code: &str) -> Expression {
	let ast = RustylogParser::parse(Rule::Expression, code).unwrap().next().unwrap();
	build_expr_from_pair(ast)
}

pub fn parse_stmt(code: &str) -> Statement {
	let ast = RustylogParser::parse(Rule::Statement, code).unwrap().next().unwrap();
	build_stmt_from_pair(ast)
}


fn build_stmt_from_pair(pair: Pair<Rule>) -> Statement {
	match pair.as_rule() {
		Rule::Statement => build_stmt_from_pair(pair.into_inner().next().unwrap()),
		Rule::AssignStmt => {
			let mut p = pair.into_inner();
			let left = build_vname_from_pair(p.next().unwrap());
			let op = AssignOp::from(p.next().unwrap().as_str());
			let right = build_expr_from_pair(p.next().unwrap());

			Statement::Assign { left, op, right }
		},
		Rule::BlockStmt => Statement::Block(build_block_stmt_from_pair(pair)),
		Rule::IfStmt => {
			let mut p = pair.into_inner();
			let cond = build_expr_from_pair(p.next().unwrap());
			let true_case = build_block_stmt_from_pair(p.next().unwrap());
			let false_case = match p.next() {
				Some(x)	=> Some(build_block_stmt_from_pair(x)),
				None	=> None,
			};

			Statement::If { cond, true_case, false_case }
		},

		r => unimplemented!("{:?}", r),
	}
}
fn build_block_stmt_from_pair(pair: Pair<Rule>) -> BlockStmt {
	let mut p = pair.into_inner();
	let mut stmts = Vec::new();
	loop {
		match p.next() {
			Some(stmt)	=> stmts.push(build_stmt_from_pair(stmt)),
			None		=> break BlockStmt(stmts),
		}
	}
}

fn build_expr_from_pair(pair: Pair<Rule>) -> Expression {
	match pair.as_rule() {
		Rule::Int => Expression::Int(pair.as_str().parse().unwrap()),
		Rule::NameExpr => Expression::VName(build_vname_from_pair(pair.into_inner().next().unwrap())),
		Rule::SubExpr => Expression::Sub(Box::new(build_expr_from_pair(pair.into_inner().next().unwrap()))),
		Rule::AddSubExpr | Rule::MulDivExpr => {
			let mut p = pair.into_inner();
			let mut left = build_expr_from_pair(p.next().unwrap());

			loop {
				let op = p.next();
				match op {
					Some(op) => {
						let right = build_expr_from_pair(p.next().unwrap());
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
						expr: Box::new(build_expr_from_pair(p.next().unwrap())),
					}
				},
				Rule::IndexExpr => build_expr_from_pair(x),
				r => unimplemented!("Invalid Rule '{:?}' for unary expression.", r),
			}
		},
		Rule::IndexExpr => {
			let mut p = pair.into_inner();
			let mut left = build_expr_from_pair(p.next().unwrap());
			loop {
				let index = p.next();
				match index {
					Some(index) => {
						left = Expression::Index {
							base: Box::new(left),
							index: build_index_from_pair(index),
						};
					},
					None => break left,
				}
			}
		},
		Rule::ConstructExpr => {
			let mut p = pair.into_inner();
			let name = build_qname_from_pair(p.next().unwrap());
			let mut entries = Vec::new();

			loop {
				match p.next() {
					Some(pair) => {
						let mut p = pair.into_inner();
						let name = p.next().unwrap().as_str().to_string();
						let expr = match p.next() {
							Some(expr)	=> Some(build_expr_from_pair(expr)),
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
			let name = build_vname_from_pair(p.next().unwrap());
			let mut params = Vec::new();

			loop {
				match p.next() {
					Some(p) => params.push(build_expr_from_pair(p)),
					None	=> break Expression::Call(name, params),
				}
			}

		},
		Rule::Expression => build_expr_from_pair(pair.into_inner().next().unwrap()),
		r => unimplemented!("Unimplemented Rule '{:?}'", r),
	}
}

fn build_index_from_pair(pair: Pair<Rule>) -> Index {
	let mut p = pair.into_inner();
	let left = p.next().unwrap().as_str().parse().unwrap();
	match p.next() {
		Some(right)	=> Index::Range(left, right.as_str().parse().unwrap()),
		None		=> Index::Single(left),
	}
}

fn build_vname_from_pair(pair: Pair<Rule>) -> VName {
	let is_self = pair.as_str().starts_with("self."); // FIXME
	let ident = pair.into_inner().as_str().to_string();
	VName { is_self, ident }
}

fn build_qname_from_pair(pair: Pair<Rule>) -> QName {
	let mut p = pair.into_inner();
	let mut left = QName::Name(p.next().unwrap().as_str().to_string());

	loop {
		match p.next() {
			Some(right) => left = QName::Sub(Box::new(left), right.as_str().to_string()),
			None		=> break left,
		}
	}
}
