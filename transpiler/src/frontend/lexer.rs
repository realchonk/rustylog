use super::{SourceLocation,
			token::{Token, TokenData},
			input::Input};
use std::fmt::{Display, Formatter};
use std::str::Chars;
use std::iter::Peekable;

#[derive(Debug)]
pub enum LexicalError {
	InvalidInput(SourceLocation, char),
}

type LResult<T> = Result<T, LexicalError>;

#[derive(Debug)]
struct Operator {
	ch: char,
	terminal: bool,
	next: Vec<Operator>,
	s: &'static str,
}

#[derive(Debug)]
pub struct Lexer<'a>(LexerImpl<'a>, Option<Token>);

#[derive(Debug)]
struct LexerImpl<'a> {
	operators: Vec<Operator>,
	keywords: &'a [&'static str],
	input: Input<'a>,
}

impl<'a> Lexer<'a> {
	pub fn new<'b>(ops: &'b [&'static str], keywords: &'a [&'static str], input: Input<'a>) -> Self {
		Self(LexerImpl::new(ops, keywords, input), None)
	}

	pub fn peek(&mut self) -> LResult<&Token> {
		if !self.1.is_some() {
			self.1 = Some(self.0.lex()?);
		}

		Ok(self.1.as_ref().unwrap())
	}

	pub fn next(&mut self) -> LResult<Token> {
		if self.1.is_some() {
			Ok(self.1.take().unwrap())
		} else {
			self.0.lex()
		}
	}

	pub fn skip(&mut self) -> LResult<()> {
		if self.1.is_some() {
			self.1 = None;
		} else {
			self.0.lex()?;
		}
		Ok(())
	}
}

impl<'a> LexerImpl<'a> {
	fn add_op(ops: &mut Vec<Operator>, mut chars: Peekable<Chars>, orig: &'static str) {
		let ch = chars.next().unwrap();
		for e in ops.iter_mut() {
			if e.ch == ch {
				if chars.peek().is_some() {
					Self::add_op(&mut e.next, chars, orig);
				} else {
					e.terminal = true;
				}
				return;
			}
		}

		let mut new_op = Operator {
			ch,
			terminal: !chars.peek().is_some(),
			next: Vec::new(),
			s: orig,
		};

		if chars.peek().is_some() {
			Self::add_op(&mut new_op.next, chars, orig);
		}

		ops.push(new_op);
	}

	fn new<'b>(ops: &'b [&'static str], keywords: &'a [&'static str], input: Input<'a>) -> Self {
		let mut operators = Vec::new();

		for op in ops {
			Self::add_op(&mut operators, op.chars().peekable(), op);
		}

		Self {
			operators,
			keywords,
			input,
		}
	}

	fn lex(&mut self) -> LResult<Token> {
		let isname0 = | ch: char | ch.is_alphabetic() || ch == '_';
		let isname = | ch: char | ch.is_alphanumeric() || ch == '_';

		self.input.skip_while_ws();
		let begin = self.input.pos();
		let ch = self.input.peek();

		let ch = match ch {
			Some(x) => x,
			None	=> return Ok(Token { end: begin.clone(), begin, data: TokenData::EndOfFile }),
		};

		if isname0(ch) {
			let mut s = String::new();
			loop {
				match self.input.peek() {
					Some(ch) => {
						if isname(ch) {
							s.push(self.input.next().unwrap());
						} else {
							break;
						}
					},
					None => break,
				}
			}

			let end = self.input.pos();

			for kw in self.keywords {
				if *kw == s {
					return Ok(Token { begin, end, data: TokenData::Keyword(kw) });
				}
			}
			Ok(Token { begin, end, data: TokenData::Name(s) })
		} else if ch.is_digit(10) {
			let mut i_val = 0u128;

			loop {
				let ch = self.input.peek();
				if ch.is_none() || !ch.unwrap().is_digit(10) {
					break;
				}
				i_val = i_val * 10 + (self.input.next().unwrap().to_digit(10).unwrap() as u128);
			}


			let end = self.input.pos();

			Ok(Token { begin, end, data: TokenData::Integer(i_val) })
		} else {
			match Self::next_op(&mut self.input, &self.operators) {
				Some(op)	=> Ok(Token { end: begin.clone(), begin, data: TokenData::Operator(op.s) }),
				None		=> Err(LexicalError::InvalidInput(begin, ch)),
			}
		}
	}
	fn next_op<'b>(input: &'b mut Input, ops: &'b Vec<Operator>) -> Option<&'b Operator> {
		let ch = input.peek()?;
		for op in ops {
			if ch == op.ch {
				input.next().unwrap();
				let x = Self::next_op(input, &op.next);

				return if x.is_some() { x } else if op.terminal { Some(&op) } else { None };
			}
		}
		None
	}
}


impl Display for LexicalError {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use LexicalError::*;
		match self {
			InvalidInput(pos, ch)	=> write!(f, "{}: invalid input: '{}'", pos, ch),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	#[test]
	fn test_lexer() {
		let operators = [ "+", "{" ];
		let keywords = [ "mut", "struct" ];

		let code = "3 + 2 mut struct {";
		let input = Input::new(code, "<source>".to_string());
		let mut lexer = Lexer::new(&operators, &keywords, input);

		use TokenData::*;
		assert_eq!(lexer.next().unwrap().data, Integer(3));
		assert_eq!(lexer.next().unwrap().data, Operator("+"));
		assert_eq!(lexer.next().unwrap().data, Integer(2));
		assert_eq!(lexer.next().unwrap().data, Keyword("mut"));
		assert_eq!(lexer.next().unwrap().data, Keyword("struct"));
		assert_eq!(lexer.next().unwrap().data, Operator("{"));

	}
}
