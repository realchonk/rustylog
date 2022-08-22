use super::SourceLocation;
use std::{rc::Rc, str::Chars};

#[derive(Debug)]
pub struct Input<'a> {
	chars: Chars<'a>,
	pos: SourceLocation,
}

impl<'a> Input<'a> {
	pub fn new(s: &'a str, file: String) -> Self {
		Self {
			chars: s.chars(),
			pos: SourceLocation { file: Rc::new(file), line: 1, col: 1 },
		}
	}

	pub fn pos(&self) -> SourceLocation { self.pos.clone() }
	pub fn peek(&self) -> Option<char> { self.chars.clone().next() }
	pub fn next(&mut self) -> Option<char> {
		let ch = self.chars.next()?;
		if ch == '\n' {
			self.pos.line += 1;
			self.pos.col = 1;
		} else {
			self.pos.col += 1;
		}

		Some(ch)
	}
	pub fn skip_while(&mut self, f: impl Fn(char) -> bool) -> bool {
		let mut skipped = false;
		loop {
			let ch = self.peek();
			if ch.is_none() || !f(ch.unwrap()) {
				break skipped;
			}

			self.next().unwrap();
			skipped = true;
		}
	}

	pub fn skip_while_ws(&mut self) -> bool { self.skip_while(|ch| ch.is_whitespace()) }
	pub fn skip_eq(&mut self, ch: char) -> bool {
		match self.peek() {
			Some(x)	=> if x == ch { self.next().unwrap(); true } else { false },
			None	=> false,
		}
	}

}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_input() {
		let code = "3 + 2";
		let mut input = Input::new(code, "<source>".to_string());

		let mut s = String::new();
		loop {
			input.skip_while_ws();

			match input.next() {
				Some(ch)	=> s.push(ch),
				None		=> break,
			}
		}

		assert_eq!(s, "3+2");
	}
}
