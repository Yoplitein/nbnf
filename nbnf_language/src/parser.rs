use std::collections::{HashMap, HashSet};
use std::iter::Peekable;
use std::mem::discriminant;
use std::rc::Rc;

use anyhow::{bail, ensure, Result as AResult};

use crate::{Literal, Token};

#[derive(Clone, Debug)]
pub struct Grammar {
	pub top_rule: Rc<String>,
	pub rules: HashMap<Rc<String>, Rule>,
	pub rule_order: Vec<Rc<String>>,
}

#[derive(Clone, Debug)]
pub struct Rule {
	pub output_type: String,
	pub body: Expr,
}

#[derive(Clone, Debug)]
pub enum Expr {
	Literal(Literal),
	Rule(String),
	Group(Vec<Expr>),
	Alternate(Vec<Expr>),
	Repeat {
		expr: Box<Expr>,
		min: usize,
		max: Option<usize>,
	},
	Discard(Box<Expr>),
	Not(Box<Expr>),
	Cut(Box<Expr>),
	Recognize(Box<Expr>),
	Epsilon,
	Map {
		expr: Box<Expr>,
		func: MapFunc,
		mapping_code: String,
	},
}

#[derive(Clone, Debug)]
pub enum MapFunc {
	Value,
	Map,
	MapOpt,
	MapRes,
}

impl From<Token> for MapFunc {
	fn from(token: Token) -> Self {
		match token {
			Token::Value => Self::Value,
			Token::Map => Self::Map,
			Token::MapOpt => Self::MapOpt,
			Token::MapRes => Self::MapRes,
			_ => panic!("no MapFunc for {token:?}"),
		}
	}
}

pub fn parse(tokens: Vec<Token>) -> AResult<Grammar> {
	ensure!(!tokens.is_empty());
	let mut parser = Parser(tokens.into_iter().peekable());
	parser.parse()
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Modifier {
	Discard,
	Not,
	Cut,
	Recognize,
}

impl From<Token> for Modifier {
	fn from(token: Token) -> Self {
		match token {
			Token::Discard => Self::Discard,
			Token::Not => Self::Not,
			Token::Cut => Self::Cut,
			Token::Recognize => Self::Recognize,
			_ => panic!("no Modifier for {token:?}"),
		}
	}
}

struct Parser<Iter: Iterator>(Peekable<Iter>);

impl<Iter: Iterator<Item = Token> + ExactSizeIterator> Parser<Iter> {
	fn peek(&mut self) -> Option<&Token> {
		self.0.peek()
	}

	fn pop(&mut self) -> Option<Token> {
		self.0.next()
	}

	fn expect(&mut self, expected: Token) -> AResult<Token> {
		let Some(token) = self.pop() else {
			bail!("expecting {expected:?} but found eof")
		};
		ensure!(
			discriminant(&token) == discriminant(&expected),
			"expecting {expected:?} but found {token:?}",
		);
		Ok(token)
	}

	fn parse(&mut self) -> AResult<Grammar> {
		let mut rules = HashMap::new();
		let mut top_rule = None;
		let mut rule_order = vec![];
		while let Some(token) = self.pop() {
			let Token::Rule(rule_name) = token else {
				bail!("expected identifier to start rule definition but got {token:?}")
			};
			let rule_name = Rc::new(rule_name);
			if top_rule.is_none() {
				top_rule = Some(rule_name.clone());
			}

			let output_type = match self.peek() {
				Some(Token::RustSrc(_)) => {
					let Some(Token::RustSrc(ty)) = self.pop() else {
						unreachable!()
					};
					ty
				},
				_ => "&str".into(),
			};

			self.expect(Token::Equals)?;
			let body = self.parse_expr()?;
			self.expect(Token::Semicolon)?;
			ensure!(
				rules.insert(rule_name.clone(), Rule { output_type, body }).is_none(),
				"found duplicate rule {rule_name:?}"
			);
			rule_order.push(rule_name);
		}
		let top_rule = top_rule.unwrap();
		Ok(Grammar { top_rule, rules, rule_order })
	}

	fn parse_expr(&mut self) -> AResult<Expr> {
		let mut alts = vec![];
		let mut exprs = vec![];
		let mut pending_modifiers = HashSet::new();
		let mut last_len = usize::MAX;
		while self.peek().is_some() {
			let len = self.0.len();
			if len == last_len {
				panic!(
					"parse_rule_body stuck in infinite loop at token {:?}",
					self.0.peek()
				);
			}
			last_len = len;

			if let Ok(expr) = self.parse_operand() {
				exprs.push(expr);
				self.process_modifiers(&mut exprs, &mut pending_modifiers);
				continue;
			}

			let Some(token) = self.peek() else { break };
			match token {
				Token::GroupOpen => {
					let group = self.parse_group()?;
					exprs.push(group);
				},
				Token::Slash => {
					self.pop().unwrap_or_else(|| unreachable!());
					match exprs.len() {
						0 => bail!("found alternate with illegal leading slash"),
						1 => {
							let Some(expr) = exprs.pop() else {
								unreachable!()
							};
							alts.push(expr);
						},
						_ => {
							alts.push(Expr::Group(exprs));
							exprs = vec![];
						},
					}
				},
				Token::Repeat { .. } => {
					let Some(Token::Repeat { min, max }) = self.pop() else {
						unreachable!()
					};
					let Some(last) = exprs.pop() else {
						bail!("found repeat at start of rule body/group")
					};
					let last = last.into();
					exprs.push(Expr::Repeat {
						expr: last,
						min,
						max,
					})
				},
				Token::Discard | Token::Not | Token::Cut | Token::Recognize => {
					let token = self.pop().unwrap_or_else(|| unreachable!());
					pending_modifiers.insert(token.into());
					continue;
				},
				Token::Value | Token::Map | Token::MapOpt | Token::MapRes => {
					let Some(token) = self.pop() else {
						unreachable!()
					};
					let Token::RustSrc(mapping_code) =
						self.expect(Token::RustSrc(String::new()))?
					else {
						unreachable!()
					};
					let Some(expr) = exprs.pop() else {
						bail!("found mapping function without any expression to map")
					};
					let expr = Box::new(expr);
					let func = token.into();
					exprs.push(Expr::Map {
						func,
						mapping_code,
						expr,
					});
					continue;
				},
				Token::Semicolon | Token::GroupClose => {
					self.process_modifiers(&mut exprs, &mut pending_modifiers);
					break;
				},
				_ => bail!("got unexpected {token:?} when parsing rule body"),
			}

			self.process_modifiers(&mut exprs, &mut pending_modifiers);
		}

		ensure!(pending_modifiers.is_empty(), "unprocessed modifiers: {pending_modifiers:?}");
		Ok(if !alts.is_empty() {
			ensure!(
				!exprs.is_empty(),
				"found alternate with illegal trailing slash"
			);
			match exprs.len() {
				0 => bail!("found alternate with illegal trailing slash"),
				1 => {
					let Some(expr) = exprs.pop() else {
						unreachable!()
					};
					alts.push(expr);
				},
				_ => {
					alts.push(Expr::Group(exprs));
				},
			}
			Expr::Alternate(alts)
		} else if exprs.len() != 1 {
			Expr::Group(exprs)
		} else {
			let Some(rule) = exprs.into_iter().next() else {
				unreachable!()
			};
			rule
		})
	}

	fn process_modifiers(
		&mut self,
		exprs: &mut Vec<Expr>,
		pending_modifiers: &mut HashSet<Modifier>,
	) {
		if !pending_modifiers.is_empty() {
			let Some(mut expr) = exprs.pop() else {
				panic!("trying to process modifiers but no expression to pop")
			};

			if pending_modifiers.contains(&Modifier::Not) {
				expr = Expr::Not(expr.into());
				pending_modifiers.remove(&Modifier::Not);
			}
			if pending_modifiers.contains(&Modifier::Recognize) {
				expr = Expr::Recognize(expr.into());
				pending_modifiers.remove(&Modifier::Recognize);
			}
			if pending_modifiers.contains(&Modifier::Cut) {
				expr = Expr::Cut(expr.into());
				pending_modifiers.remove(&Modifier::Cut);
			}
			if pending_modifiers.contains(&Modifier::Discard) {
				expr = Expr::Discard(expr.into());
				pending_modifiers.remove(&Modifier::Discard);
			}
			assert!(pending_modifiers.is_empty(), "unimplemented modifiers: {pending_modifiers:?}");

			exprs.push(expr);
		}
	}

	fn parse_group(&mut self) -> AResult<Expr> {
		self.expect(Token::GroupOpen)?;
		let body = self.parse_expr()?;
		self.expect(Token::GroupClose)?;
		Ok(body)
	}

	fn parse_operand(&mut self) -> AResult<Expr> {
		let Some(token) = self.peek() else {
			bail!("trying to parse rule operand but got eof")
		};
		Ok(match token {
			Token::Rule(_) => {
				let Some(Token::Rule(rule_name)) = self.pop() else {
					unreachable!()
				};
				Expr::Rule(rule_name)
			},
			Token::Literal(_) => {
				let Some(Token::Literal(literal)) = self.pop() else {
					unreachable!()
				};
				Expr::Literal(literal)
			},
			Token::Epsilon => {
				self.pop().unwrap_or_else(|| unreachable!());
				Expr::Epsilon
			},
			_ => bail!("expecting rule expr but got {token:?}"),
		})
	}
}
