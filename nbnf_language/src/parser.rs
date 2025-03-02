use std::collections::{HashMap, HashSet};
use std::iter::Peekable;
use std::mem::discriminant;

use anyhow::{bail, ensure, Result as AResult};

use crate::{Literal, Token};

#[derive(Clone, Debug)]
pub struct Grammar {
	pub top_rule: String,
	pub rules: HashMap<String, Rule>,
}

#[derive(Clone, Debug)]
pub enum Rule {
	Literal(Literal),
	Rule(String),
	Group(Vec<Rule>),
	Alternate(Vec<Rule>),
	Repeat {
		rule: Box<Rule>,
		min: usize,
		max: Option<usize>,
	},
	Not(Box<Rule>),
	Recognize(Box<Rule>),
	Epsilon,
}

pub fn parse(tokens: Vec<Token>) -> AResult<Grammar> {
	ensure!(!tokens.is_empty());
	let mut parser = Parser(tokens.into_iter().peekable());
	parser.parse()
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Modifier {
	Not,
	Recognize,
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
		while let Some(token) = self.pop() {
			let Token::Identifier(rule_name) = token else {
				bail!("expected identifier to start rule definition but got {token:?}")
			};
			if top_rule.is_none() {
				top_rule = Some(rule_name.clone());
			}

			self.expect(Token::Equals)?;
			let rule_body = self.parse_rule_body()?;
			self.expect(Token::Semicolon)?;
			rules.insert(rule_name, rule_body);
		}
		let top_rule = top_rule.unwrap();
		Ok(Grammar { top_rule, rules })
	}

	fn parse_rule_body(&mut self) -> AResult<Rule> {
		let mut alts = vec![];
		let mut atoms = vec![];
		let mut pending_modifiers = HashSet::new();
		let mut last_len = usize::MAX;
		while self.peek().is_some() {
			let len = self.0.len();
			if len == last_len {
				panic!("parse_rule_body stuck in infinite loop at token {:?}", self.0.peek());
			}
			last_len = len;
			
			match self.parse_operand() {
				Ok(atom) => {
					atoms.push(atom);
					self.process_modifiers(&mut atoms, &mut pending_modifiers);
					continue;
				},
				Err(_) => {},
			}

			let Some(token) = self.peek() else { break };
			match token {
				Token::GroupOpen => {
					let group = self.parse_group()?;
					atoms.push(group);
				},
				Token::Slash => {
					self.pop().unwrap_or_else(|| unreachable!());
					match atoms.len() {
						0 => bail!("found alternate with illegal leading slash"),
						1 => {
							let Some(atom) = atoms.pop() else {
								unreachable!()
							};
							alts.push(atom);
						},
						_ => {
							alts.push(Rule::Group(atoms));
							atoms = vec![];
						},
					}
				},
				Token::Repeat { .. } => {
					let Some(Token::Repeat { min, max }) = self.pop() else {
						unreachable!()
					};
					let Some(last) = atoms.pop() else {
						bail!("found repeat at start of rule body/group")
					};
					let last = last.into();
					atoms.push(Rule::Repeat {
						rule: last,
						min,
						max,
					})
				},
				Token::Not => {
					self.pop().unwrap_or_else(|| unreachable!());
					pending_modifiers.insert(Modifier::Not);
					continue;
				},
				Token::Recognize => {
					self.pop().unwrap_or_else(|| unreachable!());
					pending_modifiers.insert(Modifier::Recognize);
					continue;
				},
				Token::Semicolon | Token::GroupClose => {
					break;
				},
				_ => bail!("got unexpected {token:?} when parsing rule body"),
			}
			
			self.process_modifiers(&mut atoms, &mut pending_modifiers);
		}
		Ok(if !alts.is_empty() {
			ensure!(
				!atoms.is_empty(),
				"found alternate with illegal trailing slash"
			);
			match atoms.len() {
				0 => bail!("found alternate with illegal trailing slash"),
				1 => {
					let Some(atom) = atoms.pop() else {
						unreachable!()
					};
					alts.push(atom);
				},
				_ => {
					alts.push(Rule::Group(atoms));
				},
			}
			Rule::Alternate(alts)
		} else {
			if atoms.len() != 1 {
				Rule::Group(atoms)
			} else {
				let Some(rule) = atoms.into_iter().next() else {
					unreachable!()
				};
				rule
			}
		})
	}
	
	fn process_modifiers(&mut self, atoms: &mut Vec<Rule>, pending_modifiers: &mut HashSet<Modifier>) {
		let Some(next) = self.peek() else { return };
		if Self::triggers_modifiers(next) && !pending_modifiers.is_empty() {
			let Some(mut atom) = atoms.pop() else {
				panic!("trying to process modifiers but no atom to pop")
			};
			
			if pending_modifiers.contains(&Modifier::Not) {
				atom = Rule::Not(atom.into());
				pending_modifiers.remove(&Modifier::Not);
			}
			if pending_modifiers.contains(&Modifier::Recognize) {
				atom = Rule::Recognize(atom.into());
				pending_modifiers.remove(&Modifier::Recognize);
			}
			
			atoms.push(atom);
		}
	}

	fn parse_group(&mut self) -> AResult<Rule> {
		self.expect(Token::GroupOpen)?;
		let body = self.parse_rule_body()?;
		self.expect(Token::GroupClose)?;
		Ok(body)
	}

	fn parse_operand(&mut self) -> AResult<Rule> {
		let Some(token) = self.peek() else {
			bail!("trying to parse rule operand but got eof")
		};
		Ok(match token {
			Token::Identifier(_) => {
				let Some(Token::Identifier(rule_name)) = self.pop() else {
					unreachable!()
				};
				Rule::Rule(rule_name)
			},
			Token::Literal(_) => {
				let Some(Token::Literal(literal)) = self.pop() else {
					unreachable!()
				};
				Rule::Literal(literal)
			},
			Token::Epsilon => {
				let Some(_) = self.pop() else {
					unreachable!()
				};
				Rule::Epsilon
			}
			_ => bail!("expecting rule atom but got {token:?}"),
		})
	}
	
	fn triggers_modifiers(token: &Token) -> bool {
		match token {
			Token::Identifier(_) |
			Token::Literal(_) |
			Token::Slash |
			Token::Semicolon |
			Token::GroupOpen |
			Token::Epsilon => true,
			_ => false,
		}
	}
}
