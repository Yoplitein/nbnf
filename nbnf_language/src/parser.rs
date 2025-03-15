use std::collections::{HashMap, HashSet};
use std::iter::Peekable;
use std::mem::discriminant;
use std::rc::Rc;
use std::str::FromStr;

use anyhow::{Result as AResult, anyhow, bail, ensure};
use proc_macro2::TokenStream;
use quote::quote;

use crate::{Literal, Token};

/// A parsed grammar.
#[derive(Clone, Debug)]
pub struct Grammar {
	/**
		The topmost (first) rule in the grammar.

		Conventionally, this is the root rule acting as the grammar's entrypoint.
	*/
	pub top_rule: Rc<String>,

	/// The rules in this grammar.
	pub rules: HashMap<Rc<String>, Rule>,

	/**
		Rule names in the order as defined in the grammar.

		This is used by the generator to emit parser functions in the same order as given.
	*/
	pub rule_order: Vec<Rc<String>>,
}

/// A rule.
#[derive(Clone, Debug)]
pub struct Rule {
	/// Rust code denoting the input type expected by this rule.
	pub input_type: TokenStream,
	/// Rust code denoting the type of this rule's output.
	pub output_type: TokenStream,
	/// Rust code denoting the (uninstantiated) error type of this rule's output.
	pub error_type: Option<TokenStream>,
	/// The root expression of this rule.
	pub body: Expr,
}

/// A grammar expression.
#[derive(Clone, Debug)]
pub enum Expr {
	/// Match a literal.
	Literal(Literal),
	/**
		An identifier naming another rule; or arbitrary Rust code evaluating to a parser.

		Arbitrary Rust can be used to reference e.g. custom literals, parametric parsers such as tag/take/etc.
	*/
	Rule(TokenStream),
	/// Match a group of expressions.
	Group(Vec<Expr>),
	/**
		Match one of a group of expressions.

		Wraps the inner expression with [alt][nom::branch::alt].
	*/
	Alternate(Vec<Expr>),
	/**
		Match an expression repeatedly.

		Wraps the inner expression with one of [many0][nom::multi::many0], [many1][nom::multi::many1], [many_m_n][nom::multi::many_m_n].
	*/
	Repeat {
		/// The inner expression to match.
		expr: Box<Expr>,
		/// The minimum number of times to repeat.
		min: usize,
		/// The maximum number of times to repeat, if any.
		max: Option<usize>,
	},
	/**
		Match an expression but discard its output from the parent expression's output.

		Wraps the inner expression with a generated map function like
		```
		map(
			inner,
			|(_, p1, _, p2)| (p1, p2),
		)
		```
	*/
	Discard(Box<Expr>),
	/**
		Match only if the inner expression does not.

		Wraps the inner expression with [not][nom::combinator::not].
	*/
	Not(Box<Expr>),
	/**
		Match an expression that should fail irrecoverably, preventing backtracking.

		Wraps the inner expression with [cut][nom::combinator::cut].
	*/
	Cut(Box<Expr>),
	/**
		Match an expression but replace its output with the string of input it matched.

		Wraps the inner expression with [recognize][nom::combinator::recognize].
	*/
	Recognize(Box<Expr>),
	/**
		Match the empty string.

		A shortcut for [success][nom::combinator::success].
	*/
	Epsilon,
	/// Wrap the inner expession in arbitrary Rust code.
	Wrap {
		/// The expression to wrap.
		expr: Box<Expr>,
		/// The code wrapping the expression.
		wrapper: TokenStream,
	},
}

/// Parse a list of tokens into a grammar.
pub fn parse(tokens: Vec<Token>) -> AResult<Grammar> {
	ensure!(!tokens.is_empty());
	let mut parser = Parser::new(tokens.into_iter());
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

struct Parser<Iter: Iterator> {
	iter: Peekable<Iter>,
	default_input_type: Option<TokenStream>,
	default_output_type: Option<TokenStream>,
	error_type: Option<TokenStream>,
}

impl<Iter: Iterator<Item = Token> + ExactSizeIterator> Parser<Iter> {
	fn new(iter: Iter) -> Self {
		let iter = iter.peekable();
		Self {
			iter,
			default_input_type: None,
			default_output_type: None,
			error_type: None,
		}
	}

	fn peek(&mut self) -> Option<&Token> {
		self.iter.peek()
	}

	fn pop(&mut self) -> Option<Token> {
		self.iter.next()
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
			if let Token::Pragma { name, args } = token {
				self.handle_pragma(name, args)?;
				continue;
			}

			let Token::Rule(rule_name) = token else {
				bail!("expected identifier to start rule definition but got {token:?}")
			};
			let rule_name = Rc::new(rule_name);
			if top_rule.is_none() {
				top_rule = Some(rule_name.clone());
			}

			let first_type = match self.peek() {
				Some(Token::RustSrc(_)) => {
					let Some(Token::RustSrc(ty)) = self.pop() else {
						unreachable!()
					};
					let ty = lex_rust_tokens(&ty, || {
						format!("couldn't lex rule `{rule_name}`'s type `{ty}`")
					})?;
					Some(ty)
				},
				_ => None,
			};
			let second_type = match self.peek() {
				Some(Token::RustSrc(_)) => {
					let Some(Token::RustSrc(ty)) = self.pop() else {
						unreachable!()
					};
					let ty = lex_rust_tokens(&ty, || {
						format!("couldn't lex rule `{rule_name}`'s type `{ty}`")
					})?;
					Some(ty)
				},
				_ => None,
			};
			let (input_type, output_type) = match (first_type, second_type) {
				(None, None) => (
					self.default_input_type
						.clone()
						.unwrap_or_else(|| quote! { &str }),
					self.default_output_type
						.clone()
						.unwrap_or_else(|| quote! { &str }),
				),
				(Some(out), None) => (
					self.default_input_type
						.clone()
						.unwrap_or_else(|| quote! { &str }),
					out,
				),
				(Some(inp), Some(out)) => (inp, out),
				(None, Some(_)) => {
					unreachable!()
				},
			};
			let error_type = self.error_type.clone();

			self.expect(Token::Equals)?;
			let body = self.parse_expr()?;
			self.expect(Token::Semicolon)?;
			ensure!(
				rules
					.insert(
						rule_name.clone(),
						Rule {
							input_type,
							output_type,
							error_type,
							body
						}
					)
					.is_none(),
				"found duplicate rule {rule_name:?}"
			);
			rule_order.push(rule_name);
		}
		let top_rule = top_rule.unwrap();
		Ok(Grammar {
			top_rule,
			rules,
			rule_order,
		})
	}

	fn parse_expr(&mut self) -> AResult<Expr> {
		let mut alts = vec![];
		let mut exprs = vec![];
		let mut pending_modifiers = HashSet::new();
		let mut last_len = usize::MAX;
		while self.peek().is_some() {
			let len = self.iter.len();
			if len == last_len {
				panic!(
					"parse_rule_body stuck in infinite loop at token {:?}",
					self.iter.peek()
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
				Token::Value | Token::Map | Token::MapOpt | Token::MapRes | Token::Wrap => {
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
					let wrapper = lex_rust_tokens(&mapping_code, || {
						format!("couldn't lex mapping code `{mapping_code}`")
					})?;
					let wrapper = match token {
						Token::Value => quote! {
							$nom::combinator::value(#wrapper, $expr)
						},
						Token::Map => quote! {
							$nom::combinator::map($expr, #wrapper)
						},
						Token::MapOpt => quote! {
							$nom::combinator::map_opt($expr, #wrapper)
						},
						Token::MapRes => quote! {
							$nom::combinator::map_res($expr, #wrapper)
						},
						Token::Wrap => wrapper,
						_ => unreachable!(),
					};
					exprs.push(Expr::Wrap { expr, wrapper });
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

		ensure!(
			pending_modifiers.is_empty(),
			"unprocessed modifiers: {pending_modifiers:?}"
		);
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
			assert!(
				pending_modifiers.is_empty(),
				"unimplemented modifiers: {pending_modifiers:?}"
			);

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
				let Some(Token::Rule(rule_expr)) = self.pop() else {
					unreachable!()
				};
				let rule_expr = lex_rust_tokens(&rule_expr, || {
					format!("couldn't lex rule operand `{rule_expr}`")
				})?;
				Expr::Rule(rule_expr)
			},
			Token::RustSrc(_) => {
				let Some(Token::RustSrc(rule_expr)) = self.pop() else {
					unreachable!()
				};
				let rule_expr = lex_rust_tokens(&rule_expr, || {
					format!("couldn't lex raw rule operand `{rule_expr}`")
				})?;
				Expr::Rule(rule_expr)
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

	fn handle_pragma(&mut self, name: String, args: Vec<String>) -> AResult<()> {
		match name.as_str() {
			"input" | "output" | "error" => {
				let [ty] = args.as_slice() else {
					bail!("pragma args mismatch, expected `#{name} <ty>`")
				};
				let ty = lex_rust_tokens(ty, || format!("parsing argument to pragma `#{name}`"))?;
				if name == "input" {
					self.default_input_type = Some(ty);
				} else if name == "output" {
					self.default_output_type = Some(ty);
				} else {
					self.error_type = Some(ty);
				}
			},
			_ => bail!("unknown pragma `#{name}`"),
		}
		Ok(())
	}
}

fn lex_rust_tokens(code: &str, context: impl FnOnce() -> String) -> AResult<TokenStream> {
	TokenStream::from_str(code).map_err(|err| {
		let context = context();
		anyhow!("{context}: {err:?}")
	})
}
