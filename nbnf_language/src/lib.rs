//!
#![forbid(missing_docs)]

///
pub mod generator;
///
pub mod lexer;
///
pub mod parser;

use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::RangeInclusive;

pub use crate::generator::{generate_parser, generate_parser_tokens};
pub use crate::lexer::{Token, lex};
pub use crate::parser::{Expr, Grammar, parse};

/**
	A literal scalar, string, or character range to be matched.
*/
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GLiteral<Char, Str>
where
	Char: Clone + Copy + Debug + PartialEq + Eq + Hash,
{
	/// A character.
	Char(Char),
	/// A string.
	String(Str),
	/// A range of characters.
	Range {
		/// Literal characters
		chars: HashSet<Char>,
		/// Literal ranges
		ranges: HashSet<RangeInclusive<Char>>,
		/// Whether to match any character except those in the range
		invert: bool,
	},
}

/// A string or byte literal.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
	/// A string-type literal.
	Char(GLiteral<char, String>),
	/// A byte-type literal.
	Byte(GLiteral<u8, Vec<u8>>),
}

impl From<GLiteral<char, String>> for Literal {
	fn from(literal: GLiteral<char, String>) -> Self {
		Literal::Char(literal)
	}
}

impl From<GLiteral<u8, Vec<u8>>> for Literal {
	fn from(literal: GLiteral<u8, Vec<u8>>) -> Self {
		Literal::Byte(literal)
	}
}

/// Shortcut to parse a grammar directly from a string.
pub fn parse_grammar(input: &str) -> anyhow::Result<Grammar> {
	let tokens = lexer::lex(input)?;
	parser::parse(tokens)
}
