//!
#![forbid(missing_docs)]

///
pub mod generator;
///
pub mod lexer;
///
pub mod parser;

use std::collections::HashSet;
use std::ops::RangeInclusive;

pub use crate::generator::{generate_parser, generate_parser_tokens};
pub use crate::lexer::{Token, lex};
pub use crate::parser::{Expr, Grammar, parse};

/// A literal character, string, or character range to be matched.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
	/// A character.
	Char(char),
	/// A string.
	String(String),
	/// A range of characters.
	Range {
		/// Literal characters
		chars: HashSet<char>,
		/// Literal ranges
		ranges: HashSet<RangeInclusive<char>>,
		/// Whether to match any character except those in the range
		invert: bool,
	},
}

/// Shortcut to parse a grammar directly from a string.
pub fn parse_grammar(input: &str) -> anyhow::Result<Grammar> {
	let tokens = lexer::lex(input)?;
	parser::parse(tokens)
}
