pub mod generator;
pub mod lexer;
pub mod parser;

use std::collections::HashSet;
use std::ops::RangeInclusive;

pub use crate::generator::{generate_parser, generate_parser_tokens};
pub use crate::lexer::{lex, Token};
pub use crate::parser::{parse, Expr, Grammar};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
	Char(char),
	String(String),
	Range {
		chars: HashSet<char>,
		ranges: HashSet<RangeInclusive<char>>,
		invert: bool,
	},
}

pub fn parse_grammar(input: &str) -> anyhow::Result<Grammar> {
	let tokens = lexer::lex(input)?;
	parser::parse(tokens)
}
