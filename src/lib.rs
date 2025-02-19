#![allow(unused)]

pub mod lexer;
pub mod parser;

use std::collections::HashMap;
use std::ops::RangeInclusive;

pub use crate::lexer::Token;
pub use crate::parser::{Grammar, Rule};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
	Char(char),
	String(String),
	Range {
		chars: Vec<char>,
		ranges: Vec<RangeInclusive<char>>,
	},
}

pub fn parse_grammar(input: &str) -> anyhow::Result<Grammar> {
	let tokens = lexer::lex(input)?;
	parser::parse(tokens)
}
