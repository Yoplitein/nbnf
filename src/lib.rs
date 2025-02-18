#![allow(unused)]

pub mod lexer;
pub mod parser;

use std::collections::HashMap;

pub use crate::{lexer::Token, parser::{Grammar, Rule}};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
	Char(char),
	String(String),
}

pub fn parse_grammar(input: &str) -> anyhow::Result<Grammar> {
	let tokens = lexer::lex(input)?;
	parser::parse(tokens)
}
