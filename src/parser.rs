use std::collections::HashMap;

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
}

pub fn parse(tokens: Vec<Token>) -> anyhow::Result<Grammar> {
	todo!()
}
