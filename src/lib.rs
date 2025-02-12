#![allow(unused)]

use std::collections::HashMap;

use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while1};
use nom::combinator::{eof, map, opt, recognize};
use nom::multi::{many0_count, many1};
use nom::sequence::terminated;
use nom::{Finish, Parser};

#[derive(Clone, Debug)]
pub struct Grammar {
	pub top_rule: String,
	pub rules: HashMap<String, Rule>,
}

#[derive(Clone, Debug)]
pub enum Rule {
	Literal(String),
	Rule(String),
	Group(Vec<Rule>),
	Alternate(Vec<Rule>),
	Repeat {
		rule: Box<Rule>,
		min: usize,
		max: Option<usize>,
	},
}

pub fn parse_grammar(input: &str) -> anyhow::Result<Grammar> {
	let res = top.parse(input).finish();
	let res = res.map_err(|err| anyhow::anyhow!("{err:#?}"));
	let (_, res) = res?;
	Ok(res)
}

type PResult<'a, T> = nom::IResult<&'a str, T, nom_language::error::VerboseError<&'a str>>;

fn top(input: &str) -> PResult<Grammar> {
	let (input, (rules, ..)) = (many1(rule), whitespace(true), eof).parse(input)?;
	let top_rule = rules
		.first()
		.map(|(name, _)| name.clone())
		.unwrap_or_else(|| unreachable!());
	let rules = HashMap::from_iter(rules.into_iter());
	let grammar = Grammar { top_rule, rules };
	Ok((input, grammar))
}

fn rule(input: &str) -> PResult<(String, Rule)> {
	let (input, _) = whitespace(true).parse(input)?;
	let (input, rule_name) = identifier.parse(input)?;

	let (input, _) = opt(whitespace(false)).parse(input)?;
	let (input, _) = tag("=").parse(input)?;
	let (input, _) = opt(whitespace(false)).parse(input)?;

	let (input, rule) = rule_body.parse(input)?;

	let (input, _) = opt(whitespace(false)).parse(input)?;
	let (input, _) = rule_end.parse(input)?;

	Ok((input, (rule_name, rule)))
}

fn rule_end(input: &str) -> PResult<()> {
	map(alt((tag("\n"), eof)), |_| ()).parse(input)
}

fn rule_body(input: &str) -> PResult<Rule> {
	// TODO
	let (input, rule_name) = identifier.parse(input)?;
	Ok((input, Rule::Rule(rule_name)))
}

fn identifier(input: &str) -> PResult<String> {
	fn is_ident_char(start: bool, char: char) -> bool {
		match char {
			'_' | '-' => true,
			_ if char.is_numeric() => !start,
			_ => char.is_alphanumeric(),
		}
	}

	map(
		recognize((
			take_while1(|c| is_ident_char(true, c)),
			take_while(|c| is_ident_char(false, c)),
		)),
		str::to_string,
	)
	.parse(input)
}

fn whitespace(rule_start: bool) -> impl for<'a> Fn(&'a str) -> PResult<()> {
	if rule_start {
		fn parser(input: &str) -> PResult<()> {
			map(take_while(char::is_whitespace), |_| ()).parse(input)
		}
		parser
	} else {
		fn parser(input: &str) -> PResult<()> {
			map(
				take_while1(|c: char| match c {
					'\n' => false,
					_ => c.is_whitespace(),
				}),
				|_| (),
			)
			.parse(input)
		}
		parser
	}
}
