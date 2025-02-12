#![allow(unused)]

use std::collections::HashMap;

use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_while, take_while1};
use nom::combinator::{eof, map, map_res, not, opt, recognize, verify};
use nom::error::{ErrorKind, FromExternalError};
use nom::multi::{many0_count, many1, many1_count};
use nom::sequence::terminated;
use nom::{Finish, Parser};
use nom_language::error::VerboseError;

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

#[derive(Clone, Debug)]
pub enum Literal {
	Char(char),
	String(String),
	// TODO: HexChar/HexString for byte parsers
}

pub fn parse_grammar(input: &str) -> anyhow::Result<Grammar> {
	let res = top.parse(input).finish();
	let res = res.map_err(|err| anyhow::anyhow!("{err:#?}"));
	let (_, res) = res?;
	Ok(res)
}

type PResult<'a, T> = nom::IResult<&'a str, T, VerboseError<&'a str>>;

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
	map(
		many1((
			alt((
				map(identifier, Rule::Rule),
				map(literal, Rule::Literal),
			)),
			alt((whitespace(false), rule_end)),
		)),
		|mut xs| {
			if xs.len() == 1 {
				xs.pop().unwrap_or_else(|| unreachable!()).0
			} else {
				Rule::Group(xs.into_iter().map(|v| v.0).collect())
			}
		},
	)
	.parse(input)
}

fn literal(input: &str) -> PResult<Literal> {
	/* fn inner(char: bool) -> impl for<'a> Fn(&'a str) -> PResult<&'a str> {
		let quote = if char {
			"'"
		} else {
			"\""
		};
		move |input: &str| -> PResult<&str> {
			let body = recognize(many1_count(alt((
				dbg_dmp(map(recognize(not(tag(quote))), |v| dbg!(v)), "1"),
				dbg_dmp(map(recognize((tag("\\"), tag(quote))), |v| dbg!(v)), "2"),
			))));
			map((
				dbg_dmp(map(tag(quote), |v| dbg!(v)), "3"),
				dbg_dmp(map(body, |v| dbg!(v)), "4"),
				dbg_dmp(map(tag(quote), |v| dbg!(v)), "5"),
			), |(_, v, _)| v).parse(input)
		}
	}
	
	alt((
		map_res(inner(true), |v| {
			let mut chars = v.chars();
			let Some(char) = chars.next() else {
				return Err(todo!("empty char literal"))
			};
			let None = chars.next() else {
				// TODO: fold 
				return Err(todo!("char literal with more than one character"))
			};
			Ok(Literal::Char(char))
		}),
		map(inner(false), |v| Literal::String(v.to_string())),
	)).parse(input) */
	
	let input_start = input;
	let (input, quote) = alt((
		tag("'"),
		tag("\""),
	)).parse(input)?;
	let Some(quote_char) = quote.chars().next() else { unreachable!() };
	let (input, body) = recognize(many1_count(alt((
		verify(
			take_while(|c| c != quote_char && c != '\\'),
			|str: &str| str.len() != 0,
		),
		recognize((tag("\\"), tag(quote))),
	)))).parse(input)?;
	let (input, _) = tag(quote).parse(input)?;
	
	let literal = match quote {
		"'" => {
			let mut chars = body.chars();
			let Some(char) = chars.next() else {
				return Err(nom::Err::Failure(VerboseError::from_external_error(
					input_start,
					ErrorKind::Alpha,
					"empty char literal",
				)))
			};
			let None = chars.next() else {
				// TODO: replace escapes
				return Err(nom::Err::Failure(VerboseError::from_external_error(
					input_start,
					ErrorKind::Alpha,
					"char literal with more than one character",
				)))
			};
			Literal::Char(char)
		},
		"\"" => {
			Literal::String(body.to_string())
		},
		_ => unreachable!(),
	};
	dbg!(line!());
	Ok((input, literal))
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

pub fn dbg_dmp<'a, F, O, E: std::fmt::Debug>(
  f: F,
  context: &'static str,
) -> impl Fn(&'a str) -> nom::IResult<&'a str, O, E>
where
  F: Fn(&'a str) -> nom::IResult<&'a str, O, E>,
{
  move |i: &'a str| match f(i) {
    Err(e) => {
      println!("{}: Error({:?}) at:\n{}", context, e, i);
      Err(e)
    }
    a => a,
  }
}
