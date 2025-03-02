use std::ops::RangeInclusive;

use anyhow::bail;
use nom::branch::alt;
use nom::bytes::complete::{tag, take, take_while, take_while1};
use nom::character::anychar;
use nom::character::complete::usize;
use nom::combinator::{cut, eof, map, map_res, opt, recognize, value, verify};
use nom::error::{ErrorKind, FromExternalError};
use nom::multi::{many0, many1_count, separated_list0};
use nom::{Finish, Offset, Parser};
use nom_language::error::VerboseError;

use crate::Literal;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
	Identifier(String),
	Literal(Literal),
	Repeat { min: usize, max: Option<usize> },
	RustSrc(String),
	Equals,
	Slash,
	Semicolon,
	GroupOpen,
	GroupClose,
	Not,
	Recognize,
	Epsilon,
	Value,
	Map,
	MapOpt,
	MapRes,
}

pub fn lex(input: &str) -> anyhow::Result<Vec<Token>> {
	let res = top.parse(input).finish();
	let res = res.map_err(|err| anyhow::anyhow!("{err:#?}"));
	let (_, res) = res?;
	Ok(res)
}

type PResult<'a, T> = nom::IResult<&'a str, T, VerboseError<&'a str>>;

fn top(input: &str) -> PResult<Vec<Token>> {
	let (input, _) = whitespace.parse(input)?;
	let (input, tokens) = separated_list0(whitespace, token).parse(input)?;
	let (input, _) = whitespace.parse(input)?;
	let (input, _) = eof.parse(input)?;
	Ok((input, tokens))
}

fn token(input: &str) -> PResult<Token> {
	alt((
		map(identifier, Token::Identifier),
		map(literal, Token::Literal),
		map(rustsrc, Token::RustSrc),
		literal_range,
		repeat,
		value(
			Token::Repeat {
				min: 0,
				max: Some(1),
			},
			tag("?"),
		),
		value(Token::Repeat { min: 0, max: None }, tag("*")),
		value(Token::Repeat { min: 1, max: None }, tag("+")),
		value(Token::Equals, tag("=")),
		value(Token::Slash, tag("/")),
		value(Token::Semicolon, tag(";")),
		value(Token::GroupOpen, tag("(")),
		value(Token::GroupClose, tag(")")),
		value(Token::Recognize, tag("~")),
		value(Token::Epsilon, tag("&")),
		value(Token::Value, tag("@")),
		value(Token::Map, tag("|")),
		value(Token::MapOpt, tag("|?")),
		value(Token::MapRes, tag("|!")),
	))
	.parse(input)
}

fn literal_range(input: &str) -> PResult<Token> {
	#[derive(Clone, Debug)]
	enum CharOrRange {
		Char(char),
		Range(RangeInclusive<char>),
	}

	fn char(input: &str) -> PResult<CharOrRange> {
		verify(
			map(take(1usize), |str: &str| {
				let mut chars = str.chars();
				let Some(char) = chars.next() else {
					unreachable!()
				};
				let None = chars.next() else {
					panic!("more than one char given by take(1usize)")
				};
				CharOrRange::Char(char)
			}),
			|char| !matches!(char, CharOrRange::Char(']')),
		)
		.parse(input)
	}

	fn escape_char(input: &str) -> PResult<CharOrRange> {
		alt((
			value(CharOrRange::Char(']'), tag(r"\]")),
			// TODO: remaining escapes
		))
		.parse(input)
	}

	fn range(input: &str) -> PResult<CharOrRange> {
		let (input, CharOrRange::Char(start)) = char.parse(input)? else {
			unreachable!()
		};
		let (input, _) = tag("-").parse(input)?;
		let (input, CharOrRange::Char(end)) = char.parse(input)? else {
			unreachable!()
		};
		Ok((input, CharOrRange::Range(start ..= end)))
	}

	let (input, _) = whitespace.parse(input)?;
	let (input, _) = tag("[").parse(input)?;

	let (input, chars_and_ranges) = many0(alt((range, escape_char, char))).parse(input)?;
	let mut chars = vec![];
	let mut ranges = vec![];
	for v in chars_and_ranges {
		match v {
			CharOrRange::Char(char) => chars.push(char),
			CharOrRange::Range(range) => ranges.push(range),
		}
	}

	let (input, _) = tag("]").parse(input)?;
	let (input, _) = whitespace.parse(input)?;

	Ok((input, Token::Literal(Literal::Range { chars, ranges })))
}

#[test]
fn test_literal_range() {
	assert_eq!(
		literal_range("[]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec![],
				ranges: vec![]
			})
		)),
	);
	assert_eq!(
		literal_range(r"[[]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec!['['],
				ranges: vec![]
			})
		)),
	);
	assert_eq!(
		literal_range(r"[\]]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec![']'],
				ranges: vec![]
			})
		)),
	);
	assert_eq!(
		literal_range("[a]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec!['a'],
				ranges: vec![]
			})
		)),
	);
	assert_eq!(
		literal_range("[ab]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec!['a', 'b'],
				ranges: vec![]
			})
		)),
	);
	assert_eq!(
		literal_range("[ab-c]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec!['a'],
				ranges: vec!['b' ..= 'c']
			})
		)),
	);
	assert_eq!(
		literal_range("[ab-cd]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec!['a', 'd'],
				ranges: vec!['b' ..= 'c']
			})
		)),
	);
	assert_eq!(
		literal_range("[ab-cde-f]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec!['a', 'd'],
				ranges: vec![
					'b' ..= 'c',
					'e' ..= 'f'
				]
			})
		)),
	);
}

fn repeat(input: &str) -> PResult<Token> {
	let orig = input;
	let (input, _) = whitespace.parse(input)?;
	let (input, _) = tag("{").parse(input)?;
	let (input, _) = whitespace.parse(input)?;
	let (input, mut min) = opt(usize).parse(input)?;
	let (input, mut max) = map(opt((whitespace, tag(","), whitespace, opt(usize))), |v| {
		v.map(|v| v.3)
	})
	.parse(input)?;
	let (input, _) = whitespace.parse(input)?;
	let (input, _) = tag("}").parse(input)?;
	let (input, _) = whitespace.parse(input)?;

	match (min, max) {
		(Some(_), Some(Some(_))) => {},
		(Some(_), Some(none)) => {
			max = None;
		},
		(Some(n), None) => {
			max = Some(Some(n));
		},
		(None, _) => min = Some(0),
	}
	let min = min.unwrap();
	let max = max.flatten();
	Ok((input, Token::Repeat { min, max }))
}

#[test]
fn test_repeat() {
	assert_eq!(
		repeat("{10,20}"),
		Ok((
			"",
			Token::Repeat {
				min: 10,
				max: Some(20)
			}
		)),
	);
	assert_eq!(
		repeat("{10,}"),
		Ok(("", Token::Repeat { min: 10, max: None })),
	);
	assert_eq!(
		repeat("{,20}"),
		Ok((
			"",
			Token::Repeat {
				min: 0,
				max: Some(20)
			}
		)),
	);
	assert_eq!(repeat("{,}"), Ok(("", Token::Repeat { min: 0, max: None })),);
	assert_eq!(repeat("{}"), Ok(("", Token::Repeat { min: 0, max: None })),);
	assert_eq!(
		repeat("{10}"),
		Ok((
			"",
			Token::Repeat {
				min: 10,
				max: Some(10)
			}
		)),
	);
}

fn rustsrc(input: &str) -> PResult<String> {
	let (mut input, _) = tag("<").parse(input)?;
	let start = input;
	let output_str;
	let mut bracket_level = 1;
	loop {
		let (next_input, char) = cut(anychar).parse(input)?;
		match char {
			'<' => bracket_level += 1,
			'>' => bracket_level -= 1,
			_ => {},
		}
		if bracket_level == 0 {
			output_str = &start[.. start.offset(input)];
			input = next_input;
			break;
		}
		input = next_input;
	}
	Ok((input, output_str.into()))
}

#[test]
fn test_rustsrc() {
	let mut str = std::cell::UnsafeCell::new(String::new());
	let mut parse = move |input| {
		rustsrc(input).map(|(rest, res)| {
			*str.get_mut() = res;
			let res = unsafe {
				// SAFETY: reference used strictly just for equality checks
				str.get().cast_const().as_ref().unwrap().as_str()
			};
			(rest, res)
		})
	};
	
	assert_eq!(
		parse("<>"),
		Ok(("", "")),
	);
	assert_eq!(
		parse("<<>>"),
		Ok(("", "<>")),
	);
	assert!(parse("<<>").is_err());
	assert_eq!(
		parse("<foo>"),
		Ok(("", "foo")),
	);
	// eventually we may want to disallow this actually
	assert_eq!(
		parse("<({>"),
		Ok(("", "({")),
	);
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

fn literal(input: &str) -> PResult<Literal> {
	let input_start = input;
	let (input, quote) = alt((tag("'"), tag("\""))).parse(input)?;
	let Some(quote_char) = quote.chars().next() else {
		unreachable!()
	};
	let (input, body) = recognize(many1_count(alt((
		verify(take_while(|c| c != quote_char && c != '\\'), |str: &str| {
			str.len() != 0
		}),
		recognize((tag("\\"), tag(quote))),
	))))
	.parse(input)?;
	let (input, _) = tag(quote).parse(input)?;

	let literal = match quote {
		"'" => {
			let mut chars = body.chars();
			let Some(char) = chars.next() else {
				return Err(nom::Err::Failure(VerboseError::from_external_error(
					input_start,
					ErrorKind::Alpha,
					"empty char literal",
				)));
			};
			let None = chars.next() else {
				// TODO: replace escapes
				return Err(nom::Err::Failure(VerboseError::from_external_error(
					input_start,
					ErrorKind::Alpha,
					"char literal with more than one character",
				)));
			};
			Literal::Char(char)
		},
		"\"" => Literal::String(body.to_string()),
		_ => unreachable!(),
	};
	Ok((input, literal))
}

fn whitespace(input: &str) -> PResult<()> {
	map(take_while(char::is_whitespace), |_| ()).parse(input)
}
