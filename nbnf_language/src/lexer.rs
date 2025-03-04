use std::borrow::Cow;
use std::ops::RangeInclusive;

use anyhow::{anyhow, bail, ensure, Result as AResult};
use nom::branch::alt;
use nom::bytes::complete::{tag, take, take_while, take_while1};
use nom::character::complete::{anychar, char};
use nom::character::complete::usize;
use nom::combinator::{complete, cut, eof, map, map_res, opt, recognize, value, verify};
use nom::error::{ErrorKind, FromExternalError};
use nom::multi::{
	count,
	fold_many1,
	fold_many_m_n,
	many0,
	many0_count,
	many1_count,
	many_m_n,
	separated_list0,
	separated_list1,
};
use nom::{Finish, Offset, Parser};
use nom_language::error::VerboseError;

use crate::Literal;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
	Rule(String),
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
	let res = complete(top).parse(input).finish();
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
		map(path, Token::Rule),
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
		value(Token::Not, tag("!")),
		value(Token::Recognize, tag("~")),
		value(Token::Epsilon, tag("&")),
		value(Token::Value, tag("@")),
		value(Token::MapOpt, tag("|?")),
		value(Token::MapRes, tag("|!")),
		value(Token::Map, tag("|")),
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
			alt((
				map(escape_char, CharOrRange::Char),
				map(anychar, CharOrRange::Char),
			)),
			|char| !matches!(char, CharOrRange::Char(']')),
		)
		.parse(input)
	}

	fn bracket_escape(input: &str) -> PResult<CharOrRange> {
		value(CharOrRange::Char(']'), tag(r"\]"))
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

	let (input, invert) = opt(tag("^")).parse(input)?;
	let invert = invert.is_some();

	let (input, chars_and_ranges) = many0(alt((range, bracket_escape, char))).parse(input)?;
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

	Ok((
		input,
		Token::Literal(Literal::Range {
			chars,
			ranges,
			invert,
		}),
	))
}

#[test]
fn test_literal_range() {
	assert_eq!(
		literal_range("[]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec![],
				ranges: vec![],
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range(r"[[]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec!['['],
				ranges: vec![],
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range(r"[\]]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec![']'],
				ranges: vec![],
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range("[a]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec!['a'],
				ranges: vec![],
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range("[ab]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec!['a', 'b'],
				ranges: vec![],
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range("[^a]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec!['a'],
				ranges: vec![],
				invert: true,
			})
		)),
	);
	assert_eq!(
		literal_range("[a^]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec!['a', '^'],
				ranges: vec![],
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range("[^a]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec!['a'],
				ranges: vec![],
				invert: true,
			})
		)),
	);
	assert_eq!(
		literal_range("[^]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec![],
				ranges: vec![],
				invert: true,
			})
		)),
	);
	assert_eq!(
		literal_range("[ab-c]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec!['a'],
				ranges: vec!['b' ..= 'c'],
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range("[ab-cd]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec!['a', 'd'],
				ranges: vec!['b' ..= 'c'],
				invert: false,
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
				],
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range(r#"[\n\r\t\0\\\x7F\u{beEF}]"#),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec!['\n', '\r', '\t', '\0', '\\', '\x7F', '\u{BEEF}'],
				ranges: vec![],
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range(r#"[\u{1010}-\u{2020}]"#),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: vec![],
				ranges: vec!['\u{1010}' ..= '\u{2020}'],
				invert: false,
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

	assert_eq!(parse("<>"), Ok(("", "")),);
	assert_eq!(parse("<<>>"), Ok(("", "<>")),);
	assert!(parse("<<>").is_err());
	assert_eq!(parse("<foo>"), Ok(("", "foo")),);
	assert_eq!(parse("< >"), Ok(("", " ")),);
	assert_eq!(parse("<foo bar>"), Ok(("", "foo bar")),);
	assert_eq!(parse("< foo bar >"), Ok(("", " foo bar ")),);
	// eventually we may want to disallow this actually
	assert_eq!(parse("<({>"), Ok(("", "({")),);
}

fn path(input: &str) -> PResult<String> {
	map(
		recognize((opt(tag("::")), separated_list1(tag("::"), identifier))),
		str::to_string,
	)
	.parse(input)
}

fn identifier(input: &str) -> PResult<&str> {
	fn is_ident_char(start: bool, char: char) -> bool {
		match char {
			'_' | '-' => true,
			_ if char.is_numeric() => !start,
			_ => char.is_alphanumeric(),
		}
	}

	recognize((
		take_while1(|c| is_ident_char(true, c)),
		take_while(|c| is_ident_char(false, c)),
	))
	.parse(input)
}

fn literal(input: &str) -> PResult<Literal> {
	let input_start = input;
	let (input, quote_char) = alt((char('\''), char('"'))).parse(input)?;
	let (input, body) = fold_many1(
		alt((
			escape_char,
			verify(
				anychar,
				|&char| char != quote_char,
			),
		)),
		String::new,
		|mut str, char| {
			str.push(char);
			str
		},
	)
	.parse(input)?;
	let (input, _) = char(quote_char).parse(input)?;

	let literal = match quote_char {
		'\'' => {
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
		'"' => Literal::String(body.to_string()),
		_ => unreachable!(),
	};
	Ok((input, literal))
}

fn escape_char(input: &str) -> PResult<char> {
	alt((
		value('\'', tag(r#"\'"#)),
		value('"', tag(r#"\""#)),
		value('\\', tag(r#"\\"#)),
		value('\n', tag(r#"\n"#)),
		value('\r', tag(r#"\r"#)),
		value('\t', tag(r#"\t"#)),
		value('\0', tag(r#"\0"#)),
		hex_escape,
		unicode_escape,
	)).parse(input)
}

fn hex_char(input: &str) -> PResult<char> {
	verify(anychar, char::is_ascii_hexdigit).parse(input)
}

fn hex_digits(min: usize, max: usize) -> impl FnMut(&str) -> PResult<&str> {
	move |input: &str| -> PResult<&str> {
		recognize(fold_many_m_n(min, max, hex_char, || (), |_, _| ())).parse(input)
	}
}

fn hex_escape(input: &str) -> PResult<char> {
	let (input, _) = tag(r#"\x"#).parse(input)?;
	let (input, char) = cut(map_res(hex_digits(2, 2), |str| -> AResult<char> {
		let val = u32::from_str_radix(str, 16)?;
		ensure!(
			val < 0x80,
			"ASCII escapes must be in the range 0x00 ..= 0x7F"
		);
		let Some(char) = char::from_u32(val) else {
			unreachable!()
		};
		Ok(char)
	}))
	.parse(input)?;
	Ok((input, char))
}

fn unicode_escape(input: &str) -> PResult<char> {
	let (input, _) = tag("\\u{").parse(input)?;
	let (input, char) = cut(map_res(hex_digits(1, 6), |str| -> AResult<char> {
		let val = u32::from_str_radix(str, 16)?;
		let Some(char) = char::from_u32(val as _) else {
			bail!("U+{val:X} is not a valid codepoint");
		};
		Ok(char)
	}))
	.parse(input)?;
	let (input, _) = tag("}").parse(input)?;
	Ok((input, char))
}

#[test]
fn test_literal() {
	assert_eq!(literal(r#"'a'"#), Ok(("", Literal::Char('a'))),);
	assert!(matches!(literal(r#"''"#), Err(_),));
	assert_eq!(literal(r#""a""#), Ok(("", Literal::String("a".into()))),);
	assert_eq!(literal(r#""ab""#), Ok(("", Literal::String("ab".into()))),);
	assert!(matches!(literal(r#""""#), Err(_),));
	assert_eq!(literal(r#"'\''"#), Ok(("", Literal::Char('\''))),);
	assert_eq!(literal(r#"'\n'"#), Ok(("", Literal::Char('\n'))),);
	assert_eq!(
		literal(r#""a\"b""#),
		Ok(("", Literal::String("a\"b".into()))),
	);
	assert_eq!(
		literal(r#""a\\\"b""#),
		Ok(("", Literal::String("a\\\"b".into()))),
	);
	assert_eq!(
		literal(r#""\n\r\t\0\x7f\x7F\u{beEF}""#),
		Ok(("", Literal::String("\n\r\t\0\x7F\x7F\u{BEEF}".into()))),
	);
	assert!(matches!(literal(r#""\x80""#), Err(_),));
	assert!(matches!(literal(r#""\u{}""#), Err(_),));
	assert!(matches!(literal(r#""\u{1234567}""#), Err(_),));
}

fn whitespace(input: &str) -> PResult<()> {
	map(take_while(char::is_whitespace), |_| ()).parse(input)
}
