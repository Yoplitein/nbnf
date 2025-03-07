use std::collections::HashSet;
use std::ops::RangeInclusive;

use anyhow::{Result as AResult, bail, ensure};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while1};
use nom::character::complete::{anychar, char, usize};
use nom::combinator::{complete, cut, eof, map, map_opt, map_res, opt, recognize, value, verify};
use nom::error::{ErrorKind, FromExternalError};
use nom::multi::{fold_many_m_n, fold_many1, many0, separated_list0, separated_list1};
use nom::{Finish, Offset, Parser};
use nom_language::error::VerboseError;

use crate::Literal;

/// A parsed token.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
	/// A rule name.
	Rule(String),
	/// A literal value.
	Literal(Literal),
	/// A repeat modifier.
	Repeat {
		/// The minimum number of times to repeat.
		min: usize,
		/// The maximum number of times to repeat, if any.
		max: Option<usize>,
	},
	/// Rust source code.
	RustSrc(String),
	/// A `=` token.
	Equals,
	/// A `/` token.
	Slash,
	/// A `;` token.
	Semicolon,
	/// A `(` token.
	GroupOpen,
	/// A `)` token.
	GroupClose,
	/// A `-` token.
	Discard,
	/// A `!` token.
	Not,
	/// A `!!` token.
	Cut,
	/// A `~` token.
	Recognize,
	/// A `&` token.
	Epsilon,
	/// A `@` token.
	Value,
	/// A `|` token.
	Map,
	/// A `|?` token.
	MapOpt,
	/// A `|!` token.
	MapRes,
}

/// Parse a grammar into a list of tokens.
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
	macro_rules! wrap {
		($parser:expr) => {
			|inp| $parser.parse(inp)
		};
	}
	alt([
		wrap!(map(path, Token::Rule)),
		wrap!(map(literal, Token::Literal)),
		wrap!(map(rustsrc, Token::RustSrc)),
		wrap!(literal_range),
		wrap!(repeat),
		wrap!(value(
			Token::Repeat {
				min: 0,
				max: Some(1),
			},
			tag("?"),
		)),
		wrap!(value(Token::Repeat { min: 0, max: None }, tag("*"))),
		wrap!(value(Token::Repeat { min: 1, max: None }, tag("+"))),
		wrap!(value(Token::Equals, tag("="))),
		wrap!(value(Token::Slash, tag("/"))),
		wrap!(value(Token::Semicolon, tag(";"))),
		wrap!(value(Token::GroupOpen, tag("("))),
		wrap!(value(Token::GroupClose, tag(")"))),
		wrap!(value(Token::Discard, tag("-"))),
		wrap!(value(Token::Cut, tag("!!"))),
		wrap!(value(Token::Not, tag("!"))),
		wrap!(value(Token::Recognize, tag("~"))),
		wrap!(value(Token::Epsilon, tag("&"))),
		wrap!(value(Token::Value, tag("@"))),
		wrap!(value(Token::MapOpt, tag("|?"))),
		wrap!(value(Token::MapRes, tag("|!"))),
		wrap!(value(Token::Map, tag("|"))),
	])
	.parse(input)
}

fn literal_range(input: &str) -> PResult<Token> {
	#[derive(Clone, Debug)]
	enum CharOrRange {
		Whitespace,
		Char(char),
		Range(RangeInclusive<char>),
	}

	fn ws(input: &str) -> PResult<CharOrRange> {
		map_opt(anychar, |c| {
			Some(match c {
				' ' => CharOrRange::Char(' '),
				c if c.is_whitespace() => CharOrRange::Whitespace,
				_ => return None,
			})
		})
		.parse(input)
	}

	fn char(input: &str) -> PResult<CharOrRange> {
		verify(
			alt((
				map(escape_char, CharOrRange::Char),
				bracket_escape,
				map(verify(anychar, |&char| char != ']'), CharOrRange::Char),
			)),
			|char| !matches!(char, CharOrRange::Char(']')),
		)
		.parse(input)
	}

	fn bracket_escape(input: &str) -> PResult<CharOrRange> {
		value(CharOrRange::Char(']'), tag(r"\]")).parse(input)
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

	let (input, chars_and_ranges) = many0(alt((ws, range, bracket_escape, char))).parse(input)?;
	let mut chars = HashSet::new();
	let mut ranges = HashSet::new();
	for v in chars_and_ranges {
		match v {
			CharOrRange::Whitespace => {},
			CharOrRange::Char(char) => _ = chars.insert(char),
			CharOrRange::Range(range) => _ = ranges.insert(range),
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
				chars: HashSet::new(),
				ranges: HashSet::new(),
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range(r"[[]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: HashSet::from_iter(['[']),
				ranges: HashSet::new(),
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range(r"[]]"),
		Ok((
			"]",
			Token::Literal(Literal::Range {
				chars: HashSet::new(),
				ranges: HashSet::new(),
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range(r"[\]]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: HashSet::from_iter([']']),
				ranges: HashSet::new(),
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range("[a]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: HashSet::from_iter(['a']),
				ranges: HashSet::new(),
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range("[aa]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: HashSet::from_iter(['a']),
				ranges: HashSet::new(),
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range("[ab]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: HashSet::from_iter(['a', 'b']),
				ranges: HashSet::new(),
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range("[^a]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: HashSet::from_iter(['a']),
				ranges: HashSet::new(),
				invert: true,
			})
		)),
	);
	assert_eq!(
		literal_range("[a^]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: HashSet::from_iter(['a', '^']),
				ranges: HashSet::new(),
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range("[^a]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: HashSet::from_iter(['a']),
				ranges: HashSet::new(),
				invert: true,
			})
		)),
	);
	assert_eq!(
		literal_range("[^]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: HashSet::new(),
				ranges: HashSet::new(),
				invert: true,
			})
		)),
	);
	assert_eq!(
		literal_range("[ab-c]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: HashSet::from_iter(['a']),
				ranges: HashSet::from_iter(['b' ..= 'c']),
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range("[ab-cd]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: HashSet::from_iter(['a', 'd']),
				ranges: HashSet::from_iter(['b' ..= 'c']),
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range("[ab-cde-f]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: HashSet::from_iter(['a', 'd']),
				ranges: HashSet::from_iter([
					'b' ..= 'c',
					'e' ..= 'f'
				]),
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range(r#"[\n\r\t\0\\\x7F\u{beEF}]"#),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: HashSet::from_iter([
					'\n', '\r', '\t', '\0', '\\', '\x7F', '\u{BEEF}'
				]),
				ranges: HashSet::new(),
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range(r#"[\u{1010}-\u{2020}]"#),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: HashSet::new(),
				ranges: HashSet::from_iter(['\u{1010}' ..= '\u{2020}']),
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range("[\n\t]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: HashSet::new(),
				ranges: HashSet::new(),
				invert: false,
			})
		)),
	);
	assert_eq!(
		literal_range("[ ]"),
		Ok((
			"",
			Token::Literal(Literal::Range {
				chars: HashSet::from_iter([' ']),
				ranges: HashSet::new(),
				invert: false,
			})
		)),
	);
}

fn repeat(input: &str) -> PResult<Token> {
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
		(Some(_), Some(None)) => {
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
			'_' => true,
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
		alt((escape_char, verify(anychar, |&char| char != quote_char))),
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
	))
	.parse(input)
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
