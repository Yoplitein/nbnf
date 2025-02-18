use nom::{branch::alt, bytes::complete::{tag, take_while, take_while1}, character::complete::usize, combinator::{eof, map, recognize, value, verify}, error::{ErrorKind, FromExternalError}, multi::{many1_count, separated_list0}, Finish, Parser};
use nom_language::error::VerboseError;

use crate::Literal;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
	Identifier(String),
	Literal(Literal),
	Number(usize),
	Equals,
	Slash,
	Question,
	Star,
	Comma,
	Semicolon,
	GroupOpen,
	GroupClose,
	RangeOpen,
	RangeClose,
	RepeatOpen,
	RepeatClose,
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
		map(usize, Token::Number),
		value(Token::Equals, tag("=")),
		value(Token::Slash, tag("/")),
		value(Token::Question, tag("?")),
		value(Token::Star, tag("*")),
		value(Token::Comma, tag(",")),
		value(Token::Semicolon, tag(";")),
		value(Token::GroupOpen, tag("(")),
		value(Token::GroupClose, tag(")")),
		value(Token::RangeOpen, tag("[")),
		value(Token::RangeClose, tag("]")),
		value(Token::RepeatOpen, tag("{")),
		value(Token::RepeatClose, tag("}")),
	)).parse(input)
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
