#![allow(unused)]

use std::collections::HashMap;

use nbnf_macro::nbnf;
use nom::{branch::alt, bytes::{complete::take_while, tag}, character::anychar, combinator::{value, verify}, multi::separated_list0, IResult};

fn main() {
	let input = r#"[1, "two", {"three": false}, null]"#;
	dbg!(json.parse(input));
}

#[derive(Clone, Debug)]
enum Json {
	Null,
	Bool(bool),
	Number(i128),
	String(String),
	Array(Vec<Json>),
	Object(HashMap<String, Json>),
}

nbnf! {
	json<Json> =
		(ws json_inner ws)
		|<|(_, v, _)| v>;
	json_inner<Json> =
		null /
		boolean /
		number /
		string|<Json::String> /
		array /
		object /
		nom::combinator::eof@<Json::Null>;
	null<Json> = "null"@<Json::Null>;
	boolean<Json> =
		"true"@<Json::Bool(true)> /
		"false"@<Json::Bool(false)>;
	number<Json> =
		~([0-9]+)
		|<|str| <i128 as std::str::FromStr>::from_str(str).map(Json::Number).unwrap()>;
	string<String> =
		('"' ~string_inner+ '"')
		|<|(_, str, _): (_, &str, _)| str.to_string()>;
	array<Json> =
		('[' array_inner ']')
		|<|(_, xs, _)| Json::Array(xs)>;
	object<Json> =
		('{' object_pairs '}')
		|<|(_, xs, _)| Json::Object(xs.into_iter().collect())>;
	object_pairs<Vec<(String, Json)>> = object_pair*;
	object_pair<(String, Json)> =
		(string ':' json)
		|<|(k, _, v)| (k, v)>;
}

fn string_inner(input: &str) -> IResult<&str, char> {
	alt((
		value('"', tag("\\\"")),
		verify(
			anychar,
			|&char: &char| char != '"'
		),
	)).parse(input)
}

fn array_inner(input: &str) -> IResult<&str, Vec<Json>> {
	separated_list0(
		tag(","),
		json
	).parse(input)
}

fn ws(input: &str) -> IResult<&str, ()> {
	value(
		(),
		take_while(char::is_whitespace),
	).parse(input)
}
