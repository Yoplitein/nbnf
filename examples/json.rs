use std::collections::HashMap;

use nbnf_macro::nbnf;
use nom::bytes::complete::take_while;
use nom::bytes::tag;
use nom::combinator::value;
use nom::multi::separated_list0;
use nom::IResult;

fn main() {
	let input = r#"[1, "two", {"three": false}, null, "abc \"def\" ghi\n"]"#;
	_ = dbg!(json.parse(input));
}

#[derive(Clone, Debug)]
pub enum Json {
	Null,
	Bool(bool),
	Number(i128),
	String(String),
	Array(Vec<Json>),
	Object(HashMap<String, Json>),
}

#[rustfmt::skip]
nbnf!(r#"
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
		('"' string_inner* '"')
		|<|(_, chars, _)| String::from_iter(chars)>;
	string_inner<char> =
		"\\\""@<'"'> /
		"\\n"@<'\n'> /
		"\\r"@<'\r'> /
		"\\t"@<'\t'> /
		"\\0"@<'\0'> /
		"\\"@<'\\'> /
		[^"];
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
"#);

fn array_inner(input: &str) -> IResult<&str, Vec<Json>> {
	separated_list0(tag(","), json).parse(input)
}

fn ws(input: &str) -> IResult<&str, ()> {
	value((), take_while(char::is_whitespace)).parse(input)
}
