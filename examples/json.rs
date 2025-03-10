use std::collections::HashMap;
use std::str::FromStr;

use nbnf::nbnf;
use nbnf::nom::IResult;
use nbnf::nom::bytes::complete::take_while;
use nbnf::nom::bytes::tag;
use nbnf::nom::combinator::value;
use nbnf::nom::multi::separated_list0;

fn main() {
	let input = r#"[1, 2.3, "four", {"five": false, "six": 7}, null, "abc \"def\" ghi\n"]"#;
	_ = dbg!(json.parse(input));
}

#[derive(Clone, Copy, Debug)]
pub enum Number {
	Int(i128),
	Float(f64),
}

#[derive(Clone, Debug)]
pub enum Json {
	Null,
	Bool(bool),
	Number(Number),
	String(String),
	Array(Vec<Json>),
	Object(HashMap<String, Json>),
}

#[rustfmt::skip]
nbnf!(r#"
	json<Json> = -ws json_inner -ws;
	json_inner<Json> =
		null /
		boolean /
		number /
		string|<Json::String> /
		array /
		object /
		nbnf::nom::combinator::eof@<Json::Null>;

	null<Json> = "null"@<Json::Null>;
	boolean<Json> =
		"true"@<Json::Bool(true)> /
		"false"@<Json::Bool(false)>;

	number<Json> =
		(number_float / number_int / number_hex)
		|<Json::Number>;
	number_float<Number> =
		~([0-9]+ '.' [0-9]*)
		|!<|str| f64::from_str(str).map(Number::Float)>;
	number_int<Number> =
		~([0-9]+)
		|!<|str| i128::from_str(str).map(Number::Int)>;
	number_hex<Number> =
		(-('0' [xX]) ~([0-9a-fA-F]+))
		|!<|str| i128::from_str_radix(str, 16).map(Number::Int)>;

	string<String> =
		(-'"' string_inner* -'"')
		|<String::from_iter>;
	string_inner<char> =
		"\\\""@<'"'> /
		"\\n"@<'\n'> /
		"\\r"@<'\r'> /
		"\\t"@<'\t'> /
		"\\0"@<'\0'> /
		"\\"@<'\\'> /
		[^"];

	array<Json> =
		(-'[' array_inner -']')
		|<Json::Array>;

	object<Json> =
		(-'{' object_inner -'}')
		|<HashMap::from_iter>
		|<Json::Object>;
	object_pair<(String, Json)> = -ws string -ws -':' -ws json -ws;
"#);

fn array_inner(input: &str) -> IResult<&str, Vec<Json>> {
	separated_list0(tag(","), json).parse(input)
}

fn object_inner(input: &str) -> IResult<&str, Vec<(String, Json)>> {
	separated_list0(tag(","), object_pair).parse(input)
}

fn ws(input: &str) -> IResult<&str, ()> {
	value((), take_while(char::is_whitespace)).parse(input)
}
