use std::collections::HashMap;
use std::num::ParseIntError;
use std::str::FromStr;

use nbnf::nbnf;
use nbnf::nom::IResult;
use nbnf::nom::bytes::complete::take_while;
use nbnf::nom::combinator::value;

fn main() {
	let input = r#"[1, -2.3, "four", {"five": false, "six": 0x7}, null, "abc \"def\" ghi\n"]"#;
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
		$nom::combinator::eof@<Json::Null>;

	null<Json> = "null"@<Json::Null>;
	boolean<Json> =
		"true"@<Json::Bool(true)> /
		"false"@<Json::Bool(false)>;

	number<Json> =
		(number_hex / number_float / number_int)
		|<Json::Number>;
	number_float<Number> =
		~('-'? [0-9]+ '.' [0-9]*)
		|!<|str| f64::from_str(str).map(Number::Float)>;
	number_int<Number> =
		~('-'? [0-9]+)
		|!<|str| i128::from_str(str).map(Number::Int)>;
	number_hex<Number> =
		~('-'? '0' [xX] [0-9a-fA-F]+)
		|!<parse_number_hex>;

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
	
	comma = ",";
	array<Json> =
		(-'[' array_inner -']')
		|<Json::Array>;
	array_inner<Vec<Json>> = <$nom::multi::separated_list0(comma, json)>;

	object<Json> =
		(-'{' object_inner -'}')
		|<HashMap::from_iter>
		|<Json::Object>;
	object_inner<Vec<(String, Json)>> = <$nom::multi::separated_list0(comma, object_pair)>;
	object_pair<(String, Json)> = -ws string -ws -':' -ws json -ws;
"#);

fn ws(input: &str) -> IResult<&str, ()> {
	value((), take_while(char::is_whitespace)).parse(input)
}

fn parse_number_hex(str: &str) -> Result<Number, ParseIntError> {
	let negative = str.starts_with("-");
	let Some(digits_start) = str.find(|c| c == 'x' || c == 'X') else {
		// it must exist if the parser matched
		unreachable!()
	};
	let digits = &str[digits_start + 1 ..];
	let mut v = i128::from_str_radix(digits, 16)?;
	if negative {
		v *= -1;
	}
	Ok(Number::Int(v))
}
