#![allow(unused)]

use nom::Parser;

// #[cfg(none)]
fn main() -> anyhow::Result<()> {
	// top = 'a' 'b'
	let grammar = r#"
		top = [a-dw-z][123];
	"#;
	let grammar = nbnf::parse_grammar(grammar)?;
	// let grammar = nbnf::lexer::lex(grammar)?;
	dbg!(&grammar);
	let parser = nbnf::generator::generate_parser(&grammar)?;
	eprintln!("```\n{parser}\n```");

	Ok(())
}

#[cfg(none)]
fn main() -> anyhow::Result<()> {
	let inp = r#""ab""#;
	dbg!(nbnf::lexer::token(inp));

	Ok(())
}
