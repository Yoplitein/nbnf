#![allow(unused)]

use nom::Parser;

// #[cfg(none)]
fn main() -> anyhow::Result<()> {
	// top = 'a' 'b'
	let grammar = r#"
		top = [ab-cde-fg];
	"#;
	let grammar = nbnf::parse_grammar(grammar)?;
	// let grammar = nbnf::lexer::lex(grammar)?;
	dbg!(grammar);

	Ok(())
}

#[cfg(none)]
fn main() -> anyhow::Result<()> {
	let inp = r#""ab""#;
	dbg!(nbnf::lexer::token(inp));

	Ok(())
}
