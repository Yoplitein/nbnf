#![allow(unused)]

use nom::Parser;

// #[cfg(none)]
fn main() -> anyhow::Result<()> {
	// top = 'a' 'b'
	let grammar = r#"
		top = "ab"
	"#;
	let grammar = nbnf::parse_grammar(grammar)?;
	dbg!(grammar);

	Ok(())
}

#[cfg(none)]
fn main() -> anyhow::Result<()> {
	let inp = r#""ab""#;
	dbg!(nbnf::literal(inp));
	
	Ok(())
}
