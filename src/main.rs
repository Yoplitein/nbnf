#![allow(unused)]

use nom::Parser;

fn main() -> anyhow::Result<()> {
	// top = 'a' 'b'
	let grammar = r#"
		top = wef
	"#;
	let grammar = nbnf::parse_grammar(grammar)?;
	dbg!(grammar);

	Ok(())
}
