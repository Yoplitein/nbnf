use std::fmt::Write;

use quote::quote;

#[proc_macro]
pub fn nbnf(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let str: syn::LitStr = match syn::parse(tokens) {
		Ok(s) => s,
		Err(err) => {
			return compile_error(
				None,
				&format!("grammar must be given as a string ({err:?})"),
			)
		},
	};
	let str = str.value();
	let tokens = match nbnf_language::lexer::lex(&str) {
		Ok(t) => t,
		Err(err) => return compile_error(Some(err), "couldn't lex given grammar"),
	};
	let grammar = match nbnf_language::parser::parse(tokens) {
		Ok(g) => g,
		Err(err) => return compile_error(Some(err), "couldn't parse given grammar"),
	};
	let tokens = match nbnf_language::generate_parser_tokens(&grammar) {
		Ok(t) => t,
		Err(err) => return compile_error(Some(err), "couldn't generate parser for given grammar"),
	};
	tokens.into()
}

fn compile_error(err: Option<anyhow::Error>, msg: &str) -> proc_macro::TokenStream {
	let msg = if let Some(err) = err {
		let mut causes = String::new();
		for cause in err.chain() {
			if !causes.is_empty() {
				causes.write_str("\ncaused by: ").unwrap();
			}
			causes.write_fmt(format_args!("{cause}")).unwrap();
		}
		&format!("{msg}: {causes}")
	} else {
		msg
	};
	quote! { compile_error!(#msg); }.into()
}
