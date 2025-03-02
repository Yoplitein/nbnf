#![allow(unused)]

use quote::quote;

#[proc_macro]
pub fn nbnf(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let str = tokens.to_string();
	let grammar = match nbnf_language::parse_grammar(&str) {
		Ok(g) => g,
		Err(err) => return compile_error(&format!("couldn't parse given grammar: {err}")),
	};
	let tokens = match nbnf_language::generate_parser_tokens(&grammar) {
		Ok(t) => t,
		Err(err) => return compile_error(&format!("couldn't generate parser for given grammar: {err}")),
	};
	tokens.into()
}

fn compile_error(msg: &str) -> proc_macro::TokenStream {
	quote! { compile_error!(#msg); }.into()
}
