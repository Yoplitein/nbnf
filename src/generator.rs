use std::{collections::HashSet, fmt::Write};

use anyhow::{ensure, Result as AResult};
use proc_macro2::{Span, TokenStream, Ident};
use quote::quote;

use crate::{Grammar, Literal, Rule};

pub fn generate_parser(grammar: &Grammar) -> AResult<String> {
	let mut module = quote! {
		use nom::Parser;
	};
	for (rule_name, rule) in &grammar.rules {
		let parser = rule_top(&rule)?;
		let rule_name = Ident::new_raw(&rule_name, Span::call_site());
		module = quote! {
			#module
			
			fn #rule_name(input: &str) -> nom::IResult<&str, &str> {
				let (input, output) = #parser.parse(input)?;
				Ok((input, output))
			}
		};
	}
	
	// let res = prettyplease::unparse(file) // TODO
	let res = module.to_string();
	Ok(res)
}

fn rule_top(rule: &Rule) -> AResult<TokenStream> {
	let parser = rule_body(rule)?;
	let output = quote! {
		// TODO: figure out subparser outputs
		nom::combinator::recognize(#parser)
	};
	Ok(output)
}

fn rule_body(rule: &Rule) -> AResult<TokenStream> {
	match rule {
		Rule::Literal(v) => literal(v),
		Rule::Rule(rule_name) => Ok(quote! { #rule_name }),
		Rule::Group(rules) | Rule::Alternate(rules) => group_or_alternate(matches!(rule, Rule::Group(_)), rules),
		&Rule::Repeat { ref rule, min, max } => repeat(rule, min, max),
	}
}

fn literal(literal: &Literal) -> AResult<TokenStream> {
	Ok(match literal {
		Literal::Char(char) => {
			todo!()
		},
		Literal::String(str) => {
			quote! {
				nom::bytes::complete::tag(#str)
			}
		},
		Literal::Range { chars, ranges } => {
			todo!()
		},
	})
}

fn group_or_alternate(is_group: bool, rules: &[Rule]) -> AResult<TokenStream> {
	ensure!(
		rules.len() > 1,
		"encountered invalid group/alternate \
		with less than two elements"
	);
	
	let mut seq = quote!{};
	for child in rules {
		if !seq.is_empty() {
			seq = quote! { #seq, };
		}
		let parser = rule_body(child)?;
		seq = quote! { #seq #parser };
	}
	
	if is_group {
		seq = quote! { (#seq) };
	} else {
		seq = quote! {
			nom::branch::alt(#seq)
		};
	}
	
	Ok(seq)
}

fn repeat(rule: &Rule, min: usize, max: Option<usize>) -> AResult<TokenStream> {
	todo!()
}

fn ident(ident: &str) -> Ident {
	Ident::new(ident, Span::call_site())
}
