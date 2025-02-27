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
		Literal::Char(char) => quote! {
			nom::character::complete::char(#char)
		},
		Literal::String(str) => quote! {
			nom::bytes::complete::tag(#str)
		},
		Literal::Range { chars, ranges } => {
			let mut conditions = vec![];
			for char in chars {
				conditions.push(quote! {
					char == #char
				});
			}
			for range in ranges {
				let start = range.start();
				let end = range.end();
				conditions.push(quote! {
					(#start ..= #end).contains(&char)
				});
			}
			let conditions = conditions.into_iter().reduce(|l, r| quote! {
				#l || #r
			}).unwrap_or_else(|| quote! {
				// empty range matches exactly one character
				true
			});
			
			quote! {
				nom::combinator::map(
					nom::combinator::verify(
						nom::bytes::complete::take(1usize),
						|str: &str| {
							let mut chars = str.chars();
							let Some(char) = chars.next() else {
								unreachable!("take(1) returned empty string")
							};
							let None = chars.next() else {
								unreachable!("take(1) returned string with more than one char")
							};
							#conditions
						}
					),
					|str: &str| {
						let mut chars = str.chars();
						let Some(char) = chars.next() else {
							unreachable!("take(1) returned empty string")
						};
						let None = chars.next() else {
							unreachable!("take(1) returned string with more than one char")
						};
						char
					}
				)
			}
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
	let inner = rule_body(rule)?;
	Ok(match (min, max) {
		(0, Some(1)) => quote! {
			nom::combinator::opt(#inner)
		},
		(1, Some(1)) => inner,
		(min, Some(max)) => quote! {
			nom::multi::many_m_n(
				#min,
				#max,
				#inner,
			)
		},
		(0, None) => quote! {
			nom::multi::many0(#inner)
		},
		(min, None) => quote! {
			nom::combinator::verify(
				nom::multi::many0(#inner),
				|xs: Vec<_>| xs.len() >= #min,
			)
		},
	})
}

fn ident(ident: &str) -> Ident {
	Ident::new(ident, Span::call_site())
}
