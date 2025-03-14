use std::collections::HashSet;

use anyhow::{ensure, Context, Result as AResult};
use proc_macro2::{Group, Ident, Span, TokenStream, TokenTree};
use quote::{quote, TokenStreamExt};
use syn::Path;

use crate::parser::MapFunc;
use crate::{Expr, GLiteral, Grammar, Literal};

/**
	Generate a [String] of Rust source implememting parsers for the given grammar.

	If the `prettyplease` feature is enabled, the returned code will be pretty-printed.
*/
pub fn generate_parser(grammar: &Grammar) -> AResult<String> {
	let module = generate_parser_tokens(grammar)?;
	#[cfg(feature = "prettyplease")]
	let res = {
		let file = syn::parse2(module)?;
		prettyplease::unparse(&file)
	};
	#[cfg(not(feature = "prettyplease"))]
	let res = module.to_string();
	Ok(res)
}

/**
	Generate a `TokenStream` with implementations for the given grammar.
*/
pub fn generate_parser_tokens(grammar: &Grammar) -> AResult<TokenStream> {
	let mut module = quote! {
		use nbnf::nom::Parser;
	};

	for rule_name in &grammar.rule_order {
		let rule = grammar
			.rules
			.get(rule_name)
			.unwrap_or_else(|| unreachable!());
		let parser = expr_body(&rule.body)?;
		let rule_ident = raw_ident(rule_name);
		let input_type = &rule.input_type;
		let output_type = &rule.output_type;
		module = quote! {
			#module

			fn #rule_ident(input: #input_type) -> nbnf::nom::IResult<#input_type, #output_type> {
				#parser.parse(input)
			}
		};
	}

	Ok(module)
}

fn expr_body(body: &Expr) -> AResult<TokenStream> {
	match body {
		Expr::Literal(v) => literal(v),
		Expr::Rule(code) => {
			let code = expand_placeholders(code.clone(), &|ident| match ident {
				"nom" => Some(quote! { nbnf::nom }),
				_ => None,
			});
			Ok(quote! { #code })
		},
		Expr::Group(exprs) | Expr::Alternate(exprs) => {
			group_or_alternate(matches!(body, Expr::Group(_)), exprs)
		},
		&Expr::Repeat { ref expr, min, max } => repeat(expr, min, max),
		Expr::Discard(_) => {
			// discards must be handled by group_or_alternate
			panic!("got unexpected Discard when generating expression body");
		},
		Expr::Not(inner) => {
			let inner = expr_body(inner)?;
			Ok(quote! {
				nbnf::nom::combinator::not(#inner)
			})
		},
		Expr::Cut(inner) => {
			let inner = expr_body(inner)?;
			Ok(quote! {
				nbnf::nom::combinator::cut(#inner)
			})
		},
		Expr::Recognize(inner) => {
			let inner = expr_body(inner)?;
			Ok(quote! {
				nbnf::nom::combinator::recognize(#inner)
			})
		},
		Expr::Epsilon => Ok(quote! {
			nbnf::nom::combinator::success(())
		}),
		Expr::Map {
			expr,
			func,
			mapping_code,
		} => {
			let expr = expr_body(expr)?;
			let func_ident = path(match func {
				MapFunc::Value => "nbnf::nom::combinator::value",
				MapFunc::Map => "nbnf::nom::combinator::map",
				MapFunc::MapOpt => "nbnf::nom::combinator::map_opt",
				MapFunc::MapRes => "nbnf::nom::combinator::map_res",
			})?;
			Ok(match func {
				MapFunc::Value => quote! {
					#func_ident(#mapping_code, #expr)
				},
				_ => quote! {
					#func_ident(#expr, #mapping_code)
				},
			})
		},
	}
}

fn literal(literal: &Literal) -> AResult<TokenStream> {
	Ok(match literal {
		Literal::Char(GLiteral::Char(char)) => quote! {
			nbnf::nom::character::complete::char(#char)
		},
		Literal::Char(GLiteral::String(str)) => quote! {
			nbnf::nom::bytes::complete::tag(#str)
		},
		&Literal::Char(GLiteral::Range {
			ref chars,
			ref ranges,
			invert,
		}) => {
			let mut patterns = vec![];
			for char in chars {
				patterns.push(quote! { #char });
			}
			for range in ranges {
				let start = range.start();
				let end = range.end();
				patterns.push(quote! { #start ..= #end });
			}
			let patterns = patterns.into_iter().reduce(|left, right| {
				quote! {
					#left | #right
				}
			});

			let invert = if invert {
				quote! { ! }
			} else {
				quote! {}
			};
			quote! {
				nbnf::nom::combinator::verify(
					nbnf::nom::character::complete::anychar,
					|&char: &char| #invert match char {
						#patterns => true,
						_ => false,
					}
				)
			}
		},
		Literal::Byte(GLiteral::Char(byte)) => {
			// nom doesn't seem to have a byte equivalent of `char(_)`
			quote! {
				nbnf::nom::combinator::map_res(
					nbnf::nom::bytes::complete::take(1usize),
					|bytes: &[u8]| {
						let expected_byte = #byte;
						assert!(bytes.len() == 1, "take(1usize) unexpectedly returned {} bytes", bytes.len());
						let byte = bytes[0];
						(byte == expected_byte)
							.then_some(byte)
							.ok_or_else(|| format!("expecting 0x{expected_byte:02X} but got 0x{byte:02X}"))
					},
				)
			}
		},
		Literal::Byte(GLiteral::String(bytes)) => {
			let bytes = syn::LitByteStr::new(bytes, Span::call_site());
			quote! {
				nbnf::nom::bytes::complete::tag(#bytes.as_slice())
			}
		},
		Literal::Byte(GLiteral::Range { .. }) => {
			unimplemented!("byte range literals are not supported")
		},
	})
}

fn group_or_alternate(is_group: bool, exprs: &[Expr]) -> AResult<TokenStream> {
	ensure!(
		exprs.len() > 1,
		"encountered invalid group/alternate with less than two elements"
	);

	let mut discards = HashSet::new();
	let mut seq = quote! {};
	for (index, child) in exprs.into_iter().enumerate() {
		if !seq.is_empty() {
			seq = quote! { #seq, };
		}
		let parser = if let Expr::Discard(child) = child {
			ensure!(is_group, "found unexpected discard of alternate case");
			discards.insert(index);
			expr_body(child)?
		} else {
			expr_body(child)?
		};
		seq = quote! { #seq #parser };
	}

	if is_group {
		seq = quote! { (#seq) };
	} else {
		seq = quote! {
			nbnf::nom::branch::alt((#seq))
		};
	}

	let seq = if discards.is_empty() {
		seq
	} else {
		let mut argument = quote! {};
		let mut output = quote! {};
		for index in 0 .. exprs.len() {
			if !argument.is_empty() {
				argument = quote! { #argument, };
			}
			if discards.contains(&index) {
				argument = quote! { #argument _ };
			} else {
				if !output.is_empty() {
					output = quote! { #output, };
				}

				let param = quote::format_ident!("p{index}");
				argument = quote! { #argument #param };
				output = quote! { #output #param };
			}
		}
		quote! {
			nbnf::nom::combinator::map(#seq, |(#argument)| (#output))
		}
	};

	Ok(seq)
}

fn repeat(expr: &Expr, min: usize, max: Option<usize>) -> AResult<TokenStream> {
	let inner = expr_body(expr)?;
	Ok(match (min, max) {
		(0, Some(1)) => quote! {
			nbnf::nom::combinator::opt(#inner)
		},
		(1, Some(1)) => inner,
		(min, Some(max)) => quote! {
			nbnf::nom::multi::many_m_n(
				#min,
				#max,
				#inner,
			)
		},
		(0, None) => quote! {
			nbnf::nom::multi::many0(#inner)
		},
		(1, None) => quote! {
			nbnf::nom::multi::many1(#inner)
		},
		(min, None) => quote! {
			nbnf::nom::combinator::verify(
				nbnf::nom::multi::many0(#inner),
				|xs: Vec<_>| xs.len() >= #min,
			)
		},
	})
}

fn raw_ident(ident: &str) -> Ident {
	Ident::new_raw(ident, Span::call_site())
}

fn path(path: &str) -> AResult<Path> {
	syn::parse_str(path).context("couldn't parse Rust item path")
}

fn expand_placeholders(code: TokenStream, matcher: &impl Fn(&str) -> Option<TokenStream>) -> TokenStream {
	let mut new_code = TokenStream::new();
	let mut iter = code.into_iter().peekable();
	while let Some(tree) = iter.next() {
		match tree {
			TokenTree::Group(group) => {
				let delimiter = group.delimiter();
				let inner_code = group.stream();
				let inner_code = expand_placeholders(inner_code, matcher);
				let group = Group::new(delimiter, inner_code);
				new_code.append(group);
				continue;
			},
			TokenTree::Punct(punct) if punct.as_char() == '$' && matches!(iter.peek(), Some(TokenTree::Ident(_))) => {
				let Some(TokenTree::Ident(ident)) = iter.next() else {
					unreachable!()
				};
				let Some(replacement) = matcher(&ident.to_string()) else {
					new_code.append(punct);
					new_code.append(ident);
					continue
				};
				new_code.extend(replacement);
			},
			_ => new_code.append(tree),
		}
	}
	new_code
}

#[test]
fn test_expand_placeholders() {
	fn matcher(ident: &str) -> Option<TokenStream> {
		match ident {
			"abc" => Some(quote! { def::ghi }),
			"foo" => Some(quote! { bar(42) }),
			_ => None,
		}
	}
	
	let code = quote! { $abc::xyz };
	let code = expand_placeholders(code, &matcher);
	assert_eq!(code.to_string(), quote! { def::ghi::xyz }.to_string());
	
	let code = quote! { foo($foo) };
	let code = expand_placeholders(code, &matcher);
	assert_eq!(code.to_string(), quote! { foo(bar(42)) }.to_string());
	
	let code = quote! { foo($foo, $bar) };
	let code = expand_placeholders(code, &matcher);
	assert_eq!(code.to_string(), quote! { foo(bar(42), $bar) }.to_string());
}
