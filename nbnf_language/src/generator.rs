use std::collections::HashMap;

use anyhow::{Result as AResult, ensure};
use proc_macro2::{Group, Ident, Span, TokenStream, TokenTree};
use quote::{TokenStreamExt, quote};

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
	let default_placeholders = Placeholders(HashMap::from_iter([
		("nom".into(), quote! { nbnf::nom }),
		("complete_or_streaming".into(), quote! { complete }),
	]));

	let top_rule = grammar.rules.get(&grammar.top_rule).unwrap();
	let nom_path = if let Some(v) = top_rule.placeholders.get("nom") {
		v
	} else {
		default_placeholders.0.get("nom").unwrap()
	};
	let mut module = quote! { use #nom_path::Parser as _; };

	for rule_name in &grammar.rule_order {
		let rule = grammar
			.rules
			.get(rule_name)
			.unwrap_or_else(|| unreachable!());
		let parser = expr_body(&rule.body)?;
		let rule_ident = raw_ident(rule_name);
		let input_type = &rule.input_type;
		let output_type = &rule.output_type;
		let error_type = if let Some(error_type) = &rule.error_type {
			quote! {
				, #error_type<#input_type>
			}
		} else {
			quote! {}
		};

		let parser = quote! {
			fn #rule_ident(input: #input_type) -> $nom::IResult<#input_type, #output_type #error_type> {
				#parser.parse(input)
			}
		};
		let mut placeholders = default_placeholders.clone();
		placeholders.0.extend(
			rule.placeholders
				.iter()
				.map(|(name, expr)| (name.clone(), expr.clone())),
		);
		let parser = expand_placeholders(parser, &placeholders);
		module = quote! { #module #parser };
	}

	Ok(module)
}

fn expr_body(body: &Expr) -> AResult<TokenStream> {
	match body {
		Expr::Literal(v) => literal(v),
		Expr::Rule(code) => Ok(code.clone()),
		Expr::Group(exprs) => group(exprs),
		Expr::Alternate(exprs) => alternate(exprs),
		&Expr::Repeat { ref expr, min, max } => repeat(expr, min, max),
		Expr::Discard(_) => {
			// discards must be handled by `group``
			panic!("got unexpected Discard when generating expression body");
		},
		Expr::Not(inner) => {
			let inner = expr_body(inner)?;
			Ok(quote! {
				$nom::combinator::not(#inner)
			})
		},
		Expr::Cut(inner) => {
			let inner = expr_body(inner)?;
			Ok(quote! {
				$nom::combinator::cut(#inner)
			})
		},
		Expr::Recognize(inner) => {
			let inner = expr_body(inner)?;
			Ok(quote! {
				$nom::combinator::recognize(#inner)
			})
		},
		Expr::Epsilon => Ok(quote! {
			$nom::combinator::success(())
		}),
		Expr::Wrap { expr, wrapper } => {
			let expr = expr_body(expr)?;
			let placeholders = Placeholders(HashMap::from_iter([("expr".into(), expr)]));
			let expanded = expand_placeholders(wrapper.clone(), &placeholders);
			Ok(expanded)
		},
	}
}

fn literal(literal: &Literal) -> AResult<TokenStream> {
	Ok(match literal {
		Literal::Char(GLiteral::Char(char)) => quote! {
			$nom::character::$complete_or_streaming::char(#char)
		},
		Literal::Char(GLiteral::String(str)) => quote! {
			$nom::bytes::$complete_or_streaming::tag(#str)
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
				$nom::combinator::verify(
					$nom::character::$complete_or_streaming::anychar,
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
				$nom::combinator::map_res(
					$nom::bytes::$complete_or_streaming::take(1usize),
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
				$nom::bytes::$complete_or_streaming::tag(#bytes.as_slice())
			}
		},
		Literal::Byte(GLiteral::Range { .. }) => {
			unimplemented!("byte range literals are not supported")
		},
	})
}

fn group(exprs: &[Expr]) -> AResult<TokenStream> {
	ensure!(
		exprs.len() > 1,
		"encountered invalid group with less than two elements"
	);
	
	let mut body = quote! {};
	let mut outputs = quote! {};
	for (index, expr) in exprs.iter().enumerate() {
		let (expr, discard) = match expr {
			Expr::Discard(expr) => (expr.as_ref(), true),
			_ => (expr, false),
		};
		let expr = expr_body(expr)?;
		
		let output_name = if discard {
			"_"
		} else {
			&format!("out{index}")
		};
		let output_name = Ident::new(output_name, Span::call_site());
		
		body = quote! {
			#body
			let (input, #output_name) = #expr.parse(input)?;
		};
		
		if discard {
			continue;
		}
		if !outputs.is_empty() {
			outputs = quote! { #outputs, };
		}
		outputs = quote! { #outputs #output_name };
	}
	
	body = quote! {
		(|input| {
			#body
			Ok((input, (#outputs)))
		})
	};
	Ok(body)
}

fn alternate(exprs: &[Expr]) -> AResult<TokenStream> {
	ensure!(
		exprs.len() > 1,
		"encountered invalid alternate with less than two elements"
	);
	let mut seq = quote! {};
	for child in exprs {
		if !seq.is_empty() {
			seq = quote! { #seq, };
		}
		let parser = expr_body(child)?;
		seq = quote! { #seq |input| #parser.parse(input) };
	}
	seq = quote! {
		$nom::branch::alt([#seq])
	};
	Ok(seq)
}

fn repeat(expr: &Expr, min: usize, max: Option<usize>) -> AResult<TokenStream> {
	let inner = expr_body(expr)?;
	Ok(match (min, max) {
		(0, Some(1)) => quote! {
			$nom::combinator::opt(#inner)
		},
		(1, Some(1)) => inner,
		(min, Some(max)) => quote! {
			$nom::multi::many_m_n(
				#min,
				#max,
				#inner,
			)
		},
		(0, None) => quote! {
			$nom::multi::many0(#inner)
		},
		(1, None) => quote! {
			$nom::multi::many1(#inner)
		},
		(min, None) => quote! {
			$nom::combinator::verify(
				$nom::multi::many0(#inner),
				|xs: Vec<_>| xs.len() >= #min,
			)
		},
	})
}

fn raw_ident(ident: &str) -> Ident {
	Ident::new_raw(ident, Span::call_site())
}

#[derive(Clone)]
struct Placeholders(HashMap<String, TokenStream>);

fn expand_placeholders(code: TokenStream, placeholders: &Placeholders) -> TokenStream {
	let mut new_code = TokenStream::new();
	let mut iter = code.into_iter().peekable();
	while let Some(tree) = iter.next() {
		match tree {
			TokenTree::Group(group) => {
				let delimiter = group.delimiter();
				let inner_code = group.stream();
				let inner_code = expand_placeholders(inner_code, placeholders);
				let group = Group::new(delimiter, inner_code);
				new_code.append(group);
				continue;
			},
			TokenTree::Punct(punct)
				if punct.as_char() == '$' && matches!(iter.peek(), Some(TokenTree::Ident(_))) =>
			{
				let Some(TokenTree::Ident(ident)) = iter.next() else {
					unreachable!()
				};
				let Some(replacement) = placeholders.0.get(&ident.to_string()) else {
					new_code.append(punct);
					new_code.append(ident);
					continue;
				};
				new_code = quote! { #new_code #replacement };
			},
			_ => new_code.append(tree),
		}
	}
	new_code
}

#[test]
fn test_expand_placeholders() {
	let placeholders = &Placeholders(HashMap::from_iter([
		("abc".into(), quote! { def::ghi }),
		("foo".into(), quote! { bar(42) }),
	]));

	let code = quote! { $abc::xyz };
	let code = expand_placeholders(code, placeholders);
	assert_eq!(code.to_string(), quote! { def::ghi::xyz }.to_string());

	let code = quote! { foo($foo) };
	let code = expand_placeholders(code, placeholders);
	assert_eq!(code.to_string(), quote! { foo(bar(42)) }.to_string());

	let code = quote! { foo($foo, $bar) };
	let code = expand_placeholders(code, placeholders);
	assert_eq!(code.to_string(), quote! { foo(bar(42), $bar) }.to_string());
}
