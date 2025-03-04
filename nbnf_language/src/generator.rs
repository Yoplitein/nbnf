use anyhow::{ensure, Context, Result as AResult};
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::Path;

use crate::parser::MapFunc;
use crate::{Expr, Grammar, Literal};

pub fn generate_parser(grammar: &Grammar) -> AResult<String> {
	let module = generate_parser_tokens(grammar)?;
	let file = syn::parse2(module)?;
	let res = prettyplease::unparse(&file);
	Ok(res)
}

pub fn generate_parser_tokens(grammar: &Grammar) -> AResult<TokenStream> {
	let mut module = quote! {
		use nom::Parser;
	};

	for (rule_name, rule) in &grammar.rules {
		let parser = expr_body(&rule.body)?;
		let rule_ident = raw_ident(rule_name);
		let output_type: syn::Type = syn::parse_str(&rule.output_type)?;
		module = quote! {
			#module

			fn #rule_ident(input: &str) -> nom::IResult<&str, #output_type> {
				let (input, output) = #parser.parse(input)?;
				Ok((input, output))
			}
		};
	}

	Ok(module)
}

fn expr_body(body: &Expr) -> AResult<TokenStream> {
	match body {
		Expr::Literal(v) => literal(v),
		Expr::Rule(rule_path) => {
			let rule_path = path(rule_path)?;
			Ok(quote! { #rule_path })
		},
		Expr::Group(exprs) | Expr::Alternate(exprs) => {
			group_or_alternate(matches!(body, Expr::Group(_)), exprs)
		},
		&Expr::Repeat { ref expr, min, max } => repeat(expr, min, max),
		Expr::Not(inner) => {
			let inner = expr_body(inner)?;
			Ok(quote! {
				nom::combinator::not(#inner)
			})
		},
		Expr::Recognize(inner) => {
			let inner = expr_body(inner)?;
			Ok(quote! {
				nom::combinator::recognize(#inner)
			})
		},
		Expr::Epsilon => Ok(quote! {
			nom::combinator::success(())
		}),
		Expr::Map {
			expr,
			func,
			mapping_code,
		} => {
			let expr = expr_body(expr)?;
			let func_ident = path(match func {
				MapFunc::Value => "nom::combinator::value",
				MapFunc::Map => "nom::combinator::map",
				MapFunc::MapOpt => "nom::combinator::map_opt",
				MapFunc::MapRes => "nom::combinator::map_res",
			})?;
			let mapping_code: syn::Expr = syn::parse_str(mapping_code)
				.context(format!("failed to parse mapping code `{mapping_code}`"))?;
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
		Literal::Char(char) => quote! {
			nom::character::complete::char(#char)
		},
		Literal::String(str) => quote! {
			nom::bytes::complete::tag(#str)
		},
		&Literal::Range {
			ref chars,
			ref ranges,
			invert,
		} => {
			let mut conditions = vec![];
			let op = if invert {
				quote! { != }
			} else {
				quote! { == }
			};
			for char in chars {
				conditions.push(quote! {
					char #op #char
				});
			}
			let op = if invert {
				quote! { ! }
			} else {
				quote! {}
			};
			for range in ranges {
				let start = range.start();
				let end = range.end();
				conditions.push(quote! {
					#op(#start ..= #end).contains(&char)
				});
			}
			let conditions = conditions
				.into_iter()
				.reduce(|l, r| {
					if invert {
						quote! {
							#l && #r
						}
					} else {
						quote! {
							#l || #r
						}
					}
				})
				.unwrap_or_else(|| {
					quote! {
						// empty range matches exactly one character
						true
					}
				});

			quote! {
				nom::combinator::verify(
					nom::character::complete::anychar,
					|&char: &char| #conditions
				)
			}
		},
	})
}

fn group_or_alternate(is_group: bool, exprs: &[Expr]) -> AResult<TokenStream> {
	ensure!(
		exprs.len() > 1,
		"encountered invalid group/alternate with less than two elements"
	);

	let mut seq = quote! {};
	for child in exprs {
		if !seq.is_empty() {
			seq = quote! { #seq, };
		}
		let parser = expr_body(child)?;
		seq = quote! { #seq #parser };
	}

	if is_group {
		seq = quote! { (#seq) };
	} else {
		seq = quote! {
			nom::branch::alt((#seq))
		};
	}

	Ok(seq)
}

fn repeat(expr: &Expr, min: usize, max: Option<usize>) -> AResult<TokenStream> {
	let inner = expr_body(expr)?;
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
		(1, None) => quote! {
			nom::multi::many1(#inner)
		},
		(min, None) => quote! {
			nom::combinator::verify(
				nom::multi::many0(#inner),
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
