use std::{collections::HashSet, fmt::Write};

use anyhow::{ensure, Result as AResult};

use crate::{Grammar, Literal, Rule};

struct Function {
	body: String,
	output_index: usize,
}

impl Function {
	fn new() -> Self {
		Self {
			body: String::new(),
			output_index: 0,
		}
	}
	
	fn add_output(&mut self, rhs: &str) -> AResult<()> {
		let index = self.output_index;
		self.output_index += 1;
		self.body.write_fmt(format_args!(
			"let (input, out{index}) = {rhs}.parse(input)?;\n"
		))?;
		Ok(())
	}
	
	fn finish(self, rule_name: &str) -> AResult<String> {
		let Self { mut body, output_index: num_outputs } = self;
		assert!(num_outputs > 0);
		
		let mut return_type = String::new();
		let mut return_expr = String::new();
		let mut single_output = true;
		for output_index in 0 .. num_outputs {
			if !return_type.is_empty() {
				return_type.write_str(", ")?;
				return_expr.write_str(", ")?;
				single_output = false;
			}
			// TODO: real types
			return_type.write_str("&str")?;
			
			return_expr.write_fmt(format_args!("out{output_index}"))?;
		}
		if !single_output {
			return_type = format!("({return_type})");
			return_expr = format!("({return_expr})");
		}
		
		body = format!(
			"fn {rule_name}(input: &str) -> \
			nom::IResult<&str, {return_type}> {{\n\
				{body}\n\
				Ok((input, {return_expr}))\n
			}}\n"
		);
		Ok(body)
	}
}

pub fn generate_parser(grammar: &Grammar) -> AResult<String> {
	let mut res = String::new();
	for (rule_name, rule) in &grammar.rules {
		/* res.write_fmt(format_args!(
			"fn {rule_name}(input: &str) -> nom::IResult<&str, &str> {{"
		))?;
		res.write_str("}")?; */
		
		let mut function = Function::new();
		rule_top(&mut function, rule)?;
		let function = function.finish(rule_name)?;
		res.write_str(&function)?;
	}
	Ok(res)
}

fn rule_top(function: &mut Function, rule: &Rule) -> AResult<()> {
	let parser = rule_body(function, rule)?;
	// TODO: figure out subparser outputs
	function.add_output(&format!("nom::combinator::recognize({parser})"))?;
	Ok(())
}

fn rule_body(function: &mut Function, rule: &Rule) -> AResult<String> {
	match rule {
		Rule::Literal(v) => literal(function, v),
		Rule::Rule(rule_name) => Ok(rule_name.clone()),
		Rule::Group(rules) | Rule::Alternate(rules) => {
			ensure!(
				rules.len() > 1,
				"encountered invalid group/alternate \
				with less than two elements"
			);
			
			let mut seq = String::new();
			for child in rules {
				if !seq.is_empty() {
					seq.write_str(", ")?;
				}
				rule_body(function, child)?;
			}
			
			let func_prefix = if matches!(rule, Rule::Group(_)) {
				""
			} else {
				"nom::branch::alt"
			};
			seq = format!("{func_prefix}({seq})");
			
			Ok(seq)
		},
		#[cfg(none)]
		Rule::Alternate(rules) => {
			todo!()
		},
		&Rule::Repeat { ref rule, min, max } => repeat(function, rule, min, max),
	}
}

fn literal(function: &mut Function, literal: &Literal) -> AResult<String> {
	Ok(match literal {
		Literal::Char(char) => {
			todo!()
		},
		Literal::String(str) => {
			format!("nom::bytes::complete::tag({str:?})")
		},
		Literal::Range { chars, ranges } => {
			todo!()
		},
	})
}

fn repeat(function: &mut Function, rule: &Rule, min: usize, max: Option<usize>) -> AResult<String> {
	todo!()
}
