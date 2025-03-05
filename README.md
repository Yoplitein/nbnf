# nbnf
A parser generator based on [nom](https://github.com/rust-bakery/nom/), with syntax inspired by EBNF and regex.

## Syntax overview
A grammar is a series of rules containing expressions. Whitespace is ignored, rules must end with a semicolon:
```ebnf
rule = ...;
rule2 =
    ...
    ...;
...
```

A rule generates a parser function as Rust code, and so its name must be a valid Rust identifier.
The output type of the generated function can be specified, defaulting to `&str` if omitted:
```ebnf
rule<Output> = ...;
```
Any valid Rust code denoting a type is permitted between the chevrons.

Expressions can invoke any parser function defined in Rust, with other rules simply being resolved as symbols in the same enclosing module:
```ebnf
top = inner external_rule nbnf::nom::combinator::eof;
inner = ...;
```

Rules can match literal chars, strings, or regex-like character ranges; and supports Rust-like escapes:
```ebnf
top = 'a' "bc" [de-g] '\x2A' "\"\0\r\n\t\x7F\u{FF}";
```

Expressions can be grouped with parentheses, and [alternated](https://docs.rs/nom/latest/nom/branch/fn.alt.html) between with slash:
```ebnf
top = ('a' 'b') / ('c' 'd');
```

Expressions can be repeated with regex-like syntax:
```ebnf
r1 = 'a'?;      // zero or one
r1 = 'b'*;      // zero or more
r2 = 'c'+;      // one or more
r3 = 'd'{2};    // exactly two
r4 = 'e'{2,};   // at least two
r5 = 'f'{,2};   // at most two
r6 = 'g'{2,4};  // between two to four
```

Expressions can be tagged with various modifiers, wrapping them in combinators:
* `!!` ([cut](https://docs.rs/nom/latest/nom/combinator/fn.cut.html)) prevents backtracking, e.g. when you know no other expressions can match
```ebnf
json_object_pair<(String, Json)> = string !!(-':' json_value);
```
* `!` ([not](https://docs.rs/nom/latest/nom/combinator/fn.not.html)) matches only when the expression does not match, consuming no input
```ebnf
ident = -![0-9] ~[a-zA-Z0-9_]+;
```
* `~` ([recognize](https://docs.rs/nom/latest/nom/combinator/fn.recognize.html)) will discard the output and instead yield the portion of the input that was matched
```ebnf
r1<(i32, f64)> = ...;
r2<&str> = ~r1;
```

Expressions can be discarded from output by prefixing them with `-`:
```ebnf
string<&str> = -'"' ~(string_char+) -'"'
```
For this particular grammar, foregoing the discards would require a tuple as the return type because the quote chars are included:
```ebnf
string<(char, &str, char)> = ...;
```

The [empty string](https://docs.rs/nom/latest/nom/combinator/fn.success.html) can be matched with `&`, allowing various interesting grammar constructs:
```ebnf
parens = ~('(' parens ')') / ~&;
```

Types and output values can be massaged in a few ways by passing any valid Rust expression:
* `@<...>` ([value](https://docs.rs/nom/latest/nom/combinator/fn.value.html)) discards output and instead returns the given literal
```ebnf
token<Token> =
    ... /
    '/'@<Token::Slash> /
    ...;
```
* `|<...>` ([map](https://docs.rs/nom/latest/nom/combinator/fn.map.html)) runs a mapping function over the output
```ebnf
object<HashMap> =
    -'{' object_pair+ -'}'
    |<HashMap::from_iter>;
```
* `|?<...>` ([map_opt](https://docs.rs/nom/latest/nom/combinator/fn.map_opt.html)) runs a mapping function returning `Option` over the output
```ebnf
even_int<i32> =
    int
    |?<|v| (v & 1 == 0).then_some(v)>;
```
* `|!<...>` ([map_res](https://docs.rs/nom/latest/nom/combinator/fn.map_res.html)) runs a mapping function returning `Result` over the output
```ebnf
number<i32> =
    ~([0-9]+)
    |!<i32::from_str>
```

## Example Usage
The main entrypoint is `nbnf::nbnf`, a proc macro that expands to parsers generated from the given grammar.
Note that the input must be passed as a string (preferably a [raw string](https://doc.rust-lang.org/reference/tokens.html#raw-string-literals),)
as certain expressions which are valid grammars are invalid Rust (e.g. the unbalanced quote in `[^"]`.)

```rust
use nbnf::nbnf;

nbnf!(r#"
    top = ~('a' top 'b') / ~&;
"#);

fn main() {
    let input = "aabbc";
    let (rest, output) = top.parse(input).unwrap();
    assert_eq!(rest, "c");
    assert_eq!(output, "aabb");
}
```
