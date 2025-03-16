# nbnf
A parser generator based on [nom](https://github.com/rust-bakery/nom/), with syntax inspired by EBNF and regex.

## Syntax overview
A grammar is a series of rules containing expressions. Whitespace is insignificant, C-like comments with nesting are allowed, rules must end with a semicolon:
```ebnf
// foo
rule = ...; // bar
rule2 =
    /* /*
        baz
        qux
    */ */
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

The input type can be specified as well, also defaulting to `&str`, but requires output to also be specified:
```ebnf
rule<Input><Output> = ...;
```

Expressions can invoke any parser function defined in Rust, with other rules simply being resolved as symbols in the same enclosing module:
```ebnf
top = inner external_rule nbnf::nom::combinator::eof;
inner = ...;
```

A literal Rust expression can also be inserted, e.g. to invoke parametric parsers:
```ebnf
two_chars = <nbnf::nom::bytes::complete::take(2usize)>;
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
* `||<...>` (no corresponding nom combinator) wraps the expression in arbitrary Rust code, which should contain a placeholder `$expr` (explained below)
```ebnf
comma = ",";
pairs =
    ("foo" "bar")
    ||<nbnf::nom::multi::separated_list1(comma, $expr)>;
```

Certain behavior can be modified with pragma directives:
* `#input <ty>` allows specifying the default input type of all following rules
```ebnf
#input <&[u8]>
binary_rule<()> = b"foo"@<()>;
```
* `#output <ty>` similarly allows specifying the default output type
* `#error <ty>` allows setting the error type passed to `IResult`, e.g. to use [VerboseError](https://docs.rs/nom-language/latest/nom_language/error/struct.VerboseError.html)
```ebnf
// note that the type should not include generics, the input type is substituted per-rule
#error <nom_language::error::VerboseError>

rule = ...;
// generates `fn rule(input: &str) -> IResult<&str, &str, VerboseError<&str>>`
```
* `#placeholder <name> <expr>` allows defining new placeholders (explained below), and overriding those built into nbnf
```ebnf
#placeholder myparsers my_lib::parsers
rule = $myparsers::parser;
```

Each pragma also allows clearing user-defined values:
```ebnf
// default input type is reset to `&str`
#input $reset
// likewise for `#output`/`#error`

// placeholder `foo` is reset (to default, if any)
#placeholder foo $reset

// all user-defined placeholders are reset
#placeholder $reset
```

Placeholders are syntax that allow arbitrary substitutions. nbnf has a few predefined placeholders that can be overridden to alter generated parsers:
* `$nom` defaults to `nbnf::nom`, and is used by the generator to qualify foundational parsers. Overriding can be used to e.g. swap nom out for [winnow](https://crates.io/crates/winnow)
```ebnf
#placeholder nom winnow
// subsequent parsers now use winnow
```
* `$complete_or_streaming` defaults to `complete` and is used by the generator to qualify foundational parsers that come in complete or streaming variants (see [nom docs](https://docs.rs/nom/latest/nom/#streaming--complete) for more info)
* `$expr` is only defined in the wrapping code of wrap syntax (`||<...>`) and expands to the expression being wrapped
```ebnf
rule = inner||<foo($expr)>; // expands to `foo(inner)`
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
