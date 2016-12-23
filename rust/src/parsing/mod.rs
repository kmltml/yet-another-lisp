use data::Val;
use data::Par;
use regex::Regex;

mod tests;

pub struct ParseResult {
    val: Par,
    rest: String
}

pub fn parse(source: String) -> Par {
    parse_expression(source).expect("No expression found").val
}

pub fn parse_expression(source: String) -> Option<ParseResult> {
    let source = skip_whitespace(source);
    source.chars().next().map( |lookahead| 
        match lookahead {
            c if c.is_numeric() => parse_number(source),
            c if is_symbol_char(&c) => parse_symbol(source),
            '"' => parse_string(source),
            '\'' => parse_quoted_symbol(source),
            '[' => parse_vector(source),
            '(' => parse_list(source),
            _ => panic!("Wrong input received!")
        }
    )
}

pub fn parse_number(source: String) -> ParseResult {
    let regex = Regex::new(r"^((\d+)(\.\d+)?)(.*)").unwrap();
    let m = regex.captures_iter(&source).next().unwrap();
    let num = m.at(1).unwrap().parse::<f64>().unwrap();
    let rest = String::from(m.at(4).unwrap());
    ParseResult {
        val: Par::Num(num),
        rest: rest
    }
}

pub fn parse_symbol(source: String) -> ParseResult {
    let sym = source.chars().take_while(is_symbol_char).collect();
    let rest = source.chars().skip_while(is_symbol_char).collect();
    ParseResult {
        val: Par::Sym(sym),
        rest: rest
    }
}

pub fn parse_quoted_symbol(source: String) -> ParseResult {
    let res = parse_symbol(source.chars().skip(1).collect());
    if let Par::Sym(s) = res.val {
        ParseResult {
            val: Par::Quot(s),
            rest: res.rest
        }
    } else {
        panic!("Should not be reachable");
    }
}

pub fn parse_string(source: String) -> ParseResult {
    let string = source.chars().skip(1).take_while(|c| *c != '"').collect();
    let rest = source.chars().skip(1).skip_while(|c| *c != '"').skip(1).collect();
    ParseResult {
        val: Par::Str(string),
        rest: rest
    }
}

pub fn parse_list(source: String) -> ParseResult {
    let mut source: String = source.chars().skip(1).collect();
    let mut vals: Vec<Par> = vec![];
    loop {
        let lookahead = source.chars().next().expect("WRONG!");
        match lookahead {
            ')' => return ParseResult {
                val: Par::List(vals),
                rest: source.chars().skip(1).collect()
            },
            _ => {
                let res = parse_expression(source).expect("WRONG!");
                source = res.rest;
                vals.push(res.val);
            }
        }
    }
}

// TODO deduplicate parse_list and parse_vector
pub fn parse_vector(source: String) -> ParseResult {
    let mut source: String = source.chars().skip(1).collect();
    let mut vals: Vec<Par> = vec![];
    loop {
        let lookahead = source.chars().next().expect("WRONG!");
        match lookahead {
            ']' => return ParseResult {
                val: Par::Vec(vals),
                rest: source.chars().skip(1).collect()
            },
            _ => {
                let res = parse_expression(source).expect("WRONG!");
                source = res.rest;
                vals.push(res.val);
            }
        }
    }
}

fn is_symbol_char(c: &char) -> bool {
    c.is_alphanumeric() || "+-=_$*/\\:^!@#%&?<>".contains(*c)
}

fn skip_whitespace(source: String) -> String {
    source.chars().skip_while(|c| c.is_whitespace()).collect()
}
