use data::Val;
use regex::Regex;

mod tests;

pub struct ParseResult {
    val: Val,
    rest: String
}

pub fn parse(source: String) -> Val {
    parse_expression(source).expect("No expression found").val
}

pub fn parse_expression(source: String) -> Option<ParseResult> {
    let source = skip_whitespace(source);
    source.chars().next().map( |lookahead| 
        match lookahead {
            c if c.is_numeric() => parse_number(source),
            c if is_symbol_char(&c) => parse_symbol(source),
            '"' => parse_string(source),
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
        val: Val::Num(num),
        rest: rest
    }
}

pub fn parse_symbol(source: String) -> ParseResult {
    let sym = source.chars().take_while(is_symbol_char).collect();
    let rest = source.chars().skip_while(is_symbol_char).collect();
    ParseResult {
        val: Val::Sym(sym),
        rest: rest
    }
}

pub fn parse_string(source: String) -> ParseResult {
    let string = source.chars().skip(1).take_while(|c| *c != '"').collect();
    let rest = source.chars().skip(1).skip_while(|c| *c != '"').skip(1).collect();
    ParseResult {
        val: Val::Str(string),
        rest: rest
    }
}

pub fn parse_list(source: String) -> ParseResult {
    let mut source: String = source.chars().skip(1).collect();
    let mut vals: Vec<Val> = vec![];
    loop {
        let lookahead = source.chars().next().expect("WRONG!");
        match lookahead {
            ')' => return ParseResult {
                val: Val::List(vals),
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
