use data::Val;

mod tests;

pub struct ParseResult<'a> {
    val: Val<'a>,
    rest: &'a str
}

pub fn parse(source: &str) -> Val {
    parse_symbol(source).val
}

pub fn parse_symbol(source: &str) -> ParseResult {
    let (sym, rest) = split_where(source, is_symbol_char);
    ParseResult {
        val: Val::Sym(sym),
        rest: rest
    }
}

fn count_bytes_while<F: Fn(char) -> bool>(string: &str, pred: F) -> usize {
    let mut byteIndex = 0usize;
    for c in string.chars() {
        if pred(c) {
            byteIndex += c.len_utf8();
        } else {
            break;
        }
    }
    byteIndex
}

fn take_while<F: Fn(char) -> bool>(string: &str, pred: F) -> &str {
    &string[..count_bytes_while(string, pred)]
}

fn split_where<F: Fn(char) -> bool>(string: &str, pred: F) -> (&str, &str) {
    let split = count_bytes_while(string, pred);
    (&string[..split], &string[split..])
}

fn is_symbol_char(c: char) -> bool {
    c.is_alphanumeric()
}
