use super::*;
use data::*;

#[test]
fn parse_single_symbol() {
    assert_eq!(
        parse(String::from("foo")),
        Par::Sym(String::from("foo"))
    );
    assert_eq!(
        parse(String::from("bar")),
        Par::Sym(String::from("bar"))
    );
    assert_eq!(
        parse(String::from("+")),
        Par::Sym(String::from("+"))
    );
}

#[test]
fn parse_a_quoted_symbol() {
    assert_eq!(
        parse(String::from("'foo")),
        Par::Quot(String::from("foo"))
    );
    assert_eq!(
        parse(String::from("'+")),
        Par::Quot(String::from("+"))
    );
}

#[test]
fn parse_string_literal() {
    assert_eq!(
        parse(String::from("\"Hello, World!\"")),
        Par::Str(String::from("Hello, World!"))
    );
}

#[test]
fn parse_empty_list() {
    assert_eq!(
        parse(String::from("()")),
        Par::List(vec![])
    );
}

#[test]
fn parse_simple_list() {
    assert_eq!(
        parse(String::from("(+ a b)")),
        Par::List(vec![Par::Sym(String::from("+")), Par::Sym(String::from("a")), Par::Sym(String::from("b"))])
    );
    assert_eq!(
        parse(String::from("(+ 1.5 2)")),
        Par::List(vec![Par::Sym(String::from("+")), Par::Num(1.5), Par::Num(2.0)])
    );
}

#[test]
fn parse_nested_list() {
    assert_eq!(
        parse(String::from("((a b) c)")),
        Par::List(vec![
            Par::List(vec![
                Par::Sym(String::from("a")),
                Par::Sym(String::from("b"))
            ]),
            Par::Sym(String::from("c"))
        ])
    );
}

#[test]
fn parse_simple_vector() {
    assert_eq!(
        parse(String::from("[+ a 1]")),
        Par::Vec(vec![
            Par::Sym(String::from("+")),
            Par::Sym(String::from("a")),
            Par::Num(1.0)
        ])
    );
}

#[test]
fn parse_integer() {
    assert_eq!(
        parse(String::from("42")),
        Par::Num(42.0)
    );
}

#[test]
fn parse_float() {
    assert_eq!(
        parse(String::from("0.5")),
        Par::Num(0.5)
    );
}
