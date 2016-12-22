use super::*;
use data::*;

#[test]
fn parse_single_symbol() {
    assert_eq!(
        parse(String::from("foo")),
        Val::Sym(String::from("foo"))
    );
    assert_eq!(
        parse(String::from("bar")),
        Val::Sym(String::from("bar"))
    );
    assert_eq!(
        parse(String::from("+")),
        Val::Sym(String::from("+"))
    );
}

#[test]
fn parse_string_literal() {
    assert_eq!(
        parse(String::from("\"Hello, World!\"")),
        Val::Str(String::from("Hello, World!"))
    );
}

#[test]
fn parse_empty_list() {
    assert_eq!(
        parse(String::from("()")),
        Val::List(vec![])
    );
}

#[test]
fn parse_simple_list() {
    assert_eq!(
        parse(String::from("(+ a b)")),
        Val::List(vec![Val::Sym(String::from("+")), Val::Sym(String::from("a")), Val::Sym(String::from("b"))])
    );
    assert_eq!(
        parse(String::from("(+ 1.5 2)")),
        Val::List(vec![Val::Sym(String::from("+")), Val::Num(1.5), Val::Num(2.0)])
    );
}

#[test]
fn parse_nested_list() {
    assert_eq!(
        parse(String::from("((a b) c)")),
        Val::List(vec![
            Val::List(vec![
                Val::Sym(String::from("a")),
                Val::Sym(String::from("b"))
            ]),
            Val::Sym(String::from("c"))
        ])
    );
}

#[test]
fn parse_integer() {
    assert_eq!(
        parse(String::from("42")),
        Val::Num(42.0)
    );
}

#[test]
fn parse_float() {
    assert_eq!(
        parse(String::from("0.5")),
        Val::Num(0.5)
    );
}
