use super::*;
use data::*;

#[test]
fn parse_single_symbol() {
    match parse("foo") {
        Val::Sym("foo") => (),
        _ => assert!(false)
    }
}
