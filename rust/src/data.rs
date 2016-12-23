#[derive(PartialEq, Debug)]
pub enum Val {
    List(Vec<Val>),
    Sym(String),
    Num(f64),
    Str(String)
}

#[derive(PartialEq, Debug)]
pub enum Par {
    List(Vec<Par>),
    Sym(String),
    Num(f64),
    Str(String),
    Vec(Vec<Par>),
    Quot(String)
}
