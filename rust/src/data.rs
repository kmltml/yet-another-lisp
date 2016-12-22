#[derive(PartialEq, Debug)]
pub enum Val {
    List(Vec<Val>),
    Sym(String),
    Num(f64),
    Str(String)
}

