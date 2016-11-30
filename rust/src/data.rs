pub enum Val<'a> {
    List{ car: &'a Val<'a>, cdr: &'a Val<'a> },
    Sym(&'a str),
    Num(f64),
    Str(String)
}
