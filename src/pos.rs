#[derive(Debug, Clone, Copy)]
pub struct Pos {
    pub line: u32,
    pub start: u32,
    pub end: u32,
}

impl Pos {
    pub fn new(row: u32, start: u32, end: u32) -> Self {
        Self {
            line: row,
            start,
            end,
        }
    }
    pub fn zero() -> Self {
        Self {
            line: 0,
            start: 0,
            end: 0,
        }
    }

    // pub fn from_lexer(lexer: &Lexer) -> Self {
    //     Self {
    //         line: lexer.current,
    //         start: lexer.pos.start,
    //         end: lexer.pos.end,
    //     }
    // }
}
