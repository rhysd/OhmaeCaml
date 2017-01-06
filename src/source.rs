use std::io;
use std::fs;
use std::io::Read;

use ast;

#[derive(Debug, PartialEq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub enum Location {
    File(String),
    Stdin,
}

#[derive(Debug)]
pub struct Source {
    code: String,
    pub byte_len: usize,
    pub location: Location
    // Note:
    // Add indices of newlines to convert offset into line and column
}

impl Source {
    pub fn new(s: String, l: Location) -> Source {
        Source {
            byte_len: s.bytes().len(),
            code: s,
            location: l,
        }
    }

    pub fn from_stdin() -> io::Result<Source> {
        let mut buf = String::new();
        io::stdin().read_to_string(&mut buf)?;
        Ok(Source::new(buf, Location::Stdin))
    }

    pub fn from_file(path: String) -> io::Result<Source> {
        let mut buf = String::new();
        let mut f = fs::File::open(&path)?;
        f.read_to_string(&mut buf)?;
        Ok(Source::new(buf, Location::File(path)))
    }

    pub fn code(&self) -> &str {
        self.code.as_str()
    }

    pub fn position_from_offset<G: ast::GetOffset>(&self, g: G) -> Position {
        let bytes = self.code.as_bytes();
        let offset = bytes.len() - g.get_offset();

        let mut l = 1usize;
        let mut c = 1usize;
        for b in &bytes[..offset] {
            match char::from_u32(*b as u32) {
                Some('\n') => {
                    l += 1;
                    c = 1;
                },
                Some(_) | None => {
                    c += 1;
                }
            }
        }
        Position {line: l, column: c}
    }

    pub fn source_location<G: ast::GetOffset>(&self, g: G) -> String {
        let loc = match self.location {
            Location::File(p) => p.as_str(),
            Location::Stdin => "<stdin>",
        };
        let pos = self.position_from_offset(g);
        format!("{}:{}:{}", loc, pos.line, pos.column)
    }
}
