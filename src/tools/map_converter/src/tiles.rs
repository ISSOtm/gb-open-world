use std::fs::File;
use std::io::{self, BufReader, Read};

#[derive(Debug)]
pub struct Tile(pub [u8; 16]);

pub fn from_file(file: File) -> io::Result<Vec<Tile>> {
    let mut input = BufReader::new(file);
    let mut buf = [0u8; 16];
    let mut tiles = Vec::new();

    loop {
        match input.read(&mut buf)? {
            // EOF
            0 => return Ok(tiles),

            // New tile
            16 => tiles.push(Tile(buf)),

            // EOF in the middle of the tile's data
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::UnexpectedEof,
                    "Tile data size is not a multiple of 16",
                ))
            }
        }
    }
}
