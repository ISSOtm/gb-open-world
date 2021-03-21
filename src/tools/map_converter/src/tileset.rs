use std::fs::File;
use std::io::{self, BufReader, Read};

#[derive(Debug)]
pub struct Metatile {
    pub top_left: MetatileEntry,
    pub top_right: MetatileEntry,
    pub bottom_left: MetatileEntry,
    pub botom_right: MetatileEntry,
}

#[derive(Debug)]
pub struct MetatileEntry {
    pub id: u16,
    pub xflip: bool,
    pub yflip: bool,
    pub pal_id: u8,
}

pub fn from_files(
    map_file: File,
    pal_map_file: File,
    tileset_width: usize,
) -> io::Result<Vec<Metatile>> {
    let mut map = Vec::new();
    let nb_bytes = BufReader::new(map_file).read_to_end(&mut map)?;
    if nb_bytes % 2 != 0 {
        return Err(io::Error::new(
            io::ErrorKind::UnexpectedEof,
            "expected tileset map file to contain an even amount of bytes",
        ));
    }
    let nb_tiles = nb_bytes / 2;

    let mut pal_map = Vec::with_capacity(nb_bytes); // One u16 palette ID per tile
    let nb_map_bytes = BufReader::new(pal_map_file).read_to_end(&mut pal_map)?;
    if nb_map_bytes != nb_bytes {
        return Err(io::Error::new(
            io::ErrorKind::UnexpectedEof,
            format!(
                "expected tileset map and palette map files to have equal sizes ({} != {})",
                nb_map_bytes, nb_bytes
            ),
        ));
    }

    // Create metatiles
    // The tileset width is known to be even, so this ensures that `nb_tiles` is divisible by 4
    if nb_tiles % (tileset_width * 2) != 0 {
        return Err(io::Error::new(
            io::ErrorKind::UnexpectedEof,
            format!(
                "tileset map file must contain an even amount of rows of {} tiles (got {} tiles)",
                tileset_width, nb_tiles
            ),
        ));
    }

    let tile_at_index = |i| {
        let pal16 = pal_map[i * 2] as u16 + ((pal_map[i * 2 + 1] as u16) << 8);
        assert!(
            pal16 < 128,
            "Each map can only contain 128 palettes, {} is too many",
            pal16
        );

        MetatileEntry {
            id: map[i * 2] as u16 + (((map[i * 2 + 1] >> 3 & 1u8) as u16) << 8),
            xflip: map[i * 2 + 1] & 0x20 != 0,
            yflip: map[i * 2 + 1] & 0x40 != 0,
            pal_id: pal_map[i * 2],
        }
    };

    // There are 4 tiles per metatile, so nb_metatiles = nb_tiles / 4
    Ok((0..(nb_tiles / 4))
        .map(|i| {
            let i = ((i * 2) / tileset_width) * 2 * tileset_width + (i * 2) % tileset_width;
            Metatile {
                top_left: tile_at_index(i),
                top_right: tile_at_index(i + 1),
                bottom_left: tile_at_index(i + tileset_width),
                botom_right: tile_at_index(i + tileset_width + 1),
            }
        })
        .collect())
}
