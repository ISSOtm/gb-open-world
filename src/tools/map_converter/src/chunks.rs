use crate::tileset::{Metatile, MetatileEntry};
use crate::{NB_CHUNKS_H, NB_CHUNKS_W};
use dia_semver::Semver;
use smallvec::SmallVec;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::str::FromStr;
use tiled::{Layer, LayerData, LayerTile, Map, Tileset};

const CHUNK_WIDTH: u32 = 16;
const CHUNK_HEIGHT: u32 = 16;
pub const METATILES_PER_CHUNK: usize = 64;
type MetatileMap<T> = [[T; CHUNK_WIDTH as usize]; CHUNK_HEIGHT as usize];

#[derive(Debug)]
pub struct Chunk {
    pub map: MetatileMap<ChunkMapEntry>,
    pub metatile_ids: SmallVec<[u16; METATILES_PER_CHUNK]>, // IDs of the metatiles being referenced in the `map`
}

#[derive(Debug)]
pub struct ChunkMapEntry {
    pub is_dynamic: bool,      // Bit 7
    pub is_npc_walkable: bool, // Bit 6
    pub id: u8,                // Bits 0-5 (index into chunk's `metatile_ids`)
}

impl Default for ChunkMapEntry {
    fn default() -> Self {
        Self {
            is_dynamic: false,
            is_npc_walkable: false,
            id: 0,
        }
    }
}

impl From<&ChunkMapEntry> for u8 {
    fn from(entry: &ChunkMapEntry) -> Self {
        u8::from(entry.is_dynamic) << 7 | u8::from(entry.is_npc_walkable) << 6 | entry.id
    }
}

/// Which version of the TMX format we require
const TMX_REQUIRED_VER: Semver = Semver::new(1, 0, 0);

#[derive(Debug)]
pub enum ChunkGenError<'a> {
    BadTileDims(u32, u32),
    BadOrientation(tiled::Orientation),
    IncompatibleVersion(&'a str),
    NoBGLayer(Option<&'a str>),
    NoBGTileset(Option<&'a str>),
    SeveralBGLayers(usize),
    SeveralBGTilesets(usize),
    Unsupported(&'a str),
}

impl Display for ChunkGenError<'_> {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> fmt::Result {
        use ChunkGenError::*;

        match self {
            BadTileDims(w, h) => write!(fmt, "Tileset must have 16x16 tiles, not {}x{}", w, h),
            BadOrientation(orientation) => {
                use tiled::Orientation::*;

                write!(
                    fmt,
                    "Tiled map must be orthogonal, not {}",
                    match orientation {
                        Orthogonal => unreachable!(),
                        Isometric => "isometric",
                        Staggered => "staggered",
                        Hexagonal => "hexagonal",
                    }
                )
            }
            IncompatibleVersion(version) => write!(
                fmt,
                "TMX file uses version \"{}\", which is incompatible with expected {}",
                version, &TMX_REQUIRED_VER
            ),
            NoBGLayer(Some(similar_name)) => write!(fmt, "No layer found with \"BG\" as its name (but other layers with similar names exist, such as \"{}\")", similar_name),
            NoBGLayer(None) => write!(fmt, "No layer found with \"BG\" as its name"),
            NoBGTileset(Some(similar_name)) => write!(fmt, "No tileset found with \"BG\" as its name (but other layers with similar names exist, such as \"{}\")", similar_name),
            NoBGTileset(None) => write!(fmt, "No tileset found with \"BG\" as its name"),
            SeveralBGLayers(count) => write!(fmt, "Found {} layers with \"BG\" as their name", count),
            SeveralBGTilesets(count) => write!(fmt, "Found {} tilesets with \"BG\" as their name", count),
            Unsupported(msg) => write!(fmt, "Unsupported feature: {}", msg),
        }
    }
}

impl Error for ChunkGenError<'_> {}

pub fn gen_from_map<'a>(
    map: &'a Map,
    metatiles: &[Metatile],
    tile_ref_counts: &mut [u32],
) -> Result<
    (
        Vec<Chunk>,
        [[u8; NB_CHUNKS_W as usize]; NB_CHUNKS_H as usize],
    ),
    ChunkGenError<'a>,
> {
    if !Semver::from_str(&map.version)
        .expect("Either Tiled stopped using SemVer for maps, or your TMX file is invalid")
        .compatible_with(&TMX_REQUIRED_VER)
    {
        return Err(ChunkGenError::IncompatibleVersion(&map.version));
    }

    if map.orientation != tiled::Orientation::Orthogonal {
        return Err(ChunkGenError::BadOrientation(map.orientation));
    }

    let bg_layer = get_bg_layer(map)?;
    let bg_tileset = get_bg_tileset(map)?;

    let nb_metatiles = bg_tileset.tilecount.expect("BG tileset has no tilecount!?");

    if bg_tileset.tile_width != 16 || bg_tileset.tile_height != 16 {
        return Err(ChunkGenError::BadTileDims(
            bg_tileset.tile_width,
            bg_tileset.tile_height,
        ));
    }

    if bg_layer.offset_x != 0.0 || bg_layer.offset_y != 0.0 {
        return Err(ChunkGenError::Unsupported("BG layer offset must be (0, 0)"));
    }

    // Alright, everything's ready: generate the chunks from the map!

    let get_metatile_at = match &bg_layer.tiles {
        LayerData::Finite(map) => move |x: u32, y: u32| map[y as usize][x as usize],
        LayerData::Infinite(_) => {
            return Err(ChunkGenError::Unsupported(
                "Infinite maps are not supported yet",
            ));
        }
    };

    if map.width % CHUNK_WIDTH != 0 || map.height % CHUNK_HEIGHT != 0 {
        eprintln!("Warning: map dimensions ({}x{} metatiles) are not multiples of chunk dimensions ({}x{}); block #0 will be used as padding", map.width, map.height, CHUNK_WIDTH, CHUNK_HEIGHT);
    }

    let mut chunks = Vec::new();
    let mut chunk_map = [[0u8; NB_CHUNKS_W as usize]; NB_CHUNKS_H as usize];

    // Deduplicate the chunks based on the *global* metatiles that they reference
    // Especially useful for empty chunks
    let mut chunk_mappings = HashMap::new();

    for chunk_y in 0..NB_CHUNKS_H {
        for chunk_x in 0..NB_CHUNKS_W {
            // Generate the chunk's metatile map
            let metatile_map = extract_chunk_map(
                map,
                bg_tileset,
                chunk_x,
                chunk_y,
                get_metatile_at,
                nb_metatiles,
            )?;

            // Get the chunk's unique ID (post-deduplication)
            let chunk_id = if let Some(&id) = chunk_mappings.get(&metatile_map) {
                // If the chunk is already present in the hashmap, use that
                id
            } else {
                // Otherwise, create a new chunk

                // Insert the mapping into the map
                let id = chunks
                    .len()
                    .try_into()
                    .expect("Didn't expect more than 65536 metatiles in a single map");
                chunk_mappings.insert(metatile_map, id);

                // Generate the chunk and append it to the vec
                chunks.push(new_chunk(&metatile_map));

                id
            };
            // Write it down
            chunk_map[chunk_y as usize][chunk_x as usize] = chunk_id;

            // Ref count *once* all tiles referenced by the chunk just inserted
            ref_chunk_tiles(&chunks[usize::from(chunk_id)], metatiles, tile_ref_counts);
        }
    }

    Ok((chunks, chunk_map))
}

fn extract_chunk_map<'a, G: Fn(u32, u32) -> LayerTile>(
    map: &'a Map,
    bg_tileset: &Tileset,
    chunk_x: u32,
    chunk_y: u32,
    get_metatile_at: G,
    nb_metatiles: u32,
) -> Result<MetatileMap<u16>, ChunkGenError<'a>> {
    let mut metatile_map = [[0u16; CHUNK_WIDTH as usize]; CHUNK_HEIGHT as usize];
    for tile_y in 0..CHUNK_HEIGHT {
        for tile_x in 0..CHUNK_WIDTH {
            let y = chunk_y * CHUNK_HEIGHT + tile_y;
            let x = chunk_x * CHUNK_WIDTH + tile_x;

            metatile_map[tile_y as usize][tile_x as usize] = if y >= map.height || x >= map.width {
                // As mentioned by the warning above, pad OOB with block #0
                0
            } else {
                let metatile = get_metatile_at(x, y);
                if metatile.flip_h || metatile.flip_v || metatile.flip_d {
                    return Err(ChunkGenError::Unsupported("Tiles may not be flipped within Tiled; please create different metatiles, SuperFamiconv will optimize them"));
                }
                let id = metatile.gid - bg_tileset.first_gid;

                assert!(id < nb_metatiles, "Got metatile #{} (plus tileset offset {}), but tileset only reports {} metatiles!?", id, bg_tileset.first_gid, nb_metatiles);

                id.try_into()
                    .expect("Didn't expect more than 65536 metatiles in a single map")
            }
        }
    }

    Ok(metatile_map)
}

fn new_chunk(metatile_map: &MetatileMap<u16>) -> Chunk {
    let mut metatile_ids = SmallVec::new();
    let mut map: MetatileMap<ChunkMapEntry> = Default::default();

    for (y, row) in metatile_map.iter().enumerate() {
        for (x, &g_id) in row.iter().enumerate() {
            // `g_id` indexes the *g*lobal metatile table, and `metatiles` translates chunk-local IDs to global ones
            // Thus, we need to perform a reverse lookup
            // (Note that indexes into `metatiles` are actually local IDs, hence `l_id`...)
            let l_id_maybe = metatile_ids
                .iter()
                .enumerate()
                .find(|(_, &id)| id == g_id)
                .map(|(l_id, _)| l_id);

            // If the metatile hasn't been encountered yet, register it, and use its brand new ID
            let id = l_id_maybe
                .unwrap_or_else(|| {
                    let l_id = metatile_ids.len();
                    metatile_ids.push(g_id);
                    l_id
                })
                .try_into()
                .expect("There can't be more than 256 tiles in a single chunk");

            map[y][x] = ChunkMapEntry {
                is_dynamic: false,
                is_npc_walkable: false,
                id,
            };
        }
    }

    Chunk { map, metatile_ids }
}

fn ref_chunk_tiles(chunk: &Chunk, metatiles: &[Metatile], tile_ref_counts: &mut [u32]) {
    let mut refd_tiles = HashSet::new();
    let mut ref_tile = |tile: &MetatileEntry| refd_tiles.insert(tile.id);

    for &id in &chunk.metatile_ids {
        let metatile = &metatiles[usize::from(id)];
        ref_tile(&metatile.top_left);
        ref_tile(&metatile.top_right);
        ref_tile(&metatile.bottom_left);
        ref_tile(&metatile.botom_right);
    }

    for tile_id in refd_tiles {
        tile_ref_counts[usize::from(tile_id)] += 1;
    }
}

fn get_bg_layer<'a>(map: &'a Map) -> Result<&Layer, ChunkGenError<'a>> {
    // Keep only layers with exactly "BG" as their name
    let mut layers = map.layers.iter().filter(|layer| layer.name == "BG");
    let bg_layer = match layers.next() {
        None => {
            // If none are found, attempt a less strict search
            return Err(ChunkGenError::NoBGLayer(
                map.layers
                    .iter()
                    .find(|layer| layer.name.eq_ignore_ascii_case("BG"))
                    .map(|layer| layer.name.as_str()),
            ));
        }
        Some(layer) => layer,
    };
    // We found a suitable layer, check if there aren't any others
    let moar = layers.count();
    if moar != 0 {
        return Err(ChunkGenError::SeveralBGLayers(moar + 1));
    }

    Ok(bg_layer)
}

fn get_bg_tileset<'a>(map: &'a Map) -> Result<&Tileset, ChunkGenError<'a>> {
    if map.tilesets.len() == 1 {
        // If there is only one tileset, use it regardless of its name
        Ok(&map.tilesets[0])
    } else {
        // If there are several tilesets, use the one called "BG" exactly
        let mut tilesets = map.tilesets.iter().filter(|tileset| tileset.name == "BG");
        let bg_tileset = match tilesets.next() {
            None => {
                // If none are found, attempt a less strict search
                return Err(ChunkGenError::NoBGTileset(
                    map.tilesets
                        .iter()
                        .find(|tileset| tileset.name.eq_ignore_ascii_case("BG"))
                        .map(|tileset| tileset.name.as_str()),
                ));
            }
            Some(tileset) => tileset,
        };
        // We found a suitable tileset, check if there aren't any others
        let moar = tilesets.count();
        if moar != 0 {
            return Err(ChunkGenError::SeveralBGTilesets(moar + 1));
        }
        Ok(bg_tileset)
    }
}
