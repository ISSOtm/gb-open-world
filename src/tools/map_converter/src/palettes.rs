use serde::de::{self, Unexpected, Visitor};
use serde::{Deserialize, Deserializer};
use smallvec::SmallVec;
use std::fmt;
use std::fs::File;
use std::io::BufReader;

pub fn from_sfc_json(file: File) -> serde_json::Result<Vec<Palette>> {
    let sfc_pal_json: SFCPalJson = serde_json::from_reader(BufReader::new(file))?;
    let mut palettes = sfc_pal_json.palettes;

    // Pad palettes that aren't the last
    let len = palettes.len();
    for palette in palettes.iter_mut().take(len - 1) {
        while palette.0.len() < 4 {
            palette.0.push(Color {
                red: 0,
                green: 0,
                blue: 0,
            });
        }
    }

    Ok(palettes)
}

// SmallVec is used for all palette types because they may contain *up to* 4 colors

#[derive(Debug, Deserialize)]
struct SFCPalJson {
    palettes: Vec<Palette>,
    palettes_native_rgb: Vec<SmallVec<[[u8; 3]; 4]>>,
}

#[derive(Debug, Deserialize)]
pub struct Palette(pub SmallVec<[Color; 4]>);

#[derive(Debug)]
pub struct Color {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
}

impl<'de> Deserialize<'de> for Color {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_str(ColorVisitor)
    }
}

struct ColorVisitor;

impl<'de> Visitor<'de> for ColorVisitor {
    type Value = Color;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "a string containing a #rrggbb hex color")
    }

    fn visit_str<E: de::Error>(self, s: &str) -> Result<Self::Value, E> {
        let mut chars = s.chars();
        let err = || de::Error::invalid_value(Unexpected::Str(s), &self);

        // First, expect a hash
        chars.next().filter(|c| *c == '#').ok_or_else(err)?;

        // Then, expect 3 channels in hex
        let mut parse_channel = || {
            // I don't know about Rust's ordering guarantees, so let's play it safe
            let first = chars.next().and_then(|c| c.to_digit(16)).ok_or_else(err)? as u8;
            let second = chars.next().and_then(|c| c.to_digit(16)).ok_or_else(err)? as u8;

            Ok(first * 16 + second)
        };
        let red = parse_channel()?;
        let green = parse_channel()?;
        let blue = parse_channel()?;

        Ok(Color { red, green, blue })
    }
}
