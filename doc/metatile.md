
## Metatile format

Array of 4 "tiles" (top-left, top-right, bottom-left, bottom-right)

## Tile format

1. Palette ID (index into map's 128-palette array)
2. Tile ID (see below)
3. Attrs (`OVH0 0000`, O = OBJ priority, V/H = flip)
4. Unused byte (for indexing)

Unused bits in the attr could be used for collision type?

### Tile ID translation

In theory:
- $C0-FF = "common"
- $00-3F = chunk-exclusive tiles

In practice:
- If bit 7 set, used as-is (covers "common" tiles)
- Otherwise, bit 6 is ignored, and will be replaced with the chunk's block offset
