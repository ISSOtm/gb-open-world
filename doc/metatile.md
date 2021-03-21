
## Metatile format

Array of 4 "tiles" (order TBD, either 1234 or 1324)

## Tile format

1. Tile ID ($C0-FF = "common", $00-3F = chunk-exclusive tiles)
2. Palette ID (index into map's 128-palette array)
3. Attrs (`OVH0 0000`, O = OBJ priority, V/H = flip)
4. Unused byte (for indexing)

Unused bits in the attr could be used for collision type?
