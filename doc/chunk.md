
## Chunk format

(`ALIGN[8]`)

0. (just before the alignment) Size of map + metatile defs (since that may be smaller than the max 64x4x4 bytes) in bytes, plus $200, big-endian
1. 16×16 metatile map
2. Array of up to 64 metatile definitions
3. Chunk tile block spec: bank (if $00, none, skip rest of spec), size, LE ptr
4. Default dynamic metatile array (len, then data; 0 = skip)
5. Initialization routine ptr (ran last, to patch things up)

Pointer points to 1., not 0., to improve chunk access performance

## Metatile map entry format

`DWII IIII`

- D = If set, metatile is *dynamic*
- W = If set, this is walkable **by NPCs** (ignored if dynamic, see below); player uses something else for collision
- I = ID, different meaning whether static or dynamic

### Static

W is considered as-is; I is simply the metatile's ID

### Dynamic

W is ignored; I is an *index* into the dynamic metatile array.

The byte at the specified index is read, and treated as a static entry (with the D bit ignored).
This includes the W flag in the array overriding the map's entry.

(Possible future extension: AND/OR the two W flags, if needed by map layout)

## Chunk loading

Chunks, which are 256x256 pixels, are divided into four 128x128-pixel quadrants.
The 'active' chunks (those that are currently loaded) are determined by the location of the camera's center.
This is done so chunk swapping happens the farthest distance from chunk borders.

There are always 4 active chunks: the one containing the camera's quadrant (i.e. the one the camera's center pixel is in), and the 3 adjacent chunks.
This is necessary because the camera's frame can straddle 4 different quadrants.

### Chunk gfx

There are 4 chunk gfx slots:

Slot # | Address
-------+--------
0      | 0:9000
1      | 1:9000
2      | 0:9400
3      | 1:9400

Which of the 4 gfx slots a chunk's tiles get loaded in is simply determined by its position.
If it's on an even chunk map row, it gets loaded into slot 0 or 1; otherwise slot 2 or 3.
If it's on an even chunk map column, it gets loaded into slot 0 or 2; otherwise slot 1 or 3.
