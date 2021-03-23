
## Map format

(`ALIGN[8]`)

0. (just before the alignment) 16-bit palette table size (since it may be smaller than the max 128x4 colors) in bytes
1. 16×16 chunk ptr array (only high bytes, since chunk data is `align[8]`'d)
2. 16x16 chunk bank array
3. BG palette table, up to 128 entries
4. Common tile block spec: bank, size, LE ptr

Pointer points to 1., not 0., to improve chunk access performance

Note: there are some global flags, but also map-specific flags
