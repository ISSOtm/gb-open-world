
## Map format

(`ALIGN[8]`)

1. 16×16 chunk ptr array (only high bytes, since chunk data is `align[8]`'d)
2. 128 BG palette table

Note: there are some global flags, but also map-specific flags
