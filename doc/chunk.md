
## Chunk format

(`ALIGN[8]`)

1. 16×16 metatile map
2. Array of 64 metatile definitions
3. Default dynamic metatile array (len, then data; 0 = skip)
4. Initialization routine ptr (ran last, to patch things up)

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
