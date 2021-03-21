
Each folder here shall contain exactly 1 map.
In each of those, the following files are required:
- `map.tmx` ([`tiled`](https://www.mapeditor.org/) map file)
- `map.tsx` (`tiled` tileset file)
- `tileset.png` (image containing the tileset)

If any of those isn't found, `make` will complain about "No rule to make target 'res/maps/[map].asm".

Maps are stored in the ROM the order of their directories.
