
INCLUDE "defines.asm"


LOG_NB_MAP_FLAGS equ 7 ; TBD
NB_MAP_FLAGS equ 1 << LOG_NB_MAP_FLAGS

	static_assert NB_MAP_FLAGS % 8 == 0, "Must have at least 8 map flags, *duh*."


SECTION "ROM0 Map routines", ROM0

LoadMap::
	add a, a
	ld l, a
	ld h, HIGH(MapHeaders)

	ld a, BANK(MapHeaders)
	ldh [hCurROMBank], a
	ld [rROMB0], a

	; Store map pointer into currently-loaded vars
	ld a, [hli]
	ld [wMapBank], a
	ld h, [hl] ; Store while we switch banks
	ldh [hCurROMBank], a
	ld [rROMB0], a
	ld a, h
	ld [wMapPtrHigh], a

	;; Get palette map size and point to loadable data
	; The field is located 2 bytes before the "current" pointer, which is 256-aligned
	; "Current" is in quotes because the low byte wasn't set
	ld l, -1
	dec h
	; Since the base pointer is 256-aligned, the low byte gets added to 0: just load it directly
	; The high byte needs to be added to the current high byte of the pointer,
	; plus 1 to compensate the `dec h` above, plus 2 to skip the 2 chunk maps
	ld a, [hld] ; Read high byte of size
	ld l, [hl] ; Read low byte of size
	add a, h
	add a, 3
	ld h, a

	;; Load common gfx
	; Note: this assumes no transfer is currently in progress
	ld a, [hli]
	ldh [hVRAMTransferLen], a
	ld a, [hli]
	ldh [hVRAMTransferSrcBank], a
	ld a, [hli]
	ldh [hVRAMTransferSrcLow], a
	ld a, [hli]
	ldh [hVRAMTransferSrcHigh], a
	ld a, 1
	ldh [hVRAMTransferDestBank], a
	ld a, LOW(vCommonBGTiles)
	ldh [hVRAMTransferDestLow], a
	ld a, HIGH(vCommonBGTiles)
	ldh [hVRAMTransferDestHigh], a ; Make sure to write this last
	ret


RedrawScreen::
	; First, reset the BG palette streamer, since we're redrawing from scratch
	ld hl, wBGPaletteCounts
	call InitPaletteStreamer

	ld a, [wMapPtrHigh]
	ld d, a
	; Right now, D(E) points to the chunk high map

	;; Load the 4 chunks' gfx
	; First, determine which chunk we're currently looking at
	; That's easy, since every chunk is 256 pixels (just remember that we work in 12.4 fixed-point)
	; Plus, the chunk grid is 16 wide, so we can just multiply the Y coord by 16 (with `swap`)
	ld a, [wCameraYPos + 1]
	ld c, a
	ld a, [wCameraXPos + 1]
	swap a ; Divide by 16 (if you ignore the top 4 bits)
	xor c
	and $0F ; Keep the low 4 bits from A, use the upper 4 bits from C
	xor c
	ld e, a
	; We need to load 4 chunks, and this may not be the top-left one; account for that
	; For an explanation of chunk loading, see `doc/chunk.md`
	; The camera's center pixel is (80, 72) pixels away from the top-left,
	; so the coordinates we're looking for are (128 - 80; 128 - 72) = (48; 56) = ($30; $38)
	; If we're to the left of them, we're looking at the right chunk(s)
	; If we're above them, we're looking at the bottom chunk(s)
	; Fortunately, we still have `wCameraYPos + 1` in C, and we can easily compare $30.0 by just looking at the 12.4's top byte
	ld a, c
	and $0F ; Keep only bits 4-7 of the integer portion
	cp $30 >> 4
	jr nc, .topChunk
	ld a, e
	sub $10
	ld e, a
.topChunk
	; Now, check if LOW(INT(xpos)) < $38
	; First, compute LOW(INT(xpos))
	ld a, [wCameraXPos + 1]
	ld c, a
	ld a, [wCameraXPos]
	xor c
	and $F0 ; Keep upper 4 bits of A, use low 4 bits of C
	xor c
	swap a ; Put nibbles in their correct positions
	cp $38
	jr nc, .leftChunk
	dec e
.leftChunk

	; We got our pointer to the top-left chunk to load; now, load the 4 chunks!
	; Top-left chunk
	call .loadChunk
	inc e
	; Top-right chunk
	call .loadChunk
	ld a, e
	add a, $10
	ld e, a
	; Bottom-right chunk
	call .loadChunk
	dec e
	call .loadChunk

	; Chunk gfx are loaded, now draw the on-screen tiles
	; TODO
	ret


.loadChunk
	; Switch to map's ROM bank
	ld a, [wMapBank]
	ldh [hCurROMBank], a
	ld [rROMB0], a
	ld a, [de] ; Read high ptr
	ld h, a
	inc d ; Switch to bank table
	ld a, [de] ; Read bank
	dec d ; Switch back to high ptr table
	; Switch to chunk's bank
	ldh [hCurROMBank], a
	ld [rROMB0], a

	; Go to chunk tile block spec
	dec h
	ld l, -2
	ld a, [hli] ; Read high byte of post-metatile offset
	ld l, [hl] ; Read low byte of post-metatile offset (added to an ALIGN[8] address, so...)
	add a, h
	ld h, a
	; If a transfer is currently pending, wait for it to complete
	ldh a, [hVRAMTransferDestHigh]
	and a
	call nz, WaitVBlank ; Be careful to perform this check before touching any regs!!!
	ld a, [hli] ; Bank
	and a
	jr z, .noTiles
	ldh [hVRAMTransferSrcBank], a
	ld a, [hli] ; HDMA5-formatted len
	ldh [hVRAMTransferLen], a
	ld a, [hli] ; Src ptr low
	ldh [hVRAMTransferSrcLow], a
	ld a, [hli] ; Src ptr high
	ldh [hVRAMTransferSrcHigh], a
	; Compute dest
	xor a
	ldh [hVRAMTransferDestLow], a
	ld a, e ; Use bit 0 (column parity) to select dest bank
	ldh [hVRAMTransferDestBank], a ; No need to mask, VBK only accepts 1 writable bit
	and $10 ; Now, use row parity to compute dest addr
	rrca ; $08
	rrca ; $04
	or $90
	ldh [hVRAMTransferDestHigh], a ; Make sure to write this last!
.noTiles

	; Load chunk metatile array
	ld a, [hli]
	and a
	jr z, .noDynamicMetatiles
	; TODO
	rst Crash
.noDynamicMetatiles
	ret


SECTION "Map headers", ROMX,ALIGN[8]

MapHeaders:

; This is auto-generated from files in `src/res/maps`
INCLUDE "res/maps.asm"


SECTION "Map variables", WRAM0

wMapBank:
	db
wMapPtrHigh:
	db

SECTION "Camera variables", WRAM0

wCameraYPos::
	dw
wCameraXPos::
	dw

SECTION "Map flags", WRAM0,ALIGN[LOG_NB_MAP_FLAGS]

wMapFlags:
	ds NB_MAP_FLAGS / 8


tiles equs "* 16"

SECTION UNION "0:8000 tile block", VRAM

vNPCTiles1stHalf:
	ds $80 tiles

SECTION UNION "0:8800 tile block", VRAM

vNPCTiles2ndHalf:
	ds $80 tiles

SECTION UNION "1:8000 tile block", VRAM

vPlayerTiles:
	ds $80 tiles

SECTION UNION "1:8800 tile block", VRAM

vUITiles:
	ds $40 tiles

vCommonBGTiles:
	ds $40 tiles

SECTION UNION "0:9000 tile block", VRAM

vBGTiles0: ds $40 tiles
vBGTiles2: ds $40 tiles

SECTION UNION "1:9000 tile block", VRAM

vBGTiles1: ds $40 tiles
vBGTiles3: ds $40 tiles
