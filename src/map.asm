
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
	ldh [hVRAMTransferSrcBank], a
	ld a, [hli]
	ldh [hVRAMTransferLen], a
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
	ld b, a ; Also save the camera's current chunk for the drawing loop
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

	;; Chunk gfx are loaded, now draw the on-screen tiles
	; This is made especially tricky by the fact that up to 4 different chunks may be in the camera's frame
	; However, since chunks span a full VRAM tilemap, the chunk transitions actually occur
	; at the same time as wrapping around the tilemap! This helps alleviate the register pressure.
	; However, we also have to account for when the camera cuts a metatile in half...
	; In theory, we could just draw whole metatiles, causing just a little bit of overdraw;
	; in practice, this isn't viable because it would over-reference palettes.

	PUSHS
SECTION UNION "Scratch buffer", HRAM
hTLChunk: ds 2 ; For each, high byte then bank
hTRChunk: ds 2
hBLChunk: ds 2
hBRChunk: ds 2
hRowDrawCnt: db ; How many tiles left to draw in this row
	POPS

	; Cache chunk pointers into HRAM so that it doesn't
	ld l, b ; Restore which chunk we're reading from (might have been changed by horiz wrapping)
	ld h, d
	ld c, LOW(hTLChunk)
	call .copyChunksPtr
	ld a, b ; Again, but with next row
	add a, 16
	ld l, a
	call .copyChunksPtr

	; Compute VRAM dest addr
	; Positions can be thought of like this:
	; cccc mmmm  tppp ssss
	; |||| ||||  |||| ++++- Subpixels
	; |||| ||||  |+++------ Pixels
	; |||| ||||  +--------- Tile (within metatile)
	; |||| ++++------------ Metatile
	; ++++----------------- Chunk
	; For VRAM positions, we care about the metatile+tile position, which are on two separate bytes
	; However, only the topmost bit of the lower byte is needed, so we can just transfer it via carry
	ld a, [wCameraYPos]
	add a, a
	ld a, [wCameraYPos + 1]
	rla ; Shift in that bit
	rrca ; Rotate bits in place for "merging"
	rrca
	rrca
	ld d, a ; Store for merging
	ld a, [wCameraXPos] ; Similar trick
	add a, a
	ld a, [wCameraXPos + 1]
	rla
	xor d
	and $1F ; Keep $1F from A, $E0 from H
	xor d
	ld e, a
	ld a, d
	and $03 ; Only keep relevant bits
	or HIGH(_SCRN0)
	ld d, a

	; Compute low byte of ptr from camera pos (see VRAM dest addr comment above)
	ld a, [wCameraYPos + 1]
	swap a ; Multiply Y pos by 16, since chunks are 16 metatiles wide
	ld l, a
	ld a, [wCameraXPos + 1]
	xor l
	and $0F ; Keep $0F from X pos, and $F0 from Y pos
	xor l
	ld l, a

	; The draw loop proper
	lb bc, SCRN_Y_B + 1, \ ; Count 1 extra row in case the camera is misaligned
		LOW(hTLChunk)
.drawRow
	push bc
	ldh a, [c] ; Read high ptr
	ld h, a
	inc c
	ldh a, [c] ; Read bank and switch to it
	ldh [hCurROMBank], a
	ld [rROMB0], a

	; Draw the row
	ld a, SCRN_X_B + 1 ; Count 1 extra column in case the camera is misaligned
.drawTile
	ldh [hRowDrawCnt], a
	ld a, [hl] ; Read target metatile within chunk
	push hl ; Save metatile read ptr
	; TODO: if metatile is dynamic, deref the array

	inc h ; Switch to metatile def array
	swap a ; Multiply metatile ID by 16 (size of a metatile entry)
	ld l, a ; Save low byte for later
	and $03 ; Keep upper 2 bits
	add a, h ; Add them to high byte of base ptr (low byte is computed from 0 so can't overflow)
	ld h, a
	ld a, l
	and $F0 ; Keep lower 4 bits of metatile ID
	bit 5, e ; If on an odd row, use entry #2 or #3
	jr z, .writeEvenRow
	add a, 8
.writeEvenRow
	bit 0, e ; If on an odd column, use entry #1 or #3
	jr z, .writeEvenColumn
	add a, 4
.writeEvenColumn
	ld l, a

	ld a, [hli] ; Read palette ID
	; TODO: ref palette, possibly load it, and translate it to the corresponding hardware palette
	; For now, we translate palette IDs 1:1 to hardware palette IDs
	ld b, a
	; By default, set VRA1 attribute bit to 1
	set OAMB_BANK1, b

	xor a
	ldh [rVBK], a
:
	ldh a, [rSTAT]
	and STATF_BUSY
	jr nz, :- ; From that point on, we have 16 cycles to write to VRAM
	ld a, [hli] ; Read tile ID
	bit 7, a ; Check if tile is in "common" block
	jr nz, .commonTile ; If so, no translation needs to be performed
	; Tile may either be in block 0/1 (even chunk row, no offset) or block 2/3 (odd chunk row, set bit 6)
	; Check if within block 0/2, if so clear VRA1 bit
	; TODO
.commonTile
	ld [de], a

	; Write attribute
	ld a, 1
	ldh [rVBK], a
:
	ldh a, [rSTAT]
	and STATF_BUSY
	jr nz, :- ; From that point on, we have 16 cycles to write to VRAM
	ld a, [hli]
	xor b
	and $E0
	xor b
	ld [de], a

	pop hl ; Get back metatile read ptr
	; If we just wrote a metatile's rightmost tile, switch to the next one
	bit 0, e
	jr z, .wroteLeftTile
	inc l ; inc hl
.wroteLeftTile

	inc e ; Go to next tile
	; Check for and handle wrapping
	ld a, e
	and $1F
	call z, .wrapHoriz
	ldh a, [hRowDrawCnt]
	dec a
	jr nz, .drawTile
	pop bc ; Get back row counter & chunk ptr read ptr

	; Go to next input row: move back as much as we've advanced, but stay on the same row
	ld a, l
	sub SCRN_X_B / 2 ; = floor((SCRN_X_B + 1) / 2)
	xor l
	and $0F ; Keep $0F (horiz pos) from A, keep $F0 (vert pos) from L
	xor l
	bit 5, e ; Don't go to next input row if on an even tile row
	jr z, .noNextInputRow
	add a, 16 ; Go to next row (can't overflow)
.noNextInputRow
	ld l, a
	; Go to next output row as well
	ld a, e
	sub SCRN_X_B + 1
	xor e
	and $1F ; Keep $1F (horiz pos) from A, keep $E0 (vert pos) from E
	xor e
	add a, SCRN_VX_B ; Go to next row
	ld e, a
	adc a, d
	sub e
	ld d, a

	; Handle wrapping (which implies changing chunks)
	and $9B ; If we reached $9C00 tilemap (i.e. "below" $9800 tilemap), wrap back to top of $9800
	cp d ; Check if the wrapping did occur
	jr z, .noVertWrap
	ld d, a ; Apply the wrapping
	; Src addr wraps on its own (the metatile map is 256 bytes, so 8-bit ops implicitly wrap)
	ld c, LOW(hBLChunk) ; Switch to bottom two chunks
.noVertWrap

	dec b
	jp nz, .drawRow ; FIXME: optimize loop?

	; FIXME: For now, palette referencing isn't implemented, so hardcode-load the palettes
	ld a, [wMapBank]
	ldh [hCurROMBank], a
	ld [rROMB0], a
	ld a, [wMapPtrHigh]
	add a, 2 ; Go to map palette table
	ld d, a
	ld e, 0
	ld hl, wFadeSteps
	xor a
	ld [hli], a ; wFadeSteps
	ld [hli], a ; wFadeDelta
	ld a, $80
	ld [hli], a ; wFadeAmount
	ld a, $FF
	ld [hli], a ; wBGPaletteMask
	ld c, 8 * 4 * 3
	rst MemcpySmall
	ld [hl], 0 ; wOBJPaletteMask
	ret


.wrapHoriz
	; Go up 1 row, to compensate the offset intorduced by wrapping
	ld a, e
	sub $20
	ld e, a
	; Do the same to the src ptr low byte
	ld a, l
	sub $10
	ld l, a
	; Switch source to next chunk
	inc c
	ldh a, [c] ; Read ptr high
	ld h, a
	inc c
	ldh a, [c] ; Read bank and switch to it
	ldh [hCurROMBank], a
	ld [rROMB0], a
	ret


; Calling code relies on B being preserved
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

	; TODO: other attributes (init func?)
	ret


.copyChunksPtr
	ld e, 2
.copyChunkPtr
	ld a, [hl] ; Copy high ptr
	ldh [c], a
	inc c
	inc h
	ld a, [hli] ; Copy bank
	ldh [c], a
	inc c
	dec h
	dec e
	jr nz, .copyChunkPtr
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
