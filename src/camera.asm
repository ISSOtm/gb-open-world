
INCLUDE "defines.asm"

MAX_CAM_SPEED equ 8 * SUBPX_PER_PX
	EXPORT MAX_CAM_SPEED

VIEWPORT_HEIGHT_B equ SCRN_Y_B + 1
VIEWPORT_WIDTH_B equ SCRN_X_B + 1


SECTION "Camera movement funcs", ROMX

	rsreset
MACRO cam_movt_type
\1 rb
	EXPORT \1
	dw \2
ENDM

CameraMovtFuncs::
	cam_movt_type CAM_STATIC, StaticCamera ; Stays at the target point
	cam_movt_type CAM_FOLLOW_PLAYER, FollowPlayer


StaticCamera:
	ld hl, wCameraYPos
	ld de, wCameraTargetYPos
	ld c, 4
	jp MemcpySmall


FollowPlayer:
	; TODO
	ret


SECTION "Camera movement func", ROM0

MoveCamera::
	; Mark that no palette needs to be committed; they will be added as needed
	xor a
	ld [wBGPaletteMask], a
	ld [wFadeDelta], a ; Do not alter the fade


	; See by how much the camera would need to move to reach the target Y position
	; We'll compute (target.y - cur.y)
	ld a, [wCameraYPos]
	ld l, a ; Importantly, we keep LOW(cur.y) for later
	ld a, [wCameraYPos + 1]
	ld h, a
	; TODO: lerp this
	ld a, [wCameraTargetYPos]
	sub l
	ld e, a
	ld a, [wCameraTargetYPos + 1]
	sbc h
	ld d, a
	; Now, we'll clamp the camera's movement at 8 pixels/frame
	; The engine only supports redrawing one row per frame, so that's as fast as it can go
	assert MAX_CAM_SPEED == $80, "The clamping below relies on max cam speed == $80"
	; Here's how it works:
	; The accepted range is [-MAX_CAM_SPEED; MAX_CAM_SPEED] = [-$80; $80] = [$FF80; $0080]
	; Setting aside $0080 for a moment, all valid values follow this:
	; If bit 7 is clear (positive values), then high byte == $00
	; If bit 7 is set   (negative values), then high byte == $FF
	; Note, however, that $0080 is a valid value, yet fails this rule.
	; Therefore, it will get clamped, but that will just set it to $0080... itself!
	ld a, e
	add a, a ; Get bit 7 into carry
	sbc a, a ; Yield $00 if bit 7 was clear, or $FF if it was set
	cp d ; Check if that matches the high byte
	jr z, .noVertClamp
	assert LOW(MAX_CAM_SPEED) == LOW(-MAX_CAM_SPEED)
	ld e, MAX_CAM_SPEED
	sla d
	sbc a, a
	ld d, a
.noVertClamp

	; Now, add that clamped vector to the current position
	ld c, l
	add hl, de
	ld a, h
	ld [wCameraYPos + 1], a
	ld a, l
	ld [wCameraYPos], a

	; A note about register usage... Currently:
	; HL = current camera position
	; DE = movement vector
	; C = LOW(old camera position)
	; B, A = don't care
	; The camera position has been written to `wCameraYPos`, so HL is currently merely a cache
	; The only information that we *must* preserve is the movement direction, which is contained
	; in D's bit 7. Everything else can be freely used as scratch.

	; And now, the *plat the résistance*: redrawing the tilemap
	; When do we need to redraw it? When the camera starts showing a new row of tiles.
	; This is easily checked for by comparing the tile portion of the position, but we can do one better
	; Actually, the camera can only move by up to 1 tile at a time, therefore, the position
	; (in tile units) can only change in increments of 1, and thus, checking if the parity
	; changed is enough!
	; ld a, l ; A == L right now
	xor c
	add a, a
	jp nc, .noRowRedraw
	; Alright, we need to redraw a row. Let's detail the process:
	; First, we need to unload the row that just went off-screen;
	; then, we draw the row that's about to be shown.
	; This is slightly misleading, as a row can be considered "on-screen" (and thus loaded)
	; without being actually shown on the GB screen.
	; The GB screen is 144 pixels tall, which amounts to 18 rows of tiles; however, unless
	; it's perfectly aligned to the tile grid, a bit of the 19th row will be shown.
	; Therefore, for loading purposes, the viewport is considered to be 19 rows tall, not 18.
	;
	; How do we determine which row just came on-screen? It's fairly simple: we just use
	; the movement direction! If the camera was moving upwards, the newly shown row will
	; be the camera's topmost (so, starting exactly at the camera's position).
	; If the camera was moving downwards, it'll be the bottommost row, so 18 rows down.
	;
	; How do we determine which row just went off-screen? Again, by using the movement direction.
	; If the camera was moving downwards, the row that went off-screen was the topmost, i.e.
	; it's right above the camera's *new* topmost: row "-1". Similarly, if the camera was
	; moving upwards, it'll be row 19.

	; First, we'll unload the row that's just went off-screen
	; We need to either move up by one tile row if we moved downwards, or down by 19 otherwise
	ld bc, -8 * SUBPX_PER_PX
	bit 7, d
	jr z, .movedDownwards
	ld bc, VIEWPORT_HEIGHT_B * 8 * SUBPX_PER_PX
.movedDownwards
	add hl, bc
	PUSHS
SECTION UNION "Scratch buffer", HRAM
hChunkGfxBank: ; db ; Use does not overlap with variable below, so it shares memory
hMetatileOfs: db ; Offset within the metatile definition (%0000 VH00)
hChunkPtrLow: db ; Low byte of the pointer in the chunk map
hNbTilesToDraw: db ; Amount of tiles remaining to be drawn
	POPS
	ld a, [wCameraXPos]
	and $80 ; %H000 0000
	rrca    ; %0H00 0000
	xor l
	and LOW(~$80)
	xor l   ; %VH00 0000
	ldh [hMetatileOfs], a
	; Compute the chunk position...
	ld a, [wCameraXPos + 1]
	swap a
	xor h ; Cam Y high
	and $0F
	xor h
	ld c, a
	ldh [hChunkPtrLow], a
	ld a, [wMapPtrHigh]
	ld b, a
	ld a, [wMapBank]
	ldh [hCurROMBank], a
	ld [rROMB0], a
	; Read the chunk's ptr and bank...
	ld a, [bc] ; Chunk ptr high
	ld e, a
	inc b ; Switch to bank table
	ld a, [bc] ; Chunk bank
	ldh [hCurROMBank], a
	ld [rROMB0], a
	ld b, e ; Load metatilemap ptr high
	; Compute metatilemap ptr low
	ld a, [wCameraXPos + 1]
	swap h
	xor h
	and $0F
	xor h
	ld c, a

	; Now, run through the row
	ld e, SCRN_X_B + 1
.unloadRow
	; Read the metatile ID
	ld a, [bc]
	ld l, a ; Save low byte for later
	and $30 ; Keep upper 2 bits
	swap a ; Multiply it by 16, the size of a metatile definition
	add a, b ; Add to the high byte of the base ptr
	inc a ; The metatile definitions are located 256 bytes after the metatilemap
	ld h, a
	ldh a, [hMetatileOfs]
	or l
	swap a ; Multiply by 16 (again)
	and $FC ; Only keep the relevant bits
	ld l, a
	; Read the palette ID
	ld l, [hl]
	sla l ; Double it, since counts are 2-byte
	; Decrement its count
	ld h, HIGH(wBGPaletteCounts)
	ld a, [hl]
	dec a
	ld [hli], a
	inc a ; cp $FF
	jr nz, :+
	dec [hl]
:
	; Toggle between the left and right half of the metatile
	ldh a, [hMetatileOfs]
	xor $40
	ldh [hMetatileOfs], a
	; If we just wrote the right half (= we're about to write the left half), go to the next metatile
	add a, a ; Discard bit 7 (only bit 6 is reamining)
	jr nz, .unloadedLeftTile
	inc c
	; Check if we need to go to the next chunk
	ld a, c
	and $0F
	jr nz, .unloadedLeftTile
	ldh a, [hChunkPtrLow]
	inc a ; Go to the next chunk
	ld l, a
	ld a, [wMapPtrHigh]
	ld h, a
	ld a, [wMapBank]
	ldh [hCurROMBank], a
	ld [rROMB0], a
	; Read the chunk's ptr and bank...
	ld b, [hl] ; Chunk ptr high
	inc h ; Switch to bank table
	ld a, [hl] ; Chunk bank
	ldh [hCurROMBank], a
	ld [rROMB0], a
	; Move the ptr low up 1 row
	ld a, c
	sub 16
	ld c, a
.unloadedLeftTile
	dec e
	jr nz, .unloadRow

	; Seize the opportunity to schedule loading new chunk gfx
	; New gfx loading happens when the imaginary line in the middle of the camera crosses
	; the imaginary line in the middle of the chunk. This is easily proven to happen at the
	; same time as redrawing a row, so we check for it here.
	; Since the camera moves up to 1 row at a time, it's sufficient to check which row the
	; camera is currently pointing to (depending on the movement direction)
	ld hl, wCameraYPos
	ld a, [hli]
	ld h, [hl] ; Save camera position for later
	ld l, a
	add a, a ; Shift tile position bit into carry
	ld a, h
	rla ; Rotate tile position bit in
	and $1F ; Get tile position within chunk
	rlc d ; Make sure to preserve direction bit in bit 0 for below!
	; The target tile position is 6 when moving upwards, and 7 when moving downwards
	ccf ; Have carry set iff moving downwards
	sbc a, 6
	jr nz, .noVertChunkLoading
	; Schedule loading the two new chunks' gfx
	; Compute the current chunk's position in the map, and offset it
	ld a, [wCameraXPos + 1]
	ld e, a
	swap a
	ld c, a
	ld a, h
	xor c
	and $F0
	xor c
	bit 0, d ; Check movement direction
	jr z, :+
	sub 16 * 2 ; When moving upwards, go up one row (and another one to compensate for below)
:
	add a, 16 ; When moving downwards, go down one row
	ld l, a
	; If the camera is "leaning" towards the left half of the chunk, go one chunk to the left
	; Fortunately, the "middle X position", in pixels, is $30, so we can just check the high
	; byte of the camera's X position.
	ld a, e ; Get HIGH(camera's X position)
	and $0F ; Only keep the (meta-)tile position
	sub 3
	add a, a ; Check sign bit
	jr nc, .leaningRight
	; Move left while staying in the same row
	ld a, l
	dec l
	xor l
	and $F0
	xor l
	ld l, a
.leaningRight
	; Now, we need to determine which of the 4 slots the (left) chunk will be loaded into
	ld c, LOW(hChunkGfxPtrs)
	bit 0, l ; Odd X position?
	jr z, :+
	assert LOW(hChunkGfxPtrs.topLeft) & $08 == 0
	assert LOW(hChunkGfxPtrs.bottomLeft) & $08 == 0
	assert LOW(hChunkGfxPtrs.topRight) & $08 == $08
	assert LOW(hChunkGfxPtrs.bottomRight) & $08 == $08
	set 3, c
:
	bit 4, l ; Odd Y position?
	jr z, :+
	assert LOW(hChunkGfxPtrs.topLeft) & $04 == 0
	assert LOW(hChunkGfxPtrs.bottomLeft) & $04 == $04
	assert LOW(hChunkGfxPtrs.topRight) & $04 == 0
	assert LOW(hChunkGfxPtrs.bottomRight) & $04 == $04
	set 2, c
:
	ld a, l ; Save the other chunk's position
	inc a
	xor l
	and $0F
	xor l
	ld e, a
	ld b, c ; Save original write ptr low, for loop termination
.loadHorizChunkStrip
	ld a, [wMapBank]
	ldh [hCurROMBank], a
	ld [rROMB0], a
	ld a, [wMapPtrHigh]
	ld h, a
	inc h ; Switch to bank map
	ld a, [hl]
	dec h ; Switch to HIGH(ptr) map
	ld h, [hl] ; Read HIGH(chunk ptr)
	ldh [hCurROMBank], a
	ld [rROMB0], a
	; Read tables size
	dec h
	ld l, -2
	ld a, [hli] ; Read high byte of post-metatile offset
	ld l, [hl] ; Read low byte of post-metatile offset (added to an ALIGN[8] address, so...)
	add a, h
	ld h, a
	ld a, [hli] ; Read bank
	and a
	jr z, :+
	ldh [hChunkGfxBank], a
	; Clear the bank so that the VBlank handler will not consider our entry until it's fully written
	xor a
	ldh [c], a
	inc c
	ld a, [hli] ; Size
	ldh [c], a
	inc c
	ld a, [hli] ; LOW(ptr)
	ldh [c], a
	inc c
	ld a, [hli] ; HIGH(ptr)
	ldh [c], a
	dec c ; Skip HIGH(ptr)
	dec c ; Sjip LOW(ptr)
	dec c ; Skip size
	ldh a, [hChunkGfxBank]
	ldh [c], a ; Write bank last, as it's what actually schedules the entry
:
	ld l, e ; Switch to second chunk
	assert LOW(hChunkGfxPtrs.topLeft) ^ LOW(hChunkGfxPtrs.topRight) == $08
	assert LOW(hChunkGfxPtrs.bottomLeft) ^ LOW(hChunkGfxPtrs.bottomRight) == $08
	ld a, c
	xor $08
	ld c, a
	cp b ; Check if we came back to the original write pointer
	jr nz, .loadHorizChunkStrip
.noVertChunkLoading

	;; Alright, now we can draw the new row!
	; Begin by computing the row's target position (in pixels)
	ld hl, wCameraYPos
	ld a, [hli]
	ld h, [hl] ; Save camera position for later
	ld l, a
	bit 0, d ; bit 7, d ; Above computation moved sign bit (bit 7) to bit 0
	jr nz, .movedUpwards
	ld bc, SCRN_Y * SUBPX_PER_PX
	add hl, bc
.movedUpwards

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
	ld a, l
	add a, a
	ld a, h
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

	; Compute chunk src addr
	ld a, [wCameraXPos + 1]
	swap a
	xor h ; Cam Y high
	and $0F
	xor h
	ld c, a
	ldh [hChunkPtrLow], a
	ld a, [wMapBank]
	ldh [hCurROMBank], a
	ld [rROMB0], a
	ld a, [wMapPtrHigh]
	ld b, a
	; Read the chunk's ptr and bank...
	ld a, [bc] ; Chunk ptr high
	ld l, a
	inc b ; Switch to bank table
	ld a, [bc] ; Chunk bank
	ldh [hCurROMBank], a
	ld [rROMB0], a
	ld b, l ; Load metatilemap ptr high
	; Compute metatilemap ptr low
	ld a, [wCameraXPos + 1]
	swap h
	xor h
	and $0F
	xor h
	ld c, a

	; And now, the loop proper!
	ld a, SCRN_X_B + 1
.drawRow
	ldh [hNbTilesToDraw], a
	ld a, [bc] ; Read target metatile within chunk
	push bc ; Save metatile read ptr
	; TODO: if metatile is dynamic, deref the array

	swap a ; Multiply metatile ID by 16 (size of a metatile entry)
	ld l, a ; Save low byte for later
	and $03 ; Keep upper 2 bits
	add a, b ; Add them to high byte of base ptr (low byte is computed from 0 so can't overflow)
	ld h, a
	inc h ; Switch to metatile def array
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
	; Ref palette, possibly load it, and translate it to the corresponding hardware palette
	push hl
	; First, increase that palette's ref count
	add a, a
	assert LOW(wBGPaletteCounts) == 0
	ld l, a
	ld h, HIGH(wBGPaletteCounts)
	ld c, l ; Keep the palette ID (times 2) for comparing
	ld a, [hl]
	inc a
	ld [hli], a
	jr nz, :+
	inc [hl] ; Apply carry
:
	; Check if we just increased the ref count to 1
	dec a ; Check if low byte is 1...
	or [hl]
	; Load 2 variables for the loops below
	ld hl, wBGPaletteIDs
	; Having Z here implies `(a - 1) | [hl] == 0`
	; ⇔ `a - 1 == 0` && `[hl] == 0`
	; ⇔ `a == 1` && `[hl] == 0`, and since A holds the counter's low byte & [HL] its high byte,
	; ⇔ counter == 1, which is what we want! :)
	jr nz, .alreadyLoaded
	; Palette just became referenced, so find a free hardware slot to load into
	call .loadPalette
	jr .gotPaletteSlot ; Still make the most likely path the fastest
	; ---------------------------
.alreadyLoaded
	; Find the slot the palette is referenced in
	ld a, c ; Get ID * 2 in A for comparing
	db $FE ; cp imm8 ; Skip incrementing L on first iteration
.seekPalette
	inc l ; Go to next palette
	cp [hl]
	jr nz, .seekPalette ; We expect this loop to terminate, as reaching this loop means the palette is loaded
	; FIXME: maybe check that the slot is valid... but that may incur a certain runtime cost
.gotPaletteSlot
	assert LOW(wBGPaletteIDs) == 0

	; Incorporate the chunk's bank bit into the attrs
	ldh a, [hChunkPtrLow]
	ld c, a
	rra
	sbc a, a
	and OAMF_BANK1
	or l
	ld b, a
	pop hl ; Get back metatile entry read ptr

	xor a
	ldh [rVBK], a
:
	ldh a, [rSTAT]
	and STATF_BUSY
	jr nz, :- ; From that point on, we have 32 cycles to write to VRAM
	ld a, [hli] ; Read tile ID
	bit 7, a ; Check if tile is in "common" block
	jr nz, .commonTile ; If so, no translation needs to be performed
	; Tile may either be in block 0/1 (even chunk row, no offset) or block 2/3 (odd chunk row, set bit 6)
	bit 4, c
	jr z, .evenChunkY
	or $40
	db $DC ; call c, imm16 (skip next 2-byte instruction)
.commonTile
	set OAMB_BANK1, b ; Force VRA1 if using a "common" tile
.evenChunkY
	ld [de], a ;; *** VRAM WRITE ***

	; Write attribute
	ld a, 1
	ldh [rVBK], a
	ld a, [hli]
	xor b
	and $E0 ; We need to ignore the attr's bits 0-4
	xor b
	ld [de], a ;; *** VRAM WRITE ***

	pop bc ; Get back metatile read ptr
	inc e ; Go to next tile
	; If we just wrote a metatile's rightmost tile, switch to the next one
	ld a, e
	rra ; Carry clear from `xor b` above
	jr c, .wroteLeftTile
	inc c ; inc bc
	; Check for and handle wrapping
	and $1F >> 1
	call z, .wrapHoriz
.wroteLeftTile

	ldh a, [hNbTilesToDraw]
	dec a
	jr nz, .drawRow
.noRowRedraw


	; Most of the code here is similar, if not identical to vertical movement code, so use it as a
	; reference if you're lost and there are no comments
	; I'm assuming you already read the vertical movement piece anyway


	ld a, [wCameraXPos]
	ld l, a
	ld a, [wCameraXPos + 1]
	ld h, a
	ld a, [wCameraTargetXPos]
	sub l
	ld e, a
	ld a, [wCameraTargetXPos + 1]
	sbc h
	ld d, a
	assert MAX_CAM_SPEED == $80, "The clamping below relies on max cam speed == $80"
	ld a, e
	add a, a ; Get bit 7 into carry
	sbc a, a ; Yield $00 if bit 7 was clear, or $FF if it was set
	cp d ; Check if that matches the high byte
	jr z, .noHorizClamp
	assert LOW(MAX_CAM_SPEED) == LOW(-MAX_CAM_SPEED)
	ld e, MAX_CAM_SPEED
	sla d
	sbc a, a
	ld d, a
.noHorizClamp

	ld c, l
	add hl, de
	ld a, h
	ld [wCameraXPos + 1], a
	ld a, l
	ld [wCameraXPos], a

	; ld a, l ; A == L right now
	xor c
	add a, a
	jp nc, .noColumnRedraw

	; First, we'll unload the column that's just went off-screen
	ld bc, -8 * SUBPX_PER_PX
	bit 7, d
	jr z, .movedRightwards
	ld bc, VIEWPORT_WIDTH_B * 8 * SUBPX_PER_PX
.movedRightwards
	add hl, bc
	ld a, [wCameraYPos]
	and $80 ; %V000 0000
	rlca    ; %0000 000V
	xor l
	and LOW(~$80)
	xor l   ; %H000 000V
	rrca    ; %VH00 0000
	ldh [hMetatileOfs], a
	; Compute the chunk position...
	ld a, [wCameraYPos + 1]
	swap h
	xor h ; Cam X high
	and $F0
	xor h
	ld c, a
	ldh [hChunkPtrLow], a
	ld a, [wMapPtrHigh]
	ld b, a
	ld a, [wMapBank]
	ldh [hCurROMBank], a
	ld [rROMB0], a
	; Read the chunk's ptr and bank...
	ld a, [bc] ; Chunk ptr high
	ld e, a
	inc b ; Switch to bank table
	ld a, [bc] ; Chunk bank
	ldh [hCurROMBank], a
	ld [rROMB0], a
	ld b, e ; Load metatilemap ptr high
	; Compute metatilemap ptr low
	ld a, [wCameraYPos + 1]
	xor h
	and $0F
	xor h
	swap a
	ld c, a

	; Now, run through the column
	ld e, SCRN_Y_B + 1
.unloadColumn
	; Read the metatile ID
	ld a, [bc]
	ld l, a ; Save low byte for later
	and $30 ; Keep upper 2 bits
	swap a ; Multiply it by 16, the size of a metatile definition
	add a, b ; Add to the high byte of the base ptr
	inc a ; The metatile definitions are located 256 bytes after the metatilemap
	ld h, a
	ldh a, [hMetatileOfs]
	or l
	swap a ; Multiply by 16 (again)
	and $FC ; Only keep the relevant bits
	ld l, a
	; Read the palette ID
	ld l, [hl]
	sla l ; Double it, since counts are 2-byte
	; Decrement its count
	ld h, HIGH(wBGPaletteCounts)
	ld a, [hl]
	dec a
	ld [hli], a
	inc a ; cp $FF
	jr nz, :+
	dec [hl]
:
	; Toggle between the top and bottom half of the metatile
	ldh a, [hMetatileOfs]
	xor $80
	ldh [hMetatileOfs], a
	; If we just wrote the bottom half (= we're about to write the top half), go to the next metatile
	add a, a ; Shift out bit 7
	jr c, .unloadedTopTile
	ld a, c
	add a, 16 ; Note that this will automatically wrap within the chunk's map
	ld c, a
	; Check if we need to go to the next chunk
	and $F0 ; Get the Y coord
	jr nz, .unloadedTopTile
	ldh a, [hChunkPtrLow]
	add a, 16 ; Go to the next chunk
	ld l, a
	ld a, [wMapPtrHigh]
	ld h, a
	ld a, [wMapBank]
	ldh [hCurROMBank], a
	ld [rROMB0], a
	; Read the chunk's ptr and bank...
	ld b, [hl] ; Chunk ptr high
	inc h ; Switch to bank table
	ld a, [hl] ; Chunk bank
	ldh [hCurROMBank], a
	ld [rROMB0], a
.unloadedTopTile
	dec e
	jr nz, .unloadColumn

	ld hl, wCameraXPos
	ld a, [hli]
	ld h, [hl] ; Save camera position for later
	ld l, a
	add a, a ; Shift tile position bit into carry
	ld a, h
	rla ; Rotate tile position bit in
	and $1F ; Get tile position within chunk
	rlc d ; Make sure to preserve direction bit in bit 0 for below!
	; The target tile position is 5 when moving upwards, and 6 when moving downwards
	ccf ; Have carry set iff moving downwards
	sbc a, 5
	jr nz, .noHorizChunkLoading
	; Schedule loading the two new chunks' gfx
	; Compute the current chunk's position in the map, and offset it
	ld a, [wCameraYPos + 1]
	ld e, a
	ld a, h
	swap a
	xor e
	and $0F
	xor e
	bit 0, d ; Check movement direction
	jr z, :+
	sub 2 ; When moving leftwards, go left one column (and another one to compensate for below)
:
	inc a ; When moving rightwards, go right one column
	ld l, a
	; If the camera is "leaning" towards the top half of the chunk, go one chunk up
	ld a, [wCameraYPos]
	add a, a
	ld a, e ; Get HIGH(camera's Y position)
	rla ; Pick up the MSB of LOW(cam's Y pos)
	and $1F ; Only keep the (meta-)tile position
	sub $38 /* Account for left shift */ << 1 /* Account for subpixel shift */ >> 4
	add a, a ; Check sign bit
	jr nc, .leaningDown
	ld a, l
	sub 16
	ld l, a
.leaningDown
	; Now, we need to determine which of the 4 slots the (top) chunk will be loaded into
	ld c, LOW(hChunkGfxPtrs)
	bit 0, l ; Odd X position?
	jr z, :+
	assert LOW(hChunkGfxPtrs.topLeft) & $08 == 0
	assert LOW(hChunkGfxPtrs.bottomLeft) & $08 == 0
	assert LOW(hChunkGfxPtrs.topRight) & $08 == $08
	assert LOW(hChunkGfxPtrs.bottomRight) & $08 == $08
	set 3, c
:
	bit 4, l ; Odd Y position?
	jr z, :+
	assert LOW(hChunkGfxPtrs.topLeft) & $04 == 0
	assert LOW(hChunkGfxPtrs.bottomLeft) & $04 == $04
	assert LOW(hChunkGfxPtrs.topRight) & $04 == 0
	assert LOW(hChunkGfxPtrs.bottomRight) & $04 == $04
	set 2, c
:
	ld a, l ; Save the other chunk's position
	add a, 16
	ld e, a
	ld b, c ; Save original write ptr low, for loop termination
.loadVertChunkStrip
	ld a, [wMapBank]
	ldh [hCurROMBank], a
	ld [rROMB0], a
	ld a, [wMapPtrHigh]
	ld h, a
	inc h ; Switch to bank map
	ld a, [hl]
	dec h ; Switch to HIGH(ptr) map
	ld h, [hl] ; Read HIGH(chunk ptr)
	ldh [hCurROMBank], a
	ld [rROMB0], a
	; Read tables size
	dec h
	ld l, -2
	ld a, [hli] ; Read high byte of post-metatile offset
	ld l, [hl] ; Read low byte of post-metatile offset (added to an ALIGN[8] address, so...)
	add a, h
	ld h, a
	ld a, [hli] ; Read bank
	and a
	jr z, :+
	ldh [hChunkGfxBank], a
	; Clear the bank so that the VBlank handler will not consider our entry until it's fully written
	xor a
	ldh [c], a
	inc c
	ld a, [hli] ; Size
	ldh [c], a
	inc c
	ld a, [hli] ; LOW(ptr)
	ldh [c], a
	inc c
	ld a, [hli] ; HIGH(ptr)
	ldh [c], a
	dec c ; Skip HIGH(ptr)
	dec c ; Sjip LOW(ptr)
	dec c ; Skip size
	ldh a, [hChunkGfxBank]
	ldh [c], a ; Write bank last, as it's what actually schedules the entry
:
	ld l, e ; Switch to second chunk
	assert LOW(hChunkGfxPtrs.topLeft) ^ LOW(hChunkGfxPtrs.bottomLeft) == $04
	assert LOW(hChunkGfxPtrs.topRight) ^ LOW(hChunkGfxPtrs.bottomRight) == $04
	ld a, c
	xor $04
	ld c, a
	cp b ; Check if we came back to the original write pointer
	jr nz, .loadVertChunkStrip
.noHorizChunkLoading

	;; Alright, now we can draw the new column!
	; Begin by computing the column's target position (in pixels)
	ld hl, wCameraXPos
	ld a, [hli]
	ld h, [hl] ; Save camera position for later
	ld l, a
	bit 0, d ; bit 7, d ; Above computation moved sign bit (bit 7) to bit 0
	jr nz, .movedLeftwards
	ld bc, SCRN_X * SUBPX_PER_PX
	add hl, bc
.movedLeftwards

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
	ld a, [wCameraYPos] ; Similar trick
	add a, a
	ld a, [wCameraYPos + 1]
	rla ; Shift in that bit
	rrca ; Rotate bits in place for "merging"
	rrca
	rrca
	ld d, a ; Store for merging
	ld a, l
	add a, a
	ld a, h
	rla
	xor d
	and $1F ; Keep $1F from A, $E0 from D
	xor d
	ld e, a
	ld a, d
	and $03 ; Only keep relevant bits
	or HIGH(_SCRN0)
	ld d, a

	; Compute chunk src addr
	; TODO: factor out into a function (same-ish snippet above unload loop)
	ld a, [wCameraYPos + 1]
	swap h ; Get chunk bits in the right spot
	xor h ; Cam X high
	and $F0
	xor h
	ld c, a
	ldh [hChunkPtrLow], a
	ld a, [wMapBank]
	ldh [hCurROMBank], a
	ld [rROMB0], a
	ld a, [wMapPtrHigh]
	ld b, a
	; Read the chunk's ptr and bank...
	ld a, [bc] ; Chunk ptr high
	ld l, a
	inc b ; Switch to bank table
	ld a, [bc] ; Chunk bank
	ldh [hCurROMBank], a
	ld [rROMB0], a
	ld b, l ; Load metatilemap ptr high
	; Compute metatilemap ptr low
	ld a, [wCameraYPos + 1]
	xor h
	and $0F
	xor h
	swap a
	ld c, a

	; And now, the loop proper!
	ld a, SCRN_Y_B + 1
.drawColumn
	ldh [hNbTilesToDraw], a
	ld a, [bc] ; Read target metatile within chunk
	push bc ; Save metatile read ptr
	; TODO: if metatile is dynamic, deref the array

	swap a ; Multiply metatile ID by 16 (size of a metatile entry)
	ld l, a ; Save low byte for later
	and $03 ; Keep upper 2 bits
	add a, b ; Add them to high byte of base ptr (low byte is computed from 0 so can't overflow)
	ld h, a
	inc h ; Switch to metatile def array
	ld a, l
	and $F0 ; Keep lower 4 bits of metatile ID
	bit 5, e ; If on an odd row, use entry #2 or #3
	jr z, .writeEvenRow2
	add a, 8
.writeEvenRow2
	bit 0, e ; If on an odd column, use entry #1 or #3
	jr z, .writeEvenColumn2
	add a, 4
.writeEvenColumn2
	ld l, a

	ld a, [hli] ; Read palette ID
	; Ref palette, possibly load it, and translate it to the corresponding hardware palette
	push hl
	; First, increase that palette's ref count
	add a, a
	assert LOW(wBGPaletteCounts) == 0
	ld l, a
	ld h, HIGH(wBGPaletteCounts)
	ld c, l ; Keep the palette ID (times 2) for comparing
	ld a, [hl]
	inc a
	ld [hli], a
	jr nz, :+
	inc [hl] ; Apply carry
:
	; Check if we just increased the ref count to 1
	dec a ; Check if low byte is 1...
	or [hl]
	; Load 2 variables for the loops below
	ld hl, wBGPaletteIDs
	; Having Z here implies `(a - 1) | [hl] == 0`
	; ⇔ `a - 1 == 0` && `[hl] == 0`
	; ⇔ `a == 1` && `[hl] == 0`, and since A holds the counter's low byte & [HL] its high byte,
	; ⇔ counter == 1, which is what we want! :)
	jr nz, .alreadyLoaded2
	; Palette just became referenced, so find a free hardware slot to load into
	call .loadPalette
	jr .gotPaletteSlot2 ; Still make the most likely path the fastest
	; ---------------------------
.alreadyLoaded2
	; Find the slot the palette is referenced in
	ld a, c ; Get ID * 2 in A for comparing
	db $FE ; cp imm8 ; Skip incrementing L on first iteration
.seekPalette2
	inc l ; Go to next palette
	cp [hl]
	jr nz, .seekPalette2 ; We expect this loop to terminate, as reaching this loop means the palette is loaded
	; FIXME: maybe check that the slot is valid... but that may incur a certain runtime cost
.gotPaletteSlot2
	assert LOW(wBGPaletteIDs) == 0

	; Incorporate the chunk's bank bit into the attrs
	ldh a, [hChunkPtrLow]
	ld c, a
	rra
	sbc a, a
	and OAMF_BANK1
	or l
	ld b, a
	pop hl ; Get back metatile entry read ptr

	xor a
	ldh [rVBK], a
:
	ldh a, [rSTAT]
	and STATF_BUSY
	jr nz, :- ; From that point on, we have 32 cycles to write to VRAM
	ld a, [hli] ; Read tile ID
	bit 7, a ; Check if tile is in "common" block
	jr nz, .commonTile2 ; If so, no translation needs to be performed
	; Tile may either be in block 0/1 (even chunk row, no offset) or block 2/3 (odd chunk row, set bit 6)
	bit 4, c
	jr z, .evenChunkY2
	or $40
	db $DC ; call c, imm16 (skip next 2-byte instruction)
.commonTile2
	set OAMB_BANK1, b ; Force VRA1 if using a "common" tile
.evenChunkY2
	ld [de], a ;; *** VRAM WRITE ***

	; Write attribute
	ld a, 1
	ldh [rVBK], a
	ld a, [hli]
	xor b
	and $E0 ; We need to ignore the attr's bits 0-4
	xor b
	ld [de], a ;; *** VRAM WRITE ***

	pop bc ; Get back metatile read ptr
	ld a, e ; Go to next tile row
	add a, SCRN_VX_B
	ld e, a
	adc a, d
	sub e
	ld d, a
	; If we just wrote a metatile's bottommost tile, switch to the next one
	bit 5, e
	jr nz, .wroteUpperTile
	ld a, c
	add a, 16
	ld c, a
	; Check for and handle wrapping
	call c, .wrapVert
.wroteUpperTile

	ldh a, [hNbTilesToDraw]
	dec a
	jr nz, .drawColumn
.noColumnRedraw


	; Now, if any palettes need to be committed, do so
	ld a, [wBGPaletteMask]
	and a
	assert BANK(FadePaletteBuffers) == 0 || BANK(FadePaletteBuffers) == BANK(@)
	jp nz, FadePaletteBuffers ; Tail call
	ret


.loadPalette
	; This function is factored out because it's shared by both vertical and horizontal
	; movement, and because it helps keeping the loops small enough for `jr`
	; However, most code isn't shared, because the loops must run fast, and `call`+`ret`
	; is 10 cycles of overhead, which is too much for loops running this often.
	; This function escapes the above for two reasons:
	; 1. This function may only run up to 7 times per loop (there are 8 palette slots,
	;    but 1 should still be loaded anyway), so the actual max overhead is much smaller
	; 2. If this function is inlined, the loops become larger than `jr` can handle,
	;    wasting 1 cycle per iteration (= 21 cycles total). This means that we're still
	;    winning cycles overall as long as no more than 2 palettes are loaded per loop,
	;    which is reasonable.

	; To avoid loading the same palette in two separate hardware slots (which would make them both
	; used), walk the table once, checking if the palette is already loaded.
	; And only if it isn't, seek a free slot.
	ld b, l ; Save iteration ptr for second loop
.checkIfAlreadyLoaded
	ld a, [hl]
	cp c ; If the palette is still loaded, no need to re-load it
	ret z ; It's already loaded, no need to write it
	inc l
	bit 3, l
	jr z, .checkIfAlreadyLoaded
	ld l, b ; Reload iteration ptr
	dec l ; Compensate for the increment below
.lookupFreeSlot
	inc l ; Go to next slot
	; Assert that the selected slot is actually valid (below 8); this does incur a small overhead,
	; but this loop should only occur up to 8 times, so it's acceptable, to catch a likely bug.
	bit 3, l
	error nz
	ld a, [hl]
	srl a ; Shift bit 0 (indicating special status) into carry
	jr c, .checkFreeSlot ; If bit 0 is set, the slot is acceptable if it contains $01 ⇔ C and Z set
	; Otherwise, check if the palette in question is referenced (otherwise, the slot is free)
	adc a, a ; Cancel the above
	ld b, l ; Save LOW(iteration ptr)
	ld l, a ; Go to palette's count...
	assert HIGH(wBGPaletteCounts) == HIGH(wBGPaletteIDs) - 1
	dec h ; ...there.
	ld a, [hli]
	or [hl]
	inc h ; Resume...
	ld l, b ; ...iterating
	and a ; Restore (re-compute) Z flag
.checkFreeSlot
	jr nz, .lookupFreeSlot
.foundSlot
	ld [hl], c

	; Now, actually load the palette
	push hl
	push de
	; Each palette is 4 colors of 3 bytes each = 12 bytes
	; Though, the value we get as input is already doubled
	; Since there are 128 palettes (times two), the optimal process is:
	; 1. Tripling (16-bit, obviously) the value
	; 2. Doubling it
	; The tripling is most efficient at this point, since it can be implemented as a
	; (cheap) 16-bit doubling, followed by an "8 + 16 → 16" addition
	ld d, 0
	ld a, c ; Save this value pre-doubling
	sla c
	rl b
	add a, c
	ld e, a
	adc a, d
	sub e
	ld d, a ; * 3
	sla e
	rl d ; * 2
	ld a, [wMapPtrHigh]
	add a, 2 ; Skip the two 256-byte chunk maps
	add a, d
	ld d, a
	; Now, DE = Source address in the map's palette array
	; HL is simpler to compute, since the output buffer is known to be no larger than a page
	; So the offset can be simply computed as 8-bit
	assert HIGH(wBGPaletteBuffer) == HIGH(wBGPaletteBuffer.end - 1)
	; Since the assertion above guarantees that the offsets are below 256, the following
	; operations are guaranteed not to overflow
	ld a, l
	add a, a
	add a, l ; * 3
	add a, a ; * 2
	add a, a ; * 2
	assert LOW(wBGPaletteBuffer) != 0
	add a, LOW(wBGPaletteBuffer)
	ld l, a
	ld h, HIGH(wBGPaletteBuffer)
	; Now, HL = Destination address in the palette buffer
	ld c, 4 * 3
	rst MemcpySmall
	pop de
	pop hl

	; Now, add the palette to the fade mask
	ld a, 7 ; Palette #0 maps to bit 7, not bit 0
	sub l
	; "A = 1 << A" code courtesy of calc84maniac
	sub 4 ; Check if in high or low nibble
	jr nc, .highNibble
	; 0 → $01, 1 → $02, 2 → $04, 3 → $05
	; Overall, these two instructions add 5 to the number (to which 4 was subtracted above)
	; However, the first instruction will generate a carry for inputs of $FE and $FF
	; (which were 2 and 3 before `sub 4`); the `adc` will pick the carry up, and "separate"
	; 0 / 1 from 2 / 3 by an extra 1. Luckily, this yields correct results for 0 ($01),
	; 1 ($02), and 2 ($03 + 1 = $04). We'll see about fixing 3 after the jump.
	add a, 2
	adc a, 3
	jr .fixThree
.highNibble
	; 4 → $10, 5 → $20, 6 → $40, 7 → $50
	; This is basically the same as the above, except that we need a different initial
	; offset to generate a carry as needed.
	add a, -2
	adc a, 3
	swap a ; Switch to the high nibble, though
.fixThree
	; At this point, both inputs are identical, ignoring the nibble swapping.
	; I will describe the process for the low nibble, but it works similarly for the high one.
	; After being shifted left, the inputs are $02, $04, $08 and $0A; all are valid BCD,
	; except for $0A. Since we just performed `add a, a`, DAA will correct the latter to $10.
	; (This should be correctly emulated everywhere, since the inputs are identical to
	; "regular" BCD.)
	; When placing the results back, we'll thus get $01, $02, $04 and $08!
	add a, a
	daa
	rra ; Note that we need this specific rotate, since $A0 gets corrected to $00 with carry set
	ld c, a
	ld a, [wBGPaletteMask]
	or c
	ld [wBGPaletteMask], a
	ret


.wrapVert
	ld d, HIGH(_SCRN0)
	ldh a, [hChunkPtrLow]
	add a, 16 ; Go to next chunk
	jr .wrap

.wrapHoriz
	; Wrap metatile read ptr
	ld a, c
	sub 16
	ld c, a
	; Wrap metatile write ptr
	ld a, e
	sub SCRN_VX_B
	ld e, a
	ldh a, [hChunkPtrLow]
	inc a ; Go to next chunk
.wrap ; Code common to both vertical and horizontal wrapping
	ldh [hChunkPtrLow], a
	ld l, a
	ld a, [wMapPtrHigh]
	ld h, a
	ld a, [wMapBank]
	ldh [hCurROMBank], a
	ld [rROMB0], a
	; Read the chunk's ptr and bank...
	ld b, [hl] ; Chunk ptr high
	inc h ; Switch to bank table
	ld a, [hl] ; Chunk bank
	ldh [hCurROMBank], a
	ld [rROMB0], a
	ret


SECTION "Camera state", WRAM0

wCameraMovtType::
	db

SECTION "Camera variables", WRAM0

wCameraYPos::
	dw
wCameraXPos::
	dw

SECTION "Camera movement variables", WRAM0

; 12.4 coord pair that the camera's origin will attempt to move towards
wCameraTargetYPos::
	dw
wCameraTargetXPos::
	dw
