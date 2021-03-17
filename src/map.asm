
INCLUDE "defines.asm"


LOG_NB_MAP_FLAGS equ 7 ; TBD
NB_MAP_FLAGS equ 1 << LOG_NB_MAP_FLAGS

	static_assert NB_MAP_FLAGS % 8 == 0, "Must have at least 8 map flags, *duh*."


SECTION "ROM0 Map routines", ROM0

LoadMap:
	add a, a
	ld l, a
	ld h, HIGH(MapHeaders)

	ld a, BANK(MapHeaders)
	ldh [hCurROMBank], a
	ld [rROMB0], a

	ld a, [hli]
	ld [wMapBank], a
	ld a, [hli]
	ld [wMapPtrHigh], a
	ret


RedrawScreen:
	; First, reset the BG palette streamer, since we're redrawing from scratch
	ld hl, wBGPaletteCounts
	call InitPaletteStreamer

	ld a, [wMapBank]
	ldh [hCurROMBank], a
	ld [rROMB0], a

	ld a, [wMapPtrHigh]
	ld h, a
	ld l, 0

	;; TODO

	ret


SECTION "Temporary map headers", ROMX

; These will be auto-generated from map files at some point instead
MapHeaders:


SECTION "Map variables", WRAM0

wMapBank:
	db
wMapPtrHigh:
	db

SECTION "Camera variables", WRAM0

wCameraYPos:
	dw
wCameraXPos:
	dw

SECTION "Map flags", WRAM0,ALIGN[LOG_NB_MAP_FLAGS]

wMapFlags:
	ds NB_MAP_FLAGS / 8
