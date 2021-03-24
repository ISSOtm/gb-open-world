
NB_BG_PALETTES equ 128
NB_OBJ_PALETTES equ 128

NB_HW_PALS equ 8


; There currently only 1 small routine, it's not worth moving into ROMX for now
SECTION "Palette streamer routine", ROM0

; Inits a palette streamer struct
; @param hl Pointer to the base of the palette streamer
InitPaletteStreamer::
	xor a
	assert NB_BG_PALETTES == NB_OBJ_PALETTES, "Can't use same init func!"
	;ld c, NB_BG_PALETTES * 2
	assert NB_BG_PALETTES * 2 == 256
	ld c, a ; ld c, 0
	rst MemsetSmall
	inc a ; ld a, 1
	ld c, NB_HW_PALS
	jp MemsetSmall


SECTION "BG palette streamer", WRAM0,ALIGN[8]

; Array of ref counts indexed by the "global" BG palette ID
; If non-zero, then the palette is loaded somewhere in the `wBGPaletteIDs` array
wBGPaletteCounts::
	ds NB_BG_PALETTES * 2
.end::

	assert NB_BG_PALETTES <= 128, "Explanation below no longer true!"
; PPPP PPPU
; P = Which "global" BG palette is loaded in this slot
; U = If set, other 7 bits are meaningless
;     This is useful e.g. if coming from another menu which overwrote this
; Note: a free slot should be exactly $01; other values with the U bit set indicate that the
; palette slot is not available for dynamic allocation (e.g. reserved by UI)
wBGPaletteIDs::
	ds NB_HW_PALS
.end::

SECTION "OBJ palette streamer", WRAM0,ALIGN[8]

; Same format as above

wOBJPaletteCounts::
	ds NB_OBJ_PALETTES * 2

wOBJPaletteIDs:
	ds NB_HW_PALS
