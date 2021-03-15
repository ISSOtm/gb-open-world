
NB_BG_PALETTES equ 128
NB_OBJ_PALETTES equ 128

NB_HW_PALS equ 8


SECTION "Palette streamer", ROMX

; Inits a palette streamer struct
; @param hl Pointer to the base of the palette streamer
InitPaletteStreamer:
	xor a
	assert NB_BG_PALETTES == NB_OBJ_PALETTES, "Can't use same init func!"
	;ld c, NB_BG_PALETTES * 2
	assert NB_BG_PALETTES * 2 == 256
	ld c, a ; ld c, 0
	rst MemsetSmall
	ld c, NB_HW_PALS
	ld a, $80
	jp MemsetSmall


SECTION "BG palette streamer", WRAM0,ALIGN[8]

; Array of ref counts indexed by the "global" BG palette ID
; If non-zero, then the palette is loaded somewhere in the `wBGPaletteIDs` array
wBGPaletteCounts:
	ds NB_BG_PALETTES * 2

	assert NB_BG_PALETTES <= 128, "Explanation below no longer true!"
; UPPP PPPP
; U = If set, other 7 bits are meaningless
;     This is useful e.g. if coming from another menu which overwrote this
; P = Which "global" BG palette is loaded in this slot
wBGPaletteIDs:
	ds NB_HW_PALS

SECTION "OBJ palette streamer", WRAM0,ALIGN[8]

; Same format as above

wOBJPaletteCounts:
	ds NB_OBJ_PALETTES * 2

wOBJPaletteIDs:
	ds NB_HW_PALS
