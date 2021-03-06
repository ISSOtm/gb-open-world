
INCLUDE "defines.asm"

SECTION "Vectors", ROM0[0]
NULL::
	; This traps jumps to $0000, which is a common "default" pointer
	; $FFFF is another one, but reads rIE as the instruction byte
	; Thus, we put two `nop`s that may serve as operands, before soft-crashing
	; The operand will always be 0, so even jumps will work fine. Nice!
	nop
	nop
	rst Crash
	ds $08 - @ ; 5 free bytes

; Waits for the next VBlank beginning
; Requires the VBlank handler to be able to trigger, otherwise will loop infinitely
; This means IME should be set, the VBlank interrupt should be selected in IE,
; and the LCD should be turned on.
; WARNING: Be careful if calling this with IME reset (`di`), if this was compiled
; with the `-h` flag, then a hardware bug is very likely to cause this routine to
; go horribly wrong.
; Note: the VBlank handler recognizes being called from this function (through `hVBlankFlag`),
; and does not try to save registers if so. To be safe, consider all registers to be destroyed.
; @destroy Possibly every register. The VBlank handler stops preserving anything when executed from this function
WaitVBlank::
	ld a, 1
	ldh [hVBlankFlag], a
.wait
	halt
	jr .wait
	ds $10 - 1 - @ ; 0 free bytes

MemsetLoop:
	ld a, d
; You probably don't want to use this for writing to VRAM while the LCD is on. See LCDMemset.
Memset::
	ld [hli], a
	ld d, a
	dec bc
	ld a, b
	or c
	jr nz, MemsetLoop
	ret
	ds $18 - @ ; 0 free bytes

MemcpySmall::
	ld a, [de]
	ld [hli], a
	inc de
	dec c
	jr nz, MemcpySmall
	ret
	ds $20 - @ ; 1 free byte

MemsetSmall::
	ld [hli], a
	dec c
	jr nz, MemsetSmall
	ret
	ds $28 - 3 - @ ; 0 free bytes

; Dereferences `hl` and jumps there
; All other registers are passed to the called code intact, except Z is reset
; Soft-crashes if the jump target is in RAM
; @param hl Pointer to an address to jump to
JumpToPtr::
	ld a, [hli]
	ld h, [hl]
	ld l, a
; Jump to some address
; All registers are passed to the called code intact, except Z is reset
; (`jp CallHL` is equivalent to `jp hl`, but with the extra error checking on top)
; Soft-crashes if attempting to jump to RAM
; @param hl The address of the code to jump to
CallHL::
	bit 7, h
	error nz
	jp hl
	ds $30 - @ ; 3 free bytes


; Jumps to some address
; All registers are passed to the target code intact, except Z is reset
; (`jp CallDE` would be equivalent to `jp de` if that instruction existed)
; Soft-crashes if attempting to jump to RAM
; @param de The address of the code to jump to
CallDE::
	bit 7, d
	push de
	ret z ; No jumping to RAM, boy!
	rst Crash
	ds $38 - @ ; 3 free bytes

; Perform a soft-crash. Prints debug info on-screen
Crash::
	di ; Doing this as soon as possible to avoid interrupts messing up
	jp HandleCrash
	ds $40 - @

; VBlank handler
	push af
	ldh a, [hLCDC]
	ldh [rLCDC], a
	jp VBlankHandler
	ds $48 - @

; STAT handler
	reti
	ds $50 - @

; Timer handler
	reti
	ds $58 - @

; Serial handler
	reti
	ds $60 - @

; Joypad handler (useless)
	reti

SECTION "VBlank handler", ROM0

VBlankHandler:
	ldh a, [hSCY]
	ldh [rSCY], a
	ldh a, [hSCX]
	ldh [rSCX], a
	ldh a, [hBGP]
	ldh [rBGP], a
	ldh a, [hOBP0]
	ldh [rOBP0], a
	ldh a, [hOBP1]
	ldh [rOBP1], a

	ldh a, [rVBK]
	ldh [hVBK], a
	ldh a, [hVRAMTransferDestHigh]
	and a
	jr z, .noVRAMTransfer
	ldh [rHDMA3], a
	ldh a, [hVRAMTransferSrcHigh]
	ldh [rHDMA1], a
	ldh a, [hVRAMTransferSrcLow]
	ldh [rHDMA2], a
	ldh a, [hVRAMTransferDestLow]
	ldh [rHDMA4], a
	ldh a, [hVRAMTransferSrcBank]
	ld [rROMB0], a
	ldh a, [hVRAMTransferDestBank]
	ldh [rVBK], a
	ldh a, [hVRAMTransferLen]
	ldh [rHDMA5], a
	; Restore ROM and VRAM banks
	ldh a, [hCurROMBank]
	ld [rROMB0], a
	; ACK the transfer
	xor a
	ldh [hVRAMTransferDestHigh], a
.noVRAMTransfer

	; Load requested chunk gfx
	push bc
	ld c, LOW(hChunkGfxPtrs)
.loadChunkGfx
	ldh a, [c] ; Load bank
	inc c ; Skip bank
	and a
	jr z, .noChunkGfx
	ld [rROMB0], a
	; Check if we'll have enough time to perform the GDMA
	; It's possible to load 14.25 tiles per scanline (456 dots/scanline / 32 dots/tile),
	; so we'll be conservative (including to account for setup / teardown) and assume 13.
	ldh a, [rLY]
	and a
	jr z, .tooLateForChunkGfx ; If scanline 0 started, we're too late to load anything
	; A = $9A - A
	cpl
	add a, $9A + 1
	; Multiply by 13... (since input is at most 10, this won't overflow)
	ld b, a
	swap a ; * 16
	sub b ; * 15
	sub b ; * 14
	sub b ; * 13
	ld b, a ; B = max amount of tiles we can load
	ldh a, [c] ; Load size
	inc c ; Skip size
	cp b ; If requested >= max (to account for HDMA5 being 1 smaller than real), give up
	jr nc, .cantLoadChunkGfx
	ld b, a ; Save for writing to HDMA5
	ldh a, [c] ; Read LOW(src ptr)
	ldh [rHDMA2], a
	inc c ; Skip LOW(src ptr)
	ldh a, [c] ; Read HIGH(src ptr)
	ldh [rHDMA1], a
	assert LOW(hChunkGfxPtrs.topLeft) & $04 == 0
	assert LOW(hChunkGfxPtrs.topRight) & $04 == 0
	assert LOW(hChunkGfxPtrs.bottomLeft) & $04 == 4
	assert LOW(hChunkGfxPtrs.bottomRight) & $04 == 4
	ld a, c
	and $14 ; We load at $9[04]00, not $8[04]00; rely on bit 4 being always set!
	assert LOW(hChunkGfxPtrs) & $10 == $10
	ldh [rHDMA3], a
	assert LOW(hChunkGfxPtrs.topLeft) & $08 == 0
	assert LOW(hChunkGfxPtrs.topRight) & $08 == 8
	assert LOW(hChunkGfxPtrs.bottomLeft) & $08 == 0
	assert LOW(hChunkGfxPtrs.bottomRight) & $08 == 8
	ld a, c ; Get bit 3 into bit 1
	rra
	rra
	rra
	ldh [rVBK], a
	xor a
	ldh [rHDMA4], a
	ld a, b
	ldh [rHDMA5], a
	ldh a, [hCurROMBank]
	ld [rROMB0], a
	dec c ; Skip HIGH(src ptr)
	dec c ; Skip LOW(src ptr)
	dec c ; Skip size
	xor a
	ldh [c], a ; Reset bank, to mark gfx as loaded
	inc c ; Skip bank
.noChunkGfx
	inc c ; Skip size
.cantLoadChunkGfx
	inc c ; Skip LOW(src ptr)
	inc c ; Skip HIGH(src ptr)
	assert LOW(hChunkGfxPtrs) & $1F == $10
	bit 4, c
	jr nz, .loadChunkGfx
.tooLateForChunkGfx
	pop bc

	ldh a, [hVBK]
	ldh [rVBK], a

	; OAM DMA can occur late in the handler, because it will still work even
	; outside of VBlank. Sprites just will not appear on the scanline(s)
	; during which it's running.
	ldh a, [hOAMHigh]
	and a
	jr z, .noOAMTransfer
	call hOAMDMA
	xor a
	ldh [hOAMHigh], a
.noOAMTransfer

	; Put all operations that cannot be interrupted above this line
	; For example, OAM DMA (can't jump to ROM in the middle of it),
	; VRAM accesses (can't screw up timing), etc
	ei

	ldh a, [hVBlankFlag]
	and a
	jr z, .lagFrame
	xor a
	ldh [hVBlankFlag], a

	ld c, LOW(rP1)
	ld a, $20 ; Select D-pad
	ldh [c], a
REPT 6
	ldh a, [c]
ENDR
	or $F0 ; Set 4 upper bits (give them consistency)
	ld b, a

	; Filter impossible D-pad combinations
	and $0C ; Filter only Down and Up
	ld a, b
	jr nz, .notUpAndDown
	or $0C ; If both are pressed, "unpress" them
	ld b, a
.notUpAndDown
	and $03 ; Filter only Left and Right
	jr nz, .notLeftAndRight
	; If both are pressed, "unpress" them
	inc b
	inc b
	inc b
.notLeftAndRight
	swap b ; Put D-pad buttons in upper nibble

	ld a, $10 ; Select buttons
	ldh [c], a
REPT 6
	ldh a, [c]
ENDR
	; On SsAB held, soft-reset
	and $0F
	jr z, .perhapsReset
.dontReset

	or $F0 ; Set 4 upper bits
	xor b ; Mix with D-pad bits, and invert all bits (such that pressed=1) thanks to "or $F0"
	ld b, a

	; Release joypad
	ld a, $30
	ldh [c], a

	ldh a, [hHeldKeys]
	cpl
	and b
	ldh [hPressedKeys], a
	ld a, b
	ldh [hHeldKeys], a

	pop af ; Pop off return address as well to exit infinite loop
.lagFrame
	pop af
	ret

.perhapsReset
	ldh a, [hCanSoftReset]
	and a
	jr z, .dontReset
	jp Reset


; This alignment guarantees both that it's possible to go between pointers by simple `inc l`,
; and that the "current chunk" is easily determined by looking at two bits in the pointer
; An alignment of 4 would be sufficient, but 5 guarantees that bit 4 is clear for the entire buffer... until its end.
SECTION "Chunk gfx loading ptrs", HRAM,ALIGN[5,$10]

hChunkGfxPtrs::
; Format:
;  - ROM bank (if non-zero, will be considered for loading)
;  - LE pointer
;  - Size (in HDMA5 format)
.topLeft::
	ds 4
.bottomLeft::
	ds 4
.topRight::
	ds 4
.bottomRight::
	ds 4
.end


SECTION "VBlank HRAM", HRAM

; DO NOT TOUCH THIS
; When this flag is set, the VBlank handler will assume the caller is `WaitVBlank`,
; and attempt to exit it. You don't want that to happen outside of that function.
hVBlankFlag:: db

; High byte of the address of the OAM buffer to use.
; When this is non-zero, the VBlank handler will write that value to rDMA, and
; reset it.
hOAMHigh:: db

; Shadow registers for a bunch of hardware regs.
; Writing to these causes them to take effect more or less immediately, so these
; are copied to the hardware regs by the VBlank handler, taking effect between frames.
; They also come in handy for "resetting" them if modifying them mid-frame for raster FX.
hLCDC:: db
hSCY:: db
hSCX:: db
hBGP:: db
hOBP0:: db
hOBP1:: db

; Keys that are currently being held, and that became held just this frame, respectively.
; Each bit represents a button, with that bit set == button pressed
; Button order: Down, Up, Left, Right, Start, select, B, A
; U+D and L+R are filtered out by software, so they will never happen
hHeldKeys:: db
hPressedKeys:: db

; If this is 0, pressing SsAB at the same time will not reset the game
hCanSoftReset:: db

; The transfer will be started by the VBlank handler when [hVRAMTransferDestHigh] != $00
; The VBlank handler will write $00 back
hVRAMTransferSrcBank::  db
hVRAMTransferSrcLow::   db
hVRAMTransferSrcHigh::  db
hVRAMTransferDestBank:: db
hVRAMTransferDestLow::  db
hVRAMTransferDestHigh:: db
hVRAMTransferLen::      db

hVBK: db ; [rVBK] gets saved to here when doing a VRAM transfer
