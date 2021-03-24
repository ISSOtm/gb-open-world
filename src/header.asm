
INCLUDE "defines.asm"


SECTION "Header", ROM0[$100]

	; This is your ROM's entry point
	; You have 4 bytes of code to do... something
	sub $11 ; This helps check if we're on CGB more efficiently
	jr EntryPoint

	; Make sure to allocate some space for the header, so no important
	; code gets put there and later overwritten by RGBFIX.
	; RGBFIX is designed to operate over a zero-filled header, so make
	; sure to put zeros regardless of the padding value. (This feature
	; was introduced in RGBDS 0.4.0, but the -MG etc flags were also
	; introduced in that version.)
	ds $150 - @, 0

EntryPoint:
	ldh [hConsoleType], a

Reset::
	di ; Disable interrupts while we set up

	; Kill sound
	xor a
	ldh [rNR52], a

	; Wait for VBlank and turn LCD off
.waitVBlank
	ldh a, [rLY]
	cp SCRN_Y
	jr c, .waitVBlank
	xor a
	ldh [rLCDC], a
	; Goal now: set up the minimum required to turn the LCD on again
	; A big chunk of it is to make sure the VBlank handler doesn't crash

	ld sp, wStackBottom

	ld a, BANK(OAMDMA)
	; No need to write bank number to HRAM, interrupts aren't active
	ld [rROMB0], a
	ld hl, OAMDMA
	lb bc, OAMDMA.end - OAMDMA, LOW(hOAMDMA)
.copyOAMDMA
	ld a, [hli]
	ldh [c], a
	inc c
	dec b
	jr nz, .copyOAMDMA

	ld a, $E4
	ldh [hBGP], a
	ldh [hOBP0], a
	ldh [hOBP1], a

	; Reset variables necessary for the VBlank handler to function correctly
	; But only those for now
	xor a
	ldh [hVBlankFlag], a
	ldh [hOAMHigh], a
	ldh [hCanSoftReset], a
	ldh [hVRAMTransferDestHigh], a
	dec a ; ld a, $FF
	ldh [hHeldKeys], a

	; Load the correct ROM bank for later
	; Important to do it before enabling interrupts
	ld a, BANK(Intro)
	ldh [hCurROMBank], a
	ld [rROMB0], a

	; If on CGB and not in double-speed mode, switch to double-speed mode
	; We might be in double-speed already if soft-resetting
	ldh a, [hConsoleType]
	and a ; If not 0, we're not on a CGB
	jr nz, .noSpeedSwitch
	ldh a, [rKEY1]
	add a, a ; Bit 7 set for double-speed
	jr c, .noSpeedSwitch ; We are already in double-speed, don't switch
	ld a, $30
	ldh [rP1], a
	xor a
	ldh [rIE], a
	inc a ; ld a, 1
	ldh [rKEY1], a
	stop ; Perform speed switch
.noSpeedSwitch

	; Select wanted interrupts here
	; You can also enable them later if you want
	ld a, IEF_VBLANK
	ldh [rIE], a
	xor a
	ei ; Only takes effect after the following instruction
	ldh [rIF], a ; Clears "accumulated" interrupts

	; Init shadow regs
	; xor a
	ldh [hSCY], a
	ldh [hSCX], a
	ld a, LCDCF_ON | LCDCF_BGON
	ldh [hLCDC], a
	; And turn the LCD on!
	ldh [rLCDC], a

	; Clear OAM, so it doesn't display garbage
	; This will get committed to hardware OAM after the end of the first
	; frame, but the hardware doesn't display it, so that's fine.
	ld hl, wShadowOAM
	ld c, NB_SPRITES * 4
	xor a
	rst MemsetSmall
	ld a, h ; ld a, HIGH(wShadowOAM)
	ldh [hOAMHigh], a

	; `Intro`'s bank has already been loaded earlier
	call Intro


	;; Main game

	; Screen redrawing expects to run over already-loaded palettes;
	; Thus, we need to init the dynamic palette array to "all free"
	ld hl, wBGPaletteIDs
	ld c, wBGPaletteIDs.end - wBGPaletteIDs
	xor a
	rst MemsetSmall

	; Temporary var init while I write the code
	; Set camera position to $0808 (128.5)
	ld a, $08
	ld [wCameraYPos], a
	ld [wCameraYPos + 1], a
	ld [wCameraXPos], a
	ld [wCameraXPos + 1], a

	xor a
	call LoadMap
	call RedrawScreen

	; FIXME: temporary
	ld a, [wCameraYPos]
	ld l, a
	ld a, [wCameraYPos + 1]
	xor l
	and $0F
	xor l
	swap a
	ldh [hSCY], a
	ld a, [wCameraXPos]
	ld l, a
	ld a, [wCameraXPos + 1]
	xor l
	and $0F
	xor l
	swap a
	ldh [hSCX], a

	; Compute the palette mask from which slots are loaded
	ld hl, wBGPaletteIDs
	lb bc, 0, 8
	; Carry currently clear (`swap a`)
.checkSlot
	ld a, [hli]
	cp 1
	jr z, .slotIsFree
	scf
.slotIsFree
	rl b ; Shift carry in, and also clear carry
	dec c
	jr nz, .checkSlot
	ld hl, wFadeSteps
	xor a
	ld [wOBJPaletteMask], a
	ld [hli], a ; wFadeSteps
	ld [hli], a ; wFadeDelta
	ld a, $80
	ld [hli], a ; wFadeAmount
	ld [hl], b ; wBGPaletteMask
	call FadePaletteBuffers

.lockup
	rst WaitVBlank
	jr .lockup


SECTION "OAM DMA routine", ROMX

; OAM DMA prevents access to most memory, but never HRAM.
; This routine starts an OAM DMA transfer, then waits for it to complete.
; It gets copied to HRAM and is called there from the VBlank handler
OAMDMA:
	ldh [rDMA], a
	ld a, NB_SPRITES
.wait
	dec a
	jr nz, .wait
	ret
.end

SECTION "Global vars", HRAM

; 0 if CGB (including DMG mode and GBA), non-zero for other models
hConsoleType:: db

; Copy of the currently-loaded ROM bank, so the handlers can restore it
; Make sure to always write to it before writing to ROMB0
; (Mind that if using ROMB1, you will run into problems)
hCurROMBank:: db


SECTION "OAM DMA", HRAM

hOAMDMA::
	ds OAMDMA.end - OAMDMA


SECTION UNION "Shadow OAM", WRAM0,ALIGN[8]

wShadowOAM::
	ds NB_SPRITES * 4


; This ensures that the stack is at the very end of WRAM
SECTION "Stack", WRAM0[$E000 - STACK_SIZE]

	ds STACK_SIZE
wStackBottom:


; This is a buffer to be used by functions for variables that will not outlive their scope
; Be careful when calling a function from another module, check that it won't overwrite anything stored here!
SECTION UNION "Scratch buffer", HRAM


; Some "utility" VRAM sections, so that their constraints only need to be specified once
SECTION UNION "0:8000 tile block", VRAM[$8000],BANK[0]
SECTION UNION "1:8000 tile block", VRAM[$8000],BANK[1]
SECTION UNION "0:8800 tile block", VRAM[$8800],BANK[0]
SECTION UNION "1:8800 tile block", VRAM[$8800],BANK[1]
SECTION UNION "0:9000 tile block", VRAM[$9000],BANK[0]
SECTION UNION "1:9000 tile block", VRAM[$9000],BANK[1]
SECTION UNION "0:9800 tilemap", VRAM[$9800],BANK[0]
SECTION UNION "1:9800 attrmap", VRAM[$9800],BANK[1]
SECTION UNION "0:9C00 tilemap", VRAM[$9C00],BANK[0]
SECTION UNION "1:9C00 attrmap", VRAM[$9C00],BANK[1]
