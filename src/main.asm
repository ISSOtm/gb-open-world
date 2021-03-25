
INCLUDE "defines.asm"


SECTION "Main loop", ROM0

Main::
	;; Init engine

	; Screen redrawing expects to run over already-loaded palettes;
	; Thus, we need to init the dynamic palette array to "all free"
	ld hl, wBGPaletteIDs
	ld c, wBGPaletteIDs.end - wBGPaletteIDs - 1
	xor a
	rst MemsetSmall
	; Reserve palette #7 for UI (clear bit 1 so it doesn't get faded)
	ld [hl], $05


	; Temporary var init while I write the code
	; Set camera position to $0808 (128.5)
	ld a, $01
	ld [wCameraYPos], a
	ld [wCameraYPos + 1], a
	ld [wCameraXPos], a
	ld [wCameraXPos + 1], a
	; Load map 0
	xor a
	ld [wNextMap], a


	; Begin the main loop by fading in
	ld a, STATE_LOADMAP
	ld [wCurState], a
	xor a
	ld [wOBJPaletteMask], a
	; Pretend that we just finished fading to white
	inc a ; ld a, 1
	ld [wFadeDelta], a
	ld a, $FF
	ld [wFadeAmount], a


MainLoop:
	;; Run state's init func, if any
	ld a, [wCurState]
	assert NB_STATES <= 128
	add a, a
	jr c, .noStateInit
	assert LOW(StateInitPtrs) == 0
	ld l, a
	ld h, HIGH(StateInitPtrs)
	ld a, BANK(StateInitPtrs)
	ldh [hCurROMBank], a
	ld [rROMB0], a
	call JumpToPtr
	ld hl, wCurState
	set 7, [hl]
.noStateInit


	;; Process state's "update" function
	ld a, [wCurState]
	assert NB_STATES <= 128
	add a, a
	assert LOW(StatePtrs) == 0
	ld l, a
	ld h, HIGH(StatePtrs)
	ld a, BANK(StatePtrs)
	ldh [hCurROMBank], a
	ld [rROMB0], a
	call JumpToPtr


	;; Move scrolling to new camera position
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


	;; Wait for next frame
	rst WaitVBlank


	jp MainLoop


SECTION FRAGMENT "State pointers", ROMX

StatePtrs: align 8

SECTION FRAGMENT "State init pointers", ROMX

StateInitPtrs: align 8


NB_STATES = 0
MACRO register_state
	SECTION FRAGMENT "State pointers", ROMX
		dw \1StateUpdate
	SECTION FRAGMENT "State init pointers", ROMX
		dw \1StateInit

	DEF UPPERCASE_NAME equs STRUPR("\1")
	DEF STATE_{UPPERCASE_NAME} equ NB_STATES
	REDEF NB_STATES = NB_STATES + 1
	PURGE UPPERCASE_NAME

	SECTION "\1 state", ROM0
ENDM
; State definitions are at the end of this file



SECTION "Main loop state variables", WRAM0

; FIII IIII
; F = If reset, this is the first frame the state is running
; I = ID of the current main loop state
; Do ***NOT*** change this in the init func!!!
wCurState:: db



;; State definitions
; Note that each of these files may define more than one state

INCLUDE "main/states/normal.asm"
INCLUDE "main/states/fade.asm"
INCLUDE "main/states/load_map.asm"
