
	register_state LoadMap


LoadMapStateInit:
	ld a, [wNextMap]
	call LoadMap

	jp RedrawScreen


LoadMapStateUpdate:
	; Do this here instead of the init func because you shouldn't switch states in the init
	ld a, STATE_FADE
	ld [wCurState], a
	ld a, STATE_NORMAL
	ld [wAfterFadeState], a

	; Since we'll be fading out, we'll need to disable camera movement (lest the fade params
	; will be overwritten!)
	ld a, CAM_STATIC
	ld [wCameraMovtType], a

	; Use that time to compute fade-in params
	; Compute the palette mask from which slots are loaded
	ld hl, wBGPaletteIDs
	lb bc, 0, 8
.checkSlot
	ld a, [hli]
	rra
	jr nc, .normalSlot ; Slot isn't free or reserved, so
	rra ; Use bit 1 as-is; this also sets Z to 0
	db $28 ; jr z, imm8
.normalSlot
	ccf
	rl b ; Shift carry in
	dec c
	jr nz, .checkSlot
	ld a, b
	ld [wBGPaletteMask], a
	; Invert the fade direction
	ld a, [wFadeDelta]
	cpl
	inc a
	ld [wFadeDelta], a
	; TODO: compute the fade step count instead of hardcoding like this
	ld a, $7F
	ld [wFadeSteps], a
	ret


SECTION "Load map state vars", WRAM0

wNextMap: db
