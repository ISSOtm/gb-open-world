
	register_state Fade


FadeStateUpdate:
	call FadePaletteBuffers

	; If we're done fading, switch to next state
	ld a, [wFadeSteps]
	and a
	ret nz
	ld a, [wAfterFadeState]
	ld [wCurState], a

FadeStateInit:
	ret


SECTION "Fade state vars", WRAM0

; Which state will be switched to after fading is done
wAfterFadeState: db
