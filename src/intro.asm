
SECTION "Intro", ROMX

Intro::
	ldh a, [hConsoleType]
	and a ; Check if zero ⇔ being CGB
	jp nz, DMGLockout

	; TODO
	ret


DMGLockout:
	; TODO
.lockup
	rst WaitVBlank
	jr .lockup
