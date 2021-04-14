
	register_state Normal


NormalStateInit:
	; Since we may be coming from fading in, which makes the camera static, allow camera movement
	ld a, CAM_FOLLOW_PLAYER
	ld [wCameraMovtType], a
	ret

NormalStateUpdate:
	; TODO

	; Temporarily, allow controlling the camera's focus point directly
	ld hl, wCameraTargetYPos
	ldh a, [hHeldKeys]
	add a, a
	jr nc, .notDown
	ld a, [hl]
	add a, 1 * 16 ; Remember, there are 16 subpixels per pixel!
	ld [hli], a
	jr nc, :+
	inc [hl]
	jr :+

.notDown
	add a, a
	jr nc, .notUp
	ld a, [hl]
	sub 1 * 16
	ld [hli], a
	jr nc, :+
	dec [hl]
	db $FE

.notUp
	inc hl ; Skip Y pos low
:
	inc hl ; Skip Y pos high

	ldh a, [hHeldKeys]
	bit PADB_RIGHT, a
	jr z, .notRight
	ld a, [hl]
	add a, 1 * 16
	ld [hli], a
	ret nc
	inc [hl]
	ret

.notRight
	bit PADB_LEFT, a
	ret z
	ld a, [hl]
	sub 1 * 16
	ld [hli], a
	ret nc
	dec [hl]
	ret
