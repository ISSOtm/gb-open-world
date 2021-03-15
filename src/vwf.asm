
INCLUDE "hardware.inc/hardware.inc"

SKIP_HELD_KEYS equ PADF_B
SKIP_PRESSED_KEYS equ PADF_A

NB_CHARSETS equ 1
CHARSET_0 equs "res/optix.vwf"

lb: MACRO
	assert -128 <= (\2) && (\2) <= 255, "Second argument to `lb` must be 8-bit!"
	assert -128 <= (\3) && (\3) <= 255, "Third argument to `lb` must be 8-bit!"
	ld \1, ((\2) << 8) | (\3)
ENDM

INCLUDE "gb-vwf/vwf.asm"
