MACRO MEMCPY
	ld hl, \1 
	ld bc, \2 
	ld de, \3 
	call Memcpy
ENDM

MACRO MEMSET
	ld hl, \1 
	ld c, \2
	ld de, \1 + \3 ; Destination end (because instead size is passed in \3)
	call Memset
ENDM

SECTION "Utils", ROM0
Memcpy: ; hl: destination; bc: source; de: source end
	ld a, [bc]
	ld [hl+], a
	inc bc

	ld a, c
	cp e
	jr nz, Memcpy
	ld a, b
	cp d
	jr nz, Memcpy
	ret
Memset: ; hl: destination; de: destination end; c: value to set
	ld a, c
	ld [hl+], a
	ld a, l
	cp e
	jr nz, Memset
	ld a, h
	cp d
	jr nz, Memset
	ret
UpdateInputNibble: ; a: input group select
  	ldh [rP1], a
  	call .knownret 
  	ldh a, [rP1] 
  	ldh a, [rP1]
  	ldh a, [rP1] 
  	or $F0 
.knownret
 	ret

MACRO UPDATE_INPUT
	ldh a, [KEY_STATE]
	ldh [PREV_KEY_STATE], a

  	ld a, P1F_GET_BTN
  	call UpdateInputNibble
  	ld l, a 

  	ld a, P1F_GET_DPAD
  	call UpdateInputNibble
  	swap a
  	xor l 
	ldh [KEY_STATE], a

  	ld a, P1F_GET_NONE
  	ldh [rP1], a
ENDM

MACRO CHECK_KEY_PRESS ; \1: key, \2: function to call
	ldh a, [KEY_STATE]
	bit \1, a
	jr z, .noPress\@
	ldh a, [PREV_KEY_STATE]
	bit \1, a
	call z, \2
.noPress\@:
ENDM

MACRO START_GDMA ; \1: destination; \2: source; \3: number of bytes
	ld a, HIGH(\2)
	ldh [rHDMA1], a
	ld a, LOW(\2)
	ldh [rHDMA2], a
	ld a, HIGH(\1)
	ldh [rHDMA3], a
	ld a, LOW(\1)
	ldh [rHDMA4], a 
	ld a, (\3 >> 4) - 1
	ldh [rHDMA5], a
ENDM