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

ConvertToBCD3: ; hl -> bc
	ld bc, $FF64 ; b = -1, c = 100
.countHundreds:
	inc b
	ld a, l
	sub c
	ld l, a
	ld a, h
	sbc 0
	ld h, a
	jr nc, .countHundreds
	ld a, l
	add c
	ld c, -1
.countTenths:
	inc c
	sub 10
	jr nc, .countTenths
	swap c
	add 10
	or c
	ld c, a
	ret

Mul8x8: ; b x c -> hl
	ld hl, 0
	ld a, b
	and a
	ret z
	ld d, 0
	ld e, b
.loop
	bit 0, c
	jr z, .skipAdd
	add hl, de
.skipAdd
	sla e
	rl d
	srl c
	jr nz, .loop
	ret
	
UpdateKeys:
	MACRO readNibble
		ldh [rP1], a
		push af
		pop af
		ldh a, [rP1] 
		or $F0 
	ENDM

  	ld a, P1F_GET_BTN
	readNibble()
  	ld l, a 

	ld a, P1F_GET_DPAD
	readNibble()
  	swap a
  	xor l 
	ld l, a
	ldh a, [KEY_STATE]
	xor l
	ld h, a
	and l
	ldh [PRESSED_KEYS], a
	xor h
	ldh [RELEASED_KEYS], a
	ld a, l
	ldh [KEY_STATE], a

  	ld a, P1F_GET_NONE
  	ldh [rP1], a
	ret

MACRO START_GDMA ; \1: destination; \2: source; \3: number of bytes
	ld hl, rHDMA1
	ld a, HIGH(\2)
	ld [hl+], a
	ld a, LOW(\2)
	ld [hl+], a
	ld a, HIGH(\1)
	ld [hl+], a
	ld a, LOW(\1)
	ld [hl+], a
	ld [hl], (\3 >> 4) - 1
ENDM