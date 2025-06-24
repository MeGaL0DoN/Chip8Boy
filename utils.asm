MACRO MEMCPY
	ld hl, \1 
	ld de, \2 
	ld bc, \3 
	call Memcpy
ENDM

MACRO MEMCPY_1BIT_TILES
	ld hl, \1 
	ld de, \2 
	ld bc, \3 
	call Memcpy1BitTiles
ENDM

MACRO MEMSET
	ld hl, \1 
	ld e, \2
	ld bc, \1 + \3 ; Destination end (because instead size is passed in \3)
	call Memset
ENDM

SECTION "Utils", ROM0
Memcpy: ; hl: destination; de: source; bc: source end
	ld a, [de]
	ld [hl+], a
	inc de

	ld a, c
	cp e
	jr nz, Memcpy
	ld a, b
	cp d
	jr nz, Memcpy
	ret
Memcpy1BitTiles: ; hl: destination; de: source; bc: source end. Sets every second byte to $FF.
	ld a, [de]
	ld [hl+], a
	ld a, $FF
	ld [hl+], a
	inc de

	ld a, c
	cp e
	jr nz, Memcpy1BitTiles
	ld a, b
	cp d
	jr nz, Memcpy1BitTiles
	ret
Memset: ; hl: destination; bc: destination end; e: value to set
	ld a, e
	ld [hl+], a

	ld a, c
	cp l
	jr nz, Memset
	ld a, b
	cp h
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
	ld d, l
	ld e, b
	ld a, b
	cp c
	jr nc, .loop
	; b is smaller than c:
	ld e, c
	ld c, b
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

  	ld a, JOYP_GET_BUTTONS
	readNibble()
  	ld l, a 

	ld a, JOYP_GET_CTRL_PAD
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

  	ld a, JOYP_GET_NONE
  	ldh [rP1], a
	ret

SECTION "RNG", WRAM0
RNG_STATE:
	ds 4

; Taken from: https://gbdev.io/gb-asm-tutorial/cheatsheet.html#generate-random-numbers
MACRO RAND ; RNG_STATE -> bc
	ld hl, RNG_STATE
  	ld a, [hl]
  	add $B3
  	ld [hl+], a
  	adc [hl]
  	ld [hl+], a
  	adc [hl]
  	ld [hl+], a
  	ld c, a
  	adc [hl]
  	ld [hl], a
  	ld b, a
ENDM

; \1 - table address, 256 byte aligned. index is passed in 'a'.
MACRO JP_TABLE ; 
	add a
	ld h, HIGH(\1)
	ld l, a

	ld a, [hl+]
	ld h, [hl]
	ld l, a
	jp hl
ENDM

; \1: destination; \2: source; \3: number of bytes
MACRO START_GDMA 
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

MACRO WAIT_VRAM_ACCESS
.wait\@
	ldh a, [rSTAT]
	bit 1, a
	jr nz, .wait\@
ENDM