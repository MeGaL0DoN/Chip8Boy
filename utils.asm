MACRO MEMSET ; \3: size
	ld hl, \1 
	ld a, \2
	ld bc, ((((\3) + 255) / 256) << 8) | ((\3) % 256)  
	rst Memset
ENDM

MACRO MEMCPY ; \3: source end
    ld de, \1
    ld hl, \2
    ld bc, ((((\3) - (\2) + 255) / 256) << 8) | (((\3) - (\2)) % 256)
    rst Memcpy
ENDM

MACRO MEMCPY_1BIT_TILES ; \3: source end
	ld hl, \1 
	ld de, \2 
	ld bc, ((((\3) - (\2) + 255) / 256) << 8) | (((\3) - (\2)) % 256) 
	call Memcpy1BitTiles
ENDM

SECTION "Memset", ROM0[$0008]
Memset: ; hl: destination; a: value to set; c: len % 256, b: (len + 255) / 256
	ld [hl+], a
	dec c
	jr nz, Memset
	dec b
	jr nz, Memset
	ret
SECTION "Memcpy", ROM0[$0010]
Memcpy: ; de: destination; hl: source; c: len % 256, b: (len + 255) / 256
	ld a, [hl+]
	ld [de], a
	inc de
	dec c
	jr nz, Memcpy
	dec b
	jr nz, Memcpy
	ret

SECTION "Utils", ROM0
; sets every second byte to $FF
Memcpy1BitTiles: ; hl: destination; de: source; c: len % 256, b: (len + 255) / 256
	ld a, [de]
	ld [hl+], a
	ld a, $FF
	ld [hl+], a
	inc de
	dec c
	jr nz, Memcpy1BitTiles
	dec b
	jr nz, Memcpy1BitTiles
	ret

ConvertToBCD3: ; hl -> bc
	ld de, $FF9C ; de = -100
	ld b, d ; b = -1
.countHundreds:
	inc b
	add hl, de
	jr c, .countHundreds
	ld a, l
	add 100
	ld c, d ; c = -1
	ld e, 10
.countTenths:
	inc c
	sub e
	jr nc, .countTenths
	swap c
	add e
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
	srl c
	jr nc, .skipAdd
	add hl, de
.skipAdd
	ret z 
	sla e
	rl d
	jr .loop
	
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
; RNG_STATE -> ac
MACRO RAND
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
ENDM

; \1 - table address, 256 byte aligned. index is passed in 'a'
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
MACRO RUN_GDMA 
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

MACRO SAFE_HALT
	xor a
	ldh [rIF], a
	halt
ENDM

; \1: target
MACRO JZ_TRY
    ; try to compute the offset that JR would encode
    IF ISCONST(\1 - (@ + 2))
        IF (\1 - (@ + 2)) >= -128 && (\1 - (@ + 2)) <= 127
            jr z, \1
        ELSE
            jp z, \1
        ENDC
    ELSE
        ; can't know distance at assembly time -> use safe form
        jp z, \1
    ENDC
ENDM