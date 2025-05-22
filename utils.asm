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