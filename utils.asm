MACRO MEMCPY
	ld hl, \1 ; Destination
	ld bc, \2 ; Source
	ld de, \3 ; Source end
.loop\@:
	ld a, [bc]
	ld [hl+], a
	inc bc

	ld a, c
	cp e
	jr nz, .loop\@
	ld a, b
	cp d
	jr nz, .loop\@
ENDM

MACRO MEMSET
	ld hl, \1 ; Destination
	ld de, \1 + \3 ; Destination end (because instead size is passed in \3)
.loop\@:
	IF \2 == 0 ; Value to set
		xor a
	ELSE
		ld a, \2
	ENDC
	ld [hl+], a

	ld a, l
	cp e
	jr nz, .loop\@
	ld a, h
	cp d
	jr nz, .loop\@
ENDM