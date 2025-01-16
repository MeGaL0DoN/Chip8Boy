INCLUDE "hardware.inc"

MACRO LD_VX_HL
    ld hl, V0
    ld a, b
    and a, $0F

	add a, l
	ld l, a
	adc a, h
	sub l
	ld h, a
ENDM

MACRO LD_VY_HL
    ld hl, V0
    ld a, c
    and a, $F0
    swap a

	add a, l
	ld l, a
	adc a, h
	sub l
	ld h, a
ENDM

MACRO LD_NNN
	ld a, b
	and a, $0F
	ld b, a
ENDM

SECTION "VBlank Handler", ROM0[$0040]
    ret

SECTION "chipState", WRAM0
CHIP_STATE:
STACK:
	ds 32
CHIP_SP:
	ds 1
I_REG:
	ds 2
DELAY_TIMER:
	ds 1
SOUND_TIMER:
	ds 1
V0:
	ds 16
SCREEN_BUF:
	ds 2048
SCREEN_BUF_END:
DRAW_FLAG:
	ds 1

CHIP_STATE_END:

SECTION "chipRAM", WRAMX
CHIP_RAM:
	ds 4096

SECTION "Header", ROM0[$100]
	jp EntryPoint
	ds $150 - @, 0 ; Make room for the header

EntryPoint:
	; Shut down audio circuitry
	xor a
	ld [rNR52], a

	; set IE to vblank enable
	ld a, IEF_VBLANK
	ld [rIE], a

	; Waiting for vblank and disabling lcd to copy tile map
	ei 
	halt 
	xor a
	ld [rLCDC], a

	ld hl, $9800
	ld de, TileMap
	ld bc, TileMapEnd - TileMap
.CopyTileMap:
	ld a, [de]
	ld [hl+], a

	inc de
	dec bc
	ld a, b
	or a, c
	jr nz, .CopyTileMap

	; Setting chip8 screen to black.
	ld hl, _VRAM8000 + 16 ; First tile is reserved blank
	ld bc, SCREEN_BUF
	ld de, 32 * 16

.SetTiles:
	ld a, $FF
	ld [hl+], a
	dec de
	ld a, d
	or a, e
	jr nz, .SetTiles

	ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8000
	ld [rLCDC], a

	; Copy chip8 rom to ram
	ld bc, CHIP_ROM
	ld hl, CHIP_RAM + $200
	ld de, CHIP_ROM_END - CHIP_ROM
.CopyChipROM:
	ld a, [bc]
	ld [hl+], a

	inc bc
	dec de
	ld a, d
	or a, e
	jp nz, .CopyChipROM

	; Resetting state
	ld hl, CHIP_STATE
	ld bc, CHIP_STATE_END - CHIP_STATE
.ZeroState:
	xor a
	ld [hl+], a
	dec bc
	ld a, b
	or a, c
	jr nz, .ZeroState

	; Reset PC
	ld bc, $200
	push bc

InstrLoop:
	; loading chip8 PC to BC
	pop bc

	; loading chip8 mem pointer to HL
	ld hl, CHIP_RAM
	add hl, bc

	; Storing pc + 2 back
	inc bc
	inc bc
	push bc

	; Loading chip8 instruction to BC
	ld a, [hl+]
	ld b, a
	ld c, [hl]

	; Loading jump table address to hl based on first nibble of BC
	ld a, b
	and a, $F0
	swap a
	ld l, a
	ld h, 0
	add hl, hl ; addresses are 2 bytes, so multiply by 2

	; Loading address of the function and jumping to it.
	ld de, MainJumpTable
	add hl, de
	ld a, [hl+]   
    ld h, [hl]    
    ld l, a
    jp hl  

InstrDecodeEnd:
	; Dont render if draw flag is not set
	xor a
	ld hl, DRAW_FLAG
	or a, [hl]
	jp z, InstrLoop

	ld [hl], 0 ; reset draw flag
	
	; Waiting for vblank and disabling lcd
	ld [rIF], a ; A is still zero, resetting IF to clear vblank if its set.
	ei
	halt 
	ld [rLCDC], a

	; Converting chip8 screenbuf to tiles

	ld bc, SCREEN_BUF
	ld hl, _VRAM8000 + 16 ; First tile is reserved blank
	push hl

	; Row counter variable
	ld de, 2
	push de

.HEIGHT_LOOP:
	ld e, 8
.WIDTH_LOOP:
FOR i, 0, 8
	ld a, [bc] ; Load screen buf pixel
	and a, a ; Check if zero
	ld a, [hl]
	jr z, .OffPixel\@

	and a, ~(%10000000 >> i)
	ld [hl+], a
	ld a, [hl]	
	and a, ~(%10000000 >> i)

	jr .End\@
.OffPixel\@
	or a, %10000000 >> i
	ld [hl+], a
	ld a, [hl]	
	or a, %10000000 >> i
.End\@
	ld [hl-], a
	inc bc
ENDR
	; Adding 16 bytes to move to next horizontal tile
	ld a, e
	ld de, 16
	add hl, de
	ld e, a

	dec e
	jp nz, .WIDTH_LOOP    

	; Moving to the next row, and adding 2 to row counter variable
	pop de
	pop hl

	ld a, e
	cp a, $10
	jr z, .NewTileRow

	add sp, -2 ; Dont overwrite hl
	add hl, de
	inc e
	inc e
	push de
	jp .HEIGHT_LOOP
.NewTileRow:
	ld e, $80
	add hl, de

	ld a, h
	cp a, (_VRAM8000 + 16 + (32 * 16)) >> 8
	jr nz, .Continue
	ld a, l
	cp a, (_VRAM8000 + 16 + (32 * 16)) & $FF
	jr z, .End

.Continue:
	push hl
	ld e, 2
	push de
	jp .HEIGHT_LOOP

.End:
	ld de, SCREEN_BUF_END 

	ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8000
	ld [rLCDC], a
	jp InstrLoop

;; Instruction decoding, table for matching on first nibble
Case0:
	; if first nibble is not zero, instruction is invalid.
	and a, b
	jp nz, InstrDecodeEnd

	ld a, c
	cp a, $E0
	jr z, OP_00E0
	cp a, $EE
	jp nz, InstrDecodeEnd ; Invalid instruction

OP_00EE: ; Ret from function
	ld hl, CHIP_SP
	ld a, [hl]
	; Updating SP
	sub a, 2
	and a, $1F
	ld [hl], a

	; Loading address from the stack into BC
	; Not setting B to 0, since its guaranteed (opcode starts with 00).
	ld c, a
	ld hl, STACK
	add hl, bc
	ld a, [hl+]
	ld c, a
	ld b, [hl]

	; Updating PC
	pop af
	push bc

	jp InstrDecodeEnd

OP_00E0: ; Clear screen | REWRITE AFTER DXYN!!!
	ld hl, SCREEN_BUF
	ld de, 2048

ClearBufLoop:
	xor a
	ld [hl+], a
	dec de
	ld a, d
	or a, e
	jr nz, ClearBufLoop

	jp InstrDecodeEnd

Case1: 
OP_1NNN: ; Jump NNN
	LD_NNN
	pop af
	push bc
	
	jp InstrDecodeEnd

Case2:
OP_2NNN: ; Call function
	ld hl, CHIP_SP
	ld e, [hl]

	; Updating SP (Dont use the updated sp now though.)
	ld a, e
	add a, 2
	and a, $1F
	ld [hl], a

	; Save SP (E) to L and load current PC to DE
	ld l, e
	pop DE

	; Setting PC to new address.
	LD_NNN
	push bc

	; Now save DE to the stack
	ld b, 0
	ld c, l ; loading saved sp
	ld hl, STACK
	add hl, bc
	ld a, e
	ld [hl+], a
	ld [hl], d

	jp InstrDecodeEnd

Case3:

MACRO COND_PC_ADD 
	IF \1 == 1
		jp nz, InstrDecodeEnd
	ELSE
		jp z, InstrDecodeEnd
	ENDC

	; Adding 2 to pc
	pop hl
	ld de, 2
	add hl, de
	push hl
ENDM

OP_3XNN:
	LD_VX_HL
	ld a, [hl]
	cp a, c

	COND_PC_ADD 1 ; Check if equal
	jp InstrDecodeEnd

Case4:
OP_4XNN:
	LD_VX_HL
	ld a, [hl]
	cp a, c

	COND_PC_ADD 0 ; Check if not equal
	jp InstrDecodeEnd

Case5:
OP_5XY0:
	ld a, c
	and a
	jp nz, InstrDecodeEnd ; Invalid instruction

	LD_VY_HL
	ld e, [hl]
	LD_VX_HL
	ld a, [hl]

	cp a, e
	COND_PC_ADD 1
	jp InstrDecodeEnd

Case6:
OP_6XNN:
	LD_VX_HL
	ld [hl], c
	jp InstrDecodeEnd

Case7:
OP_7XNN:
	LD_VX_HL
	ld a, [hl]
	add a, c
	ld [hl], a

	jp InstrDecodeEnd

Case8:
	jp InstrDecodeEnd

Case9:
OP_9XY0:
	ld a, c
	and a
	jp nz, InstrDecodeEnd ; Invalid instruction

	LD_VY_HL
	ld e, [hl]
	LD_VX_HL
	ld a, [hl]

	cp a, e
	COND_PC_ADD 0
	jp InstrDecodeEnd

CaseA: 
OP_ANNN:
	ld hl, I_REG

	ld a, c
	ld [hl+], a

	ld a, b
	and a, $0F
	ld [hl], a
	
	jp InstrDecodeEnd
	
CaseB:
OP_BNNN: ; Quirk off, jump to V[0] + NNN
	ld hl, V0
	LD_NNN

	ld l, [hl]
	ld h, 0

	add hl, bc
	; update PC
	pop af
	push hl

	jp InstrDecodeEnd

CaseC:
OP_CXNN: ; RNG, TODO. Use DIV timer / joypad?
	jp InstrDecodeEnd

CaseD: ; DXYN!!! ; REWRIITE!;  TODO: collision and clipping

	; Loading X to E
	LD_VX_HL
	ld a, [hl]
	and a, 64 - 1
	ld e, a

	; Saving Y on the stack.
	LD_VY_HL
	ld a, [hl]
	and a, 32 - 1
	ld hl, sp - 1
	ld [hl], a
	dec sp

	; No longer need to save BC register, storing height - 1 into c.
	ld a, c
	and a, $0F
	jp z, .DXYN_END
	dec a
	ld c, a

	ld b, 0 ; Width loop counter

.HEIGHT_LOOP:	
	; Loading sprite data byte into reg D
	push de
	ld hl, I_REG
	ld e, [hl]
	inc hl
	ld d, [hl]
	ld hl, CHIP_RAM
	add hl, de
	add hl, bc ; B is zero (width counter was reset), C is current height counter

	pop de
	ld d, [hl]

	; Loading initial Y into hl
	ld hl, sp + 0
	ld l, [hl]
	ld h, 0

	; Setting HL to screenBuf[(y + row) * 64 + x]
	add hl, bc

	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl

	push de
	ld d, 0
	add hl, de ; Adding X
	ld de, SCREEN_BUF
	add hl, de
	pop de
	;;;

	ld b, $80 ; Setting width loop counter (msb set)

.WIDTH_LOOP:
	ld a, d ; Loading sprite data
	and a, b
	jr z, .WIDTH_LOOP_END
	
	ld a, [hl]
	xor a, 1
	ld [hl], a

.WIDTH_LOOP_END:
	inc hl ; Point to next screen buf pixel.
	srl b
	jr nz, .WIDTH_LOOP

	ld a, c
	sub a, 1
	ld c, a
	jr nc, .HEIGHT_LOOP

	ld a, 1
	ld [DRAW_FLAG], a

.DXYN_END:
	inc sp ; restoring sp || WHEN ADD COLLISIONG FLAG CHANGE TO ADD, 2
	jp InstrDecodeEnd
	
CaseE:
	jp InstrDecodeEnd
	
CaseF:
	jp InstrDecodeEnd

MainJumpTable:
    dw Case0, Case1, Case2, Case3, Case4, Case5, Case6, Case7,
    dw Case8, Case9, CaseA, CaseB, CaseC, CaseD, CaseE, CaseF   


SECTION "Chip8 ROM", ROM0
CHIP_ROM:
	db 18, 10, 96, 1, 0, 238, 96, 2, 18, 166, 0, 224, 104, 50, 107, 26, 164, 241,
	db 216, 180, 104, 58, 164, 245, 216, 180, 104, 2, 105, 6, 106, 11, 107, 1, 101,
    db 42, 102, 43, 164, 181, 216, 180, 164, 237, 217, 180, 164, 165, 54, 43, 164,
	db 161, 218, 180, 107, 6, 164, 185, 216, 180, 164, 237, 217, 180, 164, 161, 69,
	db 42, 164, 165, 218, 180, 107, 11, 164, 189, 216, 180, 164, 237, 217, 180,
	db 164, 161, 85, 96, 164, 165, 218, 180, 107, 16, 164, 197, 216, 180, 164, 237,
	db 217, 180, 164, 161, 118, 255, 70, 42, 164, 165, 218, 180, 123, 5, 164, 205,
	db 216, 180, 164, 237, 217, 180, 164, 161, 149, 96, 164, 165, 218, 180, 123, 5,
	db 164, 173, 216, 180, 164, 237, 217, 180, 164, 165, 18, 144, 164, 161, 218,
	db 180, 104, 18, 105, 22, 106, 27, 107, 1, 164, 177, 216, 180, 164, 237, 217,
	db 180, 96, 0, 34, 2, 164, 165, 64, 0, 164, 161, 218, 180, 123, 5, 164, 169,
	db 216, 180, 164, 225, 217, 180, 164, 165, 64, 2, 164, 161, 48, 0, 218, 180,
	db 123, 5, 164, 201, 216, 180, 164, 169, 217, 180, 164, 161, 101, 42, 103, 0,
	db 135, 80, 71, 42, 164, 165, 218, 180, 123, 5, 164, 201, 216, 180, 164, 173,
	db 217, 180, 164, 161, 102, 11, 103, 42, 135, 97, 71, 43, 164, 165, 218, 180,
	db 123, 5, 164, 201, 216, 180, 164, 177, 217, 180, 164, 161, 102, 120, 103, 31,
	db 135, 98, 71, 24, 164, 165, 218, 180, 123, 5, 164, 201, 216, 180, 164, 181,
	db 217, 180, 164, 161, 102, 120, 103, 31, 135, 99, 71, 103, 164, 165, 218, 180,
	db 104, 34, 105, 38, 106, 43, 107, 1, 164, 201, 216, 180, 164, 185, 217, 180,
	db 164, 161, 102, 140, 103, 140, 135, 100, 71, 24, 164, 165, 218, 180, 123, 5,
	db 164, 201, 216, 180, 164, 189, 217, 180, 164, 161, 102, 140, 103, 120, 135,
	db 101, 71, 236, 164, 165, 218, 180, 123, 5, 164, 201, 216, 180, 164, 197, 217,
	db 180, 164, 161, 102, 120, 103, 140, 135, 103, 71, 236, 164, 165, 218, 180,
	db 123, 5, 164, 201, 216, 180, 164, 193, 217, 180, 164, 161, 102, 15, 134, 102,
	db 70, 7, 164, 165, 218, 180, 123, 5, 164, 201, 216, 180, 164, 225, 217, 180,
	db 164, 161, 102, 224, 134, 110, 70, 192, 164, 165, 218, 180, 123, 5, 164, 229,
	db 216, 180, 164, 193, 217, 180, 164, 158, 241, 101, 164, 165, 48, 170, 164,
	db 161, 49, 85, 164, 161, 218, 180, 104, 50, 105, 54, 106, 59, 107, 1, 164,
	db 229, 216, 180, 164, 189, 217, 180, 164, 158, 96, 0, 97, 48, 241, 85, 164,
	db 158, 240, 101, 129, 0, 164, 159, 240, 101, 164, 165, 48, 48, 164, 161, 49,
	db 0, 164, 161, 218, 180, 123, 5, 164, 229, 216, 180, 164, 181, 217, 180, 164,
	db 158, 102, 137, 246, 51, 242, 101, 164, 161, 48, 1, 20, 50, 49, 3, 20, 50,
	db 50, 7, 20, 50, 164, 158, 102, 65, 246, 51, 242, 101, 164, 161, 48, 0, 20,
	db 50, 49, 6, 20, 50, 50, 5, 20, 50, 164, 158, 102, 4, 246, 51, 242, 101, 164,
	db 161, 48, 0, 20, 50, 49, 0, 20, 50, 50, 4, 20, 50, 164, 165, 218, 180, 123,
	db 5, 164, 229, 216, 180, 164, 225, 217, 180, 164, 161, 102, 4, 246, 30, 218,
	db 180, 123, 5, 164, 233, 216, 180, 164, 237, 217, 180, 164, 165, 102, 255,
	db 118, 10, 54, 9, 164, 161, 134, 102, 54, 4, 164, 161, 102, 255, 96, 10, 134,
	db 4, 54, 9, 164, 161, 134, 102, 54, 4, 164, 161, 102, 255, 134, 110, 134, 102,
	db 54, 127, 164, 161, 134, 102, 134, 110, 54, 126, 164, 161, 102, 5, 118, 246,
	db 54, 251, 164, 161, 102, 5, 134, 5, 54, 251, 164, 161, 102, 5, 128, 103, 48,
	db 251, 164, 161, 218, 180, 20, 156, 170, 85, 0, 0, 160, 64, 160, 0, 160, 192,
	db 128, 224, 160, 160, 224, 192, 64, 64, 224, 224, 32, 192, 224, 224, 96, 32,
	db 224, 160, 224, 32, 32, 224, 192, 32, 192, 96, 128, 224, 224, 224, 32, 64,
	db 64, 224, 224, 160, 224, 224, 224, 32, 192, 64, 160, 224, 160, 192, 224, 160,
	db 224, 224, 128, 128, 224, 192, 160, 160, 192, 224, 192, 128, 224, 224, 128,
	db 192, 128, 0, 160, 160, 64, 160, 64, 160, 160, 10, 174, 162, 66, 56, 8, 48, 
	db 184
CHIP_ROM_END:

SECTION "Tilemap", ROM0

TileMap:
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

	db 0,0,0,0,0,0, 01, 02, 03, 04, 05, 06, 07, 08,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0, 09, 10, 11, 12, 13, 14, 15, 16,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0, 17, 18, 19, 20, 21, 22, 23, 24,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0, 25, 26, 27, 28, 29, 30, 31, 32,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
TileMapEnd: