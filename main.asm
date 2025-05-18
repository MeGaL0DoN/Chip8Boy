INCLUDE "hardware.inc"
INCLUDE "utils.asm"

; TO COMPILE RUN:
; rgbasm -o chip8boy.o main.asm ; rgblink -o chip8boy.gb chip8boy.o ; rgbfix -C -v -p 0xFF chip8boy.gb

MACRO LD_X
	ld a, d
	and $F
ENDM
MACRO LD_Y
	ld a, e
	and $F0
	swap a
ENDM

MACRO LD_VX_PTR
	LD_X()
	add LOW(V0)
	ld c, a
ENDM
MACRO LD_VY_PTR
	LD_Y()
	add LOW(V0)
	ld c, a
ENDM

MACRO LD_VX
	LD_VX_PTR()
	ldh a, [c]
ENDM
MACRO LD_VY
	LD_VY_PTR()
	ldh a, [c]
ENDM

MACRO LD_NNN
	ld a, d
	and $F
	ld d, a
ENDM

DEF NN EQUS "e"

MACRO LD_N
	ld a, NN
	and $F
ENDM

; First parameter is table address, 256 byte aligned! Index is stored in A.
MACRO JP_TABLE ; 
	add a ; addresses are 2 bytes, so multiply by 2
	ld h, HIGH(\1)
	ld l, a

	ld a, [hl+]
	ld h, [hl]
	ld l, a
	jp hl
ENDM

DEF IPF EQU 10 ; Instructions per frame
DEF CHIP_SCR_WIDTH EQU 64
DEF CHIP_SCR_HEIGHT EQU 32
DEF SCREEN_BUF_SIZE EQU 2048

SECTION "VBlank Handler", ROM0[$0040]
VBlankHandler:
	push af
	ld a, 1
	ldh [VBLANK_HIT_FLAG], a
	ldh a, [CLEAR_SCREEN_FLAG]
	and a
	jr z, .copyScreen
	; Will display next frame using 9C00 (blank) tilemap instead, and not do copy.
	ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8000 | LCDCF_BG9C00
	ldh [rLCDC], a
	jr .end
.copyScreen
	ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8000
	ldh [rLCDC], a
	ld a, HIGH(SCREEN_BUF)
	ldh [rHDMA1], a
	ld a, LOW(SCREEN_BUF)
	ldh [rHDMA2], a
	ld a, HIGH(VRAM_SCREEN_BUF)
	ldh [rHDMA3], a
	ld a, LOW(VRAM_SCREEN_BUF)
	ldh [rHDMA4], a 
	ld a, $7F ; Bit 7 is clear for general-purpose DMA. All low bits are set for 2048 byte transfer.
	ldh [rHDMA5], a
.end
	pop af
    reti

SECTION "ChipState", HRAM
CHIP_STATE:
V0:
	ds 15
VF:
	ds 1
CHIP_STACK:
	ds 32
CHIP_SP:
	ds 1
I_REG:
	ds 2
DELAY_TIMER:
	ds 1
SOUND_TIMER:
	ds 1
INSTR_COUNT:
	ds 1
CLEAR_SCREEN_FLAG:
	ds 1
VBLANK_HIT_FLAG:
	ds 1
DROPPED_FRAMES:
	ds 1
CHIP_STATE_END:

SECTION "ChipRAM", WRAM0
CHIP_RAM:
	ds 4096

SECTION "ChipScreenBuf", WRAMX, ALIGN[8]
SCREEN_BUF:
	ds SCREEN_BUF_SIZE

SECTION "VRAMScreenBuf", VRAM[$8010]
VRAM_SCREEN_BUF:
	ds SCREEN_BUF_SIZE

SECTION "Header", ROM0[$100]
	jr EntryPoint
	ds $150 - @, 0 ; Make room for the header

EntryPoint:
	; Enable double speed mode
	set 0, a
	ldh [rKEY1], a
	stop

	; Shut down audio circuitry
	xor a
	ldh [rNR52], a

	; Clear IF and set IE to enable vblank interrupts
	ldh [rIF], a
	ld a, IEF_VBLANK
	ldh [rIE], a

	; Waiting for vblank and disabling lcd to copy tile map and set initial tiles
	halt 
	xor a
	ldh [rLCDC], a

	MEMCPY $9800, TILE_MAP, TILE_MAP_END
	MEMCPY $9C00, ZERO_TILE_MAP, ZERO_TILE_MAP_END
	MEMSET VRAM_SCREEN_BUF, 0, SCREEN_BUF_SIZE

	set 0, a
	ldh [rVBK], a

	; Set attribute maps to zeroes (use palette 0 and tile bank 0, no flips)
	MEMSET $9800, 0, $400
	MEMSET $9C00, 0, $400

	xor a
	ldh [rVBK], a

	; Setting bg tile (used for area outside chip8 screen) to '10'
	ld hl, BG_TILE
	ld b, $FF
REPT 8
	xor a
	ld [hl+], a
	ld a, b
	ld [hl+], a
ENDR
	; Setting clear tile to '00'
	ld hl, CLEAR_TILE
	xor a
REPT 16
	ld [hl+], a
ENDR
	; Setting palettes
	ld a, $80
	ldh [rBCPS], a ; Auto-increment, initial address 0.
	; Color 00 (black chip8 pixel)
	xor a
	ldh [rBCPD], a
	ldh [rBCPD], a
	; Color 01 (white chip8 pixel)
	ld a, $FF
	ldh [rBCPD], a
	ldh [rBCPD], a
	; Color 10 (background, purple)
	ld a, $9F
	ldh [rBCPD], a
	ld a, $4D
	ldh [rBCPD], a

	; Init chip8
	MEMCPY CHIP_RAM + $200, CHIP_ROM, CHIP_ROM_END
	MEMSET CHIP_STATE, 0, CHIP_STATE_END - CHIP_STATE
	MEMSET SCREEN_BUF, 0, SCREEN_BUF_SIZE

	; Reset chip8 PC
	ld de, $200
	push de

	; Turn LCD back on.
	ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8000
	ldh [rLCDC], a
	; Enable interrupts
	ei

	ld a, IPF

InstrLoop:
	ldh [INSTR_COUNT], a
	; loading chip8 PC to DE
	pop de

	; loading chip8 mem pointer to HL
	ld hl, CHIP_RAM
	add hl, de

	; Storing pc + 2 back
	inc de
	inc de
	push de

	; Loading chip8 instruction to DE
	ld a, [hl+]
	ld d, a
	ld e, [hl]

	; Jumping to opcode handler based on the first opcode nibble.
	and $F0
	swap a
	JP_TABLE(MainJumpTable)

MACRO CONTINUE_FRAME
	ldh a, [INSTR_COUNT]
	dec a
	jr nz, InstrLoop
	halt ; Wait for VBlank
	ldh [VBLANK_HIT_FLAG], a
	ld a, IPF
	jr InstrLoop
ENDM

InstrEnd:
	ldh a, [VBLANK_HIT_FLAG]
	and a
	jr nz, .droppedFrame
	CONTINUE_FRAME()
.droppedFrame:
	ldh a, [DROPPED_FRAMES]
	inc a
	ldh [DROPPED_FRAMES], a
	xor a 
	ldh [VBLANK_HIT_FLAG], a
	CONTINUE_FRAME()

InvalidInstruction:
	ld a, a
	jr InvalidInstruction

;; Instruction decoding, table for matching on first nibble

SECTION "Main Jump Table", ROM0, ALIGN[8] 
MainJumpTable:
    dw Case0, Case1, Case2, Case3, Case4, Case5, Case6, Case7,
    dw Case8, Case9, CaseA, CaseB, CaseC, CaseD, CaseE, CaseF  

Case0:
	; if first byte is not zero, instruction is invalid.
	ld a, d
	and a
	jp nz, InvalidInstruction

	ld a, NN
	cp $EE
	jr z, OP_00EE
	cp $E0
	jp nz, InvalidInstruction

OP_00E0: 
	ld a, 1
	ldh [CLEAR_SCREEN_FLAG], a
	MEMSET SCREEN_BUF, 0, SCREEN_BUF_SIZE
	xor a
	ldh [CLEAR_SCREEN_FLAG], a

	jp InstrEnd

OP_00EE: ; Ret from function
	ldh a, [CHIP_SP]
	; Updating SP
	sub 2
	and $1F
	ldh [CHIP_SP], a

	ld hl, sp + 0
	add LOW(CHIP_STACK)
	ld c, a
	; Loading address from the chip8 stack into PC
	ldh a, [c]
	ld [hl+], a
	inc c
	ldh a, [c]
	ld [hl], a

	jp InstrEnd

Case1: 
OP_1NNN: ; Jump NNN
	LD_NNN()
	pop af
	push de
	
	jp InstrEnd

Case2:
OP_2NNN: ; Call function
	; load current PC to HL
	pop hl

	; Setting PC to new address
	LD_NNN()
	push de
	
	; Updating SP
	ldh a, [CHIP_SP]
	ld b, a ; Saving current SP
	add 2
	and $1F
	ldh [CHIP_SP], a

	; Saving HL to the chip8 stack
	ld a, LOW(CHIP_STACK)
	add b ; saved sp
	ld c, a
	ld a, l
	ldh [c], a
	inc c
	ld a, h
	ldh [c], a

	jp InstrEnd

MACRO COND_PC_ADD ; If first parameter is 1, add if equal. Else, add if not equal.
	IF \1 == 1
		jp nz, InstrEnd
	ELSE
		jp z, InstrEnd
	ENDC

	; Adding 2 to pc
	pop de
	inc de
	inc de
	push de
ENDM

Case3:
OP_3XNN:
	LD_VX()
	cp NN
	COND_PC_ADD(1) ; Check if equal

	jp InstrEnd

Case4:
OP_4XNN:
	LD_VX()
	cp NN
	COND_PC_ADD(0) ; Check if not equal

	jp InstrEnd

Case5:
OP_5XY0:
	LD_N();
	jp nz, InvalidInstruction

	LD_VX()
	ld b, a
	LD_VY()

	cp b
	COND_PC_ADD(1)

	jp InstrEnd

Case6:
OP_6XNN:
	LD_VX_PTR()
	ld a, NN
	ldh [c], a

	jp InstrEnd

Case7:
OP_7XNN:
	LD_VX_PTR()
	ldh a, [c]
	add NN
	ldh [c], a

	jp InstrEnd

Case8:
	LD_N()
	JP_TABLE(_8XYJumpTable)

Case9:
OP_9XY0: 
	LD_N()
	jp nz, InvalidInstruction

	LD_VX()
	ld b, a

	LD_VY()
	cp b
	COND_PC_ADD(0)

	jp InstrEnd

CaseA: 
OP_ANNN:
	ld a, NN
	ldh [I_REG], a
	ld a, d
	and $F
	ldh [I_REG + 1], a

	jp InstrEnd
	
CaseB:
OP_BNNN: ; Quirk off, jump to V[0] + NNN
	ld h, 0
	ldh a, [V0]
	ld l, a

	LD_NNN()
	add hl, de

	; Updating PC
	pop af
	push hl

	jp InstrEnd

CaseC:
OP_CXNN:
	LD_VX_PTR()
	ldh a, [rDIV] ; Use timer register as rng for now.
	ldh [c], a

	jp InstrEnd

CaseD: 
OP_DXYN:
	LD_VX()
	and CHIP_SCR_WIDTH - 1
	ld b, a

	LD_VY()
	and CHIP_SCR_HEIGHT - 1
	ld d, a

	LD_N()
	ld e, a 

	jp InstrEnd
	
CaseE: ; TODO input
	jp InstrEnd
	
CaseF:
	ld a, NN
	and $7F
	JP_TABLE(FXJumpTable)

SECTION "8XY Jump Table", ROM0, ALIGN[8] 
_8XYJumpTable:
	dw _8XY0, _8XY1, _8XY2, _8XY3, _8XY4, _8XY5, _8XY6, _8XY7,
	dw InvalidInstruction, InvalidInstruction, InvalidInstruction, InvalidInstruction,
	dw InvalidInstruction, InvalidInstruction, _8XYE, InvalidInstruction

_8XY0:
	LD_VY()
	ld b, a

	LD_VX_PTR()
	ld a, b
	ldh [c], a

	jp InstrEnd
_8XY1:
	LD_VY()
	ld b, a

	LD_VX_PTR()
	ldh a, [c]
	or b
	ldh [c], a

	jp InstrEnd
_8XY2:
	LD_VY()
	ld b, a

	LD_VX_PTR()
	ldh a, [c]
	and b
	ldh [c], a

	jp InstrEnd
_8XY3:
	LD_VY()
	ld b, a

	LD_VX_PTR()
	ldh a, [c]
	xor b
	ldh [c], a

	jp InstrEnd

MACRO SET_VF_CARRY
	IF \1 == 1 ; 1 is regular carry, 0 inverse carry (used for subtractions)
		sbc a
		and $1
		ldh [VF], a
	ELSE
		sbc a
		inc a
		ldh [VF], a
	ENDC
ENDM

_8XY4:
	LD_VY()
	ld b, a

	LD_VX_PTR()
	ldh a, [c]
	add b
	ldh [c], a

	SET_VF_CARRY(1)
	jp InstrEnd
_8XY5:
	LD_VY()
	ld b, a

	LD_VX_PTR()
	ldh a, [c]
	sub b
	ldh [c], a

	SET_VF_CARRY(0)
	jp InstrEnd
_8XY6:
	LD_VX_PTR();
	ldh a, [c]
	srl a
	ldh [c], a

	SET_VF_CARRY(1)
	jp InstrEnd
_8XY7:
	LD_VY()
	ld b, a

	LD_VX_PTR()
	ldh a, [c]
	cpl ; Invert a
	add b
	inc a
	ldh [c], a

	SET_VF_CARRY(0)
	jp InstrEnd
_8XYE:
	LD_VX_PTR()
	ldh a, [c]
	sla a
	ldh [c], a

	SET_VF_CARRY(1)
	jp InstrEnd

; Matching on both lower nibbles of the instruction, 128 entries (byte is masked with $7F)
SECTION "FX Jump Table", ROM0, ALIGN[8]
FXJumpTable:
	ds 14, LOW(InvalidInstruction), HIGH(InvalidInstruction) ; $00-$06
    dw FX07                                          		 ; $07
    ds 04, LOW(InvalidInstruction), HIGH(InvalidInstruction) ; $08-$09
    dw FX0A                                          		 ; $0A
    ds 20, LOW(InvalidInstruction), HIGH(InvalidInstruction) ; $0B-$14
    dw FX15                                          		 ; $15
    ds 04, LOW(InvalidInstruction), HIGH(InvalidInstruction) ; $16-$17
    dw FX18                                          		 ; $18
    ds 10, LOW(InvalidInstruction), HIGH(InvalidInstruction) ; $19-$1D
    dw FX1E                                          		 ; $1E
    ds 20, LOW(InvalidInstruction), HIGH(InvalidInstruction) ; $1F-$28
    dw FX29                                          		 ; $29
    ds 18, LOW(InvalidInstruction), HIGH(InvalidInstruction) ; $2A-$32
    dw FX33                                          		 ; $33
    ds 66, LOW(InvalidInstruction), HIGH(InvalidInstruction) ; $34-$54
    dw FX55                                          		 ; $55
    ds 30, LOW(InvalidInstruction), HIGH(InvalidInstruction) ; $56-$64
    dw FX65                                          		 ; $65
    ds 52, LOW(InvalidInstruction), HIGH(InvalidInstruction) ; $66-$7F

FX07:
	LD_VX_PTR();
	ldh a, [DELAY_TIMER]
	ldh [c], a

	jp InstrEnd

FX0A: ; INPUT, TODO
	jp InstrEnd

FX15:
	LD_VX()
	ldh [DELAY_TIMER], a

	jp InstrEnd

FX18:
	LD_VX()
	ldh [SOUND_TIMER], a

	jp InstrEnd

FX1E:
	LD_VX()
	ld b, a
	ld c, LOW(I_REG)

	; Store low
	ldh a, [c]
	add b
	ldh [c], a

	; Store high
	inc c
	ldh a, [c]
	adc 0
	and $F ; Keep I within $FFF
	ldh [c], a
	jp InstrEnd

FX29:
	LD_VX()
	and $F
	ld b, a

	; Multiply by 5
	add a
	add a
	add b

	ldh [I_REG], a
	xor a
	ldh [I_REG + 1], a

	jp InstrEnd

MACRO LD_I_MEM_PTR
	ldh a, [I_REG]
	ld c, a
	ldh a, [I_REG + 1]
	ld b, a

	ld hl, CHIP_RAM
	add hl, bc
ENDM

FX33:
	LD_I_MEM_PTR()
	LD_VX()
	ld d, $F

MACRO MASK_I
	ld a, h
	and d
	ld h, a
ENDM

	ld b, 100
	ld c, -1

.countHundreds:
	inc c
	sub b
	jr nc, .countHundreds

	add b
	ld e, a

	ld a, c
	ld [hl+], a ; Storing hundreds
	MASK_I()

	ld a, e
	ld b, 10
	ld c, -1

.countTenths:
	inc c
	sub b
	jr nc, .countTenths

	add b
	ld e, a

	ld a, c
	ld [hl+], a ; Storing tenths
	MASK_I()

	ld [hl], e ; Storing ones

	jp InstrEnd

MACRO REG_STORE ; If first parameter is 1, store to ram. Else, load from ram.
	LD_X()
	ld b, a ; Number of registers to store
	inc b; V[x] is included

	LD_I_MEM_PTR()
	ld c, LOW(V0)
	ld e, $F
.copyLoop\@
	IF \1 == 1
		ldh a, [c]
		ld [hl+], a
	ELSE
		ld a, [hl+]
		ldh [c], a
	ENDC
	; Keep I withn $FFF
	ld a, h
	and e
	ld h, a

	inc c
	dec b
	jr nz, .copyLoop\@
ENDM

FX55: 
	REG_STORE(1)
	jp InstrEnd

FX65: 
	REG_STORE(0)
	jp InstrEnd

; Tile $0, used for background around the 128x64 screen 
SECTION "BGTile", VRAM[$8000]
BG_TILE:
	ds 16

; Tile $81, always has off pixel color, is used for zero tile map
SECTION "ClearTile", VRAM[$8010 + SCREEN_BUF_SIZE]
CLEAR_TILE:
	ds 16	

SECTION "TileMap", ROM0
TILE_MAP:
	REPT 5
	REPT 32
		db 0
	ENDR
	ENDR

	db 0,0, $01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F,$10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0, $11,$12,$13,$14,$15,$16,$17,$18,$19,$1A,$1B,$1C,$1D,$1E,$1F,$20, 0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0, $21,$22,$23,$24,$25,$26,$27,$28,$29,$2A,$2B,$2C,$2D,$2E,$2F,$30, 0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0, $31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$3F,$40, 0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0, $41,$42,$43,$44,$45,$46,$47,$48,$49,$4A,$4B,$4C,$4D,$4E,$4F,$50, 0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0, $51,$52,$53,$54,$55,$56,$57,$58,$59,$5A,$5B,$5C,$5D,$5E,$5F,$60, 0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0, $61,$62,$63,$64,$65,$66,$67,$68,$69,$6A,$6B,$6C,$6D,$6E,$6F,$70, 0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0, $71,$72,$73,$74,$75,$76,$77,$78,$79,$7A,$7B,$7C,$7D,$7E,$7F,$80, 0,0,0,0,0,0,0,0,0,0,0,0,0,0

	REPT 5
	REPT 32
		db 0
	ENDR
	ENDR
TILE_MAP_END:

SECTION "ZeroTileMap", ROM0
ZERO_TILE_MAP:
	REPT 5
	REPT 32
		db 0
	ENDR
	ENDR

	db 0,0, $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81, 0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0, $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81, 0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0, $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81, 0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0, $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81, 0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0, $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81, 0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0, $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81, 0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0, $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81, 0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0, $81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81, 0,0,0,0,0,0,0,0,0,0,0,0,0,0

	REPT 5
	REPT 32
		db 0
	ENDR
	ENDR
ZERO_TILE_MAP_END:

SECTION "Chip8 ROM", ROMX
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