INCLUDE "hardware.inc"
INCLUDE "utils.asm"

; TO COMPILE RUN:
; rgbasm -o chip8boy.o main.asm ; rgblink -o chip8boy.gbc chip8boy.o ; rgbfix -C -v -p 0xFF chip8boy.gbc

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

MACRO LD_I_MEM_PTR
	ldh a, [I_REG]
	ld c, a
	ldh a, [I_REG + 1]
	ld b, a

	ld hl, CHIP_RAM
	add hl, bc
ENDM

MACRO CHECK_MEM_WRITE_OOB
	ld a, h
	cp HIGH(CHIP_RAM_END)
	jp z, InstrEnd
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

; 85 * 4 = 340 IPF
DEF IPF_PER_BLOCK EQU 85
DEF IPF_BLOCKS_NUM EQU 4

DEF CHIP_RAM_SIZE EQU 4096
DEF CHIP_RAM_DEADBUF_SIZE EQU 31
DEF CHIP_SCR_WIDTH EQU 64
DEF CHIP_SCR_HEIGHT EQU 32
DEF SCREEN_BUF_SIZE EQU 2048

DEF DIGIT0_TILE_NUM EQU $82

SECTION "Variables", HRAM
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
INSTR_COUNTER:
	ds 1 
INSTR_BLOCK_COUNTER:
	ds 1
CLEAR_SCREEN_FLAG:
	ds 1
FRAME_DONE_FLAG:
	ds 1
FRAME_COUNTER:
	ds 1
DROPPED_FRAME_COUNTER:
	ds 1
DXYN_XOR_MASK_LOOKUP:
	ds 4
CHIP_STATE_END:
; Regular code temps:
temp1:
	ds 1
temp2:
	ds 1
temp3:
	ds 1
; For use in interrupts:
temp4:
	ds 1
temp5:
	ds 1
temp6:
	ds 1

SECTION "ChipRAM", WRAMX[$D000]
CHIP_RAM:
	ds CHIP_RAM_SIZE
CHIP_RAM_END:

; These 31 bytes are always 0, and the purpose is to protect OOB memory reads (for example in dxyn) when I register is > $FFF,
; without having to mask it every time. since CHIP_RAM ends at DFFF, E000-FDFF is a mirror of C000 and will read from here.
SECTION "ChipRAMDeadBuf", WRAM0[$C000]
CHIP_RAM_DEAD_BUF:
	ds CHIP_RAM_DEADBUF_SIZE

SECTION "ChipScreenBuf", WRAM0, ALIGN[8]
SCREEN_BUF:
	ds SCREEN_BUF_SIZE

SECTION "VRAMScreenBuf", VRAM[$8010]
VRAM_SCREEN_BUF:
	ds SCREEN_BUF_SIZE

SECTION "DXYNLookup", ROMX, ALIGN[8]
DXYN_BASE_ADDR_LOOKUP:
	FOR y, CHIP_SCR_HEIGHT
		db LOW((y / 4) * 256 + (y & 3) * 4 + SCREEN_BUF)
		db HIGH((y / 4) * 256 + (y & 3) * 4 + SCREEN_BUF)
	ENDR

SECTION "VBlank Handler", ROM0[$0040]
VBlankHandler:
	push af
	ldh a, [FRAME_DONE_FLAG]
	and a
	jr z, .droppedFrame  ; Dont render screen if didn't finish executing opcodes for this frame yet.
	ldh a, [CLEAR_SCREEN_FLAG] 
	and a
	jr z, .copyScreen
	; Will display next frame using $9C00 (blank) tilemap instead, and not do copy.
	xor a
    ldh [FRAME_DONE_FLAG], a	
	ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8000 | LCDCF_BG9C00
	ldh [rLCDC], a
	jr .end
.copyScreen
	ldh [FRAME_DONE_FLAG], a ; a is 0
	ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8000
	ldh [rLCDC], a
	START_GDMA VRAM_SCREEN_BUF, SCREEN_BUF, 2048
.end
	ldh a, [FRAME_COUNTER]
	dec a
	ldh [FRAME_COUNTER], a
	jr z, .secondElapsed
	pop af
    reti
.secondElapsed:
	push hl
	ld a, 60
	ldh [FRAME_COUNTER], a
	ld hl, DROPPED_FRAME_COUNTER
	ld a, $60 ; BCD 60
	sub [hl]
	daa
	ld [hl], 0
	; Updating fps on both tilemaps
	ldh [temp4], a
	swap a
	and $F
	add DIGIT0_TILE_NUM
	ld l, 0
	ld h, $98
	ld [hl], a
	ld h, $9C
	ld [hl], a

	ldh a, [temp4]
	and $F
	add DIGIT0_TILE_NUM
	inc l
	ld h, $98
	ld [hl], a
	ld h, $9C
	ld [hl], a

	pop hl
	pop af
	reti 
.droppedFrame
	ldh a, [DROPPED_FRAME_COUNTER]
	inc a
	daa
	ldh [DROPPED_FRAME_COUNTER], a
	jr .end

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

	; Set stack
	ld sp, $D000

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
	ld hl, VRAM_BG_TILE
	ld b, $FF
REPT 8
	xor a
	ld [hl+], a
	ld a, b
	ld [hl+], a
ENDR
	MEMSET VRAM_CLEAR_TILE, 0, 16
	MEMCPY VRAM_DIGIT_TILES, DIGIT_TILES, DIGIT_TILES_END

	; Setting palettes
	ld a, $80
	ldh [rBCPS], a ; Auto-increment, initial address 0.
	ld c, LOW(rBCPD)
	; Color 00 (black chip8 pixel)
	xor a
	ldh [c], a
	ldh [c], a
	; Color 01 (white chip8 pixel)
	ld a, $FF
	ldh [c], a
	ldh [c], a
	; Color 10 (background, light-blue)
	ld a, $55
	ldh [c], a
	ld a, $73
	ldh [c], a
	; Color 11 (text, yellow)
	ld a, $FF
	ldh [c], a
	ld a, $07
	ldh [c], a

	; Init chip8
	MEMSET CHIP_RAM, 0, $200
	MEMCPY CHIP_RAM, CHIP8_FONT, CHIP8_FONT_END
	MEMSET CHIP_RAM + $200 + (CHIP_ROM_END - CHIP_ROM), 0, CHIP_RAM_SIZE - (CHIP_ROM_END - CHIP_ROM)
	MEMCPY CHIP_RAM + $200, CHIP_ROM, CHIP_ROM_END
	MEMSET CHIP_RAM_DEAD_BUF, 0, CHIP_RAM_DEADBUF_SIZE
	MEMSET CHIP_STATE, 0, CHIP_STATE_END - CHIP_STATE
	MEMSET SCREEN_BUF, 0, SCREEN_BUF_SIZE

	ld a, 1
	ldh [CLEAR_SCREEN_FLAG], a

	; ld [CHIP_RAM + $1FF], a ; writing 1 to $1FF for quirks test to enter chip8 menu by itself.

	ld a, 60
	ldh [FRAME_COUNTER], a

	ld hl, DXYN_XOR_MASK_LOOKUP
	ld a, %11000000
	ld [hl+], a
	ld a, %00110000
	ld [hl+], a
	ld a, %00001100
	ld [hl+], a
	ld [hl], %00000011

	; Reset chip8 PC
	ld de, $200
	push de

	; Turn LCD back on.
	ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8000
	ldh [rLCDC], a
	; Enable interrupts
	xor a
	ldh [rIF], a
	ei

	ld a, IPF_PER_BLOCK 
	ldh [INSTR_COUNTER], a 
	ld a, IPF_BLOCKS_NUM
	ldh [INSTR_BLOCK_COUNTER], a

InstrLoop:
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

InstrEnd:
	ld hl, INSTR_COUNTER 
	dec [hl] 
	jr nz, InstrLoop
	ld [hl], IPF_PER_BLOCK ; Reload number of instructions per block
	ld hl, INSTR_BLOCK_COUNTER
	dec [hl]
	jr nz, InstrLoop
	ldh a, [DELAY_TIMER]
	sub 1
	adc 0 ; Bring back to 0, if decremented to $FF
	ldh [DELAY_TIMER], a
	ld [hl], IPF_BLOCKS_NUM ; Reload number of instruction blocks
	ld a, 1
	ldh [FRAME_DONE_FLAG], a
	halt ; Wait for VBlank

	jr InstrLoop

InvalidInstr:
	ld a, a
	jr InvalidInstr

;; Instruction decoding, table for matching on first nibble

SECTION "Main Jump Table", ROM0, ALIGN[8] 
MainJumpTable:
    dw Case0, Case1, Case2, Case3, Case4, Case5, Case6, Case7,
    dw Case8, Case9, CaseA, CaseB, CaseC, CaseD, CaseE, CaseF  

Case0:
	; if first byte is not zero, instruction is invalid.
	ld a, d
	and a
	jp nz, InvalidInstr

	ld a, NN
	cp $EE
	jp z, OP_00EE
	cp $E0
	jp nz, InvalidInstr

OP_00E0: 
	ld hl, SCREEN_BUF
	xor a
	ld c, 8

.clearBlock:
	REPT 127
		ld [hl+], a
		inc l
	ENDR

	; need to use 'inc hl' instead of 'inc l', because low byte will overflow.
	ld [hl+], a
	inc hl

	dec c
	jp nz, .clearBlock

	inc a ; will become 1
	ldh [CLEAR_SCREEN_FLAG], a

	jp InstrEnd

OP_00EE:
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
OP_1NNN:
	LD_NNN()
	pop af
	push de
	
	jp InstrEnd

Case2:
OP_2NNN:
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
	jp nz, InvalidInstr

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
	jp nz, InvalidInstr

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
	; Mask to $FFF
	ld a, h
	and $F
	ld h, a

	; Updating PC
	pop af
	push hl

	jp InstrEnd

CaseC:
OP_CXNN:
	LD_VX_PTR()
	ldh a, [rDIV] ; Use timer register as rng for now.
	and NN
	ldh [c], a

	jp InstrEnd

CaseD: 
OP_DXYN:
	; \1 - pixel bit, \2 -  1 if its last call in this y iteration, 0 if not, \3 - skip byte label.
	MACRO DXYN_PROCESS_PIXEL 
		ld a, d
		IF \1 != 7
			add (7 - \1)
			cp CHIP_SCR_WIDTH
			jp z, \3
		ENDC
		; (x / 4) * 16
		srl a
		srl a
		REPT 4
			add a
		ENDR
		; adding to y result:
		IF \2 == 0
			push hl
		ENDC
		ld c, a
		IF \2 == 0 ; if its the last iteration, then b (sprite data) doesn't need to be saved.
			ld a, b
		ENDC
		ld b, 0
		add hl, bc
		IF \2 == 0
			ld b, a
		ENDC

		; loading xor mask from lookup table
		ld a, d
		IF \1 != 7
 			add (7 - \1)
 		ENDC
		and $3
		add LOW(DXYN_XOR_MASK_LOOKUP)
		ld c, a
		ldh a, [c]
		ld c, a

		; loading and saving current screen buf byte
		ld a, [hl]
		IF \2 == 0
			ldh [temp2], a
		ELSE
			ld b, a
		ENDC
		; colliion detection:
		and c
		jr z, .noCollision\@
		ld a, 1
		ldh [VF], a
	.noCollision\@:
		; drawing:
		IF \2 == 0
			ldh a, [temp2]
		ELSE
			ld a, b
		ENDC
		xor c
		ld [hl+], a
		; second byte is at hl + 2, since framebuf is double scaled.
		inc hl
		ld [hl], a

		IF \2 == 0
			pop hl
		ENDC
	ENDM

	MACRO DXYN_FINISH
		xor a
		ldh [CLEAR_SCREEN_FLAG], a
		jp InstrEnd	
	ENDM

	MACRO DXYN_CHECK_LOOP
		inc e
		ld a, e
		cp CHIP_SCR_HEIGHT
		jp z, .yClip
		ld hl, temp1
		dec [hl]
		pop hl
		jp nz, \1

		DXYN_FINISH()
	ENDM

	; \1 - is 1 if no loop is needed (height is 1), 0 otherwise. 
	MACRO DXYN
	.heightLoop\@:
		IF \1 == 0
			ld a, [hl+]
			ld b, a ; storing sprite data in B
			push hl
		ELSE
			ld b, [hl]
		ENDC
		ld h, HIGH(DXYN_BASE_ADDR_LOOKUP) ; Precomputed entries for: ((y / 4) * 256) + ((y & 3) * 4) + SCREEN_BUF
		ld l, e
		sla l
		ld a, [hl+]
		ld h, [hl]
		ld l, a

		ld a, b
		cp $80
		jr nz, .regularDraw\@
		DXYN_PROCESS_PIXEL 7, 1, .skipByte\@ ; single pixel draw
		IF \1 == 0
			DXYN_CHECK_LOOP(.heightLoop\@)
		ELSE
			DXYN_FINISH()
		ENDC		

	.regularDraw\@:
    	REDEF skipByteLabel EQUS ".skipByte\@"
    	FOR i, 7, -1, -1
       		bit i, b
        	jp z, .skipPixel\@
        	DXYN_PROCESS_PIXEL i, i == 0, {skipByteLabel}
    	.skipPixel\@:
    	ENDR

 	{skipByteLabel}:
    	IF \1 == 0
        	DXYN_CHECK_LOOP(.heightLoop\@)
    	ELSE
        	DXYN_FINISH()
    	ENDC
	ENDM

	LD_I_MEM_PTR()

	LD_VX()
	and CHIP_SCR_WIDTH - 1
	ld d, a

	LD_VY()
	and CHIP_SCR_HEIGHT - 1
	ld b, a

	xor a
	ldh [VF], a

	LD_N()
	jp z, .height0
	ld e, b ; move Y to e instead
	cp 1
	jp nz, .regularDXYN
	DXYN(1)
.regularDXYN:
	ldh [temp1], a ; height loop counter
	DXYN(0)

.yClip:
	xor a
	ldh [CLEAR_SCREEN_FLAG], a
	pop hl
	jp InstrEnd
.height0:
	ldh [VF], a
	jp InstrEnd
	
CaseE: ; TODO input
	ld a, NN
	cp $A1
	jr z, OP_EXA1
	cp $9E
	jp nz, InvalidInstr

OP_EX9E:
	jp InstrEnd

OP_EXA1: ; always add 2 to pc for now (no keys are pressed)
	pop hl
	inc hl
	inc hl
	push hl

	jp InstrEnd
	
CaseF:
	ld a, NN
	and $7F
	JP_TABLE(FXJumpTable)

SECTION "8XY Jump Table", ROM0, ALIGN[8] 
_8XYJumpTable:
	dw _8XY0, _8XY1, _8XY2, _8XY3, _8XY4, _8XY5, _8XY6, _8XY7,
	dw InvalidInstr, InvalidInstr, InvalidInstr, InvalidInstr,
	dw InvalidInstr, InvalidInstr, _8XYE, InvalidInstr

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

	; vF reset quirk
	xor a
	ldh [VF], a

	jp InstrEnd
_8XY2:
	LD_VY()
	ld b, a

	LD_VX_PTR()
	ldh a, [c]
	and b
	ldh [c], a

	; vF reset quirk
	xor a
	ldh [VF], a

	jp InstrEnd
_8XY3:
	LD_VY()
	ld b, a

	LD_VX_PTR()
	ldh a, [c]
	xor b
	ldh [c], a

	; vF reset quirk
	xor a
	ldh [VF], a

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
	; shifting quirk
	LD_VY()
	ld b, a

	LD_VX_PTR()
	ld a, b
	srl a
	ldh [c], a

	SET_VF_CARRY(1)
	jp InstrEnd
_8XY7:
	LD_VY()
	ld b, a

	LD_VX_PTR()
	ldh a, [c]
	ld l, a
	ld a, b
	sub l
	ldh [c], a

	SET_VF_CARRY(0)
	jp InstrEnd
_8XYE:
	; shifting quirk
	LD_VY()
	ld b, a

	LD_VX_PTR()
	ld a, b
	add a ; Same as left shift
	ldh [c], a

	SET_VF_CARRY(1)
	jp InstrEnd

; Matching on both lower nibbles of the instruction, 128 entries (byte is masked with $7F)
SECTION "FX Jump Table", ROM0, ALIGN[8]
FXJumpTable:
	ds 14, LOW(InvalidInstr), HIGH(InvalidInstr) ; $00-$06
    dw FX07 ; $07
    ds 04, LOW(InvalidInstr), HIGH(InvalidInstr) ; $08-$09
    dw FX0A ; $0A
    ds 20, LOW(InvalidInstr), HIGH(InvalidInstr) ; $0B-$14
    dw FX15 ; $15
    ds 04, LOW(InvalidInstr), HIGH(InvalidInstr) ; $16-$17
    dw FX18 ; $18
    ds 10, LOW(InvalidInstr), HIGH(InvalidInstr) ; $19-$1D
    dw FX1E ; $1E
    ds 20, LOW(InvalidInstr), HIGH(InvalidInstr) ; $1F-$28
    dw FX29 ; $29
    ds 18, LOW(InvalidInstr), HIGH(InvalidInstr) ; $2A-$32
    dw FX33 ; $33
    ds 66, LOW(InvalidInstr), HIGH(InvalidInstr) ; $34-$54
    dw FX55 ; $55
    ds 30, LOW(InvalidInstr), HIGH(InvalidInstr) ; $56-$64
    dw FX65 ; $65
    ds 52, LOW(InvalidInstr), HIGH(InvalidInstr) ; $66-$7F

FX07:
	LD_VX_PTR();
	ldh a, [DELAY_TIMER]
	ldh [c], a

	jp InstrEnd

FX0A: ; INPUT, TODO (always subtracting 2 from pc for now)
	pop hl
	dec hl
	dec hl
	push hl
	
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

FX33:
	LD_I_MEM_PTR()
	LD_VX()

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
	CHECK_MEM_WRITE_OOB()

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
	CHECK_MEM_WRITE_OOB()

	ld [hl], e ; Storing ones

	jp InstrEnd

MACRO REG_STORE ; If first parameter is 1, store to ram. Else, load from ram.
	LD_X()
	ld e, a ; Number of registers to store
	inc e; V[x] is included

	LD_I_MEM_PTR()
	ld c, LOW(V0)
.copyLoop\@
	IF \1 == 1
		CHECK_MEM_WRITE_OOB()
		ldh a, [c]
		ld [hl+], a
	ELSE
		ld a, [hl+]
		ldh [c], a
	ENDC

	inc c
	dec e
	jr nz, .copyLoop\@

	; memory increment quirk:
	; CHIP_RAM is located in $D000, so its enough to subtract D0 from h to get the I register, and store it back.
	ld a, l
	ldh [I_REG], a
	ld a, h
	sub HIGH(CHIP_RAM)
	and $F ; keep within $FFF
	ldh [I_REG + 1], a
ENDM

FX55: 
	REG_STORE(1)
	jp InstrEnd

FX65: 
	REG_STORE(0)
	jp InstrEnd

; Tile $0, used for background around the 128x64 screen 
SECTION "VRAMBGTile", VRAM[$8000]
VRAM_BG_TILE:
	ds 16

SECTION "VRAMTiles", VRAM[$8010 + SCREEN_BUF_SIZE]
VRAM_CLEAR_TILE: ; Tile $81, always has off pixel color, is used for zero tile map
	ds 16	
VRAM_DIGIT_TILES:
	ds 160

SECTION "Tiles", ROMX
DIGIT_TILES:
    ; 0
    dw `22222222
    dw `22333322
    dw `23322332
    dw `23322332
    dw `23322332
    dw `23322332
    dw `22333322
    dw `22222222
    ; 1
    dw `22222222
    dw `22233222
    dw `22333222
    dw `22233222
    dw `22233222
    dw `22233222
    dw `22333322
    dw `22222222
    ; 2
    dw `22222222
    dw `22333322
    dw `23223332
    dw `22223332
    dw `22333222
    dw `23332222
    dw `23333332
    dw `22222222
    ; 2
    dw `22222222
    dw `23333322
    dw `22223332
    dw `22333322
    dw `22223332
    dw `22223332
    dw `23333322
    dw `22222222
    ; 4
    dw `22222222
    dw `22333322
    dw `23323322
    dw `23223322
    dw `23223322
    dw `23333332
    dw `22223322
    dw `22222222
    ; 5
    dw `22222222
    dw `23333322
    dw `23322222
    dw `23333322
    dw `22223332
    dw `23223332
    dw `22333322
    dw `22222222
    ; 6
    dw `22222222
    dw `22333322
    dw `23322222
    dw `23333322
    dw `23322332
    dw `23322332
    dw `22333322
    dw `22222222
    ; 7
    dw `22222222
    dw `23333332
    dw `22222332
    dw `22223322
    dw `22233222
    dw `22333222
    dw `22333222
    dw `22222222
    ; 8
    dw `22222222
    dw `22333322
    dw `23222332
    dw `22333322
    dw `23222332
    dw `23222332
    dw `22333322
    dw `22222222
    ; 9
    dw `22222222
    dw `22333322
    dw `23223332
    dw `23223332
    dw `22333332
    dw `22223332
    dw `22333322
    dw `22222222
DIGIT_TILES_END:

SECTION "TileMap", ROMX
TILE_MAP:
	db DIGIT0_TILE_NUM, DIGIT0_TILE_NUM, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	REPT 4
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

SECTION "ZeroTileMap", ROMX
ZERO_TILE_MAP:
	db DIGIT0_TILE_NUM, DIGIT0_TILE_NUM, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	REPT 4
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

SECTION "Chip8Font", ROMX
CHIP8_FONT:
	db $F0, $90, $90, $90, $F0, ; 0
	db $20, $60, $20, $20, $70, ; 1
	db $F0, $10, $F0, $80, $F0, ; 2
	db $F0, $10, $F0, $10, $F0, ; 3
	db $90, $90, $F0, $10, $10, ; 4
	db $F0, $80, $F0, $10, $F0, ; 5
	db $F0, $80, $F0, $90, $F0, ; 6
	db $F0, $10, $20, $40, $40, ; 7
	db $F0, $90, $F0, $90, $F0, ; 8
	db $F0, $90, $F0, $10, $F0, ; 9
	db $F0, $90, $F0, $90, $90, ; A
	db $E0, $90, $E0, $90, $E0, ; B
	db $F0, $80, $80, $80, $F0, ; C
	db $E0, $90, $90, $90, $E0, ; D
	db $F0, $80, $F0, $80, $F0, ; E
	db $F0, $80, $F0, $80, $80  ; F
CHIP8_FONT_END:

SECTION "Chip8 ROM", ROMX ; 1dcell
CHIP_ROM:
	db 18, 138, 128, 124, 1, 109, 0, 34, 33, 104, 1, 57, 0, 34, 67, 136, 162, 56,
	db 0, 221, 193, 125, 1, 61, 64, 18, 7, 123, 1, 109, 0, 0, 238, 105, 0, 128,
	db 208, 208, 177, 79, 1, 121, 2, 208, 177, 112, 255, 208, 177, 79, 1, 121, 4,
	db 208, 177, 112, 2, 208, 177, 79, 1, 121, 1, 208, 177, 0, 238, 98, 0, 136,
	db 142, 114, 1, 143, 144, 143, 39, 79, 0, 18, 69, 0, 238, 0, 224, 109, 0, 108,
	db 5, 139, 192, 122, 1, 34, 105, 96, 31, 129, 192, 162, 2, 208, 17, 0, 238,
	db 128, 160, 162, 135, 240, 51, 242, 101, 100, 0, 240, 41, 99, 0, 211, 69, 241,
	db 41, 115, 5, 211, 69, 242, 41, 115, 5, 211, 69, 0, 238, 0, 0, 0, 106, 20, 34,
	db 83, 34, 3, 76, 31, 34, 83, 18, 142
CHIP_ROM_END:

; SECTION "Chip8 ROM", ROMX ; quirks test
; CHIP_ROM:
; 	db 19, 12, 96, 0, 224, 161, 18, 4, 112, 1, 64, 16, 0, 238, 18, 4, 101, 0, 162,
; 	db 34, 241, 85, 162, 130, 241, 85, 18, 34, 67, 1, 208, 18, 34, 2, 0, 0, 245,
; 	db 30, 245, 30, 245, 30, 245, 30, 241, 101, 99, 0, 243, 21, 244, 7, 52, 0, 18,
; 	db 68, 168, 83, 208, 18, 100, 10, 244, 21, 100, 1, 131, 67, 100, 14, 228, 158,
; 	db 18, 82, 69, 0, 18, 82, 117, 255, 18, 28, 100, 15, 228, 158, 18, 96, 149, 32,
; 	db 18, 96, 117, 1, 18, 28, 134, 80, 100, 10, 228, 161, 18, 128, 100, 0, 114, 1,
; 	db 116, 1, 228, 158, 18, 120, 134, 64, 118, 255, 18, 128, 84, 32, 18, 108, 114,
; 	db 255, 18, 50, 34, 2, 0, 0, 246, 30, 246, 30, 246, 30, 246, 30, 100, 2, 244,
; 	db 30, 241, 101, 100, 16, 128, 65, 162, 154, 241, 85, 0, 0, 252, 101, 35, 2,
; 	db 65, 0, 0, 238, 128, 16, 35, 2, 66, 0, 0, 238, 128, 32, 35, 2, 67, 0, 0, 238,
; 	db 128, 48, 35, 2, 68, 0, 0, 238, 128, 64, 35, 2, 69, 0, 0, 238, 128, 80, 35,
; 	db 2, 70, 0, 0, 238, 128, 96, 35, 2, 71, 0, 0, 238, 128, 112, 35, 2, 72, 0, 0,
; 	db 238, 128, 128, 35, 2, 73, 0, 0, 238, 128, 144, 35, 2, 74, 0, 0, 238, 128,
; 	db 160, 35, 2, 75, 0, 0, 238, 128, 176, 35, 2, 76, 0, 0, 238, 128, 192, 35, 2,
; 	db 0, 238, 168, 87, 240, 30, 221, 228, 125, 4, 0, 238, 0, 224, 161, 255, 240,
; 	db 101, 64, 1, 19, 128, 64, 2, 19, 136, 64, 4, 19, 144, 64, 3, 19, 152, 109, 6,
; 	db 110, 2, 169, 3, 34, 156, 109, 10, 110, 10, 169, 17, 34, 156, 109, 10, 110,
; 	db 15, 169, 26, 34, 156, 109, 10, 110, 20, 169, 39, 34, 156, 35, 112, 96, 168,
; 	db 97, 247, 98, 2, 18, 16, 0, 224, 109, 10, 110, 2, 169, 57, 34, 156, 109, 18,
; 	db 110, 12, 169, 69, 34, 156, 109, 18, 110, 17, 169, 78, 34, 156, 35, 112, 96,
; 	db 169, 97, 49, 98, 1, 18, 16, 106, 50, 107, 27, 169, 203, 218, 180, 106, 58,
; 	db 169, 207, 218, 180, 0, 238, 168, 66, 96, 1, 240, 85, 19, 158, 168, 66, 96,
; 	db 2, 240, 85, 19, 158, 168, 66, 96, 4, 240, 85, 19, 158, 168, 66, 96, 3, 240,
; 	db 85, 34, 2, 0, 224, 168, 66, 240, 101, 48, 1, 19, 200, 39, 108, 96, 1, 51, 0,
; 	db 96, 0, 111, 6, 143, 37, 79, 0, 96, 0, 111, 6, 143, 39, 79, 0, 96, 6, 168,
; 	db 67, 240, 85, 20, 46, 39, 154, 96, 1, 51, 0, 96, 0, 111, 3, 143, 37, 79, 0,
; 	db 96, 0, 111, 3, 143, 39, 79, 0, 96, 6, 168, 67, 240, 85, 0, 255, 0, 224, 39,
; 	db 230, 97, 1, 51, 0, 97, 0, 111, 3, 143, 37, 79, 0, 97, 0, 111, 3, 143, 39,
; 	db 79, 0, 97, 6, 168, 67, 240, 101, 64, 6, 20, 40, 49, 6, 20, 16, 96, 6, 20,
; 	db 40, 48, 0, 20, 28, 65, 0, 96, 3, 65, 1, 96, 5, 48, 1, 20, 40, 65, 0, 96, 4,
; 	db 65, 1, 96, 2, 168, 67, 240, 85, 0, 254, 0, 224, 169, 186, 96, 28, 97, 29,
; 	db 208, 22, 96, 22, 97, 2, 208, 18, 133, 240, 96, 34, 208, 18, 134, 240, 0,
; 	db 224, 169, 186, 96, 61, 97, 5, 208, 22, 96, 3, 97, 4, 208, 18, 135, 240, 97,
; 	db 10, 208, 18, 136, 240, 0, 224, 96, 110, 97, 50, 208, 22, 96, 40, 97, 17,
; 	db 208, 18, 137, 240, 96, 52, 208, 18, 137, 244, 97, 23, 208, 18, 137, 244, 96,
; 	db 40, 208, 18, 137, 244, 96, 0, 69, 0, 96, 1, 85, 96, 96, 6, 85, 112, 96, 6,
; 	db 85, 128, 96, 6, 57, 4, 96, 7, 168, 68, 240, 85, 168, 66, 240, 101, 64, 1,
; 	db 21, 86, 0, 255, 0, 224, 169, 186, 96, 60, 97, 61, 208, 22, 96, 54, 97, 2,
; 	db 208, 18, 133, 240, 96, 66, 208, 18, 134, 240, 0, 224, 169, 186, 96, 125, 97,
; 	db 5, 208, 22, 96, 3, 97, 4, 208, 18, 135, 240, 97, 10, 208, 18, 136, 240, 0,
; 	db 224, 96, 174, 97, 82, 208, 22, 96, 40, 97, 17, 208, 18, 137, 240, 96, 52,
; 	db 208, 18, 137, 244, 97, 23, 208, 18, 137, 244, 96, 40, 208, 18, 137, 244, 97,
; 	db 0, 69, 0, 97, 1, 85, 96, 97, 6, 85, 112, 97, 6, 85, 128, 97, 6, 57, 4, 97,
; 	db 7, 168, 68, 240, 101, 48, 6, 21, 28, 65, 6, 21, 80, 96, 8, 21, 80, 48, 7,
; 	db 21, 40, 65, 7, 21, 80, 96, 8, 21, 80, 49, 6, 21, 48, 96, 8, 21, 80, 49, 7,
; 	db 21, 56, 96, 8, 21, 80, 48, 1, 21, 68, 65, 1, 96, 2, 65, 0, 96, 4, 48, 0, 21,
; 	db 80, 65, 1, 96, 5, 65, 0, 96, 3, 168, 68, 240, 85, 0, 254, 0, 224, 109, 1,
; 	db 110, 1, 169, 87, 34, 156, 101, 0, 111, 15, 128, 2, 79, 0, 101, 1, 102, 0,
; 	db 111, 15, 128, 1, 79, 0, 102, 1, 103, 0, 111, 15, 128, 3, 79, 0, 103, 1, 168,
; 	db 66, 240, 101, 168, 88, 48, 1, 21, 142, 69, 1, 168, 85, 21, 146, 69, 0, 168,
; 	db 85, 106, 59, 107, 2, 218, 179, 169, 142, 69, 1, 169, 139, 85, 96, 169, 171,
; 	db 85, 112, 169, 176, 109, 42, 110, 1, 34, 156, 109, 1, 110, 6, 169, 96, 34,
; 	db 156, 96, 1, 97, 2, 98, 3, 99, 4, 100, 5, 168, 69, 244, 85, 96, 11, 97, 12,
; 	db 98, 13, 168, 69, 242, 85, 240, 101, 133, 0, 240, 101, 134, 0, 168, 66, 240,
; 	db 101, 168, 88, 97, 0, 64, 1, 21, 236, 64, 2, 21, 252, 64, 4, 21, 252, 64, 3,
; 	db 21, 236, 53, 4, 21, 250, 54, 5, 21, 248, 168, 85, 21, 250, 97, 1, 22, 10,
; 	db 53, 11, 22, 10, 54, 11, 22, 8, 168, 85, 22, 10, 97, 1, 106, 59, 107, 7, 218,
; 	db 179, 169, 139, 69, 11, 169, 142, 65, 1, 169, 171, 109, 42, 110, 6, 34, 156,
; 	db 109, 1, 110, 11, 169, 103, 34, 156, 168, 66, 241, 101, 168, 88, 48, 1, 22,
; 	db 54, 65, 1, 168, 85, 48, 2, 22, 62, 65, 3, 168, 85, 48, 4, 22, 70, 65, 4,
; 	db 168, 85, 48, 3, 22, 78, 65, 3, 168, 85, 106, 59, 107, 12, 218, 179, 169,
; 	db 142, 65, 1, 169, 139, 65, 6, 169, 146, 65, 4, 169, 151, 65, 5, 169, 156, 65,
; 	db 2, 169, 161, 65, 3, 169, 166, 109, 42, 110, 11, 34, 156, 109, 1, 110, 16,
; 	db 169, 113, 34, 156, 168, 66, 242, 101, 168, 88, 48, 1, 22, 138, 66, 1, 168,
; 	db 85, 48, 2, 22, 146, 66, 2, 168, 85, 48, 4, 22, 154, 66, 2, 168, 85, 48, 3,
; 	db 22, 162, 66, 3, 168, 85, 106, 59, 107, 17, 218, 179, 169, 142, 66, 1, 169,
; 	db 139, 66, 4, 169, 151, 66, 5, 169, 156, 66, 2, 169, 161, 66, 3, 169, 166, 66,
; 	db 6, 169, 171, 66, 7, 169, 176, 66, 8, 169, 181, 109, 42, 110, 16, 34, 156,
; 	db 109, 1, 110, 21, 169, 122, 34, 156, 101, 0, 102, 8, 103, 0, 104, 32, 133,
; 	db 110, 135, 134, 168, 66, 240, 101, 168, 88, 48, 1, 22, 242, 53, 0, 168, 85,
; 	db 48, 2, 22, 250, 69, 0, 168, 85, 48, 4, 23, 2, 69, 0, 168, 85, 48, 3, 23, 10,
; 	db 53, 0, 168, 85, 106, 59, 107, 22, 218, 179, 169, 142, 69, 0, 169, 139, 85,
; 	db 112, 169, 171, 109, 42, 110, 21, 34, 156, 109, 1, 110, 26, 169, 131, 34,
; 	db 156, 96, 152, 110, 156, 190, 0, 168, 66, 240, 101, 168, 88, 48, 1, 23, 60,
; 	db 69, 0, 168, 85, 48, 2, 23, 68, 53, 0, 168, 85, 48, 4, 23, 76, 53, 0, 168,
; 	db 85, 48, 3, 23, 84, 69, 0, 168, 85, 106, 59, 107, 27, 218, 179, 169, 142, 69,
; 	db 1, 169, 139, 109, 42, 110, 26, 34, 156, 240, 10, 34, 2, 19, 12, 39, 202,
; 	db 169, 192, 246, 101, 169, 199, 245, 21, 101, 30, 208, 17, 63, 0, 23, 136, 96,
; 	db 54, 246, 7, 134, 102, 134, 102, 128, 101, 117, 255, 53, 0, 23, 120, 130, 68,
; 	db 131, 244, 254, 7, 62, 0, 23, 118, 0, 238, 39, 202, 169, 192, 246, 101, 133,
; 	db 86, 169, 199, 245, 21, 101, 30, 208, 17, 63, 0, 23, 184, 96, 32, 246, 7,
; 	db 134, 102, 134, 102, 128, 101, 117, 255, 53, 0, 23, 168, 130, 68, 131, 244,
; 	db 254, 7, 62, 0, 23, 166, 0, 238, 96, 8, 97, 0, 98, 15, 169, 211, 208, 31,
; 	db 242, 30, 112, 8, 48, 56, 23, 224, 96, 8, 113, 15, 49, 30, 23, 210, 0, 238,
; 	db 96, 16, 97, 0, 98, 32, 170, 135, 208, 16, 242, 30, 112, 16, 48, 112, 23,
; 	db 252, 96, 16, 113, 16, 49, 64, 23, 238, 169, 192, 246, 101, 128, 4, 129, 20,
; 	db 133, 86, 169, 201, 208, 18, 112, 8, 48, 60, 24, 12, 169, 199, 208, 18, 112,
; 	db 1, 48, 64, 24, 22, 245, 21, 101, 15, 208, 18, 63, 0, 24, 48, 96, 108, 246,
; 	db 7, 134, 102, 128, 101, 117, 255, 53, 0, 24, 34, 130, 68, 131, 244, 254, 7,
; 	db 62, 0, 24, 32, 0, 238, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
; 	db 192, 192, 160, 192, 128, 160, 64, 160, 224, 160, 160, 224, 192, 64, 64, 224,
; 	db 224, 32, 192, 224, 224, 96, 32, 224, 160, 224, 32, 32, 224, 192, 32, 192,
; 	db 224, 128, 224, 224, 224, 32, 32, 32, 224, 224, 160, 224, 224, 224, 32, 224,
; 	db 64, 160, 224, 160, 192, 224, 160, 224, 224, 128, 128, 224, 192, 160, 160,
; 	db 192, 224, 192, 128, 224, 224, 128, 192, 128, 96, 128, 160, 96, 160, 224,
; 	db 160, 160, 224, 64, 64, 224, 96, 32, 32, 192, 160, 192, 160, 160, 128, 128,
; 	db 128, 224, 224, 224, 160, 160, 192, 160, 160, 160, 224, 160, 160, 224, 192,
; 	db 160, 192, 128, 64, 160, 224, 96, 192, 160, 192, 160, 96, 192, 32, 192, 224,
; 	db 64, 64, 64, 160, 160, 160, 96, 160, 160, 160, 64, 160, 160, 224, 224, 160,
; 	db 64, 160, 160, 160, 160, 64, 64, 224, 96, 128, 224, 0, 0, 0, 0, 0, 224, 0, 0,
; 	db 0, 0, 0, 64, 6, 11, 3, 128, 6, 16, 3, 76, 6, 21, 3, 152, 104, 76, 52, 84,
; 	db 148, 104, 88, 44, 120, 64, 100, 112, 92, 0, 8, 148, 52, 72, 76, 104, 152,
; 	db 36, 0, 12, 148, 116, 124, 104, 60, 112, 152, 52, 72, 76, 104, 0, 16, 148,
; 	db 136, 100, 152, 52, 72, 76, 104, 0, 14, 13, 3, 136, 14, 18, 3, 144, 104, 76,
; 	db 52, 84, 148, 120, 44, 112, 68, 60, 120, 0, 8, 148, 92, 100, 56, 60, 112, 96,
; 	db 0, 12, 148, 88, 60, 68, 44, 52, 140, 0, 128, 64, 148, 112, 60, 116, 60, 120,
; 	db 0, 92, 60, 92, 100, 112, 140, 0, 56, 76, 116, 104, 156, 132, 44, 76, 120, 0,
; 	db 52, 88, 76, 104, 104, 76, 96, 68, 0, 116, 72, 76, 64, 120, 76, 96, 68, 0,
; 	db 80, 124, 92, 104, 76, 96, 68, 0, 100, 96, 0, 100, 64, 64, 0, 116, 88, 100,
; 	db 132, 0, 88, 112, 60, 116, 0, 72, 112, 60, 116, 0, 48, 100, 120, 72, 0, 96,
; 	db 100, 96, 60, 0, 60, 112, 112, 8, 0, 60, 112, 112, 12, 0, 60, 112, 112, 16,
; 	db 0, 254, 254, 254, 254, 254, 254, 10, 31, 0, 0, 1, 180, 0, 128, 128, 255,
; 	db 255, 10, 174, 162, 66, 56, 8, 48, 184, 15, 2, 2, 2, 2, 2, 0, 0, 31, 63, 113,
; 	db 224, 229, 224, 232, 160, 13, 42, 40, 40, 40, 0, 0, 24, 184, 184, 56, 56, 63,
; 	db 191, 0, 25, 165, 189, 161, 157, 0, 0, 12, 29, 29, 1, 13, 29, 157, 1, 199,
; 	db 41, 41, 41, 39, 0, 0, 248, 252, 206, 198, 198, 198, 198, 0, 73, 74, 73, 72,
; 	db 59, 0, 0, 0, 1, 3, 3, 3, 1, 240, 48, 144, 0, 0, 128, 0, 0, 0, 254, 199, 131,
; 	db 131, 131, 198, 252, 231, 224, 224, 224, 224, 113, 63, 31, 0, 0, 7, 2, 2, 2,
; 	db 2, 57, 56, 56, 56, 56, 184, 184, 56, 0, 0, 49, 74, 121, 64, 59, 221, 221,
; 	db 221, 221, 221, 221, 221, 221, 0, 0, 160, 56, 32, 160, 24, 206, 252, 248,
; 	db 192, 212, 220, 196, 197, 0, 0, 48, 68, 36, 20, 99, 241, 3, 7, 7, 119, 23,
; 	db 99, 113, 0, 0, 40, 142, 168, 168, 166, 206, 135, 3, 3, 3, 135, 254, 252, 0,
; 	db 0, 96, 144, 240, 128, 112, 0, 255, 0, 255, 0, 12, 0, 12, 0, 12, 0, 12, 0,
; 	db 12, 0, 12, 0, 12, 0, 12, 0, 12, 0, 12, 0, 0, 0, 0, 0, 0, 0, 0, 204, 0, 204,
; 	db 0, 0, 243, 0, 243, 12, 204, 12, 204, 12, 192, 12, 192, 12, 192, 12, 192, 12,
; 	db 192, 12, 192, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 195, 3, 195, 204, 51,
; 	db 204, 51, 207, 243, 207, 243, 204, 3, 204, 3, 195, 243, 195, 243, 0, 0, 0, 0,
; 	db 0, 0, 0, 0, 0, 3, 0, 3, 240, 63, 240, 63, 12, 195, 12, 195, 12, 195, 12,
; 	db 195, 12, 195, 12, 195, 12, 63, 12, 63, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
; 	db 48, 195, 48, 195, 48, 204, 48, 204, 48, 195, 48, 195, 48, 192, 48, 192, 15,
; 	db 207, 15, 207, 0, 0, 0, 0, 0, 0, 0, 0, 15, 0, 15, 0, 195, 0, 195, 0, 0, 0, 0,
; 	db 0, 0, 0, 0, 0, 192, 0, 192, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 255,
; 	db 3, 255, 15, 255, 15, 255, 63, 3, 63, 3, 252, 0, 252, 0, 252, 51, 252, 51,
; 	db 252, 0, 252, 0, 252, 192, 252, 192, 252, 63, 252, 63, 3, 192, 3, 192, 207,
; 	db 192, 207, 192, 207, 192, 207, 192, 15, 192, 15, 192, 15, 192, 15, 192, 15,
; 	db 255, 15, 255, 207, 255, 207, 255, 15, 195, 15, 195, 0, 240, 0, 240, 3, 243,
; 	db 3, 243, 3, 243, 3, 243, 0, 3, 0, 3, 0, 243, 0, 243, 3, 243, 3, 243, 195,
; 	db 243, 195, 243, 243, 243, 243, 243, 255, 192, 255, 192, 255, 240, 255, 240,
; 	db 240, 252, 240, 252, 240, 60, 240, 60, 240, 60, 240, 60, 240, 60, 240, 60,
; 	db 240, 60, 240, 60, 240, 252, 240, 252, 0, 0, 0, 0, 0, 3, 0, 3, 0, 15, 0, 15,
; 	db 0, 15, 0, 15, 0, 15, 0, 15, 0, 3, 0, 3, 255, 0, 255, 0, 255, 3, 255, 3, 255,
; 	db 252, 255, 252, 240, 63, 240, 63, 192, 15, 192, 15, 192, 15, 192, 15, 192,
; 	db 15, 192, 15, 240, 60, 240, 60, 255, 240, 255, 240, 240, 252, 240, 252, 252,
; 	db 0, 252, 0, 252, 0, 252, 0, 252, 0, 252, 0, 252, 0, 252, 0, 63, 3, 63, 3, 15,
; 	db 255, 15, 255, 3, 255, 3, 255, 0, 0, 0, 0, 15, 192, 15, 192, 15, 192, 15,
; 	db 192, 15, 192, 15, 192, 15, 192, 15, 192, 207, 192, 207, 192, 207, 192, 207,
; 	db 192, 15, 192, 15, 192, 0, 0, 0, 0, 243, 243, 243, 243, 243, 243, 243, 243,
; 	db 243, 243, 243, 243, 243, 243, 243, 243, 243, 243, 243, 243, 243, 243, 243,
; 	db 243, 243, 243, 243, 243, 0, 0, 0, 0, 255, 240, 255, 240, 255, 192, 255, 192,
; 	db 240, 0, 240, 0, 243, 48, 243, 48, 243, 240, 243, 240, 240, 48, 240, 48, 240,
; 	db 51, 240, 51, 0, 0, 0, 0, 0, 15, 0, 15, 0, 63, 0, 63, 0, 63, 0, 63, 63, 63,
; 	db 63, 63, 3, 63, 3, 63, 60, 15, 60, 15, 63, 3, 63, 3, 0, 0, 0, 0, 192, 63,
; 	db 192, 63, 0, 15, 0, 15, 0, 15, 0, 15, 0, 15, 0, 15, 192, 63, 192, 63, 255,
; 	db 252, 255, 252, 255, 240, 255, 240, 0, 0, 0, 0, 0, 0, 0, 0, 0, 63, 0, 63, 0,
; 	db 12, 0, 12, 0, 12, 0, 12, 0, 12, 0, 12, 0, 12, 0, 12, 0, 0, 0, 0, 0, 0, 0, 0,
; 	db 0, 0, 0, 0, 15, 3, 15, 3, 48, 204, 48, 204, 63, 195, 63, 195, 48, 0, 48, 0,
; 	db 15, 207, 15, 207, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 204, 0, 204, 0, 15,
; 	db 192, 15, 192, 12, 0, 12, 0, 204, 0, 204, 0, 3, 192, 3, 192, 0, 0, 0, 0, 0,
; 	db 0, 0, 0, 0, 0, 0, 0, 15, 0, 15, 0, 48, 48, 48, 48, 12, 48, 12, 48, 3, 48, 3,
; 	db 48, 60, 15, 60, 15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 192, 12, 192,
; 	db 192, 252, 192, 252, 204, 192, 204, 192, 204, 192, 204, 192, 204, 60, 204,
; 	db 60, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 60, 0, 60, 0, 195, 0, 195, 0, 255,
; 	db 0, 255, 0, 192, 0, 192, 0, 63, 0, 63, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
; 	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
; 	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
; 	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
; 	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
; 	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
; 	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
; 	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
; 	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
; 	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
; 	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
; 	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 101, 0, 23, 46,
; 	db 101, 1, 23, 46
; CHIP_ROM_END: