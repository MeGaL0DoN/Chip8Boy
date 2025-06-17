INCLUDE "hardware.inc"
INCLUDE "utils.asm"

; TO COMPILE RUN:
; rgbasm -o chip8boy.o main.asm ; rgblink -o chip8boy.gbc chip8boy.o ; rgbfix -c -v -p 0xFF chip8boy.gbc

MACRO LD_X
	ld a, d
	and $F
ENDM
MACRO LD_Y
	ld a, e
	and $F0
	swap a
ENDM

MACRO LD_VX_PTR_HL
	LD_X()
	ld h, HIGH(V0)
	add LOW(V0)
	ld l, a
ENDM
MACRO LD_VX_PTR_C
	LD_X()
	add LOW(V0)
	ld c, a
ENDM
MACRO LD_VY_PTR_HL
	LD_Y()
	ld h, HIGH(V0)
	add LOW(V0)
	ld l, a
ENDM
MACRO LD_VY_PTR_C
	LD_Y()
	add LOW(V0)
	ld c, a
ENDM

MACRO LD_VX
	LD_VX_PTR_C()
	ldh a, [c]
ENDM
MACRO LD_VY
	LD_VY_PTR_C()
	ldh a, [c]
ENDM

DEF NN EQUS "e"

MACRO LD_N
	ld a, NN
	and $F
ENDM

MACRO LD_NNN
	ld a, d
	and $F
	ld h, a
	ld l, NN
ENDM
MACRO LD_NNN_RAM_PTR
	ld a, d
	and $F
	add HIGH(CHIP_RAM)
	ld h, a
	ld l, NN
ENDM

; \1 - high register, \2 - low register
MACRO LD_I_MEM_PTR 
	ldh a, [I_REG]
	ld \2, a
	ldh a, [I_REG + 1]
	add HIGH(CHIP_RAM)
	ld \1, a
ENDM

; \1 - label to jump on OOB
MACRO CHECK_MEM_WRITE_OOB
	ld a, h
	cp HIGH(CHIP_RAM_END)
	jr z, \1
ENDM

MACRO SKIP_NEXT_INSTR
	pop bc
	inc bc
	inc bc
	push bc
ENDM

DEF CHIP_RAM_SIZE EQU 4096
DEF CHIP_ROM_SIZE EQU (CHIP_RAM_SIZE - $200)
DEF CHIP_RAM_DEADBUF_SIZE EQU 32
DEF CHIP_STACK_SIZE EQU 32
DEF CHIP_SCR_WIDTH EQU 64
DEF CHIP_SCR_HEIGHT EQU 32
DEF SCHIP_SCR_WIDTH EQU 128
DEF SCHIP_SCR_HEIGHT EQU 64
DEF SCREEN_BUF_SIZE EQU 2048

DEF FX0A_NOT_ACTIVE_FLAG EQU -1
DEF FX0A_DONE_FLAG EQU -2

DEF BG_TILE_TILEMAP_NUM EQU $80
DEF DIGIT0_TILE_NUM EQU $81
DEF F_TILE_NUM EQU DIGIT0_TILE_NUM + 10
DEF P_TILE_NUM EQU DIGIT0_TILE_NUM + 11
DEF S_TILE_NUM EQU DIGIT0_TILE_NUM + 12
DEF I_TILE_NUM EQU DIGIT0_TILE_NUM + 13
DEF COL_TILE_NUM EQU DIGIT0_TILE_NUM + 14
DEF RIGHT_LINE_TILE_NUM EQU DIGIT0_TILE_NUM + 15
DEF LEFT_LINE_TILE_NUM EQU DIGIT0_TILE_NUM + 16

DEF FPS_DIGIT0_TILEMAP_NUM EQU 4
DEF IPF_DIGIT0_TILEMAP_NUM EQU 17
DEF PAUSE_ICON_TILEMAP_NUM EQU 73

SECTION "Variables", HRAM
IS_GBC:
	ds 1
CHIP_STATE:
V0:
	ds 15
VF:
	ds 1
CHIP_SP:
	ds 1
I_REG:
	ds 2
SCHIP_RPL_FLAGS:
	ds 8
DELAY_TIMER:
	ds 1
SOUND_TIMER:
	ds 1
HIGH_RES_MODE_FLAG:
	ds 1
INSTR_COUNTER:
	ds 1 
INSTR_BLOCK_COUNTER:
	ds 1
IPF_PER_BLOCK:
	ds 1
IPF_DISPLAY:
	ds 2
DRAW_FLAG:
	ds 1
FRAME_DONE_FLAG:
	ds 1
FRAME_COUNTER:
	ds 1
DROPPED_FRAME_COUNTER:
	ds 1
FX0A_KEY_REG:
	ds 1
CHIP_STATE_END:
KEY_STATE:
	ds 1
PRESSED_KEYS:
	ds 1
RELEASED_KEYS:
	ds 1
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

SECTION "ChipStack", WRAM0, ALIGN[8]
CHIP_STACK:
	ds CHIP_STACK_SIZE

; These 32 bytes are always 0, and the purpose is to protect OOB memory reads (for example in dxyn) when I register is > $FFF,
; without having to mask it every time. since CHIP_RAM ends at DFFF, E000-FDFF is a mirror of C000 and will read from here.
SECTION "ChipRAMDeadBuf", WRAM0[$C000]
CHIP_RAM_DEAD_BUF:
	ds CHIP_RAM_DEADBUF_SIZE

SECTION "ChipScreenBuf", WRAM0, ALIGN[8]
SCREEN_BUF:
	ds SCREEN_BUF_SIZE

SECTION "VRAMScreenBuf", VRAM[$8000]
VRAM_SCREEN_BUF:
	ds SCREEN_BUF_SIZE

MACRO UPDATE_IPF ; \1 = 0 - decrease, else increase
	ldh a, [IPF_PER_BLOCK]
	IF \1 == 0
		dec a
	ELSE
		inc a
	ENDC
	ret z
	ldh [IPF_PER_BLOCK], a
	ld a, [IPF_DISPLAY]
	ld hl, IPF_BLOCKS_NUM
	IF \1 == 0
		sub [hl]
	ELSE
		add [hl]
	ENDC
	daa
	ld hl, IPF_DISPLAY
	ld [hl+], a
	ldh [temp4], a
	ld a, [hl]
	IF \1 == 0
		sbc 0
	ELSE
		adc 0
	ENDC
	daa
	ld [hl], a
	add DIGIT0_TILE_NUM
	ld hl, ($98 << 8) | IPF_DIGIT0_TILEMAP_NUM
	ld [hl+], a
	ldh a, [temp4]
	swap a
	and $F
	add DIGIT0_TILE_NUM
	ld [hl+], a
	ldh a, [temp4]
	and $F
	add DIGIT0_TILE_NUM
	ld [hl], a
	ret
ENDM

SECTION "Functions", ROM0
IncreaseIPF:
	UPDATE_IPF(1)
DecreaseIPF:
	UPDATE_IPF(0)
SecondElapsed:
	ld [hl], 60
	ld hl, DROPPED_FRAME_COUNTER
	ld a, $60 ; BCD 60
	sub [hl]
	daa
	ld [hl], 0
	; Updating fps
	ldh [temp4], a
	swap a
	and $F
	add DIGIT0_TILE_NUM
	ld hl, ($98 << 8) | FPS_DIGIT0_TILEMAP_NUM
	ld [hl+], a

	ldh a, [temp4]
	and $F
	add DIGIT0_TILE_NUM
	ld [hl], a
	ret 
ResetChip8:
	ld sp, $D000
	xor a
	ldh [rLCDC], a ; disable LCD
	jp InitChip8
PauseChip8: ; Waits for start or select press to resume.
	ld hl, ($98 << 8) | PAUSE_ICON_TILEMAP_NUM
	ld a, RIGHT_LINE_TILE_NUM
	ld [hl+], a
	ld [hl], LEFT_LINE_TILE_NUM
.continueWait:
	xor a
	ldh [rIF], a
	halt
	call UpdateKeys
	ldh a, [PRESSED_KEYS]
	bit PADB_START, a
	jr nz, .end
	bit PADB_SELECT, a
	jr z, .continueWait
.end:
	ld hl, ($98 << 8) | PAUSE_ICON_TILEMAP_NUM
	ld a, BG_TILE_TILEMAP_NUM
	ld [hl+], a
	ld [hl], a
	ret
CheckFX0A:
	ldh a, [RELEASED_KEYS]
	and ~(PADF_START | PADF_SELECT) ; ignore start and select
	ret z
	push bc
	ld b, a
	ldh a, [FX0A_KEY_REG]
	add LOW(V0)
	ld c, a
	ld h, HIGH(FX0A_KEY_MAP)

	MACRO CHECK_KEY ; \1: key, \2: label to jump on fail
		bit \1, b
		jr z, \2
		ld l, LOW(FX0A_KEY_MAP) + \1
		jr .end
	ENDM

	CHECK_KEY PADB_A, .checkB
.checkB
	CHECK_KEY PADB_B, .checkDpadUp
.checkDpadUp:
	CHECK_KEY PADB_UP, .checkDpadDown
.checkDpadDown:
	CHECK_KEY PADB_DOWN, .checkDpadRight
.checkDpadRight:
	CHECK_KEY PADB_RIGHT, .hitDpadLeft
.hitDpadLeft:	
	ld l, LOW(FX0A_KEY_MAP) + PADB_LEFT
.end:
	; load chip8 key number from map at [hl] to V register pointer at [c]
	ld a, [hl] 
	ldh [c], a 
	ld a, FX0A_DONE_FLAG
	ldh [FX0A_KEY_REG], a
	pop bc
	ret

SECTION "VBlank Handler", ROM0[$0040]
VBlankHandler:
	push af
	push hl
	ldh a, [FRAME_DONE_FLAG]
	and a
	jr z, .droppedFrame  ; if didn't finish executing opcodes for this frame yet.
	xor a
	ldh [FRAME_DONE_FLAG], a
	ldh a, [IS_GBC]
	and a
	jr z, .noFramebufCopy
	ldh a, [DRAW_FLAG] 
	and a
	jr z, .noFramebufCopy
	xor a
	ldh [DRAW_FLAG], a
	START_GDMA VRAM_SCREEN_BUF, SCREEN_BUF, 2048
.noFramebufCopy:
	ld hl, FRAME_COUNTER
	dec [hl]
	call z, SecondElapsed
	call UpdateKeys
	ldh a, [KEY_STATE]
	and $F
	cp $F ; check if all keys are held
	jp z, ResetChip8
	ldh a, [PRESSED_KEYS]
	and (PADF_START | PADF_SELECT)
	cp (PADF_START | PADF_SELECT) ; check if start + select are pressed
	jr nz, .notPaused
	call PauseChip8
	jr .end
.notPaused:
	bit PADB_START, a
	jr z, .checkSelect
	call IncreaseIPF
	jr .end
.checkSelect:
	bit PADB_SELECT, a
	call nz, DecreaseIPF
.end:
	ldh a, [FX0A_KEY_REG]
	cp FX0A_NOT_ACTIVE_FLAG
	call nz, CheckFX0A
	pop hl
	pop af
	reti 
.droppedFrame
	ldh a, [DROPPED_FRAME_COUNTER]
	inc a
	daa
	ldh [DROPPED_FRAME_COUNTER], a
	jr .noFramebufCopy

SECTION "Header", ROM0[$100]
	jr EntryPoint
	ds $150 - @, 0 ; Make room for the header

EntryPoint:
	cp $11
	jr nz, .noGBC
	ld a, 1
	jr .init
.noGBC:
	xor a
.init:
	ldh [IS_GBC], a

	; set stack
	ld sp, $D000

	; disabling audio circuitry
	xor a
	ldh [rNR52], a

	; enabling vblank interrupts
	ldh [rIF], a
	ld a, IEF_VBLANK
	ldh [rIE], a

	; waiting for vblank to disable lcd
	halt 
	xor a
	ldh [rLCDC], a

	; GBC specifc initialization:
	ldh a, [IS_GBC]
	and a
	jr z, .dmg
	; enable double speed mode
	ldh [rKEY1], a
	stop
	; set attribute map to zero (use palette 0 and tile bank 0, no flips)
	set 0, a
	ldh [rVBK], a
	MEMSET $9800, 0, $400
	xor a
	ldh [rVBK], a

	; setting palettes
	ld a, $80
	ldh [rBCPS], a ; auto-increment, initial address 0.
	ld c, LOW(rBCPD)
	; color 00 (black chip8 pixel)
	xor a
	ldh [c], a
	ldh [c], a
	; color 01 (white chip8 pixel)
	ld a, $FF
	ldh [c], a
	ldh [c], a
	; color 10 (background, light-blue)
	ld a, $55
	ldh [c], a
	ld a, $73
	ldh [c], a
	; color 11 (text, yellow)
	ld a, $FF
	ldh [c], a
	ld a, $07
	ldh [c], a
	jr .after
.dmg:
	ld a, %01100011 ; id 3 = light gray, 2 = dark gray, 1 = white, 0 = black
	ldh [rBGP], a	
.after:
	MEMCPY $9800, TILE_MAP, TILE_MAP_END
	MEMCPY_1BIT_TILES VRAM_TILES, TILES, TILES_END

	xor a
	ldh [KEY_STATE], a
	ldh [PRESSED_KEYS], a
	ldh [RELEASED_KEYS], a

	; xor first byte of RNG state with DIV so it's not all zeroes on emulators which don't emulate random WRAM values.
	ldh a, [rDIV]
	ld b, a
	ld a, [RNG_STATE]
	xor b
	ld [RNG_STATE], a

InitChip8:
	MEMCPY MAIN_JUMP_TABLE, MAIN_JUMP_TABLE_ROM, MAIN_JUMP_TABLE_ROM_END
	ldh a, [IS_GBC]
	and a
	jr nz, .after
	; rewrite DXYN jump table entry to point to the DMG version, because GBC is default.
	ld hl, (HIGH(MAIN_JUMP_TABLE) << 8) | ($D * 2)
	ld a, LOW(DXYN_LOW_RES_DMG)
	ld [hl+], a
	ld [hl], HIGH(DXYN_LOW_RES_DMG)
.after:
	MEMSET CHIP_RAM, 0, $200
	MEMCPY CHIP_RAM, CHIP8_FONT, CHIP8_FONT_END
	MEMCPY CHIP_RAM + (CHIP8_FONT_END - CHIP8_FONT), SCHIP_FONT, SCHIP_FONT_END
	MEMCPY CHIP_RAM + $200, CHIP_ROM, CHIP_ROM_END
	MEMSET CHIP_RAM_DEAD_BUF, 0, CHIP_RAM_DEADBUF_SIZE
	MEMSET CHIP_STACK, 0, CHIP_STACK_SIZE
	MEMSET CHIP_STATE, 0, CHIP_STATE_END - CHIP_STATE
	MEMSET SCREEN_BUF, 0, SCREEN_BUF_SIZE
	MEMSET VRAM_SCREEN_BUF, 0, SCREEN_BUF_SIZE

	; reset chip8 PC
	ld bc, CHIP_RAM + $200
	push bc

	ld a, 60
	ldh [FRAME_COUNTER], a

	ld a, -1
	ldh [FX0A_KEY_REG], a

	ld a, 1
	ldh [DRAW_FLAG], a

	; setting IPF variables
	ld a, [IPF_BLOCKS_NUM]
	ld e, a
	ldh [INSTR_BLOCK_COUNTER], a
	ld a, [INITIAL_IPF_PER_BLOCK]
	ldh [IPF_PER_BLOCK], a
	ldh [INSTR_COUNTER], a 

	ld b, a
	ld c, e
	call Mul8x8
	call ConvertToBCD3
	ld hl, IPF_DISPLAY
	ld a, c
	ld [hl+], a
	ld [hl], b

	; setting initial IPF digits on the tilemap.
	ld hl, ($98 << 8) + IPF_DIGIT0_TILEMAP_NUM
	ld e, DIGIT0_TILE_NUM
	ld a, b
	add e
	ld [hl+], a
	ld a, c
	swap a
	and $F
	add e
	ld [hl+], a
	ld a, c
	and $F
	add e
	ld [hl], a

	; turning LCD back on.
	ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8000
	ldh [rLCDC], a
	; enabling interrupts
	xor a
	ldh [rIF], a
	ei

InstrLoop:
	; loading chip8 PC to HL
	pop hl

	; loading chip8 instruction to DE and incrementing pc by 2
	ld a, [hl+]
	ld d, a
	ld e, [hl]
	inc hl

	; storing PC back
	push hl

	; jumping to the opcode handler based on the first opcode nibble.
	and $F0
	swap a
	JP_TABLE(MAIN_JUMP_TABLE)

MACRO INSTR_END
	ld hl, INSTR_COUNTER 
	dec [hl] 
	jp nz, InstrLoop
	jp InstrBlockEnd
ENDM

InstrBlockEnd:
	ldh a, [IPF_PER_BLOCK]
	ld [hl], a ; reload number of instructions per block
	ld hl, INSTR_BLOCK_COUNTER
	dec [hl]
	jr nz, InstrLoop
	ld a, [IPF_BLOCKS_NUM]
	ld [hl], a ; reload number of instruction blocks
InstrLoopEnd:
	ld b, 1
	ldh a, [DELAY_TIMER]
	sub b
	adc 0 ; bring back to 0, if decremented to $FF
	ldh [DELAY_TIMER], a
	ldh a, [SOUND_TIMER]
	sub b
	adc 0
	ldh [SOUND_TIMER], a
	ld a, b
	ldh [FRAME_DONE_FLAG], a
	halt ; wait for VBlank

	jr InstrLoop

InvalidInstr:
	ld b, b
	jr @

;; Instruction decoding, table for matching on first nibble

SECTION "MainJumpTable", WRAM0, ALIGN[8]
MAIN_JUMP_TABLE:
	ds 32

SECTION "MainJumpTableROM", ROM0
MAIN_JUMP_TABLE_ROM:
    dw Case0, Case1, Case2, Case3, Case4, Case5, Case6, Case7,
    dw Case8, Case9, CaseA, CaseB, CaseC, DXYN_LOW_RES_GBC, CaseE, CaseF  
MAIN_JUMP_TABLE_ROM_END:

Case0:
	; if first byte is not zero, instruction is invalid.
	ld a, d
	and a
	jp nz, InvalidInstr

	ld a, NN
	cp $EE
	jr z, OP_00EE
	cp $E0
	jr z, OP_00E0
	sub $FB
	jp z, OP_00FB
	dec a ; $FC
	jp z, OP_00FC
	dec a ; $FD
	jp z, ResetChip8 ; $00FD is exit interpreter, just doing reset for now.
	dec a ; $FE
	jp z, OP_00FE
	dec a ; $FF
	jp z, OP_00FF
	ld a, NN
	and $F0
	cp $C0
	jp z, OP_00CN
	jp InvalidInstr

OP_00EE:
	ldh a, [CHIP_SP]
	; updating SP
	sub 2
	and $1F
	ldh [CHIP_SP], a

	ld hl, sp + 0
	ld d, HIGH(CHIP_STACK)
	ld e, a
	; loading address from the chip8 stack into PC
	ld a, [de]
	ld [hl+], a
	inc e
	ld a, [de]
	ld [hl], a

	INSTR_END()

; \1 - 1 if running on GBC, 0 if not.
MACRO CLEAR_SCREEN
	IF \1 == 0
		ld hl, VRAM_SCREEN_BUF
		; change palette for color id 1 (set chip8 pixel) to be black so screen is seen as clear immediately with no tearing,
		; while actual framebuf is being cleared (takes more than half of the frame)
		ld a, %01101111
		ldh [rBGP], a
	ELSE
		ld hl, SCREEN_BUF
		xor a
	ENDC
	ld c, 8

.clearBlock\@:
	FOR i, 127
		IF \1 == 0 && (i % 5) == 0
			WAIT_VRAM_ACCESS()
			xor a
		ENDC
		ld [hl+], a
		inc l
	ENDR

	; need to use 'inc hl' instead of 'inc l', because low byte will overflow.
	ld [hl+], a
	inc hl

	dec c
	jp nz, .clearBlock\@

	IF \1 == 0
		; switch back to the regular palette.
		ld a, %01100011
		ldh [rBGP], a	
	ELSE
		inc a ; will become 1
		ldh [DRAW_FLAG], a
	ENDC

	INSTR_END()
ENDM

OP_00E0:
	ldh a, [IS_GBC]
	and a
	jp nz, .gbc
	CLEAR_SCREEN(0)
.gbc:
	CLEAR_SCREEN(1)

; \1 = 0 - left, otherwise right; \2 - 1 if running on GBC, 0 if not.
MACRO SCROLL_HORIZONTAL 
	IF \2 == 0
		IF \1 == 0
			ld hl, VRAM_SCREEN_BUF
			ld de, VRAM_SCREEN_BUF + 128
		ELSE
			ld hl, VRAM_SCREEN_BUF + (128 * 16) - 2
			ld de, VRAM_SCREEN_BUF + (128 * 15) - 2
		ENDC
	ELSE
		ld a, 1
		ldh [DRAW_FLAG], a

		IF \1 == 0
			ld hl, SCREEN_BUF
			ld de, SCREEN_BUF + 128
		ELSE
			ld hl, SCREEN_BUF + (128 * 16) - 2
			ld de, SCREEN_BUF + (128 * 15) - 2
		ENDC
	ENDC

	ld c, 120

	ldh a, [HIGH_RES_MODE_FLAG]
	and a
	jp nz, .highResMode\@
	ld a, [IS_LEGACY_SCHIP]
	and a
	jp nz , .highResMode\@

.moveTile\@:
	FOR i, 7
		IF \2 == 0 && (i % 2) == 0
			WAIT_VRAM_ACCESS()
		ENDC

		ld a, [de]
		IF \1 == 0
			ld [hl+], a
			inc l
			inc e
			inc e
		ELSE
			ld [hl-], a
			dec l
			dec e
			dec e
		ENDC
	ENDR

	; increment hl/de every eigth iteration since it can carry to high byte.
	IF \1 == 0
		ld a, [de]
		ld [hl+], a
		inc hl
		inc e
		inc de
	ELSE
		ld a, [de]
		ld [hl-], a
		dec l
		dec de
		dec e
	ENDC

	dec c
	jr nz, .moveTile\@

	; clear right/left column
	IF \2 == 0
		IF \1 == 0
			ld hl, VRAM_SCREEN_BUF + (128 * 15)
		ELSE
			ld hl, VRAM_SCREEN_BUF
		ENDC
	ELSE
		IF \1 == 0
			ld hl, SCREEN_BUF + (128 * 15)
		ELSE
			ld hl, SCREEN_BUF
		ENDC
		xor a
	ENDC

	ld c, 8

.clearTile\@:
	FOR i, 8
		IF \2 == 0 && (i % 4) == 0
			WAIT_VRAM_ACCESS()
			xor a
		ENDC

		ld [hl+], a
		inc l
	ENDR
	dec c
	jr nz, .clearTile\@

	; early instruction loop break, because scroll takes big part of the frame.
	jp InstrLoopEnd

.highResMode\@:
	REPT 8
		IF \2 == 0
			WAIT_VRAM_ACCESS()
		ENDC

		ld a, [de]
		; if scrolling left, create mask with msb nibble moved to lsb. If right, then lsb to msb.
		swap a
		IF \1 == 0 ; a >> 4
			and $0F 
		ELSE ; a << 4
			and $F0
		ENDC
		ld b, a
		ld a, [hl]
		swap a
		IF \1 == 0 ; a << 4
			and $F0
		ELSE ; a >> 4
			and $0F
		ENDC
		; apply mask
		or b
		IF \1 == 0
			ld [hl+], a
			inc hl
			inc e
			inc de
		ELSE
			ld [hl-], a
			dec l
			dec de
			dec e
		ENDC
	ENDR

	dec c
	jp nz, .highResMode\@

	; clear half of right/left column
	IF \1 == 0
		IF \2 == 0
			ld hl, VRAM_SCREEN_BUF + (128 * 15)
		ELSE
			ld hl, SCREEN_BUF + (128 * 15)
		ENDC
		ld bc, ($F0 << 8) | 8
	ELSE
		IF \2 == 0
			ld hl, VRAM_SCREEN_BUF
		ELSE
			ld hl, SCREEN_BUF
		ENDC
		ld bc, ($0F << 8) | 8
	ENDC

.clearHalfTile\@
	FOR i, 8
		IF \2 == 0 && (i % 2) == 0
			WAIT_VRAM_ACCESS()
		ENDC

		ld a, [hl]
		and b
		ld [hl+], a
		inc l
	ENDR
	dec c
	jr nz, .clearHalfTile\@

	jp InstrLoopEnd
ENDM

OP_00FB: ; scroll right 4 pixels 
	ldh a, [IS_GBC]
	and a
	jp nz, .gbc
	SCROLL_HORIZONTAL 1, 0
.gbc:
	ld b, b
	SCROLL_HORIZONTAL 1, 1

OP_00FC: ; scroll left 4 pixels
	ldh a, [IS_GBC]
	and a
	jp nz, .gbc
	SCROLL_HORIZONTAL 0, 0
.gbc:
	ld c, c
	SCROLL_HORIZONTAL 0, 1

; \1 - 1 if running on GBC, 0 if not.
MACRO SCROLL_DOWN
	LD_N()
	jp z, .scroll0\@
	ld b, a
	; scroll twice as much in modern schip lores mode
	ldh a, [HIGH_RES_MODE_FLAG]
	and a
	jr nz, .skipAdd\@
	ld a, [IS_LEGACY_SCHIP]
	and a
	jr nz, .skipAdd\@
	ld a, b
	add a
	ld b, a
.skipAdd\@:
	; temp1, c = 64 - b (number of iterations per column)
	ld a, b
	cpl
	add 64 + 1
	ldh [temp1], a
	ld c, a

	IF \1 == 0
		ld hl, VRAM_SCREEN_BUF + (128 * 16) - 2
	ELSE
		ld hl, SCREEN_BUF + (128 * 16) - 2
		ld a, 1
		ldh [DRAW_FLAG], a 
	ENDC
	; de = hl - (a * 2)
	ld a, b
	ldh [temp2], a ; saving N in temp2
	add a  
	ld b, a ; saving (a * 2) in b
	ld a, l
	sub b
	ld d, h
	ld e, a

	REPT 16
	.moveTile\@:
		IF \1 == 0
			WAIT_VRAM_ACCESS()
		ENDC

		ld a, [de]
		ld [hl-], a

		dec l
		dec de
		dec e

		dec c
		jr nz, .moveTile\@

		; src ptr (de) now points to the end of the new column, so set hl to it.
		ld h, d
		ld l, e

		; subtracting bytes per iteration from e to move src ptr to the correct position in the new column.
		ld a, e
		sub b
		ld e, a

		; reloading c
		ldh a, [temp1]
		ld c, a
	ENDR

	; clearing first N rows:

	ldh a, [temp2] ; saved N
	ld b, a
	; storing 128 - (b * 2) in de (number of bytes to the first row of the next column).
	ld a, 128
	sub b
	sub b
	ld d, 0
	ld e, a
	
	IF \1 == 0
		ld hl, VRAM_SCREEN_BUF
	ELSE
		ld hl, SCREEN_BUF
		xor a
	ENDC

	REPT 16
		ld c, b
	.clearTile\@:
		IF \1 == 0
			WAIT_VRAM_ACCESS()
			xor a
		ENDC

		ld [hl+], a
		inc l
		dec c
		jr nz, .clearTile\@
		; moving to the next horizontal tile:
		add hl, de
	ENDR

	; early instruction loop break, because scroll takes big part of the frame.
	jp InstrLoopEnd

.scroll0\@:
	INSTR_END()
ENDM

OP_00CN: ; scroll down N pixels
	ldh a, [IS_GBC]
	and a
	jp nz, .gbc
	SCROLL_DOWN 0
.gbc:
	SCROLL_DOWN 1

; \1 - 0 is lores, 1 is hires
MACRO CHANGE_RES_MODE
	IF \1 == 0
		xor a
	ELSE
		ld a, 1
	ENDC
	ldh [HIGH_RES_MODE_FLAG], a

	; rewriting jump table entry for DXYN to point to the new version.
	ld hl, (HIGH(MAIN_JUMP_TABLE) << 8) | ($D * 2)
	ldh a, [IS_GBC]
	and a
	jr nz, .GBC\@
	IF \1 == 0
		ld a, LOW(DXYN_LOW_RES_DMG)
		ld [hl+], a
		ld [hl], HIGH(DXYN_LOW_RES_DMG)
	ELSE
		ld a, LOW(DXYN_HIGH_RES_DMG)
		ld [hl+], a
		ld [hl], HIGH(DXYN_HIGH_RES_DMG)
	ENDC
	jr .end\@
.GBC\@:
	IF \1 == 0
		ld a, LOW(DXYN_LOW_RES_GBC)
		ld [hl+], a
		ld [hl], HIGH(DXYN_LOW_RES_GBC)
	ELSE
		ld a, LOW(DXYN_HIGH_RES_GBC)
		ld [hl+], a
		ld [hl], HIGH(DXYN_HIGH_RES_GBC)
	ENDC
.end\@:
	ld a, [IS_LEGACY_SCHIP]
	and a
	jp z, OP_00E0 ; legacy schip doesn't clear screen on mode changes
	INSTR_END()
ENDM

OP_00FE:
	CHANGE_RES_MODE(0)
OP_00FF:
	CHANGE_RES_MODE(1)

Case1: 
OP_1NNN:
	ld hl, sp + 1
	ld a, d
	and $F
	add HIGH(CHIP_RAM)
	ld [hl-], a
	ld [hl], NN

	INSTR_END()

Case2:
OP_2NNN:
	; saving current PC in BC
	pop bc

	; storing new PC
	LD_NNN_RAM_PTR()
	push hl
	
	; updating SP
	ldh a, [CHIP_SP]
	ld e, a ; saving current SP
	add 2
	and $1F
	ldh [CHIP_SP], a

	; saving BC (old PC) to the chip8 stack
	ld d, HIGH(CHIP_STACK)
	ld a, c
	ld [de], a
	inc e
	ld a, b
	ld [de], a

	INSTR_END()

Case3:
OP_3XNN:
	LD_VX()
	cp NN
	jr nz, .end ; skip if equal
	SKIP_NEXT_INSTR()
.end:
	INSTR_END()

Case4:
OP_4XNN:
	LD_VX()
	cp NN
	jr z, .end ; skip if not equal
	SKIP_NEXT_INSTR()
.end:
	INSTR_END()

Case5:
OP_5XY0:
	LD_N()
	jp nz, InvalidInstr

	LD_VY_PTR_HL()
	LD_VX()

	cp [hl]
	jr nz, .end
	SKIP_NEXT_INSTR()
.end:
	INSTR_END()

Case6:
OP_6XNN:
	LD_VX_PTR_C()
	ld a, NN
	ldh [c], a

	INSTR_END()

Case7:
OP_7XNN:
	LD_VX()
	add NN
	ldh [c], a

	INSTR_END()

Case8:
	LD_N()
	JP_TABLE(_8XYJumpTable)

Case9:
OP_9XY0: 
	LD_N()
	jp nz, InvalidInstr

	LD_VY_PTR_HL()
	LD_VX()

	cp [hl]
	jr z, .end
	SKIP_NEXT_INSTR()
.end:
	INSTR_END()

CaseA: 
OP_ANNN:
	ld a, NN
	ldh [I_REG], a
	ld a, d
	and $F
	ldh [I_REG + 1], a

	INSTR_END()
	
CaseB:
OP_BNNN: ; Quirk off, jump to V[0] + NNN
	ldh a, [V0]
	ld b, 0
	ld c, a

	LD_NNN()
	add hl, bc
	; Mask to $FFF
	ld a, h
	and $F

	; Updating PC
	add HIGH(CHIP_RAM)
	ld h, a
	pop af
	push hl

	INSTR_END()

CaseC:
OP_CXNN: 
	RAND()
	ld a, b
	xor c
	ld b, a
	LD_VX_PTR_C()
	ld a, b
	ldh [c], a

	INSTR_END()

SECTION "DXYNGBCLoresLookup", ROM0, ALIGN[8]
DXYN_GBC_LORES_BASE_ADDR_LOOKUP:
	FOR x, CHIP_SCR_WIDTH
		db LOW((x / 4) * 128 + SCREEN_BUF)
		db HIGH((x / 4) * 128 + SCREEN_BUF)
	ENDR
SECTION "DXYNGBCHiresLookup", ROM0, ALIGN[8]
DXYN_GBC_HIRES_BASE_ADDR_LOOKUP:
	FOR x, SCHIP_SCR_WIDTH
		db LOW((x / 8) * 128 + SCREEN_BUF)
		db HIGH((x / 8) * 128 + SCREEN_BUF)
	ENDR

SECTION "DXYNDMGLoresLookup", ROM0, ALIGN[8]
DXYN_DMG_LORES_BASE_ADDR_LOOKUP:
	FOR x, CHIP_SCR_WIDTH
		db LOW((x / 4) * 128 + VRAM_SCREEN_BUF)
		db HIGH((x / 4) * 128 + VRAM_SCREEN_BUF)
	ENDR
SECTION "DXYNDMGHiresLookup", ROM0, ALIGN[8]
DXYN_DMG_HIRES_BASE_ADDR_LOOKUP:
	FOR x, SCHIP_SCR_WIDTH
		db LOW((x / 8) * 128 + VRAM_SCREEN_BUF)
		db HIGH((x / 8) * 128 + VRAM_SCREEN_BUF)
	ENDR

SECTION "DXYNLoresMaskLookup", ROM0, ALIGN[8]
DXYN_LORES_MASK_LOOKUP:
	FOR i, CHIP_SCR_WIDTH 
		db (%11000000 >> ((i & 3) * 2))
	ENDR
SECTION "DXYNHiresMaskLookup", ROM0, ALIGN[8]
DXYN_HIRES_MASK_LOOKUP:
	FOR i, SCHIP_SCR_WIDTH 
		db (%10000000 >> (i & 7))
	ENDR

CaseD: 
OP_DXYN:
	; \1 - 1 if running on GBC, 0 if not
	; \2 - 1 if superchip hires, 0 if not 
	; \3 - 0 if need to preserve hl, 1 if not
	MACRO DXYN_PROCESS_PIXEL ; mask pointer is in BC
		ld a, [bc]
		ld c, a ; saving mask in 'c'

		; DMG, must wait for hblank/vblank so VRAM is accessible
		IF \1 == 0
			WAIT_VRAM_ACCESS()
			ld a, c
		ENDC

		; collision detection:
		and [hl]
		jr z, .noCollision\@
		; don't set VF immediately to 1 on DMG since it costs 2 more cycles, instead set VF to any non-zero value (a),
		; and correct it to be 1 at the end of DXYN. This is needed so even in the worst case 
		; (WAIT_VRAM_ACCESS returns in the start of OAM scan), second one is not needed because the last vram write (ld [hl-], a)
		; will occur on the last oam scan cycle so VRAM is still writable (4 + 1 + 2 + 2 + 3 + 1 + 2 + 2 + 1 + 2 = 20)
		IF \1 != 0
			ld a, 1
		ENDC
		ldh [VF], a
	.noCollision\@:
		; drawing:
		ld a, c
		xor [hl]
		; if superchip hires is not active then write the byte again to hl + 2, since framebuf is double scaled.
		IF \2 == 0 
			ld [hl+], a
			inc l
			ld [hl-], a
			IF \3 == 0
				dec l
			ENDC
		ELSE
			ld [hl], a
		ENDC
	ENDM

	; \1 - 1 if running on GBC, 0 if not
	MACRO DXYN_FINISH
		; on DMG correct VF to be either 0 or 1 if collision occured, not other value.
		IF \1 == 0 
			ldh a, [VF]
			jr z, .noCollision\@
			ld a, 1
			ldh [VF], a
		.noCollision\@:
		ENDC
		INSTR_END()
	ENDM

	; \1 - loop label
	; \2 - 1 if running on GBC, 0 if not
	; \3 - 1 if superchip hires, 0 if not
	MACRO DXYN_CHECK_LOOP
		pop bc
		ldh a, [temp2] ; Y
		inc a
		IF \3 == 0
			cp CHIP_SCR_HEIGHT
		ELSE
			cp SCHIP_SCR_HEIGHT
		ENDC
		jr z, .yClip\@
		ldh [temp2], a
		; add 2 to screen buf pointer
		inc l
		inc l
		IF \3 == 0 ; add 2 more (skip next row since in lores it already was writen to)
			inc l
			inc l
		ENDC
		ldh a, [temp1] ; height counter
		dec a
		ldh [temp1], a
		jp nz, \1
	.yClip\@:
		DXYN_FINISH(\2)
	ENDM

	; \1 - 1 if superchip hires, 0 if not
	MACRO DXYN_LD_MASK_HIGH_PTR
		IF \1 == 0
			ld b, HIGH(DXYN_LORES_MASK_LOOKUP)
		ELSE
			ld b, HIGH(DXYN_HIRES_MASK_LOOKUP)
		ENDC
	ENDM

	; \1 - 1 if running on GBC, 0 if not.
	; \2 - 1 if superchip hires, 0 if not;
	; \3 - height or -1 if not known at compile time
	MACRO DRAW_SPRITE
	.heightLoop\@:
		ld a, [bc]
		IF \3 != 1
			inc bc
			IF \3 == 16
				ld e, a
				ld a, [bc]
				ldh [temp3], a ; saving second byte
				inc bc
			ENDC
			push bc
		ENDC

		DXYN_LD_MASK_HIGH_PTR(\2)
		ld c, d

		IF \3 != 16
			cp $80
			jr nz, .regularDraw\@
			DXYN_PROCESS_PIXEL \1, \2, \3 == 1 ; single-pixel draw
			IF \3 == 1
				DXYN_FINISH(\1)
			ELSE
				DXYN_CHECK_LOOP .heightLoop\@, \1, \2
			ENDC		
		ENDC

    REDEF xClipLabel EQUS ".xClip\@"

	.regularDraw\@:
		IF \3 != 16 ; otherwise, it was already saved to e
			ld e, a
		ENDC
		push hl
    	FOR i, 7, -1, -1
			IF i != 7
				ld a, d
				add (7 - i)
				IF \2 == 0
					cp CHIP_SCR_WIDTH
					jp z, xClipLabel
					and $3
				ELSE
					cp SCHIP_SCR_WIDTH
					jp z, xClipLabel
					and $7
				ENDC
				jr nz, .noOffset\@
				ld bc, 128
				add hl, bc
				DXYN_LD_MASK_HIGH_PTR(\2)
			ENDC
		.noOffset\@:
       		bit i, e
        	jr z, .skipPixel\@
			IF i != 7
				ld c, a ; x-offset
			ENDC
        	DXYN_PROCESS_PIXEL \1, \2, (i == 0 && \3 != 16)
    	.skipPixel\@:
    	ENDR

		IF \3 == 16 ; 16x16
			ldh a, [temp3] ; loading second column
			ld e, a

			FOR i, 7, -1, -1
				ld a, d
				add (15 - i)
				IF \2 == 0
					cp CHIP_SCR_WIDTH
					jp z, xClipLabel
					and $3
				ELSE
					cp SCHIP_SCR_WIDTH
					jp z, xClipLabel
					and $7
				ENDC
				jr nz, .noOffset\@
				ld bc, 128
				add hl, bc
				DXYN_LD_MASK_HIGH_PTR(\2)				
			.noOffset\@:
       			bit i, e
        		jr z, .skipPixel\@
				ld c, a
        		DXYN_PROCESS_PIXEL \1, \2, i == 0
    		.skipPixel\@:
    		ENDR
		ENDC

 	{xClipLabel}:
		pop hl
    	IF \3 == 1
		    DXYN_FINISH(\1)
    	ELSE
		    DXYN_CHECK_LOOP .heightLoop\@, \1, \2
    	ENDC
	ENDM

	; \1 - 1 if running on GBC, 0 if not.
	; \2 - 1 if superchip hires, 0 if not; 
	; \3 - 1 if need to save Y in temp2, 0 if not.
	MACRO LOAD_Y
		LD_VY()
		IF \2 == 0
			and CHIP_SCR_HEIGHT - 1
		ELSE
			and SCHIP_SCR_HEIGHT - 1
		ENDC
		IF \3 != 0
			ldh [temp2], a
		ENDC

		; add y * 2 to screen buf addr in hires, y * 4 in lores
		add a
		IF \2 == 0
			add a
		ENDC
		add l
		ld l, a

		; reset VF (both VX and VY are fetched now)
		xor a
		ldh [VF], a

		; set draw flag to 1 on GBC
		IF \1 != 0
			inc a
			ldh [DRAW_FLAG], a
		ENDC
	ENDM

	; \1 - 1 if running on GBC, 0 if not. \2 - 1 if superchip hires, 0 if not
	MACRO DXYN
		LD_VX()
		IF \1 == 0
			IF \2 == 0
				and CHIP_SCR_WIDTH - 1
				ld h, HIGH(DXYN_DMG_LORES_BASE_ADDR_LOOKUP)
			ELSE
				and SCHIP_SCR_WIDTH - 1
				ld h, HIGH(DXYN_DMG_HIRES_BASE_ADDR_LOOKUP)
			ENDC
		ELSE
			IF \2 == 0
				and CHIP_SCR_WIDTH - 1
				ld h, HIGH(DXYN_GBC_LORES_BASE_ADDR_LOOKUP)
			ELSE
				and SCHIP_SCR_WIDTH - 1
				ld h, HIGH(DXYN_GBC_HIRES_BASE_ADDR_LOOKUP)
			ENDC
		ENDC

		ld d, a
		add a ; x * 2 (each entry is 2 bytes)
		ld l, a
		ld a, [hl+]
		ld h, [hl]
		ld l, a

		LD_N()
		dec a ; check if N == 1 
		jp nz, .regularDraw\@
		LOAD_Y \1, \2, 0
		LD_I_MEM_PTR b, c 
		DRAW_SPRITE \1, \2, 1 ; 8x1 (single row) draw
	.regularDraw\@:
		inc a
		jp z, .draw16x16\@
		ldh [temp1], a ; saving height in temp1
		LOAD_Y \1, \2, 1
		LD_I_MEM_PTR b, c
	.draw8xN\@:
		DRAW_SPRITE \1, \2, -1 
	.draw16x16\@:
		ld a, 16
		ldh [temp1], a
		LOAD_Y \1, \2, 1
		LD_I_MEM_PTR b, c
		IF \2 == 0 ; legacy schip draws 8x16 instead of 16x16 in lores mode
			ld a, [IS_LEGACY_SCHIP]
			and a
			jp nz, .draw8xN\@
		ENDC
		DRAW_SPRITE \1, \2, 16
	ENDM

DXYN_LOW_RES_DMG:
	DXYN 0, 0
DXYN_HIGH_RES_DMG:
	DXYN 0, 1
DXYN_LOW_RES_GBC:
	DXYN 1, 0
DXYN_HIGH_RES_GBC:
	DXYN 1, 1

MACRO LD_KEY
	LD_VX()
	and $F
	ld h, HIGH(KEY_MAP)
	ld l, a
	ld a, [hl]
	cp -1 ; key is not mapped
	jr z, .end\@
	ld b, a
	ldh a, [KEY_STATE]
	and b
.end\@:
ENDM
	
CaseE: 
	ld a, NN
	cp $A1
	jr z, OP_EXA1
	cp $9E
	jp nz, InvalidInstr

OP_EX9E:
	LD_KEY()
	jr z, .end ; dont skip if key is not pressed
	SKIP_NEXT_INSTR()
.end:
	INSTR_END()

OP_EXA1:
	LD_KEY()
	jr nz, .end ; dont skip if key is pressed
	SKIP_NEXT_INSTR()
.end:
	INSTR_END()
	
CaseF:
	ld a, NN
	bit 7, a
	jr nz, .bit7Set
	JP_TABLE(FXJumpTable)
.bit7Set:
	cp $85
	jp z, FX85
	jp InvalidInstr

SECTION "8XY Jump Table", ROM0, ALIGN[8] 
_8XYJumpTable:
	dw _8XY0, _8XY1, _8XY2, _8XY3, _8XY4, _8XY5, _8XY6, _8XY7,
	dw InvalidInstr, InvalidInstr, InvalidInstr, InvalidInstr,
	dw InvalidInstr, InvalidInstr, _8XYE, InvalidInstr

_8XY0:
	LD_VX_PTR_HL()
	LD_VY()
	ld [hl], a

	INSTR_END()
_8XY1:
	LD_VY_PTR_HL()
	LD_VX()

	or [hl]
	ldh [c], a

	; vF reset quirk
	xor a
	ldh [VF], a

	INSTR_END()
_8XY2:
	LD_VY_PTR_HL()
	LD_VX()

	and [hl]
	ldh [c], a

	; vF reset quirk
	xor a
	ldh [VF], a

	INSTR_END()
_8XY3:
	LD_VY_PTR_HL()
	LD_VX()

	xor [hl]
	ldh [c], a

	; vF reset quirk
	xor a
	ldh [VF], a

	INSTR_END()

; \1 = 0  - inverse carry (used for subtractions), 1 is regular carry.
MACRO SET_VF_CARRY
	IF \1 == 0
		sbc a
		inc a
		ldh [VF], a
	ELSE
		sbc a
		and $1
		ldh [VF], a
	ENDC
ENDM

_8XY4:
	LD_VY_PTR_HL()
	LD_VX()

	add [hl]
	ldh [c], a

	SET_VF_CARRY(1)
	INSTR_END()
_8XY5:
	LD_VY_PTR_HL()
	LD_VX()

	sub [hl]
	ldh [c], a

	SET_VF_CARRY(0)
	INSTR_END()
_8XY6:
	; shifting quirk
	LD_VX_PTR_HL()
	LD_VY()
	rra ; carry should be clear after LD_VY
	ld [hl], a

	SET_VF_CARRY(1)
	INSTR_END()
_8XY7:
	LD_VX_PTR_HL()
	LD_VY()

	sub [hl]
	ld [hl], a

	SET_VF_CARRY(0)
	INSTR_END()
_8XYE:
	; shifting quirk
	LD_VX_PTR_HL()
	LD_VY()
	add a ; Same as left shift
	ld [hl], a

	SET_VF_CARRY(1)
	INSTR_END()

; Matching on 7 low bits of the instruction, so 128 entries fit in a page.
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
    ds 12, LOW(InvalidInstr), HIGH(InvalidInstr) ; $2A-$2F
	dw FX30 ; $30
	ds 4, LOW(InvalidInstr), HIGH(InvalidInstr)  ; $31-$32
    dw FX33 ; $33
    ds 66, LOW(InvalidInstr), HIGH(InvalidInstr) ; $34-$54
    dw FX55 ; $55
    ds 30, LOW(InvalidInstr), HIGH(InvalidInstr) ; $56-$64
    dw FX65 ; $65
	ds 30, LOW(InvalidInstr), HIGH(InvalidInstr) ; $66-$74
	dw FX75 ; $75
    ds 20, LOW(InvalidInstr), HIGH(InvalidInstr) ; $76-$7F

FX07:
	LD_VX_PTR_C()
	ldh a, [DELAY_TIMER]
	ldh [c], a

	INSTR_END()

FX0A:
	ldh a, [FX0A_KEY_REG]
	cp FX0A_NOT_ACTIVE_FLAG
	jr nz, .skipFX0AInit
	LD_X()
	ldh [FX0A_KEY_REG], a
.skipFX0AInit:
	cp FX0A_DONE_FLAG
	jr z, .FX0ADone
	; PC -= 2
	pop bc
	dec bc
	dec bc
	push bc
	; jump straight to the frame end to limit CPU usage while waiting for key.
	jp InstrLoopEnd
.FX0ADone:
	ld a, FX0A_NOT_ACTIVE_FLAG
	ldh [FX0A_KEY_REG], a
	INSTR_END()

FX15:
	LD_VX()
	ldh [DELAY_TIMER], a

	INSTR_END()

FX18:
	LD_VX()
	ldh [SOUND_TIMER], a

	INSTR_END()

FX1E:
	LD_VX_PTR_HL()
	ld c, LOW(I_REG)

	; Store low
	ldh a, [c]
	add [hl]
	ldh [c], a

	; Store high
	inc c
	ldh a, [c]
	adc 0
	and $F ; Keep I within $FFF
	ldh [c], a

	INSTR_END()

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

	INSTR_END()

FX30:
	LD_VX()
	and $F
	ld b, a

	; Multiply by 10
	add a
	add a
	add b
	add a

	; schip font is stored after chip8 font, so add offset
	add (CHIP8_FONT_END - CHIP8_FONT)

	ldh [I_REG], a
	xor a
	ldh [I_REG + 1], a

	INSTR_END()

FX33:
	LD_I_MEM_PTR h, l
	LD_VX()

	ld b, 100
	ld c, -1

.countHundreds:
	inc c
	sub b
	jr nc, .countHundreds

	add b
	ld b, a

	ld a, c
	ld [hl+], a ; Storing hundreds
	CHECK_MEM_WRITE_OOB(.end)

	ld a, b
	ld b, 10
	ld c, -1

.countTenths:
	inc c
	sub b
	jr nc, .countTenths

	add b
	ld b, a

	ld a, c
	ld [hl+], a ; Storing tenths
	CHECK_MEM_WRITE_OOB(.end)

	ld [hl], b ; Storing ones
.end:
	INSTR_END()

MACRO REG_STORE ; \1 = 0 - load from ram, else, store to ram.
	LD_X()
	ld b, a 
	inc b ; v[x] is included

	LD_I_MEM_PTR h, l
	ld c, LOW(V0)
.copyLoop\@
	IF \1 == 0
		ld a, [hl+]
		ldh [c], a
	ELSE
		CHECK_MEM_WRITE_OOB(.end\@)
		ldh a, [c]
		ld [hl+], a
	ENDC

	inc c
	dec b
	jr nz, .copyLoop\@

	; memory increment quirk:
	; CHIP_RAM is located in $D000, so its enough to subtract D0 from h to get the I register, and store it back.
	ld a, l
	ldh [I_REG], a
	ld a, h
	sub HIGH(CHIP_RAM)
	and $F ; keep within $FFF
	ldh [I_REG + 1], a
.end\@:
	INSTR_END()
ENDM

FX55: 
	REG_STORE(1)

FX65: 
	REG_STORE(0)

MACRO SCHIP_RPL_STORE ; \1 = 0 - load from flags, else, store to flags.
	LD_VX()
	and $7
	ld b, a
	inc b ; v[x] is included

	ld hl, SCHIP_RPL_FLAGS
	ld c, LOW(V0)
.copyLoop\@
	IF \1 == 0
		ld a, [hl+]
		ldh [c], a
	ELSE
		ldh a, [c]
		ld [hl+], a
	ENDC

	inc c
	dec b
	jr nz, .copyLoop\@

	INSTR_END()
ENDM

FX75:
	SCHIP_RPL_STORE(1)

FX85:
	SCHIP_RPL_STORE(0)

SECTION "VRAMTiles", VRAM[$8000 + SCREEN_BUF_SIZE]
VRAM_TILES:

SECTION "Tiles", ROM0
TILES:
	ds 8, $00 ; Tile $80, used for background around the 128x64 screen 
	db 124, 198, 206, 222, 246, 230, 124, 0 ; 0
	db 48, 112, 48, 48, 48, 48, 252, 0 		; 1
	db 120, 204, 12, 56, 96, 204, 252, 0	; 2
	db 120, 204, 12, 56, 12, 204, 120, 0	; 3
	db 28, 60, 108, 204, 254, 12, 30, 0		; 4
	db 252, 192, 248, 12, 12, 204, 120, 0	; 5
	db 56, 96, 192, 248, 204, 204, 120, 0	; 6
    db 252, 204, 12, 24, 48 ,48, 48, 0 		; 7
    db 120, 204, 204, 120, 204, 204, 120, 0	; 8
	db 120, 204, 204, 124, 12, 24, 112, 0	; 9
	db 254, 98, 104, 120, 104, 96, 240, 0	; F
	db 252, 102, 102, 124, 96, 96, 240, 0	; P
	db 120, 204, 96, 48, 24, 204, 120, 0	; S
	db 120, 48, 48, 48, 48, 48, 120, 0		; I
	db 0, 48, 48, 0, 0, 48, 48, 0			; :
	db 6, 6, 6, 6, 6, 6, 6, 6			    ;  |
	db 96, 96, 96, 96, 96, 96, 96, 96		; |
TILES_END:

SECTION "TileMap", ROM0
TILE_MAP:
    db F_TILE_NUM, P_TILE_NUM, S_TILE_NUM, COL_TILE_NUM, DIGIT0_TILE_NUM, DIGIT0_TILE_NUM
    db $80,$80,$80,$80,$80,$80,$80

    db I_TILE_NUM, P_TILE_NUM, F_TILE_NUM, COL_TILE_NUM, DIGIT0_TILE_NUM, DIGIT0_TILE_NUM, DIGIT0_TILE_NUM
    db $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80

    REPT 4
    REPT 32
        db $80
    ENDR
    ENDR

    db $80,$80, $00,$08,$10,$18,$20,$28,$30,$38,$40,$48,$50,$58,$60,$68,$70,$78, $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
    db $80,$80, $01,$09,$11,$19,$21,$29,$31,$39,$41,$49,$51,$59,$61,$69,$71,$79, $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
    db $80,$80, $02,$0A,$12,$1A,$22,$2A,$32,$3A,$42,$4A,$52,$5A,$62,$6A,$72,$7A, $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
    db $80,$80, $03,$0B,$13,$1B,$23,$2B,$33,$3B,$43,$4B,$53,$5B,$63,$6B,$73,$7B, $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
    db $80,$80, $04,$0C,$14,$1C,$24,$2C,$34,$3C,$44,$4C,$54,$5C,$64,$6C,$74,$7C, $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
    db $80,$80, $05,$0D,$15,$1D,$25,$2D,$35,$3D,$45,$4D,$55,$5D,$65,$6D,$75,$7D, $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
    db $80,$80, $06,$0E,$16,$1E,$26,$2E,$36,$3E,$46,$4E,$56,$5E,$66,$6E,$76,$7E, $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
    db $80,$80, $07,$0F,$17,$1F,$27,$2F,$37,$3F,$47,$4F,$57,$5F,$67,$6F,$77,$7F, $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80

    REPT 5
    REPT 32
        db $80
    ENDR
    ENDR
TILE_MAP_END:

SECTION "Settings", ROMX[$4000]
KEY_MAP:
	db -1
	db PADF_B
	db PADF_A
	db PADF_DOWN
	db -1
	db -1
	db PADF_UP
	db PADF_LEFT
	db -1
	db PADF_RIGHT
	db -1
	db -1
	db -1
	db -1
	db -1
	db -1
FX0A_KEY_MAP: ; The opposite (gb key bits as indexes to chip8 keys)
	db 2 ; A
	db 1 ; B
	db -1 ; start, unused
	db -1 ; select, unused
	db 9 ; dpad right
	db 7 ; dpad left
	db 6 ; dpad up
	db 3 ; dpad down
CHIP8_FONT:
    db $60, $A0, $A0, $A0, $C0, ; 0
    db $40, $C0, $40, $40, $E0, ; 1
    db $C0, $20, $40, $80, $E0, ; 2
    db $C0, $20, $40, $20, $C0, ; 3
    db $20, $A0, $E0, $20, $20, ; 4
    db $E0, $80, $C0, $20, $C0, ; 5
    db $40, $80, $C0, $A0, $40, ; 6
    db $E0, $20, $60, $40, $40, ; 7
    db $40, $A0, $40, $A0, $40, ; 8
    db $40, $A0, $60, $20, $40, ; 9
    db $40, $A0, $E0, $A0, $A0, ; A
    db $C0, $A0, $C0, $A0, $C0, ; B
    db $60, $80, $80, $80, $60, ; C
    db $C0, $A0, $A0, $A0, $C0, ; D
    db $E0, $80, $C0, $80, $E0, ; E
    db $E0, $80, $C0, $80, $80, ; F
CHIP8_FONT_END:
SCHIP_FONT:
	db $7C, $C6, $CE, $DE, $D6, $F6, $E6, $C6, $7C, $00, ; 0
    db $10, $30, $F0, $30, $30, $30, $30, $30, $FC, $00, ; 1
    db $78, $CC, $CC, $0C, $18, $30, $60, $CC, $FC, $00, ; 2
    db $78, $CC, $0C, $0C, $38, $0C, $0C, $CC, $78, $00, ; 3
    db $0C, $1C, $3C, $6C, $CC, $FE, $0C, $0C, $1E, $00, ; 4
    db $FC, $C0, $C0, $C0, $F8, $0C, $0C, $CC, $78, $00, ; 5
    db $38, $60, $C0, $C0, $F8, $CC, $CC, $CC, $78, $00, ; 6
    db $FE, $C6, $C6, $06, $0C, $18, $30, $30, $30, $00, ; 7
    db $78, $CC, $CC, $EC, $78, $DC, $CC, $CC, $78, $00, ; 8
    db $7C, $C6, $C6, $C6, $7C, $18, $18, $30, $70, $00, ; 9
    db $30, $78, $CC, $CC, $CC, $FC, $CC, $CC, $CC, $00, ; A
    db $FC, $66, $66, $66, $7C, $66, $66, $66, $FC, $00, ; B
    db $3C, $66, $C6, $C0, $C0, $C0, $C6, $66, $3C, $00, ; C
    db $F8, $6C, $66, $66, $66, $66, $66, $6C, $F8, $00, ; D
    db $FE, $62, $60, $64, $7C, $64, $60, $62, $FE, $00, ; E
    db $FE, $66, $62, $64, $7C, $64, $60, $60, $F0, $00, ; F
SCHIP_FONT_END:
INITIAL_IPF_PER_BLOCK:
	db 100; 1; 96 ; 202
IPF_BLOCKS_NUM:
	db 2
IS_LEGACY_SCHIP:
	db 0

; CHIP_ROM: ; 1dcell
; 	db 18, 138, 128, 124, 1, 109, 0, 34, 33, 104, 1, 57, 0, 34, 67, 136, 162, 56,
; 	db 0, 221, 193, 125, 1, 61, 64, 18, 7, 123, 1, 109, 0, 0, 238, 105, 0, 128,
; 	db 208, 208, 177, 79, 1, 121, 2, 208, 177, 112, 255, 208, 177, 79, 1, 121, 4,
; 	db 208, 177, 112, 2, 208, 177, 79, 1, 121, 1, 208, 177, 0, 238, 98, 0, 136,
; 	db 142, 114, 1, 143, 144, 143, 39, 79, 0, 18, 69, 0, 238, 0, 224, 109, 0, 108,
; 	db 5, 139, 192, 122, 1, 34, 105, 96, 31, 129, 192, 162, 2, 208, 17, 0, 238,
; 	db 128, 160, 162, 135, 240, 51, 242, 101, 100, 0, 240, 41, 99, 0, 211, 69, 241,
; 	db 41, 115, 5, 211, 69, 242, 41, 115, 5, 211, 69, 0, 238, 0, 0, 0, 106, 20, 34,
; 	db 83, 34, 3, 76, 31, 34, 83, 18, 142

; 	ds CHIP_ROM_SIZE - (@ - CHIP_ROM), 0
; CHIP_ROM_END:

; CHIP_ROM: ; edge scroll test
; 	db 0,255,0,224,96,0,97,0,162,176,208,16,112,16,208,16,112,16,208,16,112,16,208,16,112,16,208,16,112,16,208,16,112,16,208,16,112,16,208,16,96,0,113,16,208,16,112,16,208,16,112,16,208,16,112,16,208,16,112,16,208,16,112,16,208,16,112,16,208,16,112,16,208,16,96,0,113,16,208,16,112,16,208,16,112,16,208,16,112,16,208,16,112,16,208,16,112,16,208,16,112,16,208,16,112,16,208,16,96,0,113,16,208,16,112,16,208,16,112,16,208,16,112,16,208,16,112,16,208,16,112,16,208,16,112,16,208,16,112,16,208,16,255,10,0,212,255,10,0,196,255,10,0,196,255,10,0,212,255,10,0,252,255,10,0,251,255,10,0,251,255,10,0,252,18,174,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255

; 	ds CHIP_ROM_SIZE - (@ - CHIP_ROM), 0
; CHIP_ROM_END:

; CHIP_ROM: ; 16x16 lores test
; 	db 0,255,34,20,34,26,34,26,34,32,34,26,96,1,240,21,34,66,18,6,99,0,100,0,0,238,162,74,211,64,0,238,96,8,224,158,18,40,116,1,96,5,224,158,18,48,116,255,96,7,224,158,18,56,115,255,96,9,224,158,18,64,115,1,0,238,240,7,48,0,18,66,0,238,227,199,128,1,153,193,9,20,9,200,9,84,157,193,128,1,128,1,131,57,41,32,17,56,41,40,131,185,128,1,227,199,63,127,127,127,240,99,224,99,192,99,192,99,202,99,192,127,209,127,206,99,192,99,192,99,224,99,240,99,127,127,63,127

; 	ds CHIP_ROM_SIZE - (@ - CHIP_ROM), 0
; CHIP_ROM_END:

; CHIP_ROM: ; 16x16 hires test
; 	db 0,255,34,20,34,26,34,26,34,32,34,26,96,1,240,21,34,66,18,6,99,0,100,0,0,238,162,74,211,64,0,238,96,8,224,158,18,40,116,1,96,5,224,158,18,48,116,255,96,7,224,158,18,56,115,255,96,9,224,158,18,64,115,1,0,238,240,7,48,0,18,66,0,238,227,199,128,1,153,193,9,20,9,200,9,84,157,193,128,1,128,1,131,57,41,32,17,56,41,40,131,185,128,1,227,199,63,127,127,127,240,99,224,99,192,99,192,99,202,99,192,127,209,127,206,99,192,99,192,99,224,99,240,99,127,127,63,127

; 	ds CHIP_ROM_SIZE - (@ - CHIP_ROM), 0
; CHIP_ROM_END:

; CHIP_ROM: ; HEX-MIX-V2.sc8
; 	db 96,1,110,5,111,7,34,100,49,0,18,40,204,63,205,31,111,60,143,197,79,0,124,252,111,27,143,213,79,0,125,251,207,15,255,41,220,213,18,66,204,127,205,63,111,119,143,197,79,0,124,248,111,53,143,213,79,0,125,246,207,15,255,48,220,218,63,0,18,72,18,4,254,24,129,4,129,2,111,7,34,100,0,224,254,24,65,0,0,254,49,0,0,255,254,24,0,224,18,4,255,21,255,7,63,0,18,102,0,238

; 	ds CHIP_ROM_SIZE - (@ - CHIP_ROM), 0
; CHIP_ROM_END:
; 

; CHIP_ROM: ; sweetcopter
; 	db 18,182,255,7,63,0,18,2,111,2,255,21,0,238,163,89,96,88,97,8,208,16,112,16,48,168,18,20,0,238,163,57,77,3,163,25,104,0,218,176,136,241,162,217,76,1,162,249,96,16,128,183,218,0,136,241,0,238,224,161,0,238,105,0,0,238,96,6,57,0,18,58,224,158,0,238,105,1,96,255,141,3,125,1,0,238,123,255,96,63,128,178,64,0,119,1,0,238,96,1,140,3,34,86,192,1,64,1,34,86,138,212,34,66,0,238,96,56,97,18,163,121,208,16,111,6,239,158,18,126,208,16,0,238,111,16,255,24,255,21,0,252,0,251,0,251,0,252,255,7,63,0,18,140,162,214,247,51,242,101,241,48,96,55,97,30,208,26,242,48,112,9,208,26,111,6,239,158,18,176,0,224,0,255,106,56,107,48,109,3,105,0,103,0,34,14,34,30,34,116,34,30,34,98,34,30,56,0,18,134,34,2,18,200,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,128,2,64,122,94,137,145,240,15,1,128,1,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,128,2,64,14,240,49,12,62,252,1,128,1,128,0,0,7,192,24,48,32,8,35,184,68,68,72,138,72,2,72,2,52,68,35,184,48,8,75,244,72,20,48,24,17,32,15,192,3,224,12,24,16,4,29,196,34,34,81,18,64,18,64,18,34,44,29,196,16,12,47,146,40,18,24,12,4,136,3,240,255,255,0,0,195,195,126,126,60,60,60,60,60,60,126,126,255,255,0,0,255,255,0,0,0,0,0,0,0,0,0,0,63,252,127,254,255,255,240,15,240,15,243,255,240,127,240,127,243,255,240,15,240,15,255,255,127,254,63,252,0,192,0,128
; 	ds CHIP_ROM_SIZE - (@ - CHIP_ROM), 0
; CHIP_ROM_END:

; CHIP_ROM: ; corax opcode test
;  	db 18,10,96,1,0,238,96,2,18,166,0,224,104,50,107,26,164,241,216,180,104,58,164,245,216,180,104,2,105,6,106,11,107,1,101,42,102,43,164,181,216,180,164,237,217,180,164,165,54,43,164,161,218,180,107,6,164,185,216,180,164,237,217,180,164,161,69,42,164,165,218,180,107,11,164,189,216,180,164,237,217,180,164,161,85,96,164,165,218,180,107,16,164,197,216,180,164,237,217,180,164,161,118,255,70,42,164,165,218,180,123,5,164,205,216,180,164,237,217,180,164,161,149,96,164,165,218,180,123,5,164,173,216,180,164,237,217,180,164,165,18,144,164,161,218,180,104,18,105,22,106,27,107,1,164,177,216,180,164,237,217,180,96,0,34,2,164,165,64,0,164,161,218,180,123,5,164,169,216,180,164,225,217,180,164,165,64,2,164,161,48,0,218,180,123,5,164,201,216,180,164,169,217,180,164,161,101,42,103,0,135,80,71,42,164,165,218,180,123,5,164,201,216,180,164,173,217,180,164,161,102,11,103,42,135,97,71,43,164,165,218,180,123,5,164,201,216,180,164,177,217,180,164,161,102,120,103,31,135,98,71,24,164,165,218,180,123,5,164,201,216,180,164,181,217,180,164,161,102,120,103,31,135,99,71,103,164,165,218,180,104,34,105,38,106,43,107,1,164,201,216,180,164,185,217,180,164,161,102,140,103,140,135,100,71,24,164,165,218,180,123,5,164,201,216,180,164,189,217,180,164,161,102,140,103,120,135,101,71,236,164,165,218,180,123,5,164,201,216,180,164,197,217,180,164,161,102,120,103,140,135,103,71,236,164,165,218,180,123,5,164,201,216,180,164,193,217,180,164,161,102,15,134,102,70,7,164,165,218,180,123,5,164,201,216,180,164,225,217,180,164,161,102,224,134,110,70,192,164,165,218,180,123,5,164,229,216,180,164,193,217,180,164,158,241,101,164,165,48,170,164,161,49,85,164,161,218,180,104,50,105,54,106,59,107,1,164,229,216,180,164,189,217,180,164,158,96,0,97,48,241,85,164,158,240,101,129,0,164,159,240,101,164,165,48,48,164,161,49,0,164,161,218,180,123,5,164,229,216,180,164,181,217,180,164,158,102,137,246,51,242,101,164,161,48,1,20,50,49,3,20,50,50,7,20,50,164,158,102,65,246,51,242,101,164,161,48,0,20,50,49,6,20,50,50,5,20,50,164,158,102,4,246,51,242,101,164,161,48,0,20,50,49,0,20,50,50,4,20,50,164,165,218,180,123,5,164,229,216,180,164,225,217,180,164,161,102,4,246,30,218,180,123,5,164,233,216,180,164,237,217,180,164,165,102,255,118,10,54,9,164,161,134,102,54,4,164,161,102,255,96,10,134,4,54,9,164,161,134,102,54,4,164,161,102,255,134,110,134,102,54,127,164,161,134,102,134,110,54,126,164,161,102,5,118,246,54,251,164,161,102,5,134,5,54,251,164,161,102,5,128,103,48,251,164,161,218,180,20,156,170,85,0,0,160,64,160,0,160,192,128,224,160,160,224,192,64,64,224,224,32,192,224,224,96,32,224,160,224,32,32,224,192,32,192,96,128,224,224,224,32,64,64,224,224,160,224,224,224,32,192,64,160,224,160,192,224,160,224,224,128,128,224,192,160,160,192,224,192,128,224,224,128,192,128,0,160,160,64,160,64,160,160,10,174,162,66,56,8,48,184

; 	ds CHIP_ROM_SIZE - (@ - CHIP_ROM), 0
; CHIP_ROM_END:

; CHIP_ROM: ; schip scrolling test
; 		db 19,12,96,0,224,161,18,4,112,1,64,16,0,238,18,4,101,0,162,34,241,85,162,130,241,85,18,34,67,1,208,18,34,2,0,0,245,30,245,30,245,30,245,30,241,101,99,0,243,21,244,7,52,0,18,68,165,231,208,18,100,10,244,21,100,1,131,67,100,14,228,158,18,82,69,0,18,82,117,255,18,28,100,15,228,158,18,96,149,32,18,96,117,1,18,28,134,80,100,10,228,161,18,128,100,0,114,1,116,1,228,158,18,120,134,64,118,255,18,128,84,32,18,108,114,255,18,50,34,2,0,0,246,30,246,30,246,30,246,30,100,2,244,30,241,101,100,16,128,65,162,154,241,85,0,0,252,101,35,2,65,0,0,238,128,16,35,2,66,0,0,238,128,32,35,2,67,0,0,238,128,48,35,2,68,0,0,238,128,64,35,2,69,0,0,238,128,80,35,2,70,0,0,238,128,96,35,2,71,0,0,238,128,112,35,2,72,0,0,238,128,128,35,2,73,0,0,238,128,144,35,2,74,0,0,238,128,160,35,2,75,0,0,238,128,176,35,2,76,0,0,238,128,192,35,2,0,238,165,235,240,30,221,228,125,4,0,238,161,255,240,101,64,1,19,202,64,2,19,250,64,3,20,134,64,4,21,2,64,5,21,106,0,254,0,224,109,6,110,2,166,147,34,156,109,12,110,12,166,161,34,156,109,12,110,17,166,173,34,156,35,150,96,166,97,139,98,1,18,16,0,254,0,224,109,7,110,2,166,191,34,156,109,15,110,12,166,205,34,156,109,15,110,17,166,215,34,156,35,150,96,166,97,183,98,1,18,16,0,254,0,224,109,10,110,2,166,234,34,156,109,18,110,12,166,246,34,156,109,18,110,17,166,255,34,156,35,150,96,166,97,226,98,1,18,16,106,50,107,27,167,42,218,180,106,58,167,46,218,180,0,238,167,41,96,0,240,85,19,74,167,41,96,1,240,85,19,74,167,41,240,101,64,0,19,112,21,2,167,41,240,101,64,0,20,134,21,106,0,224,167,8,96,46,97,11,208,24,0,252,0,252,0,252,0,252,0,252,0,252,167,16,96,10,97,11,208,24,0,251,0,251,0,251,167,32,96,28,97,0,208,24,0,198,20,58,0,224,167,8,96,46,97,11,208,24,0,252,0,252,0,252,0,252,0,252,0,252,0,252,0,252,0,252,0,252,0,252,0,252,167,16,96,10,97,11,208,24,0,251,0,251,0,251,0,251,0,251,0,251,167,32,96,28,97,0,208,24,0,204,167,40,96,21,97,33,98,27,99,26,100,15,101,4,208,49,209,49,208,65,209,65,210,81,112,1,113,1,114,1,48,31,20,72,96,20,97,31,98,32,99,43,100,26,101,37,102,16,103,5,208,97,209,97,210,97,211,97,212,113,213,113,118,1,119,1,54,26,20,108,240,10,34,2,19,12,0,224,0,255,167,8,96,78,97,22,208,24,0,252,0,252,0,252,0,252,0,252,0,252,167,16,96,42,97,22,208,24,0,251,0,251,0,251,167,32,96,60,97,11,208,24,0,204,167,40,96,53,97,65,98,59,99,43,100,32,101,21,208,49,209,49,208,65,209,65,210,81,112,1,113,1,114,1,48,63,20,196,96,52,97,63,98,64,99,75,100,58,101,69,102,33,103,22,208,97,209,97,210,97,211,97,212,113,213,113,118,1,119,1,55,32,20,232,240,10,34,2,19,12,0,224,167,8,96,45,97,23,208,24,0,252,0,252,0,252,0,252,0,252,0,252,167,16,96,10,97,12,208,24,0,251,0,251,0,251,167,24,96,22,97,23,208,24,0,220,167,32,96,33,97,0,208,24,0,198,167,40,96,21,97,26,98,15,99,4,208,17,208,33,208,49,112,1,48,42,21,68,96,20,97,31,98,42,99,5,208,49,209,49,210,49,115,1,51,26,21,88,240,10,34,2,19,12,0,224,0,255,167,8,96,77,97,46,208,24,0,252,0,252,0,252,0,252,0,252,0,252,167,16,96,42,97,35,208,24,0,251,0,251,0,251,167,24,96,54,97,46,208,24,0,220,0,220,167,32,96,65,97,11,208,24,0,204,167,40,96,53,97,21,98,32,99,43,208,17,208,33,208,49,112,1,48,74,21,176,96,52,97,63,98,74,99,22,208,49,209,49,210,49,115,1,51,43,21,196,240,10,34,2,19,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,192,192,160,192,128,160,64,160,224,160,160,224,192,64,64,224,224,32,192,224,224,96,32,224,160,224,32,32,224,192,32,192,224,128,224,224,224,32,32,32,224,224,160,224,224,224,32,224,64,160,224,160,192,224,160,224,224,128,128,224,192,160,160,192,224,192,128,224,224,128,192,128,96,128,160,96,160,224,160,160,224,64,64,224,96,32,32,192,160,192,160,160,128,128,128,224,224,224,160,160,192,160,160,160,224,160,160,224,192,160,192,128,64,160,224,96,192,160,192,160,96,192,32,192,224,64,64,64,160,160,160,96,160,160,160,64,160,160,224,224,160,64,160,160,160,160,64,64,224,96,128,224,0,0,0,0,0,224,0,0,0,0,0,64,8,13,3,166,8,18,3,174,104,76,52,84,148,104,88,44,120,64,100,112,92,0,8,148,116,124,104,60,112,52,72,76,104,0,12,148,136,100,152,52,72,76,104,0,11,13,3,182,11,18,3,192,104,76,52,84,148,116,52,112,148,116,76,144,60,0,8,148,88,100,132,148,112,60,116,0,12,148,72,76,68,72,148,112,60,116,0,14,13,3,202,14,18,3,250,104,76,52,84,148,120,44,112,68,60,120,0,8,148,92,100,56,60,112,96,0,12,148,88,60,68,44,52,140,0,255,231,207,129,129,207,231,255,255,231,243,129,129,243,231,255,255,231,195,129,165,231,231,255,255,231,231,165,129,195,231,255,128,0,10,174,162,66,56,8,48,184

; 		ds CHIP_ROM_SIZE - (@ - CHIP_ROM), 0
; CHIP_ROM_END:

; CHIP_ROM: ; quirks test
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

; 	ds CHIP_ROM_SIZE - (@ - CHIP_ROM), 0
; CHIP_ROM_END:


; CHIP_ROM: ; random number
; 	db 101,0,0,224,195,255,162,34,243,51,242,101,100,0,240,41,212,85,116,5,241,41,212,85,116,5,242,41,212,85,243,10,18,2

; 	ds CHIP_ROM_SIZE - (@ - CHIP_ROM), 0
; CHIP_ROM_END:

CHIP_ROM:
	; keypad
	db 19, 12, 96, 0, 224, 161, 18, 4, 112, 1, 64, 16, 0, 238, 18, 4, 101, 0, 162,
	db 34, 241, 85, 162, 130, 241, 85, 18, 34, 67, 1, 208, 18, 34, 2, 0, 0, 245,
	db 30, 245, 30, 245, 30, 245, 30, 241, 101, 99, 0, 243, 21, 244, 7, 52, 0, 18,
	db 68, 164, 35, 208, 18, 100, 10, 244, 21, 100, 1, 131, 67, 100, 14, 228, 158,
	db 18, 82, 69, 0, 18, 82, 117, 255, 18, 28, 100, 15, 228, 158, 18, 96, 149, 32,
	db 18, 96, 117, 1, 18, 28, 134, 80, 100, 10, 228, 161, 18, 128, 100, 0, 114, 1,
	db 116, 1, 228, 158, 18, 120, 134, 64, 118, 255, 18, 128, 84, 32, 18, 108, 114,
	db 255, 18, 50, 34, 2, 0, 0, 246, 30, 246, 30, 246, 30, 246, 30, 100, 2, 244,
	db 30, 241, 101, 100, 16, 128, 65, 162, 154, 241, 85, 0, 0, 252, 101, 35, 2,
	db 65, 0, 0, 238, 128, 16, 35, 2, 66, 0, 0, 238, 128, 32, 35, 2, 67, 0, 0, 238,
	db 128, 48, 35, 2, 68, 0, 0, 238, 128, 64, 35, 2, 69, 0, 0, 238, 128, 80, 35,
	db 2, 70, 0, 0, 238, 128, 96, 35, 2, 71, 0, 0, 238, 128, 112, 35, 2, 72, 0, 0,
	db 238, 128, 128, 35, 2, 73, 0, 0, 238, 128, 144, 35, 2, 74, 0, 0, 238, 128,
	db 160, 35, 2, 75, 0, 0, 238, 128, 176, 35, 2, 76, 0, 0, 238, 128, 192, 35, 2,
	db 0, 238, 164, 39, 240, 30, 221, 228, 125, 4, 0, 238, 0, 224, 161, 255, 240,
	db 101, 64, 1, 19, 84, 64, 2, 19, 88, 64, 3, 19, 190, 109, 10, 110, 2, 164,
	db 211, 34, 156, 109, 8, 110, 10, 164, 223, 34, 156, 109, 8, 110, 15, 164, 235,
	db 34, 156, 109, 8, 110, 20, 164, 245, 34, 156, 106, 50, 107, 27, 165, 137,
	db 218, 180, 106, 58, 165, 141, 218, 180, 96, 164, 97, 199, 98, 2, 18, 16, 97,
	db 158, 19, 90, 97, 161, 96, 238, 163, 158, 241, 85, 0, 224, 165, 51, 255, 101,
	db 164, 18, 255, 85, 109, 18, 110, 3, 165, 67, 34, 156, 109, 18, 110, 10, 165,
	db 75, 34, 156, 109, 18, 110, 17, 165, 83, 34, 156, 109, 18, 110, 24, 165, 91,
	db 34, 156, 110, 0, 35, 150, 126, 1, 78, 16, 110, 0, 19, 140, 164, 18, 254, 30,
	db 240, 101, 98, 1, 238, 161, 98, 0, 144, 32, 19, 188, 128, 224, 128, 14, 165,
	db 99, 240, 30, 241, 101, 165, 131, 208, 22, 164, 18, 254, 30, 128, 32, 240,
	db 85, 0, 238, 0, 224, 109, 6, 110, 13, 165, 3, 34, 156, 96, 3, 240, 21, 240,
	db 10, 241, 7, 49, 0, 19, 242, 224, 161, 19, 248, 0, 224, 164, 37, 96, 30, 97,
	db 9, 208, 19, 109, 16, 110, 17, 165, 17, 34, 156, 34, 2, 240, 10, 34, 2, 19,
	db 12, 109, 10, 165, 26, 19, 252, 109, 8, 165, 38, 0, 224, 110, 17, 34, 156,
	db 164, 40, 96, 30, 97, 9, 208, 19, 34, 2, 240, 10, 34, 2, 19, 12, 0, 0, 0, 0,
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 192, 192, 160, 192, 128, 160, 64,
	db 160, 224, 160, 160, 224, 192, 64, 64, 224, 224, 32, 192, 224, 224, 96, 32,
	db 224, 160, 224, 32, 32, 224, 192, 32, 192, 224, 128, 224, 224, 224, 32, 32,
	db 32, 224, 224, 160, 224, 224, 224, 32, 224, 64, 160, 224, 160, 192, 224, 160,
	db 224, 224, 128, 128, 224, 192, 160, 160, 192, 224, 192, 128, 224, 224, 128,
	db 192, 128, 96, 128, 160, 96, 160, 224, 160, 160, 224, 64, 64, 224, 96, 32,
	db 32, 192, 160, 192, 160, 160, 128, 128, 128, 224, 224, 224, 160, 160, 192,
	db 160, 160, 160, 224, 160, 160, 224, 192, 160, 192, 128, 64, 160, 224, 96,
	db 192, 160, 192, 160, 96, 192, 32, 192, 224, 64, 64, 64, 160, 160, 160, 96,
	db 160, 160, 160, 64, 160, 160, 224, 224, 160, 64, 160, 160, 160, 160, 64, 64,
	db 224, 96, 128, 224, 0, 0, 0, 0, 0, 224, 0, 0, 0, 0, 0, 64, 4, 11, 3, 84, 4,
	db 16, 3, 88, 4, 21, 3, 190, 104, 76, 52, 84, 148, 100, 104, 52, 100, 56, 60,
	db 0, 8, 148, 60, 136, 40, 60, 148, 56, 100, 132, 96, 0, 12, 148, 60, 136, 44,
	db 8, 148, 124, 104, 0, 16, 148, 64, 136, 4, 44, 148, 68, 60, 120, 84, 60, 140,
	db 0, 104, 112, 60, 116, 116, 148, 44, 96, 140, 148, 84, 60, 140, 0, 44, 88,
	db 88, 148, 68, 100, 100, 56, 0, 96, 100, 120, 148, 72, 44, 88, 120, 76, 96,
	db 68, 0, 96, 100, 120, 148, 112, 60, 88, 60, 44, 116, 60, 56, 0, 0, 0, 0, 0,
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 148, 12, 148, 16, 148, 52, 0, 20,
	db 148, 24, 148, 28, 148, 56, 0, 32, 148, 36, 148, 40, 148, 60, 0, 44, 148, 4,
	db 148, 48, 148, 64, 0, 24, 23, 16, 2, 24, 2, 32, 2, 16, 9, 24, 9, 32, 9, 16,
	db 16, 24, 16, 32, 16, 16, 23, 32, 23, 40, 2, 40, 9, 40, 16, 40, 23, 254, 254,
	db 254, 254, 254, 254, 10, 174, 162, 66, 56, 8, 48, 184
	
	ds CHIP_ROM_SIZE - (@ - CHIP_ROM), 0
CHIP_ROM_END:

; SECTION "Chip8 ROM", ROMX
; CHIP_ROM:
; 	; ; brix
; 	; db 110, 5, 101, 0, 107, 6, 106, 0, 163, 12, 218, 177, 122, 4, 58, 64, 18, 8,
; 	; db 123, 2, 59, 18, 18, 6, 108, 32, 109, 31, 163, 16, 220, 209, 34, 246, 96, 0,
; 	; db 97, 0, 163, 18, 208, 17, 112, 8, 163, 14, 208, 17, 96, 64, 240, 21, 240, 7,
; 	; db 48, 0, 18, 52, 198, 15, 103, 30, 104, 1, 105, 255, 163, 14, 214, 113, 163,
; 	; db 16, 220, 209, 96, 4, 224, 161, 124, 254, 96, 6, 224, 161, 124, 2, 96, 63,
; 	; db 140, 2, 220, 209, 163, 14, 214, 113, 134, 132, 135, 148, 96, 63, 134, 2, 97,
; 	; db 31, 135, 18, 71, 31, 18, 172, 70, 0, 104, 1, 70, 63, 104, 255, 71, 0, 105,
; 	; db 1, 214, 113, 63, 1, 18, 170, 71, 31, 18, 170, 96, 5, 128, 117, 63, 0, 18,
; 	; db 170, 96, 1, 240, 24, 128, 96, 97, 252, 128, 18, 163, 12, 208, 113, 96, 254,
; 	; db 137, 3, 34, 246, 117, 1, 34, 246, 69, 96, 18, 222, 18, 70, 105, 255, 128,
; 	; db 96, 128, 197, 63, 1, 18, 202, 97, 2, 128, 21, 63, 1, 18, 224, 128, 21, 63,
; 	; db 1, 18, 238, 128, 21, 63, 1, 18, 232, 96, 32, 240, 24, 163, 14, 126, 255,
; 	; db 128, 224, 128, 4, 97, 0, 208, 17, 62, 0, 18, 48, 18, 222, 120, 255, 72, 254,
; 	; db 104, 255, 18, 238, 120, 1, 72, 2, 104, 1, 96, 4, 240, 24, 105, 255, 18, 112,
; 	; db 163, 20, 245, 51, 242, 101, 241, 41, 99, 55, 100, 0, 211, 69, 115, 5, 242,
; 	; db 41, 211, 69, 0, 238, 224, 0, 128, 0, 252, 0, 170, 0, 0, 0, 0, 0

; 	; oob test
; 	db 35,92,106,0,107,0,34,42,34,86,34,120,35,160,110,2,54,0,36,8,110,9,55,0,36,8,110,16,56,0,36,8,110,23,34,154,57,0,36,8,18,40,241,10,130,16,114,176,111,1,255,21,225,161,106,1,226,161,107,1,255,7,79,0,18,48,58,0,18,80,59,0,18,80,106,0,107,0,18,48,138,178,134,160,0,238,96,10,240,41,97,1,98,26,209,37,110,65,111,58,222,245,138,240,164,136,209,37,139,240,74,0,18,118,75,0,103,1,0,238,96,10,240,41,97,16,98,1,209,37,96,186,240,41,209,37,138,240,164,136,209,37,139,240,74,0,18,152,75,0,104,1,0,238,106,0,107,0,108,0,111,0,96,1,175,255,240,30,63,0,0,238,96,234,175,255,240,51,242,101,109,32,240,41,221,229,125,5,241,41,221,229,125,5,242,41,221,229,96,255,129,0,130,16,175,255,242,85,160,0,240,101,48,255,124,1,160,1,240,101,48,255,124,1,98,1,160,0,240,101,129,0,175,255,242,30,240,101,80,16,124,1,98,2,160,1,240,101,129,0,175,255,242,30,240,101,80,16,124,1,96,1,143,192,143,2,79,1,0,238,106,1,175,255,96,1,97,56,98,30,209,34,209,33,138,242,240,30,114,1,209,33,139,240,76,0,19,46,107,1,209,33,96,0,175,255,240,85,60,4,19,86,164,136,209,33,96,1,128,243,139,0,75,0,19,86,209,33,175,255,240,101,48,0,19,86,164,18,98,29,209,35,138,178,137,160,0,238,164,71,96,5,109,12,110,10,221,229,125,8,240,30,221,229,125,8,240,30,221,229,125,6,240,30,221,229,125,6,240,30,221,229,109,13,126,7,240,30,221,229,125,8,240,30,221,229,125,6,240,30,221,229,125,4,240,30,221,229,125,9,240,30,221,229,0,238,0,224,164,21,96,5,109,13,110,2,221,229,125,8,240,30,221,229,125,9,240,30,221,229,36,0,164,56,109,13,110,9,221,229,125,8,240,30,221,229,125,5,240,30,221,229,36,0,164,46,109,13,110,16,221,229,125,9,240,30,221,229,125,251,111,238,255,41,221,229,36,0,164,121,109,13,110,23,221,229,125,6,240,30,221,229,125,4,240,30,221,229,36,0,0,238,109,7,164,36,221,229,0,238,36,0,111,5,255,30,221,229,0,238,171,171,74,64,20,218,82,82,96,169,233,137,134,64,224,64,64,96,0,160,64,160,0,0,160,192,128,0,160,160,224,160,160,160,160,64,160,160,32,42,236,168,232,96,16,240,144,96,136,168,168,112,80,96,170,236,136,136,70,168,228,130,108,96,128,64,32,192,96,16,240,144,96,5,165,215,145,150,132,170,206,168,166,160,160,224,32,192,64,224,64,64,96,137,168,171,113,81,4,106,142,136,102,0,80,168,168,168,64,160,224,128,96,0,82,168,170,168,255,255,255,255,255
; CHIP_ROM_END: