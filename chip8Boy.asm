INCLUDE "hardware.inc"
INCLUDE "utils.asm"
INCLUDE "config.asm"

; TO COMPILE RUN:
; rgbasm -o chip8boy.o chip8boy.asm ; rgblink -o chip8boy.gbc chip8boy.o ; rgbfix -c -v -p 0xFF chip8boy.gbc

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
ROM_NUM:
	ds 1
ROM_CONFIG_PTR:
	ds 2
KEY_STATE:
	ds 1
PRESSED_KEYS:
	ds 1
RELEASED_KEYS:
	ds 1
IPF_PER_BLOCK:
	ds 1
IPF_BLOCKS_NUM:
	ds 1
IPF_DISPLAY:
	ds 2
INSTR_COUNTER:
	ds 1 
INSTR_BLOCK_COUNTER:
	ds 1
QUIRKS:
	ds 1
KEY_MAP:
	ds 16
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
SwitchROM:
	ldh [ROM_NUM], a
	; update config ptr:
	ld hl, ROM_CONFIG_START
	and a
	jr z, .end
	ld b, a
.calculatePtr
	ld de, CHIP_ROM_SIZE - ROM_CONFIG_START
	add hl, de
	ld a, [hl+]
	ld e, a
	ld a, [hl+]
	ld d, a
	add hl, de
	dec b
	jr nz, .calculatePtr
.end:
	ld a, l
	ldh [ROM_CONFIG_PTR], a
	ld a, h
	ldh [ROM_CONFIG_PTR + 1], a
	jr ResetChip8
SwitchToNextROM:
	call Resume
	ld a, [NUM_OF_ROMS]
	ld l, a
	ldh a, [ROM_NUM]
	inc a
	cp l
	jr nz, SwitchROM
	xor a ; go back to rom 0
	jr SwitchROM
SwitchToPrevROM:
	call Resume
	ldh a, [ROM_NUM]
	sub 1
	jr nc, SwitchROM
	ld a, [NUM_OF_ROMS]
	dec a ; go to last rom
	jr SwitchROM
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
	bit B_PAD_START, a
	jr nz, Resume
	bit B_PAD_SELECT, a
	jr nz, Resume
	bit B_PAD_RIGHT, a
	jr nz, SwitchToNextROM
	bit B_PAD_LEFT, a
	jr nz, SwitchToPrevROM
	jr .continueWait
Resume:
	ld hl, ($98 << 8) | PAUSE_ICON_TILEMAP_NUM
	ld a, BG_TILE_TILEMAP_NUM
	ld [hl+], a
	ld [hl], a
	ret
CheckFX0A:
	ldh a, [RELEASED_KEYS]
	and ~(PAD_START | PAD_SELECT) ; ignore start and select
	ret z
	push bc

	MACRO CHECK_KEY ; \1: key, \2: label to jump on fail
		bit \1, a
		jr z, \2
		ld b, (1 << \1)
		jr .end
	ENDM

	CHECK_KEY B_PAD_A, .checkB
.checkB
	CHECK_KEY B_PAD_B, .checkDpadUp
.checkDpadUp:
	CHECK_KEY B_PAD_UP, .checkDpadDown
.checkDpadDown:
	CHECK_KEY B_PAD_DOWN, .checkDpadRight
.checkDpadRight:
	CHECK_KEY B_PAD_RIGHT, .hitDpadLeft
.hitDpadLeft:	
	ld b, PAD_LEFT
.end:
	; find chip8 key number from KEY_MAP by gameboy key
	ld hl, KEY_MAP + 15
	ld c, 16
.searchKey:
	ld a, [hl-]
	cp b
	jr z, .foundKey
	dec c
	jr nz, .searchKey
	pop bc
	ret
.foundKey:
	dec c
	ld b, c
	; load it to V register (FX0A_KEY_REG)
	ldh a, [FX0A_KEY_REG]
	add LOW(V0)
	ld c, a
	ld a, b
	ldh [c], a

	ld a, FX0A_DONE_FLAG
	ldh [FX0A_KEY_REG], a
	pop bc
	ret
SetupJumpTables:
	MEMCPY MAIN_JUMP_TABLE, MAIN_JUMP_TABLE_ROM, MAIN_JUMP_TABLE_ROM_END
	ldh a, [IS_GBC]
	and a
	ret nz
	; rewrite DXYN jump table entry to point to the DMG version, because GBC is default.
	ld hl, (HIGH(MAIN_JUMP_TABLE) << 8) | ($D * 2)
	ld a, LOW(DXYN_LOW_RES_DMG)
	ld [hl+], a
	ld [hl], HIGH(DXYN_LOW_RES_DMG)
	ret
LoadChipROM:
	ldh a, [ROM_CONFIG_PTR]
	ld l, a
	ldh a, [ROM_CONFIG_PTR + 1]
	ld h, a
	ld bc, (16 << 8) | LOW(KEY_MAP)
.copyKeyMap:
	ld a, [hl+]
	ldh [c], a
	inc c
	dec b
	jr nz, .copyKeyMap

	ldh a, [IS_GBC]
	and a
	jr nz, .gbc
	ld a, [hl+]
	ldh [IPF_PER_BLOCK], a
	ld a, [hl+]
	ldh [IPF_BLOCKS_NUM], a
	; skip gbc section
	inc hl
	inc hl
	jr .after
.gbc:
	; skip dmg section
	inc hl
	inc hl
	ld a, [hl+]
	ldh [IPF_PER_BLOCK], a
	ld a, [hl+]
	ldh [IPF_BLOCKS_NUM], a
.after:
	ld a, [hl+]
	ldh [QUIRKS], a

	; loading rom size to bc
	ld a, [hl+]
	ld c, a
	ld a, [hl+]
	ld b, a

	; setting source start ptr (de)
	ld d, h
	ld e, l

	; setting source end ptr (bc)
	add hl, bc
	ld b, h
	ld c, l
	
	; setting dest ptr and tail-calling memcpy
	ld hl, CHIP_RAM + $200 
	jp Memcpy

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
	and (PAD_START | PAD_SELECT)
	cp (PAD_START | PAD_SELECT) ; check if start + select are pressed
	jr nz, .notPaused
	call PauseChip8
	jr .end
.notPaused:
	bit B_PAD_START, a
	jr z, .checkSelect
	call IncreaseIPF
	jr .end
.checkSelect:
	bit B_PAD_SELECT, a
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
	ld a, IE_VBLANK
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

	MEMSET SCREEN_BUF, 0, SCREEN_BUF_SIZE

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
	; id 3 = light gray, 2 = dark gray, 1 = white, 0 = black
	ld a, %01100011 
	ldh [rBGP], a
.after:
	MEMCPY $9800, TILE_MAP, TILE_MAP_END
	MEMCPY_1BIT_TILES VRAM_TILES, TILES, TILES_END

	xor a
	ldh [KEY_STATE], a
	ldh [PRESSED_KEYS], a
	ldh [RELEASED_KEYS], a

	ldh [ROM_NUM], a
	ld a, LOW(ROM_CONFIG_START)
	ldh [ROM_CONFIG_PTR], a
	ld a, HIGH(ROM_CONFIG_START)
	ldh [ROM_CONFIG_PTR + 1], a

	; xor first byte of RNG state with DIV so it's not all zeroes on emulators which don't emulate random WRAM values.
	ldh a, [rDIV]
	ld b, a
	ld a, [RNG_STATE]
	xor b
	ld [RNG_STATE], a

InitChip8:
	MEMSET CHIP_RAM, 0, CHIP_RAM_SIZE
	MEMCPY CHIP_RAM, CHIP_FONTS, CHIP_FONTS_END
	MEMSET CHIP_RAM_DEAD_BUF, 0, CHIP_RAM_DEADBUF_SIZE
	MEMSET CHIP_STACK, 0, CHIP_STACK_SIZE
	MEMSET CHIP_STATE, 0, CHIP_STATE_END - CHIP_STATE
	MEMSET SCREEN_BUF, 0, SCREEN_BUF_SIZE
	MEMSET VRAM_SCREEN_BUF, 0, SCREEN_BUF_SIZE

	call LoadChipROM
	call SetupJumpTables

	; reset chip8 PC
	ld bc, CHIP_RAM + $200
	push bc

	ld a, 60
	ldh [FRAME_COUNTER], a

	ld a, -1
	ldh [FX0A_KEY_REG], a

	; setting IPF variables
	ldh a, [IPF_PER_BLOCK]
	ldh [INSTR_COUNTER], a
	ld b, a
	ldh a, [IPF_BLOCKS_NUM]
	ldh [INSTR_BLOCK_COUNTER], a
	ld c, a

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
	ld a, LCDC_ON | LCDC_BG | LCDC_BLOCK01
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
	ldh a, [IPF_BLOCKS_NUM]
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
	cp $D0
	jp z, OP_00DN
	cp $B0
	jp z, OP_00BN
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
		; change palette for color id 1 (set chip8 pixel) to be black so screen is seen as clear immediately,
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
	ldh a, [QUIRKS]
	bit LEGACY_SCHIP_FLAG, a
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

	; scroll and clear half of right/left column
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
		swap a
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
	SCROLL_HORIZONTAL 1, 1

OP_00FC: ; scroll left 4 pixels
	ldh a, [IS_GBC]
	and a
	jp nz, .gbc
	SCROLL_HORIZONTAL 0, 0
.gbc:
	SCROLL_HORIZONTAL 0, 1

; \1 = 0 - down, otherwise up; \2 - 1 if running on GBC, 0 if not.
MACRO SCROLL_VERTICAL
	LD_N()
	jp z, .scroll0\@
	ld b, a
	; scroll twice as much in modern schip lores mode
	ldh a, [HIGH_RES_MODE_FLAG]
	and a
	jr nz, .skipAdd\@
	ldh a, [QUIRKS]
	bit LEGACY_SCHIP_FLAG, a
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

	IF \2 == 0
		IF \1 == 0
			ld hl, VRAM_SCREEN_BUF + (128 * 16) - 2
		ELSE
			ld hl, VRAM_SCREEN_BUF
		ENDC 
	ELSE
		ld a, 1
		ldh [DRAW_FLAG], a 

		IF \1 == 0
			ld hl, SCREEN_BUF + (128 * 16) - 2
		ELSE
			ld hl, SCREEN_BUF
		ENDC
	ENDC
	; de = hl +- (a * 2)
	ld a, b
	ldh [temp2], a ; saving N in temp2
	add a  
	ld b, a ; saving (a * 2) in b
	IF \1 == 0
		ld a, l
		sub b
		ld e, a
	ELSE
		ld e, b
	ENDC
	ld d, h

	REPT 16
	.moveTile\@:
		IF \2 == 0
			WAIT_VRAM_ACCESS()
		ENDC

		ld a, [de]

		IF \1 == 0
			ld [hl-], a
			dec l
			dec de
			dec e
		ELSE
			ld [hl+], a
			inc l
			inc e
			inc de
		ENDC

		dec c
		jr nz, .moveTile\@

		; src ptr (de) now points to the end of the new column, so set hl to it.
		ld h, d
		ld l, e

		; subtracting/adding bytes per iteration from e to move src ptr to the correct position in the new column.
		ld a, e
		IF \1 == 0
			sub b
		ELSE
			add b
		ENDC
		ld e, a

		; reloading c
		ldh a, [temp1]
		ld c, a
	ENDR

	; clearing top/bottom N rows:

	ldh a, [temp2] ; saved N
	ld b, a
	; storing 128 - (b * 2) in de (number of bytes to the first row of the next column).
	ld a, 128
	sub b
	sub b
	ld d, 0
	ld e, a
	
	IF \2 == 0
		ld hl, VRAM_SCREEN_BUF
	ELSE
		ld hl, SCREEN_BUF
		xor a
	ENDC

	; for scroll up need to clear bottom rows, so point hl to the first row to be cleared.
	IF \1 != 0
		add hl, de
	ENDC

	REPT 16
		ld c, b
	.clearTile\@:
		IF \2 == 0
			WAIT_VRAM_ACCESS()
			xor a
		ENDC

		ld [hl+], a
		IF \1 == 0
			inc l
		ELSE
			inc hl
		ENDC
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
	SCROLL_VERTICAL 0, 0
.gbc:
	SCROLL_VERTICAL 0, 1

; scroll up N pixels
OP_00BN: 
OP_00DN:
	ldh a, [IS_GBC]
	and a
	jp nz, .gbc
	SCROLL_VERTICAL 1, 0
.gbc:
	SCROLL_VERTICAL 1, 1

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
	ldh a, [QUIRKS]
	bit LEGACY_SCHIP_FLAG, a
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
			and a
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
			ldh a, [QUIRKS]
			bit LEGACY_SCHIP_FLAG, a
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
	add LOW(KEY_MAP)
	ld c, a
	ldh a, [c]
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
	add a ; same as left shift
	ld [hl], a

	SET_VF_CARRY(1)
	INSTR_END()

; Matching on 7 low bits of the opcode, so 128 entries fit in a page.
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

SECTION "Data", ROM0
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