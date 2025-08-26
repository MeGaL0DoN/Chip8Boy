INCLUDE "hardware.inc"
INCLUDE "utils.asm"
INCLUDE "config.asm"

; rgbasm -o chip8boy.o chip8boy.asm ; rgblink -t -o chip8boy.gbc chip8boy.o ; rgbfix -c -v -p 0xFF chip8boy.gbc

DEF CHIP_PC EQUS "de"
DEF CHIP_PC_HI EQUS "d"
DEF CHIP_PC_LO EQUS "e"

DEF OP_HI EQUS "b"
DEF NN EQUS "c"

MACRO LD_OP_LOW
	ld a, [CHIP_PC]
ENDM
MACRO LD_PC_HI_OP_HI
	ld a, OP_HI
	and $F
	add HIGH(CHIP_RAM)
	ld CHIP_PC_HI, a
ENDM
MACRO SKIP_NEXT_INSTR
	inc CHIP_PC
	inc CHIP_PC
ENDM

MACRO LD_X
	and $F
ENDM
MACRO LD_Y
	ld a, NN
	and $F0
	swap a
ENDM
MACRO LD_N
	ld a, NN
	and $F
ENDM

MACRO LD_VX_PTR
	LD_X()
	ld h, HIGH(V0)
	ld l, a
ENDM
MACRO LD_VX
	LD_VX_PTR()
	ld a, [hl]
ENDM

; Y index is already in a
MACRO LD_VX_PTR_SAVE_VY
	ld h, HIGH(V0)	
	ld l, a
	ld NN, [hl]

	ld a, OP_HI
	LD_X()
	ld l, a
ENDM
MACRO LD_VX_SAVE_VY
	LD_VX_PTR_SAVE_VY()
	ld a, [hl]
ENDM

; \1 - high register, \2 - low register
MACRO LD_I_REG
	ldh a, [I_REG]
	ld \2, a
	ldh a, [I_REG + 1]
	ld \1, a
ENDM
; \1 - high register of the pointer, \2 - label to jump on OOB
MACRO CHECK_MEM_WRITE_OOB
	ld a, \1
	cp HIGH(CHIP_RAM_END)
	jr z, \2
ENDM

DEF CHIP_RAM_SIZE EQU 4096
DEF CHIP_RAM_DEADBUF_SIZE EQU 32
DEF CHIP_STACK_SIZE EQU 16
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
IGNORE_KEYS:
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
FRAME_DONE_FLAG:
	ds 1
DRAW_FLAG:
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
temp4:
	ds 1
; For use in interrupts:
temp5:
	ds 1
temp6:
	ds 1
temp7:
	ds 1

SECTION "Stack", WRAM0
	ds (CHIP_STACK_SIZE * 2) + (64 * 2) ; 16 entries for chip8 stack + 64 for the program
STACK_TOP:

SECTION "ChipRAM", WRAMX
CHIP_RAM:
	ds CHIP_RAM_SIZE
CHIP_RAM_END:

; These 32 bytes are always 0, and the purpose is to protect OOB memory reads (for example in dxyn) when I register is > $FFF,
; without having to mask it every time. since CHIP_RAM ends at DFFF, E000-FDFF is a mirror of C000 and will read from here.
SECTION "ChipRAMDeadBuf", WRAM0[$C000]
CHIP_RAM_DEAD_BUF:
	ds CHIP_RAM_DEADBUF_SIZE

SECTION "ChipRegs", WRAM0, ALIGN[8]
V0:
	ds 15
VF:
	ds 1	

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
	ldh [temp5], a
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
	ldh a, [temp5]
	swap a
	and $F
	add DIGIT0_TILE_NUM
	ld [hl+], a
	ldh a, [temp5]
	and $F
	add DIGIT0_TILE_NUM
	ld [hl], a
	ret
ENDM
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
	ret
ENDM

SECTION "Functions", ROM0
IncreaseIPF:
	UPDATE_IPF(1)
DecreaseIPF:
	UPDATE_IPF(0)
	
ClearChipScreen:
	ldh a, [IS_GBC]
	and a
	jp nz, .gbc
	CLEAR_SCREEN(0)
.gbc:
	CLEAR_SCREEN(1)

SecondElapsed:
	ld [hl], 60
	ld hl, DROPPED_FRAME_COUNTER
	ld a, $60 ; BCD 60
	sub [hl]
	daa
	ld [hl], 0
	; Updating fps
	ldh [temp5], a
	swap a
	and $F
	add DIGIT0_TILE_NUM
	ld hl, ($98 << 8) | FPS_DIGIT0_TILEMAP_NUM
	ld [hl+], a

	ldh a, [temp5]
	and $F
	add DIGIT0_TILE_NUM
	ld [hl], a
	ret 

ResetChip8:
	call ClearChipScreen
	SAFE_HALT()
	ldh a, [IS_GBC]
	and a
	jr z, .wait
	RUN_GDMA VRAM_SCREEN_BUF, SCREEN_BUF, 2048
	; wait for any to release first
.wait:
	call UpdateKeys
	ldh a, [KEY_STATE]
	and $F
	cp $F
	jr nz, .done
	SAFE_HALT()
	jr .wait
.done:
	; put held keys in ignore list so they are not seen as pressed to the ROM until first release after reset.
	ldh a, [KEY_STATE]
	cpl
	ldh [IGNORE_KEYS], a
	ld sp, STACK_TOP
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
	SAFE_HALT()
	call UpdateKeys
	; remove released keys from ignore list
	ldh a, [IGNORE_KEYS]
	ld hl, RELEASED_KEYS
	or [hl]
	ldh [IGNORE_KEYS], a
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
	ld hl, IGNORE_KEYS
	and [hl]
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
	ld h, HIGH(V0)
	ld l, a
	ld [hl], b

	ld a, FX0A_DONE_FLAG
	ldh [FX0A_KEY_REG], a
	pop bc
	ret

MACRO SETUP_JUMP_TABLES
	MEMCPY MAIN_JUMP_TABLE, MAIN_JUMP_TABLE_ROM, MAIN_JUMP_TABLE_ROM_END
	MEMCPY _8XYN_JUMP_TABLE, _8XYN_JUMP_TABLE_ROM, _8XYN_JUMP_TABLE_ROM_END
	MEMCPY _8XYN_HANDLERS, _8XYN_DEFAULT_HANDLERS, _8XYN_DEFAULT_HANDLERS_END
	MEMCPY FX_JUMP_TABLE, FX_JUMP_TABLE_ROM, FX_JUMP_TABLE_ROM_END
	ldh a, [QUIRKS]
	ld b, a
	bit MEM_INCREMENT_QUIRK, b
	jr nz, .memIncrQuirkOn
	ld hl, (HIGH(FX_JUMP_TABLE) << 8) | ($55 * 2)
	ld a, LOW(FX55_MEM_INCREMENT_OFF)
	ld [hl+], a
	ld [hl], HIGH(FX55_MEM_INCREMENT_OFF)
	ld l, ($65 * 2)
	ld a, LOW(FX65_MEM_INCREMENT_OFF)
	ld [hl+], a
	ld [hl], HIGH(FX65_MEM_INCREMENT_OFF)
.memIncrQuirkOn:
	bit VF_RESET_QUIRK, b
	jr nz, .vfResetQuirkOn
	MEMCPY (_8XYN_HANDLERS + _8XY1 - _8XYN_DEFAULT_HANDLERS), _8XYN_VF_RESET_OFF_HANDLERS, _8XYN_VF_RESET_OFF_HANDLERS_END, 
	ldh a, [QUIRKS]
	ld b, a
.vfResetQuirkOn
	bit SHIFTING_QUIRK, b
	jr z, .shiftingQuirkOff
	MEMCPY (_8XYN_HANDLERS + _8XY6 - _8XYN_DEFAULT_HANDLERS), _8XY6_SHIFTING_ON, _8XY6_SHIFTING_ON_END
	MEMCPY (_8XYN_HANDLERS + _8XYE - _8XYN_DEFAULT_HANDLERS), _8XYE_SHIFTING_ON, _8XYE_SHIFTING_ON_END
	ldh a, [QUIRKS]
	ld b, a
.shiftingQuirkOff:
	ld h, HIGH(MAIN_JUMP_TABLE)
	bit JUMPING_QUIRK, b
	jr z, .jumpQuirkOff
	ld l, ($B * 16)
	ld a, HIGH(OP_BNNN_JUMPING_ON)
	REPT 16
		ld [hl+], a
	ENDR
.jumpQuirkOff:
	ldh a, [IS_GBC]
	and a
	jr nz, .end
	; rewrite DXYN jump table entry to point to the DMG version, because GBC is default.
	ld l, ($D * 16)
	ld a, HIGH(OP_DXYN_LORES_DMG)
	REPT 16
		ld [hl+], a
	ENDR
.end:
ENDM

MACRO LOAD_CHIP_ROM
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

	; loading rom size to bc, and converting it to format memcpy uses (if low % 256 != 0, increment high)
	ld a, [hl+]
	and a
	ld c, a
	ld a, [hl+]
	ld b, a
	jr z, .noInc
    inc b
.noInc:
	; setting dest ptr and calling memcpy
	ld de, CHIP_RAM + $200 
	rst Memcpy
ENDM

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
	RUN_GDMA VRAM_SCREEN_BUF, SCREEN_BUF, 2048
.noFramebufCopy:
	ld hl, FRAME_COUNTER
	dec [hl]
	call z, SecondElapsed
	call UpdateKeys
	ldh a, [KEY_STATE]
	and $F
	cp $F ; check if all keys are held
	jp z, ResetChip8
	ldh a, [FX0A_KEY_REG]
	cp FX0A_NOT_ACTIVE_FLAG
	call nz, CheckFX0A
	; remove released keys from ignore list
	ldh a, [IGNORE_KEYS]
	ld hl, RELEASED_KEYS
	or [hl]
	ldh [IGNORE_KEYS], a
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
	ld sp, STACK_TOP

	; disabling audio circuitry
	xor a
	ldh [rNR52], a

	; enabling vblank interrupts
	ldh [rIF], a
	ld a, IE_VBLANK
	ldh [rIE], a

	; waiting for vblank to disable lcd
	SAFE_HALT() 
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
	; id 3 = light gray, 2 = dark gray, 1 = white, 0 = black
	ld a, %01100011 
	ldh [rBGP], a
.after:
	MEMCPY $9800, TILE_MAP, TILE_MAP_END
	MEMCPY_1BIT_TILES VRAM_TILES, TILES, TILES_END
	MEMSET SCREEN_BUF, 0, SCREEN_BUF_SIZE
	MEMSET VRAM_SCREEN_BUF, 0, SCREEN_BUF_SIZE

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
	MEMSET CHIP_STATE, 0, CHIP_STATE_END - CHIP_STATE
	MEMSET V0, 0, 16

	ld a, CHIP_STACK_SIZE + 1
	ldh [CHIP_SP], a

	ld a, HIGH(CHIP_RAM)
	ldh [I_REG + 1], a

	ld a, 60
	ldh [FRAME_COUNTER], a

	ld a, FX0A_NOT_ACTIVE_FLAG
	ldh [FX0A_KEY_REG], a

	LOAD_CHIP_ROM()
	SETUP_JUMP_TABLES()

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
	WAIT_VRAM_ACCESS()
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

	; reset PC
	ld CHIP_PC, CHIP_RAM + $200

	; turning LCD back on.
	ld a, LCDC_ON | LCDC_BG | LCDC_BLOCK01
	ldh [rLCDC], a
	; enabling interrupts
	xor a
	ldh [rIF], a
	ei

MACRO EXEC
	; loading first byte of the opcode and incrementing pc
	ld a, [CHIP_PC]
	inc CHIP_PC

	; jumping to the opcode handler based on the first byte
	; (table entries are high address bytes and low byte is 0 for all)
	ld h, HIGH(MAIN_JUMP_TABLE)
	ld l, a
	ld h, [hl]
	ld l, 0
	jp hl
ENDM

InstrLoop:
	EXEC()

FrameEnd:
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
	SAFE_HALT() ; wait for VBlank

	jr InstrLoop

MACRO DISPATCH
	ld hl, INSTR_COUNTER 
	dec [hl]
	jr z, .instrBlockEnd\@
	EXEC()
.instrBlockEnd\@:
	rst InstrBlockEnd
ENDM

SECTION "InstrBlockEndHandler", ROM0[$0020]
InstrBlockEnd:
	pop af ; discard return address
	ldh a, [IPF_PER_BLOCK]
	ld [hl], a ; reload number of instructions per block
	ld hl, INSTR_BLOCK_COUNTER
	dec [hl]
	jp nz, InstrLoop
	ldh a, [IPF_BLOCKS_NUM]
	ld [hl], a ; reload number of instruction blocks
	jp FrameEnd

SECTION "InvalidInstrHandler", ROM0[$0000]
InvalidInstr:
	ld b, b
	jr @

;; Instruction decoding, table for matching on the first byte

SECTION "MainJumpTable", WRAM0, ALIGN[8]
MAIN_JUMP_TABLE:
	ds 256

MACRO FILL_NIBBLE
	REPT 16
		db HIGH(\1)
	ENDR
ENDM

SECTION "MainJumpTableROM", ROM0
MAIN_JUMP_TABLE_ROM:
	db HIGH(Case0)
	REPT 15
		db HIGH(InvalidInstr)
	ENDR
	FILL_NIBBLE(Case1)
	FILL_NIBBLE(Case2)
	FILL_NIBBLE(Case3)
	FILL_NIBBLE(Case4)	
	FILL_NIBBLE(Case5)
	FILL_NIBBLE(Case6)
	FILL_NIBBLE(Case7)
	FILL_NIBBLE(Case8)
	FILL_NIBBLE(Case9)
	FILL_NIBBLE(CaseA)
	FILL_NIBBLE(OP_BNNN_JUMPING_OFF)
	FILL_NIBBLE(CaseC)
	FILL_NIBBLE(OP_DXYN_LORES_GBC)
	FILL_NIBBLE(CaseE)
	FILL_NIBBLE(CaseF)
MAIN_JUMP_TABLE_ROM_END:

SECTION "Case0", ROM0, ALIGN[8]
Case0:
	LD_OP_LOW()
	cp $EE
	jr nz, .after

.OP_00EE:
	; underflow check and sp update
	ldh a, [CHIP_SP]
	cp (CHIP_STACK_SIZE + 1)
	jr z, .underflow
	inc a
	ldh [CHIP_SP], a

	; restoring pc from the stack and incrementing, because pc - 1 is saved in 2NNN
	pop CHIP_PC
	inc CHIP_PC

	DISPATCH()
.underflow:
	ld b, b
	jr @

.after:
	inc CHIP_PC
	cp $E0
	jr z, OP_00E0
	ld NN, a
	sub $FB
	jr z, OP_00FB
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
	
	rst InvalidInstr

OP_00E0:
	call ClearChipScreen
	DISPATCH()

; \1 = 0 - left, otherwise right; \2 - 1 if running on GBC, 0 if not.
MACRO SCROLL_HORIZONTAL 
	push CHIP_PC
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

	pop CHIP_PC
	; early instruction loop break, because scroll takes big part of the frame.
	jp FrameEnd

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

	pop CHIP_PC
	jp FrameEnd
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
	push CHIP_PC
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

	pop CHIP_PC
	; early instruction loop break, because scroll takes big part of the frame.
	jp FrameEnd

.scroll0\@:
	DISPATCH()
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
	ld hl, (HIGH(MAIN_JUMP_TABLE) << 8) | ($D * 16)
	ldh a, [IS_GBC]
	and a
	jr nz, .GBC\@
	IF \1 == 0
		ld a, HIGH(OP_DXYN_LORES_DMG)
	ELSE
		ld a, HIGH(OP_DXYN_HIRES_DMG)
	ENDC
	jr .end\@
.GBC\@:
	IF \1 == 0
		ld a, HIGH(OP_DXYN_LORES_GBC)
	ELSE
		ld a, HIGH(OP_DXYN_HIRES_GBC)
	ENDC
.end\@:
	REPT 16
		ld [hl+], a
	ENDR
	ldh a, [QUIRKS]
	bit LEGACY_SCHIP_FLAG, a
	call z, ClearChipScreen ; legacy schip doesn't clear screen on mode changes
	DISPATCH()
ENDM

OP_00FE:
	CHANGE_RES_MODE(0)
OP_00FF:
	CHANGE_RES_MODE(1)

MACRO JMP
	ld OP_HI, a
	LD_OP_LOW()
	ld CHIP_PC_LO, a
	LD_PC_HI_OP_HI()

	DISPATCH()
ENDM

SECTION "Case1", ROM0, ALIGN[8]
Case1: 
OP_1NNN:
	JMP()

SECTION "Case2", ROM0, ALIGN[8]
Case2:
OP_2NNN:
	; overflow check and sp update
	ld hl, CHIP_SP
	dec [hl]
	jr z, .overflow

	; storing current pc to the stack
	push CHIP_PC
	; setting pc to the new value
	JMP()

.overflow:
	ld b, b
	jr @

SECTION "Case3", ROM0, ALIGN[8]
Case3:
OP_3XNN:
	LD_VX_PTR()
	LD_OP_LOW()
	cp [hl]
	jr nz, .end ; skip if equal
	SKIP_NEXT_INSTR()
.end:
	inc CHIP_PC
	DISPATCH()

SECTION "Case4", ROM0, ALIGN[8]
Case4:
OP_4XNN:
	LD_VX_PTR()
	LD_OP_LOW()
	cp [hl]
	jr z, .end ; skip if not equal
	SKIP_NEXT_INSTR()
.end:
	inc CHIP_PC
	DISPATCH()

SECTION "Case5", ROM0, ALIGN[8]
Case5:
OP_5XY0:
	ld OP_HI, a
	LD_OP_LOW()
	ld NN, a
	and $F
	jr nz, .invalid

	LD_Y()
	LD_VX_SAVE_VY()
	cp NN
	jr nz, .end
	SKIP_NEXT_INSTR()
.end:
	inc CHIP_PC
	DISPATCH()

.invalid:
	rst InvalidInstr

SECTION "Case6", ROM0, ALIGN[8]
Case6:
OP_6XNN:
	LD_VX_PTR()
	LD_OP_LOW()
	ld [hl], a

	inc CHIP_PC
	DISPATCH()

SECTION "Case7", ROM0, ALIGN[8]
Case7:
OP_7XNN:
	LD_VX_PTR()
	LD_OP_LOW()
	add [hl]
	ld [hl], a

	inc CHIP_PC
	DISPATCH()

SECTION "Case8", ROM0, ALIGN[8]
Case8:
	ld OP_HI, a
	LD_OP_LOW()
	inc CHIP_PC

	ld h, HIGH(_8XYN_JUMP_TABLE)
	ld l, a
	ld l, [hl]
	inc h
	; load Y
	and $F0
	swap a
	
	jp hl

; short jump table (entries are 1 byte low address bytes for handlers in the next page)
SECTION "8XYNJumpTableROM", ROM0
_8XYN_JUMP_TABLE_ROM:
	REPT 16
		db _8XY0 - _8XYN_DEFAULT_HANDLERS, _8XY1 - _8XYN_DEFAULT_HANDLERS,
		db _8XY2 - _8XYN_DEFAULT_HANDLERS, _8XY3 - _8XYN_DEFAULT_HANDLERS,
		db _8XY4 - _8XYN_DEFAULT_HANDLERS, _8XY5 - _8XYN_DEFAULT_HANDLERS,
		db _8XY6 - _8XYN_DEFAULT_HANDLERS, _8XY7 - _8XYN_DEFAULT_HANDLERS,
		db _8XYN_Invalid - _8XYN_DEFAULT_HANDLERS, _8XYN_Invalid - _8XYN_DEFAULT_HANDLERS,
		db _8XYN_Invalid - _8XYN_DEFAULT_HANDLERS, _8XYN_Invalid - _8XYN_DEFAULT_HANDLERS,
		db _8XYN_Invalid - _8XYN_DEFAULT_HANDLERS, _8XYN_Invalid - _8XYN_DEFAULT_HANDLERS,
		db _8XYE - _8XYN_DEFAULT_HANDLERS, _8XYN_Invalid - _8XYN_DEFAULT_HANDLERS,
	ENDR
_8XYN_JUMP_TABLE_ROM_END:

_8XYN_DEFAULT_HANDLERS:

_8XYN_Invalid:
	rst InvalidInstr

_8XY0:
	LD_VX_PTR_SAVE_VY()
	ld [hl], NN

	DISPATCH()
_8XY1: ; vf reset quirk on
	LD_VX_SAVE_VY()
	or NN
	ld [hl], a

	xor a
	ld [VF], a

	DISPATCH()
_8XY2: ; vf reset quirk on
	LD_VX_SAVE_VY()
	and NN
	ld [hl], a

	xor a
	ld [VF], a

	DISPATCH()
_8XY3: ; vf reset quirk on
	LD_VX_SAVE_VY()
	xor NN
	ld [hl], a

	xor a
	ld [VF], a

	DISPATCH()

MACRO SET_VF_CARRY
	sbc a
	and $1
	ld [VF], a
ENDM
MACRO SET_VF_BORROW
	sbc a
	inc a
	ld [VF], a
ENDM

_8XY4:
	LD_VX_SAVE_VY()
	add NN
	ld [hl], a
	SET_VF_CARRY()

	DISPATCH()
_8XY5:
	LD_VX_SAVE_VY()
	sub NN
	ld [hl], a
	SET_VF_BORROW()

	DISPATCH()
_8XY6: ; shifting quirk off
	LD_VX_PTR_SAVE_VY()
	srl NN
	ld [hl], NN
	SET_VF_CARRY()

	DISPATCH()
_8XY7:
	LD_VX_PTR_SAVE_VY()
	ld a, NN
	sub [hl]
	ld [hl], a
	SET_VF_BORROW()

	DISPATCH()
_8XYE: ; shifting quirk off
	LD_VX_PTR_SAVE_VY()
	sla NN
	ld [hl], NN
	SET_VF_CARRY()

	DISPATCH()

_8XYN_DEFAULT_HANDLERS_END:

DEF HANDLERS_END EQU _8XYE - _8XYN_DEFAULT_HANDLERS
STATIC_ASSERT (HANDLERS_END < 256), STRFMT("8XYN handlers exceed page! (%d bytes)", HANDLERS_END)

_8XYN_VF_RESET_OFF_HANDLERS:
_8XY1_VF_RESET_OFF:
	LD_VX_SAVE_VY()
	or NN
	ld [hl], a

	DISPATCH()
_8XY2_VF_RESET_OFF:
	LD_VX_SAVE_VY()
	and NN
	ld [hl], a

	DISPATCH()
_8XY3_VF_RESET_OFF:
	LD_VX_SAVE_VY()
	xor NN
	ld [hl], a

	DISPATCH()
_8XYN_VF_RESET_OFF_HANDLERS_END:

_8XY6_SHIFTING_ON:
	LD_VX_PTR()
	srl [hl]
	SET_VF_CARRY()

	DISPATCH()
_8XY6_SHIFTING_ON_END:

_8XYE_SHIFTING_ON:
	LD_VX_PTR()
	sla [hl]
	SET_VF_CARRY()

	DISPATCH()
_8XYE_SHIFTING_ON_END:

SECTION "8XYNJumpTable", WRAM0, ALIGN[8]
_8XYN_JUMP_TABLE:
	ds 256
_8XYN_HANDLERS:
	ds _8XYN_DEFAULT_HANDLERS_END - _8XYN_DEFAULT_HANDLERS

SECTION "Case9", ROM0, ALIGN[8]
Case9:
OP_9XY0: 
	ld OP_HI, a
	LD_OP_LOW()
	ld NN, a
	and $F
	jr nz, .invalid

	LD_Y()
	LD_VX_SAVE_VY()
	cp NN
	jr z, .end
	SKIP_NEXT_INSTR()
.end:
	inc CHIP_PC
	DISPATCH()

.invalid:
	rst InvalidInstr

SECTION "CaseA", ROM0, ALIGN[8]
CaseA: 
OP_ANNN:
	; set high byte
	and $F
	add HIGH(CHIP_RAM)
	ldh [I_REG + 1], a

	; set low byte
	LD_OP_LOW()
	ldh [I_REG], a

	inc CHIP_PC
	DISPATCH()

; \1 = 0 jump quirk off (pc = V[0] + NNN), else on (pc = V[x] + NNN).
MACRO BNNN
	ld OP_HI, a
	IF \1 == 0
		ld hl, V0
	ELSE
		LD_VX_PTR()
	ENDC
	LD_OP_LOW()
	add [hl]
	ld CHIP_PC_LO, a
	ld a, OP_HI
	jr nc, .noCarry
	inc a
.noCarry:
	and $F
	add HIGH(CHIP_RAM)
 	ld CHIP_PC_HI, a

	DISPATCH()
ENDM

SECTION "BNNN_JumpingOff", ROM0, ALIGN[8]
OP_BNNN_JUMPING_OFF:
	BNNN(0)

SECTION "CaseBJumpQuirk", ROM0, ALIGN[8]
OP_BNNN_JUMPING_ON:
	BNNN(1)

SECTION "CaseC", ROM0, ALIGN[8]
CaseC:
OP_CXNN: ; V[x] = rand() & NN
	ld OP_HI, a
	RAND()
	xor c
	ld c, a
	ld a, OP_HI
	LD_VX_PTR()
	LD_OP_LOW()
	and c
	ld [hl], a

	inc CHIP_PC
	DISPATCH()

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
	; \1 - 1 if running on GBC, 0 if not
	; \2 - 1 if superchip hires, 0 if not 
	; \3 - 0 if need to preserve hl, 1 if not
	MACRO DXYN_PROCESS_PIXEL ; mask pointer is in BC
		ld a, [bc]
		ld c, a ; saving mask

		; DMG, must wait for hblank/vblank so VRAM is accessible
		IF \1 == 0
			WAIT_VRAM_ACCESS()
			ld a, c
		ENDC

		; collision detection:
		and [hl]
		jr z, .noCollision\@
		; don't set VF immediately to 1 on DMG since it costs 2 more cycles, instead set temp to any non-zero value (a),
		; and correct it to be 1 and laod to VF at the end of DXYN. This is needed so even in the worst case 
		; (WAIT_VRAM_ACCESS returns in the start of OAM scan), second one is not needed because the last vram write (ld [hl-], a)
		; will occur on the last oam scan cycle so VRAM is still writable (4 + 1 + 2 + 2 + 3 + 1 + 2 + 2 + 1 + 2 = 20)
		IF \1 == 0
			ldh [temp4], a
		ELSE
			ld a, 1 
			ld [VF], a
		ENDC
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
		pop CHIP_PC
		; on DMG correct VF to be either 0 or 1 if collision occured, not other value.
		IF \1 == 0 
			ldh a, [temp4]
			and a
			jr z, .noCollision\@
			ld a, 1
		.noCollision\@:
			ld [VF], a
		ENDC
		DISPATCH()
	ENDM

	; \1 - loop label
	; \2 - 1 if running on GBC, 0 if not
	; \3 - 1 if superchip hires, 0 if not
	MACRO DXYN_CHECK_LOOP
		pop bc
		ldh a, [temp2] ; Y
		IF \3 == 0
			cp (CHIP_SCR_HEIGHT - 1)
		ELSE
			cp (SCHIP_SCR_HEIGHT - 1)
		ENDC
		jr z, .yClip\@
		inc a
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

    REDEF xClip EQUS ".xClip\@"
	REDEF spriteByteEnd EQUS ".spriteByteEnd\@"

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
					JZ_TRY xClip
					and $3
				ELSE
					cp SCHIP_SCR_WIDTH
					JZ_TRY xClip
					and $7
				ENDC
				jr nz, .noOffset\@
				ld bc, 128
				add hl, bc
				DXYN_LD_MASK_HIGH_PTR(\2)
			ENDC
		.noOffset\@:
       		sla e
        	jr nc, .skipPixel\@
			IF i != 7
				ld c, a ; x-offset
			ENDC
        	DXYN_PROCESS_PIXEL \1, \2, (i == 0 && \3 != 16)
			IF i != 0 && \3 != 16
				ld a, e
				and a
			ENDC
		.skipPixel\@:
			IF i != 0 && \3 != 16
				JZ_TRY spriteByteEnd
			ENDC
    	ENDR

	{spriteByteEnd}:
		IF \3 == 16 ; 16x16
			ldh a, [temp3] ; loading second column
			ld e, a

			FOR i, 7, -1, -1
				ld a, d
				add (15 - i)
				IF \2 == 0
					cp CHIP_SCR_WIDTH
					JZ_TRY xClip
					and $3
				ELSE
					cp SCHIP_SCR_WIDTH
					JZ_TRY xClip
					and $7
				ENDC
				jr nz, .noOffset\@
				ld bc, 128
				add hl, bc
				DXYN_LD_MASK_HIGH_PTR(\2)				
			.noOffset\@:
				sla e
        		jr nc, .skipPixel\@
				ld c, a
        		DXYN_PROCESS_PIXEL \1, \2, i == 0
				IF i != 0
					ld a, e
					and a
				ENDC
			.skipPixel\@:
				IF i != 0
					JZ_TRY xClip
				ENDC
    		ENDR
		ENDC

 	{xClip}:
		pop hl
    	IF \3 == 1
		    DXYN_FINISH(\1)
    	ELSE
		    DXYN_CHECK_LOOP .heightLoop\@, \1, \2
    	ENDC
	ENDM

	; \1 - 1 if running on GBC, 0 if not.
	; \2 - 1 if superchip hires, 0 if not; 
	; \3 - 1 if height == 1, 0 if not
	MACRO DXYN_LOAD_VALS
		LD_Y()
		ld h, HIGH(V0)
		ld l, a
		ld a, [hl]

		IF \2 == 0
			and CHIP_SCR_HEIGHT - 1
		ELSE
			and SCHIP_SCR_HEIGHT - 1
		ENDC
		IF \3 == 0
			ldh [temp2], a
		ENDC
		ld NN, a

		ld a, OP_HI
		LD_X()
		ld l, a
		ld a, [hl]
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
		
		; fetching base framebuf address:
		add a ; x * 2 (each entry is 2 bytes)
		ld l, a
		ld a, [hl+]
		ld h, [hl]
		; adding y * 2 to the address in hires, y * 4 in lores
		add NN
		add NN
		IF \2 == 0
			add NN
			add NN
		ENDC
		ld l, a

		; clearing VF (both VX and VY are fetched now)
		xor a
		IF \1 == 0
			ldh [temp4], a ; VF is in temp4 during DXYN on DMG
		ELSE
			ld [VF], a
		ENDC

		; set draw flag to 1 on GBC
		IF \1 != 0
			inc a
			ldh [DRAW_FLAG], a
		ENDC

		LD_I_REG b, c
	ENDM

	; \1 - 1 if running on GBC, 0 if not. \2 - 1 if superchip hires, 0 if not
	MACRO DXYN
		ld OP_HI, a
		LD_OP_LOW()
		inc CHIP_PC
		push CHIP_PC
		ld NN, a
		and $F
		dec a ; check if N == 1 
		jp nz, .regularDraw\@
		DXYN_LOAD_VALS \1, \2, 1
		DRAW_SPRITE \1, \2, 1 ; 8x1 (single row) draw
	.regularDraw\@:
		inc a
		jp z, .draw16x16\@
		ldh [temp1], a ; saving height in temp1
		DXYN_LOAD_VALS \1, \2, 0
	.draw8xN\@:
		DRAW_SPRITE \1, \2, -1 
	.draw16x16\@:
		ld a, 16
		ldh [temp1], a
		DXYN_LOAD_VALS \1, \2, 0
		IF \2 == 0 ; legacy schip draws 8x16 instead of 16x16 in lores mode
			ldh a, [QUIRKS]
			bit LEGACY_SCHIP_FLAG, a
			jp nz, .draw8xN\@
		ENDC
		DRAW_SPRITE \1, \2, 16
	ENDM

SECTION "DXYN_LORES_DMG", ROM0, ALIGN[8]
OP_DXYN_LORES_DMG:
	DXYN 0, 0
SECTION "DXYN_HIRES_DMG", ROM0, ALIGN[8]
OP_DXYN_HIRES_DMG:
	DXYN 0, 1

SECTION "DXYN_LORES_GBC", ROM0, ALIGN[8]
OP_DXYN_LORES_GBC:
	DXYN 1, 0
SECTION "DXYN_HIRES_GBC", ROM0, ALIGN[8]
OP_DXYN_HIRES_GBC:
	DXYN 1, 1

MACRO LD_KEY
	ld a, OP_HI
	LD_VX()
	and $F
	add LOW(KEY_MAP)
	ld c, a
	ldh a, [c]
	ld hl, KEY_STATE
	and [hl]
	inc l ; KEY_STATE + 1 is IGNORE_KEYS
	and [hl]
ENDM
	
SECTION "CaseE", ROM0, ALIGN[8]
CaseE: 
	ld OP_HI, a
	LD_OP_LOW()
	inc CHIP_PC
	cp $A1
	jr z, .OP_EXA1
	cp $9E
	jr nz, .invalid

.OP_EX9E:
	LD_KEY()
	jr z, .end1 ; dont skip if key is not pressed
	SKIP_NEXT_INSTR()
.end1:
	DISPATCH()

.OP_EXA1:
	LD_KEY()
	jr nz, .end2 ; dont skip if key is pressed
	SKIP_NEXT_INSTR()
.end2:
	DISPATCH()

.invalid:
	rst InvalidInstr
	
SECTION "CaseF", ROM0, ALIGN[8]
CaseF:
	ld OP_HI, a
	LD_OP_LOW()
	inc CHIP_PC

	bit 7, a
	jr nz, .bit7Set
	JP_TABLE(FX_JUMP_TABLE)	
.invalid:
	rst InvalidInstr
.bit7Set:
	cp $85
	jr nz, .invalid

SECTION "FXJumpTable", WRAM0, ALIGN[8]
FX_JUMP_TABLE:
	ds 256

; Matching on 7 low bits of the opcode, so 128 entries fit in a page.
SECTION "FXJumpTableROM", ROM0
FX_JUMP_TABLE_ROM:
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
    dw FX55_MEM_INCREMENT_ON ; $55
    ds 30, LOW(InvalidInstr), HIGH(InvalidInstr) ; $56-$64
    dw FX65_MEM_INCREMENT_ON ; $65
	ds 30, LOW(InvalidInstr), HIGH(InvalidInstr) ; $66-$74
	dw FX75 ; $75
    ds 20, LOW(InvalidInstr), HIGH(InvalidInstr) ; $76-$7F
FX_JUMP_TABLE_ROM_END:

; \1 = 0 - load from flags; else, store to flags.
MACRO SCHIP_RPL_STORE 
	ld a, OP_HI
	LD_VX()
	and $7
	ld b, a
	inc b ; v[x] is included

	ld hl, V0
	ld c, LOW(SCHIP_RPL_FLAGS)
.copyLoop\@
	IF \1 == 0
		ldh a, [c]
		ld [hl+], a
	ELSE
		ld a, [hl+]
		ldh [c], a
	ENDC

	inc c
	dec b
	jr nz, .copyLoop\@

	DISPATCH()
ENDM

FX85:
	SCHIP_RPL_STORE(0)

FX75:
	SCHIP_RPL_STORE(1)

FX07:
	ld a, OP_HI
	LD_VX_PTR()
	ldh a, [DELAY_TIMER]
	ld [hl], a

	DISPATCH()

FX0A:
	ldh a, [FX0A_KEY_REG]
	cp FX0A_NOT_ACTIVE_FLAG
	jr nz, .skipFX0AInit
	ld a, OP_HI
	LD_X()
	ldh [FX0A_KEY_REG], a
.skipFX0AInit:
	cp FX0A_DONE_FLAG
	jr z, .FX0ADone
	; PC -= 2
	dec CHIP_PC
	dec CHIP_PC
	; limit CPU usage while waiting for key:
	jp FrameEnd
.FX0ADone:
	ld a, FX0A_NOT_ACTIVE_FLAG
	ldh [FX0A_KEY_REG], a
	DISPATCH()

FX15:
	ld a, OP_HI
	LD_VX()
	ldh [DELAY_TIMER], a

	DISPATCH()

FX18:
	ld a, OP_HI
	LD_VX()
	ldh [SOUND_TIMER], a

	DISPATCH()

FX1E:
	ld a, OP_HI
	LD_VX_PTR()
	ld c, LOW(I_REG)

	ldh a, [c]
	add [hl]
	ldh [c], a
	jr nc, .noCarry
	inc c
	ldh a, [c]
	cp (HIGH(CHIP_RAM_END) - 1)
	jr z, .overflow
	inc a
.storeHigh:
	ldh [c], a
.noCarry:
 	DISPATCH()
.overflow:
	ld a, HIGH(CHIP_RAM)
	jr .storeHigh

FX29:
	ld a, OP_HI
	LD_VX()
	and $F
	ld b, a
	; multiply by 5
	add a
	add a
	add b

	ldh [I_REG], a
	ld a, HIGH(CHIP_RAM)
	ldh [I_REG + 1], a

	DISPATCH()

FX30:
	ld a, OP_HI
	LD_VX()
	and $F
	ld b, a
	; multiply by 10
	add a
	add a
	add b
	add a
	; schip font is stored after chip8 font, so add offset
	add (CHIP8_FONT_END - CHIP8_FONT)

	ldh [I_REG], a
	ld a, HIGH(CHIP_RAM)
	ldh [I_REG + 1], a

	DISPATCH()

FX33:
	ld a, OP_HI
	LD_VX()

	ld hl, I_REG
	ld c, [hl]
	inc l
	ld h, [hl]
	ld l, c

	ld bc, (100 << 8) | $FF ; b = 100, c = -1

.countHundreds:
	inc c
	sub b
	jr nc, .countHundreds

	add b
	ld b, a
	ld a, c
	ld [hl+], a ; storing hundreds
	CHECK_MEM_WRITE_OOB h, .end

	ld a, b
	ld bc, (10 << 8) | $FF ; b = 10, c = -1

.countTenths:
	inc c
	sub b
	jr nc, .countTenths

	add b
	ld b, a
	ld a, c
	ld [hl+], a ; storing tenths
	CHECK_MEM_WRITE_OOB h, .end

	ld [hl], b ; storing ones	
.end:
	DISPATCH()

; \1 = 0 - load from ram, else, store to ram.
; \2 = 0 - memory increment quirk off, else on.
MACRO REG_STORE 
	push CHIP_PC
	ld a, OP_HI
	LD_X()
	ld b, a 
	inc b ; V[x] is included

	LD_I_REG h, l
	ld de, V0
.copyLoop\@
	IF \1 == 0
		ld a, [hl+]
		ld [de], a
	ELSE
		CHECK_MEM_WRITE_OOB h, .end\@
		ld a, [de]
		ld [hl+], a
	ENDC

	inc e
	dec b
	jr nz, .copyLoop\@

.end\@:
	pop CHIP_PC
	IF \2 == 0
		DISPATCH()
	ELSE
		ld a, l
		ldh [I_REG], a
		ld a, h
		cp HIGH(CHIP_RAM_END)
		jr z, .overflow\@
	.storeHigh\@
		ldh [I_REG + 1], a
		DISPATCH()
	.overflow\@:
		ld a, HIGH(CHIP_RAM)
		jr .storeHigh\@
	ENDC
ENDM

FX55_MEM_INCREMENT_OFF: 
	REG_STORE 1, 0
FX55_MEM_INCREMENT_ON: 
	REG_STORE 1, 1

FX65_MEM_INCREMENT_OFF: 
	REG_STORE 0, 0
FX65_MEM_INCREMENT_ON:
	REG_STORE 0, 1

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