; asmsyntax=ca65

.feature underline_in_numbers

.include "nes2header.inc"

nes2mapper 4
nes2prg 2 * 8 * 1024
nes2chr 1 *  8 * 1024
nes2mirror 'V'
nes2tv 'N'
nes2end

; Button Constants
BUTTON_A        = 1 << 7
BUTTON_B        = 1 << 6
BUTTON_SELECT   = 1 << 5
BUTTON_START    = 1 << 4
BUTTON_UP       = 1 << 3
BUTTON_DOWN     = 1 << 2
BUTTON_LEFT     = 1 << 1
BUTTON_RIGHT    = 1 << 0

; This is OR'd with the nametable before being
; written.
PPU_CTRL_VAL = $88

; Top bank is split in two, bottom bank is split
; in three.  Bank IDs are lowest common denomitator
BANK_CLOUDS_A = 0
BANK_CLOUDS_B = 2
BANK_MOUNTAIN_A = 4
BANK_MOUNTAIN_B = 6

; All peak sprites are at the same Y value
PEAK_Y_VAL = 104

; Ratios for the splits.  These are both relative
; to the bottom split, not eachother.
ScrollMid_Ratio = $80
ScrollTop_Ratio = $40

; Attribute quadrant values
ATTR_BOTL = %0001_0000
ATTR_BOTR = %0100_0000
ATTR_TOPL = %0000_0001
ATTR_TOPR = %0000_0100

.segment "VECTORS"
    .word NMI
    .word RESET
    .word IRQ

.segment "ZEROPAGE"

; Counts remaining background tiles to draw.
DoubleCount: .res 2
; Points to BG tile data.
PointerA: .res 2

; Low byte is fraction
; Mid byte is scroll
; Bit 0 of high byte is nametable
ScrollTop: .res 3
ScrollMid: .res 3
ScrollBot: .res 3

; Scroll rate for the bottom (fastest) plane.
; Derive the others from this. Start at half for each step?
ScrollRate: .res 2

; Left or right
ScrollDirection: .res 1

; Scanline values for split.
ScanlineA: .res 1
ScanlineB: .res 1

; For the controller input code
Controller:     .res 1
Controller_Old: .res 1
btnX:           .res 1
btnY:           .res 1

; Nametable value written during IRQ
Irq2000Val: .res 1

; For the IRQ check
IrqWait: .res 1

; For the NMI check
sleeping: .res 1

; Used during the sprite update to differentiate
; between which nametable the sprite belongs on.
spriteNtCheck: .res 1

; Id and offset for the current sprite being
; updated.
id:     .res 1
offset: .res 1

.segment "TILES0"
    .incbin "tiles_top.chr"

.segment "TILES1"
    .incbin "tiles_bot.chr"

.segment "TILES2"
    .incbin "sprites.chr"

.segment "SPRITES"
SpriteY     = 0
SpriteTile  = 1
SpriteAttr  = 2
SpriteX     = 3

Sprites: .res 256

.segment "RAM"

.segment "PAGE_FIXED"

RESET:
    sei         ; Disable IRQs
    cld         ; Disable decimal mode

    ldx #$40
    stx $4017   ; Disable APU frame IRQ

    ldx #$FF
    txs         ; Setup new stack

    inx         ; Now X = 0

    stx $2000   ; disable NMI
    stx $2001   ; disable rendering
    stx $4010   ; disable DMC IRQs

:   ; First wait for VBlank to make sure PPU is ready.
    bit $2002   ; test this bit with ACC
    bpl :- ; Branch on result plus

:   ; Clear RAM
    lda #$00
    sta $0000, x
    sta $0100, x
    sta $0200, x
    sta $0300, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x

    inx
    bne :-  ; loop if != 0

; Clear attr table first
    bit $2002
    lda #$23
    sta $2006
    lda #$C0
    sta $2006
    jsr WriteAttr

    lda #$27
    sta $2006
    lda #$C0
    sta $2006
    jsr WriteAttr

    bit $2002
    lda #$20
    sta $2006
    lda #$00
    sta $2006

    ; Top, Nametable 0
    lda Table_TopA_Size
    sta DoubleCount
    lda Table_TopA_Size+1
    sta DoubleCount+1

    lda #<Table_TopA
    sta PointerA+0
    lda #>Table_TopA
    sta PointerA+1

    jsr DrawBg

    ; Bottom, Nametable 0
    lda Table_BotA_Size
    sta DoubleCount
    lda Table_BotA_Size+1
    sta DoubleCount+1

    lda #<Table_BotA
    sta PointerA+0
    lda #>Table_BotA
    sta PointerA+1

    jsr DrawBg

    bit $2002
    lda #$24
    sta $2006
    lda #$00
    sta $2006

    ; Top, Nametable 1
    lda Table_TopB_Size
    sta DoubleCount
    lda Table_TopB_Size+1
    sta DoubleCount+1

    lda #<Table_TopB
    sta PointerA+0
    lda #>Table_TopB
    sta PointerA+1

    jsr DrawBg

    ; Bottom, Nametable 1
    lda Table_BotB_Size
    sta DoubleCount
    lda Table_BotB_Size+1
    sta DoubleCount+1

    lda #<Table_BotB
    sta PointerA+0
    lda #>Table_BotB
    sta PointerA+1

    jsr DrawBg

    ; Setup sprites
    ; Only attributes and tiles are set here.  The
    ; X and Y values are rewritten on every frame.
    lda #0
    ; Set all the attributes
    .repeat 8, i
    sta Sprites+SpriteAttr + (i * 4)
    .endrepeat

    ; Using .repeat macros for simplicity
    ; 1st peak
    lda #$F0
    sta Sprites+SpriteTile + (0 * 4)

    ; 2nd peak
    .repeat 3, i
    lda #$F1 + i
    sta Sprites+SpriteTile + ((i + 1) * 4)
    .endrepeat

    ; 3rd peak
    .repeat 4, i
    lda #$F4 + i
    sta Sprites+SpriteTile + ((i + 4) * 4)
    .endrepeat

    lda #111
    sta ScanlineA
    lda #62
    sta ScanlineB

    lda ScrollMid+2
    and #$01

    ; NMI on and Sprite pattern table
    ora #$98
    sta Irq2000Val

    cli
    lda #$98
    sta $2001
    lda #$88
    sta $2000
Frame:

    ;
    ;   Split A
    ;

    ; Setup Mapper
    lda #$00
    sta $8000   ; R0
    lda #BANK_CLOUDS_A
    sta $8001   ; Clouds top

    lda #$01
    sta $8000   ; R1
    lda #BANK_CLOUDS_B
    sta $8001 ; Clouds bottom (3rd bank)

    ; Setup IRQ scroll value for next frame
    lda ScrollMid+2
    and #$01 ; nametable

    ; NMI on and Sprite pattern table
    ora #PPU_CTRL_VAL
    sta Irq2000Val

    lda ScanlineA
    sta $E001   ; enable IRQ
    sta $C000   ; latch the value
    sta $C001   ; reload value

    ldx Irq2000Val
    ldy ScrollMid + 1

    ; Setup IRQ and wait for it
    lda #1
    sta IrqWait
:
    lda IrqWait
    bne :-

    ; Wait until the next scanline.  We wait
    ; because there isn't enough time between the
    ; IRQ and the next scanline.
    .repeat 30
    nop
    .endrepeat

    ;bit $2002

    sty $2005   ; fine scroll
    stx $2000   ; nametable

    ; Setup Mapper
    lda #$00
    sta $8000   ; R0
    lda #BANK_MOUNTAIN_A
    sta $8001   ; Mountain top

    lda #$01
    sta $8000   ; R1
    lda #BANK_MOUNTAIN_B
    sta $8001   ; Mountain bottom

    ;
    ;   Split B
    ;

    ; Setup IRQ scroll value for next frame
    lda ScrollBot+2
    and #$01 ; nametable

    ; NMI on and Sprite pattern table
    ora #PPU_CTRL_VAL
    sta Irq2000Val

    lda ScanlineB
    sta $E001   ; enable IRQ
    sta $C000
    sta $C001

    ldy ScrollBot + 1   ; Fine scroll
    ldx Irq2000Val

    ldx Irq2000Val
    lda #1
    sta IrqWait
:
    lda IrqWait
    bne :-

    ; Apparently we don't need to do the wait
    ; here like the first split.  Idk.

    bit $2002
    sty $2005   ; fine scroll
    stx $2000   ; nametable

; This runs after all the splits

    jsr ReadControllers

    ; Toggle scroll direction when A is pressed
    lda #BUTTON_A
    jsr ButtonPressed
    beq :+
    lda #$FF
    eor ScrollDirection
    sta ScrollDirection
:

    ; Scroll in the appropriate direction
    lda ScrollDirection
    bne :+
    jsr ScrollRight
    jmp :++
:
    jsr ScrollLeft
:

    ; Update sprites after all the scroll values
    ; have been written.
    jsr UpdateSprites

    ; Wait for NMI
    lda #1
    sta sleeping
:
    lda sleeping
    bne :-
    jmp Frame

IRQ:
    ; Stop waiting for IRQ
    lda #0
    sta IrqWait
    sta $E000   ; disable IRQ
    rti

Palettes:
    .byte $31, $20, $10, $00
    .byte $31, $21, $11, $19

NMI:
    bit $2002

    ; Sprites
    lda #$00
    sta $2003
    lda #$02
    sta $4014

    ; Palettes
    lda #$3F
    sta $2006
    lda #$00
    sta $2006

    ldx #0
:
    lda Palettes, x
    sta $2007
    inx
    cpx #8
    bne :-

    lda #$3F
    sta $2006
    lda #$10
    sta $2006
    ldx #4
:
    lda Palettes, x
    sta $2007
    inx
    cpx #8
    bne :-

    ; Nametable
    lda ScrollTop+2
    and #$01

    ; NMI on and Sprite pattern table
    ora #$88
    sta $2000

    bit $2002

    lda ScrollTop+1
    sta $2005

    lda #0
    sta $2005
    sta sleeping
    rti

DrawBg:
    ldy #0
@loop:
    lda (PointerA), y
    sta $2007

    inc PointerA
    bne :+
    inc PointerA+1
:

    lda DoubleCount
    sec
    sbc #1
    sta DoubleCount

    lda DoubleCount+1
    sbc #0
    sta DoubleCount+1

    lda DoubleCount
    bne @loop
    lda DoubleCount+1
    bne @loop
    rts

ScrollLeft:
    lda ScrollBot + 1
    sec
    sbc #1
    sta ScrollBot + 1
    bcs :+
    dec ScrollBot + 2
:

    ; fractonial
    lda ScrollMid + 0
    sec
    sbc #ScrollMid_Ratio
    sta ScrollMid + 0
    bcs :+
    ; whole
    lda ScrollMid + 1
    sbc #0
    sta ScrollMid + 1

    bcs :+
    ; nametable
    dec ScrollMid + 2
:

    ; fractonial
    lda ScrollTop + 0
    sec
    sbc #ScrollTop_Ratio
    sta ScrollTop + 0
    bcs :+
    ; whole
    lda ScrollTop + 1
    sbc #0
    sta ScrollTop + 1

    bcs :+
    ; nametable
    dec ScrollTop + 2
:
    rts

ScrollRight:
    lda ScrollBot + 1
    clc
    adc #1
    sta ScrollBot + 1
    bcc :+
    inc ScrollBot + 2   ; inc nametable
:

    ; fractional
    lda ScrollMid + 0
    clc
    adc #ScrollMid_Ratio
    sta ScrollMid + 0
    bcc :+
    ; whole
    lda ScrollMid + 1
    adc #0
    sta ScrollMid + 1
    bcc :+
    ; nametable
    inc ScrollMid + 2
:

    ; fractional
    lda ScrollTop + 0
    clc
    adc #ScrollTop_Ratio
    sta ScrollTop + 0
    bcc :+
    ; whole
    lda ScrollTop + 1
    adc #0
    sta ScrollTop + 1
    bcc :+
    ; nametable
    inc ScrollTop + 2
:
    rts

; The clouds use a different palette than the
; mountains.  This should be called after the
; PPU address has been written.
WriteAttr:
    lda #0
    ldx #$18
@clouds:
    sta $2007
    dex
    bne @clouds

    lda #ATTR_BOTL | ATTR_BOTR
    ldx #8
@transition:
    sta $2007
    dex
    bne @transition

    lda #$55
    ldx #32
@mountains:
    sta $2007
    dex
    bne @mountains
    rts

; Update sprites for both nametables. This is
; split into two loops for simplicity.
UpdateSprites:
; Nametable 0
    lda #0
    sta id

    lda #0
    sta offset
@loop:

    lda id
    cmp #4
    bcc @firstNt
    ldy #1
    sty spriteNtCheck
    jmp @next
@firstNt:
    ldy #0
    sty spriteNtCheck
@next:

    asl a
    asl a
    sta offset

    jsr sp_Update
    inc id

    lda id
    cmp #8  ; TODO: remove magic number
    bcc @loop

    rts

; 'id' should contain the sprite ID
sp_Update:
    ; if on NT0
    ;   if offset > scroll
    ;     draw
    ; if on NT1
    ;   if scroll > offset
    ;     draw

    ; If Sprite is on the second nametable, invert
    ; the onscreen check.
    lda spriteNtCheck
    bne @secondNt

    lda ScrollMid+2
    and #$01
    beq @nt0
    jmp @nt1

@secondNt:
    lda ScrollMid+2
    and #$01
    beq @nt1
    jmp @nt0

@nt1:

    ; if (256 - Scroll) + offset < 256
    ;  draw
    lda #0
    sec
    sbc ScrollMid+1
    beq :+
    clc
    ldx id
    adc SpriteOffsets, x
    bcc :++
:
    ;
    ; not on screen
    lda #$F0
    ldx offset
    sta Sprites+SpriteY, x
    rts
:
    ldx offset
    sta Sprites+SpriteX, x
    jmp @onScreen

@nt0:
    ldx id
    lda SpriteOffsets, x
    sec
    sbc ScrollMid+1
    bcs :+

    ;
    ; not on screen
    lda #$F0
    ldx offset
    sta Sprites+SpriteY, x
    rts

:
    ldx offset
    sta Sprites+SpriteX, x

@onScreen:
    lda #PEAK_Y_VAL
    ldx offset
    sta Sprites+SpriteY, x
    rts

ReadControllers:
    lda Controller
    sta Controller_Old

    ; Freeze input
    lda #1
    sta $4016
    lda #0
    sta $4016

    ldx #$08
@player1:
    lda $4016
    lsr A           ; Bit0 -> Carry
    rol Controller ; Bit0 <- Carry
    dex
    bne @player1
    rts

; Was a button pressed this frame?
ButtonPressed:
    sta btnX
    and Controller
    sta btnY

    lda Controller_Old
    and btnX

    cmp btnY
    bne @btnPress_stb

    ; no button change
    rts

@btnPress_stb:
    ; button released
    lda btnY
    bne @btnPress_stc
    rts

@btnPress_stc:
    ; button pressed
    lda #1
    rts

; X offsets
SpriteOffsets:
    ; Peak 1
    .byte 114

    ; Peak 2
    .byte 205
    .byte 213
    .byte 221

    ; Peak 3
    .repeat 4, i
    .byte 38 + (i * 8)
    .endrepeat

; Tile IDs for the top half of the image (clouds)
Table_TopA_Size = :+
Table_TopA      = :++
Table_TopB_Size = :+++
Table_TopB      = :++++

    .include "tiles_top.ids.asm"

; Tile IDs for the bottom half of the image (mountains)
Table_BotA_Size = :+
Table_BotA      = :++
Table_BotB_Size = :+++
Table_BotB      = :++++

    .include "tiles_bot.ids.asm"

; None of these are required, but are supported
; because we're using MMC3

;.segment "LOW_00"
;.segment "LOW_01"
;.segment "LOW_02"
;.segment "LOW_03"
;.segment "LOW_04"
;.segment "LOW_05"
;.segment "LOW_06"
;.segment "LOW_07"
;.segment "LOW_08"
;.segment "LOW_09"
;.segment "LOW_10"
;.segment "LOW_11"
;.segment "LOW_12"
;.segment "LOW_13"
;.segment "LOW_14"
;.segment "LOW_15"
;.segment "LOW_16"
;.segment "LOW_17"
;.segment "LOW_18"
;.segment "LOW_19"
;.segment "LOW_20"
;.segment "LOW_21"
;.segment "LOW_22"
;.segment "LOW_23"
;.segment "LOW_24"
;.segment "LOW_25"
;.segment "LOW_26"
;.segment "LOW_27"
;.segment "LOW_28"
;.segment "LOW_29"
;.segment "LOW_30"

;.segment "HIGH_00"
;.segment "HIGH_01"
;.segment "HIGH_02"
;.segment "HIGH_03"
;.segment "HIGH_04"
;.segment "HIGH_05"
;.segment "HIGH_06"
;.segment "HIGH_07"
;.segment "HIGH_08"
;.segment "HIGH_09"
;.segment "HIGH_10"
;.segment "HIGH_11"
;.segment "HIGH_12"
;.segment "HIGH_13"
;.segment "HIGH_14"
;.segment "HIGH_15"
;.segment "HIGH_16"
;.segment "HIGH_17"
;.segment "HIGH_18"
;.segment "HIGH_19"
;.segment "HIGH_20"
;.segment "HIGH_21"
;.segment "HIGH_22"
;.segment "HIGH_23"
;.segment "HIGH_24"
;.segment "HIGH_25"
;.segment "HIGH_26"
;.segment "HIGH_27"
;.segment "HIGH_28"
;.segment "HIGH_29"
;.segment "HIGH_30"
