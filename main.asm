; asmsyntax=ca65

.feature underline_in_numbers

.include "nes2header.inc"

nes2mapper 4
nes2prg 512 * 1024
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

.segment "VECTORS"
    .word NMI
    .word RESET
    .word IRQ

.segment "ZEROPAGE"

DoubleCount: .res 2
PointerA: .res 2

; Low byte is fraction
; Mid byte is scroll
; Bit 0 of high byte is nametable
ScrollTop: .res 3
ScrollMid: .res 3
ScrollBot: .res 3

; Sprite peaks
PEAK_A_OFFSET = 112
PEAK_B_OFFSET = 205
PEAK_C_OFFSET = 0; ??

; Scroll rate for the bottom (fastest) plane.
; Derive the others from this. Start at half for each step?
ScrollRate: .res 2

sleeping: .res 1

ScanlineA: .res 1

ATTR_BOTL = %0001_0000
ATTR_BOTR = %0100_0000
ATTR_TOPL = %0000_0001
ATTR_TOPR = %0000_0100

Controller:     .res 1
Controller_Old: .res 1

PpuCtrlMid: .res 1

diff:   .res 1
id:     .res 1
offset: .res 1
offsetB: .res 1

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

    ; Top A
    lda Table_TopA_Size
    sta DoubleCount
    lda Table_TopA_Size+1
    sta DoubleCount+1

    lda #<Table_TopA
    sta PointerA+0
    lda #>Table_TopA
    sta PointerA+1

    jsr DrawBg

    ; Bottom A
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

    ; Top B
    lda Table_TopB_Size
    sta DoubleCount
    lda Table_TopB_Size+1
    sta DoubleCount+1

    lda #<Table_TopB
    sta PointerA+0
    lda #>Table_TopB
    sta PointerA+1

    jsr DrawBg

    ; Bottom B
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
    ; 1st peak
    ldy #103; Y pos
    ldx #0  ; attr
    sty Sprites+SpriteY + (0 * 4)
    stx Sprites+SpriteAttr + (0 * 4)
    lda #$80
    sta Sprites+SpriteTile + (0 * 4)
    ;lda #PEAK_A_OFFSET
    lda SpriteOffsetsA+0
    sta Sprites+SpriteX + (0 * 4)

    ; 2nd peak

    .repeat 3, i
    sty Sprites+SpriteY + ((i + 1) * 4)
    stx Sprites+SpriteAttr + ((i + 1) * 4)
    lda #$81 + i
    sta Sprites+SpriteTile + ((i + 1) * 4)
    ;lda #PEAK_B_OFFSET + (i * 8)
    lda SpriteOffsetsA+i
    sta Sprites+SpriteX + ((i + 1) * 4)
    .endrepeat

    ; 3rd peak
;    .repeat 4, i
;    sty Sprites+SpriteY + ((i + 4) * 4)
;    stx Sprites+SpriteAttr + ((i + 4) * 4)
;    lda #$84 + i
;    sta Sprites+SpriteTile + ((i + 4) * 4)
;    ;lda #PEAK_B_OFFSET + (i * 8)
;    lda SpriteOffsetsB+i
;    sta Sprites+SpriteX + ((i + 4) * 4)
;    .endrepeat

    lda #111
    sta ScanlineA

    cli
    lda #$18
    sta $2001
    lda #$88
    sta $2000
Frame:

    jsr ReadControllers

    lda Controller
    and #BUTTON_LEFT
    beq :+
    ; move left
    jsr ScrollLeft
:

    lda Controller
    and #BUTTON_RIGHT
    beq :+
    ; move right
    jsr ScrollRight
:

    jsr UpdateSprites

    ; Nametable
    lda ScrollMid+2
    and #$01

    ; NMI on and Sprite pattern table
    ora #$98
    ;sta PpuCtrlMid
    tax

    ldy ScrollMid + 1

    lda ScanlineA
    sta $E001   ; enable IRQ
    sta $C000   ; latch the value
    sta $C001   ; reload value

    ; prep for IRQ
    ;lda #$80

    ; Wait for NMI
    lda #1
    sta sleeping
:
    lda sleeping
    bne :-
    jmp Frame

IRQ:
    ; Put pattern table swap here
    ;lda PpuCtrlCache
    .repeat 46
    nop
    .endrepeat
    sty $2005   ; fine scroll
    stx $2000   ; nametable
    ;bit $2002   ; reset latch
    stx $E000   ; disable IRQ
    rti

Palettes:
    .byte $2C, $20, $10, $00
    .byte $2C, $21, $11, $19

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
    lda ScrollMid + 1
    sec
    sbc #1
    sta ScrollMid + 1
    bcs :+
    dec ScrollMid + 2   ; inc nametable
:

    ; fractonial
    lda ScrollTop + 0
    sec
    sbc #$80
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
    lda ScrollMid + 1
    clc
    adc #1
    sta ScrollMid + 1
    bcc :+
    inc ScrollMid + 2   ; inc nametable
:

    ; fractional
    lda ScrollTop + 0
    clc
    adc #$80
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

WriteAttr:
    lda #0
    ldx #$18
:
    sta $2007
    dex
    bne :-

    lda #ATTR_BOTL | ATTR_BOTR
    ldx #8
:
    sta $2007
    dex
    bne :-

    lda #$55
    ldx #32
:
    sta $2007
    dex
    bne :-
    rts

PEAK_Y_VAL = 103

SpriteOffsetsA:
    .byte 112
    .byte 205
    .byte 213
    .byte 221

SpriteOffsetsB:
    .byte 112
    .byte 205
    .byte 213
    .byte 221

UpdateSprites:
    lda #0
    sta id

    lda #0
    sta offset
@loop:
    jsr sp_UpdateA

    inc id

    lda id
    asl a
    asl a
    sta offset

    lda id
    cmp #4
    bcc @loop
    rts

; `id` should contain the sprite ID
sp_UpdateA:
    ; if on NT0
    ;   if offset > scroll
    ;     draw
    ; if on NT1
    ;   if scroll > offset 
    ;     draw

    lda ScrollMid+2
    and #$01
    beq @nt0

    ; if (256 - Scroll) + offset < 256
    ;  draw
    lda #0
    sec
    sbc ScrollMid+1
    clc
    ldx id
    adc SpriteOffsetsA, x
    ;adc #PEAK_A_OFFSET
    bcc :+

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
    lda SpriteOffsetsA, x
    ;lda #PEAK_A_OFFSET
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

sp_UpdateB:
    ; if on NT0
    ;   if offset > scroll
    ;     draw
    ; if on NT1
    ;   if scroll > offset 
    ;     draw

    lda ScrollMid+2
    and #$01
    bne @nt1

    ; if (256 - Scroll) + offset < 256
    ;  draw
    lda #0
    sec
    sbc ScrollMid+1
    clc
    ldx id
    adc SpriteOffsetsB, x
    ;adc #PEAK_A_OFFSET
    bcc :+

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

@nt1:
    ldx id
    lda SpriteOffsetsB, x
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

Table_TopA_Size = :+
Table_TopA      = :++
Table_TopB_Size = :+++
Table_TopB      = :++++

    .include "tiles_top.ids.asm"

Table_BotA_Size = :+
Table_BotA      = :++
Table_BotB_Size = :+++
Table_BotB      = :++++

    .include "tiles_bot.ids.asm"

.segment "LOW_00"
.segment "LOW_01"
.segment "LOW_02"
.segment "LOW_03"
.segment "LOW_04"
.segment "LOW_05"
.segment "LOW_06"
.segment "LOW_07"
.segment "LOW_08"
.segment "LOW_09"
.segment "LOW_10"
.segment "LOW_11"
.segment "LOW_12"
.segment "LOW_13"
.segment "LOW_14"
.segment "LOW_15"
.segment "LOW_16"
.segment "LOW_17"
.segment "LOW_18"
.segment "LOW_19"
.segment "LOW_20"
.segment "LOW_21"
.segment "LOW_22"
.segment "LOW_23"
.segment "LOW_24"
.segment "LOW_25"
.segment "LOW_26"
.segment "LOW_27"
.segment "LOW_28"
.segment "LOW_29"
.segment "LOW_30"

.segment "HIGH_00"
.segment "HIGH_01"
.segment "HIGH_02"
.segment "HIGH_03"
.segment "HIGH_04"
.segment "HIGH_05"
.segment "HIGH_06"
.segment "HIGH_07"
.segment "HIGH_08"
.segment "HIGH_09"
.segment "HIGH_10"
.segment "HIGH_11"
.segment "HIGH_12"
.segment "HIGH_13"
.segment "HIGH_14"
.segment "HIGH_15"
.segment "HIGH_16"
.segment "HIGH_17"
.segment "HIGH_18"
.segment "HIGH_19"
.segment "HIGH_20"
.segment "HIGH_21"
.segment "HIGH_22"
.segment "HIGH_23"
.segment "HIGH_24"
.segment "HIGH_25"
.segment "HIGH_26"
.segment "HIGH_27"
.segment "HIGH_28"
.segment "HIGH_29"
.segment "HIGH_30"
