;;; Simple sprite that can be moved with the dpad easily. Use it to have
;;; something funny moving.
;; Taken from https://github.com/mssola/code.nes.

.include "joypad.s"

.scope Diskun
    m_screen_x = $30
    m_screen_y = $31

    .proc init_palettes
        lda #$3F
        sta $2006                   ; PPUADDR
        lda #$00
        sta $2006                   ; PPUADDR

        ldx #0
    @load_palettes_loop:
        lda palettes, x
        sta $2007                   ; PPUDATA
        inx
        cpx #$20
        bne @load_palettes_loop
        rts
    palettes:
        DEFAULT_COLOR = $11

        ;; Background
        .byte DEFAULT_COLOR, $36, $17, $0F
        .byte DEFAULT_COLOR, $00, $00, $00
        .byte DEFAULT_COLOR, $00, $00, $00
        .byte DEFAULT_COLOR, $00, $00, $00

        ;; Foreground
        .byte DEFAULT_COLOR, $28, $0F, $30
        .byte DEFAULT_COLOR, $00, $00, $00
        .byte DEFAULT_COLOR, $00, $00, $00
        .byte DEFAULT_COLOR, $00, $00, $00

        rts
    .endproc

    .proc update
        lda #Joypad::BUTTON_UP
        and Joypad::m_buttons1
        beq @check_down

        dec Diskun::m_screen_y
        dec Diskun::m_screen_y
        jmp @check_left
    @check_down:
        lda #Joypad::BUTTON_DOWN
        and Joypad::m_buttons1
        beq @check_left

        inc Diskun::m_screen_y
        inc Diskun::m_screen_y
    @check_left:
        lda #Joypad::BUTTON_LEFT
        and Joypad::m_buttons1
        beq @check_right

        dec Diskun::m_screen_x
        dec Diskun::m_screen_x
    @check_right:
        lda #Joypad::BUTTON_RIGHT
        and Joypad::m_buttons1
        beq @end

        inc Diskun::m_screen_x
        inc Diskun::m_screen_x
    @end:
        rts
    .endproc

    .proc nmi_update
        lda Diskun::m_screen_x
        sta $203
        sta $20B
        clc
        adc #8
        sta $207
        sta $20F

        lda Diskun::m_screen_y
        sta $200
        sta $204
        clc
        adc #8
        sta $208
        sta $20C

        rts
    .endproc
.endscope

