.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $02, $01
    .byte $00
    .byte $00

.segment "CHARS"
.byte 0

.segment "CODE"
;; asan:stack full

;; NOTE: adapted from git.mssola.com/nes/code.nes
.scope Joypad
    ;; Button masks.
    BUTTON_A      = 1 << 7
    BUTTON_B      = 1 << 6
    BUTTON_UP     = 1 << 3
    BUTTON_DOWN   = 1 << 2

    ;; Port addresses for controllers.
    m_joypad1 = $4016

    ;; After running a `joypad_read_*` function these two variables will contain
    ;; the given result.
    zp_buttons1 = $22

    ;;;
    ;; Safely read via a re-read algorithm the joypad as indexed by the X register
    ;; (0 for controller 1; 1 for controller 2).
    .proc read_x
        jsr Joypad::unsafe_read_x

        ;; The main idea around a re-read algorithm is that you read the
        ;; controller "unsafely" once, then you do it again and compare both
        ;; reads. If they were the same then we are on the safe side. Otherwise
        ;; we would need to loop until we get two identical reads. This sounds
        ;; bad but in practice it's not so much (and hey, if it worked for Super
        ;; Mario Bros. 3, it should work for us too :P). Otherwise there is the
        ;; algorithm via OAM DMA, but it sure is tricky.
    @reread:
        lda Joypad::zp_buttons1, x
        tay
        jsr Joypad::unsafe_read_x
        tya
        cmp Joypad::zp_buttons1, x
        bne @reread

        rts
    .endproc

    ;;;
    ;; Read the joypad as indexed by the X register (0 for controller 1; 1 for
    ;; controller 2). This method is fast but it might be vulnerable to the DPCM
    ;; bug (see: https://www.nesdev.org/wiki/Controller_reading_code).
    .proc unsafe_read_x
        ;; Start the latch process.
        lda #$01
        sta Joypad::m_joypad1
        sta Joypad::zp_buttons1, x   ; Bit as a guard for the loop below.
        lsr
        sta Joypad::m_joypad1

        ;; Now the joypad is ready to accept reads.
    @loop:
        lda Joypad::m_joypad1, x
        and #%00000011              ; Ignore bits other than controller.
        cmp #$01                    ; Set carry if and only if nonzero.
        rol Joypad::zp_buttons1, x   ; Carry -> bit 0; bit 7 -> Carry
        bcc @loop
        rts
    .endproc
.endscope

;; Shortcut for reading the joypad from the first player safely.
.macro READ_JOYPAD1
    ldx #$00
    jsr Joypad::read_x
.endmacro

zp_count = $00
zp_up    = $01
zp_down  = $02
zp_a     = $03
zp_b     = $04

.proc reset
    lda #0
    sta zp_count
    sta zp_up
    sta zp_down
    sta zp_a
    sta zp_b

    READ_JOYPAD1
    lda #(Joypad::BUTTON_UP | Joypad::BUTTON_A)
    and Joypad::zp_buttons1
    beq :+
    inc zp_up
    inc zp_a
    inc zp_count
    inc zp_count

:
    READ_JOYPAD1
    lda #(Joypad::BUTTON_DOWN | Joypad::BUTTON_B)
    and Joypad::zp_buttons1
    beq :+
    inc zp_down
    inc zp_b
    inc zp_count
    inc zp_count

:
    rts
.endproc

.segment "VECTORS"
    .addr reset, reset, reset
