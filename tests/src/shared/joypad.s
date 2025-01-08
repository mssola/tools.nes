;; Taken from https://github.com/mssola/code.nes.

.scope Joypad
    ;; Button masks.
    BUTTON_A      = 1 << 7
    BUTTON_B      = 1 << 6
    BUTTON_SELECT = 1 << 5
    BUTTON_START  = 1 << 4
    BUTTON_UP     = 1 << 3
    BUTTON_DOWN   = 1 << 2
    BUTTON_LEFT   = 1 << 1
    BUTTON_RIGHT  = 1 << 0

    ;; Port addresses for controllers.
    JOYPAD1 = $4016
    JOYPAD2 = $4017

    ;; After running a `joypad_read_*` function these two variables will contain
    ;; the given result.
    m_buttons1 = $22
    m_buttons2 = $23
.endscope

;;;
;; Read the first joypad. This method is fast but it might be vulnerable to the
;; DPCM bug (see: https://www.nesdev.org/wiki/Controller_reading_code).
joypad_unsafe_read:
    ldx #$00
    ;; NOTE: fallthrough

;;;
;; Read the joypad as indexed by the X register (0 for controller 1; 1 for
;; controller 2). This method is fast but it might be vulnerable to the DPCM bug
;; (see: https://www.nesdev.org/wiki/Controller_reading_code).
joypad_unsafe_read_x:
    ;; Start the latch process.
    lda #$01
    sta Joypad::JOYPAD1
    sta Joypad::m_buttons1, x   ; Bit as a guard for the loop below.
    lsr a
    sta Joypad::JOYPAD1

    ;; Now the joypad is ready to accept reads.
@joypad_unsafe_read_x_loop:
    lda Joypad::JOYPAD1, x
    and #%00000011              ; Ignore bits other than controller.
    cmp #$01                    ; Set carry if and only if nonzero.
    rol Joypad::m_buttons1, x   ; Carry -> bit 0; bit 7 -> Carry
    bcc @joypad_unsafe_read_x_loop
    rts

;;;
;; Safely read the first controller via a re-read algorithm.
joypad_read:
    ldx #$00

    ;; NOTE: uncomment these two lines to also read safely the second
    ;; controller.
    ;;
    ;; jsr joypad_read_x
    ;; inx

    ;; NOTE: fallthrough

;;;
;; Safely read via a re-read algorithm the joypad as indexed by the X register
;; (0 for controller 1; 1 for controller 2).
joypad_read_x:
    jsr joypad_unsafe_read_x

    ;; The main idea around a re-read algorithm is that you read the controller
    ;; "unsafely" once, then you do it again and compare both reads. If they
    ;; were the same then we are on the safe side. Otherwise we would need to
    ;; loop until we get two identical reads. This sounds bad but in practice
    ;; it's not so much (and hey, if it worked for Super Mario Bros. 3, it
    ;; should work for us too :P). Otherwise there is the algorithm via OAM DMA,
    ;; but it sure is tricky.
@joypad_read_x_reread:
    lda Joypad::m_buttons1, x
    pha
    jsr joypad_unsafe_read_x
    pla
    cmp Joypad::m_buttons1, x
    bne @joypad_read_x_reread

    rts
