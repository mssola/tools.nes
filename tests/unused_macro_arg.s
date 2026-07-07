.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $02, $01
    .byte $00
    .byte $00

.segment "CODE"

;;; asan:stack full

.macro BCD_ADD ADDR
    adc ADDR
    bcc :+
    nop
:
.endmacro

.proc add_to_player_y
    BCD_ADD additions

    rts

additions:
    .byte $FF
.endproc

jsr add_to_player_y
