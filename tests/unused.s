.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $02
    .byte $01
    .byte $00
    .byte $00

.segment "CODE"

;;; Macros

.macro USED_MACRO UNUSED_ARG
    nop
.endmacro

.macro UNUSED_MACRO ARG1
    lda ARG1
.endmacro

USED_MACRO 1

;;; Variables

zp_unused  = $00
zp_used    = $01
zp_another = $02                ; check:ignore
zp_more    = $03
zp_yet     = $04

lda zp_used
lda (zp_more), y
lda #.lobyte(zp_yet)

;;; asan:stack full
