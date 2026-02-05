.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $02
    .byte $01
    .byte $00
    .byte $00

.segment "CODE"

.scope Scope
    zp_valid = $02

    .scope Inner
        zp_valid_too = $03
    .endscope
.endscope

zp_used    = $00                ; asan:reserve $02
whatever   = $01

lda zp_used
lda $00
lda whatever
lda Scope::zp_valid
lda Scope::Inner::zp_valid_too
lda zp_used + 1
lda zp_used + 2
lda zp_used - 1

@something:
    beq @something

ldx #0
lda palettes, x
lda palettes + 1, x

palettes:
    .byte $0F, $FF

lda $200                        ; asan:ignore
lda $200

jmp $8000
jmp ($8000)
