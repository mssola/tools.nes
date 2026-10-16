.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $10, $10
    .byte $43
    .byte $08
    .byte $00
    .byte $00
    .byte $70
    .res $05, $00

.segment "TAIL"
;;; asan:stack full

;;; TODO: into another segment, writing to WRAM clc; adc wr_whatever; sta wr_whatever; this is then called via different segments

.scope PPU
    m_status  = $2002
    m_address = $2006
.endscope

.scope MMC3
    m_bank_select = $8000
    m_bank_data   = $8001
    m_mirroring   = $A000
    m_ram_protect = $A001
    m_irq_disable = $E000
.endscope

.macro BANK_REGISTER_SET REGISTER_ID, REGISTER_VALUE
    lda #REGISTER_ID
    sta MMC3::m_bank_select
    lda #REGISTER_VALUE
    sta MMC3::m_bank_data
.endmacro

.proc regular_mmc3_init
    ;; Init scanline counter.
    bit PPU::m_status

    lda #$00
    sta PPU::m_address
    sta PPU::m_address
    lda #$10
    sta PPU::m_address
    sta PPU::m_address

    lda #$00
    sta PPU::m_address
    sta PPU::m_address
    lda #$10
    sta PPU::m_address
    sta PPU::m_address

    ;; Setup mirroring.
    lda #$00
    sta MMC3::m_mirroring

    ;; Disable interrupts.
    sta MMC3::m_irq_disable

    ;; Enable PRG RAM and allow writes to it.
    lda #$80
    sta MMC3::m_ram_protect

    ;; PRG banking setup.
    BANK_REGISTER_SET 6, 0
    BANK_REGISTER_SET 7, 1

    rts
.endproc

.proc basic
    jsr regular_mmc3_init

    jsr foo                     ; asan:safe

    BANK_REGISTER_SET 6, 1

    ;; NOTE: this is actually not safe, as it will call another_foo() instead :)
    jsr foo                     ; asan:safe

    ;; NOTE: calling a different address but it's the same function as per the
    ;; previous mapping :)
    jsr another_foo             ; asan:safe

    rts
.endproc

.proc no_writes_allowed
    jsr regular_mmc3_init

    lda #$C0
    sta MMC3::m_ram_protect

    jmp foo                     ; asan:safe
.endproc

.proc no_wram
    jsr regular_mmc3_init

    lda #$00
    sta MMC3::m_ram_protect

    jmp foo                     ; asan:safe
.endproc

.proc reset
    ;; NOTE: avoid warnings from unused functions.
    jsr basic
    jsr no_writes_allowed
    jsr no_wram

    rts
.endproc

.proc nmi
    rti
.endproc

.proc irq
    rti
.endproc

.segment "PRG0_00"

wr_value = $6000

.proc foo
    clc
    adc wr_value
    sta wr_value
    rts
.endproc

.segment "PRG0_01"

.proc another_foo
    inc wr_value
    rts
.endproc

.segment "CHARS"
.res $F000, $00
.res $F000, $00
.res $2000, $00

.segment "VECTORS"
    .addr nmi, reset, irq

.segment "PRG0_00"
.byte $00

.segment "PRG0_01"
.byte $00

.segment "PRG0_02"
.byte $00

.segment "PRG0_03"
.byte $00

.segment "PRG0_04"
.byte $00

.segment "PRG0_05"
.byte $00

.segment "PRG0_06"
.byte $00

.segment "PRG0_07"
.byte $00

.segment "PRG0_08"
.byte $00

.segment "PRG0_09"
.byte $00

.segment "PRG0_0A"
.byte $00

.segment "PRG0_0B"
.byte $00

.segment "PRG0_0C"
.byte $00

.segment "PRG0_0D"
.byte $00

.segment "PRG0_0E"
.byte $00

.segment "PRG1_00"
.byte $00

.segment "PRG1_01"
.byte $00

.segment "PRG1_02"
.byte $00

.segment "PRG1_03"
.byte $00

.segment "PRG1_04"
.byte $00

.segment "PRG1_05"
.byte $00

.segment "PRG1_06"
.byte $00

.segment "PRG1_07"
.byte $00

.segment "PRG1_08"
.byte $00

.segment "PRG1_09"
.byte $00

.segment "PRG1_0A"
.byte $00

.segment "PRG1_0B"
.byte $00

.segment "PRG1_0C"
.byte $00

.segment "PRG1_0D"
.byte $00

.segment "PRG1_0E"
.byte $00

.segment "FIXED"
.byte $00
