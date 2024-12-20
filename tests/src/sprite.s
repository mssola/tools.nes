;;; NOTE: taken from https://github.com/mssola/NES; which I will open source
;;; sooner rather than later.
;;;
;; Show a sprite to the screen! This example also contains fully detailed
;; explanations on each section on how an NES game is initialized and stored.

;;;
;; The iNES is the de facto standard for the distribution of NES binary programs
;; and it's compatible with the format used by NES cartridges themselves (used,
;; even, by the Wii Virtual Console). The layout is composed by segments in
;; memory, which are defined with the `.segment` macro and known by the compiler
;; through a linker configuration. You can provide a configuration of your own,
;; but bear in mind that compilers like `cc65` (the one used here, which is the
;; most common) already provide a default configuration for the linker that
;; glues a set of pretty common defined named segments. You can read about this
;; in `cfg/nes.cfg` from inside your cc65 installation. Otherwise, I have also
;; written linker configuration files for other examples: take a look, for
;; instance, at the `config/unrom.cfg` file, which is used by the
;; `basics/unrom.s` program.
;;;

;;;
;; The "HEADER" is the first segment of any iNES binary and it contains basic
;; information about what this "cartridge" requires in order to work. Besides
;; basic identification, it defines some relevant things like PRG and CHR sizes,
;; plus mapping if desired. See https://www.nesdev.org/wiki/NES_2.0#Header for
;; documentation on this, or also: https://www.nesdev.org/neshdr20.txt.
;;;
.segment "HEADER"
    ;; The first thing to do is to define the magic "NES\0" string identifier
    ;; ($1A is the ASCII that MS-DOS wanted as end of string). Some people write
    ;; it in hexadecimal through and through, but cc65 already compiles
    ;; characters into their proper hexadecimal values, so there's no need to be
    ;; extra cryptic here.
    .byte 'N', 'E', 'S', $1A

    ;; The next two bytes define the size of the PRG and CHR ROMs in this order.
    ;; Hence, the next two bytes define a 32KB (2 x 16KB) of PRG-ROM, and 8KB
    ;; (1 x 8KB) of CHR-ROM.
    .byte $02
    .byte $01

    ;; Next we have two bytes for selecting a mapper. This is a huge topic (see
    ;; the NesDev wiki for this), but it basically refers to the fact that some
    ;; cartridges had specific requirements on how to place their data, or the
    ;; amount of it they required, or how they expected their data to be
    ;; mirrored in the memory, etc. During the life-time of the NES, and as
    ;; developers pushed the boundaries of the NES hardware, more intricate
    ;; mappers were used on the hardware of cartridges themselves. Thus, in
    ;; these two bytes we are telling the emulator: "hey, act as if this was a
    ;; cartridge that used this kind of mapper".
    .byte $00                   ; Horizontal mirroring (good for vertical scrollers)
    .byte $00                   ; No mapper nor special-case flags.

    ;; The previous are the mandatory bytes in order to get a "cartridge" going.
    ;; After this there are some other bytes you can put into the header, like
    ;; specifying the region (NTSC vs PAL), but for now this falls out of my
    ;; radar :-)

;;;
;; This is the segment where we tell the processor where to find the code for
;; three important topics: the Non-Maskable Interrupts handler, the Reset
;; handler, and the IRQ handler. If you look at the configuration from `cc65`
;; that was provided on your installation (or if you are pesky enough to create
;; one yourself), you will notice that these vector addresses are placed at the
;; very end of memory ($fffa-ffff). The NES (and emulators) will look at these
;; three last positions in memory to know where to jump for each case.
;;;
.segment "VECTORS"
    .addr nmi
    .addr reset
    .addr irq

.segment "CODE"

;;;
;; The Reset handler is executed when the whole thing starts (i.e. the user has
;; pressed either the reset or the power on buttons on the NES). Thus, this
;; piece of code is pretty standard and an implementation is even given in the
;; NesDev wiki which is pretty much followed by everyone as I could see. The
;; main idea is to leave the hardware in a known state and then jump into the
;; main game subroutine.
;;;
reset:
    ;; We first instruct the NES to disable everything. That is, we don't want
    ;; any pesky interrupt to make us jump into the `nmi` section, for example,
    ;; before we have configured everything.

    ;; Ignore IRQs and disable decimal mode (the NES 6502 chip, for copyright
    ;; issues or legal reasons against MOS that I don't fully know nor care,
    ;; does not have decimal mode anyway, but it's considered good practice).
    sei
    cld

    ;; Disable APU frame IRQ. This is the first instance we see of Memory-Mapped
    ;; I/O. This is a core concept in NES programming and, to sum things up, the
    ;; memory range $2000-$6000 is reserved to I/O operations, and each address
    ;; is reserved to a specific hardware operation. This is because the NES CPU
    ;; doesn't directly control the PPU nor other chips. In this case, ranges
    ;; $4000-$4017 control the APU (Audio Processing Unit). More precisely, the
    ;; $4017 address controls what is called the "Frame counter" from the APU
    ;; (https://www.nesdev.org/wiki/APU#Frame_Counter_($4017)). Setting #$40 to
    ;; it disables it completely, so we are in a known state. If we were to use
    ;; sound, at the end of the reset code we should enable it back. We do *not*
    ;; do it here because we don't need it.
    ldx #$40
    stx $4017                   ; APU Frame Counter

    ;; Set up the stack register with the proper value (the stack will grow in
    ;; decreasing order from $01FF -> $0100).
    ldx #$ff
    txs

    ;; And now disable, in this order, NMI, rendering and DMC IRQs. Note that
    ;; `x` was set to $ff, so increasing it by one results in a zero, which is
    ;; the value then stored in the aforementioned memory locations.
    ;;
    ;; The one on $4010 refers again to the APU (as described before), and it
    ;; directly controls the DMC. Again, if you wanted sound, you should enable
    ;; this back after the whole reset block.
    ;;
    ;; On the other side, the low addresses of $2000 control the PPU. In
    ;; particular, we disable NMIs from the PPU by setting the PPUCTRL address
    ;; ($2000) to zero, and we do the same for the PPUMASK ($2001). Don't worry
    ;; about them just now, we will go deeper down below.
    inx
    stx $2000                   ; PPUCTRL
    stx $2001                   ; PPUMASK
    stx $4010                   ; APU DMC

    ;;;
    ;; NOTE: If you are using a mapper which needs some special configuration,
    ;; now it would be a good time set it up. I am not using a special mapper,
    ;; so there's nothing from me to do here.
    ;;;

    ;; At this point, we have to wait for the PPU to stabilize. This is
    ;; typically done by checking a flag from the PPUSTATUS address ($2002) and
    ;; waiting until the proper value is set by the PPU. Since this wait can
    ;; take a while, programmers typically put other initialization code here,
    ;; like sprite resetting and such.
    ;;
    ;; The PPUSTATUS memory address contains general information on the status
    ;; of the PPU, and is read-only. Moreover, reading from the PPUSTATUS has a
    ;; convenient side-effect: it resets the "address latch" for PPUADDR. We
    ;; will see how we can take advantage of this when we use the PPU properly
    ;; down the road.

    ;; First of the two waits.
@vblankwait1:
    bit $2002                   ; PPUSTATUS
    bpl @vblankwait1

    ;; The PPU has at least started, now we have a bunch of cycles for it to
    ;; stabilize, which will be properly announced through the PPUSTATUS memory
    ;; address. Instead of just waiting for it again, we will take the chance to
    ;; initialize more stuff.

    ;; One typical thing to do is to leave the RAM in a known state. That is, we
    ;; will set to 0 addresses $0000 - $07FF. Apparently there are some people
    ;; who say that doing this is bad because it will hide programming mistakes
    ;; (e.g. bad initialization code). So, if you are one of these people, you
    ;; can safely remove this loop. Otherwise let's get the RAM clean. That
    ;; being said, notice that we are skipping $200-$2ff. This is no mistake as
    ;; you will see below.
    ldx #0
    lda #0
@ram_reset_loop:
    sta $000, x
    sta $100, x
    sta $300, x
    sta $400, x
    sta $500, x
    sta $600, x
    sta $700, x
    inx
    bne @ram_reset_loop         ; if x overflows back to #00, then we are done.

    ;; And now another thing we can do is to reset the sprites. We will reserve
    ;; space for sprites in $0200-02ff (the range we did not initialize when
    ;; cleaning up RAM). With this in mind we will perform a DMA process right
    ;; after this block of code that will take this memory range and bulk it
    ;; into PPU OAM space.
    ;;
    ;; "Resetting sprites" is just a matter of giving them a value which will
    ;; not bother us in the future. One way to do this is to set each value to
    ;; $ef, which will give each "sprite" off-screen Y-coordinates. How any of
    ;; this is the case will be shown whenever we deal with loading proper
    ;; sprites below.
    lda #$ef
@sprite_reset_loop:
    sta $200, x
    inx
    bne @sprite_reset_loop

    ;; And write these resetted "sprites" into the PPU. How does this work?
    ;; Well, address $2003 has the OAMADDR. That is, from which position the PPU
    ;; should start the DMA process. In our case we set it to the very
    ;; beginning. Then, if you write to the OAMDMA memory address ($4014), you
    ;; will instruct the PPU to start a DMA process starting at $1XX*N. XX is
    ;; the byte given at the OAMADDR, thus XX = #$00; and N = 2 (see `lda
    ;; #$02`). Therefore, we are telling the PPU to start the DMA process from
    ;; $200. The PPU will assume that the following 256 bytes of memory are the
    ;; ones to be copied, resulting in a DMA copy of $200-$2ff, right where we
    ;; stored the sprite data in advance.
    lda #$00
    sta $2003                   ; OAMADDR
    lda #$02
    sta $4014                   ; OAMDMA

    ;; As advertised, the other wait. Once we are out of this loop, we are 100%
    ;; guaranteed to have a properly working PPU which is ready to render stuff
    ;; into the screen.
@vblankwait2:
    bit $2002                   ; PPUSTATUS
    bpl @vblankwait2

    ;; And we also reset the palettes. How is this done? Well, the $2006 memory
    ;; address is the PPU address (PPUADDR). This address is given byte by byte,
    ;; the most significat byte first. Thus, the four lines below store into
    ;; $2006 the value $3F00. This is the first address where palettes are
    ;; stored. Palettes are the answer from old systems like the NES to: how can
    ;; you display this amount of colors on screen without taking too much
    ;; memory? The NES allows developers to store eight palettes (four
    ;; background, four foreground), and each palette group four colors. This
    ;; way, whenever we want to draw a sprite or a piece of background, we don't
    ;; specify which colors to pick for each pixel, but we rather apply a
    ;; palette to a sprite or background tile definition.
    lda #$3F
    sta $2006                   ; PPUADDR
    lda #$00
    sta $2006                   ; PPUADDR

    ;; After setting the address for the first palette, now we loop #$20 times:
    ;; 8 palettes * 4 bytes per palette (1 byte per color) = 32 (#$20
    ;; hexadecimal). At each iteration we will write into $2007 the value #$0F.
    ;; This memory address is the PPUDATA, and #$0F corresponds to a black
    ;; color. In each iteration, therefore, we are telling the PPU that we want
    ;; a black color for all the colors from all palettes. Moreover, whenever
    ;; you write into PPUDATA, the memory address for the next store is
    ;; incremented by one automatically. This is why we don't have to index the
    ;; write or anything like that, but we can simply write to the same memory
    ;; address 32 times and it will be performed onto 32 consecutive positions.
    lda #$0F
    ldx #$20
@palettes_reset_loop:
    sta $2007                   ; PPUDATA
    dex
    bne @palettes_reset_loop

    ;; At this point everything is clear and with a state we know, now we can
    ;; jump into our main subroutine and start loading sprites, palettes, etc.;
    ;; and start the game proper.
    jmp main

;;;
;; This is our main subroutine. At this point we can assume that the hardware
;; has already been set to a proper and defined state. So now we can load all
;; the data we need for our game and enter the main game loop.
;;;
.proc main
    ;; Before starting the game loop proper we initialize all our assets: load
    ;; the palettes, nametables and sprites for this game.
    jsr init_palettes
    jsr init_nametable
    jsr init_sprites

    ;; Remember when we disabled rendering and NMI on the reset code? Now it's
    ;; time to enable them back. Things to note:
    ;;   - PPUCTRL: we enable three bits:
    ;;     - 4: background pattern table starts at $1000. The CHR is divided by
    ;;          two pattern tables, and each table contains 256 8x8 tiles. In my
    ;;          case, my sprite data is located at the first pattern table (thus
    ;;          $0000, and that's why the 3rd bit from PPUCTRL is set to zero),
    ;;          and the background data is on the second pattern table (that
    ;;          starting at $1000). In the end this all depends on how you want
    ;;          to structure your CHR file.
    ;;     - 5: sprite size is 8x16. Set this to 0 if your sprites in the
    ;;          character file are 8x8.
    ;;     - 7: allow a NMI at the start of the vertical blanking interval.
    ;;          That is, whenever the screen has displayed one frame, send us
    ;;          an interrupt so we can generate the next one (see the code on
    ;;          `nmi`).
    ;;   - PPUMASK: we enable 4 bits:
    ;;     - 1: Show background in leftmost pixels on the screen.
    ;;     - 2: Show sprites in leftmost pixels on the screen.
    ;;     - 3: Show background.
    ;;     - 4: Show sprites.
    ;;
    ;; As for showing on the leftmost pixels, this might not be a good idea if
    ;; the game is a scroller, since the game might flicker depending on how you
    ;; manage the camera. This is out of scope.
    cli
    lda #%10110000
    sta $2000                   ; PPUCTRL
    lda #%00011110
    sta $2001                   ; PPUMASK

@main_game_loop:
    ;;;
    ;; NOTE: enter here your game loop logic.
    ;;;

    ;; This is a hand-shake between the code on `main` and the code on the
    ;; `nmi`. That is, whenever we are done with the game loop logic, we set
    ;; this flag to indicate that the rendering is doable. After this, the code
    ;; on `main` will be blocked until this flag is unset by the `nmi` code,
    ;; which will indicate that it can proceed with another iteration of the
    ;; game loop. On the contrary, the `nmi` code will be blocked until this
    ;; flag is set, at which point it will start rendering and unset the flag
    ;; whenever that is done.
    lda #%10000000
    ora $20
    sta $20
@wait_for_render:
    bit $20
    bmi @wait_for_render

    ;; Rendering is done, we can perform another iteration of the loop!
    jmp @main_game_loop
.endproc

;; init_palettes copies all the palettes for our game into the proper PPU
;; address.
.proc init_palettes
    ;; Remember these four instructions? That's what we also did when resetting
    ;; palettes on the reset code. That is, we are preparing the PPU to write
    ;; data starting from $3f00.
    lda #$3F
    sta $2006                   ; PPUADDR
    lda #$00
    sta $2006                   ; PPUADDR

    ;; We are going to copy the eight palettes stored in the `palettes` tag into
    ;; PPU. This is done by indexing with the `x` register, which is incremented
    ;; until it reaches #$20 (32 in hexadecimal), which corresponds to 8
    ;; palettes * 4 bytes each (just like we did in the reset code).
    ldx #0
@load_palettes_loop:
    lda palettes, x
    sta $2007                   ; PPUDATA
    inx
    cpx #$20
    bne @load_palettes_loop
    rts
palettes:
    ;; Let there be palettes! This is the data for all the palettes we have for
    ;; our game. Each hexadecimal value is a byte that represents a color (you
    ;; can use YY-CHR to quickly take a look at which color represents each
    ;; hexadecimal value, or use any of the tables that can be found online, or
    ;; even fceux has this with its "Palette editor" under "Tools"). There is,
    ;; though, one big catch: the first color from all palettes *must* be the
    ;; same. To be more precise, it's not like the NES will explode if you pick
    ;; other colors, but the NES *will* assume that the first color from all
    ;; palettes is the one found in $3F00 (that is, the first color from the
    ;; first palette). In this case, $0F will be this "default color". Moreover,
    ;; for this simple game we are not using all palettes, and that's why for
    ;; some of them all four bytes are simply zero'ed.

    ;; Background
    .byte $0F, $12, $22, $32
    .byte $0F, $00, $28, $30
    .byte $0F, $28, $16, $2D
    .byte $0F, $28, $16, $2D

    ;; Foreground
    .byte $0F, $00, $05, $30
    .byte $0F, $00, $00, $00
    .byte $0F, $00, $00, $00
    .byte $0F, $00, $00, $00
.endproc

;; WRITE_PPU_DATA is a macro that will write into PPUADDR the given address and
;; into PPUDATA the given byte value.
.macro WRITE_PPU_DATA address, value
    bit $2002                   ; PPUSTATUS
    lda #.HIBYTE(address)
    sta $2006                   ; PPUADDR
    lda #.LOBYTE(address)
    sta $2006                   ; PPUADDR
    lda #value
    sta $2007                   ; PPUDATA
.endmacro

;; init_nametable loads the relevant data to the nametable that is then going to
;; be used in order to build up the background.
;;
;; NOTE: this function is called after the PPU has been initialized, but NMIs
;; are still disabled. This is important because you **cannot** write into PPU
;; data outside of VBlank space (during `nmi` code). If you do so it will result
;; into rendering glitches. This is better explained at the `scroll` examples,
;; where VRAM buffering techniques are applied and explained.
.proc init_nametable
    ;; The general idea here is that the background defaults to the
    ;; "transparent" color, which is the first color on the palette. Then for
    ;; the background we only need to store into the PPU those elements which
    ;; are not the default thing. Hence, if we want to draw a couple of
    ;; background elements in our game in some positions, we have to instruct
    ;; the PPU where to put each item and where to find it on the pattern table.
    ;; After that, we can change/toggle the palette for a background tile if we
    ;; so want it, and we are done with it.

    ;; Anyways, before loading data into the PPU, we always have to check the
    ;; PPUSTATUS, that's why the first instruction of each block is a `bit
    ;; $2002`. This is a safe-guard so to reset the "address latch". That is, if
    ;; instead of writing two consecutive bytes we only wrote one by mistake (or
    ;; because we only wanted to write the high byte), it resets back its
    ;; internal index.

    bit $2002                   ; PPUSTATUS

    ;; Load the first item for the background! Let's make sense of the values.
    ;;
    ;; We first need to set the address that the PPU will use (that is, which
    ;; couple of bytes we need to pass to PPUADDR). That is: which tile index is
    ;; to be used from the nametable (read more about PPU Nametables here:
    ;; https://www.nesdev.org/wiki/PPU_nametables). Think of this as the screen
    ;; being set as a grid of 8x8 pixels (tile), and each of these tiles is
    ;; indexed in PPU memory: 960 bytes of actual data, and 64 bytes for the
    ;; attribute table (see more on attribute tables below). You can draw this
    ;; by hand by using resources like Morhpcat's developer spreadsheet
    ;; (https://ko-fi.com/s/ad5d7601e0), based on the ones being used at
    ;; Nintendo. This is good but there are also handy tools that make things
    ;; easier for us. For example the NEXXT tool allows users to draw a full NES
    ;; screen and then it gives the offset address that you can use for each
    ;; drawn element. I did this and I got that the offset for the first element
    ;; was $0C8, which added to the base address of $2000 (start of the first
    ;; nametable), gave me the address $20C8. If you want to make more sense of
    ;; it by just reading the number, notice that each row has exactly 32 tiles.
    ;; Thus, you can divide 0x0C8 by 32 and you will get 6.25: that is, on the
    ;; vertical axis it's the row '6'. Then subtract 0xC8 by 192 (6 rows of 32
    ;; tiles) to get '8' as the value on the horizontal axis. As you can see,
    ;; doing things manually can be tedious, that's why people either use a dev
    ;; spreadsheet or a computer assisted tool.
    ;;
    ;; Anyways, if I want this background element to be rendered in the position
    ;; I envisioned on this tool, I need to write $20C8 into PPUADDR, which adds
    ;; for a total of two bytes to be loaded for operations that only support
    ;; one byte at a time. Hence, we have to load byte by byte (in little-endian
    ;; format) and store them. As for the PPUDATA address, I need to pass #$02
    ;; because that's the index inside of the CHR file of the star in the second
    ;; pattern table (check the PPUCTRL setting at the end of the `main`
    ;; function on why it's the second pattern table). You can check that by
    ;; opening the `.chr` file for this example on YY-CHR.
    lda #$20
    sta $2006                   ; PPUADDR
    lda #$C8
    sta $2006                   ; PPUADDR
    lda #$02
    sta $2007                   ; PPUDATA

    ;; Now we have to do the same for the rest of the elements. I have gone
    ;; through the same process of fetching the offset from a canvas I drew on
    ;; NEXXT and here's the result. Instead of repeating te same code over and
    ;; over but with slightly different values, I am using this macro I created
    ;; to do the same.
    WRITE_PPU_DATA $20B9, $04
    WRITE_PPU_DATA $21CE, $04
    WRITE_PPU_DATA $21BA, $04
    WRITE_PPU_DATA $22B8, $04
    WRITE_PPU_DATA $22E7, $04
    WRITE_PPU_DATA $227A, $02

    ;; Now everything we wanted to write for the background is there, but we
    ;; might want to toggle the colors for some of the elements. You can do this
    ;; with palettes, which for background tiles is done via attribute tables.
    ;; Each attribute table lives right after each nametable, in which you can
    ;; establish the palettes to be used for the tiles appearing on the
    ;; background screen as defined on that specific nametable. Information on
    ;; this is also given to us by tools like NEXXT. Shortly, for one of the
    ;; elements I want to change their palette I was told that the "attribute
    ;; offset" was $03CE. Again, added to the base address, it means that we
    ;; need to pass $23CE to the PPUADDR. The value for PPUDATA is a bit more
    ;; tricky: on the attribute table each 8x8 tile is broken down into 4
    ;; 2-sized squares. Each of these 4 squares of the tile has a palette
    ;; assigned to it ($00 by default). Thanks to this setup, a single byte can
    ;; encode four palettes by addressing each square: %44332211. The tool also
    ;; tells us in which of these smaller squares our element resides. With this
    ;; info, and since we have two bits available for each of these smaller
    ;; squares and, therefore, can encode up to 4 palettes, it means that we
    ;; just have to assign either on the range of 00-11 to the pair of bits
    ;; representing the smaller square that NEXXT is telling us that our
    ;; background element resides in. In this case it's the first square, so we
    ;; just need to assign $01 to the bits reserved to this square if I want to
    ;; change the palette to 1. This is what we pass as a value.
    WRITE_PPU_DATA $23CE, %00000001

    rts
.endproc

;; init_sprites loads all the sprites we want from our game.
.proc init_sprites
    ;; This is tied to the number of sprites stored in the `initial_sprite_data`
    ;; section. Remember that this can be 64 *maximum*: 64 sprites * 4 bytes per
    ;; sprite = 256 bytes; which is the reserved space in memory we have for
    ;; sprites: $0200-$02ff.
    NUM_SPRITES = 2

    ;; The loading is quite straight-forward. We just store whatever is on
    ;; initial_sprite_data + x into $0200 + x, where x is our index that gets
    ;; increased on each iteration. We will stop the loop whenever x reaches
    ;; NUM_SPRITES * 4 (each sprite takes 4 bytes, more below).
    ldx #$00
@load_sprites_loop:
    lda initial_sprite_data, x
    sta $0200, x
    inx
    cpx #(4 * NUM_SPRITES)
    bne @load_sprites_loop
    rts
initial_sprite_data:
    ;; Each sprite consists of 4 bytes:
    ;;  1. The Y position.
    ;;  2. The sprite position on the pattern table in hexadecimal. Make sure to
    ;;     get this position right by using YY-CHR (or whatever program you are
    ;;     using to manipulate the CHR file). You can get this wrong also
    ;;     depending if you are using 8x8 or 8x16 tiles mode (check the bit set
    ;;     on PPUCTRL on the `main` code).
    ;;  3. The attributes for the sprite. The first two bits control which
    ;;     palette is used to draw the sprite (in our case palette $00). Bits 2
    ;;     to 4 are not used. Bit 5 sets the sprite behind the background if set
    ;;     to 1; bit 6 flips the sprite horizontally; and bit 7 flips the sprite
    ;;     vertically.
    ;;  4. The X position.
    ;;
    ;; In our case, notice that we are using the same sprite (located at $00),
    ;; but the second sprite is moved eight pixels right and flipped
    ;; horizontally. This is because the sprite located at $00 is just half of
    ;; the real "meta-sprite", and so we can build the entire "meta-sprite" by
    ;; simply flipping the same part horizontally. This is one of the many
    ;; tricks we can use to save space on CHR-ROM.
    .byte $B0, $00, %00000000, $7A
    .byte $B0, $00, %01000000, $82
.endproc

;;;
;; Non-Maskable Interrupts handler. After being enabled by the initialization
;; code, you can count on this code being called at the end of each PPU
;; rendering frame during the Vertical Blanking Interval (VBLANK). This time
;; span is pretty tight (around 2273 CPU cycles), so you better update
;; everything to be rendered before that if you don't want graphical glitches.
;;
;; NOTE: NMI code can be more complex as we want to do more complex stuff, but
;; we ought to keep things as simple and fast as possible. You can take a look
;; at the examples from `scroll` for more complex NMI code that have to handle
;; stuff like VRAM buffering or setting other PPU registers.
;;;
nmi:
    ;; As mentioned on the `main` subroutine, rendering will be skipped until
    ;; the proper flag is set.
    bit $20
    bpl @next

    ;; An NMI can happen at any time. Hopefully whenever that happens we are
    ;; already done with the main code, so replacing the current value of
    ;; registers isn't that big of a deal, but it's considered good practice to
    ;; not assume that (e.g. a particular frame being too laggy). Because of
    ;; this, we backup registers now and we restore them at the end.
    pha
    txa
    pha
    tya
    pha

    ;; We are instructed that we can start rendering stuff. Transfer the sprites
    ;; via OAM. This is the same we did when we resetted sprites in our `reset`
    ;; code.
    lda #$00
    sta $2003                   ; OAMADDR
    lda #$02
    sta $4014                   ; OAMDMA

    ;; Reset the scroll. This is needed because we have touched the PPUADDR in
    ;; multiple places. Touching the PPUADDR memory address will also toggle the
    ;; PPUSCROLL one because they share a register on hardware. Because of this,
    ;; we always need to reset the scroll back to the coordinates we want, and
    ;; we do it right here, when everything has already been sent and we are
    ;; done.
    bit $2002                   ; PPUSTATUS
    lda #$00
    sta $2005                   ; PPUSCROLL
    sta $2005                   ; PPUSCROLL

    ;; And unset the render flag so the `main` code is unblocked.
    lda #%01111111
    and $20
    sta $20

    ;; Restore registers.
    pla
    tay
    pla
    tax
    pla
@next:
    rti

;;;
;; Interrupt Requests handler. This is triggered by the NES' sound processor
;; (APU) or by some specific cartridge hardware (e.g. something specific to a
;; mapper). In our case we don't have to do anything here, so we just return
;; from the interrupt.
;;;
irq:
    rti

;;;
;; Include into this all the data that needs to go into the CHR ROM. One typical
;; implementation for this is by using the `.incbin` macro, which will blindly
;; copy the bitmap that you have generated through a program such as YY-CHR into
;; the CHR ROM. Note that not all games used the CHR-ROM for storing their
;; assets and instead used the PRG-ROM for that as well (e.g. The Legend of
;; Zelda). Take a look at the `basics/chr-ram.s` example for more. There are
;; multiple technical reasons to do this, but this falls out of the scope of
;; this example.
;;;
.segment "CHARS"
    .incbin "../assets/basic.chr"
