;compile to this filename
!to "project1.prg",cbm

;address of the screen buffer and colour RAM

SSP = $cc00
SSC = $d805
Sprite1Colour = 2
Sprite2Colour = 2


; PARAM

PARAM1                  = $03
PARAM2                  = $04
PARAM3                  = $05
PARAM4                  = $06
PARAM5                  = $07

;Sprite Pointers

SPRITE_POINTER_BASE = SSP + 1016

;Number of Sprites divided by four

S4D = 1

;Sprite number constants

SPRITE_BASE = 64
SPRITE_PLAYER = SPRITE_BASE - 1 

;zero page pointer

ZEROPAGE_POINTER_1 = $17
ZEROPAGE_POINTER_2 = $19
ZEROPAGE_POINTER_3 = $21
ZEROPAGE_POINTER_4 = $23

;level data constants
LD_END                  = 0
LD_LINE_H               = 1
LD_LINE_V               = 2


;this creates a basic start
*=$0801

          ;SYS 2064
          !byte $0C,$08,$0A,$00,$9E,$20,$32,$30,$36,$34,$00,$00,$00,$00,$00

          ;init sprite registers
          ;no visible sprites
          lda #$0
          sta $d000 + $15
          
          ;set charset
          lda #$3c
          sta $d018

          ;VIC bank
          lda $dd00
          and #$fc
          sta $dd00
          
   

  ;----------------------
          ;copy charset to target          
          ;----------------------
          
          ;block interrupts 
          ;since we turn ROMs off this would result in crashes if we didn't
          sei
          
          ;save old configuration
          lda $01
          sta $03
          
          ;only RAM
          ;to copy under the IO rom
          lda #%00110000
          sta $01
          
          ;take source address from CHARSET
          LDA #<CHARSET
          STA ZEROPAGE_POINTER_1
          LDA #>CHARSET
          STA ZEROPAGE_POINTER_1 + 1
          
          ;now copy
          jsr CopyCharSet

 ;take source address from SPRITES
          
          lda #<SPRITES
          sta ZEROPAGE_POINTER_1
          lda #>SPRITES
          sta ZEROPAGE_POINTER_1 + 1
          
          jsr CopySprites

          ;restore ROMs
          lda $03
          sta $01
          
                 
          cli
         
                  
          ;test charset
          lda #'"'
          
          sta SSP
           

 ;set up level
          lda #0
          sta LEVEL_NR
          jsr BuildScreen
          

  ;initialize sprite 1 pos          
          lda #100
          sta $d000
          sta $d000 + 1
          sta SPRITE_POS_X
          sta SPRITE_POS_Y
          ;set sprite image
          lda #SPRITE_PLAYER
          sta SPRITE_POINTER_BASE
          
          ;Enable 1st Sprite
          
          lda #SPRITE_PLAYER + 1
          sta SPRITE_POINTER_BASE 
          ;enable sprite 1
          lda #1
          sta 53248 + 21
          
       
          
          
          LDA Sprite1Colour
          STA $d027 
         
          
         
          ;main game loop
Gameloop
          ;recolour background
          
          lda #3
          sta $d020
          lda $d020
          sta $d021
          
        
          
          jsr WaitFrame
          
          jsr PlayerControl
          
          
          
          jmp Gameloop    
          
          
             
;------------------------------------------------------------
;check joystick (player control)
;------------------------------------------------------------
!zone PlayerControl
PlayerControl
          lda #$02
          bit $dc00
          bne .NotDownPressed
          jsr PlayerMoveDown
          
.NotDownPressed          
          lda #$01
          bit $dc00
          bne .NotUpPressed
          jsr PlayerMoveUp
          
.NotUpPressed          
          lda #$04
          bit $dc00
          bne .NotLeftPressed
          jsr PlayerMoveLeft
          
.NotLeftPressed
          lda #$08
          bit $dc00
          bne .NotRightPressed
          jsr PlayerMoveRight

.NotRightPressed
          rts

PlayerMoveLeft
          ldx #0
          jsr MoveSpriteLeft
          rts
          
PlayerMoveRight
          ldx #0
          jsr MoveSpriteRight
          rts

PlayerMoveUp
          ldx #0
          jsr MoveSpriteUp
          rts
          
PlayerMoveDown
          ldx #0
          jsr MoveSpriteDown
          rts

;------------------------------------------------------------
;Move Sprite Left
;expect x as sprite index (0 to 7)
;------------------------------------------------------------
!zone MoveSpriteLeft
MoveSpriteLeft
          dec SPRITE_POS_X,x
          bpl .NoChangeInExtendedFlag
          
          lda BIT_TABLE,x
          eor #$ff
          and SPRITE_POS_X_EXTEND
          sta SPRITE_POS_X_EXTEND
          sta 53248 + 16
          
.NoChangeInExtendedFlag     
          txa
          asl
          tay
          
          lda SPRITE_POS_X,x
          sta 53248,y
          rts  

;------------------------------------------------------------
;Move Sprite Right
;expect x as sprite index (0 to 7)
;------------------------------------------------------------
!zone MoveSpriteRight
MoveSpriteRight
          inc SPRITE_POS_X,x
          lda SPRITE_POS_X,x
          bne .NoChangeInExtendedFlag
          
          lda BIT_TABLE,x
          ora SPRITE_POS_X_EXTEND
          sta SPRITE_POS_X_EXTEND
          sta 53248 + 16
          
.NoChangeInExtendedFlag     
          txa
          asl
          tay
          
          lda SPRITE_POS_X,x
          sta 53248,y
          rts  

;------------------------------------------------------------
;Move Sprite Up
;expect x as sprite index (0 to 7)
;------------------------------------------------------------
!zone MoveSpriteUp
MoveSpriteUp
          dec SPRITE_POS_Y,x
          
          txa
          asl
          tay
          
          lda SPRITE_POS_Y,x
          sta 53249,y
          rts  

;------------------------------------------------------------
;Move Sprite Down
;expect x as sprite index (0 to 7)
;------------------------------------------------------------
!zone MoveSpriteDown
MoveSpriteDown
          inc SPRITE_POS_Y,x
          
          txa
          asl
          tay
          
          lda SPRITE_POS_Y,x
          sta 53249,y
          rts  
          
          ;------------------------------------------------------------
;BuildScreen
;creates a screen from level data
;------------------------------------------------------------
!zone BuildScreen
BuildScreen
          lda #0
          ldy #6
          jsr ClearPlayScreen

          ;get pointer to real level data from table
          ldx LEVEL_NR
          lda SCREEN_DATA_TABLE,x
          sta ZEROPAGE_POINTER_1
          lda SCREEN_DATA_TABLE + 1,x
          sta ZEROPAGE_POINTER_1 + 1
          
          jsr .BuildLevel

          ;get pointer to real level data from table
          lda #<LEVEL_BORDER_DATA
          sta ZEROPAGE_POINTER_1
          lda #>LEVEL_BORDER_DATA
          sta ZEROPAGE_POINTER_1 + 1
          
          jsr .BuildLevel
          rts
          
.BuildLevel
          ;work through data
          ldy #255
          
.LevelDataLoop
          iny
          
          lda (ZEROPAGE_POINTER_1),y
          cmp #LD_END
          beq .LevelComplete
          cmp #LD_LINE_H
          beq .LineH
          cmp #LD_LINE_V
          beq .LineV
          
.LevelComplete          
          rts
          
.NextLevelData
          pla
          
          ;adjust pointers so we're able to access more 
          ;than 256 bytes of level data
          clc
          adc #1
          adc ZEROPAGE_POINTER_1
          sta ZEROPAGE_POINTER_1
          lda ZEROPAGE_POINTER_1 + 1
          adc #0
          sta ZEROPAGE_POINTER_1 + 1
          ldy #255
          
          jmp .LevelDataLoop

.LineH
          ;X pos
          iny
          lda (ZEROPAGE_POINTER_1),y
          sta PARAM1 
          
          ;Y pos
          iny
          lda (ZEROPAGE_POINTER_1),y
          sta PARAM2

          ;width
          iny
          lda (ZEROPAGE_POINTER_1),y
          sta PARAM3
          
          ;char
          iny
          lda (ZEROPAGE_POINTER_1),y
          sta PARAM4
          
          ;color
          iny
          lda (ZEROPAGE_POINTER_1),y
          sta PARAM5
          
          ;store target pointers to screen and color ram
          ldx PARAM2
          lda SCREEN_LINE_OFFSET_TABLE_LO,x
          sta ZEROPAGE_POINTER_2
          sta ZEROPAGE_POINTER_3
          lda SCREEN_LINE_OFFSET_TABLE_HI,x
          sta ZEROPAGE_POINTER_2 + 1
          clc
          adc #( ( SSC - SSP ) & 0xff00 ) >> 8
          sta ZEROPAGE_POINTER_3 + 1
          
          tya
          pha
          
          ldy PARAM1
.NextChar          
          lda PARAM4
          sta (ZEROPAGE_POINTER_2),y
          lda PARAM5
          sta (ZEROPAGE_POINTER_3),y
          iny
          dec PARAM3
          bne .NextChar
          
          jmp .NextLevelData
          
.LineV
          ;X pos
          iny
          lda (ZEROPAGE_POINTER_1),y
          sta PARAM1 
          
          ;Y pos
          iny
          lda (ZEROPAGE_POINTER_1),y
          sta PARAM2

          ;height
          iny
          lda (ZEROPAGE_POINTER_1),y
          sta PARAM3
          
          ;char
          iny
          lda (ZEROPAGE_POINTER_1),y
          sta PARAM4
          
          ;color
          iny
          lda (ZEROPAGE_POINTER_1),y
          sta PARAM5
          
          ;store target pointers to screen and color ram
          ldx PARAM2
          lda SCREEN_LINE_OFFSET_TABLE_LO,x
          sta ZEROPAGE_POINTER_2
          sta ZEROPAGE_POINTER_3
          lda SCREEN_LINE_OFFSET_TABLE_HI,x
          sta ZEROPAGE_POINTER_2 + 1
          clc
          adc #( ( SSC - SSP ) & 0xff00 ) >> 8
          sta ZEROPAGE_POINTER_3 + 1
          
          tya
          pha
          
          ldy PARAM1
.NextCharV         
          lda PARAM4
          sta (ZEROPAGE_POINTER_2),y
          lda PARAM5
          sta (ZEROPAGE_POINTER_3),y
          
          ;adjust pointer
          lda ZEROPAGE_POINTER_2
          clc
          adc #40
          sta ZEROPAGE_POINTER_2
          sta ZEROPAGE_POINTER_3
          lda ZEROPAGE_POINTER_2 + 1
          adc #0
          sta ZEROPAGE_POINTER_2 + 1
          clc
          adc #( ( SSC - SSP ) & 0xff00 ) >> 8
          sta ZEROPAGE_POINTER_3 + 1
          
          dec PARAM3
          bne .NextCharV
          
          jmp .NextLevelData
          
          
          
!zone WaitFrame
          ;wait for the raster to reach line $f8
          ;this is keeping our timing stable
          
          ;are we on line $F8 already? if so, wait for the next full screen
          ;prevents mistimings if called too fast
WaitFrame 
          lda $d012
          cmp #$F8
          beq WaitFrame

          ;wait for the raster to reach line $f8 (should be closer to the start of this line this way)
.WaitStep2
          lda $d012
          cmp #$F8
          bne .WaitStep2
          
          rts
          
       ;------------------------------------------------------------
;clears the play area of the screen
;A = char
;Y = color
;------------------------------------------------------------

!zone ClearPlayScreen
ClearPlayScreen
            ldx #$00
.ClearLoop          
            sta SSP,x
            sta SSP + 220,x
            sta SSP + 440,x
            sta SSP + 660,x
            inx
            cpx #220
            bne .ClearLoop
            
            tya
            ldx #$00
.ColorLoop          
            sta $d800,x
            sta $d800 + 220,x
            sta $d800 + 440,x
            sta $d800 + 660,x
            inx
            cpx #220
            bne .ColorLoop
            
            rts   

!zone CopyCharSet
CopyCharSet
          ;set target address ($F000)
          lda #$00
          sta ZEROPAGE_POINTER_2
          lda #$F0
          sta ZEROPAGE_POINTER_2 + 1

          ldx #$00
          ldy #$00
          lda #0
          sta $04

.NextLine
          lda (ZEROPAGE_POINTER_1),Y
          sta (ZEROPAGE_POINTER_2),Y
          inx
          iny
          cpx #$08
          bne .NextLine
          cpy #$00
          bne .PageBoundaryNotReached
          
          ;we've reached the next 256 bytes, inc high byte
          inc ZEROPAGE_POINTER_1 + 1
          inc ZEROPAGE_POINTER_2 + 1

.PageBoundaryNotReached

          ;only copy 254 chars to keep irq vectors intact
          inc $04
          lda $04
          cmp #254
          beq .CopyCharsetDone
          ldx #$00
          jmp .NextLine

.CopyCharsetDone
          rts

;------------------------------------------------------------
;copies sprites from ZEROPAGE_POINTER_1 to ZEROPAGE_POINTER_2
;       sprites are copied in numbers of four
;------------------------------------------------------------
!zone CopySprites
CopySprites
ldy #$00
          ldx #$00
          
          lda #00
          sta ZEROPAGE_POINTER_2
          lda #$d0
          sta ZEROPAGE_POINTER_2 + 1
          
          ;4 sprites per loop
.SpriteLoop
          lda (ZEROPAGE_POINTER_1),y
          sta (ZEROPAGE_POINTER_2),y
          iny
          bne .SpriteLoop
          inx
          inc ZEROPAGE_POINTER_1+1
          inc ZEROPAGE_POINTER_2+1
          cpx #S4D
          bne .SpriteLoop

          rts

;------------------------------------------------------------
;screen data
;------------------------------------------------------------
SCREEN_DATA_TABLE
          !word LEVEL_1
          !word 0
          
          
LEVEL_1
          !byte LD_LINE_H,5,5,10,128,9
          !byte LD_LINE_H,30,12,9,129,9
          !byte LD_LINE_H,10,19,20,128,9
          !byte LD_LINE_V,7,6,4,128,9
          !byte LD_END


LEVEL_BORDER_DATA
          !byte LD_LINE_H,0,0,40,129,9
          !byte LD_LINE_H,1,22,38,129,9
          !byte LD_LINE_V,0,1,22,128,9
          !byte LD_LINE_V,39,1,22,128,9
          !byte LD_END




;Game variables

LEVEL_NR  
          !byte 0

SPRITE_POS_X
          !byte 0,0,0,0,0,0,0,0
SPRITE_POS_X_EXTEND
          !byte 0
SPRITE_POS_Y
          !byte 0,0,0,0,0,0,0,0
BIT_TABLE
          !byte 1,2,4,8,16,32,64,128

SCREEN_LINE_OFFSET_TABLE_LO
          !byte ( SSP +   0 ) & 0x00ff
          !byte ( SSP +  40 ) & 0x00ff
          !byte ( SSP +  80 ) & 0x00ff
          !byte ( SSP + 120 ) & 0x00ff
          !byte ( SSP + 160 ) & 0x00ff
          !byte ( SSP + 200 ) & 0x00ff
          !byte ( SSP + 240 ) & 0x00ff
          !byte ( SSP + 280 ) & 0x00ff
          !byte ( SSP + 320 ) & 0x00ff
          !byte ( SSP + 360 ) & 0x00ff
          !byte ( SSP + 400 ) & 0x00ff
          !byte ( SSP + 440 ) & 0x00ff
          !byte ( SSP + 480 ) & 0x00ff
          !byte ( SSP + 520 ) & 0x00ff
          !byte ( SSP + 560 ) & 0x00ff
          !byte ( SSP + 600 ) & 0x00ff
          !byte ( SSP + 640 ) & 0x00ff
          !byte ( SSP + 680 ) & 0x00ff
          !byte ( SSP + 720 ) & 0x00ff
          !byte ( SSP + 760 ) & 0x00ff
          !byte ( SSP + 800 ) & 0x00ff
          !byte ( SSP + 840 ) & 0x00ff
          !byte ( SSP + 880 ) & 0x00ff
          !byte ( SSP + 920 ) & 0x00ff
          !byte ( SSP + 960 ) & 0x00ff
SCREEN_LINE_OFFSET_TABLE_HI
          !byte ( ( SSP +   0 ) & 0xff00 ) >> 8
          !byte ( ( SSP +  40 ) & 0xff00 ) >> 8
          !byte ( ( SSP +  80 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 120 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 160 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 200 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 240 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 280 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 320 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 360 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 400 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 440 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 480 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 520 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 560 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 600 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 640 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 680 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 720 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 760 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 800 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 840 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 880 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 920 ) & 0xff00 ) >> 8
          !byte ( ( SSP + 960 ) & 0xff00 ) >> 8

CHARSET
          !BIN "chars.raw"

SPRITES   !BIN "CatHi.Spr"

MESSAGE   !TEXT "Hello"

 