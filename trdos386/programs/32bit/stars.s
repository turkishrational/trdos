; STARS.S (NASM version of STARS.ASM)
;==============================================================================;
;                                                                              ;
;     TITLE: Star field                                                        ;
;WRITTEN BY: DRAEDEN                                                           ;
;      DATE: 03/15/93                                                          ;
;                                                                              ;
;     NOTES: Need 386 to execute.                                              ;
;                                                                              ;
;ASSOCIATED FILES:                                                             ;
;                                                                              ;
;       STARGEN.BAS =>  Basic program that generates a set of 'randomized'     ;  
;                       numbers.  Creates STARRND.DW                           ;
;                                                                              ;
;       STARS.TXT   =>  The text file that explains starfields...              ;
;                                                                              ;
;       STARRND.DW  =>  File that contains a set of shuffled numbers.          ;  
;                       Used to create 'random' star field.                    ;
;                                                                              ;
;==============================================================================;

; NASM version: Erdogan Tan, 02/10/2016

;=== CODE

[Bits 16]   ; Real Mode (MSDOS) Program

[org 100h]  ; MSDOS COM File

START:
    mov     ax,cs
    mov     ds,ax
    mov     es,ax

    ; clear BSS
    xor     ax, ax
    mov     di, bss_start
    mov     cx, (bss_end-bss_start)+1
    shr     cx, 1
    rep     stosw

    mov     ax,0013h                ;set vid mode 320x200x256 graph
    int     10h
    
    mov     dx,Palette
    mov     ax,1012h                ; WRITE palette 
    mov     bx,0                    
    mov     cx,256                  ;write entire palette
    int     10h                     ;doesn't matter if we didnt define it all

StarLoop:
    call    MakeStar        ;make stars 2x as thick
    call    MakeStar

    mov     dx,3dah
VRT:
    in      al,dx
    test    al,8
    jnz     short VRT       ;wait until Verticle Retrace starts

NoVRT:
    in      al,dx
    test    al,8
    jz      short NoVRT     ;wait until Verticle Retrace Ends

    call    DisplayStars

    mov     ah,1            ;check to see if a char is ready
    int     16h
    jz      short StarLoop  ;nope, continue
    
    mov     ah,0
    int     16h             ;get the character & put in AX

    cmp     al,"+"          ;compare ASCII part (al) to see what was pressed
    jne     short NotPlus

    inc     word [WarpSpeed]
    cmp     word [WarpSpeed],MaxWarp
    jbe     short StarLoop

    mov     word [WarpSpeed],MaxWarp
    jmp     short StarLoop

NotPlus:
    cmp     al,"-"
    jne     short NotMinus

    dec     word [WarpSpeed]
    cmp     word [WarpSpeed],0
    jge     short StarLoop

    mov     word [WarpSpeed],0
    Jmp     short StarLoop

NotMinus:

    mov     ax,0003h        ;set 80x25x16 char mode
    int     10h
    mov     ax,4c00h        ;return control to DOS
    int     21h

;=== Code Includes
;=== SUBROUTINES

    ;finds 1st available slot for a star and puts it there
MakeStar:
    pusha
    mov     ax,cs
    mov     es,ax
    mov     ds,ax

    cmp     word [NumActive],MaxStars   ;is there room for another star?
    jae     NoEmptySpace            

    ;search for 1st available slot

    mov     si, Stars
TryAgain:
    cmp     word [Stars.Z+si],0         ;is this slot empty?
    je      short GotOne                ;yes, go fill it

    add     si,StarStrucSize
    cmp     si,Stars+(MaxStars*StarStrucSize)
    jb      short TryAgain
    jmp     NoEmptySpace

GotOne:         ;si points to the record for the star to fill
    mov     di,[Yindex]         ;grab index for Ypos
    add     di,di               ;multiply by 2 to make it a WORD index
    mov     ax,[StarRnd+di]     ;get the number
    shl     ax,3                ;multiply by 8- could been done in BAS file
    mov     [Stars.Y+si],ax     ;and save off the number
    
    mov     di,[Xindex]         ;grab index for Xpos
    add     di,di               ;... same as above, but for Xpos
    mov     ax,[StarRnd+di]
    shl     ax,3
    mov     [Stars.X+si],ax

    mov     word [Stars.Z+si],MaxZpos ;reset Zpos to the max
    inc     word [NumActive]    ;we added a star so increase the counter

    mov     di,[Cindex]             ;grab the color index
    mov     al,[ColorChart+di]      ;grab the BaseColor for the star
    mov     [Stars.Color+si],al     ;save it in the record

    ;increase all the index pointers

    inc     word [Cindex]           ;increases the color counter
    cmp     word [Cindex],NumColors
    jb      short OkColor
    mov     word [Cindex],0
OkColor:
    inc     word [Yindex]           ;increases Yindex
    cmp     word [Yindex],NumRnds   ;note that for this one we
    jb      short YindNotZero       ; subtract NumRnds from Yindex if we
    sub     word [Yindex],NumRnds   ; go off the end of the chart
YindNotZero:
    inc     word [Xindex]           ;increase Xindex
    cmp     word [Xindex],NumRnds   ;have we gone through the entire chart?
    jb      short XindNotZero       ;nope...

;This clever bit of code makes more use out of the chart by increasing Yindex
; one additional unit each time Xindex goes through the entire chart... the
; result is nearly NumRND^2 random non-repeating points
        
    inc     word [Yindex]           ;yes, so change Yindex so that we get a
    mov     ax,[Yindex]             ;new set of random (x,y)
    cmp     ax,[Xindex]             ;does Xindex = Yindex?
    jne     short NotTheSame        ;if the index were the same, you'd see 
                                    ;a graph of the line Y = X, not good...
    inc     word [Yindex]           ;if they are the same, inc Yindex again
NotTheSame:
    mov     word [Xindex],0         ;reset Xindex to 0
XindNotZero:                        ;all done making the star...

NoEmptySpace:
    popa
    retn

DisplayStars:
    pusha
    mov     ax,cs
    mov     ds,ax
    mov     ax,0a000h
    mov     es,ax

    mov     si, Stars               ; si points to first record
DispLoop:
    mov     cx,[Stars.Z+si]
    or      cx,cx                   ;if Zpos = 0 then this star is dead...
    jz      short Cont              ;continue to the next one- skip this one

    mov     di,[Stars.OldDi+si]     ;grab old Di
    mov     byte [es:di],0          ;erase the star
    
    cmp     cx,MinZpos
    jl      short TermStar          ;if Zpos < MinZpos then kill the star

    mov     ax,[Stars.Y+si]
    movsx   dx,ah                   ;'multiply' Ypos by 256
    shl     ax,8
    
    idiv    cx                      ;and divide by Zpos
    add     ax,ScreenHeight/2       ;center it on the screen
    mov     di,ax
    cmp     di,ScreenHeight         ;see if the star is in range. 
    jae     PreTermStar             ; If not, kill it
    imul    di,ScreenWidth          ; DI = Y*ScreenWidth

    mov     ax,[Stars.X+si]
    movsx   dx,ah                   ;multiply Xpos by 256
    shl     ax,8

    idiv    cx                      ;and divide by Zpos
    add     ax,ScreenWidth/2        ;center it on the screen
    cmp     ax,ScreenWidth          ;are we inside the screen boundries?
    jae     short PreTermStar
    add     di,ax                   ; DI = Y * ScreenWidth + X

    mov     [Stars.OldDi+si],di     ;save old di

    ;calculate the color below

    add     ch,[Stars.Color+si]     ;i'm dividing cx (the zpos) by 256 and
                                    ; putting the result in ch and adding
                                    ; the base color to it in one instruction
    mov     [es:di],ch              ;put the dot on the screen

    mov     ax,[WarpSpeed]
    sub     [Stars.Z+si],ax         ;move the stars inward at WarpSpeed

Cont:
    add     si,StarStrucSize        ;point to next record
    cmp     si,Stars+(MaxStars*StarStrucSize)  ;are we done yet?
    jb      short DispLoop
    popa
    retn

PreTermStar:
    mov     word [Stars.Z+si],1 ;this is here so that the star will get erased
    jmp     short Cont      ;next time through if I just went off and killed
                            ;the star, it would leave a dot on the screen
TermStar:
    mov     word [Stars.Z+si],0 ;this actually kills the star, after it has
    dec     word [NumActive]  ;been erased
    jmp     short Cont

;=== GLOBALS
;=== Data Includes

;%include starrnd.dw      ;file that has label StarRnd numbers 

StarRnd: dw  166
dw   67, 102,  46,-173,-154,-210,-192, 173,-196, -81 
dw  -50,  36,  50,-200, -95, 209, -16,-179, -30,  18 
dw  174, 197, 127,  71,  29,-121,-160,-176,  19, -52 
dw -185,  89, 172,  74,-156, 157,-125, 144, -34,  69 
dw   17, -40,  64, -98,-153, 125, 160, 140,-204, 141 
dw  137,-165, -14, 154,-146, 119, 123, 165,-130, 168 
dw -180, 143,  52, 107,-107,-102,  57,  27, 117,  37 
dw  126,  15, -89, 184, 116, 183, -99,-139, 150, 188 
dw   38,  90,  93,-194, 207,-187,  62,  59, 196,  12 
dw -174,  54, 146,-137, 198, 162, 155,-163, -77,-144 
dw  191,-132, -43, 151,-103,  20, -46,  13,-140,  31 
dw  130,-169,-188, 109, -33,-150,-170,  68, -75,-201 
dw -100,-171, -19, -61,-206, 149,  99, -76,-186, -44 
dw -178,  34,  61,  28, 114, 199, 201, -83, -27,  63 
dw  -38, 204, 208,-112,-208, 122, -90,  23,-122, 161 
dw   35,-168, 170,-164,-151,  75, -60,-109,  85, 193 
dw   45,-175,-134, 205, -21,  49, 133, -85, -47, -37 
dw  -29, -96, -66,  73,-118, 147, -53, 120, 153,-155 
dw  -11,  11,  95, -26, 134,-145, -49, -74,  42,-124 
dw  189, -42,  92,-167,  88,-126,-129,-108,-193, 195 
dw  190,-106,-117, 203,  84, 139,-123, -94, -88,-158 
dw  181, -97, -20,  82, -57, 112, -35,  14, -56, -58 
dw  200,  80,-183, 106,  87,  30,  51, -28,  98, -12 
dw -191,-128, -13,-184, 136,  43,-166, -62, -73,-116 
dw  -31,-135,-101,  25,  41, -82, 110,  10, -45, -41 
dw   97, 175, 138, 171,  72,-133,-157,  58,-104, 187 
dw  192, -68, -87, 169,-110,  91, 129, 104, -70,-114 
dw -138,-115,-141, -67,-195, -79, -69,  40,-147, -80 
dw -119, 128, 152,-209,  83,  53, 159,  66,-190,  81 
dw  -92, -10,-181, 135,  60,  33, -25,  70,  22, -72 
dw  103, -23, 131,  79, -64,  55, -86, -32,-182,-136 
dw   26, -54,-172,-148, 148, -65,-152,-207, -39, -71 
dw   65, 179,-177,  24, 118, -59, -63,  44, 105, 206 
dw  178, -84,-202, 132, 186, -17,  76, 176, -22, 177 
dw -198,-159,-162,  78,  77, -55,-120,-203,-113, 156 
dw -189,-197, 124, 121,-142, -15,-205,  56, 158, -18 
dw  -93,-161,  39,  48, 101, -91, 182,-127, 108, 111 
dw  -36,-143,  21,-149, -78, -48, 164, 202, 185, 180 
dw  -51,-199, 100, 194,  32, -24, 142,  86,-111,  47 
dw  115,-105,  16, 167,  94, 163,  96, 113,-131, 145 

;=== DATA Structures
    
    ;Star_Struc      STRUC   
    ;    X       dw  0
    ;    Y       dw  0
    ;    Z       dw  0
    ;    OldDi   dw  0      ;where to erase last dot
    ;    Color   db  0      ;BASE color. a number 0-16 is added to it
    ;Star_Struc      ENDS

    ;StarStrucSize = 9     ;number of bytes per entry

;=== DATA

ScreenWidth EQU 320
ScreenHeight EQU 200

NumRnds     EQU 400     ;number of random numbers defined

MaxZpos     EQU 4096
MinZpos     EQU 2
MaxStars    EQU 190
NumColors   EQU 5       ;number of Base colors in the Color Chart

WarpSpeed:  dw  15      ;how quickly the stars move toward ya
MaxWarp     EQU 90

Xindex:     dw  30      ;index into the StarRnd chart for X & Y
Yindex:     dw  230     ; -note they must be different; set em the same to
                        ;see why
Cindex:     dw  0       ;index into ColorChart

ColorChart: db  0,16,32,48,64,80    ;a list of base colors (-1)

;Stars      Star_Struc MaxStars DUP (<>) ;where all the data is held
NumActive:  dw  0       ;number of stars active

Palette:    ;the palette.. first entrie is BG color (black)
    db 0,0,0
    db 2*15,3*15,4*15
    db 2*14,3*14,4*14
    db 2*13,3*13,4*13
    db 2*12,3*12,4*12
    db 2*11,3*11,4*11
    db 2*10,3*10,4*10
    db 2*9,3*9,4*9
    db 2*8,3*8,4*8
    db 2*7,3*7,4*7
    db 2*6,3*6,4*6
    db 2*5,3*5,4*5
    db 2*4,3*4,4*4
    db 2*3,3*3,4*3
    db 2*2,3*2,4*2
    db 2*1,3*1,4*1
    db 2*0,3*0,4*0
    db 2*15,2*15,4*15
    db 2*14,2*14,4*14
    db 2*13,2*13,4*13
    db 2*12,2*12,4*12
    db 2*11,2*11,4*11
    db 2*10,2*10,4*10
    db 2*9,2*9,4*9
    db 2*8,2*8,4*8
    db 2*7,2*7,4*7
    db 2*6,2*6,4*6
    db 2*5,2*5,4*5
    db 2*4,2*4,4*4
    db 2*3,2*3,4*3
    db 2*2,2*2,4*2
    db 2*1,2*1,4*1
    db 2*0,2*0,4*0
    db 3*15,3*15,4*15
    db 3*14,3*14,4*14
    db 3*13,3*13,4*13
    db 3*12,3*12,4*12
    db 3*11,3*11,4*11
    db 3*10,3*10,4*10
    db 3*9,3*9,4*9
    db 3*8,3*8,4*8
    db 3*7,3*7,4*7
    db 3*6,3*6,4*6
    db 3*5,3*5,4*5
    db 3*4,3*4,4*4
    db 3*3,3*3,4*3
    db 3*2,3*2,4*2
    db 3*1,3*1,4*1
    db 3*0,3*0,4*0
    db 3*15,2*15,4*15
    db 3*14,2*14,4*14
    db 3*13,2*13,4*13
    db 3*12,2*12,4*12
    db 3*11,2*11,4*11
    db 3*10,2*10,4*10
    db 3*9,2*9,4*9
    db 3*8,2*8,4*8
    db 3*7,2*7,4*7
    db 3*6,2*6,4*6
    db 3*5,2*5,4*5
    db 3*4,2*4,4*4
    db 3*3,2*3,4*3
    db 3*2,2*2,4*2
    db 3*1,2*1,4*1
    db 3*0,2*0,4*0
    db 3*15,3*15,3*15
    db 3*14,3*14,3*14
    db 3*13,3*13,3*13
    db 3*12,3*12,3*12
    db 3*11,3*11,3*11
    db 3*10,3*10,3*10
    db 3*9,3*9,3*9
    db 3*8,3*8,3*8
    db 3*7,3*7,3*7
    db 3*6,3*6,3*6
    db 3*5,3*5,3*5
    db 3*4,3*4,3*4
    db 3*3,3*3,3*3
    db 3*2,3*2,3*2
    db 3*1,3*1,3*1
    db 3*0,3*0,3*0
    db 2*15,4*15,3*15
    db 2*14,4*14,3*14
    db 2*13,4*13,3*13
    db 2*12,4*12,3*12
    db 2*11,4*11,3*11
    db 2*10,4*10,3*10
    db 2*9,4*9,3*9
    db 2*8,4*8,3*8
    db 2*7,4*7,3*7
    db 2*6,4*6,3*6
    db 2*5,4*5,3*5
    db 2*4,4*4,3*4
    db 2*3,4*3,3*3
    db 2*2,4*2,3*2
    db 2*1,4*1,3*1
    db 2*0,4*0,3*0

bss_start:

ABSOLUTE bss_start

alignb 2

Star_Struct:
     Stars.X   equ   0        ; X-position of star
     Stars.Y   equ   2        ; Y-position of star
     Stars.Z   equ   4        ; Z-position of star
     Stars.OldDi equ 6        ; Where to erase old star
     Stars.Color equ 8        ; Color of star

StarStrucSize equ 9    ; Number of bytes per entry ( 4 wordz and a byte )

Stars:
     resb StarStrucSize * MaxStars  ; Array of star-records

bss_end: