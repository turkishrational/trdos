; BOARDZ.S (NASM version of BOARDZ.ASM)
;==============================================================================;
;                                                                              ;
;       Assembler program by Vulture.                                          ;
;       This program scrolls a text and displays a 3d-starfield.               ;
;       It's a BBS advertisement.                                              ;
;                                                                              ;
;       Current Date: 13-2-95         Vulture                                  ;
;                                                                              ;
;==============================================================================;

; NASM version: Erdogan Tan, 01/10/2016

; === MAIN PROGRAM ===

[Bits 16]   ; Real Mode (MSDOS) Program

[org 100h]  ; MSDOS COM File

START:
    cli                         ; Clear interrupt flag
    call    SetVGA              ; Get in GFX-mode

; === Set the palette ===
    mov     ax,cs               ; Move CS into AX
    mov     ds,ax
    mov     es,ax               ; es points to codesegment
    ;mov    ss,ax

    ; clear BSS
    xor     ax, ax
    mov     di, bss_start
    mov     cx, (bss_end-bss_start)+1
    shr     cx, 1
    rep     stosw

    mov     ax,1012h            ; Select write palette function
    mov     bx,0                ; Start at color 0
    mov     cx,23               ; Write 23 colors
    mov     dx,Palette   	; es:dx points to palette data
    int     10h                 ; Call VID interrupt & set palette
    call    SavePalette         ; And save palette into array

; === Initialize pointers ===
    mov     ax,0a000h
    mov     es,ax               ; es points to VGA
    mov     ax,cs
    mov     ds,ax               ; ds points to codesegment (data)

; === Start scroll ===
Reset:
    lea     si, [Text]          ; si points to start of Text
Mainthing:                      ; Main loop
    lodsb                       ; Load a character in al    (si increased)
    cmp     al,0                ; Have we reached the end of da text ?
    je      short Reset         ; Yep => Start over
    push    si                  ; Save character-offset on stack
    mov     bx,ax               ; Save the character into bx
    mov     cx,0                ; Set character-counter for position in font
    lea     si, [Order]         ; si points to offset Order
Again:
    lodsb                       ; Load the order-character (abcdef etc...)
    cmp     ax,bx               ; Is it da same letter/character ?
    je      short Found         ; Yeah => Found it. . .
    inc     cx                  ; Nope => Increase character-counter
    jmp     short Again         ; Compare with next character
Found:
    mov     ax,8                ; 7 pixels + black pixel = 8 pixels
    mul     cx                  ; ax=ax*cx    (e.g:  E := 4 * 10;)

    mov     bx,4                ; Draw 4 * 2 vertical lines
Hloop:
    lea     si, [Font]          ; si points to start of Font
    add     si,ax               ; si now points to character
    mov     di,320*91           ; di points to startposition on VGA
    mov     cx,9                ; Write 9 horizontal
Vloop:
    push    cx                  ; Save first loop-counter
    mov     cx,2                ; Draw 2 new horizontal pixels
    repz    movsb               ; And go !
    add     di,318              ; Point to next location on VGA
    add     si,318              ; Point to next source-location
    pop     cx                  ; Restore loop-counter
    loop    Vloop               ; Loop 9 times
; === Scroll the text and improve stars ===
    push    ax                  ; Save pointer to character
    mov     ax,0a000h           ; VGA-segment
    mov     ds,ax               ; ds points to VGA
    mov     si,320*90+2         ; Destination offset
    mov     di,320*90           ; Source offset
    mov     cx,10*320           ; Repeat factor => Number of bytes to copy
    rep     movsb               ; And go ! (Hint: why not use words instead)

    mov     di,320*101          ; On some slow VGA-cards we have to plot
    mov     cx,5                ; 5 black pixels just below the scroller
    mov     al,0                ; on the left on the screen. Erase this
    rep     stosb               ; code to see what I mean.

    mov     ax, cs
    mov     ds, ax
	
    call    WaitVrt             ; Wait for vertical retrace
    call    CalcStar            ; Calculate new stars
    call    ShowStars           ; Show all stars on VGA

; === Want to quit ? ===
    ;in     al,60h              ; Was ESCAPE pressed ?
    ;cmp    al,1
    ;je     QuitNow             ; If so, quit now. . .
    mov	    ah, 1
    int     16h
    jnz     short check_CR_key

; === No quit ? then continue ===
    pop     ax                  ; Restore pointer to character
    add     ax,2                ; And add 2 to point to next 2 vertical lines
    dec     bx                  ; Decrease line-counter
    jnz     short Hloop         ; If it's 0 then jump
    pop     si                  ; Restore character-offset to do next char
    jmp     short Mainthing     ; And start over again

check_CR_key:
    pop     ax
    pop     si

    xor	    ah, ah
    int     16h
    cmp     al, 13  ; Carriage Return (ENTER) Key ?
    je	    short QuitNow
    mov	    byte [NewMessage], '$' ; Hide the new (2016) message !

QuitNow:                        ; Quit everything
    call    FadeOut             ; Fade da screen to black
    call    SetText             ; Get in TXT-mode
   
    ; ds points to codesegment (data)
    lea     dx, [Message]       ; Load offset message
    mov     ah,9                ; Select function 9 (print string)
    int     21h                 ; Print the message
    mov     ax,4c00h            ; Quit program
    int     21h

; === PROCEDURES ===

SetVGA:				; Get into VGA mode
    mov     ax,0013h            ; Set the videomode 320*200*256
    int     10h                 ; Call VID interrupt
    retn

SetText:			; Get into character mode
    mov     ax,0003h            ; Set 80x25x16 char mode
    int     10h                 ; Call VID interrupt
    retn

WaitVrt:			; Waits for vertical retrace to reduce "snow"
    mov     dx,3dah
Vrt:
    in      al,dx
    test    al,8
    jnz     short Vrt		; Wait until Verticle Retrace starts
NoVrt:
    in      al,dx
    test    al,8
    jz      short NoVrt		; Wait until Verticle Retrace ends
    retn                        ; Return to main program

SavePalette:			; Saves entire palette in array
    cli                         ; Disable interrupts
    mov     bp, PaletteArray    ; Point to start of array
    mov     cx,768              ; Save all R,G,B registers
    mov     dx,03c7h            ; Read register
    mov     al,0                ; Start at 0
    out     dx,al               ; Write to port
    mov     dx,03c9h            ; Read data register
Grab:
    in      al,dx               ; Read value from port
    and     al,3fh              ; Mask of upper 2 bits
    mov     byte [bp],al        ; Store the value in array
    inc     bp                  ; Point to the next one
    loop    Grab                ; Loop until cx = 0
    sti                         ; Enable interrupts
    retn                        ; Return to main program

FadeOut:			; Fades screen to black
    cli                         ; Disable interrupts
    mov     bp,PaletteArray     ; Point to start of array
    mov     cx,64               ; Repeat 64 times (0..63)
OneCycle:
    mov     bx,0                ; Set counter
Decrease:
    cmp     byte [bp],0         ; Is it 0 already ?
    je      short Fading        ; Yep => Do the next
    dec     byte [bp]           ; Nope => Decrease by one
Fading:
    inc     bp                  ; Point to next value
    inc     bx                  ; Increase counter
    cmp     bx,768              ; Have we reached the end ?
    jl      short Decrease      ; No => Do another one
    push    cx                  ; Save 1st loop counter
    call    WaitVrt             ; Wait for retrace
    sub     bp,768              ; Point to start
    mov     bx,0                ; Reset counter
    mov     cx,768              ; Do all colors
    mov     dx,03c8h            ; Write register
    mov     al,0                ; Start at 0
    out     dx,al               ; Write to port
    inc     dx                  ; Writing => 03c8h + 1 = 03c9h
WriteAll:
    mov     al,byte [bp]    ; Store value in al
    out     dx,al               ; Give it to the VGA
    inc     bp                  ; Point to next one
    inc     bx                  ; Increase counter
    loop    WriteAll            ; Loop while cx > 0
    pop     cx                  ; Restore 1st loop counter
    sub     bp,768              ; Point to start
    loop    OneCycle            ; Loop while cx > 0
    sti                         ; Enable interrupts
    retn                        ; Return to main program

CalcStar:
    pusha                               ; Put all registers on stack
    mov     si, Stars                   ; si points to first star
StartCalc:                              ; Start searching for empty slots
    cmp     word [NumActive], MaxStars  ; Check for room
    jae     short NoEmptySpace		; No room => exit

SearchSlot:
    cmp     word [Stars_Z+si],MinZ  ; If Z = 0 then slot is empty
    je      short FillSlot

    add     si,StarStrucSize            ; si points to next star
    cmp     si,Stars+(StarStrucSize*MaxStars) ; Have we done entire array ?
    jb      short SearchSlot		; No => search again
    jmp     short NoEmptySpace		; Yes => exit

FillSlot:
    mov     di,[XIndex]                 ; Grab Xindex and put it in di
    add     di,di                       ; Make WORD index
    mov     ax,[Numbers+di]             ; Get the number
    shl     ax,3                        ; Multiply by 8   
    mov     [Stars_X+si],ax             ; Save the number

    mov     di,[YIndex]                 ; Do the same for Y
    add     di,di
    mov     ax,[Numbers+di]
    shl     ax,3
    mov     [Stars_Y+si],ax

    mov     word [Stars_Z+si],MaxZ      ; Give star the Z offset
    mov     al,0                        ; Also give it basecolor 0 (black)   
    mov     [Stars_Col+si],al           ; Store the color

    inc     word [NumActive]            ; Increase star counter

    inc     word [XIndex]               ; Increase the X index
    cmp     word [XIndex],MaxNumbers    ; Have we reached the end of the list?
    jb      short XindNotMax            ; No => continue
    mov     word [XIndex],0             ; Yes => go to start of list

XindNotMax:
    inc     word [YIndex]               ; Increase the Y index   
    cmp     word [YIndex],MaxNumbers    ; Have we reached the end of the list?
    jb      short StartCalc             ; No => continue
    mov     word [YIndex],0             ; Yes => go to start of list
    
NoEmptySpace:
    popa                                ; Restore all registers
    retn                                ; Return to main program   

ShowStars:
    pusha                        ; Save all registers
    mov     si, Stars            ; si points to first record
ShowLoop:
    mov     cx,[Stars_Z+si]      ; Grab Z value of star
    cmp     cx,0                 ; If Z = 0 then exit
    je      short ContinueStar   ; Do the next star

    mov     di,[Stars_Old+si]    ; Get old position of star
    mov     byte [es:di],0       ; Erase the old star

    mov     ax,[Stars_X+si]      ; Grab X value of star
    mov     dx,256               ; Multiply X with 256
    imul    dx
    idiv    cx                   ; Divide by Z
    add     ax,ScreenWidth/2     ; Add 160 to center it on the screen
    mov     di,ax                ; di = X
    cmp     di,ScreenWidth       ; Is the star in range ?
    jae     short TermStar       ; No => Do next star

    mov     ax,[Stars_Y+si]      ; Grab an Y value
    mov     dx,256               ; Multiply Y with 256
    imul    dx
    idiv    cx                   ; Divide by Z (a bit slow but who carez)
    add     ax,ScreenHeight/2    ; Add 100 to center it on the screen
    cmp     ax,ScreenHeight      ; Is the star in range ?
    jae     short TermStar       ; No => Do next star
    cmp     ax,90                ; Do not affect scroller
    jl      short InRange
    cmp     ax,100               ; Text scrolls between 90 & 100
    jna     short TermStar       ; Star affects scroller so terminate it

InRange:
    imul    ax,ScreenWidth       ; ax = Y * ScreenWidth
    add     di,ax                ; di = X + (Y * 320) 

    mov     [Stars_Old+si],di    ; Save the position

    add     ch, [Stars_Col+si]   ; Divide Z by 256 & add basecolor 0
    mov     al,ch                ; Move color into al
    add     al,5                 ; Add 5 to avoid fontcolors

    mov     byte [es:di],al  	 ; Place the dot on the screen

    mov     ax,[WarpSpeed]
    sub     cx,ax                ; Decrease Z with WarpSpeed
    mov     [Stars_Z+si],cx      ; Save the new Z

    jmp     short ContinueStar   ; Do the next star

TermStar:
    mov     word [Stars_Z+si],MinZ ; Set Z to 0 => Star is terminated
    dec     word [NumActive]     ; Decrease number of active stars

ContinueStar:
    add     si,StarStrucSize     ; si points to next record
    cmp     si,Stars+(StarStrucSize*MaxStars) ; Reached end of array ?
    jb      short ShowLoop       ; Continue with next star

    popa                         ; Restore all registers
    retn                         ; Return to main program

; === DATA ===   

;%include 'FONT.DAT'    ; File with font data
;%include 'NUMBERS.DAT' ; Include 500 random numbers between -200 and 200

;Message: DB   13,10,"Code by Vulture.",13,10,"$"  ; Important message  :)
Message: DB   13,10,"Code by Vulture.",13,10
NewMessage: DB "Reprogrammed by Erdogan Tan (via NASM), 01/10/2016." 
         DB 13, 10, "$"

Text:   DB 'if u wanna experience some cewl boardz in the netherlands '
        DB 'call firehouse 058-661590    detonator 05111-4307   or   '
        DB 'mark of cain 058-672111      cu around. . . . . .'
        DB '                            ',0           ; Text to scroll

Order:  DB 'abcdefghijklmnopqrstuvwxyz0123456789-. '  ; Order of characters

align 2

Font:
db 0,2,2,2,2,2,0,0,2,2,2,2,2,2,0,0,0,2,2,2
db 2,2,0,0,2,2,2,2,2,2,0,0,2,2,2,2,2,2,2,0
db 2,2,2,2,2,2,3,0,0,2,2,2,2,2,0,0,2,2,0,0
db 0,2,2,0,2,2,2,2,2,3,3,0,0,0,0,0,0,2,3,0
db 2,2,0,0,0,2,3,0,2,2,0,0,0,0,0,0,2,2,3,0
db 2,2,2,0,2,2,2,2,2,2,0,0,0,2,2,2,2,2,0,0
db 2,2,2,2,2,2,0,0,0,2,2,2,2,2,0,0,2,2,2,2
db 2,2,0,0,0,2,2,2,2,2,2,0,2,2,2,2,2,3,3,0
db 2,2,0,0,0,2,2,0,2,3,0,0,0,2,2,0,2,2,0,0
db 0,2,2,0,2,2,0,0,0,2,2,0,2,2,0,0,0,2,2,0
db 2,2,2,2,2,2,2,0,0,0,2,2,2,2,2,0,0,0,0,0
db 2,2,0,0,0,2,2,2,2,2,2,0,0,2,2,2,2,2,2,0
db 0,2,2,0,0,0,2,2,0,2,2,2,2,2,2,2,0,0,2,2
db 2,2,2,0,0,2,2,2,2,2,2,0,0,0,2,2,2,2,2,0
db 0,0,2,2,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0
db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
db 2,2,0,0,0,2,2,0,2,2,0,0,0,2,2,0,2,2,0,0
db 0,2,3,0,2,2,0,0,3,2,3,0,2,2,0,0,0,0,0,0
db 2,2,0,0,0,0,0,0,2,2,0,0,0,2,3,0,2,2,0,0
db 0,2,3,0,0,0,2,2,0,0,0,0,0,0,0,0,0,2,3,0
db 2,2,0,0,2,3,0,0,2,2,0,0,0,0,0,0,2,2,2,2
db 3,2,3,0,2,3,0,0,3,2,3,0,2,2,0,0,0,2,3,0
db 2,2,0,0,0,2,2,0,2,2,0,0,0,2,2,0,2,2,0,0
db 0,2,2,0,2,2,0,0,0,0,0,0,0,0,2,2,0,0,0,0
db 2,3,0,0,0,2,3,0,2,3,0,0,0,2,3,0,2,3,0,0
db 0,2,3,0,3,2,3,0,3,2,3,0,2,2,0,0,0,2,3,0
db 0,0,0,0,0,2,2,0,0,2,2,0,0,0,2,3,0,0,0,0
db 2,2,0,0,0,0,0,0,0,0,2,2,0,0,0,0,0,0,2,2
db 0,2,3,0,0,0,2,3,0,2,2,0,0,0,0,0,0,2,2,0
db 0,0,2,3,0,0,0,0,0,3,2,3,0,2,2,0,0,0,2,3
db 0,2,2,0,0,0,2,3,0,0,0,0,0,0,0,0,0,0,0,0
db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
db 2,2,0,0,0,2,2,0,2,2,0,0,0,2,2,0,2,3,0,0
db 0,0,0,0,2,3,0,0,0,2,3,0,2,3,0,0,0,0,0,0
db 2,3,0,0,0,0,0,0,2,3,0,0,0,0,0,0,2,3,0,0
db 0,2,3,0,0,0,2,3,0,0,0,0,0,0,0,0,0,2,3,0
db 2,3,0,2,3,0,0,0,2,3,0,0,0,0,0,0,2,3,0,3
db 0,2,3,0,2,3,0,0,0,2,3,0,2,3,0,0,0,2,3,0
db 2,2,0,0,0,2,3,0,2,3,0,0,0,2,3,0,2,2,0,0
db 0,2,3,0,2,3,0,0,0,0,0,0,0,0,2,3,0,0,0,0
db 2,3,0,0,0,2,3,0,2,2,0,0,0,2,3,0,2,3,0,0
db 0,2,3,0,0,3,2,3,2,3,0,0,2,2,3,0,2,2,3,0
db 0,0,0,0,3,2,3,0,0,2,3,0,0,2,2,3,0,0,0,2
db 2,3,0,0,0,0,0,0,0,0,2,2,0,0,0,0,0,0,2,2
db 0,2,3,0,0,0,2,3,0,2,2,0,0,0,0,0,0,2,3,0
db 0,0,0,0,0,0,0,0,0,0,2,3,0,2,3,0,0,0,2,3
db 0,2,3,0,0,0,2,3,0,0,0,0,0,0,0,0,0,0,0,0
db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
db 2,2,2,3,3,2,2,0,2,2,2,3,3,2,0,0,2,3,0,0
db 0,0,0,0,2,3,0,0,0,2,3,0,2,3,2,2,2,3,0,0
db 2,3,2,2,2,3,0,0,2,3,0,0,2,2,2,0,2,3,2,2
db 2,2,3,0,0,0,2,3,0,0,0,0,0,0,0,0,0,2,3,0
db 2,3,2,2,0,0,0,0,2,3,0,0,0,0,0,0,2,3,0,0
db 0,2,3,0,2,3,0,0,0,2,3,0,2,3,0,0,0,2,3,0
db 2,2,2,3,2,3,0,0,2,3,0,0,0,2,3,0,2,2,2,3
db 2,3,0,0,0,2,2,2,2,2,0,0,0,0,2,3,0,0,0,0
db 2,3,0,0,0,2,3,0,3,2,3,0,2,2,3,0,2,3,0,2
db 0,2,3,0,0,0,3,2,3,0,0,0,0,2,2,3,2,3,0,0
db 0,2,2,2,2,3,0,0,0,2,3,0,2,2,2,3,0,0,0,0
db 2,3,0,0,0,0,2,2,2,3,2,0,0,0,0,2,2,3,2,0
db 0,2,2,2,2,2,2,3,0,2,2,2,3,3,3,0,0,2,3,3
db 2,2,2,0,0,0,0,0,0,0,2,3,0,0,2,2,2,2,2,0
db 0,0,2,2,2,2,2,3,0,0,2,2,2,2,0,0,0,0,0,0
db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
db 2,2,3,0,0,2,3,0,2,2,2,0,0,2,2,0,2,2,3,0
db 0,0,0,0,2,2,3,0,0,2,3,0,2,2,2,0,0,0,0,0
db 2,2,3,0,0,0,0,0,2,2,3,0,0,2,3,0,2,2,3,0
db 0,2,3,0,0,0,2,2,3,0,0,0,0,0,0,0,2,2,3,0
db 2,2,2,2,3,0,0,0,2,2,3,0,0,0,0,0,2,2,3,0
db 0,2,3,0,2,2,3,0,0,2,3,0,2,2,3,0,0,2,3,0
db 2,2,3,0,0,0,0,0,2,2,3,0,0,2,3,0,2,2,3,0
db 3,2,3,0,0,0,0,0,3,2,2,0,0,0,2,2,3,0,0,0
db 2,2,3,0,0,2,3,0,0,2,2,0,2,2,0,0,2,2,0,2
db 0,2,3,0,0,0,2,2,3,0,0,0,0,0,2,2,3,0,0,0
db 2,2,2,3,0,0,0,0,0,2,2,2,2,3,2,3,0,0,0,0
db 2,3,0,0,0,2,2,2,3,0,0,0,0,0,0,0,0,0,2,2
db 0,0,0,0,0,0,2,3,0,0,0,0,3,2,2,2,0,2,2,3
db 0,0,2,3,0,0,0,0,0,0,2,3,0,2,2,3,0,0,2,3
db 0,0,0,0,0,0,2,3,0,0,3,3,3,3,0,0,0,0,0,0
db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
db 2,2,3,0,0,2,3,0,2,2,3,0,0,2,3,0,2,2,3,0
db 0,2,2,0,2,2,3,0,0,2,3,0,2,2,3,0,0,0,0,0
db 2,2,3,0,0,0,0,0,2,2,3,0,0,2,3,0,2,2,3,0
db 0,2,3,0,0,0,2,2,3,0,0,0,2,2,0,0,2,2,3,0
db 2,2,3,2,2,3,0,0,2,2,3,0,0,0,0,0,2,2,3,0
db 0,2,3,0,2,2,3,0,0,2,3,0,2,2,3,0,0,2,3,0
db 2,2,3,0,0,0,0,0,2,2,3,0,0,2,3,0,2,2,3,0
db 0,2,3,0,0,0,0,0,0,2,3,0,0,0,2,2,3,0,0,0
db 2,2,3,0,0,2,3,0,0,3,2,3,2,3,0,0,2,2,0,2
db 0,2,3,0,0,2,2,3,2,3,0,0,0,0,2,2,3,0,0,0
db 2,2,3,0,0,0,0,0,0,2,2,3,3,0,2,3,0,0,0,0
db 2,3,0,0,0,2,2,3,0,0,0,0,0,0,0,0,0,0,2,3
db 0,0,0,0,0,0,2,3,0,0,0,0,0,2,2,3,0,2,2,3
db 0,0,2,3,0,0,0,0,0,0,2,3,0,2,2,3,0,0,2,3
db 0,0,0,0,0,0,2,3,0,0,0,0,0,0,0,0,0,0,0,0
db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
db 2,2,3,0,0,2,3,0,2,2,3,0,0,2,3,0,2,2,3,0
db 0,2,3,0,2,2,3,0,0,2,3,0,2,2,3,0,0,0,0,0
db 2,2,3,0,0,0,0,0,2,2,3,0,0,2,3,0,2,2,3,0
db 0,2,3,0,0,0,2,2,3,0,0,0,2,3,0,0,2,2,3,0
db 2,2,3,0,2,2,0,0,2,2,3,0,0,0,0,0,2,2,3,0
db 0,2,3,0,2,2,3,0,0,2,3,0,2,2,3,0,0,2,3,0
db 2,2,3,0,0,0,0,0,2,2,3,0,0,2,0,0,2,2,3,0
db 0,2,3,0,0,0,0,0,0,2,3,0,0,0,2,2,3,0,0,0
db 2,2,3,0,0,2,3,0,0,0,2,2,2,0,0,0,2,2,0,2
db 0,2,3,0,2,2,3,0,2,2,3,0,0,0,2,2,3,0,0,0
db 2,2,3,0,0,0,0,0,0,2,2,3,0,0,2,3,0,0,0,0
db 2,3,0,0,0,2,2,3,0,0,0,0,0,0,0,0,0,0,2,3
db 0,0,0,0,0,0,2,3,0,0,0,0,0,2,2,3,0,2,2,3
db 0,0,2,3,0,0,0,0,0,0,2,3,0,2,2,3,0,0,2,3
db 0,0,0,0,0,0,2,3,0,0,0,0,0,0,0,0,0,0,0,2
db 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
db 2,2,3,0,0,2,3,0,2,2,3,0,0,2,3,0,2,2,3,0
db 0,2,3,0,2,2,3,0,3,2,3,0,2,2,3,0,0,0,0,0
db 2,2,3,0,0,0,0,0,2,2,3,0,0,2,3,0,2,2,3,0
db 0,2,3,0,0,0,2,2,3,0,0,0,2,3,0,0,2,2,3,0
db 2,2,3,0,3,2,3,0,2,2,3,0,0,0,0,0,2,2,3,0
db 0,2,3,0,2,2,3,0,0,2,3,0,2,2,3,0,0,2,3,0
db 2,2,3,0,0,0,0,0,2,2,3,0,2,2,2,0,2,2,3,0
db 0,2,3,0,0,0,0,0,3,2,3,0,0,0,2,2,3,0,0,0
db 2,2,3,0,0,2,3,0,0,0,3,2,3,0,0,0,2,2,2,2
db 3,2,3,0,2,2,0,0,0,2,3,0,0,0,2,2,3,0,0,0
db 2,2,3,0,0,0,0,0,0,2,2,3,0,0,2,3,0,0,0,0
db 2,2,0,0,0,2,2,3,0,0,0,0,0,0,0,0,0,0,2,3
db 0,0,0,0,0,0,2,3,0,0,0,0,3,2,2,3,0,2,2,3
db 0,0,2,3,0,0,0,0,0,0,2,3,0,2,2,3,0,0,2,3
db 0,0,0,0,0,3,2,3,0,0,0,0,0,0,0,0,0,0,0,2
db 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
db 3,3,3,0,0,3,3,0,3,3,3,3,3,3,0,0,0,3,3,3
db 3,3,0,0,3,3,3,3,3,3,0,0,3,3,3,3,3,3,3,0
db 3,3,3,0,0,0,0,0,0,3,3,3,3,3,0,0,3,3,3,0
db 0,2,3,0,2,2,2,3,3,3,3,0,0,2,3,3,3,3,0,0
db 3,3,3,0,0,2,3,0,3,3,3,3,3,3,3,0,3,3,3,0
db 0,3,3,0,3,3,3,0,0,3,3,0,0,3,3,3,3,3,0,0
db 3,3,3,0,0,0,0,0,0,3,3,3,2,3,3,0,3,3,3,0
db 0,2,3,0,3,3,3,3,3,3,0,0,0,0,2,3,3,0,0,0
db 0,3,3,3,3,3,0,0,0,0,0,3,0,0,0,0,0,3,3,3
db 3,3,0,0,2,3,0,0,0,2,3,0,0,0,2,3,3,0,0,0
db 3,3,3,3,3,3,3,0,0,0,3,3,3,3,3,0,0,2,2,2
db 2,3,3,3,0,3,3,3,3,3,3,3,0,3,3,3,3,3,3,0
db 0,0,0,0,0,0,2,3,0,3,3,3,3,3,3,0,0,0,3,3
db 3,3,3,0,0,0,0,0,0,0,3,3,0,0,3,3,3,3,3,0
db 0,3,3,3,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0
db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

Numbers: DW 11
dw 170,138,-242,-39,-283,-72,-129,-164,-14,-4
dw 101,-190,-89,4,101,-218,-14,-201,-157,150
dw -173,-72,-232,73,-199,1,-278,72,-98,-185
dw 137,-49,194,-253,110,113,-276,74,84,-16
dw -15,-14,36,-125,94,-291,131,-239,-4,-274
dw -13,38,119,-212,-232,-11,40,-49,-4,-214
dw -163,61,53,-8,-66,199,6,141,-129,-262
dw -8,124,45,-214,20,197,116,-83,-296,84
dw -133,196,25,-236,-42,178,-133,136,128,120
dw -292,-81,20,-172,-61,-264,-66,155,-57,-36
dw -55,-203,169,72,-67,161,66,-218,-294,-297
dw -118,-3,24,-233,-75,111,-50,58,50,-120
dw 87,42,-281,-282,-128,147,144,-142,-254,-183
dw 113,-22,117,44,-238,-5,171,164,135,-135
dw -277,-194,46,-151,-102,32,-148,-161,82,-198
dw -127,-60,191,41,-163,154,-57,-286,-166,131
dw -67,-76,-97,58,-58,185,-12,-88,42,140
dw 130,-93,161,130,-100,90,-71,128,166,186
dw -276,197,-36,-115,-22,-271,-186,-72,-81,-111
dw -2,-68,60,-245,-104,52,-145,-162,132,194
dw 83,-280,-97,97,145,-259,-34,-227,63,-272
dw -78,-137,128,-55,17,136,-261,10,-153,-35
dw 117,196,-57,-262,-294,129,-132,-212,175,38
dw 52,61,92,-172,-252,-152,-66,-130,88,51
dw -244,43,-130,61,-69,162,-199,-34,25,75
dw 79,-82,-14,-176,-202,129,-47,-98,-206,68
dw 106,-267,175,88,123,172,-94,-199,-80,-87
dw -71,178,-29,-173,-44,-54,151,-122,153,-1
dw 145,-98,100,166,110,125,12,-215,-107,-29
dw -158,171,-269,-215,-1,199,-11,-247,63,-176
dw 127,-71,-27,12,178,-131,-178,160,189,38
dw -19,-290,165,-238,-58,-185,-295,115,-147,-10
dw 127,-229,-126,-146,18,-31,90,171,-240,-140
dw -150,10,-288,141,24,11,-144,-1,-198,-177
dw -171,-2,-238,-72,-91,-287,84,197,119,-25
dw -89,126,-239,-223,-48,-150,-182,-24,88,132
dw -236,70,-156,180,-35,-63,-293,-266,-10,86
dw -70,31,-261,32,44,-169,-40,102,-49,-130
dw -167,48,95,180,-90,1,-172,-133,-135,-89
dw -299,62,87,-250,66,-119,-190,-132,-34,-156
dw 76,38,-68,-144,-33,-175,-53,-51,157,65
dw -88,120,-38,-157,135,-262,198,-4,-48,90
dw -226,-209,-293,-208,73,56,-15,-114,-199,149
dw 66,101,114,-198,-269,-153,-53,-22,159,164
dw 98,29,-218,-41,-247,34,82,-10,-245,-147
dw -31,184,45,49,-44,5,-177,-66,12,197
dw 127,161,93,-128,115,-137,-57,31,-6,5
dw -60,-289,137,-293,-179,126,85,-44,-194,-275
dw -280,-296,-279,-215,58,30,-134,-265,196,117
dw 6,39,109,-54,-48,122,97,-252,-117,-92

ScreenWidth   EQU 320   ; Obvious
ScreenHeight  EQU 200   ; Obvious
MaxStars      EQU 250   ; Maximum number of stars
MaxNumbers    EQU 500   ; Number of random numbers defined  
MaxZ          EQU 4096  ; StartZvalue for all stars
MinZ          EQU 0     ; If Z = 0 then star is dead

XIndex:       DW  250   ; Index to X-numbers
YIndex:       DW  125   ; Index to Y-numbers
WarpSpeed:    DW  20    ; Speed of stars  
NumActive:    DW  0     ; Number of stars active

;Star_Struc      STRUC   ; Format of star (like a record in Pascal)
;     X   DW   0         ; X-position of star
;     Y   DW   0         ; Y-position of star
;     Z   DW   0         ; Z-position of star
;     Old DW   0         ; Where to erase old star
;     Col DB   0         ; Color of star
;Star_Struc      ENDS

Palette:
	DB 0,0,0        ; Palette info for first 5 colors (font)
        DB 0,0,0
        DB 52,0,0
        DB 42,0,0
        DB 32,0,0
        DB 0,0,0                    ; Base color black => R,G,B
    	; 16 grey shades
        DB 3*15,3*15,3*15
        DB 3*14,3*14,3*14
        DB 3*13,3*13,3*13
        DB 3*12,3*12,3*12
        DB 3*11,3*11,3*11
        DB 3*10,3*10,3*10
        DB 3*9,3*9,3*9
        DB 3*8,3*8,3*8
        DB 3*7,3*7,3*7
        DB 3*6,3*6,3*6
        DB 3*5,3*5,3*5
        DB 3*4,3*4,3*4
        DB 3*3,3*3,3*3
        DB 3*2,3*2,3*2
        DB 3*1,3*1,3*1
        DB 3*0,3*0,3*0

bss_start:

ABSOLUTE bss_start

PaletteArray:
     resb 768		      ; Array to hold the palette

alignb 2

Star_Struct:
     Stars_X   equ   0        ; X-position of star
     Stars_Y   equ   2        ; Y-position of star
     Stars_Z   equ   4        ; Z-position of star
     Stars_Old equ   6        ; Where to erase old star
     Stars_Col equ   8        ; Color of star

StarStrucSize equ 9    ; Number of bytes per entry ( 4 wordz and a byte )

Stars:
     resb StarStrucSize * MaxStars  ; Array of star-records

bss_end:

; Code by Vulture.
; Thanx to Draeden of VLA for example code.
; Don't be lame. Don't just rip the code.
; Give credit where it should be. I did.
; See ya in the next release.

_end:                       	; End Of C<><>L Program !!!