; ****************************************************************************
; boardz32.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'boardz32.prg')
; ----------------------------------------------------------------------------
; BOARDZ32.PRG ! TEST program !  TRDOS 386 VGA Functionality test !
;
; 01/10/2016
;
; [ Last Modification: 02/10/2016 ]
;
; Derived from source code of 'BOARDZ.EXE' (MSDOS) intro file
; (BOARDZ.ASM, 13/02/1995)
;
; BOARDZ.ASM by by Vulture.
; NASM version of BOARDZ.ASM: Erdogan Tan, 01/10/2016 (boardz32.s)
;
; Assembler: NASM 2.11
;
; (Original -msdos- code has been modifed for TRDOS 386 system calls and
; other protected mode (TRDOS 386) interrupts.)
; ****************************************************************************

; 19/05/2016
; 29/04/2016
; TRDOS 386 system calls (temporary list!)
_ver 	equ 0
_exit 	equ 1
_fork 	equ 2
_read 	equ 3
_write	equ 4
_open	equ 5
_close 	equ 6
_wait 	equ 7
_creat 	equ 8
_link 	equ 9
_unlink	equ 10
_exec	equ 11
_chdir	equ 12
_time 	equ 13
_mkdir 	equ 14
_chmod	equ 15
_chown	equ 16
_break	equ 17
_stat	equ 18
_seek	equ 19
_tell 	equ 20
_mount	equ 21
_umount	equ 22
_setuid	equ 23
_getuid	equ 24
_stime	equ 25
_quit	equ 26	
_intr	equ 27
_fstat	equ 28
_emt 	equ 29
_mdate 	equ 30
_video	equ 31
_audio	equ 32
_timer	equ 33
_sleep	equ 34
_msg    equ 35
_geterr equ 36
_rsrvd1	equ 37
_pri	equ 38
_rele 	equ 39

%macro sys 1-4
    ; 29/04/2016 - TRDOS 386 (TRDOS v2.0)	
    ; 03/09/2015	
    ; 13/04/2015
    ; Retro UNIX 386 v1 system call.	
    %if %0 >= 2   
        mov ebx, %2
        %if %0 >= 3    
            mov ecx, %3
            %if %0 = 4
               mov edx, %4   
            %endif
        %endif
    %endif
    mov eax, %1
    ;int 30h
    int 40h ; TRDOS 386 (TRDOS v2.0)	   
%endmacro

; TRDOS 386 (and Retro UNIX 386 v1) system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

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

[Bits 32]   ; Protected Mode (TRDOS 386) Program

[org 0]  ; TRDOS 386 PRG File

START:
    ; DIRECT VGA MEMORY ACCESS
    ;xor    ebx, ebx
    mov     bh, 5 ; Direct access/map to VGA memory (0A0000h)
    ;mov    eax, _video ; 1Fh
    mov     al, 1Fh ; sys _video ; TRDOS 386 Video functions
    int     40h   ; TRDOS 386 system call

    ; eax = 0A0000h
    and     eax, eax
    jz      terminate ; error (eax = 0)

    call    SetVGA              ; Get in GFX-mode

; === Set the palette ===

    mov     ax,1012h            ; Select write palette function
    xor     bx,bx               ; Start at color 0
    mov     ecx,23              ; Write 23 colors
    mov     edx,Palette   	; es:dx points to palette data
    ;int    10h                 ; Call VID interrupt & set palette
    int	    31h ; TRDOS 386 - Video interrupt
    call    SavePalette         ; And save palette into array

; === Initialize pointers ===
    mov     edi, 0A0000h	; es = 0A000h (VGA)

; === Start scroll ===
Reset:
    lea     esi, [Text]         ; si points to start of Text
Mainthing:                      ; Main loop
    lodsb                       ; Load a character in al (si increased)
    cmp     al,0                ; Have we reached the end of da text ?
    je      short Reset         ; Yep => Start over
    push    esi                 ; Save character-offset on stack
    mov     bl,al               ; Save the character into bx
    mov     cx,0                ; Set character-counter for position in font
    lea     esi, [Order]        ; si points to offset Order
Again:
    lodsb                       ; Load the order-character (abcdef etc...)
    cmp     al,bl               ; Is it da same letter/character ?
    je      short Found         ; Yeah => Found it. . .
    inc     cx                  ; Nope => Increase character-counter
    jmp     short Again         ; Compare with next character
Found:
    mov     ax,8                ; 7 pixels + black pixel = 8 pixels
    mul     cx                  ; ax=ax*cx    (e.g:  E := 4 * 10;)

    mov     bx,4                ; Draw 4 * 2 vertical lines
Hloop:
    lea     esi, [Font]         ; si points to start of Font
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
    push    eax                 ; Save pointer to character
    mov	    esi, 0A0000h        ; ds = 0A000h (VGA-segment)
    mov     si,320*90+2         ; Destination offset
    mov     di,320*90           ; Source offset
    mov     cx,10*320           ; Repeat factor => Number of bytes to copy
    rep     movsb               ; And go ! (Hint: why not use words instead)

    mov     di,320*101          ; On some slow VGA-cards we have to plot
    mov     cx,5                ; 5 black pixels just below the scroller
    mov     al,0                ; on the left on the screen. Erase this
    rep     stosb               ; code to see what I mean.

    call    WaitVrt             ; Wait for vertical retrace
    call    CalcStar            ; Calculate new stars
    call    ShowStars           ; Show all stars on VGA

; === Want to quit ? ===
    mov     ah, 1
    ;int    16h
    int     32h	; TRDOS 386 Keyboard interrupt
    jz      short no_quit
    xor     ax, ax
    ;int    16h
    int     32h	; TRDOS 386 Keyboard interrupt
    jmp     short QuitNow

no_quit:
; === No quit ? then continue ===
    pop     eax                 ; Restore pointer to character
    add     ax,2                ; And add 2 to point to next 2 vertical lines
    dec     bx                  ; Decrease line-counter
    jnz     Hloop               ; If it's 0 then jump
    pop     esi                 ; Restore character-offset to do next char
    jmp     Mainthing           ; And start over again

QuitNow:                        ; Quit everything
    pop     eax
    pop     esi
	
    call    FadeOut             ; Fade da screen to black
    call    SetText             ; Get in TXT-mode

    mov     ebx, Message ; message offset
    mov     ecx, 255 ; message length 
    mov     edx, 0Fh ; white color
    mov     eax, 35 ; 'sysmsg'
    int     40h     ; TRDOS 386 system call

terminate:
    sys     _exit   ; INT 40h
here:
    jmp	    short here

; === PROCEDURES ===

SetVGA:				; Get into VGA mode
    mov     ax,0013h            ; Set the videomode 320*200*256
    ;int    10h                 ; Call VID interrupt
    int     31h ; TRDOS 386 - Video interrupt
    retn

SetText:			; Get into character mode
    mov     ax,0003h            ; Set 80x25x16 char mode
    ;int    10h                 ; Call VID interrupt
    int     31h ; TRDOS 386 - Video interrupt
    retn

WaitVrt:			; Waits for vertical retrace to reduce "snow"
    mov     dx,3dah
    mov     ah, 0 ; in (byte)
Vrt:
    ;in     al,dx
    int     34h ; TRDOS 386 - IOCTL interrupt
    test    al, 8
    jnz     short Vrt		; Wait until Verticle Retrace starts
NoVrt:
    ;in     al,dx
    int     34h ; TRDOS 386 - IOCTL interrupt
    test    al, 8
    jz      short NoVrt		; Wait until Verticle Retrace ends
    retn                        ; Return to main program

SavePalette:			; Saves entire palette in array
    mov     ebp, PaletteArray   ; Point to start of array
    mov     cx,768              ; Save all R,G,B registers
    mov     dx,03c7h            ; Read register
    mov     al,0                ; Start at 0
    ;out    dx,al               ; Write to port
    mov     ah, 1 ; out (byte)
    int     34h ; TRDOS 386 - IOCTL interrupt
    mov     dx,03c9h            ; Read data register
    mov     ah, 0 ; in (byte)
Grab:
    ;in     al,dx               ; Read value from port
    int     34h ; TRDOS 386 - IOCTL interrupt
    and     al,3fh              ; Mask of upper 2 bits
    mov     byte [ebp],al       ; Store the value in array
    inc     ebp                 ; Point to the next one
    loop    Grab                ; Loop until cx = 0
    retn                        ; Return to main program

FadeOut:			; Fades screen to black
    mov     ebp, PaletteArray   ; Point to start of array
    mov     cx,64               ; Repeat 64 times (0..63)
OneCycle:
    mov     bx,0                ; Set counter
Decrease:
    cmp     byte [ebp],0        ; Is it 0 already ?
    je      short Fading        ; Yep => Do the next
    dec     byte [ebp]          ; Nope => Decrease by one
Fading:
    inc     ebp                 ; Point to next value
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
    ;out    dx,al               ; Write to port
    mov     ah, 1 ; out (byte)
    int     34h ; TRDOS 386 - IOCTL interrupt
    inc     dx                  ; Writing => 03c8h + 1 = 03c9h
WriteAll:
    mov     al,byte [ebp]       ; Store value in al
    ;out    dx,al               ; Give it to the VGA
    ;mov    ah, 1 ; out (byte)
    int     34h ; TRDOS 386 - IOCTL interrupt
    inc     ebp                 ; Point to next one
    inc     bx                  ; Increase counter
    loop    WriteAll            ; Loop while cx > 0
    pop     cx                  ; Restore 1st loop counter
    sub     ebp,768             ; Point to start
    loop    OneCycle            ; Loop while cx > 0
    retn                        ; Return to main program

CalcStar:
    pusha                               ; Put all registers on stack
    mov     esi, Stars                  ; si points to first star
StartCalc:                              ; Start searching for empty slots
    cmp     word [NumActive], MaxStars  ; Check for room
    jae     NoEmptySpace		; No room => exit

SearchSlot:
    cmp     word [Stars_Z+esi],MinZ  ; If Z = 0 then slot is empty
    je      short FillSlot

    add     esi,StarStrucSize           ; si points to next star
    cmp     esi,Stars+(StarStrucSize*MaxStars) ; Have we done entire array ?
    jb      short SearchSlot		; No => search again
    jmp     short NoEmptySpace		; Yes => exit

FillSlot:
    mov     bx,[XIndex]                 ; Grab Xindex and put it in di
    add     bx,bx                       ; Make WORD index
    mov     ax,[Numbers+ebx]            ; Get the number
    shl     ax,3                        ; Multiply by 8   
    mov     [Stars_X+esi],ax            ; Save the number

    mov     bx,[YIndex]                 ; Do the same for Y
    add     bx,bx
    mov     ax,[Numbers+ebx]
    shl     ax,3
    mov     [Stars_Y+esi],ax

    mov     word [Stars_Z+esi],MaxZ     ; Give star the Z offset
    mov     al,0                        ; Also give it basecolor 0 (black)   
    mov     [Stars_Col+esi],al          ; Store the color

    inc     word [NumActive]            ; Increase star counter

    inc     word [XIndex]               ; Increase the X index
    cmp     word [XIndex],MaxNumbers    ; Have we reached the end of the list?
    jb      short XindNotMax            ; No => continue
    mov     word [XIndex],0             ; Yes => go to start of list

XindNotMax:
    inc     word [YIndex]               ; Increase the Y index   
    cmp     word [YIndex],MaxNumbers    ; Have we reached the end of the list?
    jb      StartCalc                   ; No => continue
    mov     word [YIndex],0             ; Yes => go to start of list
    
NoEmptySpace:
    popa                                ; Restore all registers
    retn                                ; Return to main program   

ShowStars:
    pusha                        ; Save all registers
    mov     esi, Stars           ; si points to first record
ShowLoop:
    mov     cx,[Stars_Z+esi]     ; Grab Z value of star
    cmp     cx,0                 ; If Z = 0 then exit
    je      short ContinueStar   ; Do the next star

    mov     di,[Stars_Old+esi]   ; Get old position of star
    mov     byte [edi],0         ; Erase the old star

    mov     ax,[Stars_X+esi]     ; Grab X value of star
    mov     dx,256               ; Multiply X with 256
    imul    dx
    idiv    cx                   ; Divide by Z
    add     ax,ScreenWidth/2     ; Add 160 to center it on the screen
    mov     di,ax                ; di = X
    cmp     di,ScreenWidth       ; Is the star in range ?
    jae     short TermStar       ; No => Do next star

    mov     ax,[Stars_Y+esi]     ; Grab an Y value
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

    mov     [Stars_Old+esi],di   ; Save the position

    add     ch,[Stars_Col+esi]   ; Divide Z by 256 & add basecolor 0
    mov     al,ch                ; Move color into al
    add     al,5                 ; Add 5 to avoid fontcolors

    mov     byte [edi],al  	 ; Place the dot on the screen

    mov     ax,[WarpSpeed]
    sub     cx,ax                ; Decrease Z with WarpSpeed
    mov     [Stars_Z+esi],cx     ; Save the new Z

    jmp     short ContinueStar   ; Do the next star

TermStar:
    mov     word [Stars_Z+esi],MinZ ; Set Z to 0 => Star is terminated
    dec     word [NumActive]     ; Decrease number of active stars

ContinueStar:
    add     esi,StarStrucSize    ; si points to next record
    cmp     esi,Stars+(StarStrucSize*MaxStars) ; Reached end of array ?
    jb      ShowLoop             ; Continue with next star

    popa                         ; Restore all registers
    retn                         ; Return to main program

; === DATA ===   

;%include 'FONT.DAT'    ; File with font data
;%include 'NUMBERS.DAT' ; Include 500 random numbers between -200 and 200

;Message: DB   13,10,"Code by Vulture.",13,10,"$"  ; Important message  :)
Message: DB   13,10,"Code by Vulture.",13,10
NewMessage: DB "Reprogrammed by Erdogan Tan (via NASM), 02/10/2016." 
         DB 13, 10, 0
Text:
        DB 'if u wanna experience some cewl boardz in the netherlands '
        DB 'call firehouse 058-661590    detonator 05111-4307   or   '
        DB 'mark of cain 058-672111      cu around. . . . . .'
        DB '                            ', 0           ; Text to scroll

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