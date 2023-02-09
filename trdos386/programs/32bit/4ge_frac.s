; TRDOS 386 Version (22/08/2016)
; NASM Version: Erdogan Tan (22/08/2016)

; 4ge's Xmas 94 Intro fractalzoom source
; ======================================

; Copyright (C) 1994/5 Samuel Marshall. All rights reserved. 

; Text and program code by Samuel Marshall, a.k.a. CuteElf / 4ge.

; Contact me at the following email address:

; Samuel.Marshall@durham.ac.uk

; and if you don't get a response, probably it's not during termtime, and
; I am at home - in which case, I check email here only about once a month
; (if that). You will get a reply eventually.

; My WWW homepage is

; http://www.dur.ac.uk/~d405ua/

; Enjoy the program... hope it helps you. Don't expect a wonderfully-
; optimised piece of code, because 1) the zoom routine wasn't really time-
; critical, and 2) this is the first time I ever wrote a fractal program,
; in my entire life, and 3) I didn't know how to do fixed-point numbers
; properly when I wrote this ;)

; General Principles for a real-time fractalzoom using this method
; ================================================================
;
; First, we calculate a fractal at twice the size, each way (i.e. 4 times
; the area) as the screen display area.
;
; We then display that fractal, zoomed-out to half size each way, so that
; it will exactly fill the screen display area.
;
; Then, we calculate another fractal twice the size of the display area,
; but this one is calculated "zoomed in" so that this fractal is a more
; detailed view of one-quarter of the area of the fractal just calculated.
;
; While we are calculating this, which takes a few seconds, we gradually
; zoom in the fractal we already have - using standard bitmap-zoom 
; techniques - until eventually by the time the new fractal is finished,
; the old fractal will be showing at 1:1 size, and will can then be 
; seamlessly replaced on the display by the new fractal at 1:2. Then
; repeat.
;
; Note: the fractal being DISPLAYED, i.e. the one that's already been
; calculated, can be zoomed to any point at all within the region, but
; this decision must be known in advance so that the next fractal is
; calculated from the right point. That's why you can't change the direction
; "realtime", only every so often.

; I would include references here but I worked the method out myself with
; no help from anything or anyone, so there aren't any... (oh, by the
; way, I assume this is the standard method everyone else uses too, it's
; nothing special or anything, just that I reinvented the wheel one more
; time ;)

; More details are included in the individual function descriptions.

; ---- TRDOS 386 --------------------------------------------------------

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

[BITS 32] ; 80386 Protected Mode (32 bit) intructions

[ORG 0] 

jmp Start

Getch:
mov ax, 0
;int 16h
int 32h  ; TRDOS 386 Keyborad Interrupt
retn

; You can probably change height without messing things up, but
; changing the width will I think need some work.

FRACWIDTH equ 256
FRACHEIGHT equ 128

; Fractal parameter
frac:    dd 3

; Memory
enlargebufferseg:        dd 0    ; segment address of the 64*32 enlarge buf.
textureseg:              dd 0    ; the segment address of texture to display.
fractalseg:              dd 0    ; the segment address of texture to create

;====================================================================
;ZOOMTEXTURE - display picture at given zoom fraction
;--------------------------------------------------------------------

; This is pretty obvious; just a standard, fixed-point bitmap
; display routine. The only extra bit you might notice is that
; it doubles each pixel, so as to get a larger screen display without
; taking too long to calculate the fractals.

; Data_______________________________________________________________

lfrac:   dw 0    
hfrac:   dw 0    ; texture pixels per real pixel
hfracb:  db 0    ; same but a byte

ystart:  db 0
xstart:  db 0    ; where to start, in texturemap

oldbx:   dw 0

yfracpos: dw 0   ; fractional y-position

stopat:  dd 0    ; where to stop in screen ram

; Source_____________________________________________________________

ZoomTexture:
push eax
push ebx
push ecx
push edx
push esi
push edi
push ebp

mov edi, [screenstart]      ; start position on display memory
mov [stopat], edi
add dword [stopat], FRACHEIGHT*320 ; stop position on display memory
movzx ebx, byte [xstart]           ; start position on texture memory
mov bh, [ystart]
mov ebp, [textureseg]

mov al, [hfrac]
mov [hfracb], al

mov dx, [lfrac]

  ZoomYLoop:

  mov si, FRACWIDTH/2
  mov [oldbx], bx
  ;mov cx, 0
  xor ecx, ecx	

    ZoomXLoop:

    mov al, [ebx+ebp]
    mov ah, al
    mov [edi], ax
    mov [edi+320], ax
    add di, 2
    add cx, [lfrac]
    adc bx, [hfrac]

    dec si
    jnz short ZoomXLoop

  mov bx, [oldbx]

  add [yfracpos], dx
  adc bh, [hfracb]

  add edi, 640-FRACWIDTH
  cmp edi, [stopat]
  jb short ZoomYLoop

; Check for keypress
mov ah,1
;int 16h
int 32h ; TRDOS 386 Keyborad Interrupt
jz short zt_afterkeyhit  

jmp dfs_keyhit

zt_afterkeyhit:

pop ebp
pop edi
pop esi
pop edx
pop ecx
pop ebx
pop eax
retn

;====================================================================
;CALCULATE - work out one pixel of Mandelbrot fractal
;--------------------------------------------------------------------

; The fractal formula used, with coordinates x,y, is:
; A = B = 0. Colour=starting colour.
;
; new A = a squared - b squared - x
; new B = 2 * a * b - y
;
; If A squared + B squared > some number frac, then stop.
; If been round loop more than colour limit (64) times, then stop too.
;
; Otherwise, work out next A and B, and increment the colour we're going
; to use for the pixel.
;
; When we get out of the loop, that colour value is the one to draw at
; this pixel.

; Note:
; The fixed point sections of this are pretty crap; there are proper ways
; to do fixed point (I think...) so for god's sake don't copy those
; bits for your own fixed point code.

; Data_______________________________________________________________

colour:  db 0    ; colour to plot the pixel

esign:    db 0    ; esign of multiplication result

la:      dd 0
ha:      dd 0    
lb:      dd 0
hb:      dd 0    ; a,b from a+bi. Low and high words thereof.

lasq:    dd 0
hasq:    dd 0
lbsq:    dd 0
hbsq:    dd 0    ; a squared, b squared, low and high words.

lnewa:   dd 0
hnewa:   dd 0    ; temporary storage for `new' veresion of `a'.

lx:      dd 0
hx:      dd 0
ly:      dd 0
hy:      dd 0    ; co-oredinates in complex plane of this point

; Source_____________________________________________________________

Calculate:
  push eax
  push ebx
  push ecx
  push edx
  push esi
  push edi

; 0) Fix up the x,y data to specific point
;  mov dword [lx], 2621
;  mov dword [hx], 1
;  mov dword [ly], 11107
;  mov dword [hy], 1

; 1) setup colour
  mov byte  [colour], 32

; 2) clear the a,b and squared variables,
  xor eax, eax ; 0
  mov [la], eax
  mov [ha], eax
  mov [lb], eax
  mov [hb], eax
  mov [lasq], eax
  mov [hasq], eax
  mov [lbsq], eax
  mov [hbsq], eax

  CalcLoop:

  ; 3) increment colour
  inc byte [colour]

  ; 4) set up a-squared
asquared:  
  mov eax, [lasq]
  mov edx, [hasq]

minusbsquared:
  ; 5) subtract b-squared
  sub eax, [lbsq]
  sbb edx, [hbsq]

minusx:  
  ; 6) subtract x and store result in new-a
  sub eax, [lx]
  sbb edx, [hx]
  mov [lnewa], eax
  mov [hnewa], edx

  ; 7) multiply a and b
  
atimesb:
  ; a. setup variables
  mov ebx, [la]
  mov ecx, [ha]
  mov esi, [lb]
  mov edi, [hb]

fixesigns:  
  ; b. sort out esigns to be poesitive
  mov byte [esign], 0
  cmp ecx, 0
  jge short ecxok_1
    xor ecx, 0ffffffffh
    xor ebx, 0ffffffffh
    add ebx, 1
    adc ecx, 0
    inc byte [esign]
  ecxok_1:
  cmp edi, 0
  jge short ediok_1
    xor edi, 0ffffffffh
    xor esi, 0ffffffffh
    add esi, 1
    adc edi, 0
    inc byte [esign]
  ediok_1:

multiply:
  ; c. multiply the two numbers
  mov dword [hb], 0
  mov eax, ebx
  mov edx, esi
  mul edx
  mov [lb], edx
  mov eax, ebx
  mov edx, edi
  mul edx
  add [lb], eax
  adc [hb], edx
  mov eax, ecx
  mov edx, esi
  mul edx
  add [lb], eax
  adc [hb], edx
  mov eax, ecx
  mov edx, edi
  mul edx
  add [hb], eax

fixresultesign:
  ; d. fix the esign of the result
  test byte [esign], 1
  jz short esignok_1
    xor dword [hb], 0ffffffffh
    xor dword [lb], 0ffffffffh
    add dword [lx], 1
    adc dword [hb], 0
  esignok_1:

doublenumber:
  ; 8) Add this to itself, (with carry)
  mov eax, [lb]
  mov edx, [hb]
  add eax, eax
  adc edx, edx

subtracty:
  ; 9) Subtract y and store in b
  sub eax, [ly]
  sbb edx, [hy]
  mov [lb], eax
  mov [hb], edx

  ; 10) Update a from new-a : combined with
  ; 11) Square a and store in a-squared
  ; a. setup variable
starttosquare:  
  mov ebx, [lnewa]
  mov ecx, [hnewa]
  mov [la], ebx
  mov [ha], ecx

fixesign:
  ; b. sort out esign to be poesitive
  cmp ecx, 0
  jge short ecxok_2
    xor ecx, 0ffffffffh
    xor ebx, 0ffffffffh
    add ebx, 1
    adc ecx, 0
  ecxok_2:

squareit:
  ; c. square data
  mov dword [hasq], 0
  mov eax, ebx
  mul eax
  mov [lasq], edx
  mov eax, ebx
  mul ecx
  add [lasq], eax
  adc [hasq], edx
  add [lasq], eax
  adc [hasq], edx
  mov eax, ecx
  mul eax
  add [hasq], eax

sametosquareb:
  ; 12) Square b and store in b-squared
  ; a. setup variable
  mov ebx, [lb]
  mov ecx, [hb]

  ; b. sort out esign to be poesitive
  cmp ecx, 0
  jge short ecxok_3
    xor ecx, 0ffffffffh
    xor ebx, 0ffffffffh
    add ebx, 1
    adc ecx, 0
  ecxok_3:

  ; c. square data
  mov dword [hbsq], 0
  mov eax, ebx
  mul eax
  mov [lbsq], edx
  mov eax, ebx
  mul ecx
  add [lbsq], eax
  adc [hbsq], edx
  add [lbsq], eax
  adc [hbsq], edx
  mov eax, ecx
  mul eax
  add [hbsq], eax

asquaredaddbsquared:  
  ; 13) Setup a-squared
  mov eax, [lasq]
  mov edx, [hasq]

  ; 14) Add b-squared
  add eax, [lbsq]
  adc edx, [hbsq]

  ; 15) Compare with *n*, stop if >
  cmp edx, [frac]
  jg short CalcFinish

  ; 16) If colour > *c*, stop
  cmp byte [colour], 63
  jg short CalcFinish

jmp CalcLoop

CalcFinish:
; 17) Return pixel colour
pop edi
pop esi
pop edx
pop ecx
pop ebx
pop eax
mov al, [colour]
retn

;====================================================================
;FRACTAL - loop round to draw a 256x256 fractal using Calculate
;--------------------------------------------------------------------

; Basically, this just uses fixed-point to go through all the 
; x and y coordinates corresponding to SCREEN x and y.
; If you're confused about the "normaltime", "othertime", etc crap
; in the zooming-in section, well, I *think* this is because the
; Y-position sometimes needs to start at a half-pixel (i.e. "othertime")
; but usually ("normaltime") starts on a whole pixel.

; Note, it would be possible to speed this up by 1/4, simply by re-using
; the relevant pixels from the fractal calculated last: see this diagram

; We're zooming in to top left corner.

; Original fractal      Fractal needs calculating next
;
; abcd....              a?b?c?d?
; efgh....              ????????
; ijkl....              e?f?g?h?
; mnop....              ????????
; ........              i?j?k?l?
; ........              ????????
; ........              m?n?o?p?
; ........              ????????
;
; where "." has been calculated on original fractal, but will not be used
; for the new one, and ? represents what actually needs to be calculated
; in the new one. (a,b,c,... could be copied from the old one).
;
; Actually, this routine doesn't copy over a,b,c,..., they are recalculated.

; Data_______________________________________________________________

lxcentre:    dd 0
hxcentre:    dd 0
lycentre:    dd 0
hycentre:    dd 0    ; co-ordinates of the window's centre, not used here
                     ; (they are used in the main loop...)
lxs:     dd 0
hxs:     dd 0
lys:     dd 0
hys:     dd 0    ; co-oredinates of the window's top left corner

lxi:     dd 0
hxi:     dd 0
lyi:     dd 0
hyi:     dd 0    ; amount to increment fractal parameter per pixel

ycount:  dw 0
xcount:  dw 0    ; loop counters

nodraw:  db 1    ; whether or not to draw the last one

ydirection: db 1
xdirection: db 1 ; direction of the zoom (0 = left/up, 1=centre, 2=rt/down)
newxdirection: db 1
newydirection: db 1

screenstart:   dd 0    ; screen edisplay offset

; Source_____________________________________________________________

Fractal:
push eax
push ebx
push ecx
push edx
push esi
push edi

; 1) Initialise x and y parameters of fractal to xstart,ystart
mov eax, [lxs]
mov edx, [hxs]
mov [lx], eax
mov [hx], edx
mov eax, [lys]
mov edx, [hys]
mov [ly], eax
mov [hy], edx

; 1.5) Initialise zoom parameters
mov word [lfrac], 1024*63
mov word [hfrac], 1
mov byte [ystart], 0
mov byte [xstart], 0

; 2) Set up screen pointer
mov edi, [fractalseg]

mov word [ycount], FRACHEIGHT
  FracYLoop:

  mov word [xcount], FRACWIDTH
    FracXLoop:
    
    ; 3) Calculate pixel
    call Calculate

    ; 4) Draw pixel
    mov [edi], al

    ; 5) Add X increment to X
    mov eax, [lxi]
    mov edx, [hxi]
    add [lx], eax
    adc [hx], edx

    ; 6) Increment screen position
    inc di
    
    ; 7) If count >127, stop
    dec word [xcount]
    jnz short FracXLoop

  ; 8) Set X to xstart
  mov eax, [lxs]
  mov edx, [hxs]
  mov [lx], eax
  mov [hx], edx

  ; 10) Add Y increment to Y
  mov eax, [lxi]
  mov edx, [hxi]
  add [ly], eax
  adc [hy], edx

  ; 10.5) Draw zoomed last fractal if count%2==0
  mov ax, [ycount]
  and al, 1
  jnz short notthistime
  ; Update zoom position
    mov word [yfracpos], 0
    mov al, [xdirection]
    add [xstart], al
    cmp byte [ydirection], 0
    je short normaltime
    cmp byte [ydirection], 2
    jne short ydirection1
    inc byte [ystart]
    jmp short normaltime
    ydirection1:
    test word [ycount], 2
    jz short othertime
    inc byte [ystart]
    jmp short normaltime
  
  othertime:
    mov word [yfracpos], 8000h

  normaltime:
    cmp byte [nodraw], 0
    jne short dontdraw
    call ZoomTexture
    dontdraw:
    sub word [lfrac], 1024

  notthistime:

  ; 11) If count>127, stop
  dec word [ycount]
  jnz FracYLoop

pop edi
pop esi
pop edx
pop ecx
pop ebx
pop eax
retn

;====================================================================
; SWITCHTEXTURES - switch around the two buffers
;--------------------------------------------------------------------

; This just changes the two buffers when one has been finished; so
; that the new fractal becomes the one that gets drawn to screen, and 
; the old fractal will get written over by the one newly being
; calculated.

; This is a separate function, because if you decide to implement
; the 25% saving described above, you'll need to copy over those
; re-cycled pixels at some point, and this is a good time to do that.

; Source_____________________________________________________________

SwitchTextures:
push eax
push edx

mov eax, [fractalseg]
mov edx, [textureseg]
mov [fractalseg], edx
mov [textureseg], eax

pop edx
pop eax
retn

;====================================================================
;DOBACKGROUND - the background for lo-res part of demo
;--------------------------------------------------------------------

; Just draws the swirly background things, very simple.

DoBackground:
push eax
push ebx
push ecx
push edi

sub ebx, ebx ;0        ; bh=y, bl=x

mov edi, 0A0000h
mov esi, edi
mov cx, 64000 ; ecx = 64000
mov al, bl ;0
rep stosb
mov edi, esi ; 0A0000h

db_loop:
  mov al, bh
  mul bl
  and al, 0fh
  add al, 16
  mov [edi], al
  inc di
  inc bx
  cmp bl, 0
  jne short notnextline
  add di, 320-256
notnextline:
  cmp bx, 256*193
  jb short db_loop

mov di, 256
mov dx, 192

copy_loop:
  mov cx, 32
  rep movsw
  add si, 320-64
  add di, 320-64
  dec dx
  jnz short copy_loop

;mov di, 256*193
;mov cx, 8*320
;mov al, 0
;rep stosb

pop edi
pop ecx
pop ebx
pop eax

retn

;====================================================================
;DRAWSQUARE - fills a square, for showing which way things are going
;--------------------------------------------------------------------

; I won't bother explaining this, you can all manage to draw a square
; by now...

DrawSquare:
push eax
push edx
push esi
push edi

mov si, ax

; bh=starty, ax(now si)=startx
; dx=width and height
; cl=colour
push dx
mov di, bx
shr di, 8
mov ax, 320
mul di
add ax, si

mov edi, 0A0000h
mov di, ax
pop dx

mov al, cl

mov si, dx
ds_yloop:
  mov cx, dx
  rep stosb
  add di, 320
  sub di, dx
  dec si
  jnz short ds_yloop

pop edi
pop esi
pop edx
pop eax
retn

;====================================================================
;DRAWDIRECTIONSQUARES - draw the motion direction indicators
;--------------------------------------------------------------------

; this is pretty trivial too.

DrawDirectionSquares:
push eax
push ebx
push ecx
push edx
push esi
push edi

; Clear all squares
mov bh, 0        ; was 16
mov ax, 0        ; was 16
mov cl, 0
mov dx, 32

mov si, 3
dds_yloop:
  mov di, 3
  dds_xloop:
    cmp di, 2
    jne short dds_drawit
    cmp si, 2
    je short dds_skipit

    dds_drawit:
    call DrawSquare

    dds_skipit:
    add ax, 9*16
    dec di
    jnz short dds_xloop
  mov ax, 0      
  add bh, 5*16   
  dec si
  jnz short dds_yloop

; Draw chosen square
cmp byte [newydirection], 1
jne short drawchosen
cmp byte [newxdirection], 1 
je short afterchosen

drawchosen:
mov ah, 0
mov al, [newydirection]
imul ax, 5*16    
mov bh, al
;mov eax, 0
;mov al, [newxdirection]
movzx eax, byte [newxdirection]
mov si, 9*16
mul si
mov dx, 32               ; square side length
mov cl, 1
call DrawSquare
add bh, 4
add ax, 4
sub dx, 8
mov cl, 0
call DrawSquare         ; clear the inside

afterchosen:
; Draw actual (current) square
cmp byte [ydirection], 1
jne short drawcurrent
cmp byte [xdirection], 1
je short aftercurrent

drawcurrent:
mov ah, 0
mov al, [ydirection]
imul ax, 5*16  
add ax, 0        
mov bh, al
mov ah, 0
mov al, [xdirection]
mov si, 9*16
mul si
add bh, 4
add ax, 4                ; current square start now in bx.
mov dx, 24               ; square side length
mov cl, 2
call DrawSquare

aftercurrent:
pop edi
pop esi
pop edx
pop ecx
pop ebx
pop eax
retn

;====================================================================
;DOFRACTALSECTION - the controllable fractals part of the demo
;--------------------------------------------------------------------

; hopefully what with the background you have already read, this
; routine is self-explanatory.

DoFractalSection:
; Now setup fractal parameters
; Start at preplanned position
mov dword [hxcentre], 000000000h
mov dword [lxcentre], 04afadfffh
mov dword [hycentre], 0fffffffeh
mov dword [lycentre], 08f71bfffh

; and increment by 1/64 per pixel
mov dword [hxi], 0
mov dword [lxi], 1024*65536
mov dword [hyi], 0
mov dword [lyi], 1024*65536

mov dword [screenstart], 0A0000h + (33*320+32)

dfs_fracloop:

; Calculate new hxcentre etc depending on xdirection,ydirection
; (new position = FW/4*(xdirection+1),FH/4*(ydirection+1) on the display)

; xcentre=xcentre+fw/4*(xdirection-1)*xi 
cmp byte [xdirection], 1
je short xchangedone
mov edx, FRACWIDTH/2
mov eax, [lxi]
mul edx
cmp byte [xdirection], 2
jne short xchangeminus
  add [lxcentre], eax
  adc [hxcentre], edx
  jmp short xchangedone
xchangeminus:
  sub [lxcentre], eax
  sbb [hxcentre], edx
xchangedone:

; Same for Y:
cmp byte [ydirection], 1
je short ychangedone
mov edx, FRACHEIGHT/2
mov eax, [lyi]
mul edx
cmp byte [ydirection], 2
jne short ychangeminus
  add [lycentre], eax
  adc [hycentre], edx
  jmp short ychangedone
ychangeminus:
  sub [lycentre], eax
  sbb [hycentre], edx
ychangedone:

; Calculate start hxs,lxs hys,lys to keep hxcentre in middle (at 128,128)

; xs=xcentre-128*xi ys=ycentre-128*yi
mov eax, [lxi]
mov edx, FRACWIDTH/2
mul edx
mov ebx, eax     ; bx is l(xi*128)
mov ecx, edx     ; cx is h(xi*128)
xor ebx, 0ffffffffh
xor ecx, 0ffffffffh
inc ecx         ; cx:bx now negative'd
add ebx, [lxcentre]
adc ecx, [hxcentre]
mov [lxs], ebx
mov [hxs], ecx

; Same for Y:
mov eax, [lyi]
mov edx, FRACHEIGHT/2
mul edx
mov ebx, eax     ; bx is l(xi*128)
mov ecx, edx     ; cx is h(xi*128)
xor ebx, 0ffffffffh
xor ecx, 0ffffffffh
inc ecx         ; cx:bx now negative'd
add ebx, [lycentre]
adc ecx, [hycentre]
mov [lys], ebx
mov [hys], ecx

; Calculate next fractal while we zoom the last one
call Fractal

; Switch the texture buffers, including copying 1/4 of the pixels
call SwitchTextures

xor ecx, ecx ; 0
cmp byte [nodraw], 1
jne alreadydrawing
mov byte [nodraw], 0
call DoBackground
;mov cx, 0
alreadydrawing:
; Double magnification
mov eax, [lxi]
mov edx, [hxi]
shr eax, 1
shr edx, 1
mov [lxi], eax
mov [hxi], edx
mov [lyi], eax
mov [hyi], edx

; Update direction
mov al, [newxdirection]
mov [xdirection], al
mov al, [newydirection]
mov [ydirection], al

call DrawDirectionSquares

jmp dfs_fracloop

dfs_keyhit:

call Getch

cmp al,'7'
jne short not7
mov byte [newxdirection], 0
mov byte [newydirection], 0
not7:

cmp al,'4'
jne short not4
mov byte [newxdirection], 0
mov byte [newydirection], 1
not4:

cmp al,'1'
jne short not1
mov byte [newxdirection], 0
mov byte [newydirection], 2
not1:

cmp al,'8'
jne short not8
mov byte [newxdirection], 1
mov byte [newydirection], 0
not8:

cmp al,'5'
jne short not5
mov byte [newxdirection], 1
mov byte [newydirection], 1
not5:

cmp al,'2'
jne short not2
mov byte [newxdirection], 1
mov byte [newydirection], 2
not2:

cmp al, '9'
jne short not9
mov byte [newxdirection], 2
mov byte [newydirection], 0
not9:

cmp al, '6'
jne short not6
mov byte [newxdirection], 2
mov byte [newydirection], 1
not6:

cmp al,'3'
jne not3
mov byte [newxdirection],2
mov byte [newydirection],2
not3:

cmp al, 27
je breakout

call DrawDirectionSquares

donethecentrechange:
jmp zt_afterkeyhit    

;====================================================================
;MAIN SECTION & MISC
;--------------------------------------------------------------------

Init:
; 1) Setup ES segment
; 2) Allocate RAM
mov dword [textureseg], 10000h ; textureseg is 64k above our segment
mov dword [fractalseg], 20000h ; and fractalseg is 128k above
mov dword [enlargebufferseg], _end
; 3) Do graphics mode
mov ax, 0013h
; Int 10h
int 31h ; TRDOS 386 Video Interrupt
retn

Shutdown:
mov ax, 0003h
;int 10h
int 31h ; TRDOS 386 Video Interrupt
retn

message:
db 'Thanks for watching the modified version of 4ge',39,'S XMaS 94 iNTRo.'
db 13,10,13,10
db 'Get 4ge-xmas.zip from ftp.cdrom.com for the full version, Tseng gfx only.',13,10,13,10, 0

Start:
; DIRECT VGA MEMORY ACCESS
;xor ebx, ebx
mov bh, 5 ; Direct access/map to VGA memory (0A0000h)
;mov eax, _video ; 1Fh
mov al, 1Fh ; sys _video ; TRDOS 386 Video functions
int 40h   ; TRDOS 386 system call
; eax = 0A0000h
and eax, eax
jz short terminate ; error (eax = 0)

call Init
call DoFractalSection
breakout:
call Shutdown
mov ah, 09h
mov esi, message
print_msg:
mov bx, 7
mov ah, 0Eh
pmsg_loop:
lodsb
and al, al
jz short terminate
int 31h	; TRDOS 386 video interrupt
jmp short pmsg_loop	
terminate:
sys _exit   ; INT 40h
here:
jmp short here

align 2

_end: