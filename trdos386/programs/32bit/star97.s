; STAR97.S (NASM version of STAR97.ASM)
;
;                              [ STARFIELD '97 ]
;
; [ Explanation ]
;
; well, here's an explanation of what's going on. first off we generate the
; starfeild data, star structures are qword aligned for easy offset calc's,
; and a floating random number seed is used. the initial random seed is mov'd
; into the very last star structure, and the next random seed is stored in
; the structure below, and this routine is followed to the very first star
; structure. instead using the modulus of the random number and using it as
; the saved value we mask off all the unwanted bits in the random number, and
; then modify the values to fit in the ranges we need. i was looking for a 4
; layer field (00000011b), and 16 scales of white (00001111b). nifty both
; numbers are binary constants, lucky me! most things don't work out so well.
;
; [ Excuses ]
;
; ok, your saying "Big woop!, i've seen 20 byte starfields". well this is
; true, so have i. however all those starfields are just random color shit.
; not only is the randomness shitty, but they don't check for a keypress, and
; they don't even look like a starfield, otherwise there not usable at all!
; this routine has parallax style scrolling of a 4 layer starfield, 16 random
; shades of stars, a fairly random placement of stars, it's fast, and it even
; checks for vertical retrace and keypress!
;
; [ Questions / Answers ]
;
; you may be scratching your head on some of the sections of this code, so
; let me give you my reasoning before you call me a dumbshit.
;
; Q: why don't you use 'in ax,40h' as a random number routine rather than
;    using imul and such?
;
; A: well 'in ax,40h' sucks, it's not random enough, try it. you'll see my
;    reasoning, the method used is much better. btw i got the random routine
;    i'm using by simplifying the random routine used by borland turbo c 3.0
;    i just disassembled the lib call and this is pretty much what i got.
;    otherwise i had no idea how to make random numbers, this concept was new
;    to me!, wow a learning experience.
;
; Q: why don't you use a 4 byte structure, a word for the address, and a byte
;    for the speed, and another byte for the color?
;
; A: well in the main loop you'll see that i add the speed to the address,
;    and that i store the address in di, well this means that if i wanted to
;    add anything to a 16bit register i would have to use another 16bit mem
;    location or register, and i can't otherwise it screws up. i wish there
;    was some kind of addzx instruction which added the contents of a smaller
;    register but internally masked it to the same size as the destination
;    with zero's extended. ohh well there are too many instructions anyways.
;
; Q: only 255 stars?
;
; A: it looks ok doesn't it?!, you can use up to something like 8000 stars if
;    you change the 'mov cl,NSTARS' to 'mov cx,NSTARS'. however the code size
;    will increase two bytes (this was my origional goal 99 bytes), however i
;    was told it would be "cooler" to release something that referenced the
;    current year??
;
; Q: my god your a moron, i could make this much smaller, and still retain
;    all of the kewl features you talk about.
;
; A: great, please do, and send me your changes to brandt@europa.com. i tried
;    to the best of my ability to make it smaller, i even got it to 87 bytes
;    using the 'in ax,40h', and commenting out the random number seed setup,
;    and the imul, along with the following mov. but like i said a couple of
;    questions above 'in ax,40h' looks shitty.
;
; ohh, one last thing i do use the following 80386 instructions:
;
; shl    reg16,immediate > 1
; imul   reg16,mem | reg16,reg16 | immediate
;
;                                      deathlock ; brandt@europa.com

; NASM version: Erdogan Tan, 03/10/2016

NSTARS  EQU     255                     ; number of stars to draw

[Bits 16]   ; Real Mode (MSDOS) Program

[org 100h]  ; MSDOS COM File

start:

 mov    al,13h                          ; function 00h, mode 13h 
 int    10h                             ; bios video interrupt
 mov    dx,03DAh                        ; vga status port for retrace
 push   0A000h                          ; use es for segment overrides
 pop    es                              ; when writing to the screen

;[ initialize starfield variables ]------------------------------------------

 mov    cl,NSTARS                       ; cx = number of stars to initialize
 mov    byte [NSTARS*8+stars+8],1       ; set the begining random number seed

loop1:
 mov    bx,cx                           ; copy cx to bx for indexing
 shl    bx,3                            ; multiply bx by 8 (structure size)
 add    bx,stars                        ; add the start offset of stars to bx

 imul   ax,word [bx+8],4E35h            ; multiply to get next random number
 mov    word [bx],ax                    ; stars[cx].addr = ax
 and    ax,0000001100001111b            ; mask off unwanted values
 add    al,14                           ; make sure al < 30 & al > 14
 mov    word [bx+2],ax                  ; stars[cx].col = ax
 shr    ax,8                            ; shift hi byte to low byte
 inc    al                              ; stars[cx].spd can't be zero
 mov    word [bx+4],ax                  ; stars[cx].spd = ax
 loop   loop1                           ; do this again for the next star

;[ the main loop, holy shit! ]-----------------------------------------------

drawstars:

vend:
 in     al,dx                           ; al = vga status byte (03DAh)
 and    al,08h                          ; keep the vretrace status bit
 jz     short vend                      ; not equal to 1?, check again

 mov    cl,NSTARS                       ; lets draw NSTARS to the screen

loop2:
 mov    bx,cx                           ; copy cx to bx for indexing again
 shl    bx,3                            ; multiply bx by 8 (structure size)
 add    bx,stars                        ; add start offset of stars to bx
 mov    di,word [bx]                    ; di = stars.addr
 mov    [es:di],ah                      ; erase star (ah isn't ever used)
 add    di,word [bx+4]                  ; stars.addr = stars.addr + stars.spd
 mov    al,byte [bx+2]                  ; al = stars.col
 mov    [es:di],al                      ; draw new star to the screen
 mov    word [bx],di                    ; save the updated star posistion 
 loop   loop2                           ; erase/draw all NSTARS to the screen

 in     al,60h                          ; al = port 60h, keyboard
 cmp    al,1                            ; escape key pressed?
 jnz    short drawstars                 ; was escape pressed, no do it again

 mov    al,03h                          ; escape was pressed, lets quit
 int    10h                             ; al = 03h text mode, int 10 video
 retn                                   ; com file trick to exit to dos

stars:                                  ; start of starfield structure