; ****************************************************************************
; mandala.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'mandala.prg')
; ----------------------------------------------------------------------------
; MANDALA.PRG ! TEST program !  TRDOS 386 VGA Functionality test !
;
; 31/10/2017
;
; [ Last Modification: 07/11/2017 ]
;
; Derived from source code of 'MANDALA.EXE' by G bor Papp [rod/mandula]
;          MANDALA.ASM (MSDOS) intro file, 1997
;
; Assembler: NASM 2.11

; (Original source code has been modifed for TRDOS 386 system calls and
; other protected mode (TRDOS 386) interrupts.)
; ****************************************************************************

; 01/03/2017
; 16/10/2016
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
_video 	equ 31
_audio	equ 32
_timer	equ 33
_sleep	equ 34
_msg    equ 35
_geterr	equ 36
_fpsave	equ 37
_pri	equ 38
_rele	equ 39
_fff	equ 40
_fnf	equ 41
_alloc	equ 42
_dalloc equ 43
_calbac equ 44	

[BITS 32] ; 80386 Protected Mode (32 bit) intructions

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


;========================================================================
; MANDALA.ASM - 04/06/1997
;========================================================================

; mandala source (c) 1996/97 by G bor Papp [rod/mandula]
; contact me at s5001@sun10.vsz.bme.hu or rod@inf.bme.hu
; visit us at www.inf.bme.hu/~mandula
;
; this header must stay at the top of this file.
;
; this material is not freeware. you are allowed to copy it without
; restrictions for non-commercial use. no payment of any kind may be charged
; for this product or any combination of products or services including this
; product without the author's authorization and official written license.
; commercial use is strictly prohibited.
;
; you may only look at the source, you may not use it.
; you must not use this code or derivatives in your productions.
; you may use the algorithms in non commercial productions if you credit me.


;========================================================================
; MACROS
;========================================================================

;
;%macro	load 2
;	push %2
;	pop  %1
;%endmacro
;

;========================================================================
; EQUATIONS
;========================================================================

np	 equ 19
addxball equ 160
addyball equ 100
addzball equ 600

t_fadeleft  equ 128
t_left      equ 450
t_right     equ 850
t_faderight equ 978

t_mandalain  equ 1106
t_mandala    equ 1600
t_mandalaout equ 1728

t_stripein  equ 1856
t_stripe    equ 2300
t_stripeout equ 2428

t_firein    equ 2556
t_fireballs equ 3600
t_fireout   equ 3728

decay	equ 30

;========================================================================
; CODE
;========================================================================

[ORG 0] 

main:
		; clear bss
		;mov	edi, bss_start ;
		mov	di, bss_start
		mov	ecx, (bss_end - bss_start)/4
		;xor	eax, eax
		rep	stosd

		; DIRECT VGA MEMORY ACCESS
		; EAX = 31, sysvideo
		; EBX = 0500h, direct access/map to VGA memory (0A0000h)
		sys	_video, 0500h 

		; EAX = 0A0000h
		and	eax, eax
		jz      terminate ; error (eax = 0)

		fninit

		mov	ecx, 2
		loop	$

		fnstcw	word [fpu_cw]
		cmp	byte [fpu_cw+1], 3
		jne	@@out

		;in	al, 21h
		;or	al, 6
		;out	21h, al

		call    genmaze

		;load    es, 0
		
		;cli
		;mov	eax, [es:4*8]
		;mov	[cs:oldint8], eax
		;lea	bx, [cs:newint8]
		;mov	[es:4*8], bx
		;mov	[es:4*8+2], cs

		;mov	al, 36h
		;out	43h, al
		;mov	ax, 428Bh
		;out	40h, al
		;mov	al, ah
		;out	40h, al

		; set timer speed to 1193180/17035 (TRDOS 386)
		; (70 Hz)
		mov	al, 36h
		;out	43h, al
		mov	ah, 1 ; out (byte)
		mov	dx, 43h
		int	34h ; TRDOS 386 - IOCTL interrupt

		mov	al, 8Bh
		;out	40h, al
		mov	dx, 40h
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt

		mov	al, 42h
		;out	40h, al
		;mov	ah, 1 ; out (byte)
		;mov	dx, 40h
		int	34h ; TRDOS 386 - IOCTL interrupt

		;sti

		; Start Timer Event
		; EAX = 33, systimer
		; EBX = 8400h ; current timer setup, callback method
 		; ECX = 1 ; 1 tick
		; EDX = newint8, Timer (event) callback service addr
		sys	_timer, 8400h, 1, newint8

		mov	[timer_event_number], al

		call    gensintable
		call	initwave
		call	mandala

		mov	edi, fire - 4
		;mov	cx, (32*16/2+4)
		mov	ecx, (32*16/2+4)/2
		xor	eax, eax
		;rep	stosw
		rep	stosd

		mov	word [radius], 14
		mov	cl, 7
		mov	edi, balldata1
		xor	ebx, ebx
@@transfcalc:   
		mov	[balloffsets+ebx], edi
		push	ecx
		push	ebx
		call	balltransform
		pop	ebx
		pop	ecx
		add	ebx, 4
		dec	word [radius]
		loop	@@transfcalc

		mov	esi, cubes
		mov	edi, object
		mov	cx, np*3*2
		rep	movsb

 		; Set video mode to 320x200x256 graphics
		mov     ax, 13h
		;int	10h
		int	31h ; TRDOS 386 video interrupt

		call    genlogo
		call    genclouds

;------------------------------------------------------------------------

		xor	eax, eax ; 0

		;mov	[alpha], eax
		;mov	[gamma], ax

		;mov	[skyxstart], eax
		;mov	[angle], ax
		;mov	[xpos], ax

		mov	[timer], eax
		mov	[fadeinval], ax
		;;mov	[mandinit], ax
		;mov	[mandinit], al
		;;mov	[stripeinit], ax
		;mov	[stripeinit], al
		;mov	[deg], ax
		;;mov	[fireinit], ax
		;mov	[fireinit], al

@@timingeffects:
		mov     eax, [timer]

		cmp	eax, t_fadeleft
		jb	@@fadeleft

		cmp	eax, t_left
		jb	@@toleft

		cmp	eax, t_right
		jb	@@toright

		cmp	eax, t_faderight
		jb	@@faderight

		;cmp	word [mandinit], 0
		cmp	byte [mandinit], 0
		je	@@mandinit

		cmp	eax, t_mandalain
		jb	@@mandalain

		cmp	eax, t_mandala
		jb	@@mandala

		cmp	eax, t_mandalaout
		jb	@@mandalaout

		;cmp	word [stripeinit], 0
		cmp	byte [stripeinit], 0
		je	@@stripeinit

		cmp	eax, t_stripein
		jb	@@stripein

		cmp	eax, t_stripe
		jb	@@stripe

		cmp	eax,t_stripeout
		jb	@@stripeout

		;cmp	word [fireinit], 0
		cmp	byte [fireinit], 0
		je	@@fireinit

		cmp	eax, t_firein
		jb	@@firein

		mov	word [fireroot], 0BFBFh

		cmp	eax, t_fireballs
		jb	@@fireballs

		mov	word [fireroot], 0

		cmp	eax, t_fireout
		jb	@@fireout

		jmp	@@todos

@@fadeleft:
		mov	bp, [fadeinval]
		call	fade

@@toleft:
		;load	es,@virtscr
		call    sky
		call    drawmaze
		call    drawvirt

		inc     dword [skyxstart]
		inc     byte [angle]
		add     byte [xpos], 2
		add     byte [ypos], 2

		jmp	@@endeffect

;------------------------------------------------------------------------

@@faderight:
		mov	bp, [fadeoutval]
		call	fade
@@toright:	
		cmp	dword [timer], t_right
		ja	short @@nomax
		mov	word [fadeoutval], 128
@@nomax:
		;load	es,@virtscr
		call    sky
		call    drawmaze
		call    drawvirt

		dec     dword [skyxstart]
		dec     byte [angle]
		sub     byte [xpos], 2
		add     byte [ypos], 4

		mov	word [fadeinval], 0

		jmp	@@endeffect

;------------------------------------------------------------------------

@@stripeinit:   
		mov	esi, stripepal
		call	fillpalette

		call	stripes

		mov	edi, 0A0000h
		mov	ecx, 16000
		xor	eax, eax
		rep	stosd

		;mov	dword ptr cs:shadepatch1,0100878ah ; nop, nop
				; mov al, [bx+100h] <-- nop, nop
		;mov	dword ptr cs:shadepatch2,0100a78ah ; nop, nop
				; mov ah, [bx+100h] <-- nop, nop
 
		;mov	word [stripeinit], 1
		mov	byte [stripeinit], 1
		mov	word [fadeinval], 0
@@stripein:
		mov	bp, [fadeinval]
		call	fade
		jmp	short @@stripe
@@stripeout:
		mov	bp, [fadeoutval]
		call	fade
		jmp	short @@stripewave
@@stripe:	
		mov	word [fadeoutval], 128
@@stripewave:	
		mov	word [morph], 20
		call	distabgen

		mov	dword [_fs], distable
		jmp	short @@mandalawave

;------------------------------------------------------------------------

@@mandinit:     
		mov	esi, mandalapal
		call	fillpalette

		mov	ecx, 16000
		xor	eax, eax
		mov	edi, 0A0000h
		rep	stosd

		;mov	word [mandinit], 1
		mov	byte [mandinit], 1
@@mandalain:
		mov	bp, [fadeinval]
		call	fade
		jmp	short @@mandala
@@mandalaout:
		mov	bp, [fadeoutval]
		call	fade
@@mandala:
		mov	dword [_fs], circtable
		cmp	dword [timer], t_mandala
		ja	short @@mandalawave
		mov	word [fadeoutval], 128
@@mandalawave:	
		mov	ecx, 256
		mov	edi, multable
		movzx	esi, word [deg]
		add	si, si
@@0:            
		mov	ax, [sintable+esi]

		push	eax
		mov	bl, 16
		sar	ax, 4
		neg	al
		mov	[edi+256], al
		pop	eax

		sar	ax, 5
		add	ax, 128
		stosb

		add	si, 8
		and	si, 511
		loop	@@0

		sub	byte [deg], 5

		call	wave
		;call	waitrtc

		jmp	@@endeffect

;------------------------------------------------------------------------

@@fireinit:     
		call	wood

		mov	esi, firepal
		call	fillpalette

		mov	word [fadeinval], 0
		;mov	word [fireinit], 1
		mov	byte [fireinit], 1
@@firein:
		mov	bp, [fadeinval]
		call	fade
		jmp	short @@fireballs

@@fireout:
		mov	bp, [fadeoutval]
		call	fade
@@fireballs:	
		cmp	dword [timer], t_fireballs
		ja	short @@goballs
		mov	word [fadeoutval], 128
@@goballs:	
		call	makefire

		mov	edi, virtscr
		mov	esi, mazeseg
		mov	ecx, 16000
		rep	movsd

		call	drawobject
		call	drawvirt

		add	byte [alpha], 1
		add	byte [beta], -2
		add	byte [gamma], 1

		;jmp	short @@endeffect

@@endeffect:	
		;in	al, 60h
		mov	ah, 0 ; in (byte)
		mov	dx, 60h
		int	34h ; TRDOS 386 - IOCTL interrupt

		dec	al
		jnz	@@timingeffects

;------------------------------------------------------------------------

@@todos:        
		;in	al, 21h
		;and	al, 255-6
		;out	21h, al

		mov	ah, 1
		;int	16h
		int	32h ; TRDOS 386 keyboard interrupt
		jz	short @@pufferisempty

		xor	eax, eax
		;int	16h
		int	32h ; TRDOS 386 keyboard interrupt

@@pufferisempty:
		;xor	ex, ax
		;mov	es, ax ; 0
		;cli
		;mov    eax, cs:oldint8
		;mov    es:[4*8], eax

		;mov    al,36h
		;out    43h,al
		;xor    ax,ax
		;out    40h,al
		;out    40h,al

		; reset timer speed to 1193180/65536 (TRDOS 386)
		; (18.2 Hz)
		mov	al, 36h
		;out	43h, al
		mov	ah, 1 ; out (byte)
		mov	dx, 43h
		int	34h ; TRDOS 386 - IOCTL interrupt

		mov	al, 0
		;out	40h, al
		mov	dx, 40h
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt

		;mov	al, 0
		;out	40h, al
		;mov	ah, 1 ; out (byte)
		;mov	dx, 40h
		int	34h ; TRDOS 386 - IOCTL interrupt

		;; Stop timer event
		movzx	ebx, byte [timer_event_number]
		; bh = 0 -> stop timer event
		sys	_timer

		;sti

		; Reset video mode to 80x25 text mode (03h)
		mov     ax, 3
		;int	10h
		int	31h ; TRDOS 386 video interrupt
@@out:		
		;push	cs
		;pop	ds
		;lea	dx, cs:endmessage
		;mov	ah, 9
		;int	21h

		; Display message
		; ebx = message address
		; ecx/cl = maximum length of the message (<=255)
		; edx/dl = message/text color (0Fh = white) 	
		sys	_msg, endmessage, 255, 0Fh

		;mov	ax,4c00h
		;int	21h

		; TRDOS 386 - sysexit (terminate process)
terminate:
		sys	_exit
;here:
;		jmp	short here

endmessage:
		db	"(c) 1996/97 by rod/mandula",13,10, ;,$
		db	"MANDALA.PRG by Erdogan Tan, 07/11/2017", 13, 10, 0

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
wave:
		mov	esi, multable

		mov	edi, 32

		mov	dh, 0
@@yloop:	
		mov	dl, 0
@@xloop:
		mov	ebx, [_fs]
		mov	cx, [ebx+edi]
		movzx	ebx, cl
shadepatch1:
		;mov	al, 0
		;nop
		;nop
		;mov	al, [bx+100h]

		cmp	byte [stripeinit], 0
		ja	short shadepatch1s
		; mandalainit
		mov	al, 0
		jmp	short shadepatch1m
shadepatch1s:
		; stripeinit
		mov	al, [esi+ebx+100h]
shadepatch1m: 
		mov	bh, [esi+ebx]
		mov	bl, dl
		mov	ebp, esi
		add	bp, bx
		;mov	cl, [esi+ebx]
		mov	cl, [ebp]
		mov	bl, dh
		;mov	bh, [esi+ebx]
		mov	ebp, esi
		add	bp, bx
		mov	bh, [ebp]
		mov	bl, cl
		mov	ebp, mandalaseg
		;add	al, [mandalaseg+ebx+7168]
		add	bp, bx
		add	bp, 7168
		add	al, [ebp]

		inc	dl

		;mov	bl, ch
		;mov	bh, 0
		movzx	ebx, ch
shadepatch2:	
		;mov	ah, 0
		;nop
		;nop
		;mov	ah, [bx+100h]

		cmp	byte [stripeinit], 0
		ja	short shadepatch2s
		; mandalainit
		mov	ah, 0
		jmp	short shadepatch2m
shadepatch2s:
		; stripeinit
		mov	ah, [esi+ebx+100h]
shadepatch2m: 
		mov	bh, [esi+ebx]
		mov	bl, dl
		mov	ebp, esi
		add	bp, bx
		;mov	ch, [esi+ebx]
		mov	ch, [ebp]
		mov	bl, dh
		mov	ebp, esi
		add	bp, bx
		;mov	bh, [esi+ebx]
		mov	bh, [ebp]
		mov	bl, ch
		mov	ebp, mandalaseg
		add	bp, bx
		;add	ah, [mandalaseg+ebx+7168]
		add	bp, 7168
		add	ah, [ebp]

		mov	[0A0000h+edi], ax

		inc	dl
		add	di, 2

		cmp	dl, 0
		jne	@@xloop


		add	di, 320-256
		inc	dh
		cmp	dh, 200
		jne	@@yloop

		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
distabgen:
		mov	ax, [deg]
		;mov	cs:byte ptr @@patch+2,al
		mov	byte [patch], al

		mov	edi, 63999

		mov	dx, [morph]
@@loop:        
		mov	cl, [circtable+edi]
		mov	bl, [degtable+edi]
@@patch:        
		;add	bl, 0ffh
		add	bl, [patch]

		;mov	bh, 0
		and	ebx, 0FFh
		;add	bx, bx
		add	ebx, ebx
		mov	ax, [sintable+ebx]
		idiv	dl

		add	al, cl
		mov	[distable+edi], al
		dec	edi
		jnz	short @@loop

		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
stripes:
		mov	edi, mandalaseg
		mov	ebx, edi
		mov	ecx, 16384
		xor	eax, eax
		rep	stosd

		xor	esi, esi

		;xor	ecx, ecx
		mov	edi, ebx ; mandalaseg
		xor	ebx, ebx
		mov	ecx, 65536
@@1:
		mov	ax, 15
		call	random
		inc	ax

		mov	ebp, edi
		mov	bx, [ctable+esi-2]
		;mov	ah, [edi+ebx]		
		add	bp, bx
		mov	ah, [ebp]
		mov	bx, [ctable+esi]
		mov	ebp, edi
		add	bp, bx
		;mov	ah, [edi+ebx]	
		add	ah, [ebp]
		shr	ah, 1
		add	al, ah
		add	al, 16
		;stosb
		mov	[edi], al
		inc	di	

		add	si, 2
		and	si, 15

		;dec	cx
		dec	ecx
		jnz	short @@1

		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
drawobject:
		call	calcrot

		mov	esi, object
		mov	edi, currentobject
		mov	ecx, np
		call	rotatexyz

		mov	ecx, np
		mov	esi, currentobject
		mov	edi, zlist
@@fillzlist:	
		mov     ax, [esi+4]
		neg	ax
		stosw
		mov	ax, si
		stosw
		add	si, 6
		loop	@@fillzlist

		mov	ecx, zlist
		mov	edx, zlist+(np-1)*4
		call	qsort

		mov	ecx, np
		mov	esi, zlist

		;load	es,@virtscr
@@perspective:
		;movzx	edi, word [esi+2]
		mov	di, [esi+2]

		mov	bx, [edi+4]
		add	bx, addzball
		mov     ax, [edi]
		cwd
		mov	dl, ah
		mov	ah, al
		mov	al, 0
		idiv	bx
		add	ax, addxball
		mov	[ballx], ax

		mov     ax, [edi+2]
		cwd
		mov	dl, ah
		mov	ah, al
		mov	al, 0
		idiv	bx
		add	ax, addyball
		mov	[bally], ax

		pushad

		sub	bx, 400
		sar	bx, 6
		call	drawfireball

		popad

		;add	esi, 4
		add	si, 4

		loop	@@perspective

		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
qsort:
		mov	ebx, ecx
		add	ebx, edx
		rcr	bx, 1
		and	ebx, 0FFFCh

		mov	esi, ecx
		mov	edi, edx

		mov	bx, [ebx]
		sub	si, 4
		add	di, 4
@@repeat:
@@while_i:
		add	si, 4
		cmp	[esi], bx
		jl	short @@while_i
@@while_j:
		sub	di, 4
		cmp	[edi], bx
		jg	short @@while_j
		cmp	esi, edi
		jae	short @@break
		mov	eax, [esi]
		mov	ebp, [edi]
		mov	[esi], ebp
		mov	[edi], eax
		jmp	short @@repeat
@@break:
		sub	si, 4
		add	di, 4

		cmp	ecx, esi
		jae	short @@noleft

		push	edx
		push	edi
		mov	edx, esi
		call	qsort
		pop	edi
		pop	edx
@@noleft:
		cmp	edi, edx
		jae	short @@noright
		mov	ecx, edi
		call	qsort
@@noright:
		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
drawfireball:
		movzx	ebx, bx

		sub	word [ballx], 16
		sub	word [bally], 16

		shl	bx, 2
		mov	esi, [balloffsets+ebx]

		mov	ax, [bally]
		shl	ax, 6
		mov	di, ax
		shl	ax, 2
		add	di, ax
		add	di, [ballx]

		movzx	edi, di

		mov	edx, 32
_yloop:	
		mov	ecx, 32
_xloop:	
		mov	bx, [esi]
		mov	al, [fire+ebx]
		or	al, al
		jz	short _black
		mov	[virtscr+edi], al

		mov	al, [virtscr+edi+320*5+10]
		cmp	al, 192+12
		jb	short _black

		sub	byte [virtscr+edi+320*5+10], 12
_black:	
		inc	di
		add	esi, 2
		loop	_xloop
		add	di, 320-32

		dec	edx
		jnz	short _yloop

		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
balltransform:
		;cld
		finit

		mov	dx, -16
y_loop:	
		mov	cx, -16
		mov	bx, dx
		imul	bx, bx
		mov	[bally], dx
x_loop:
		mov	[ballx], cx
		or	cx, cx
		jne	short notzero
		cmp	dx, 0
		jge	short ygr0
		fldpi
		fld1
		fld1
		fadd

		fdiv
		jmp	short morecalc
ygr0:         
		fldpi
		fld	dword [ddtemp]
		fmul
		jmp	short morecalc
notzero:	
		fild	word [bally]
		fild	word [ballx]
		fpatan
		fldpi
		fadd
morecalc:     
		mov	word [dwtemp], 32
		fild	word [dwtemp]
		fmul
		fldpi
		fdiv

		fistp	word [dwtemp]

		mov	ax, cx
		imul	ax, ax
		add	ax, bx
		mov	[sqr], ax
		fild	word [sqr]
		fsqrt
		fistp	word [sqr]

		mov	ax, [sqr]
		cmp	ax, [radius]
		ja	short black_

		sub	ax, [radius]
		neg	ax
		shl	ax, 5
		add	ax, [dwtemp]
		jmp	short store

black_:
		xor	eax, eax
store:
		stosw
		inc     cx
		cmp	cx, 16
		jl	x_loop

		inc	dx
		cmp	dx, 16
		jl	y_loop

		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
makefire:
		mov	esi, fire+32
		mov	edi, fire
		mov	ecx, 32*16
		xor	ebx, ebx
@@actflame:     
		lodsb
		or	al, al
		jz	short @@black

		cmp	al, decay
		jb	short @@black

		mov	dl, al
		mov	ax, 7
		call	random
		sub	ax, 3
		mov	bx, ax
		mov	ax, decay
		call	random
		sub	dl, al
		mov	[edi+ebx], dl
		inc	edi
		loop	@@actflame
		jmp	short @@actflameready
@@black:        
		mov	byte [edi], 0
		inc	edi
		loop	@@actflame
@@actflameready:
		mov	byte [fire], 0

		mov	edi, fire+15*32
	
		mov	ax, [fireroot]

		mov	cl, 16
		rep	stosw

		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
calcrot:
		movzx	ebx, word [alpha]
		add	bx, bx
		mov	ax, [sintable+ebx]
		mov	[sinalpha], ax
		mov	di, [costable+ebx]

		mov	bx, [beta]
		add	bx, bx
		mov	si, [sintable+ebx]
		mov	bp, [costable+ebx]

		mov	bx, [gamma]
		add	bx, bx
		mov	cx, [sintable+ebx]
		mov	ax, [costable+ebx]
		mov	bx, ax

		imul	bp
		shrd	ax, dx, 8
		mov	[_r11], ax

		mov	ax, cx
		imul	bp
		shrd	ax, dx, 8
		mov	[_r12], ax

		mov	ax, si
		mov	[_r13], ax
		neg	word [_r13]

		mov	ax, di
		imul	cx
		shrd	ax, dx, 8
		mov	[add11], ax

		mov	ax, [sinalpha]
		imul	bx
		shrd	ax, dx, 8
		mov	[add12], ax
		imul	si
		shrd	ax, dx, 8
		sub	ax, [add11]

		mov	[_r21], ax

		mov	ax, [add11]
		imul	si
		shrd	ax, dx, 8
		sub	ax, [add12]
		mov	[_r32], ax

		mov	ax, di
		imul	bx
		shrd	ax, dx, 8
		mov	[add11], ax

		mov	ax, [sinalpha]
		imul	cx
		shrd	ax, dx, 8
		mov	[add12], ax
		imul	si
		shrd	ax, dx, 8
		add	ax, [add11]
		mov	[_r22], ax

		mov	ax, [add11]
		imul	si
		shrd	ax, dx, 8
		add	ax, [add12]
		mov	[_r31], ax

		mov	ax, [sinalpha]
		imul	bp
		shrd	ax, dx, 8
		mov	[_r23], ax

		mov	ax, di
		imul	bp
		shrd	ax, dx, 8

		mov	[_r33], ax

		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
rotatexyz:
@@rotxyz:       mov	ax, [_r12]
		imul	word [esi]
		shrd	ax, dx, 8
		mov	bx, ax

		mov	ax, [_r22]
		imul	word [esi+2]
		shrd	ax, dx, 8
		add	bx, ax

		mov	ax, [_r32]
		imul	word [esi+4]
		shrd	ax, dx, 8
		add	ax, bx
		mov	[edi+2], ax

		mov	ax, [_r11]
		imul	word [esi]
		shrd	ax, dx, 8
		mov	bx, ax

		mov	ax, [_r21]
		imul	word [esi+2]
		shrd	ax, dx, 8
		add	bx, ax

		mov	ax, [_r31]
		imul	word [esi+4]
		shrd	ax, dx, 8
		add	ax, bx
		mov	[edi], ax

		mov	ax, [_r13]
		imul	word [esi]
		shrd	ax, dx, 8
		mov	bx, ax

		mov	ax, [_r23]
		imul	word [esi+2]
		shrd	ax, dx, 8
		add	bx, ax

		mov	ax, [_r33]
		imul	word [esi+4]
		shrd	ax, dx, 8
		add	ax, bx
		mov	[edi+4], ax

		add	edi, 6
		add	esi, 6
		dec	ecx
		jnz	@@rotxyz

		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
wood:
		mov	edi, mazeseg
		mov	eax, 0E1E0E1E0h
		mov	ecx, 16000
		rep	stosd

		mov	ecx, 65536
		xor	ebp, ebp
_woo_:		
		mov	eax, ebp
		add	ebp, ebx
		and	ax, 7

		;mov	bl, [edi-320]
		mov	edx, edi
		sub	dx, 320
		mov	bl, [edx]
		or	al, al
		jz	short _0_
		dec	al
		jz	short _1_
		dec	al
		jz	short _2_
		dec	al
		jz	short _3_

_4_:		
		mov	[edi], bl
		inc	di
		loop	_woo_

		retn

_0_:		cmp	bl, 253
		ja	short _4_
		inc	bl
_01_:		
		inc	bl
		jmp	short _4_

_1_:		cmp	bl, 194
		jb	short _4_
		dec	bl
_11_:		
		dec	bl
		jmp	short _4_

_2_:		
		;cmp	bl, [edi-321]
		mov	edx, edi
		sub	dx, 321
		cmp	bl, [edx]
_21_:		
		je	short _4_
		jb	short _01_
		jmp	short _11_

_3_:		
		;cmp	bl, [edi-319]
		mov	edx, edi
		sub	dx, 319
		cmp	bl, [edx]
		jmp	short _21_

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
mandala:
		mov	esi, mandata
		mov	edi, x0
		mov	ecx, 24
		rep	movsd

		fninit

		mov	ebx, 85
		xor	edi, edi
yloop_:
		fld	qword [x0]
		fstp	qword [x]

		mov	edx, 85
xloop_:
		fld	qword [x]
		fmul	qword [x]
		fld	qword [y]
		fmul	qword [y]
		fadd
		fsqrt
		fstp	qword [r]
		fld	qword [y]
		fld	qword [x]
		fpatan
		fstp	qword [phi]

		xor	ecx, ecx
@@iter:		
		inc	cx

		fld1
		fdiv	qword [r]
		fld	st0
		fmul
		fst	qword [r]

		fld	qword [phi]
		fadd	qword [phi]
		fchs
		fstp	qword [phi]

		fcomp	qword [bailout]

		fnstsw	word [status]

		test	byte [status+1], 01000101b
		jz	short out_

		jmp	short @@iter
out_:		
		fld	qword [phi]
		fsin
		fmul	qword [r]
		frndint
		fistp	word [status]
		shl	cx, 3
		add	cx, [status]
		cmp	cx, 256
		jb	short @@iterout
		mov	cl, 0
@@iterout:	
		mov	ch, cl

		;mov	[mandalaseg+edi], cl
		;mov	[mandalaseg+edi+85], cl
		;mov	[mandalaseg+edi+85*2], cx

		;mov	[mandalaseg+edi+256*85], cl
		;mov	[mandalaseg+edi+256*85+85], cl
		;mov	[mandalaseg+edi+256*85+85*2], cx

		;mov	[mandalaseg+edi+256*85*2], cl
		;mov	[mandalaseg+edi+256*85*2+85], cl
		;mov	[mandalaseg+edi+256*85*2+85*2], cx

		;mov	[mandalaseg+edi+256*85*3], cl
		;mov	[mandalaseg+edi+256*85*3+85], cl
		;mov	[mandalaseg+edi+256*85*3+85*2], cx

		mov	ebp, mandalaseg
		mov	bp, di
		mov	[ebp], cl
		add	bp, 85
		mov	[ebp], cl
		add	bp, 85
		mov	[ebp], cx

		mov	ebp, mandalaseg
		mov	bp, di
		add	bp, 256*85
		mov	[ebp], cl
		add	bp, 85
		mov	[ebp], cl
		add	bp, 85
		mov	[ebp], cx

		mov	ebp, mandalaseg
		mov	bp, di
		add	bp, 256*85*2
		mov	[ebp], cl
		add	bp, 85
		mov	[ebp], cl
		add	bp, 85
		mov	[ebp], cx

		mov	ebp, mandalaseg
		mov	bp, di
		add	bp, 256*85*3
		mov	[ebp], cl
		add	bp, 85
		mov	[ebp], cl
		add	bp, 85
		mov	[ebp], cx

		fld	qword [x]
		fadd	qword [addx]
		fstp	qword [x]

		inc	di
		dec	edx
		jnz	xloop_

		add	di, 256-85
		fld	qword [y]
		fadd	qword [addy]
		fstp	qword [y]

		dec	ebx
		jnz	yloop_

		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
initwave:
		mov	esi, circtable
		mov	edi, degtable

		;mov	[sz255], __float32__ (1275.0)

		mov	dx, -100
_yloop_:	
		mov	cx, -160
		mov	bx, dx
		imul	bx, bx
_xloop_:	
		mov	ax, cx
		imul	ax, ax
		add	ax, bx
		mov	[status], ax
		fild	word [status]
		fsqrt
		fistp	word [esi]

		cmp	cx, 0
		jne	short @@notzero
		cmp	dx, 0
		jge	short @@ygr0
		mov	al, 64+128
		jmp	short @@storedeg
@@ygr0:		
		mov	al, 64
		jmp	short @@storedeg
@@notzero:
		mov	[status], dx
		fild	word [status]
		mov	[status], cx
		fild	word [status]
		fpatan
		fmul	dword [sz255]
		fldpi
		fadd	st0, st0
		fdiv
		fistp	word [status]
		mov	ax, [status]
@@storedeg:	
		mov	[edi], al
		inc	di
		inc	si
		inc	cx
		cmp	cx, 160
		jne	short _xloop_
		inc	dx
		cmp	dx, 100
		jne	_yloop_

		mov	edi, multable

		mov	bx, 0
@@yloop2:	
		mov	cx, -127
@@xloop2:	
		mov	ax, cx
		imul	bx
		shrd	ax, dx, 7
		add	al, 128
		stosb
		inc	cx
		cmp	cx, 128
		jle	short @@xloop2
		inc	bx
		cmp	bx, 256
		jbe	short @@yloop2

		retn

;oldint8:	dd      ?

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
newint8:	; Timer callback service for TRDOS 386

		inc     dword [timer]

		cmp	word [fadeinval], 128
		je	short _n1
		inc	word [fadeinval]
_n1:		
		cmp	word [fadeoutval], 0
		je	short _n2
		dec	word [fadeoutval]
_n2:
		;jmp	dword [oldint8]
		
		sys	_rele ; TRDOS 386 : return from callback service 

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
gensintable:
		xor     edi, edi
		mov     ebx, 65535
		mov     ebp, 2*804

		xor     esi, esi
		;mov	cx, 320
		mov     ecx, 320
@@singen:       
		mov     eax, edi
		shr     eax, 8
		mov     [sintable+esi], ax

		inc     esi
		inc     esi

		mov     eax, edi
		imul    ebp
		shrd    eax, edx, 16
		sub     ebx, eax
		mov     eax, ebx
		imul    ebp
		shrd    eax, edx, 16
		add     edi, eax

		loop    @@singen

		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
drawmaze:
		xor     ebx, ebx
		mov     edi, virtscr+32000+160-2
@@draw:         
		push    ebx

		mov     ax, [dist+ebx]
		movzx 	esi, word [angle]
		add     si, si
		mov     cx, [costable+esi]
		push    eax
		imul    cx
		shrd    ax, dx, 8
		mov     cx, ax
		mov     bp, ax
		neg     bp
		pop     eax
		mov     bx, [sintable+esi]
		imul    bx
		shrd    ax, dx, 8
		mov     dx, ax
		mov     si, ax

		shl     cx, 8
		shl     dx, 8
		add     ch, [xpos]
		add     dh, [ypos]

		;push	ds
		;load	ds, @mazeseg

		;std
		push    edi
		push	ecx
		push	edx
		%rep	80
			mov     bl, ch
			mov     bh, dh
			mov     ah, [mazeseg+ebx]
			add     cx, si
			add     dx, bp
			mov     bl, ch
			mov     bh, dh
			mov     al, [mazeseg+ebx]
			add     cx, si
			add     dx, bp
			;stosw
			mov	[edi], ax
			dec	edi
			dec	edi
		%endrep
		pop     edx
		pop	ecx
		pop	edi

		inc     edi
		inc     edi

		;cld
		%rep	80
			sub     cx, si
			sub     dx, bp
			mov     bl, ch
			mov     bh, dh
			mov     al, [mazeseg+ebx]
			sub     cx, si
			sub     dx, bp
			mov     bl, ch
			mov     bh, dh
			mov     ah, [mazeseg+ebx]
			stosw
		%endrep

		;pop	ds

		add     edi, 160-2

		pop     ebx

		inc     ebx
		inc     ebx
		;cmp	bx, 200
		cmp	bl, 200
		jb      @@draw

		mov     edi, virtscr+32000-320
		;mov	ecx, 320 ; ?
@@3d:           
		push    edi
_d1:            
		mov     dl, 10
		;mov	bl, es:[di]
		mov	bl, [edi]
		dec     bl
_d2:
		add     edi, 320
		cmp     edi, virtscr+64000
		ja      short @@endrow
		;mov	al, es:[di]
		mov	al, [edi]
		or      al, al
		jnz     short _d1

		or      dl, dl
		jz      short _d2
		dec     dl

		;mov     es:[di], bl
		mov	[edi], bl
		sub     bl, 2

		jmp     short _d2
@@endrow:       
		pop     edi
		inc     edi
		cmp     edi, virtscr+32320
		jb      short @@3d

		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
genmaze:
		;mov	edi, mazeseg+32768
		mov	edi, 32768
		mov	ecx, 65536
@@pixloop:
		mov     eax, edi
@@randseed:     
		;imul	ax, 01234h
		imul    ax, word [c_randseed] ; 01
		add     ax, di
		;mov	word [cs:@@randseed+2], ax
		mov     [c_randseed], ax 

		sar     ax, 0Ch
		inc     ax

		mov	ebp, mazeseg
		mov	bp, di
		sub	bp, 256
		;mov    ah, [edi-256]
		mov	ah, [ebp]
		dec	bp
		;add	ah, [edi-257]
		add	ah, [ebp]
		rcr     ah, 1
		add     al, ah
		and     al, 127
		add     al, 128
		;;stosb
		;mov	[edi], al
		mov	[mazeseg+edi], al
		inc	di
		loop    @@pixloop

		;std
		mov	edi, mazeseg+65535
		mov     esi, mazeseg+32767
		mov     dl, 128
@@row2:         
		;mov	ecx, 256
		mov	cx, 256
		;rep	movsb
@@row2_loop1:
		mov	al, [esi]
		dec	si
		mov	[edi], al
		dec	di
		loop	@@row2_loop1
		
		add     si, 256
		mov     cx, 256
		;rep	movsb
@@row2_loop2:
		mov	al, [esi]
		dec	si
		mov	[edi], al
		dec	di
		loop	@@row2_loop2
		
		dec     dl
		jnz     short @@row2

		mov     edx, 65536-256
		mov     ebx, 256
		mov	esi, mazeseg
		mov	ebp, esi ; mazeseg
		call    alias

		;cld
		mov	esi, mazedat
		mov	edi, mazeseg
		mov     dl, 16
@@gen:          
		lodsw
		mov     bx, ax
		xor     eax, eax

		mov     ebp, 16
@@line:         
		rcl     bx, 1
		jnc     short @@notblack
		push    edi

		mov     dh, 16
@@row:          
		;mov	cx, 8
		mov	cl, 8
		;rep	stosw
@@row1:
		mov	[edi], ax
		inc	di
		inc	di
		loop	@@row1

		add     di, 256-16
		dec     dh
		jnz     short @@row

		pop     edi
@@notblack:     
		add     di, 16
		dec     ebp
		jnz     short @@line

		add     di, 256*15
		dec     dl
		jnz     short @@gen


		xor     ebx, ebx
		;mov	cx, 30
		mov	cl, 30
@@gendist:      
		mov     ax, 256*50
		;cwd
		xor	dx, dx		
		idiv    cx
		mov     [dist+ebx], ax
		;inc	cx
		;inc	ebx
		;inc	ebx
		inc	cl
		inc	bl
		inc	bl
		;cmp	bx, 200
		cmp	bl, 200
		jne	short @@gendist

		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
genclouds:
		mov	edi, palette

		;mov	ecx, 768
		xor     eax, eax
		;rep	stosb
		mov	ecx, 768/4
		rep	stosd

		call    setpalette

		;load    es, 0A000h

		;mov	ecx, 100
		mov	cl, 100
		mov     word [linexstart], 0
		mov     word [linexend], 639
@@backloop:     
		mov     [liney], cx
		mov     eax, ecx
		shr     ax, 1
		add     al, 10
		mov     [color], al
		call    horizline
		;dec	ecx
		dec	cl
		jns     short @@backloop

		mov     ebp, 150
@@cloudloop:    
		mov     eax, 640-20
		mov	ebx, random
		call    ebx
		add     ax, 10
		mov     [xc], ax
		mov     ax, 80
		call    ebx
		add     ax, 10
		mov     [yc], ax
		mov     ax, 10
		call    ebx
		inc     ax
		mov     [radius], ax
		mov     ax, 50
		call    ebx
		add     al, 30
		mov     [color], al
		call    filledcircle
		dec     ebp
		jnz     short @@cloudloop

		call    copy

		xor     edi, edi
@@smearlooprow: 
		xor     ebx, ebx
		mov     dh, 0
@@smearloopcol: 
		mov     esi, edi
		add     esi, ebx
		mov     dl, [clouds+esi]
		mov     cl, dh
		shr     cl, 1
		add     cl, 10
		cmp     dl, cl
		je      short @@marad
		mov     ax, 50
		call    random
		add     dl, al
@@marad:        
		mov     ax, 31
		call    random
		sub     ax, 15
		add     si, ax
		mov     [0A0000h+esi], dl
		add     bx, 640
		inc     dh
		cmp     dh, 100
		jb      short @@smearloopcol
		inc     edi
		cmp     di, 640
		jb      short @@smearlooprow

		call    copy

		mov     ebx, 640
		mov     edx, 64000-640
		mov	esi, clouds
		mov	ebp, 0A0000h
		call    alias
		mov	ebx, 640
		;mov    edx, 64000-640
		mov	esi, clouds
		mov	ebp, 0A0000h
		call    alias

		call    copy

		mov	edi, 0A0000h
		mov     ecx, 16000
		xor     eax, eax
		rep     stosd

		mov	esi, mazepal
		call	fillpalette

		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
genlogo:
		mov	word [randseed], 35212

		mov	edi, palette

		;mov	ecx, 768
		xor     eax, eax
		;rep	stosb
		mov	ecx, 768/4
		rep	stosd

		call    setpalette

		mov	esi, logo
		mov     edi, 0A0000h + (40+85*320)

		mov     dh, 15
_col:
		mov     dl, 15
_row:          
		lodsb
		mov     bh, al
		mov     cl, 8
@@byte:         
		xor     eax, eax
		rcl     bh, 1
		jnc     short _black_
		mov     ax, 0FFFFh
		call    random
		and     ax, 7F7Fh
_black_:        
		mov     [edi+320], ax
		stosw
		loop    @@byte

		dec     dl
		jnz     short _row

		add     edi, 320+320-120*2
		dec     dh
		jnz     short _col
@@alias:   
		mov     edx, 3
@@aliasonce:
		xor     ah, ah
		mov	esi, 0A0000h
		mov	edi, esi ; 0A0000h

		mov     ebp, 160
@@aliaschar:    
		mov     ecx, 320
@@aliasrow:     
		lodsb
		add     al, [esi]
		adc     ah, 0
		add     al, [esi+319]
		adc     ah, 0
		add     al, [esi+320]
		adc     ah, 0
		shr     ax, 2
		stosb
		loop    @@aliasrow

		dec     ebp
		jnz     short @@aliaschar

		dec     edx
		jnz     short @@aliasonce

		mov	esi, logopal
		call	fillpalette

		xor	ebp, ebp
		mov	ebx, 64
@@fadein:	
		;call	waitrtc
		call	fade
		inc	ebp
		inc	ebp
		dec	ebx
		jnz	short @@fadein

		mov	dword [timer], 0
@@wait:		
		cmp	dword [timer], 100
		jb	short @@wait

		;mov	ebx, 64
		mov	bl, 64
@@fadeout:	
		;call	waitrtc
		call	fade
		dec	ebp
		dec	ebp
		;dec	ebx
		dec	bl
		jnz	short @@fadeout

		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
alias:
		mov     edi, ebx ; offset (start) ; bx
		add	esi, ebx ; DS segment + offset (start) ; [di]
		add	edi, ebp ; * ; ES segment + offset (start)
		; dx = offset (stop) ; **
		mov	ebp, esi ; *** DS segment + Offset (start)
		sub	bp, bx 	 ; **** DS seg +  65536 - offset (start)
		xor     ecx, ecx 
		add	ebx, esi ; ebx == [di+bx] 
				 ; ebp == [di-bx]
				 ; esi == [di]
				 ; edi == es:[di]
@@aliasloop:    
		mov     cl, [esi] ; [di]
		shl     cx, 2
		mov     eax, ecx
		mov     cl, [esi+1] ; [di+1]
		mov     ch, 0
		shl     cx, 1
		add     ax, cx
		mov     cl, [esi-1] ; [di-1]
		mov     ch, 0
		shl     cx, 1
		add     ax, cx
		mov	cl, [ebx] ; [di+bx]
		mov     ch, 0
		shl     cx, 1
		add     ax, cx
		mov	cl, [ebp] ; ***, **** ; [di-bx]		
		mov     ch, 0
		shl     cx, 1
		add     ax, cx
		mov	cl, [ebp-1] ; ***, **** ; [di-bx-1]
		mov     ch, 0
		add     ax, cx
		mov	cl, [ebp+1] ; ****, **** ; [di-bx+1]
		add     ax, cx
		mov     cl, [ebx+1] ; [di+bx+1]
		add     ax, cx
		mov     cl, [ebx-1] ; [di+bx-1]
		add     ax, cx
		shr     ax, 4
		mov	[edi], al ; * ; [es:di]
		inc	di
		inc     si
		inc	bp
		inc	bx
		cmp	di, dx ; *, **
		jb      short @@aliasloop
		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
copy:
		mov	edi, clouds
		mov	esi, 0A0000h
		mov     ecx, 16000
		rep     movsd
		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
filledcircle:
		xor     ebx, ebx
		;movzx	ecx, word [radius]
		mov	cx, [radius]
		mov     edx, ecx
		shl     dx, 1
		neg     dx
		add     dx, 3
@@circloop:     
		cmp     bx, cx
		jge     short @@vege

		call    @@circline
		cmp     dx, 0
		jge     short _fc1

		mov     ax, bx
		shl     ax, 2
		add     ax, 6
		add     dx, ax
		jmp     short @@ki
_fc1:            
		mov     ax, bx
		sub     ax, cx
		shl     ax, 2
		add     ax, 10
		add     dx, ax

		dec    	cx
@@ki:           
		inc     bx
		jmp     short @@circloop

@@vege:         cmp     bx, cx
		jne     @@vege2
@@circline:
		mov     ax, [yc]
		sub     ax, cx
		mov     [liney], ax
		mov     ax, [xc]
		sub     ax, bx
		mov     [linexstart], ax
		mov     ax, [xc]
		add     ax, bx
		mov     [linexend], ax
		call    horizline

		mov     ax, [yc]
		add     ax, cx
		mov     [liney], ax
		call    horizline
		mov     ax, [yc]
		sub     ax, bx
		mov     [liney], ax
		mov     ax, [xc]
		sub     ax, cx
		mov     [linexstart], ax 
		mov     ax, [xc]
		add     ax, cx
		mov     [linexend], ax
		call    horizline

		mov     ax, [yc]
		add     ax, bx
		mov     [liney], ax
		call    horizline
@@vege2:
		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
horizline:
		pushad
		mov	edi, 0A0000h
		mov	bx, [linexstart]

		mov     di, [liney]
		mov     ax, di
		shl     di, 9
		shl     ax, 7
		add     di, ax
		add     di, bx

		;movzx	ecx, word [linexend]
		mov	cx, [linexend]
		sub     cx, bx
		inc     cx
		mov     al, [color]
		rep     stosb
		popad

		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
sky:
		mov	ebx, [skyxstart]
		mov	edi, virtscr
		mov	ebp, 100
@@skyloop:
		mov	esi, ebx
		add	esi, clouds
		mov	ecx, 320
		rep	movsb

		add	ebx, 640
		dec	ebp
		jnz	short @@skyloop

		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
fillpalette:
		;cld
		mov	edi, palette

		xor	eax, eax
		stosw
		stosb
		;xor	ecx, ecx
@@set:		
		lodsw
		xchg	cx, ax
		jcxz	@@end
		lodsw
		xchg	bx, ax
		lodsw
		xchg	dx, ax
		lodsw
@@col:		
		mov	[edi], bh
		mov	[edi+1], dh
		mov	[edi+2], ah

		add	bx, [esi]
		add	dx, [esi+2]
		add	ax, [esi+4]

		add	edi, 3
		;loop	@@col
		dec	cx
		jnz	short @@col

		add	esi, 6
		jmp	short @@set
@@end:
		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
setpalette:
		mov	esi, palette
		mov     dx, 03C8h
		xor     al, al
		;out	dx, al
		mov	ah, 1 ; outb
		int	34h	
		inc     dl
		mov     ecx, 768
setpal_loop:
		;rep	outsb
		lodsb
		;out	dx, al
		mov	ah, 1
		int	34h
		loop	setpal_loop
		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
fade:
		;cld
		mov	esi, palette
		mov     dx, 03C8h
		xor     eax, eax
		;out	dx, al
		mov	ah, 1 ; outb
		int	34h	

		inc     dl
		mov     ecx, 768
@@3:            
		sub	ah, ah
		lodsb
		imul    ax, bp
		shr     ax, 7
		;out	dx, al
		mov	ah, 1 ; outb
		int	34h	
		loop	@@3
		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
drawvirt:
		;call	waitrtc

		mov	esi, virtscr
		mov	edi, 0A0000h
		mov     ecx, 16000
		rep     movsd
		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
;waitrtc:
;		;cli
;		mov 	dx, 3DAh
;		mov	ah, 0 ; inb
;@@wait1:
;		;in 	al, dx
;		int	34h	
;		test 	al, 08h
;		jz 	short @@wait1
;@@wait2:
;		;in 	al, dx
;		int	34h	
;		test	al, 08h
;		jnz  	short @@wait2
;		;sti
;		retn

;------------------------------------------------------------------------
;
;------------------------------------------------------------------------
random:
		push    ebx
		push	ecx
		push	edx
		mov     cx, ax
		mov     ax, 75
		mov     bx, [randseed]
		mul     bx
		add     ax, 74
		sbb     ax, dx
		mov     [randseed], ax
		xor     dx, dx
		div     cx
		mov     ax, dx
		pop	edx
		pop	ecx
		pop	ebx
		retn

;========================================================================
; DATA
;========================================================================

align 2

		dw	-257
ctable:		dw	-256,-255,1,257,256,255,-1,-257

randseed:
		dw      0
mazedat:
		dw      1000100100000100b
		dw      1110101111010111b
		dw      0000101001010000b
		dw      1011101101011011b
		dw      1000100100000010b
		dw      1011110101111010b
		dw      1000010001000010b
		dw      1101010111101110b
		dw      0001010100000000b
		dw      0111011110111111b
		dw      0000000000100000b
		dw      1111011011101111b
		dw      0100000000001000b
		dw      0101111011111101b
		dw      0000001010000001b
		dw      1111101111101101b

;------------------------------------------------------------------------
logo:

 db 2,2,0,2,0,152,17,252,0,8,4,128,0,16,0,2,135,0,7,0
 db 80,28,127,128,28,3,0,0,56,0,7,134,0,7,32,120,8,56,192,28
 db 131,0,0,57,0,7,142,0,47,64,124,24,48,32,189,7,0,1,122,0
 db 7,207,0,27,128,50,24,48,48,110,7,64,0,220,0,15,203,64,9,128
 db 51,24,48,80,38,3,128,0,76,0,13,217,128,17,200,49,184,48,16,71
 db 33,128,0,142,64,44,209,0,16,240,113,176,32,16,67,193,128,0,135,128
 db 24,240,160,255,224,48,176,32,51,255,133,128,7,255,0,24,240,192,54,224
 db 52,244,40,80,219,131,160,1,183,0,60,224,192,32,224,56,120,112,16,131
 db 129,192,1,7,0,56,112,192,64,112,120,112,96,49,1,193,192,2,3,128
 db 48,112,96,64,112,112,48,96,225,1,193,132,66,3,128,116,120,104,208,120
 db 240,49,127,195,65,227,248,198,131,192,248,236,241,240,204,152,120,255
 db 7,195,51,255,143,134,96

align 2

;------------------------------------------------------------------------
cubes:
		dw	 100, 100, 100
		dw	-100,-100,-100
		dw	-100, 100, 100
		dw	 100,-100, 100
		dw	 100, 100,-100
		dw	-100,-100, 100
		dw	 100,-100,-100
		dw	-100, 100,-100

		dw	 50, 50, 50
		dw	-50, 50, 50
		dw	 50,-50, 50
		dw	 50, 50,-50
		dw	-50,-50, 50
		dw	 50,-50,-50
		dw	-50, 50,-50
		dw	-50,-50,-50
		dw	0,0,0

;------------------------------------------------------------------------

logopal:
		dw      48,103,114,163,103,114,163
		dw	55,4944,5472,7824,204,195,153
		dw	0

mazepal:
		dw	127,17*256,14*256,34*256,46*2,49*2,29*2
		dw	128,24*256,4*256,0,78,118,126
		dw	0

mandalapal:
		dw      127,13*256,10*256,26*256,100,102,74
		dw	128,63*256,63*256,63*256,-100,-102,-74
		dw	0

firepal:
		dw	63,256,57,0,256,57,0
		dw	64,63*256,14*256,0,-8,142,85
		dw	64,61*256,49*256,21*256,8,57,171
		dw	64,0,14*256,26*256,252,199,150
		dw	0

stripepal:
		dw	143,5376,512,512,68,81,18
		dw	0

c_randseed:
		dw	01234h
align 4

ddtemp:		dd	1.5
sz255:		dd	1275.0

align 8

mandata:	dq	-1.7473876,-1.7473876,1.72384644,0.0415735
		dq	-0.0410288,8.0

bss_start:

ABSOLUTE bss_start

;========================================================================
; UNINITIALIZED DATA
;========================================================================

alignb 2

sintable:	resw	64
costable:	resw	256

palette:	resb	768

fadeinval:	resw	1
fadeoutval:	resw	1
;mandinit:	resw	1

dist:		resw	200
angle:		resw	1
xpos:		resb	1
ypos:		resb 	1

alignb 4

xc:		resw	1
yc:		resw	1
radius:		resw	1
color:		resb	1

alignb 4

liney:		resw	1
linexstart:	resw	1
linexend:	resw	1

skyxstart:	resd	1

timer:		resd	1

deg:		resw	1

alignb 8

x0:		resq	1
x:		resq	1
y:		resq	1
addx:		resq	1
addy:		resq	1
bailout:	resq	1

r:		resq	1
phi:		resq	1
status:		resw	1

alignb 4
		resd	1
fire:		resb	32*16
		resd	1

balldata1:	resw	32*32
balldata2:	resw	32*32
balldata3:	resw	32*32
balldata4:	resw	32*32
balldata5:	resw	32*32
balldata6:	resw	32*32
balldata7:	resw	32*32

balloffsets:	resd	7

sqr:		resw	1
dwtemp:		resw	1
;ddtemp:	resd	1

alpha:		resw	1
beta:		resw	1
gamma:		resw	1

sinalpha:	resw	1

_r11:		resw	1
_r12:		resw	1
_r13:		resw	1
_r21:		resw	1
_r22:		resw	1
_r23:		resw	1
_r31:		resw	1
_r32:		resw	1
_r33:		resw	1

add11:		resw	1
add12:		resw	1

		resb	1
		resw	1

object:		resw	3*(np-2)


currentobject:	resw	3*np

alignb 4

zlist:		resw	2*np

ballx:		resw	1
bally:		resw	1

;fireinit:	resw	1

;sz255:		resd	1
;stripeinit:	resw	1
fireinit:	resb	1
stripeinit:	resb	1
mandinit:	resb	1
patch:		resb 	1
morph:		resw	1

fpu_cw:		resw	1

_fs:		resd	1
fireroot:	resw	1
;patch:		resb	1

timer_event_number: resb 1

alignb 65536

virtscr:
		resb	64000
		resb	1536

mazeseg:
		resb	65535
		resb	1

clouds:
		resb	64000
		resb	1536	

mandalaseg:
		resb	65535
		resb	1

distable:
		resb	64000
		resb	1536

circtable:
		resb	64000
		resb	1536

degtable:
		resb	64000
		resb	1536	

multable:
		resb	65535
		resb	1

bss_end:
