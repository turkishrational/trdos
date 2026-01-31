; FASM for DOS example: MANDEL.ASM
; TRDOS 386 Adaptation: Erdogan Tan - October 9, 2024
; ---------------------------------------------------
; MANDEL32.ASM

; Mandelbrot Set - fasm example program

; requires FPU

	;org	100h

	use32
	;org	0x0

 	mov	bh,5 ; Direct access/map to VGA memory
 	;sys _video
	mov	eax,31 ; sysvideo
	int	40h
	
	; eax = 0A0000h
	cmp	eax, 0A0000h ; VGA memory address
	jne	exit_process ; error (eax = 0)
	mov	edi,eax	

	;mov	ax,13h
	;int	10h
	
	; set video mode to 13h
	mov	al,13h 	; function 00h, mode 13h 
	int	31h ; TRDOS 386 - Video interrupt

	;push	0A000h
	;pop	es

	;mov	dx,3C8h
	;xor	al,al
	;out	dx,al
	;inc	dl
	;mov	cx,64
    ;vga_palette:
	;out	dx,al
	;out	dx,al
	;out	dx,al
	;inc	al
	;loop	vga_palette

	mov	edx,3C8h
	xor	al,al
	mov	ah,1 ; outb
	int	34h ; TRDOS 386 IOCTL
	inc	dl
	mov	ecx,64
    vga_palette:
	int	34h
	int	34h
	int	34h
	inc	al
	loop	vga_palette

	;xor	di,di
	;xor	dx,dx
	xor	edx,edx

	finit
	fld	[y_top]
	fstp	[y]
screen:
	;xor	bx,bx
	xor	ebx,ebx
	fld	[x_left]
	fstp	[x]
   row:
	finit
	fldz
	fldz
	;mov	cx,63
	mov	cl,63
    iterate:
	fld	st0
	fmul	st0,st0
	fxch	st1
	fmul	st0,st2
	fadd	st0,st0
	fxch	st2
	fmul	st0,st0
	fsubp	st1,st0
	fxch	st1
	fadd	[y]
	fxch	st1
	fadd	[x]
	fld	st1
	fmul	st0,st0
	fld	st1
	fmul	st0,st0
	faddp	st1,st0
	fsqrt
	fistp	[i]
	cmp	[i],2
	ja	over
	loop	iterate
    over:
	mov	al,cl
	stosb
	fld	[x]
	fadd	[x_step]
	fstp	[x]
	;inc	bx
	inc	ebx
	;cmp	bx,320
	cmp	ebx,320
	jb	row
	fld	[y]
	fsub	[y_step]
	fstp	[y]
	;inc	dx
	inc	edx
	;cmp	dx,200
	cmp	edx,200
	jb	screen

	; wait for a char (getchar)
	xor	ah,ah
	;int	16h
	int	32h  ; TRDOS 386 Keyboard
	; set video mode to 03h (text mode)
	mov	ax,3
	;int	10h
	int	31h

	;int	20h
exit_process:
	; sys _exit
	;mov	ebx, 0
	xor	ebx,ebx
	mov	eax, 1 ; sysexit
	int	40h

x_left dd -2.2
y_top dd 1.25

x_step dd 0.009375
y_step dd 0.0125

x dd ?
y dd ?

i dw ?
