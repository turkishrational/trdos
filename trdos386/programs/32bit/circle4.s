; ****************************************************************************
; circle4.s - TRDOS 386 (TRDOS v2.0.3) Test Program - 'sysvideo' pixel tests
; ----------------------------------------------------------------------------
;
; 15/02/2021
;
; ****************************************************************************
; nasm circle4.s -l circle4.txt -o CIRCLE4.PRG -Z error.txt
; (modified from 'circle1.s', 14/02/2021)

; Draw circle by using 'sysvideo' bx=0305h

; 14/07/2020
; 31/12/2017
; TRDOS 386 (v2.0) system calls
_ver 	equ 0
_exit 	equ 1
_fork 	equ 2
_read 	equ 3
_write	equ 4
_open	equ 5
_close 	equ 6
_wait 	equ 7
_create	equ 8
_rename	equ 9
_delete	equ 10
_exec	equ 11
_chdir	equ 12
_time 	equ 13
_mkdir 	equ 14
_chmod	equ 15
_rmdir	equ 16
_break	equ 17
_drive	equ 18
_seek	equ 19
_tell 	equ 20
_memory	equ 21
_prompt	equ 22
_path	equ 23
_env	equ 24
_stime	equ 25
_quit	equ 26	
_intr	equ 27
_dir	equ 28
_emt 	equ 29
_ldrvt 	equ 30
_video 	equ 31
_audio	equ 32
_timer	equ 33
_sleep	equ 34
_msg    equ 35
_geterr	equ 36
_fpstat	equ 37
_pri	equ 38
_rele	equ 39
_fff	equ 40
_fnf	equ 41
_alloc	equ 42
_dalloc equ 43
_calbac equ 44
_dma	equ 45	

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

; Retro UNIX 386 v1 system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

[BITS 32] ; We need 32-bit intructions for protected mode

[ORG 0] 

START_CODE:
	; clear bss
	mov	edi, bss_start
	mov	ecx, (bss_end - bss_start)/4
	;xor	eax, eax
	rep	stosd

	; program message
	mov	esi, program_msg
	call	print_msg

	xor	ah, ah
	;int	16h	; KEYBOARD - READ CHAR FROM BUFFER, WAIT IF EMPTY
			; Return: AH = scan code, AL = character
	int	32h	; TRDOS 386 Keyboard interrupt 

	;; Set Video Mode to 13h
	;sys	_video, 0813h
	;cmp	eax, 14h 
	;je	short mode_13h_set_ok

	; set VGA mode by using int 31h
	mov	ax, 13h	; mode 13h ; 
	int	31h	; real mode: int 10h
	;jmp	short mode_13h_set_ok

mode_13h_set_ok:
	; Set squares of number from 0 to 255
	mov	edi, _squares
	mov	ecx, 255
	mov	ebx, 1
_ss_x:
	mov	eax, ebx
	mul	ebx
	stosd
	inc	ebx
	loop	_ss_x
reset_color:
	mov	byte [color], 142 ; initial color
reset_diameter:
	mov	eax, 100 ; initial diameter 
newdiameter:	
	; Set radius to 100
	;mov	dword [radius], 100
	;mov	dword [_r2], 10000
	mov	[radius], eax
	mov	ebx, eax
	mul	ebx
	mov	[_r2], eax ; square of circle radius
	; x2+y2 = r2	
	; Set Y values for X values from 1 to Radius - 1
	mov	edi, _fx
_yy_x:	
	dec	ebx
	jz	short center
	mov	eax, ebx
	mul	eax
	; eax = square of ebx
	mov	edx, [_r2]
	sub	edx, eax
	call	get_squareroot
	stosd
	jmp	short _yy_x

	; ***

terminate:
	call	set_text_mode
	sys	_exit
halt:
	jmp	short halt

	; ***

	; move circle to center of screen
center:
	call	movecenter
_0:
	call	drawcircle
waitforkey:
	mov	ah, 1
	int	32h
	jz	short getkey
	inc	word [counter]
	nop
	nop
	nop
	jmp	short waitforkey
getkey:
	xor	ah, ah
	int	32h

	cmp	ax, 2E03h
	je	short terminate
	cmp	al, 1Bh ; ESC key
	je	short terminate	

	cmp	al, '+'
	jne	short _1
	
	mov	eax, [radius]

	cmp	al, 99
	jnb	short _3_  ; beep
	
	; delete circle by drawing black circle
	; with same diameter and at same coordinate
	call	black_circle
	; increase radius of the circle
	inc	eax	
	;mov	[radius], eax
	jmp	newdiameter ; draw with new diameter
_1:
	cmp	al, '-'
	jne	short _2

	mov	eax, [radius]

	cmp	al, 1
	jna	short _3_ ; beep
	
	; delete circle by drawing black circle
	; with same diameter and at same coordinate
	call	black_circle
	; decrease radius of the circle
	dec	eax	
	;mov	[radius], eax
	jmp	newdiameter ; draw with new diameter
_2:
	cmp	al, 20h  ; space
	jne	short _3
	add	byte [color], 8 	
	jmp	short _0
_3:
	cmp	ah, 4Bh
	jne	short _4
	; left arrow
	mov	eax, [radius]
	cmp	eax, [_x0]
	jnb	short _3_
	call	black_circle ; clear current position 
	dec	dword [_x0]
	jmp	_0 ; draw 
_3_:
	call	beep
	jmp	waitforkey
_4:
	cmp	ah, 4Dh
	jne	short _5

	; right arrow
	mov	eax, [radius]
	add	eax, [_x0]
	cmp	eax, 319
	jnb	short _3_
	call	black_circle ; clear current position 
	inc	dword [_x0]
	jmp	_0 ; draw 
_5:
	cmp	ah, 50h
	jne	short _6
	; down arrow
	mov	eax, [radius]
	add	eax, [_y0]
	cmp	eax, 199
	jnb	short _3_
	call	black_circle ; clear current position 
	inc	dword [_y0]
	jmp	_0 ; draw 
_6:
	cmp	ah, 48h
	jne	short _7
	; up arrow
	mov	eax, [radius]
	cmp	eax, [_y0]
	jnb	short _3_
	call	black_circle ; clear current position 
	dec	dword [_y0]
	jmp	_0 ; draw 
_7:
	cmp	ah, 47h ; Home key
	jne	short _8
	call	black_circle ; clear current position 
	call	beep
	jmp	reset_diameter
		; reset diameter, move to center
_8:
	cmp	ah, 4Fh ; End key
	jne	short _9
	call	black_circle ; clear current position 
	call	beep
	jmp	reset_color 
		; reset color and diameter, move to center
_9:	
	cmp	ax, 1C0Dh
	jne	short _10
	add	byte [color], 4
	jmp	short _14
_10:	
	cmp	ah, 53h ; INSERT
	je	short _14
_11:
	cmp	ah, 52h  ; DEL
	je	short _14
_12:
	cmp	ah, 49h  ; Page UP
	jne	short _13
	dec	byte [color]
	jmp	short _14
_13:
	cmp	ah, 51h  ; Page Down
	jne	short _15
	inc	byte [color]
_14:
	call	beep
	jmp	_0
_15:
	jmp	waitforkey

print_msg:
	mov	ah, 0Eh
	mov	ebx, 7
	;mov	bl, 7 ; char attribute & color
p_next_chr:
	lodsb
	or	al, al
	jz	short p_retn ; retn	
	int	31h
	jmp	short p_next_chr
p_retn:
	retn

drawcircle:
	; INPUT:
	;	[_x0]
	;	[_y0]
	;	[radius]
	;	[color]
	;
	; Modified registers: esi, edi, eax, ecx, ebx, edx

	; set pixel pointer position to start of circle buffer
	mov	eax, circlebuffer
	mov	[pixelpos], eax	
_dc_ph0:
	; quarter 1	
	; start from y = 0, x = radius
	xor	eax, eax
	mov	[_y1], eax ; 0
	mov	[phase], al ; 0
	mov	ebp, [radius]
	mov	[_x1], ebp ; y = 0, x = r
	mov	esi, _fx
_dc_ph0_n:
	dec	dword [_x1]
	lodsd
_dc_ph0_x:
	mov	edx, [_y1]
	inc	edx
	cmp	edx, eax
	jnb	short _dc_ph0_y
	push	eax
	mov	[_y1], edx
	call	get_start_offset
	call	write_pixel
	pop	eax
	jmp	short _dc_ph0_x	
_dc_ph0_y:
	mov	[_y1], eax
	call	get_start_offset
	call	write_pixel
	dec	ebp
	jnz	short _dc_ph0_n
_dc_ph1:
	; quarter 2	
	; start from y = radius, x = 0
	inc	byte [phase]
	xor	eax, eax
	mov	[_x1], eax ; 0
	mov	ebp, [radius]
	mov	[_y1], ebp ; y = r, x = 0
	mov	esi, _fx
_dc_ph1_n:
	dec 	dword [_y1]
	lodsd
_dc_ph1_x:
	mov	edx, [_x1]
	inc	edx
	cmp	edx, eax
	jnb	short _dc_ph1_y
	push	eax
	mov	[_x1], edx
	call	get_start_offset
	call	write_pixel
	pop	eax
	jmp	short _dc_ph1_x	
_dc_ph1_y:
	mov	[_x1], eax
	call	get_start_offset
	call	write_pixel
	dec	ebp
	jnz	short _dc_ph1_n
_dc_ph2:
	; quarter 3	
	; start from y = 0, x = radius
	inc	byte [phase]
	xor	eax, eax
	mov	[_y1], eax ; 0
	mov	ebp, [radius]
	mov	[_x1], ebp ; y = 0, x = r
	mov	esi, _fx
_dc_ph2_n:
	dec	dword [_x1]
	lodsd
_dc_ph2_x:
	mov	edx, [_y1]
	inc	edx
	cmp	edx, eax
	jnb	short _dc_ph2_y
	push	eax
	mov	[_y1], edx
	call	get_start_offset
	call	write_pixel
	pop	eax
	jmp	short _dc_ph2_x	
_dc_ph2_y:
	mov	[_y1], eax
	call	get_start_offset
	call	write_pixel
	dec	ebp
	jnz	short _dc_ph2_n
_dc_ph3:
	; quarter 4	
	; start from y = radius, x = 0
	inc	byte [phase]
	xor	eax, eax
	mov	[_x1], eax ; 0
	mov	ebp, [radius]
	mov	[_y1], ebp ; y = r, x = 0
	mov	esi, _fx
_dc_ph3_n:
	dec	dword [_y1]
	lodsd
_dc_ph3_x:
	mov	edx, [_x1]
	inc	edx
	cmp	edx, eax
	jnb	short _dc_ph3_y
	push	eax
	mov	[_x1], edx
	call	get_start_offset
	call	write_pixel
	pop	eax
	jmp	short _dc_ph3_x	
_dc_ph3_y:
	mov	[_x1], eax
	call	get_start_offset
	call	write_pixel
	dec	ebp
	jnz	short _dc_ph3_n
_dc_ph4:
write_circle:
	mov	esi, circlebuffer
	mov	edx, [pixelpos]
	sub	edx, esi
	shr	edx, 2 ; / 4
	; edx = pixel count
	; esi = user's single color pixel buffer address
	sys	_video, 0305h, [color]

	retn	

write_pixel:
	; eax = (screen) pixel position
	mov	edi, [pixelpos] ; pointer
	stosd
	mov	[pixelpos], edi ; pointer
	retn

movecenter:
	mov	dword [_x0], 320/2
	mov	dword [_y0], 200/2
	retn

get_start_offset:
	mov	eax, 320
	mov	edx, [_y0]
	cmp	byte [phase], 0
	ja	short gso_1
gso_0:
	; quarter 1
	sub	edx, [_y1] ; y = 0 -> r
	mul	edx
	add	eax, [_x0]
	add	eax, [_x1] ; x = r -> 0
	retn
gso_1:
	cmp	byte [phase], 1
	ja	short gso_2
	; quarter 2
	sub	edx, [_y1] ; y = r -> 0
	mul	edx
	add	eax, [_x0]
	sub	eax, [_x1] ; x = 0 -> -r
	retn
gso_2:
	cmp	byte [phase], 2
	ja	short gso_3
	; quarter 3
	add	edx, [_y1] ; y = 0 -> -r 
	mul	edx
	add	eax, [_x0]
	sub	eax, [_x1] ; x = -r -> 0 
	retn
gso_3:
	; quarter 4
	add	edx, [_y1] ; y = -r -> 0
	mul	edx
	add	eax, [_x0]
	add	eax, [_x1] ; x = 0 -> r 
	retn

black_circle:
	xor	ah, ah
	xchg	[color], ah ; color = 0 
	push	eax
	call	drawcircle
	pop	eax
	xchg	[color], ah ; restore color
	retn

beep:
	; call beep function (16/64 second, 886Hz)
	sys	_audio, 16, 1331
	retn

get_squareroot:
	; input: edx = square of the number (y)
	; output: eax = approx. square root of ebx 
	mov	esi, _squares
	push	ebx
	xor	ebx, ebx
	;mov	ecx, 256
	mov	ecx, [radius] ; max. value of radius is 256
q_sr_x:	
	lodsd
	cmp	eax, edx
	jnb	short q_sr_ok
	inc	ebx
	loop	q_sr_x
q_sr_ok:
	mov	eax, ebx
	pop	ebx
	retn

set_text_mode:
	xor    ah, ah
	mov    al, 3                        
 	;int   10h ; al = 03h text mode, int 10 video
	int    31h ; TRDOS 386 - Video interrupt
	retn
		
program_msg:
	db "TRDOS 386 v2.0.3 - ('sysvideo') Test Program - Draw Circle"
	db 0Dh, 0Ah
	db "by Erdogan Tan - 15/02/2021"
	;db 0Dh, 0Ah, 0
	db 0Dh, 0Ah, 0Dh, 0Ah

	db "Use Arrow Keys, Home, End to move the CIRCLE .."
	db 0Dh, 0Ah
	db "Use +,- keys to increase and decrease DIAMETER .."		
	db 0Dh, 0Ah
	db "Use ENTER key to draw CIRCLE .."
	db 0Dh, 0Ah
	db "Use SPACE, Pg Up, Pg Down keys to change COLOR .."
	db 0Dh, 0Ah	
	db "Press ESC to exit .."
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db "Press any key to continue .."
nextline:
	db 0Dh, 0Ah, 0	

bss:

ABSOLUTE bss

alignb 4

bss_start:
_squares:
	resd 256 ; squares of numbers from 0 t0 255	
counter:
	resw 1
phase:	resw 1
radius:	resd 1 ; Current Radius value
_r2:	resd 1 ; Square of R
color:	resd 1
_x0:	resd 1
_y0:	resd 1
_x1:	resd 1
_y1:	resd 1
_fx:	resd 256 ; For every X values from 0 to 255
pixelpos:
	resd 1
circlebuffer:
	resd 10000 ; 100*100*4 bytes

bss_end: