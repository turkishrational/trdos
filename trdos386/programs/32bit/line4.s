; ****************************************************************************
; line4.s - TRDOS 386 (TRDOS v2.0.3) Test Program - 'sysvideo' pixel w/r tests
; ----------------------------------------------------------------------------
;
; 10/02/2021
;
; ****************************************************************************
; nasm line4.s -l line4.txt -o LINE4.PRG -Z error.txt
; (modified from 'line2.s', 10/02/2021)

; Draw lines by using 'sysvideo' bh=020Eh (VESA VBE mode version)

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

	; Set Video Mode to 101h ; 640x480, 256 colors
	sys	_video, 08FFh, 101h
	or	eax, eax
	;jz	short terminate
	;mov	[LFB_ADDR], edx
	jnz	short set_vesa_mode_101h_ok

terminate:
	call	set_text_mode
	sys	_exit
halt:
	jmp	short halt

set_vesa_mode_101h_ok:
	; create a box and the center of 640x480 screen 
reset:	
	mov	bl, 148 ; color
	mov	[color], bl

	mov	cl, 64
	mov	[length], cl

	; BL = Line Color
	; CL = Line Size

	; move square to center of screen
center:
	call	movecenter
_0:
	call	drawline
	jc	short waitforkey
	inc	byte [lphase] 
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

	cmp	byte [lphase], 4
	jb	_14

	call	clear_lines
	; increase length
	inc	dword [length]
	jnz	short _incl
	dec	dword [length]
_incl:
_decl:
	call	get_start_offset
	add	eax, [length]
	cmp	eax, 640*480
	jna	short _0
	dec	dword [length]
	jnz	short _incl
	call	beep
	jmp	short waitforkey
_1:
	cmp	al, '-'
	jne	short _2

	cmp	byte [lphase], 4
	jb	_14

	call	clear_lines
	; decrease length
	dec	dword [length]
	jnz	short _decl
	inc	dword [length]
	jmp	short _decl
_2:
	cmp	al, 20h  ; space
	jne	short _3
	add	byte [color], 8 	
	call	drawline
	jmp	waitforkey
_3:
	cmp	ah, 4Bh
	jne	short _4

	; left arrow
	cmp	byte [lphase], 4
	jb	_14
	cmp	word [x_pos], 0
	ja	short _3_
	call	beep
	jmp	waitforkey
_3_:
	call	clear_lines
	dec	word [x_pos]
	jmp	_0
_4:
	cmp	ah, 4Dh
	jne	short _5

	; right arrow
	cmp	byte [lphase], 4
	jb	_14
	call	clear_lines
	mov	ecx, [x_pos]
	add	ecx, [length]
	cmp	ecx, 640
	jb	short _4_
	dec	dword [length]
	jnz	short _4_
	inc	dword [length]
	call	beep
	jmp	waitforkey
_4_:
	inc	word [x_pos]
	jmp	_0
_5:
	cmp	ah, 50h
	jne	short _6
	; down arrow
	cmp	byte [lphase], 4
	jb	_14
	call	clear_lines
	mov	eax, [y_pos]
	add	eax, [length]
	cmp	eax, 480
	jb	short _5_
	dec	dword [length]
	jnz	short _5_
	inc	dword [length]
	call	beep
	jmp	waitforkey
_5_:
	inc	word [y_pos]
	jmp	_0
_6:
	cmp	ah, 48h
	jne	short _7
	; up arrow
	cmp	byte [lphase], 4
	jb	_14
	cmp	word [y_pos], 0
	ja	short _6_
	call	beep
	jmp	waitforkey
_6_:
	call	clear_lines
	dec	word [y_pos]
	jmp	_0
_7:
	cmp	ah, 47h ; Home key
	jne	short _8
	call	clear_lines
	call	movecenter
	call	drawline_x
	call	drawline_x
	call	drawline_x
	jmp	short _14
_8:
	cmp	ah, 4Fh ; End key
	jne	short _9
	call	clear_lines
	mov	bl, 148  ; color
	mov	[color], bl
	mov	cl, 64
	mov	[length], cl
	call	movecenter
	call	drawline_x
	call	drawline_x
	call	drawline_x
	jmp	short _14
_9:	
	cmp	ax, 1C0Dh
	je	short _14
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
	jne	waitforkey
	inc	byte [color]
_14:
	call	drawline_x
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

drawline_x:
	cmp	byte [lphase], 4
	jnb	short clear_lines
	call	drawline
	jnc	short drawline_x_ok
	mov	byte [lphase], 4
	retn
drawline_x_ok:
	inc	byte [lphase]
drawline_r:
	retn
clear_lines:
	; clear previous square by drawing black square
	xor	ah, ah
	mov	al, [lphase]
	mov	[lphase], ah ; 0
	xchg	[color], ah ; 0
csq_1:
	push	eax
	call	drawline
	pop	eax
	and	al, al
	jz	short csq_2
	inc	byte [lphase]
	dec	al
	jmp	short csq_1
csq_2:
	mov	[lphase], al ; 0
	mov	[color], ah ; restore color
	retn
drawline:
	; INPUT:
	;	[x_pos]
	;	[y_pos]
	;	[length]
	;	[color]
	;
	; Modified registers: edi, eax, ecx, ebx, edx	

	; draw one segment of a square
_dline0:
	cmp	byte [lphase], 0
	ja	short _dvline1
	; draw (x1,y1) to (x2,y1) horizontal line at first
_dline0_x:
	mov	ebp, [length]
	call	get_start_offset
	; eax = start pos
	; ebp = length
	mov	edi, eax
	add	eax, ebp
	cmp	eax, 640*480
	jna	short _dline0_y
	dec	dword [length]
	jnz	short _dline0_x
_dvline_err:
	call	beep
	stc
	retn
_dline0_y:
	mov	[p1pos], edi
	dec	eax
	mov	[p2pos], eax
	; draw horizontal line
	mov	si, [y_pos]
	shl	esi, 16
	mov	si, [x_pos]
	jmp	short _dvline_h 
_dvline1:
	cmp	byte [lphase], 1
	ja	short _dvline2
	; draw (x2,y1) to (x2,y2) vertical line
_dline1_x:
	mov	ebp, [length]

	mov	si, [y_pos]
	shl	esi, 16
	mov	si, [x_pos]
	add	si, bp
	dec	si
 
	mov	eax, 640
	mul	ebp
	mov	edi, [p2pos]
	add	eax, edi
	cmp	eax, 640*480
	jna	short _dline1_y

	cmp	byte [length], 1 
	jna	short _dvline_err
	
	sub	al, al
	xchg	[color], al ; 0
	push	eax
	call	_dline0_x
	pop	eax
	mov	[color], al
	jc	short _dvline_ok
	dec	byte [length]
	call	_dline0_x
	jc	short _dvline_ok
	jmp	short _dline1_x
_dline1_y:
	mov	[p3pos], eax
_dvline_v:
	; draw vertical line
	or	bp, 1000h ; vertical line
_dvline_h:
	; esi = line start position (row, column)
	sys	_video, 020Eh, [color], ebp
_dvline_ok:
	retn	
_dvline2:
	cmp	byte [lphase], 2
	ja	short _dvline3
	; draw (x2,y2) to (x3,y2) horizontal line
	mov	ebp, [length]
	mov	eax, [p3pos]
	inc	eax
	mov	edi, eax
	sub	eax, ebp	 
	;jb	_dvline_err
	mov	[p4pos], eax
	; draw horizontal line
	mov	si, [y_pos]
	add	si, bp
	dec	si
	shl	esi, 16
	mov	si, [x_pos]
	jmp	short _dvline_h
_dvline3:
	; draw (x3,y2) to (x1,y1) vertical line
	mov	ebp, [length] 
	mov	edi, [p4pos]
	; draw vertical line
	mov	si, [y_pos]
	shl	esi, 16
	mov	si, [x_pos]
	jmp	short _dvline_v

movecenter:
	; Modified registers: ecx	

	mov	ecx, 640
	sub	ecx, [length]
	shr	ecx, 1
	mov	[x_pos], ecx
	mov	ecx, 480
	sub	ecx, [length]
	shr	ecx, 1
	mov	[y_pos], ecx
	retn

get_start_offset:
	mov	eax, 640
	mul	dword [y_pos]
	add	eax, [x_pos]
	retn
beep:
	; call beep function (16/64 second, 886Hz)
	sys	_audio, 16, 1331
	retn

set_text_mode:
	xor    ah, ah
	mov    al, 3                        
 	;int   10h	; al = 03h text mode, int 10 video
	int    31h ; TRDOS 386 - Video interrupt
	retn
		
program_msg:
	db "TRDOS 386 v2.0.3 - ('sysvideo') Test Program - Draw Lines"
	db 0Dh, 0Ah
	db "by Erdogan Tan - 10/02/2021"
	;db 0Dh, 0Ah, 0
	db 0Dh, 0Ah, 0Dh, 0Ah

	db "Use Arrow Keys, Home, End to move the LINE .."
	db 0Dh, 0Ah
	db "Use +,- keys to increase and decrease LENGTH .."		
	db 0Dh, 0Ah
	db "Use ENTER key to draw SQUARE .."
	db 0Dh, 0Ah
	db "SPACE, Pg Up, Pg Down keys to change LINE COLOR .."
	db 0Dh, 0Ah	
	db "Use Insert, Delete keys to delete LINE or SQUARE .."
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db "Press any key to continue .."
nextline:
	db 0Dh, 0Ah, 0	

bss:

ABSOLUTE bss

alignb 4

bss_start:


lphase:	resb 1
color:	resb 1

counter: resw 1

y_pos:	resd 1
x_pos:	resd 1
length: resd 1

p1pos: resd 1
p2pos: resd 1
p3pos: resd 1
p4pos: resd 1

bss_end: