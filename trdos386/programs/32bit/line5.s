; ****************************************************************************
; line5.s - TRDOS 386 (TRDOS v2.0.3) Test Program - 'sysvideo' pixel w/r tests
; ----------------------------------------------------------------------------
;
; 12/02/2021 (11/02/2021)
;
; ****************************************************************************
; nasm line5.s -l line5.txt -o LINE5.PRG -Z error.txt
; (modified from 'line4.s', 11/02/2021)

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
	mov	bl, 162 ; color
	mov	[color], bl

	; initializing position difference parameters
	;mov	al, [maxdx]
	;shr	al, 1
	;mov	[posdx], al
	;mov	al, [maxdy]
	;shr	al, 1
	;mov	[posdy], al

next_phase:
	call	drawrect
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
	jne	short _0

	inc	byte [color]
	cmp	byte [color], 255
	jna	short next_phase
	dec	byte [color]
	jmp	short waitforkey
_0:
	cmp	al, '-'
	jne	short _1

	dec	byte [color]
	jns	short next_phase
	inc	byte [color]
	jmp	short waitforkey
_1:
	cmp	al, 20h  ; space
	jne	short _3
	add	byte [color], 8 	
	call	drawrect
_2:
	jmp	short waitforkey
_3:
	cmp	ah, 4Bh
	jne	short _4
	; left arrow
	mov	byte [direction], 1
	jmp	next_phase
_4:
	cmp	ah, 4Dh
	jne	short _5
	; right arrow
	mov	byte [direction], 2
	jmp	next_phase
_5:
	cmp	ah, 50h
	jne	short _6
	; down arrow
	mov	byte [direction], 3
	jmp	next_phase
_6:
	cmp	ah, 48h
	jne	short _7
	; up arrow
	mov	byte [direction], 4
	jmp	next_phase
_7:
	cmp	ah, 47h ; Home key
	jne	short _9
_8:
	mov	byte [phase], 0
	jmp	next_phase
_9:
	cmp	ah, 4Fh ; End key
	jne	short _10
	mov	byte [color], 162
	jmp	short _8
_10:	
	cmp	ax, 1C0Dh
	jne	short _12
	
	inc	byte [phase]
	cmp	byte [phase], 5
	ja	short _11
	jmp	next_phase
_11:
	mov	byte [phase], 0
	jmp	next_phase
_12:	
	cmp	ah, 49h  ; Page UP
	jne	short _14
	dec	byte [phase]
	jns	short _13
	call	beep
	inc	byte [phase]
	jmp	short _15
_13:
	jmp	next_phase
_14:
	cmp	ah, 51h  ; Page Down
	jne	short _15
	inc	byte [phase]
	cmp	byte [phase], 5
	jna	short _13
	call	beep
	dec	byte [phase]
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

drawrect:
	; draw rectangles
	cmp	byte [phase], 3
	jnb	short dr_x
	xor	al, al
	cmp	byte [direction], al ; 0
	jna	short dr_x
	xchg	al, [color]
	push	eax
	call	dr_x
	pop	eax
	mov	[color], al
	call	change_positions
dr_x:
	cmp	byte [phase], 0
	ja	short dr_2
	; position 1 rectangles
	mov	esi, positions1
	mov	edi, rectbuffer1
dr_0:
	mov	ecx, 20 ; 20 lines, 5 rectangles
	push	edi 
dr_1:
	lodsd
	stosd	; position
	lodsd	; 
	stosd	; length (and type, h/v)
	loop	dr_1
	pop	esi

	; BL bit 5 = 1 -> write lines via user buffer
	sys	_video, 022Eh, [color], 20
	retn
dr_2:
	cmp	byte [phase], 1
	ja	short dr_3
	; position 2 rectangles
	mov	esi, positions2
	mov	edi, rectbuffer2
	jmp	short dr_0
dr_3:
	cmp	byte [phase], 2
	ja	short dr_4
	; position 3 rectangles
	mov	esi, positions3
	mov	edi, rectbuffer3
	jmp	short dr_0
dr_4:
	cmp	byte [phase], 3
	ja	short dr_6
	; delete position 1 rectangles
	mov	esi, rectbuffer1
dr_5:
	; clear previous positions (by using black color)
	; BL bit 5 = 1 -> write lines via user buffer
	sys	_video, 022Eh, 0, 20
	retn			
dr_6:	  
	cmp	byte [phase], 4
	ja	short dr_7
	; delete position 2 rectangles
	mov	esi, rectbuffer2
	jmp	short dr_5
dr_7:
	cmp	byte [phase], 5
	ja	short dr_8
	; delete position 2 rectangles
	mov	esi, rectbuffer3
	jmp	short dr_5
dr_8:
	mov	byte [phase], 0
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

change_positions:
	mov	ecx, 60 ; 60 lines, 15 rectangles
	cmp	byte [direction], 2
	ja	short chgpos3 ; scroll down/up
	mov	ah, [maxdx]
	mov	al, [posdx]
	mov	esi, positions1 ; start address 
	je	short chgpos1 ; slide to right
	; slide to left
	and	al, al
	jna	short chgpos_beep
	dec	al
	mov	[posdx], al
	; (horizontal positions) -L-
chgpos0:
	dec	word [esi]
	add	esi, 8
	loop	chgpos0
	jmp	short chgpos_retn
chgpos_beep:
	call	beep
chgpos_retn:
	mov	byte [direction], 0
	retn
chgpos1:
	cmp	al, ah ; right (dx) limit ?
	jnb	short chgpos_beep
	inc	al
	mov	[posdx], al
	; (horizontal positions) -R-
chgpos2:
	inc	word [esi]
	add	esi, 8
	loop	chgpos2
	jmp	short chgpos_retn
chgpos3:
	mov	ah, [maxdy]
	mov	al, [posdy]
	mov	esi, positions1+2 ; start address 
	cmp	byte [direction], 3
	ja	short chgpos4 ; scroll up
	; scroll down
	cmp	al, ah ; down (dy) limit ?
	jnb	short chgpos_beep
	inc	al
	mov	[posdy], al
	; (vertical positions) -D-
	jmp	short chgpos2
chgpos4:
	; scroll up
	or	al, al
	jna	short chgpos_beep
	dec	al
	mov	[posdy], al
	; (vertical positions) -U-
	jmp	short chgpos0
		
program_msg:
	db "TRDOS 386 v2.0.3 - ('sysvideo') Test Program - Draw Lines"
	db 0Dh, 0Ah
	db "by Erdogan Tan - 12/02/2021"
	;db 0Dh, 0Ah, 0
	db 0Dh, 0Ah, 0Dh, 0Ah
	db "Press ENTER to draw SQUAREs .."
	db 0Dh, 0Ah
	db "Press SPACE to change COLOR .."
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db "Press any key to continue .."
nextline:
	db 0Dh, 0Ah, 0	

positions1:
	; rectangle 1
	dw 48,60,36,0
	dw 83,60,4096+36,0
	dw 48,95,36,0
	dw 48,60,4096+36,0 
	; rectangle 2 
	dw 296,120,156,0
	dw 451,120,4096+120,0
	dw 296,239,156,0
	dw 296,120,4096+120,0
	; rectangle 3 
	dw 488,96,24,0
	dw 511,96,4096+24,0
	dw 488,119,24,0
	dw 488,96,4096+24,0
	; rectangle 4 
	dw 216,336,108,0
	dw 323,336,4096+12,0
	dw 216,347,108,0
	dw 216,336,4096+12,0
	; rectangle 5 	
	dw 536,326,12,0
	dw 547,326,4096+60,0
	dw 536,385,12,0
	dw 536,326,4096+60,0
positions2:
	; rectangle 6
	dw 236,204,24,0	     ; line 1, x1, y1, h
	dw 259,204,4096+84,0 ; line 2, x2, y1, v 
	dw 236,287,24,0	     ; line 3, x1, y2, h 	
	dw 236,204,4096+84,0 ; line 4, x1, y1, v 
	; rectangle 7 
	dw 84,300,96,0
	dw 179,300,4096+84,0
	dw 84,383,96,0
	dw 84,300,4096+84,0
	; rectangle 8 
	dw 504,290,24,0
	dw 527,290,4096+24,0
	dw 504,313,24,0
	dw 504,290,4096+24,0
	; rectangle 9 
	dw 296,72,72,0
	dw 367,72,4096+24,0
	dw 296,95,72,0
	dw 296,72,4096+24,0
	; rectangle 10 	
	dw 348,276,48,0
	dw 395,276,4096+36,0
	dw 348,311,48,0
	dw 348,276,4096+36,0
positions3:
	; rectangle 11 
	dw 464,72,24,0
	dw 487,72,4096+24,0
	dw 464,95,24,0
	dw 464,72,4096+24,0 
	; rectangle 12 
	dw 132,108,72,0
	dw 203,108,4096+72,0
	dw 132,179,72,0
	dw 132,108,4096+72,0
	; rectangle 13 
	dw 512,120,48,0
	dw 559,120,4096+36,0
	dw 512,155,48,0
	dw 512,120,4096+36,0
	; rectangle 14 
	dw 408,264,12,0
	dw 419,264,4096+24,0
	dw 408,287,12,0
	dw 408,264,4096+24,0
	; rectangle 15 	
	dw 420,326,72,0
	dw 491,326,4096+60,0
	dw 420,385,72,0
	dw 420,326,4096+60,0

maxdx:	dw 96 	
maxdy:	dw 96	
posdx:	dw 48
posdy:	dw 48

bss:

ABSOLUTE bss

alignb 4

bss_start:
	color:	resd 1
	phase:  resb 1
	direction: resb 1
	counter: resw 1
	;posdx:	resw 1
	;posdy:	resw 1

rectbuffer1:
	resd	40
rectbuffer2:
	resd	40
rectbuffer3:
	resd	40

bss_end: