; ****************************************************************************
; line6.s - TRDOS 386 (TRDOS v2.0.3) Test Program - 'sysvideo' pixel w/r tests
; ----------------------------------------------------------------------------
;
; 12/02/2021
;
; ****************************************************************************
; nasm line6.s -l line6.txt -o LINE6.PRG -Z error.txt
; (modified from 'line5.s', 12/02/2021)

; Draw lines by using 'sysvideo' bh=010Eh (Std VGA mode version)

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

	; set VGA mode by using int 31h
	mov	ax, 13h	; mode 13h ; 
	int	31h	; real mode: int 10h
	jmp	short set_mode_13h_ok

terminate:
	call	set_text_mode
	sys	_exit
halt:
	jmp	short halt

set_mode_13h_ok:
	mov	bl, 162 ; color
	mov	[color], bl

	; Save default values of rectangles
	mov	esi, rectangles
	mov	edi, restorebuff
	mov	ecx, (20*8)/4
	rep	movsd

next_pos:
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
_0:
	cmp	al, '+'
	jne	short _1

	inc	byte [color]
	cmp	byte [color], 255
	jna	short next_pos
	dec	byte [color]
	jmp	short waitforkey
_1:
	cmp	al, '-'
	jne	short _2

	dec	byte [color]
	jns	short next_pos
	inc	byte [color]
	jmp	short waitforkey
_2:
	cmp	al, 20h  ; space
	jne	short _3
	add	byte [color], 8 	
	jmp	short next_pos
_3:
	cmp	ah, 4Bh
	jne	short _4
	; left arrow
	mov	byte [direction], 1
	jmp	short next_pos
_4:
	cmp	ah, 4Dh
	jne	short _5
	; right arrow
	mov	byte [direction], 2
	jmp	short next_pos
_5:
	cmp	ah, 50h
	jne	short _6
	; down arrow
	mov	byte [direction], 3
	jmp	next_pos
_6:
	cmp	ah, 48h
	jne	short _7
	; up arrow
	mov	byte [direction], 4
	jmp	next_pos
_7:
	cmp	ah, 47h ; Home key
	jne	short _9
_8:
	call	beep
	call	clear_vpage
	mov	byte [posdx], 24
	mov	byte [posdy], 24
	mov	ecx, (20*8)/4
	mov	esi, restorebuff
	mov	edi, rectangles
	rep	movsd
	jmp	next_pos
_9:
	cmp	ah, 4Fh ; End key
	jne	short _10
	mov	byte [color], 162
	jmp	short _8
_10:	
	cmp	ax, 1C0Dh
	jne	short _11
	call	beep
	add	byte [color], 4
	jmp	next_pos
_11:	
	cmp	ah, 49h  ; Page UP
	jne	short _12
	dec	byte [color]
	jmp	next_pos
_12:
	cmp	ah, 51h  ; Page Down
	jne	short _13
	inc	byte [color]
	jmp	next_pos
_13:
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
	cmp	byte [direction], 0
	jna	short dr_x
	call	clear_vpage
	call	change_positions
dr_x:
	mov	esi, rectangles ; predefined dimensions
	mov	edi, rectbuffer ; final rectangle buffer
	mov	ecx, 20 ; 20 lines, 5 rectangles
dr_y:
	lodsd
	stosd	; position
	lodsd	; 
	stosd	; length (and type, h/v)
	loop	dr_y
dr_z:
	mov	esi, rectbuffer
	; BL bit 5 = 1 -> write lines via user buffer
	sys	_video, 012Eh, [color], 20
	retn

clear_vpage:
	xor	al, al
	xchg	al, [color]
	push	eax
	call	dr_z
	pop	eax
	mov	[color], al
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
	mov	ecx, 20 ; 20 lines, 5 rectangles
	cmp	byte [direction], 2
	ja	short chgpos3 ; scroll down/up
	mov	ah, [maxdx]
	mov	al, [posdx]
	mov	esi, rectangles ; start address 
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
	mov	esi, rectangles+2 ; start address 
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
	db "Press SPACE or ENTER to change COLOR .."
	db 0Dh, 0Ah
	db "Press ESC to exit .."
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db "Press any key to continue .."
nextline:
	db 0Dh, 0Ah, 0	

rectangles:
	; rectangle 1
	dw 24,30,18,0
	dw 41,30,4096+18,0
	dw 24,47,18,0
	dw 24,30,4096+18,0 
	; rectangle 2 
	dw 148,60,78,0
	dw 225,60,4096+60,0
	dw 148,119,78,0
	dw 148,60,4096+60,0
	; rectangle 3 
	dw 244,48,12,0
	dw 255,48,4096+12,0
	dw 244,59,12,0
	dw 244,48,4096+12,0
	; rectangle 4 
	dw 108,168,54,0
	dw 161,168,4096+6,0
	dw 108,173,54,0
	dw 108,168,4096+6,0
	; rectangle 5 	
	dw 268,163,6,0
	dw 273,163,4096+30,0
	dw 268,192,6,0
	dw 268,163,4096+30,0

maxdx:	dw 48	
maxdy:	dw 48
posdx:	dw 24
posdy:	dw 24

bss:

ABSOLUTE bss

alignb 4

bss_start:
	color:	resd 1
	direction: resw 1
	counter: resw 1
	;posdx:	resw 1
	;posdy:	resw 1

restorebuff:
	resd	40

rectbuffer:
	resd	40

bss_end: