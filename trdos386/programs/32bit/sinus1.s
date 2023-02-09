; ****************************************************************************
; sinus1.s - TRDOS 386 (TRDOS v2.0.3) Test Program - 'sysvideo' pixel tests
; ----------------------------------------------------------------------------
;
; 19/02/2021
;
; ****************************************************************************
; nasm sinus1.s -l sinus1.txt -o SINUS1.PRG -Z error.txt
; (modified from 'circle5.s', 15/02/2021)

; Draw sinus wave/curve by using 'sysvideo' bx=0305h

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
	jmp	short mode_13h_set_ok

terminate:
	call	set_text_mode
	sys	_exit
halt:
	jmp	short halt

mode_13h_set_ok:
	mov	byte [color], 28 ; initial pixel color
_0:
	call	drawsinewave
waitforkey:
	;mov	ah, 1
	;int	32h
	;jz	short getkey
	;inc	word [counter]
	;nop
	;nop
	;nop
	;jmp	short waitforkey
getkey:
	xor	ah, ah
	int	32h

	cmp	ax, 2E03h
	je	short terminate
	cmp	al, 1Bh ; ESC key
	je	short terminate	

	cmp	al, '+'
	jne	short _1
	
	inc	byte [color]
	jmp	short _0
_1:
	cmp	al, '-'
	jne	short _2

	dec	byte [color]
	jmp	short _0
_2:
	cmp	al, 20h  ; space
	jne	short _3
	add	byte [color], 8 	
	jmp	short _0
_3:
	cmp	ah, 4Bh
	jne	short _5
	; left arrow
_4:
	call	beep
	jmp	waitforkey
_5:
	cmp	ah, 4Dh
	jne	short _6

	; right arrow
	jmp	short _4
_6:
	cmp	ah, 50h
	jne	short _7
	; down arrow
	jmp	short _4
_7:
	cmp	ah, 48h
	jne	short _8
	; up arrow
	jmp	short _4
_8:	
	cmp	ax, 1C0Dh
	jne	short _9
	call	beep
	add	byte [color], 4
	jmp	short _0
_9:	
	call	beep
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

drawsinewave:
	; INPUT:
	;	sinustable
	;
	; Modified registers: esi, edi, eax, ecx, ebx, edx

	; fill _fx table by using sine wave table
	; x = 0 to 319
	; y = +99 to -99
	; +99 --> 197 -> screen row position = 198-197 = 1
	; -99 --> 0 -> screen row position = 198-0 = 198

	mov	esi, sinustable
	mov	edi, _fx
	xor	eax, eax
	sub	ebx, ebx ; 0 ; x 
	xor	ebp, ebp ; pixel count
	lodsb 
	jmp	short _dsw_5
_dsw_0:
	lodsb	; al = 198-y value
	;
	cmp	al, cl ; [prevy]
	je	short _dsw_5
	jb	short _dsw_3
_dsw_1:
	inc	cl ; previous 198-y
	cmp	al, cl
	jna	short _dsw_5
	; ebx = x
	; eax = 198 - y
	call	_dsw_4
	jmp	short _dsw_1
_dsw_2:
	push	eax
	call	getpixeloffset
	stosd
	pop	eax
	inc	ebp ; increase pixel count
	retn
_dsw_3:
	dec	cl ; previous 198-y
	cmp	al, cl
	jnb	short _dsw_5
	; ebx = x
	; eax = 198 - y
	call	_dsw_4
	jmp	short _dsw_3
_dsw_4:
	push	ebx
	mov	ebx, [prevx]
	xchg	al, cl ; [prevy]
	call	_dsw_2
	xchg	cl, al
	pop	ebx
	inc	dword [prevx]
	retn
_dsw_5:
	mov	[prevx], ebx ; previous x value 
	mov	cl, al  ; 0 ; previous 198-y value
	call	_dsw_2
	inc	ebx
	cmp	ebx, 320
	jb	short _dsw_0

	mov	esi, _fx
	;mov	edx, ebp
	;
	; edx = pixel count
	; esi = user's single color pixel buffer address
	sys	_video, 0305h, [color], ebp

	retn
	
getpixeloffset:
	; ebx = x position
	; eax = 198 - y position
	mov	edx, 198
	sub	edx, eax
	mov	eax, 320 ; screen width
	mul	edx
 	add	eax, ebx ; add x to y*320
	; eax = pixel offset on display page
	retn
beep:
	; call beep function (16/64 second, 886Hz)
	sys	_audio, 16, 1331
	retn

set_text_mode:
	xor    ah, ah
	mov    al, 3                        
 	;int   10h ; al = 03h text mode, int 10 video
	int    31h ; TRDOS 386 - Video interrupt
	retn
		
program_msg:
	db "TRDOS 386 v2.0.3 - ('sysvideo') Test Program - Draw Sine Wave"
	db 0Dh, 0Ah
	db "by Erdogan Tan - 19/02/2021"
	;db 0Dh, 0Ah, 0
	db 0Dh, 0Ah, 0Dh, 0Ah

	db "Use SPACE,ENTER,'+','-' keys to change COLOR .."		
	db 0Dh, 0Ah
	db "Press ESC to exit .."
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db "Press any key to continue .."
nextline:
	db 0Dh, 0Ah, 0

sinustable: ; sine wave table (x=0 to 319, y= +99 to -99)
	; 19/02/2021
	; https://daycounter.com/Calculators/Sine-Generator-Calculator2.phtml
	; 320x198 (x= 0 to 319, y = 0 to 197)
	db 99,100,102,104,106,108,110,112,114,116,118,120,121,123,125,127
	db 129,131,133,134,136,138,140,141,143,145,147,148,150,152,153,155
	db 156,158,159,161,162,164,165,167,168,170,171,172,173,175,176,177
	db 178,179,180,181,182,183,184,185,186,187,188,189,190,190,191,192
	db 192,193,193,194,194,195,195,195,196,196,196,197,197,197,197,197
	db 197,197,197,197,197,197,196,196,196,195,195,195,194,194,193,193
	db 192,192,191,190,190,189,188,187,186,185,184,183,182,181,180,179
	db 178,177,176,175,173,172,171,170,168,167,165,164,162,161,159,158
	db 156,155,153,152,150,148,147,145,143,141,140,138,136,134,133,131
	db 129,127,125,123,121,120,118,116,114,112,110,108,106,104,102,100
	db 99,97,95,93,91,89,87,85,83,81,79,77,76,74,72,70
	db 68,66,64,63,61,59,57,56,54,52,50,49,47,45,44,42
	db 41,39,38,36,35,33,32,30,29,27,26,25,24,22,21,20
	db 19,18,17,16,15,14,13,12,11,10,9,8,7,7,6,5
	db 5,4,4,3,3,2,2,2,1,1,1,0,0,0,0,0
	db 0,0,0,0,0,0,1,1,1,2,2,2,3,3,4,4
	db 5,5,6,7,7,8,9,10,11,12,13,14,15,16,17,18
	db 19,20,21,22,24,25,26,27,29,30,32,33,35,36,38,39
	db 41,42,44,45,47,49,50,52,54,56,57,59,61,63,64,66
	db 68,70,72,74,76,77,79,81,83,85,87,89,91,93,95,97
bss:

ABSOLUTE bss

alignb 4

color:	resd 1
prevx:	resd 1
;prevy:	resd 1

bss_start:
_fx:	resd 320 ; for every X values from 0 to 319
	resd 1024-320 ; used for repetitive x values for continuity
bss_end: