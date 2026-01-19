; ****************************************************************************
; sinus5.s - TRDOS 386 (TRDOS v2.0.3) Test Program - 'sysvideo' pixel tests
; ----------------------------------------------------------------------------
;
; 19/02/2021
;
; ****************************************************************************
; nasm sinus5.s -l sinus5.txt -o SINUS5.PRG -Z error.txt
; (modified from 'sinus3.s', 19/02/2021)

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

	; Set Video Mode to 101h ; 640x480, 256 colors
	sys	_video, 08FFh, 101h
	or	eax, eax
	;jz	short terminate
	;mov	[LFB_ADDR], edx ; pointer to LFB info table/structure
	jnz	short set_vesa_mode_101h_ok

terminate:
	call	set_text_mode
	sys	_exit
halt:
	jmp	short halt

set_vesa_mode_101h_ok:
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
	; x = 0 to 359
	; y = +128 to -128
	; +128 --> 255 -> screen row position = (256-255)+111 = 112
	; -128 --> 0 -> screen row position = (256-0)+111 = 367

	mov	esi, sinustable
	mov	edi, _fx
	xor	eax, eax
	sub	ebx, ebx ; 0 ; x 
	xor	ebp, ebp ; pixel count
	lodsb
	jmp	short _dsw_5
_dsw_0:
	lodsb	; al = 256-y value
	;
	cmp	al, cl ; [prevy]
	je	short _dsw_5
	jb	short _dsw_3
_dsw_1:
	inc	cl ; previous 256-y
	cmp	al, cl
	jna	short _dsw_5
	; ebx = x
	; eax = 256 - y
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
	dec	cl ; previous 256-y
	cmp	al, cl
	jnb	short _dsw_5
	; ebx = x
	; eax = 256 - y
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
	mov	cl, al ; previous 256-y value
	call	_dsw_2
	inc	ebx
	cmp	ebx, 360 ; x < 360
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
	; eax = 256 - y position
	mov	edx, 256+111 ; 367
	sub	edx, eax ; convert row position from 256-y
	; row = (256-y)+111
	mov	eax, 640 ; screen width
	mul	edx
 	add	eax, ebx ; add x to y*640
	add	eax, 480-360 ; add eax, 120
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

sinustable: ; sine wave table (x=0 to 639, y= +200 to -200)
	; 19/02/2021
	; https://daycounter.com/Calculators/Sine-Generator-Calculator2.phtml
	; 360x256 (x= 0 to 359, y = 0 to 255)
	db 128,130,132,134,136,139,141,143,145,147,150,152,154,156,158,160
	db 163,165,167,169,171,173,175,177,179,181,183,185,187,189,191,193
	db 195,197,199,201,202,204,206,208,209,211,213,214,216,218,219,221
	db 222,224,225,227,228,229,231,232,233,234,236,237,238,239,240,241
	db 242,243,244,245,246,247,247,248,249,249,250,251,251,252,252,253
	db 253,253,254,254,254,255,255,255,255,255,255,255,255,255,255,255
	db 254,254,254,253,253,253,252,252,251,251,250,249,249,248,247,247
	db 246,245,244,243,242,241,240,239,238,237,236,234,233,232,231,229
	db 228,227,225,224,222,221,219,218,216,214,213,211,209,208,206,204
	db 202,201,199,197,195,193,191,189,187,185,183,181,179,177,175,173
	db 171,169,167,165,163,160,158,156,154,152,150,147,145,143,141,139
	db 136,134,132,130,128,125,123,121,119,116,114,112,110,108,105,103
	db 101,99,97,95,92,90,88,86,84,82,80,78,76,74,72,70
	db 68,66,64,62,60,58,56,54,53,51,49,47,46,44,42,41
	db 39,37,36,34,33,31,30,28,27,26,24,23,22,21,19,18
	db 17,16,15,14,13,12,11,10,9,8,8,7,6,6,5,4
	db 4,3,3,2,2,2,1,1,1,0,0,0,0,0,0,0
	db 0,0,0,0,1,1,1,2,2,2,3,3,4,4,5,6
	db 6,7,8,8,9,10,11,12,13,14,15,16,17,18,19,21
	db 22,23,24,26,27,28,30,31,33,34,36,37,39,41,42,44
	db 46,47,49,51,53,54,56,58,60,62,64,66,68,70,72,74
	db 76,78,80,82,84,86,88,90,92,95,97,99,101,103,105,108
	db 110,112,114,116,119,121,123,125,128

bss:

ABSOLUTE bss

alignb 4

color:	resd 1
prevx:	resd 1
;prevy:	resd 1

bss_start:
_fx:	resd 640 ; for every X values from 0 to 639
	resd 2048-640 ; used for repetitive x values for continuity
bss_end: