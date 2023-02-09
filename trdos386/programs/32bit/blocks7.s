; ****************************************************************************
; blocks7.s - TRDOS 386 (TRDOS v2.0.3) Test Program - 'sysvideo' pixel tests
; ----------------------------------------------------------------------------
;
; 27/02/2021
;
; ****************************************************************************
; nasm blocks4.s -l blocks4.txt -o BLOCKS4.PRG -Z error.txt
; (modified from 'blocks4.s', 22/02/2021)

; 'sysvideo' bh = 1, block copy and modification test (VGA mode 13h)

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
	;jmp	terminate

	; set VGA mode by using int 31h
	mov	ax, 13h	; mode 13h ; 
	int	31h	; real mode: int 10h
	;jmp	short mode_13h_set_ok

mode_13h_set_ok: 
	mov	ecx, 0F0F0F0Fh ; white
	mov	ebx, 0101h ; Full screen, new color
	sys	_video

	;mov	byte [tcolor], 0
	mov	esi, 68*65536+72
	mov	ebp, txt_white
 	call	print_text

	call	waitforkey

	; full screen replace color (replace black colors)
	mov	cl, 0
	mov	dl, 28h ; RED
	mov	bl, 0Ch
	sys	_video

	call	waitforkey

	; full screen replace color (replace white colors)
	mov	cl, 0Fh ; WHITE
	mov	dl, 0 ; BLACK
	;mov	bl, 0Ch
	sys	_video
	
	; full screen replace color (replace red colors)
	mov	cl, 28h ; RED
	mov	dl, 0Fh ; WHITE
	;mov	bl, 0Ch
	sys	_video
	
	; now screen color is black and text color is white

	call	waitforkey

	mov	ecx, 20202020h ; blue
	mov	ebx, 0101h ; Full screen, new color
	sys	_video

	mov	byte [tcolor], 0Fh
	mov	esi, 68*65536+90
	mov	ebp, txt_blue
 	call	print_text

	call	waitforkey

	; full screen replace color (replace white colors)
	mov	cl, 0Fh ; WHITE
	mov	dl, 2Ch ; YELLOW
	mov	bl, 0Ch
	sys	_video

	call	waitforkey
	
	; full screen replace color (replace blue colors)
	mov	cl, 20h ; BLUE
	mov	dl, 0 ; BLACK
	;mov	bl, 0Ch
	sys	_video
	
	; full screen replace color (replace yellow colors)
	mov	cl, 2Ch ; YELLOW
	mov	dl, 20h ; BLUE
	;mov	bl, 0Ch
	sys	_video

	; now screen color is black and text color is blue

	call	waitforkey

	mov	ecx, 28282828h ; red
	mov	ebx, 0101h ; Full screen, new color
	sys	_video

	;mov	byte [tcolor], 0Fh
	mov	esi, 68*65536+108
	mov	ebp, txt_red
 	call	print_text

	call	waitforkey

	; full screen replace color (replace white colors)
	mov	cl, 0Fh ; WHITE
	mov	dl, 2Ch ; YELLOW
	mov	bl, 0Ch
	sys	_video

	call	waitforkey
	
	; full screen replace color (replace red colors)
	mov	cl, 28h ; RED
	mov	dl, 0 ; BLACK
	;mov	bl, 0Ch
	sys	_video
	
	; full screen replace color (replace yellow colors)
	mov	cl, 2Ch ; YELLOW
	mov	dl, 28h ; RED
	;mov	bl, 0Ch
	sys	_video

	; now screen color is black and text color is red

	call	waitforkey

	; full screen - green color

	mov	ecx, 30303030h ; green 
	mov	ebx, 0101h ; Full screen, new color
	sys	_video

	mov	byte [tcolor], 0
	mov	esi, 68*65536+72
	mov	ebp, txt_green
 	call	print_text

	call	waitforkey

	; full screen replace color (replace black colors)
	mov	cl, 0	; BLACK
	mov	dl, 0Fh ; WHITE
	mov	bl, 0Ch
	sys	_video

	call	waitforkey
	
	; full screen replace color (replace green colors)
	mov	cl, 30h ; GREEN
	mov	dl, 0 ; BLACK
	;mov	bl, 0Ch
	sys	_video
	
	; full screen replace color (replace white colors)
	mov	cl, 0Fh ; WHITE
	mov	dl, 30h ; GREEN
	;mov	bl, 0Ch
	sys	_video

	; now screen color is black and text color is green

	call	waitforkey

	; full screen - yellow color

	mov	ecx, 2C2C2C2Ch ; yellow
	mov	ebx, 0101h ; Full screen, new color
	sys	_video

	mov	byte [tcolor], 0Fh
	mov	esi, 68*65536+54
	mov	ebp, txt_yellow
 	call	print_text

	call	waitforkey

	; full screen replace color (replace yellow colors)
	mov	cl, 2Ch ; YELLOW
	mov	dl, 0 ; BLACK
	mov	bl, 0Ch
	sys	_video
	
	; full screen replace color (replace white colors)
	mov	cl, 0Fh ; WHITE
	mov	dl, 2Ch ; YELLOW
	;mov	bl, 0Ch
	sys	_video

	; now screen color is black and text color is yellow

	call	waitforkey

	; Full screen copy
	mov	esi, fullscreen_buffer
	mov	edi, esi
	xor	eax, eax ; black
	mov	ecx, (320*5)/4
	rep	stosd
	mov	eax, 0F0F0F0Fh ; white
	mov	ecx, (320*2)/4	
	rep	stosd
	xor	eax, eax ; black
	mov	ecx, (320*3)/4	
	rep	stosd
	mov	eax, 20202020h ; blue
	mov	ecx, (320*90)/4
	rep	stosd
	mov	eax, 28282828h ; red
	mov	ecx, (320*90)/4
	rep	stosd
	xor	eax, eax ; black
	mov	ecx, (320*3)/4	
	rep	stosd
	mov	eax, 0F0F0F0Fh ; white
	mov	ecx, (320*2)/4	
	rep	stosd
	xor	eax, eax ; black
	mov	ecx, (320*5)/4	
	rep	stosd

	mov	ebx, 0100h ; Full screen copy
	sys	_video

	call	waitforkey

	mov	byte [tcolor], 0Fh

	mov	esi, 23*65536+43
	mov	ebp, txt_blue
 	call	print_text
	
	call	waitforkey

	mov	esi, 113*65536+43
	mov	ebp, txt_red
 	call	print_text
	
	call	waitforkey

	; screen copy and replace window sub functions

	; fill white color in 1st 10 rows 
	; in user's fullscreen buff
	mov	edi, fullscreen_buffer
	mov	ecx, (10*320)/4
	mov	eax, 0F0F0F0Fh
	rep	stosd
	; fill green color to 180 rows after white rows  
	mov	eax, 30303030h ; GREEN
	mov	ecx, (180*320)/4
	rep	stosd
	; fill white color in last 10 rows
	mov	eax, 0F0F0F0Fh
	mov	ecx, (10*320)/4
	rep	stosd

	; copy blue block (on screen) to user buffer
	; (overwrites green colors partially)
	mov	edi, fullscreen_buffer + (10*320)+40
	mov	ecx, 10*65536+40 ; column 40, row 10
	mov	edx, 90*65536+160 ; size: 90*160
	mov	ebx, 0141h ; system to user window copy
	sys	_video	

	; Replace white color (text) only in blue block
	; (blue block starts at row 10)
	
	mov	cl, 0Fh ; WHITE (current color)
	mov	dl, 0  ; BLACK (new color)
	mov	esi, 10*65536+40 ; column 40, row 10
	mov	edi, 90*65536+160 ; size: 90*160 
	mov	ebx, 011Ch ; Replace color in window
	sys	_video

	call	waitforkey

	; copy red block (on screen) to user buffer
	mov	edi, fullscreen_buffer + (100*320)+40
	mov	ecx, 100*65536+40 ; column 40, row 100
	mov	edx, 90*65536+160 ; size: 90*160
	mov	ebx, 0141h ; system to user window copy
	sys	_video	

	; Replace white color (text) only in red block
	; (red block starts at row 100)
	
	mov	cl, 0Fh ; WHITE (current color)
	mov	dl, 0  ; BLACK (new color)
	mov	esi, 100*65536+40 ; column 40, row 100
	mov	edi, 90*65536+160 ; size: 90*160 
	mov	ebx, 011Ch ; Replace color in window
	sys	_video

	call	waitforkey

	; copy red block to blue block position
	; (system to system copy)
	
	; copy red block (overwrite blue block)
	mov	ecx, 100*65536 ; column 0, row 100
	mov	edx, 90*65536+320 ; size: 90*320
	mov	esi, 10*65536
	mov	ebx, 010Dh ; system to system window copy
	sys	_video

	; Replace black color (text) only in blue block
	; (blue block starts at row 10)
	
	mov	cl, 0 ; BLACK (current color)
	mov	dl, 0Fh ; WHITE (new color)
	mov	esi, 10*65536+40 ; column 40, row 10
	mov	edi, 90*65536+160 ; size: 90*160 
	mov	ebx, 011Ch ; Replace color in window
	sys	_video
	
	call	waitforkey

	; fill green block on red block position	
	mov	cl, 30h
	mov	edx, 100*65536+0 
	mov	esi, 90*65536+320 ; size: 90*320
	mov	ebx, 0111h ; new color, window
	sys	_video

	call	waitforkey

	; copy	blocks to system from user's buffer
	mov	esi, blockdatabuffer ; 32+32 bits
	mov	cl, 20h ; BLUE BLOCKS
	mov	edx, 6 ; 6 blocks
 	mov	ebx, 012Dh ; indirect pixel blocks
 	sys	_video

	call	waitforkey

	; replace color
	mov	cl, 20h ; BLUE (current color)
	mov	dl, 28h ; RED (new color)
	mov	esi, 100*65536 ; column 0, row 100
	mov	edi, 90*65536+320 ; size: 90*320
	mov	ebx, 011Ch ; Replace color in window
	sys	_video

	call	waitforkey

	; copy blue block (with 'blue" text)
	; from users full screen buffer to system
	; (to its old position)
	
	mov	ecx, 10*65536+40
	mov	edx, 90*65536+160
	mov	esi, fullscreen_buffer+(10*320)+40
	mov	ebx, 0110h ; copy from user to sys 
	sys	_video

	call	waitforkey

	; copy red block (with 'red" text)
	; from users full screen buffer to system
	; (to its old position)
	mov	ecx, 100*65536+40
	mov	edx, 90*65536+160
	mov	esi, fullscreen_buffer+(100*320)+40
	mov	ebx, 0110h ; copy from user to sys 
	sys	_video

	call	waitforkey

	; copy (full) screen to full screen buffer
	mov	edi, fullscreen_buffer
	mov	ebx, 0140h ; copy from sys to user 
	sys	_video

	; replace color (full screen)
	mov	cl, 0Fh ; WHITE (current color)
	mov	dl, 0  ; BLACK (new color)
	mov	ebx, 010Ch ; Replace color on screen
	sys	_video

	call	waitforkey

	; full screen NOT operation
	mov	ebx, 0107h
	sys	_video

	call	waitforkey

	; copy full screen buffer to screen
	mov	esi, fullscreen_buffer
	mov	ebx, 0100h
	sys	_video
	
	call	waitforkey  
		; wait for key stroke before exit
terminate:
	call	set_text_mode
	sys	_exit
halt:
	jmp	short halt

waitforkey:
	mov	ah, 1
	int	32h
	jz	short getkey
	inc	dword [counter]
	nop
	nop
	nop
	jmp	short waitforkey
getkey:
	xor	ah, ah
	int	32h

	cmp	ax, 2E03h
	je	short _terminate
	cmp	al, 1Bh ; ESC key
	je	short _terminate
	retn
_terminate:
	pop	eax ; return address
	jmp	short terminate
	
set_text_mode:
	xor    ah, ah
	mov    al, 3                        
 	;int   10h ; al = 03h text mode, int 10 video
	int    31h ; TRDOS 386 - Video interrupt
	retn

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

print_text:
	; ebp = text address
	; esi = row/column position (si = column)
p_d_x:
	;mov	dh, 0 ; 8x16 system font
	mov	dh, 6 ; 32*64 scaled font (base: 8*16 system font) 
p_d_x_n:
	mov	dl, [ebp]
	and	dl, dl
	jz	short p_d_x_ok
	sys	_video, 010Fh, [tcolor] 
	inc	ebp
	add	si, 36 ; next char pos
	jmp	short p_d_x_n
p_d_x_ok:
	retn

program_msg:
	db "TRDOS 386 v2.0.3 - ('sysvideo') Test Program - Block Operations"
	db 0Dh, 0Ah
	db "by Erdogan Tan - 27/02/2021"
	;db 0Dh, 0Ah, 0
	db 0Dh, 0Ah, 0Dh, 0Ah
	db "Press any key to continue .."
	db 0Dh, 0Ah	
	db "(Press ESC to exit) .."
	db 0Dh, 0Ah
	db 0Dh, 0Ah

nextline:
	db 0Dh, 0Ah, 0

txt_blue:
	db "BLUE", 0
txt_red:
	db "RED", 0
txt_green:
	db "GREEN", 0
txt_yellow:
	db "YELLOW", 0
txt_white:
	db "WHITE", 0
txt_black:
	db "BLACK", 0

blockdatabuffer:
	dd	10*65536	; block 1 position
	dd	90*65536+40	; block 1 size (w, h)
	dd	100*65536+40	; block 5 position
	dd	90*65536+160	; block 5 size (w, h)
	dd	10*65536+200	; block 3 position
	dd	90*65536+120	; block 3 size (w, h)
	dd	100*65536+200	; block 6 position
	dd	90*65536+120	; block 6 size (w, h)
	dd	10*65536+40	; block 2 position
	dd	90*65536+160	; block 2 size (w, h)
	dd	100*65536	; block 4 position
	dd	90*65536+40	; block 4 size (w, h)
	db	0		
bss:

ABSOLUTE bss

alignb 4

counter:
	resd 1	

bss_start:
tcolor: resd 1

fullscreen_buffer:
	resb 64000
bss_end: