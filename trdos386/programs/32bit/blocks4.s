; ****************************************************************************
; blocks4.s - TRDOS 386 (TRDOS v2.0.3) Test Program - 'sysvideo' pixel tests
; ----------------------------------------------------------------------------
;
; 22/02/2021
;
; ****************************************************************************
; nasm blocks4.s -l blocks4.txt -o BLOCKS4.PRG -Z error.txt
; (modified from 'blocks3.s', 21/02/2021)

; 'sysvideo' bh = 2, block copy and modification test (VESA VBE mode 101h)

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
	jmp	terminate

set_vesa_mode_101h_ok:
	; full screen - white color 
	mov	ecx, 0F0F0F0Fh ; white
	mov	ebx, 0201h ; Full screen, new color
	sys	_video

	; Black "white" text 
	;mov	byte [tcolor], 0
	mov	esi, 208*65536+240
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

	; full screen - blue color 

	mov	ecx, 20202020h ; blue
	mov	ebx, 0201h ; Full screen, new color
	sys	_video

	mov	byte [tcolor], 0Fh
	mov	esi, 208*65536+258
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

	mov	ecx, 28282828h ; RED
	mov	ebx, 0201h ; Full screen, new color
	sys	_video

	;mov	byte [tcolor], 0Fh
	mov	esi, 208*65536+276
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
	mov	ebx, 0201h ; Full screen, new color
	sys	_video

	mov	byte [tcolor], 0
	mov	esi, 208*65536+240
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
	mov	ebx, 0201h ; Full screen, new color
	sys	_video

	;mov	byte [tcolor], 0
	mov	esi, 208*65536+222
	mov	ebp, txt_yellow
 	call	print_text

	call	waitforkey

	; full screen replace color (replace black colors)
	mov	cl, 0	; BLACK
	mov	dl, 0Fh ; WHITE
	mov	bl, 0Ch
	sys	_video

	call	waitforkey
	
	; full screen replace color (replace yellow colors)
	mov	cl, 2Ch ; YELLOW
	mov	dl, 0 ; BLACK
	;mov	bl, 0Ch
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
	mov	ecx, (640*10)/4
	rep	stosd
	mov	eax, 0F0F0F0Fh ; white
	mov	ecx, (640*5)/4	
	rep	stosd
	xor	eax, eax ; black
	mov	ecx, (640*5)/4	
	rep	stosd
	mov	eax, 20202020h ; blue
	mov	ecx, (640*110)/4
	rep	stosd
	mov	eax, 28282828h ; red
	mov	ecx, (640*110)/4
	rep	stosd
	mov	eax, 30303030h ; green 
	mov	ecx, (640*110)/4
	rep	stosd
	mov	eax, 2C2C2C2Ch ; yellow
	mov	ecx, (640*110)/4
	rep	stosd
	xor	eax, eax ; black
	mov	ecx, (640*5)/4	
	rep	stosd
	mov	eax, 0F0F0F0Fh ; white
	mov	ecx, (640*5)/4	
	rep	stosd
	xor	eax, eax ; black
	mov	ecx, (640*10)/4	
	rep	stosd

	mov	ebx, 0200h ; Full screen copy
	sys	_video

	call	waitforkey

	mov	byte [tcolor], 0Fh

	mov	esi, 43*65536+43
	mov	ebp, txt_blue
 	call	print_text
	
	call	waitforkey

	mov	esi, 153*65536+43
	mov	ebp, txt_red
 	call	print_text
	
	call	waitforkey

	mov	esi, 263*65536+43
	mov	ebp, txt_green
 	call	print_text
	
	call	waitforkey

	mov	esi, 373*65536+43
	mov	ebp, txt_yellow
 	call	print_text
	
	call	waitforkey

	mov	byte [tcolor], 0

	mov	esi, 263*65536+43
	mov	ebp, txt_green
 	call	print_text
	
	call	waitforkey

	mov	esi, 373*65536+43
	mov	ebp, txt_yellow
 	call	print_text
	
	call	waitforkey

	; screen copy and replace window sub functions

	; fill white color in 1st 20 rows 
	; in user's fullscreen buff
	mov	edi, fullscreen_buffer
	mov	ecx, (20*640)/4
	mov	eax, 0F0F0F0Fh
	rep	stosd
	; fill red color to 440 rows after white rows  
	mov	eax, 28282828h ; RED
	mov	ecx, (440*640)/4
	rep	stosd
	; fill white color in last 20 rows
	mov	eax, 0F0F0F0Fh
	mov	ecx, (20*640)/4
	rep	stosd

	; copy blue block (on screen) to user buffer
	; (overwrites red colors partially)
	mov	edi, fullscreen_buffer + (20*640)+40
	mov	ecx, 20*65536+40 ; column 40, row 20
	mov	edx, 110*65536+160 ; size: 110*160
	mov	ebx, 0241h ; system to user window copy
	sys	_video	

	; Replace white color (text) only in blue block
	; (blue block starts at row 20)
	
	mov	cl, 0Fh ; WHITE (current color)
	mov	dl, 0  ; BLACK (new color)
	mov	esi, 20*65536+40 ; column 40, row 20
	mov	edi, 110*65536+160 ; size: 110*160 
	mov	ebx, 021Ch ; Replace color in window
	sys	_video

	call	waitforkey

	; copy red block (on screen) to user buffer
	mov	edi, fullscreen_buffer + (130*640)+40
	mov	ecx, 130*65536+40 ; column 40, row 130
	mov	edx, 110*65536+120 ; size: 110*120
	mov	ebx, 0241h ; system to user window copy
	sys	_video	

	; Replace white color (text) only in red block
	; (red block starts at row 130)
	
	mov	cl, 0Fh ; WHITE (current color)
	mov	dl, 0  ; BLACK (new color)
	mov	esi, 130*65536+40 ; column 40, row 130
	mov	edi, 110*65536+120 ; size: 110*120 
	mov	ebx, 021Ch ; Replace color in window
	sys	_video

	; copy yellow block (on screen) to user buffer
	mov	edi, fullscreen_buffer + (350*640)
	mov	ecx, 350*65536+0 ; column 0, row 350
	mov	edx, 110*65536+640 ; size: 110*640
	mov	ebx, 0241h ; system to user window copy
	sys	_video		

	; copy green block (on screen) to user buffer
	mov	edi, fullscreen_buffer + (240*640)
	mov	ecx, 240*65536+0 ; column 0, row 240
	mov	edx, 110*65536+640 ; size: 110*640
	mov	ebx, 0241h ; system to user window copy
	sys	_video

	call	waitforkey

	; copy yellow block to red block position
	; and green block to blue block position on screen
	; (system to system copy)
	
	; copy yellow block (overwrite red block)
	mov	ecx, 350*65536 ; column 0, row 350
	mov	edx, 110*65536+640 ; size: 110*640
	mov	esi, 130*65536
	mov	ebx, 020Dh ; system to system window copy
	sys	_video	

	call	waitforkey

	; copy green block (overwrite blue block)
	mov	ecx, 240*65536 ; column 0, row 240
	mov	edx, 110*65536+640 ; size: 110*640
	mov	esi, 20*65536
	mov	ebx, 020Dh ; system to system window copy
	sys	_video	

	call	waitforkey

	; fill blue block on yellow block position	
	mov	cl, 20h
	mov	edx, 350*65536+0 
	mov	esi, 110*65536+640 ; size: 110*640
	mov	ebx, 0211h ; new color, window
	sys	_video

	call	waitforkey

	; fill red block on green block position	
	mov	cl, 28h
	mov	edx, 240*65536+0 
	mov	esi, 110*65536+640 ; size: 110*640
	mov	ebx, 0211h ; new color, window
	sys	_video

	call	waitforkey

	; copy	blocks to system from user's buffer
	mov	esi, blockdatabuffer ; 32+32 bits
	mov	cl, 20h ; BLUE BLOCKS
	mov	edx, 7 ; 7 blocks
 	mov	ebx, 022Dh ; indirect pixel blocks
 	sys	_video

	call	waitforkey

	; replace color
	mov	cl, 2Ch ; YELLOW (current color)
	mov	dl, 20h ; BLUE (new color)
	mov	esi, 130*65536 ; column 0, row 130
	mov	edi, 110*65536+640 ; size: 110*640
	mov	ebx, 021Ch ; Replace color in window
	sys	_video

	call	waitforkey

	; copy blue block (with 'blue" text)
	; from users full screen buffer to system
	; (to its old position)
	
	mov	ecx, 20*65536+40
	mov	edx, 110*65536+160
	mov	esi, fullscreen_buffer+(20*640)+40
	mov	ebx, 0210h ; copy from user to sys 
	sys	_video

	; copy blue block to usr's buffer again
	mov	ecx, 20*65536
	mov	edx, 110*65536+640
	mov	edi, fullscreen_buffer+(20*640)
	mov	ebx, 0241h ; copy from sys to user 
	sys	_video

	call	waitforkey

	; fill red block on red block position	
	mov	cl, 28h
	mov	edx, 130*65536+0 
	mov	esi, 110*65536+640 ; size: 110*640
	mov	ebx, 0211h ; new color, window
	sys	_video

	call	waitforkey

	; copy red block (with 'red" text)
	; from users full screen buffer to system
	; (to its old position)
	mov	ecx, 130*65536+40
	mov	edx, 110*65536+120
	mov	esi, fullscreen_buffer+(130*640)+40
	mov	ebx, 0210h ; copy from user to sys 
	sys	_video

	; copy red block to usr's buffer again
	mov	ecx, 130*65536
	mov	edx, 110*65536+640
	mov	edi, fullscreen_buffer+(130*640)
	mov	ebx, 0241h ; copy from sys to user 
	sys	_video

	call	waitforkey

	; replace color (full screen)
	mov	cl, 0Fh ; WHITE (current color)
	mov	dl, 0  ; BLACK (new color)
	mov	ebx, 020Ch ; Replace color on screen
	sys	_video

	call	waitforkey

	; copy full screen buffer to screen
	mov	esi, fullscreen_buffer
	mov	ebx, 0200h
	sys	_video

	; erase full screen buffer
	mov	ecx, (640*480)/4
	xor	eax, eax
	mov	edi, esi ; fullscreen_buffer
	rep	stosd

	call	waitforkey

	; copy (full) screen to full screen buffer
	mov	edi, fullscreen_buffer
	mov	ebx, 0240h ; copy from sys to user 
	sys	_video

	; full screen NOT operation
	mov	ebx, 0207h
	sys	_video

	call	waitforkey

	; copy full screen buffer to screen
	mov	esi, fullscreen_buffer
	mov	ebx, 0200h
	sys	_video
	
	; here.. it is the End of TRDOS 386 v2.0.3
	; 'sysvideo' LFB block/window and full screen
	; pixel-data transfer function tests
	; (for VESA VBE video modes). 22/01/2021
	;
	; NOTE: This test program (blocks4.s) 
	; contains only non-masked data transfer tests.
	; (same functions must be tested for color-masked
	;  pixel operations, also.) 

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
	sys	_video, 020Fh, [tcolor] 
	inc	ebp
	add	si, 36 ; next char pos
	jmp	short p_d_x_n
p_d_x_ok:
	retn

program_msg:
	db "TRDOS 386 v2.0.3 - ('sysvideo') Test Program - Block Operations"
	db 0Dh, 0Ah
	db "by Erdogan Tan - 22/02/2021"
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
	dd	20*65536
	dd	110*65536+40
	dd	130*65536+200
	dd	110*65536+440
	dd	130*65536+40
	dd	110*65536+160
	dd	350*65536
	dd	110*65536+640
	dd	20*65536+40
	dd	110*65536+160
	dd	240*65536
	dd	110*65536+640
	dd	20*65536+200
	dd	110*65536+440
	db	0		
bss:

ABSOLUTE bss

alignb 4

counter:
	resd 1	

bss_start:
tcolor: resd 1

fullscreen_buffer:
	resb 307200
bss_end: