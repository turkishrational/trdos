; ****************************************************************************
; blocks13.s - TRDOS 386 (TRDOS v2.0.3) Test Program - 'sysvideo' pixel tests
; ----------------------------------------------------------------------------
;
; 01/03/2021
;
; ****************************************************************************
; nasm blocks13.s -l blocks13.txt -o BLOCKS13.PRG -Z error.txt
; (modified from 'blocks12.s', 01/03/2021 & 'blocks10.s', 28/02/2021)

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

	; Get VESA VBE video bios number 
	;	(vbe2 or vbe3, emulator or not)  
	;mov	ebx, 0906h
	;sys	_video
	
	; 01/03/2021
	; get [truecolor] status (24bpp or 32bpp for VBE3 vbios)
	mov	ebx, 0909h
	sys	_video

	mov	ecx, 112h ; VESA VBE video mode	(640x480, 24bpp)
	
	;cmp	ah, 3
	;je	short set_vmode
	;cmp	ah, 2
	;;jne	terminate ; invalid !?
	;jne	short set_vmode
	;and	al, al
	;jz	short set_vmode ; invalid !? (AL must be >= C0h.)
	
	; 01/03/2021
	cmp	al, 32
	je	short set_vmode
	
	; VBE2 bios or default truecolor bpp is 24 bpp	
	; so, we are using video mode 142h 
	; (TRDOS 386 running in emulator or very old hardware!)
	
	; BOCHS/QEMU/VBOX emulator 
	mov	cl, 42h ; Bochs/Plex86 video mode 142h
			; (640*480, 32bpp) 		

	; Set Video Mode to 112h ; 640x480, 32 bit true colors
	;
	; NOTE: NVIDIA GEFORCE FX 550 VIDEO BIOS uses 32bpp 
	;       instead of 24bpp for same VBE video mode numbers. 
	;       So, 112h is 640*480, 24bpp for BOCHS/QEMU emulator
	;	but, it is 640*480, 32 bpp for real computer 
	;	with NVIDIA graphics card and video bios. 
	; (Also it is -it must be- 32bpp for other new hardware.)	  	

	;sys	_video, 08FFh, 112h
set_vmode:
	; ecx = VESA VBE video mode
	sys	_video, 08FFh
	or	eax, eax
	;jz	short terminate
	;mov	[LFB_ADDR], edx ; pointer to LFB info table/structure
	jnz	short set_vesa_mode_112h_ok
	jmp	terminate

set_vesa_mode_112h_ok:
	mov	ecx, 0FFFFFFh ; white
	mov	ebx, 0201h ; Full screen, new color
	sys	_video

	;mov	dword [tcolor], 0
	mov	esi, 208*65536+240
	mov	ebp, txt_white
 	call	print_text

	call	waitforkey

	; full screen replace color (replace black colors)
	sub	ecx, ecx ; 0 ; BLACK
	mov	edx, 0FF0000h ; RED
	mov	bl, 0Ch
	sys	_video

	call	waitforkey

	; full screen replace color (replace white colors)
	mov	ecx, 0FFFFFFh  ; WHITE
	sub	edx, edx ; 0 ; BLACK
	;mov	bl, 0Ch
	sys	_video
	
	; full screen replace color (replace red colors)
	mov	ecx, 0FF0000h ; RED
	mov	edx, 0FFFFFFh  ; WHITE
	;mov	bl, 0Ch
	sys	_video
	
	; now screen color is black and text color is white

	call	waitforkey

	; full screen - blue color 

	mov	ecx, 0FFh  ; Blue
	mov	ebx, 0201h ; Full screen, new color
	sys	_video

	mov	dword [tcolor], 0FFFFFFh
	mov	esi, 208*65536+258
	mov	ebp, txt_blue
 	call	print_text

	call	waitforkey

	; full screen replace color (replace white colors)
	mov	ecx, 0FFFFFFh ; WHITE
	mov	edx, 0FFFF00h ; YELLOW
	mov	bl, 0Ch
	sys	_video

	call	waitforkey
	
	; full screen replace color (replace blue colors)
	mov	ecx, 0FFh ; BLUE
	xor	edx, edx ; 0 ; BLACK
	;mov	bl, 0Ch
	sys	_video
	
	; full screen replace color (replace yellow colors)
	mov	ecx, 0FFFF00h ; YELLOW
	mov	edx, 0FFh ; BLUE
	;mov	bl, 0Ch
	sys	_video

	; now screen color is black and text color is blue

	call	waitforkey

	mov	ecx, 0FF0000h ; RED
	mov	ebx, 0201h ; Full screen, new color
	sys	_video

	;mov	byte [tcolor], 0Fh
	mov	esi, 208*65536+276
	mov	ebp, txt_red
 	call	print_text

	call	waitforkey

	; full screen replace color (replace white colors)
	mov	ecx, 0FFFFFFh ; WHITE
	mov	edx, 0FFFF00h ; YELLOW
	mov	bl, 0Ch
	sys	_video

	call	waitforkey
	
	; full screen replace color (replace red colors)
	mov	ecx, 0FF0000h ; RED
	sub	edx, edx ; 0 ; BLACK
	;mov	bl, 0Ch
	sys	_video
	
	; full screen replace color (replace yellow colors)
	mov	ecx, 0FFFF00h ; YELLOW
	mov	edx, 0FF0000h ; RED
	;mov	bl, 0Ch
	sys	_video

	; now screen color is black and text color is red

	call	waitforkey

	; full screen - green color

	mov	ecx, 0FF00h ; green 
	mov	ebx, 0201h ; Full screen, new color
	sys	_video

	mov	dword [tcolor], 0
	mov	esi, 208*65536+240
	mov	ebp, txt_green
 	call	print_text

	call	waitforkey

	; full screen replace color (replace black colors)
	sub	ecx, ecx ; 0 ; BLACK
	mov	edx, 0FFFFFFh  ; WHITE
	mov	bl, 0Ch
	sys	_video

	call	waitforkey
	
	; full screen replace color (replace green colors)
	mov	ecx, 0FF00h ; GREEN
	xor	edx, edx ; 0 ; BLACK
	;mov	bl, 0Ch
	sys	_video
	
	; full screen replace color (replace white colors)
	mov	ecx, 0FFFFFFh  ; WHITE
	mov	edx, 0FF00h ; GREEN 
	;mov	bl, 0Ch
	sys	_video

	; now screen color is black and text color is green

	call	waitforkey

	; full screen - yellow color

	mov	ecx, 0FFFF00Fh  ; yellow
	mov	ebx, 0201h ; Full screen, new color
	sys	_video

	;mov	dword [tcolor], 0
	mov	esi, 208*65536+222
	mov	ebp, txt_yellow
 	call	print_text

	call	waitforkey

	; full screen replace color (replace black colors)
	xor	ecx, ecx ; 0 ; BLACK
	mov	edx, 0FFFFFFh  ; WHITE
	mov	bl, 0Ch
	sys	_video

	call	waitforkey
	
	; full screen replace color (replace yellow colors)
	mov	ecx, 0FFFF00h ; YELLOW
	sub	edx, edx ; 0 ; BLACK
	;mov	bl, 0Ch
	sys	_video
	
	; full screen replace color (replace white colors)
	mov	ecx, 0FFFFFFh ; WHITE
	mov	edx, 0FFFF00h ; YELLOW
	;mov	bl, 0Ch
	sys	_video

	; now screen color is black and text color is yellow

	call	waitforkey

	; Full screen copy
	mov	esi, fullscreen_buffer
	mov	edi, esi

	; Black
	mov	ecx, 640*10
	xor	eax, eax ; black
	rep	stosd

	; White
	mov	ecx, 640*5
	mov	eax, 0FFFFFFh
	rep	stosd

	; Black
	mov	ecx, 640*5
	xor	eax, eax ; black
	rep	stosd

	; Blue
	mov	ecx, 640*110
	mov	al, 0FFh
	rep	stosd

	; Red
	mov	ecx, 640*110
	mov	eax, 0FF0000h
	rep	stosd

	; Green
	mov	ecx, 640*110
	mov	eax, 0FF00h
	rep	stosd

	; Yellow
	mov	ecx, 640*110
	mov	eax, 0FFFF00h
	rep	stosd

	; Black
	mov	ecx, 640*5
	xor	eax, eax ; black
	rep	stosd

	; White
	mov	ecx, 640*5
	mov	eax, 0FFFFFFh
	rep	stosd

	; Black
	mov	ecx, 640*10
	xor	eax, eax ; black
	rep	stosd

	mov	ebx, 0200h ; Full screen copy
	sys	_video

	call	waitforkey

	mov	dword [tcolor], 0FFFFFFh

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

	mov	dword [tcolor], 0

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
	mov	ecx, 20*640
	mov	eax, 0FFFFFFh
	rep	stosd
 
	; fill red color to 440 rows after white rows  
	mov	ecx, 440*640
	mov	eax, 0FF0000h ; RED
	rep	stosd

	; fill white color in last 20 rows
	mov	ecx, 20*640
	mov	eax, 0FFFFFFh
	rep	stosd

	; copy blue block (on screen) to user buffer
	; (overwrites red colors partially)
	mov	edi, fullscreen_buffer + ((20*640)+40)*4
	mov	ecx, 20*65536+40 ; column 40, row 20
	mov	edx, 110*65536+160 ; size: 110*160
	mov	ebx, 0241h ; system to user window copy
	;mov	bl, 41h
	sys	_video	

	; Replace white color (text) only in blue block
	; (blue block starts at row 20)
	
	mov	ecx, 0FFFFFFh ; WHITE (current color)
	sub	edx, edx ; 0 ; BLACK (new color)
	mov	esi, 20*65536+40 ; column 40, row 20
	mov	edi, 110*65536+160 ; size: 110*160 
	;mov	ebx, 021Ch ; Replace color in window
	mov	bl, 1Ch
	sys	_video

	call	waitforkey

	; copy red block (on screen) to user buffer
	mov	edi, fullscreen_buffer + ((130*640)+40)*4
	mov	ecx, 130*65536+40 ; column 40, row 130
	mov	edx, 110*65536+120 ; size: 110*120
	;mov	ebx, 0241h ; system to user window copy
	mov	bl, 41h
	sys	_video	

	; Replace white color (text) only in red block
	; (red block starts at row 130)
	
	mov	ecx, 0FFFFFFh ; WHITE (current color)
	sub	edx, edx ; 0 ; BLACK (new color)
	mov	esi, 130*65536+40 ; column 40, row 130
	mov	edi, 110*65536+120 ; size: 110*120 
	;mov	ebx, 021Ch ; Replace color in window
	mov	bl, 1Ch
	sys	_video

	; copy yellow block (on screen) to user buffer
	mov	edi, fullscreen_buffer + (350*640)*4
	mov	ecx, 350*65536+0 ; column 0, row 350
	mov	edx, 110*65536+640 ; size: 110*640
	;mov	ebx, 0241h ; system to user window copy
	mov	bl, 41h
	sys	_video		

	; copy green block (on screen) to user buffer
	mov	edi, fullscreen_buffer + (240*640)*4
	mov	ecx, 240*65536+0 ; column 0, row 240
	mov	edx, 110*65536+640 ; size: 110*640
	;;mov	ebx, 0241h ; system to user window copy
	;mov	bl, 41h
	sys	_video

	call	waitforkey

	; copy yellow block to red block position
	; and green block to blue block position on screen
	; (system to system copy)
	
	; copy yellow block (overwrite red block)
	mov	ecx, 350*65536 ; column 0, row 350
	mov	edx, 110*65536+640 ; size: 110*640
	mov	esi, 130*65536
	;mov	ebx, 020Dh ; system to system window copy
	mov	bl, 0Dh
	sys	_video	

	call	waitforkey

	; copy green block (overwrite blue block)
	mov	ecx, 240*65536 ; column 0, row 240
	mov	edx, 110*65536+640 ; size: 110*640
	mov	esi, 20*65536
	;;mov	ebx, 020Dh ; system to system window copy
	;mov	bl, 0Dh
	sys	_video	

	call	waitforkey

	; fill blue block on yellow block position	
	mov	ecx, 0FFh
	mov	edx, 350*65536+0 
	mov	esi, 110*65536+640 ; size: 110*640
	;mov	ebx, 0211h ; new color, window
	mov	bl, 11h
	sys	_video

	call	waitforkey

	; fill red block on green block position	
	mov	ecx, 0FF0000h
	mov	edx, 240*65536+0 
	mov	esi, 110*65536+640 ; size: 110*640
	;;mov	ebx, 0211h ; new color, window
	;mov	bl, 11h
	sys	_video

	call	waitforkey

	; copy	blocks to system from user's buffer
	mov	esi, blockdatabuffer ; 32+32 bits
	mov	ecx, 0FFh
	mov	edx, 7 ; 7 blocks
 	;mov	ebx, 022Dh ; indirect pixel blocks
	mov	bl, 2Dh
 	sys	_video

	call	waitforkey

	; replace color
	mov	ecx, 0FFFF00h ; YELLOW (current color)
	mov	edx, 0FFh ; BLUE (new color)
	mov	esi, 130*65536 ; column 0, row 130
	mov	edi, 110*65536+640 ; size: 110*640
	;mov	ebx, 021Ch ; Replace color in window
	mov	bl, 1Ch
	sys	_video

	call	waitforkey

	; copy blue block (with 'blue" text)
	; from users full screen buffer to system
	; (to its old position)
	
	mov	ecx, 20*65536+40
	mov	edx, 110*65536+160
	mov	esi, fullscreen_buffer+((20*640)+40)*4
	;mov	ebx, 0210h ; copy from user to sys 
	mov	bl, 10h
	sys	_video

	; copy blue block to usr's buffer again
	mov	ecx, 20*65536
	mov	edx, 110*65536+640
	mov	edi, fullscreen_buffer+(20*640)*4
	;mov	ebx, 0241h ; copy from sys to user
	mov	bl, 41h 
	sys	_video

	call	waitforkey

	; fill red block on red block position	
	mov	ecx, 0FF0000h  ;RED
	mov	edx, 130*65536+0 
	mov	esi, 110*65536+640 ; size: 110*640
	;mov	ebx, 0211h ; new color, window
	mov	bl, 11h
	sys	_video

	call	waitforkey

	; copy red block (with 'red" text)
	; from users full screen buffer to system
	; (to its old position)
	mov	ecx, 130*65536+40
	mov	edx, 110*65536+120
	mov	esi, fullscreen_buffer+((130*640)+40)*4
	;mov	ebx, 0210h ; copy from user to sys 
	mov	bl, 10h
	sys	_video

	; copy red block to usr's buffer again
	mov	ecx, 130*65536
	mov	edx, 110*65536+640
	mov	edi, fullscreen_buffer+(130*640)*4
	;mov	ebx, 0241h ; copy from sys to user 
	mov	bl, 41h
	sys	_video

	call	waitforkey

	; replace color (full screen)
	mov	ecx, 0FFFFFFh ; WHITE (current color)
	xor	edx, edx ; 0 ; BLACK (new color)
	;mov	ebx, 020Ch ; Replace color on screen
	mov	bl, 0Ch
	sys	_video

	call	waitforkey

	; copy full screen buffer to screen
	mov	esi, fullscreen_buffer
	;mov	ebx, 0200h
	mov	bl, 0
	sys	_video

	; erase full screen buffer
	;mov	ecx, (640*480)*4/4
	mov	ecx, 640*480
	xor	eax, eax ; 0
	mov	edi, esi ; fullscreen_buffer
	rep	stosd

	call	waitforkey

	; copy (full) screen to full screen buffer
	mov	edi, fullscreen_buffer
	;mov	ebx, 0240h ; copy from sys to user 
	mov	bl, 40h
	sys	_video

	; full screen NOT operation
	;mov	ebx, 0207h
	mov	bl, 07h
	sys	_video

	call	waitforkey

	; copy full screen buffer to screen
	mov	esi, fullscreen_buffer
	;mov	ebx, 0200h
	xor	bl, bl ; 0
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
	sys	_video, 020Fh, [tcolor] 
	inc	ebp
	add	si, 36 ; next char pos
	jmp	short p_d_x_n
p_d_x_ok:
	retn

program_msg:
	db "TRDOS 386 v2.0.3 - ('sysvideo') Test Program - Block Operations"
	db 0Dh, 0Ah
	db "by Erdogan Tan - 01/03/2021"
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
	resb 307200*4
bss_end: