; ****************************************************************************
; blocks16.s - TRDOS 386 (TRDOS v2.0.3) Test Program - 'sysvideo' pixel tests
; ----------------------------------------------------------------------------
;
; 02/03/2021
;
; ****************************************************************************
; nasm blocks16.s -l blocks16.txt -o BLOCKS16.PRG -Z error.txt
; (modified from 'blocks15.s', 02/03/2021 & 'blocks13.s', 01/03/2021)

; 'sysvideo' bh = 2, block copy and modification test (VESA VBE mode 112h)
; (mask color version)

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
	mov	ecx, 0FFFFFFh ; WHITE
	mov	ebx, 0201h ; Full screen, new color
	sys	_video

	;mov	dword [tcolor], 0
	mov	esi, 208*65536+240
	mov	ebp, txt_white
 	call	print_text

	call	waitforkey

	; full screen replace color (replace black colors)
	sub	ecx, ecx ; 0 ; BLACK
	mov	edx, 0FF8000h ; ORANGE
	mov	bl, 0Ch
	sys	_video
	
	call	waitforkey

	; Mask color = black
	; full screen NOT (except mask color)
	mov	edi, 0FF8000h ; ORANGE
	mov	ebx, 0227h ; masked 'NOT', full screen
	sys	_video
	
	call	waitforkey

	; full screen replace color (replace black colors)
	mov	ecx, 0FF8000h ; ORANGE
	mov	edx, 0FFFFFFh ; WHITE
	mov	bl, 0Ch
	sys	_video
	
	call	waitforkey

	; full screen - blue color 

	mov	ecx, 0FFh ; BLUE
	mov	bl, 01h ; Full screen, new color
	sys	_video

	mov	dword [tcolor], 0FFFFFFh
	mov	esi, 208*65536+258
	mov	ebp, txt_blue
 	call	print_text

	call	waitforkey

	; Mask color = white
	; full screen NOT (except mask color)
	mov	edi, 0FFFFFFh
	mov	bl, 27h ; masked 'NOT', full screen
	sys	_video

	call	waitforkey
	
	mov	ecx, 0FF0000h ; RED
	mov	bl, 01h ; Full screen, new color
	sys	_video

	;mov	dword [tcolor], 0FFFFFFh
	mov	esi, 208*65536+276
	mov	ebp, txt_red
 	call	print_text

	call	waitforkey

	; Mask color = red
	; full screen NEW COLOR (except mask color)
	mov	edi, 0FF0000h ; mask color, RED
	mov	ecx, 0FFFF00h ; YELLOW
	mov	bl, 21h ; masked new color, full screen
	sys	_video

	call	waitforkey
	
	; full screen replace color (replace yellow colors)
	mov	ecx, 0FFFF00h ; YELLOW
	mov	edx, 0FF8000h ; new color, ORANGE
	mov	bl, 0Ch
	sys	_video

	call	waitforkey

	; full screen - green color

	mov	ecx, 0FF00h ; GREEN
	mov	ebx, 0201h ; Full screen, new color
	sys	_video

	mov	dword [tcolor], 0
	mov	esi, 208*65536+240
	mov	ebp, txt_green
 	call	print_text

	call	waitforkey

	; Masked new color
	mov	edi, 0FF00h ; mask color, GREEN
	mov	ecx, 0FFFFFFh ; WHITE
	mov	bl, 21h ; masked new color, full screen
	sys	_video

	call	waitforkey
	
	; Masked mix colors
	mov	edi, 0FFFFFFh ; WHITE
	mov	ecx, 0FFh 
	mov	bl, 2Bh  ; masked MIX colors, full screen
	sys	_video

	call	waitforkey

	; full screen - yellow color

	mov	ecx, 0FFFF00h ; YELLOW
	mov	ebx, 0201h ; Full screen, new color
	sys	_video

	;mov	dword [tcolor], 0
	mov	esi, 208*65536+222
	mov	ebp, txt_yellow
 	call	print_text

	call	waitforkey

	; masked add color
	xor	edi, edi ; mask color is BLACK 
	mov	ecx, 80h ; add 80h to current color
	mov	bl, 22h
	sys	_video

	call	waitforkey

	; masked sub color
	;xor	edi, edi ; mask color is BLACK 
	;mov	ecx, 80h ; sub 80h from current color
	mov	bl, 23h
	sys	_video

	call	waitforkey

	; masked AND colors
	;xor	edi, edi ; mask color is BLACK
	mov	ecx, 303000h ; and 30h with current color
	mov	bl, 25h
	sys	_video
	
	call	waitforkey

	; masked OR colors
	mov	edi, 0FFFF00h ; mask color is Yellow
	mov	ecx, 002040h ; or 40h with current color
	mov	bl, 24h
	sys	_video
	
	call	waitforkey

	; masked XOR colors
	;mov	edi, 0FFFF00h ; mask color is Yellow
	mov	ecx, 2F2F2Fh ; xor 2Fh with current color
	mov	bl, 26h
	sys	_video

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

	; Masked new color, window
	; (blue block starts at row 20)
	; ((white text color will be changed to black))
	mov	edi, 0FFh ; mask color, BLUE
	xor	ecx, ecx ; 0 ; BLACK (new color)
	mov	edx, 20*65536+40 ; column 40, row 20
	mov	esi, 110*65536+160 ; size: 110*160
	mov	ebx, 0231h ; Masked new color in window
	sys	_video

	call	waitforkey

	; Masked AND colors, window
	; (red block starts at row 130)
	; ((white text color will be changed to black))
	mov	edi, 0FF0000h ; mask color, RED
	;xor	ecx, ecx ; BLACK (and color)
	mov	edx, 130*65536+40 ; column 40, row 130
	mov	esi, 110*65536+160 ; size: 110*160
	;mov	ebx, 0235h ; Masked AND colors in window
	mov	bl, 35h
	sys	_video

	call	waitforkey

	; Masked ADD to yellow block position
	sub	edi, edi ; mask color, BLACK
	mov	cl, 88h ; add 88h to current color
	mov	edx, 350*65536 ; column 0, row 350
	mov	esi, 110*65536+640 ; size: 110*640
	;mov	ebx, 0232h ; add color, window, masked
	mov	bl, 32h
	sys	_video	

	call	waitforkey

	; Masked SUB from green block position
	;sub	edi, edi ; mask color, BLACK
	;mov	ecx, 88h ; sub 88h from current color
	mov	edx, 240*65536 ; column 0, row 240
	;mov	esi, 110*65536+640 ; size: 110*640
	;mov	ebx, 0233h ; sub color, window, masked
	mov	bl, 33h
	sys	_video	

	call	waitforkey

	; Masked SUB from yellow block position
	;sub	edi, edi ; mask color, BLACK
	;mov	cl, 88h ; add 88h to current color
	mov	edx, 350*65536 ; column 0, row 350
	;mov	esi, 110*65536+640 ; size: 110*640
	;mov	ebx, 0233h ; sub color, window, masked
	sys	_video	

	call	waitforkey

	; Masked ADD to green block position
	;sub	edi, edi ; mask color, BLACK
	;mov	cl, 88h ; sub 88h from current color
	mov	edx, 240*65536 ; column 0, row 240
	;mov	esi, 110*65536+640 ; size: 110*640
	;mov	ebx, 0232h ; add color, window, masked
	mov	bl, 32h
	sys	_video	

	call	waitforkey

	; Masked OR colors, window
	; (white block starts at row 10)
	mov	edi, 0FFh ; mask color, BLUE
	mov	ecx, 808000h ; OR value (with current color)
	mov	edx, 10*65536+0 ; column 0, row 10
	mov	esi, 120*65536+640 ; size: 120*640
	mov	ebx, 0234h ; Masked OR colors in window
	sys	_video

	call	waitforkey

	; Masked XOR colors, window
	; (white block starts at row 465)
	mov	edi, 0FFFF00h ; mask color, YELLOW
	mov	ecx, 4F4F4Fh ; XOR value (with current color)
	mov	edx, 350*65536+0 ; column 0, row 465
	mov	esi, 120*65536+640 ; size: 120*640
	;mov	ebx, 0236h ; Masked XOR colors in window
	mov	bl, 34h
	sys	_video

	call	waitforkey

	; Masked mix color, window
	; (blue block starts at row 20)
	mov	edi, 0FFh ; mask color, BLUE
	mov	ecx, 303030h
	mov	edx, 20*65536+40 ; column 40, row 20
	mov	esi, 110*65536+160 ; size: 110*160
	;mov	ebx, 023Bh ; Masked mix colors in window
	mov	bl, 3Bh
	sys	_video

	call	waitforkey

	; Masked mix color, window
	; (red block starts at row 130)
	mov	edi, 0FF0000h ; mask color, RED
	mov	ecx, 8E8E8Eh
	mov	edx, 130*65536+40 ; column 40, row 130
	mov	esi, 110*65536+160 ; size: 110*160
	;mov	ebx, 023Bh ; Masked mix colors in window
	sys	_video

	call	waitforkey

	; Masked mix color, window
	; (green block starts at row 240)
	mov	edi, 0FF00h ; mask color, GREEN
	mov	ecx, 808080h
	mov	edx, 240*65536+40 ; column 40, row 240
	mov	esi, 110*65536+180 ; size: 110*180
	;mov	ebx, 023Bh ; Masked mix colors in window
	sys	_video

	call	waitforkey

	; copy full screen buffer to screen
	mov	esi, fullscreen_buffer
	;mov	ebx, 0200h
	xor	bl, bl ; mov bl, 0
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
	db "by Erdogan Tan - 02/03/2021"
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