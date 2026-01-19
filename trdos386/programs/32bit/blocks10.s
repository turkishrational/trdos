; ****************************************************************************
; blocks10.s - TRDOS 386 (TRDOS v2.0.3) Test Program - 'sysvideo' pixel tests
; ----------------------------------------------------------------------------
;
; 28/02/2021
;
; ****************************************************************************
; nasm blocks10.s -l blocks10.txt -o BLOCKS10.PRG -Z error.txt
; (modified from 'blocks9.s', 28/02/2021)

; 'sysvideo' bh = 2, block copy and modification test (VESA VBE mode 112h)
; (640*480, 32 bit true colors version)


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
	mov	ebx, 0906h
	sys	_video
	mov	ecx, 112h ; VESA VBE video mode	(640x480, 24bpp)
	cmp	ah, 3
	je	short set_vmode
	cmp	ah, 2
	;jne	terminate ; invalid !?
	jne	short set_vmode
	and	al, al
	jz	short set_vmode ; invalid !? (AL must be >= C0h.)
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
	; Set initial block colors
	mov	ecx, 120*160
	mov	eax, 0FFFFFFh ; white 
	mov	edi, whiteblock
	rep	stosd

	mov	cx, 120*160
	mov	eax, 0FFh ; blue
	mov	edi, blueblock
	rep	stosd

	mov	cx, 120*160
	mov	eax, 0FF0000h ; red
	mov	edi, redblock
	rep	stosd

	mov	cx, 120*160
	mov	eax, 0FF00h ; green 
	mov	edi, greenblock
	rep	stosd

	mov	cx, 120*160
	mov	eax, 0FFFF00h ; yellow
	mov	edi, yellowblock
	rep	stosd

	; copy white block to screen
	; at row 70, column 120
	; (block size: 120(w)*160(h) 

	mov	ecx, (70*65536)+120
	mov	edx, (160*65536)+120 
	mov	esi, whiteblock
	mov	ebx, 0210h ; non masked window copy
	sys	_video

	; copy blue block to screen
	; at row 160, column 260
	; (block size: 120(w)*160(h) 

	mov	ecx, (160*65536)+260
	mov	edx, (160*65536)+120 
	mov	esi, blueblock
	mov	ebx, 0210h ; non masked window copy
	sys	_video

	; copy red block to screen
	; at row 250, column 120
	; (block size: 120(w)*160(h) 

	mov	ecx, (250*65536)+120
	mov	edx, (160*65536)+120 
	mov	esi, redblock
	mov	ebx, 0210h ; non masked window copy
	sys	_video

	; copy green block to screen
	; at row 70, column 400
	; (block size: 120(w)*160(h) 

	mov	ecx, (70*65536)+400
	mov	edx, (160*65536)+120 
	mov	esi, greenblock
	mov	ebx, 0210h ; non masked window copy
	sys	_video

	; copy yellow block to screen
	; at row 250, column 400
	; (block size: 120(w)*160(h) 

	mov	ecx, (250*65536)+400
	mov	edx, (160*65536)+120 
	mov	esi, yellowblock
	mov	ebx, 0210h ; non masked window copy
	sys	_video

	call	waitforkey

	; continue by using
	; window color modification sub functions

	; apply SUB to window's pixel colors
	mov	ecx, 408060h  
	mov	edx, (70*65536)+120
	mov	esi, (160*65536)+120
	mov	ebx, 0213h ; non masked window SUB op
	sys	_video

	call	waitforkey

	; apply ADD to pixel colors of
	; the window/block on row 70, column 120 with 
	; block size 120(w)*160(h).
	;mov	ecx, 408060h
	;mov	edx, (70*65536)+120
	;mov	esi, (160*65536)+120 
	;mov	ebx, 0212h ; non masked window ADD op
	mov	bl, 12h
	sys	_video

	call	waitforkey

	; apply ADD to pixel colors of
	; the window/block on row 250, column 120 with 
	; block size 120(w)*160(h).
	mov	ecx, 40h
	mov	edx, (250*65536)+120
	;mov	esi, (160*65536)+120 
	;mov	ebx, 0212h ; non masked window ADD op
	mov	bl, 12h
	sys	_video

	call	waitforkey

	; apply SUB to window's pixel colors
	;mov	ecx, 40h  
	;mov	edx, (250*65536)+120
	;mov	esi, (160*65536)+120 
	;mov	ebx, 0213h ; non masked window SUB op
	mov	bl, 13h
	sys	_video

	call	waitforkey

	; or, and, xor

	; apply AND to window's pixel colors
	mov	ecx, 077F7Fh 
	mov	edx, (70*65536)+120
	;mov	esi, (160*65536)+120 
	;mov	ebx, 0215h ; non masked window AND op
	mov	bl, 15h
	sys	_video

	call	waitforkey

	; apply AND to pixel colors of
	; the window/block on row 160, column 260 with 
	; block size 120(w)*160(h).
	;mov	ecx, 077F7Fh
	mov	edx, (160*65536)+260
	;mov	esi, (160*65536)+120 
	;mov	ebx, 0215h ; non masked window AND op
	sys	_video

	call	waitforkey

	; apply OR to pixel colors of
	; the window/block on row 70, column 400 with 
	; block size 120(w)*160(h).
	mov	ecx, 103050h
	mov	edx, (70*65536)+400
	;mov	esi, (160*65536)+120 
	;mov	ebx, 0214h ; non masked window OR op
	mov	bl, 14h
	sys	_video

	call	waitforkey

	; apply OR to pixel colors of
	; the window/block on row 250, column 400 with 
	; block size 120(w)*160(h).
	;mov	ecx, 103050h
	mov	edx, (250*65536)+400
	;mov	esi, (160*65536)+120 
	;mov	ebx, 0214h ; non masked window OR op
	sys	_video

	call	waitforkey

	; apply XOR to pixel colors of
	; the window/block on row 250, column 120 with 
	; block size 120(w)*160(h).
	mov	ecx, 3F7F4Fh
	mov	edx, (250*65536)+120
	;mov	esi, (160*65536)+120 
	;mov	ebx, 0216h ; non masked window XOR op
	mov	bl, 16h
	sys	_video

	call	waitforkey

	; apply XOR to pixel colors of
	; the window/block on row 70, column 400 with 
	; block size 120(w)*160(h).
	;mov	ecx, 3F7F4Fh
	mov	edx, (70*65536)+400
	;mov	esi, (160*65536)+120 
	;mov	ebx, 0216h ; non masked window XOR op
	sys	_video

	call	waitforkey

	; Mix colors

	; MIX pixel colors of
	; the window/block on row 70, column 120 with 
	; block size 120(w)*160(h).
	mov	ecx, 102040h
	mov	edx, (70*65536)+120
	;mov	esi, (160*65536)+120 
	;mov	ebx, 021Bh ; non masked window MIX op
	mov	bl, 1Bh
	sys	_video

	call	waitforkey

	; MIX pixel colors of
	; the window/block on row 160, column 260 with 
	; block size 120(w)*160(h).
	;mov	ecx, 102040h
	mov	edx, (160*65536)+260
	;mov	esi, (160*65536)+120 
	;mov	ebx, 021Bh ; non masked window MIX op
	sys	_video

	call	waitforkey

	; MIX pixel colors of
	; the window/block on row 250, column 400 with 
	; block size 120(w)*160(h).
	;mov	ecx, 102040h
	mov	edx, (250*65536)+400
	;mov	esi, (160*65536)+120 
	;mov	ebx, 021Bh ; non masked window MIX op
	sys	_video

	call	waitforkey

	; CHANGE color (full screen)
	xor	ecx, ecx ; 0 ; black
	mov	ebx, 0201h ; Full screen, new color
	sys	_video

	mov	ecx, 65536
blackloop:
	or	ecx, ecx
	nop
	nop
	nop
 	loop	blackloop

	mov	ecx, 0FFFFFFh ; white
	mov	ebx, 0201h ; Full screen, new color
	sys	_video

	;mov	dword [tcolor], 0
	mov	esi, 208*65536+240
	mov	ebp, txt_white
 	call	print_text

	call	waitforkey

	; full screen sub
	mov	ecx, 0FF0000h
	mov	bl, 03h
	sys	_video	

	call	waitforkey

	; full screen add
	;mov	ecx, 0FF0000h
	mov	bl, 02h
	sys	_video

	call	waitforkey

	mov	ecx, 0FFh ; blue
	mov	ebx, 0201h ; Full screen, new color
	sys	_video

	mov	dword [tcolor], 0FFFFFFh
	mov	esi, 208*65536+258
	mov	ebp, txt_blue
 	call	print_text

	call	waitforkey

	; full screen add
	mov	ecx, 0FF0000h
	mov	bl, 02h
	sys	_video

	call	waitforkey
	
	; full screen sub
	;mov	ecx, 0FF0000h
	mov	bl, 03h
	sys	_video

	call	waitforkey

	;mov	ecx, 0FF0000h ; red
	mov	ebx, 0201h ; Full screen, new color
	sys	_video

	;mov	dword [tcolor], 0FFFFFFh
	mov	esi, 208*65536+276
	mov	ebp, txt_red
 	call	print_text

	call	waitforkey

	; full screen add
	mov	ecx, 0040FFh
	mov	bl, 02h
	sys	_video

	call	waitforkey
	
	; full screen sub
	;mov	ecx, 0040FFh
	mov	bl, 03h
	sys	_video

	call	waitforkey

	; full screen OR
	mov	ecx, 00FF00h
	mov	bl, 04h
	sys	_video

	call	waitforkey

	; full screen AND
	;mov	ecx, 001F00h
	mov	ch, 1Fh
	mov	bl, 05h
	sys	_video

	call	waitforkey
	
	; full screen XOR
	mov	ecx, 073F7Fh
	mov	bl, 06h
	sys	_video	
	
	call	waitforkey

	mov	ecx, 00FF00h ; green 
	mov	ebx, 0201h ; Full screen, new color
	sys	_video

	mov	dword [tcolor], 0
	mov	esi, 208*65536+240
	mov	ebp, txt_green
 	call	print_text

	call	waitforkey

	; full screen MIX
	mov	ecx, 3F007Fh ; green
	mov	bl, 0Bh
	sys	_video

	call	waitforkey

	mov	ecx, 0FFFF00h ; yellow
	mov	ebx, 0201h ; Full screen, new color
	sys	_video

	mov	dword [tcolor], 0FFFFFFh
	mov	esi, 208*65536+222
	mov	ebp, txt_yellow
 	call	print_text

	call	waitforkey

	; full screen NOT
	mov	bl, 07h
	sys	_video

	call	waitforkey

	; full screen NOT
	;mov	bl, 07h
	sys	_video

	call	waitforkey

	; full screen MIX
	mov	ecx, 7F00FFh
	mov	bl, 0Bh
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
	;xor	eax, eax
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

	;mov	byte [tcolor], 0FFFFFFh

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

	; Replace white color (text) only in blue block
	; (blue block starts at row 20)
	
	mov	ecx, 0FFFFFFh ; WHITE (current color)
	sub	edx, edx ; 0  ; BLACK (new color)
	mov	esi, 20*65536+40 ; column 40, row 20
	mov	edi, 110*65536+160 ; size: 110*160 
	mov	ebx, 021Ch ; Replace color in window
	sys	_video

	call	waitforkey

	; Replace white color (text) only in red block
	; (red block starts at row 130)
	
	;mov	ecx, 0FFFFFFh ; WHITE (current color)
	;sub	edx, edx ; 0  ; BLACK (new color)
	mov	esi, 130*65536+40 ; column 40, row 130
	mov	edi, 110*65536+120 ; size: 110*120 
	;mov	ebx, 021Ch ; Replace color in window
	sys	_video

	call	waitforkey

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
	db "by Erdogan Tan - 28/02/2021"
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
whiteblock:
	resb 120*160*4	
blueblock:
	resb 120*160*4
redblock:
	resb 120*160*4
greenblock:
	resb 120*160*4
yellowblock:
	resb 120*160*4

	resb (307200*4)-(96000*4)	
bss_end: