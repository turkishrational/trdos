; ****************************************************************************
; blocks6.s - TRDOS 386 (TRDOS v2.0.3) Test Program - 'sysvideo' pixel tests
; ----------------------------------------------------------------------------
;
; 28/02/2021 (27/02/2021)
;
; ****************************************************************************
; nasm blocks3.s -l blocks3.txt -o BLOCKS3.PRG -Z error.txt
; (modified from 'blocks3.s', 22/02/2021)

; 'sysvideo' bh = 1, block copy and modification test (VGA Mode 13h)

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
	; Set initial block colors
	mov	ecx, (60*80)/4
	mov	eax, 0F0F0F0Fh ; white 
	mov	edi, whiteblock
	rep	stosd
	mov	cx, 60*20
	mov	eax, 20202020h ; blue
	mov	edi, blueblock
	rep	stosd
	mov	cx, 60*20
	mov	eax, 28282828h ; red
	mov	edi, redblock
	rep	stosd
	mov	cx, 60*20
	mov	eax, 30303030h ; green 
	mov	edi, greenblock
	rep	stosd
	mov	cx, 60*20
	mov	eax, 2C2C2C2Ch ; yellow
	mov	edi, yellowblock
	rep	stosd

	; copy white block to screen
	; at row 15, column 60
	; (block size: 60(w)*80(h) 

	mov	ecx, (15*65536)+60
	mov	edx, (80*65536)+60 
	mov	esi, whiteblock
	mov	ebx, 0110h ; non masked window copy
	sys	_video

	; copy blue block to screen
	; at row 55, column 130
	; (block size: 60(w)*80(h) 

	mov	ecx, (55*65536)+130
	;mov	edx, (80*65536)+60 
	mov	esi, blueblock
	;mov	ebx, 0110h ; non masked window copy
	sys	_video

	; copy red block to screen
	; at row 105, column 60
	; (block size: 60(w)*80(h) 

	mov	ecx, (105*65536)+60
	;mov	edx, (80*65536)+60
	mov	esi, redblock
	;mov	ebx, 0110h ; non masked window copy
	sys	_video

	; copy green block to screen
	; at row 15, column 200
	; (block size: 60(w)*80(h) 

	mov	ecx, (15*65536)+200
	;mov	edx, (80*65536)+60 
	mov	esi, greenblock
	;mov	ebx, 0110h ; non masked window copy
	sys	_video

	; copy yellow block to screen
	; at row 105, column 200
	; (block size: 60(w)*80(h) 

	mov	ecx, (105*65536)+200
	;mov	edx, (80*65536)+60
	mov	esi, yellowblock
	;mov	ebx, 0110h ; non masked window copy
	sys	_video

	call	waitforkey

	; continue by using
	; window color modification sub functions

	; apply ADD to pixel colors of
	; the window/block on row 15, column 60 with 
	; block size 60(w)*80(h).
	mov	cl, 32   
	mov	edx, (15*65536)+60
	mov	esi, (80*65536)+60
	;mov	ebx, 0112h ; non masked window ADD op
	mov	bl, 12h
	sys	_video

	call	waitforkey

	; apply SUB to window's pixel colors
	;mov	cl, 32   
	;mov	edx, (15*65536)+60
	;mov	esi, (80*65536)+60 
	;mov	ebx, 0113h ; non masked window SUB op
	mov	bl, 13h
	sys	_video

	call	waitforkey

	; apply ADD to pixel colors of
	; the window/block on row 55, column 130 with 
	; block size 60(w)*80(h).
	;mov	cl, 32   
	mov	edx, (55*65536)+130
	;mov	esi, (80*65536)+60
	;mov	ebx, 0112h ; non masked window ADD op
	mov	bl, 12h
	sys	_video

	call	waitforkey

	; apply SUB to window's pixel colors
	;mov	cl, 32   
	;mov	edx, (55*65536)+130
	;mov	esi, (80*65536)+60
	;mov	ebx, 0113h ; non masked window SUB op
	mov	bl, 13h
	sys	_video

	call	waitforkey

	; apply ADD to pixel colors of
	; the window/block on row 105, column 60 with 
	; block size 60(w)*80(h).
	;mov	cl, 32   
	mov	edx, (105*65536)+60
	;mov	esi, (80*65536)+60
	;mov	ebx, 0112h ; non masked window ADD op
	mov	bl, 12h
	sys	_video

	call	waitforkey

	; apply SUB to window's pixel colors
	;mov	cl, 32   
	;mov	edx, (105*65536)+60
	;mov	esi, (80*65536)+60
	;mov	ebx, 0113h ; non masked window SUB op
	mov	bl, 13h
	sys	_video

	call	waitforkey

	; apply ADD to pixel colors of
	; the window/block on row 15, column 200 with 
	; block size 60(w)*80(h).
	;mov	cl, 32   
	mov	edx, (15*65536)+200
	;mov	esi, (80*65536)+60
	;mov	ebx, 0112h ; non masked window ADD op
	mov	bl, 12h
	sys	_video

	call	waitforkey

	; apply SUB to window's pixel colors
	;mov	cl, 32   
	;mov	edx, (15*65536)+200
	;mov	esi, (80*65536)+60
	;mov	ebx, 0113h ; non masked window SUB op
	mov	bl, 13h
	sys	_video

	call	waitforkey

	; apply ADD to pixel colors of
	; the window/block on row 105, column 200 with 
	; block size 60(w)*80(h).
	;mov	cl, 32   
	mov	edx, (105*65536)+200
	;mov	esi, (80*65536)+60
	;mov	ebx, 0112h ; non masked window ADD op
	mov	bl, 12h
	sys	_video

	call	waitforkey

	; apply SUB to window's pixel colors
	;mov	cl, 32   
	;mov	edx, (105*65536)+200
	;mov	esi, (80*65536)+60
	;mov	ebx, 0113h ; non masked window SUB op
	mov	bl, 13h
	sys	_video

	call	waitforkey

	; or, and, xor

	; apply AND to window's pixel colors
	mov	cl, 15
	mov	edx, (15*65536)+60
	;mov	esi, (80*65536)+60
	;mov	ebx, 0115h ; non masked window AND op
	mov	bl, 15h
	sys	_video

	call	waitforkey

	; apply AND to pixel colors of
	; the window/block on row 55, column 130 with 
	; block size 60(w)*80(h).
	;mov	cl, 15
	mov	edx, (55*65536)+130
	;mov	esi, (80*65536)+60
	;;mov	ebx, 0115h ; non masked window AND op
	;mov	bl, 15h
	sys	_video

	call	waitforkey

	; apply AND to pixel colors of
	; the window/block on row 105, column 60 with 
	; block size 60(w)*80(h).
	;mov	cl, 15
	mov	edx, (105*65536)+60
	;mov	esi, (80*65536)+60
	;;mov	ebx, 0115h ; non masked window AND op
	;mov	bl, 15h
	sys	_video

	call	waitforkey

	; apply AND to pixel colors of
	; the window/block on row 15, column 200 with 
	; block size 60(w)*80(h).
	;mov	cl, 15
	mov	edx, (15*65536)+200
	;mov	esi, (80*65536)+60
	;;mov	ebx, 0115h ; non masked window AND op
	;mov	bl, 15h
	sys	_video

	call	waitforkey

	; apply AND to pixel colors of
	; the window/block on row 105, column 200 with 
	; block size 60(w)*80(h).
	;mov	cl, 15
	mov	edx, (105*65536)+200
	;mov	esi, (80*65536)+60
	;;mov	ebx, 0115h ; non masked window AND op
	;mov	bl, 15h
	sys	_video

	call	waitforkey

	; apply OR to pixel colors of
	; the window/block on row 15, column 60 with 
	; block size 60(w)*80(h).
	mov	cl, 32
	mov	edx, (15*65536)+60
	;mov	esi, (80*65536)+60
	;mov	ebx, 0114h ; non masked window OR op
	mov	bl, 14h
	sys	_video

	call	waitforkey

	; apply OR to pixel colors of
	; the window/block on row 55, column 130 with 
	; block size 60(w)*80(h).
	;mov	cl, 32   
	mov	edx, (55*65536)+130
	;mov	esi, (80*65536)+60
	;mov	ebx, 0114h ; non masked window OR op
	mov	bl, 14h
	sys	_video

	call	waitforkey

	; apply OR to pixel colors of
	; the window/block on row 105, column 60 with 
	; block size 60(w)*80(h).
	;mov	cl, 32   
	mov	edx, (105*65536)+60
	;mov	esi, (80*65536)+60
	;mov	ebx, 0114h ; non masked window OR op
	mov	bl, 14h
	sys	_video

	call	waitforkey

	; apply OR to pixel colors of
	; the window/block on row 15, column 200 with 
	; block size 60(w)*80(h).
	;mov	cl, 32   
	mov	edx, (15*65536)+200
	;mov	esi, (80*65536)+60
	;mov	ebx, 0114h ; non masked window OR op
	mov	bl, 14h
	sys	_video

	call	waitforkey

	; apply OR to pixel colors of
	; the window/block on row 105, column 200 with 
	; block size 60(w)*80(h).
	;mov	cl, 32   
	mov	edx, (105*65536)+200
	;mov	esi, (80*65536)+60
	;mov	ebx, 0114h ; non masked window OR op
	mov	bl, 14h
	sys	_video

	call	waitforkey

	; apply XOR to pixel colors of
	; the window/block on row 15, column 60 with 
	; block size 60(w)*80(h).
	mov	cl, 63
	mov	edx, (15*65536)+60
	;mov	esi, (80*65536)+60
	;mov	ebx, 0116h ; non masked window XOR op
	mov	bl, 16h
	sys	_video

	call	waitforkey

	; apply XOR to pixel colors of
	; the window/block on row 55, column 130 with 
	; block size 60(w)*80(h).
	;mov	cl, 63 
	mov	edx, (55*65536)+130
	;mov	esi, (80*65536)+60
	;;mov	ebx, 0116h ; non masked window XOR op
	;mov	bl, 16h
	sys	_video

	call	waitforkey

	; apply XOR to pixel colors of
	; the window/block on row 105, column 60 with 
	; block size 60(w)*80(h).
	;mov	cl, 63
	mov	edx, (105*65536)+60
	;mov	esi, (80*65536)+60
	;;mov	ebx, 0116h ; non masked window XOR op
	;mov	bl, 16h
	sys	_video

	call	waitforkey

	; apply XOR to pixel colors of
	; the window/block on row 15, column 200 with 
	; block size 60(w)*80(h).
	;mov	cl, 63
	mov	edx, (15*65536)+200
	;mov	esi, (80*65536)+60
	;;mov	ebx, 0116h ; non masked window XOR op
	;mov	bl, 16h
	sys	_video

	call	waitforkey

	; apply XOR to pixel colors of
	; the window/block on row 105, column 200 with 
	; block size 60(w)*80(h).
	;mov	cl, 63
	mov	edx, (105*65536)+200
	;mov	esi, (80*65536)+60
	;;mov	ebx, 0116h ; non masked window XOR op
	;mov	bl, 16h
	sys	_video

	call	waitforkey
	
	; Mix colors

	; MIX pixel colors of
	; the window/block on row 15, column 60 with 
	; block size 60(w)*80(h).
	mov	cl, 16
	mov	edx, (15*65536)+60
	;mov	esi, (80*65536)+60
	;mov	ebx, 011Bh ; non masked window MIX op
	mov	bl, 1Bh
	sys	_video

	call	waitforkey

	; MIX pixel colors of
	; the window/block on row 55, column 130 with 
	; block size 60(w)*80(h).
	;mov	cl, 16
	mov	edx, (55*65536)+130
	;mov	esi, (80*65536)+60
	;;mov	ebx, 011Bh ; non masked window MIX op
	;mov	bl, 1Bh
	sys	_video

	call	waitforkey

	; MIX pixel colors of
	; the window/block on row 105, column 60 with 
	; block size 60(w)*80(h).
	;mov	cl, 16
	mov	edx, (105*65536)+60
	;mov	esi, (80*65536)+60
	;;mov	ebx, 011Bh ; non masked window MIX op
	;mov	bl, 1Bh
	sys	_video

	call	waitforkey

	; MIX pixel colors of
	; the window/block on row 15, column 200 with 
	; block size 60(w)*80(h).
	;mov	cl, 16  
	mov	edx, (15*65536)+200
	;mov	esi, (80*65536)+60
	;;mov	ebx, 011Bh ; non masked window MIX op
	;mov	bl, 1Bh
	sys	_video

	call	waitforkey

	; MIX pixel colors of
	; the window/block on row 105, column 200 with 
	; block size 60(w)*80(h).
	;mov	cl, 16  
	mov	edx, (105*65536)+200
	;mov	esi, (80*65536)+60
	;;mov	ebx, 011Bh ; non masked window MIX op
	;mov	bl, 1Bh
	sys	_video

	call	waitforkey

	; CHANGE color (full screen)
	xor	ecx, ecx ; 0 ; black
	mov	ebx, 0101h ; Full screen, new color
	mov	ecx, 65536
blackloop:
	or	ecx, ecx
	nop
	nop
	nop
 	loop	blackloop

	mov	ecx, 0F0F0F0Fh ; white
	mov	ebx, 0101h ; Full screen, new color
	sys	_video

	;mov	byte [tcolor], 0
	mov	esi, 68*65536+72
	mov	ebp, txt_white
 	call	print_text

	call	waitforkey

	; full screen add
	mov	cl, 28h
	mov	bl, 02h
	sys	_video

	call	waitforkey
	
	; full screen sub
	mov	cl, 28h
	mov	bl, 03h
	sys	_video	

	call	waitforkey

	mov	ecx, 20202020h ; blue
	mov	ebx, 0101h ; Full screen, new color
	sys	_video

	mov	byte [tcolor], 0Fh
	mov	esi, 68*65536+90
	mov	ebp, txt_blue
 	call	print_text

	call	waitforkey

	; full screen add
	mov	cl, 28h
	mov	bl, 02h
	sys	_video

	call	waitforkey
	
	; full screen sub
	mov	cl, 28h
	mov	bl, 03h
	sys	_video	

	call	waitforkey

	mov	ecx, 28282828h ; red
	mov	ebx, 0101h ; Full screen, new color
	sys	_video

	;mov	byte [tcolor], 0Fh
	mov	esi, 68*65536+108
	mov	ebp, txt_red
 	call	print_text

	call	waitforkey

	; full screen add
	mov	cl, 28h
	mov	bl, 02h
	sys	_video

	call	waitforkey
	
	; full screen sub
	mov	cl, 28h
	mov	bl, 03h
	sys	_video

	call	waitforkey

	; full screen OR
	mov	cl, 0Fh
	mov	bl, 04h
	sys	_video

	call	waitforkey

	; full screen AND
	mov	cl, 1Fh
	mov	bl, 05h
	sys	_video

	call	waitforkey
	
	; full screen XOR
	mov	cl, 1Fh
	mov	bl, 06h
	sys	_video	
	
	call	waitforkey

	mov	ecx, 30303030h ; green 
	mov	ebx, 0101h ; Full screen, new color
	sys	_video

	mov	byte [tcolor], 0
	mov	esi, 68*65536+72
	mov	ebp, txt_green
 	call	print_text

	call	waitforkey

	; full screen MIX
	mov	cl, 24h
	mov	bl, 0Bh
	sys	_video

	call	waitforkey

	mov	ecx, 2C2C2C2Ch ; yellow
	mov	ebx, 0101h ; Full screen, new color
	sys	_video

	mov	byte [tcolor], 0Fh
	mov	esi, 68*65536+54
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
	mov	cl, 10h
	mov	bl, 0Bh
	sys	_video

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

	;mov	byte [tcolor], 0Fh

	mov	esi, 23*65536+23
	mov	ebp, txt_blue
 	call	print_text
	
	call	waitforkey

	mov	esi, 113*65536+23
	mov	ebp, txt_red
 	call	print_text
	
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
	sys	_video, 010Fh, [tcolor] 
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
	resb 60*80	
blueblock:
	resb 60*80
redblock:
	resb 60*80
greenblock:
	resb 60*80
yellowblock:
	resb 60*80

	resb 64000-24000	
bss_end: