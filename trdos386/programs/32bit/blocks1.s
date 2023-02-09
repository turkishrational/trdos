; ****************************************************************************
; blocks1.s - TRDOS 386 (TRDOS v2.0.3) Test Program - 'sysvideo' pixel tests
; ----------------------------------------------------------------------------
;
; 21/02/2021
;
; ****************************************************************************
; nasm blocks1.s -l blocks.txt -o BLOCKS1.PRG -Z error.txt

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
	; Set initial block colors
	mov	ecx, (120*160)/4
	mov	eax, 0F0F0F0Fh ; white 
	mov	edi, whiteblock
	rep	stosd
	mov	cx, 120*40
	mov	eax, 20202020h ; blue
	mov	edi, blueblock
	rep	stosd
	mov	cx, 120*40
	mov	eax, 28282828h ; red
	mov	edi, redblock
	rep	stosd
	mov	cx, 120*40
	mov	eax, 30303030h ; green 
	mov	edi, greenblock
	rep	stosd
	mov	cx, 120*40
	mov	eax, 2C2C2C2Ch ; yellow
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

	call	waitforkey

	; copy blue block to screen
	; at row 160, column 260
	; (block size: 120(w)*160(h) 

	mov	ecx, (160*65536)+260
	mov	edx, (160*65536)+120 
	mov	esi, blueblock
	mov	ebx, 0210h ; non masked window copy
	sys	_video

	call	waitforkey

	; copy red block to screen
	; at row 250, column 120
	; (block size: 120(w)*160(h) 

	mov	ecx, (250*65536)+120
	mov	edx, (160*65536)+120 
	mov	esi, redblock
	mov	ebx, 0210h ; non masked window copy
	sys	_video

	call	waitforkey

	; copy green block to screen
	; at row 70, column 400
	; (block size: 120(w)*160(h) 

	mov	ecx, (70*65536)+400
	mov	edx, (160*65536)+120 
	mov	esi, greenblock
	mov	ebx, 0210h ; non masked window copy
	sys	_video

	call	waitforkey

	; copy yellow block to screen
	; at row 250, column 400
	; (block size: 120(w)*160(h) 

	mov	ecx, (250*65536)+400
	mov	edx, (160*65536)+120 
	mov	esi, yellowblock
	mov	ebx, 0210h ; non masked window copy
	sys	_video
_2:
	call	waitforkey

	; continue by using
	; full screen color modification sub functions

	; NOT pixel colors
	mov	ebx, 0207h
	sys	_video

	call	waitforkey

	; NOT pixel colors
	mov	ebx, 0207h
	sys	_video

	call	waitforkey

	; NEGate pixel colors
	mov	ebx, 0208h
	sys	_video

	call	waitforkey

	; NEGate pixel colors
	mov	ebx, 0208h
	sys	_video

	call	waitforkey
_0:
	; INCrease pixel colors
	mov	ebx, 0209h
	sys	_video

	call	waitforkey
	cmp	al, '+'
	je	short _0
_1:
	; DECrease pixel colors
	mov	ebx, 020Ah
	sys	_video

	call	waitforkey

	cmp	al, '-'
	je	short _1
	cmp	al, '+'
	je	short _0

	; CHANGE color (full screen)
	xor	ecx, ecx ; 0 ; black
	mov	ebx, 0201h ; Full screen, new color
	mov	ecx, 65536
blackloop:
	or	ecx, ecx
	nop
	nop
	nop
 	loop	blackloop

	mov	ecx, 0F0F0F0Fh ; white
	mov	ebx, 0201h ; Full screen, new color
	sys	_video
	call	waitforkey
	mov	ecx, 20202020h ; blue
	mov	ebx, 0201h ; Full screen, new color
	sys	_video
	call	waitforkey
	mov	ecx, 28282828h ; red
	mov	ebx, 0201h ; Full screen, new color
	sys	_video
	call	waitforkey
	mov	ecx, 30303030h ; green 
	mov	ebx, 0201h ; Full screen, new color
	sys	_video
	call	waitforkey
	mov	ecx, 2C2C2C2Ch ; yellow
	mov	ebx, 0201h ; Full screen, new color
	sys	_video

	call	waitforkey

	; Full screen copy
	mov	esi, fullscreen_buffer
	mov	edi, esi
	xor	eax, eax ; black
	mov	ecx, (640*15)/4
	rep	stosd
	mov	eax, 0F0F0F0Fh ; white
	mov	ecx, (640*10)/4	
	rep	stosd
	xor	eax, eax ; black
	mov	ecx, (640*5)/4	
	rep	stosd
	mov	eax, 20202020h ; blue
	mov	ecx, (640*100)/4
	rep	stosd
	xor	eax, eax ; black
	mov	ecx, (640*5)/4	
	rep	stosd
	mov	eax, 28282828h ; red
	mov	ecx, (640*100)/4
	rep	stosd
	xor	eax, eax ; black
	mov	ecx, (640*10)/4	
	rep	stosd
	mov	eax, 30303030h ; green 
	mov	ecx, (640*100)/4
	rep	stosd
	xor	eax, eax ; black
	mov	ecx, (640*5)/4	
	rep	stosd
	mov	eax, 2C2C2C2Ch ; yellow
	mov	ecx, (640*100)/4
	rep	stosd
	xor	eax, eax ; black
	mov	ecx, (640*5)/4	
	rep	stosd
	mov	eax, 0F0F0F0Fh ; white
	mov	ecx, (640*10)/4	
	rep	stosd
	xor	eax, eax ; black
	mov	ecx, (640*15)/4	
	rep	stosd

	mov	ebx, 0200h ; Full screen copy
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

program_msg:
	db "TRDOS 386 v2.0.3 - ('sysvideo') Test Program - Block Operations"
	db 0Dh, 0Ah
	db "by Erdogan Tan - 21/02/2021"
	;db 0Dh, 0Ah, 0
	db 0Dh, 0Ah, 0Dh, 0Ah
	db "Press any key to continue .."
	db 0Dh, 0Ah	
	db "(Press ESC to exit) .."
	db 0Dh, 0Ah
	db 0Dh, 0Ah

nextline:
	db 0Dh, 0Ah
phase:	
	db 0

bss:

ABSOLUTE bss

alignb 4

counter: resd 1	

bss_start:
fullscreen_buffer:
whiteblock:
	resb 120*160	
blueblock:
	resb 120*160
redblock:
	resb 120*160
greenblock:
	resb 120*160
yellowblock:
	resb 120*160

	resb 307200-96000	
bss_end: