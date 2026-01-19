; ****************************************************************************
; svgabox.s - TRDOS 386 (TRDOS v2.0.3) Test Program - LFB data transfers
; ----------------------------------------------------------------------------
;
; 25/01/2021
;
; ****************************************************************************
; nasm svgabox.s -l svgabox.txt -o SVGABOX.PRG -Z error.txt

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

; 25/01/2021 (TRDOS 386 v2.0.3)

	; DIRECT LINEAR FRAME BUFFER ACCESS
 	;xor    ebx, ebx
 	;mov	bh, 6 ; Direct access/map to LFB address
 	;mov	bl, 5 ; bl = 05h -> 105h, VESA MODE 105h
	mov	bx, 605h  ; Direct access/map to LFB for VBE video mode 105h
	;mov    eax, _video ; 1Fh
 	mov     eax, 1Fh ; sys _video ; TRDOS 386 Video functions
 	int     40h   ; TRDOS 386 system call
	or	eax, eax
	jz	short lfb_error
	mov	[LFB_addr], eax

	shr	eax, 16 ; ax = high word of LFB address	
	call	wordtohex
	mov	[lfb_addr_str], eax

	mov	esi, msg_lfb_ok
	call	print_msg	

	xor	ah, ah
	;int	16h	; KEYBOARD - READ CHAR FROM BUFFER, WAIT IF EMPTY
			; Return: AH = scan code, AL = character
	int	32h	; TRDOS 386 Keyboard interrupt 

	;xor	ecx, ecx			
	
set_vesa_vbe_mode:
	mov    	ax, 4F02h ; vbe function 02h, set video mode
	;int    10h	  ; bios video interrupt
	mov	bx, 4101h ; vbe mode 101h with LFB (640x480, 256 colors)
	int     31h ; TRDOS 386 - Video interrupt
	cmp	ax, 004Fh
	je	short vbe_mode_set_ok
vbe_error:
	mov	esi, msg_vbe_error
	jmp	short error
lfb_error:
	mov	esi, msg_lfb_error
	jmp	short error
error:
	call	print_msg
_terminate:
	sys	_exit
_hang:
	jmp	short _hang

vbe_mode_set_ok:
	; create a box and the center of 640x480 screen 
reset:	
	mov	bl, 128  ; color
	mov	[boxcolor], bl
	mov	[color], bl

	mov	cl, 64
	mov	[boxsize], cl 

	; BL = Box (Square) Color
	; CL = Box (Square) Size
	call	colorbox

	; move colorbox to center of screen
center:
	call	movecenter
_0:
	call	drawbox

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
	je	terminate
	cmp	al, 1Bh ; ESC key
	je	terminate	

	cmp	al, '+'
	jne	short _2

	; black box (clear box)
	cmp	byte [boxsize], 120
	ja	short waitforkey
	mov	byte [color], 0
	call	drawbox
	; increase box size
	add	byte [boxsize], 8
_1:
	mov	al, [boxcolor]
	mov	[color], al
	jmp	short _0
_2:
	cmp	al, '-'
	jne	short _3

	; black box (clear box)
	cmp	byte [boxsize], 72
	jb	short _3
	mov	byte [color], 0
	call	drawbox
	; decrease box size
	sub	byte [boxsize], 8
	jmp	short _1
_3:
	cmp	al, 20h  ; space
	jne	short _4
	add	byte [boxcolor], 1
	jmp	short _1
_4:
	cmp	ah, 4Bh
	jne	short _5

	; left arrow
	cmp	word [x_pos], 4
	jna	waitforkey

	mov	byte [color], 0
	call	drawbox
	
	sub	word [x_pos], 4
	jmp	short _1
_5:
	cmp	ah, 4Dh
	jne	short _6

	; right arrow
	mov	ecx, [x_pos]
	add	ecx, [boxsize]
	cmp	ecx, 636
	ja	waitforkey

	mov	byte [color], 0
	call	drawbox
	
	add	word [x_pos], 4
	jmp	_1
_6:
	cmp	ah, 50h
	jne	short _7

	; down arrow
	mov	ecx, [y_pos]
	add	ecx, [boxsize]
	cmp	ecx, 476
	ja	waitforkey

	mov	byte [color], 0
	call	drawbox
	
	add	word [y_pos], 4
	jmp	_1
_7:
	cmp	ah, 48h
	jne	short _8

	; up arrow
	cmp	word [y_pos], 4
	jna	waitforkey

	mov	byte [color], 0
	call	drawbox
	
	sub	word [y_pos], 4
	jmp	_1
_8:
	cmp	ah, 47h ; Home key
	jne	short _9
	jmp	center
_9:
	cmp	ah, 4Fh ; End key
	jne	short _10
	jmp	reset
_10:	
	cmp	ax, 1C0Dh
	jne	waitforkey
	add	byte [boxcolor], 16
	jmp	_1	

terminate:
	call	set_text_mode
	sys	_exit
halt:
	jmp	short halt
	
next_line:
	; next line
	mov	esi, nextline
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

colorbox:
	; BL = color
	; CL = box size in pixels
	
	; Modified regs: eax, ecx, edi
	
	movzx	eax, cl
	mul	cl
	mov	ecx, eax
	mov	edi, boxbuff
	mov	al, bl
	rep	stosb
	retn	
	
drawbox:
	; INPUT:
	;	[LFB_addr]
	;	[x_pos]
	;	[y_pos]
	;	[boxsize]
	;	[color]
	;
	; Modified registers: edi, eax, ecx, ebx, edx	

	mov	edi, [LFB_addr]
	mov	eax, 640
	mul	dword [y_pos]
	add	eax, [x_pos]
	mov	ecx, [boxsize]
	add	edi, eax
	mov	ebx, ecx  ; height = width	
_dbox:
	push	ecx
	push	edi
	mov	ecx, ebx ; width
	mov	al, [color]
	rep	stosb
	pop	edi
	add	edi, 640
	pop	ecx 	
	loop	_dbox 	 			
	retn

movecenter:
	; Modified registers: ecx	

	mov	ecx, 640
	sub	cx, [boxsize]
	shr	cx, 1
	mov	[x_pos], cx
	mov	cx, 480
	sub	cx, [boxsize]
	shr	cx, 1
	mov	[y_pos], cx
	retn	

wordtohex:
	; INPUT ->
	; 	ax = word (binary number)
	; OUTPUT ->
	;	eax = hexadecimal string
	;
	push	ebx
	xor	ebx, ebx
	xchg	ah, al
	push	ax
	mov	bl, ah
	shr	bl, 4
	mov	al, [ebx+hexchrs] 	 	
	mov	bl, ah
	and	bl, 0Fh
	mov	ah, [ebx+hexchrs]
	shl	eax, 16
	pop	ax
	mov	bl, al
	shr	bl, 4
	mov	bl, [ebx+hexchrs] 	 	
	xchg	bl, al	 	
	and	bl, 0Fh
	mov	ah, [ebx+hexchrs] 
	pop	ebx	
	retn

set_text_mode:
	xor    ah, ah
	mov    al, 3                        
 	;int   10h	; al = 03h text mode, int 10 video
	int    31h ; TRDOS 386 - Video interrupt
	retn
		
program_msg:
	db "TRDOS 386 v2.0.3 - Linear Frame Buffer ('sysvideo') Test Program"
	db 0Dh, 0Ah
	db "by Erdogan Tan - 25/01/2021"
	db 0Dh, 0Ah, 0

press_any_key_msg:
	db "Use Arrow Keys, Page Up, Page Down, Home, End to move the BOX .."
	db 0Dh, 0Ah
	db "Use +,- to increase and decrease BOX size .."		
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db "Press any key to continue .."
nextline:
	db 0Dh, 0Ah, 0	

msg_vbe_error:
	db 0Dh, 0Ah
	db 'vesa vbe video mode error !'
	db 0Dh, 0Ah, 0		
msg_lfb_error:
	db 0Dh, 0Ah
	db 'linear frame buffer error !'
	db 0Dh, 0Ah, 0

msg_lfb_ok:
	db 0Dh, 0Ah
	db 'linear frame buffer ready .. (at address '
lfb_addr_str: ; 8 (hex) digits
	db '00000000h)'
	db 0Dh, 0Ah
	db 'press a key to continue ..'	
	db 0Dh, 0Ah, 0

hexchrs:
	db '0123456789ABCDEF'

bss:

ABSOLUTE bss

alignb 4

bss_start:

LFB_addr:
	resd 1	

y_pos:	resd 1

x_pos:	resd 1

boxsize:
	resd 1
boxcolor:
	resb 1
color:	resb 1
	resw 1

counter:
	resd 1

boxbuff:
	resb 128*128

bss_end: