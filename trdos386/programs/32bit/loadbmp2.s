; ****************************************************************************
; loadbmp2.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'loadbmp2.prg')
; ----------------------------------------------------------------------------
; LOADBMP.PRG ! VESA VBE (bitmap image display) TEST program for TRDOS 386 !
;
; 19/12/2020
;
; [ Last Modification: 20/12/2020 ]
;
; derived from (bmp loading) asm source code (for msdos) by Glen Quinn, 1998
; modified from 'loadpcx.s' (by Erdogan Tan), 14/10/2016
;
; Assembler: NASM 2.15 
;   command: nasm loadbmp2.s -l loadbmp2.txt -o LOADBMP2.PRG -Z error.txt
;
; Note: This program displays 1024x768 8bpp non-compressed BMP files only.
; ****************************************************************************
; Modified from loadbmp1.s (640x480, 8bpp) source code 

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

; TRDOS 386 (and Retro UNIX 386 v1) system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

struc BMPHEAD	; This is the bitmap structure
   .id:		resw 1
   .filesize:	resd 1
   .reserved:	resd 1
   .headersize:	resd 1
   .infosize:	resd 1
   .width:	resd 1
   .depth:	resd 1
   .biplanes:	resw 1
   .bits:	resw 1
   .bicomp:	resd 1
   .bisizeim:	resd 1
   .bixpels:	resd 1
   .biypels:	resd 1
   .biclrused:	resd 1
   .biclrimp:	resd 1
 .size:	; 54 bytes
endstruc

struc RGBQUAD	; This is how the bitmap stores its colours
   .blue:	resb 1
   .green:	resb 1
   .red:	resb 1
   .fill:	resb 1
endstruc

;BMP equ BUFFER ; buffer which bmp file will be loaded

[BITS 32] ; 32-bit protected mode intructions for 80386 (compatible) cpu

[ORG 0] 

START_CODE:
	mov	esi, esp
	lodsd
	cmp	eax, 2 ; two arguments (program file name & bmp file name)
	;jb	terminate ; nothing top do
	jb	_terminate ; write program name, usage & exit
	lodsd ; program file name address 
	lodsd ; bmp file name address
	; 20/12/2020
	;push	eax ; arg2 ; file name

	sys	_open, eax, 0 ; open for reading
	jc	open_error

	mov	[fhandle], eax ; file handle/index number

	sys	_read, [fhandle], BUFFER, 0FFFFFFFFh ; read all bytes of the file 
 	jc	read_error  ; disk read or memory allocation error
	; eax = file size

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

	; 19/12/2020
	shr	eax, 16 ; ax = high word of LFB address	
	call	wordtohex
	mov	[lfb_addr_str], eax

	mov	esi, msg_lfb_ok
	call	print_msg	

	xor	ah, ah
	;int	16h		; KEYBOARD - READ CHAR FROM BUFFER, WAIT IF EMPTY
				; Return: AH = scan code, AL = character
	int	32h		; TRDOS 386 Keyboard interrupt 

	;xor	ecx, ecx			
	
set_vesa_vbe_mode:
	mov    	ax, 4F02h	; vbe function 02h, set video mode
	;int    10h		; bios video interrupt
	mov	bx, 4105h ; vbe mode 105h with LFB 
	int     31h ; TRDOS 386 - Video interrupt
	cmp	ax, 004Fh
	jne	short vbe_error	

	call	loadbmp

	xor	ah, ah
	;int	16h		; KEYBOARD - READ CHAR FROM BUFFER, WAIT IF EMPTY
				; Return: AH = scan code, AL = character
	int	32h		; TRDOS 386 Keyboard interrupt 

	call	set_text_mode
	
	; Write GLEN message
	;mov	esi, msg_glen
	;call	print_msg

cf_terminate:
	sys	_close, [fhandle] ; close file

terminate:
	sys 	_exit			   ; INT 40h
here:
	jmp	short here

_terminate:
	mov	esi, msg_program
	jmp	short write_and_exit

lfb_error:
	mov	esi, msg_lfb_error
	jmp	short err_close_file

vbe_error:
	mov	esi, msg_vbe_error
	jmp	short err_close_file
	
open_error:
	push	eax
	mov	esi, msg_open_error
	call	print_msg		   ; INT 31h
	pop	eax
	cmp	eax, 2
	jne	short terminate
	mov	esi, msg_not_found
write_and_exit:
	call	print_msg		   ; INT 31h
	jmp	short terminate	

read_error:
	call	set_text_mode
	mov	esi, msg_read_error
err_close_file:
	call	print_msg		   ; INT 31h
	jmp	short cf_terminate

print_msg:
	;mov	ebx, 0Eh       ; yellow characters (bl)
	mov	ebx, 0Fh       ; white characters
		               ; video page 0 (bh)
	mov	ah, 0Eh ; teletype output (write tty)
	;mov	ah, bl
	lodsb
_1:
	int	31h
	lodsb
	and	al, al
	jnz	short _1
_2:
	retn

set_text_mode:
	xor    ah, ah
	mov    al, 3                        
 	;int   10h             ; al = 03h text mode, int 10 video
	int    31h ; TRDOS 386 - Video interrupt

	retn

;-----------------------------------------------------------------
; subroutine - loadbmp
;-----------------------------------------------------------------

loadbmp: 	; this procedure is for loading in the bitmap

	; 20/12/2020
	;mov	ebx, [LFB_addr] ; Linear Frame Buffer base addr

	mov	esi, BUFFER+BMPHEAD.size  ; BUFFER+54
 	sub	cl, cl ; 0 ; reading in the palette
G1:
	mov	al, cl
	mov	dx, 3C8h
	;out	dx, al
	mov	ah, 1	
	int	34h
	mov	al, [esi+RGBQUAD.red]
	shr	al, 2
	;mov	dx, 3C9h
	inc	dx
	;out	dx, al
	mov	ah, 1	
	int	34h
	mov	al, [esi+RGBQUAD.green]
	shr	al, 2
	;out	dx, al
	mov	ah, 1	
	int	34h
	;mov	al, [esi+RGBQUAD.blue]
	mov	al, [esi]
	shr	al, 2
	;out	dx, al
	mov	ah, 1	
	int	34h
	add	esi, 4
	inc	cl
	jnz	short G1 ; palette read ends

	mov	word [Y], 767 ; bottom row
	mov	word [X], 0  ; column 0
A1:
 	; writing a single pixel to the display in SVGA

	; start address of pixel array is at header ofset 10
	mov	esi, [BUFFER+BMPHEAD.headersize]
	add	esi, BUFFER
A2:
	call	putpixel

	inc	word [X]
	cmp	word [X], 1024 ; end of row ?
	jne	short A2
	mov	word [X], 0 ; start of row 
	dec	word [Y] ; next/prev row (from bottom to up/top)
	cmp	word [Y], -1 ; row 0 done ?
	jne	short A2  ; no

	retn

;-----------------------------------------------------------------
; subroutine - putpixel
;-----------------------------------------------------------------

putpixel: ; this procedure is for putting a single pixel in LFB

; The linear address of the pixel is y*1024+x
	;mov	eax, 1024
	;mul	dword [Y] ; calculating linear = y*1024
	mov	eax, [Y]
	shl	eax, 10 ; * 1024
	mov	ebx, [LFB_addr]
	add	ebx, eax
	add	ebx, [X]  ; now adding x to the expression
	lodsb
	
	mov	[ebx], al ; writing a single pixel
			  ; to the display	 
	retn


;-----------------------------------------------------------------
; subroutine - wordtohex
;-----------------------------------------------------------------

; Convert binary number to hexadecimal string
; 10/05/2015  
; dsectpm.s (28/02/2015)
; Retro UNIX 386 v1 - Kernel v0.2.0.6  
; 01/12/2014
; 25/11/2014

	; 19/12/2020
wordtohex:
	; INPUT ->
	; 	AX = word (binary number)
	; OUTPUT ->
	;	EAX = hexadecimal string
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
	
;-----------------------------------------------------------------
; data
;-----------------------------------------------------------------

; 19/12/2020
hexchrs:
	db '0123456789ABCDEF'

;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

msg_program:
	db 0Dh, 0Ah
	db "LOADBMP2.PRG /// TRDOS 386 VESA VBE function test program"
	db 0Dh, 0Ah
	db "by Erdogan Tan, 19/12/2020", 0Dh, 0Ah
	db 0Dh, 0Ah, 0Dh, 0Ah
	db "Usage: loadbmp1 <bmp file name>"
	db 0Dh, 0Ah, 0
;msg_glen:
;	db 0Dh, 0Ah
;	db "Programmed by Glen Quinn, BINARY SOFTWARE (1998)"
;	db 0Dh, 0Ah, 0

msg_open_error:
	db 0Dh, 0Ah
	db 'sysopen error !'
	db 0Dh, 0Ah, 0
msg_not_found:
	db 0Dh, 0Ah
	db 'file not found !'
	db 0Dh, 0Ah, 0
msg_read_error:
	db 0Dh, 0Ah
	db 'read error !'
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
	db 'press a key to load bmp image ..'	
	db 0Dh, 0Ah, 0
;bss

bss_start:

ABSOLUTE bss_start

;-----------------------------------------------------------------
;  uninitialized data
;-----------------------------------------------------------------

alignb 4

; fhandle is used to store the file pointer

fhandle: resd 1	

; linear frame buffer address

LFB_addr: resd 1

; X and Y is used to fill the screen with pixels
       
X:	resw 1	
	resw 1	; double word for 32 bit multiplication
Y:	resw 1
	resw 1	; double word for 32 bit multiplication

;alignb 4

BUFFER:
BMP: