; ****************************************************************************
; loadpcx.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'loadpcx.prg')
; ----------------------------------------------------------------------------
; LOADPCX.PRG ! 'sysopen' and 'sysread' and VGA TEST program for TRDOS 386 !
;
; 14/10/2016
;
; [ Last Modification: 14/10/2016 ]
;
; Derived from: MICROPCX.ASM (1994, Lord Debugger. Poznan, Poland.)
;
; Note: This program displays 320x200x256 pcx files only.
; ****************************************************************************
;
; 29/04/2016
; TRDOS 386 system calls (temporary list!)
_ver 	equ 0
_exit 	equ 1
_fork 	equ 2
_read 	equ 3
_write	equ 4
_open	equ 5
_close 	equ 6
_wait 	equ 7
_creat 	equ 8
_link 	equ 9
_unlink	equ 10
_exec	equ 11
_chdir	equ 12
_time 	equ 13
_mkdir 	equ 14
_chmod	equ 15
_chown	equ 16
_break	equ 17
_stat	equ 18
_seek	equ 19
_tell 	equ 20
_mount	equ 21
_umount	equ 22
_setuid	equ 23
_getuid	equ 24
_stime	equ 25
_quit	equ 26	
_intr	equ 27
_fstat	equ 28
_emt 	equ 29
_mdate 	equ 30
_stty 	equ 31
_gtty	equ 32
_ilgins	equ 33
_sleep	equ 34
_msg    equ 35

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

[BITS 32] ; We need 32-bit intructions for protected mode

[ORG 0] 

START_CODE:
	mov	esi, esp
	lodsd
	cmp	eax, 2 ; two arguments (program file name & text file name)
	jb	terminate ; nothing top do
	lodsd ; program file name address 
	lodsd ; text file name address
	push	eax ; arg2 ; file name

	sys	_open, eax, 0 ; open for reading
	jc	open_error

	mov	[fhandle], eax ; file handle/index number

	; DIRECT VGA MEMORY ACCESS
 	;xor    ebx, ebx
 	mov     bh, 5 ; Direct access/map to VGA memory (0A0000h)
 	;mov    eax, _video ; 1Fh
 	mov     al, 1Fh ; sys _video ; TRDOS 386 Video functions
 	int     40h   ; TRDOS 386 system call

	sys	_read, [fhandle], BUFFER, 0FFFFFFFFh ; read all bytes of the file 
 	jc	read_error  ; disk read or memory allocation error

; print file size (debug)
;	push	eax
;	mov	ecx, 10
;	mov	ebp, esp
;	sub	esp, ecx
;	dec	ebp
;	mov	[ebp], ch ; 0
;dec_digit:
;	xor	edx, edx
;	div 	ecx
;	dec	ebp
;	add	dl, '0'
;	mov	[ebp], dl
;	and	eax, eax
;	jnz	short dec_digit
;	dec	ebp
;	mov	byte [ebp], 0Ah
;	dec	ebp
;	mov	byte [ebp], 0Dh
;	mov	esi, ebp
;	call	print_msg
;	add	esp, ecx
;	;xor	ah, ah
;	;int	32h
;	pop	eax
		
	xor	ecx, ecx			
	mov	edi, eax ; file size (read bytes)
	
set_vga_mode:
	mov    	ax, 13h                  ; function 00h, mode 13h 
	;int    10h                      ; bios video interrupt
	int     31h ; TRDOS 386 - Video interrupt

	add	edi, BUFFER+3
	and	di, 0FFFCh ; dword alignment
	mov	ebp, edi
	mov	edx, ebp
	add	edx, 64001
	mov	esi, BUFFER + 128 ; PCX header + data
ldlp1:
	lodsb		
	mov	ah, al
	and	ah, 0C0h
	cmp	ah, 0C0h
	jne	short single
	and	al, 3Fh
	mov	cl, al
	lodsb
ldlp2:  rep stosb
	jmp	short cnt
single:
	stosb
cnt:
	cmp	edi, edx ; 64001 ; end of pixel data
	jb	short ldlp1

	mov	edx, esi	
	mov	edi, esi
	mov	cx, 768
set256:
	lodsb
	shr	al, 2
	stosb
	loop    set256
	mov	ax, 1012h 	; set palette registers
	xor	ebx, ebx
	mov	cx, 256
	;int	10h
	int	31h		; TRDOS 386 Video interrupt

; move_image_to_VGA_address:
	mov	esi, ebp
	mov	cx, 16000
	mov	edi, 0A0000h
	rep	movsd	

	xor	ah, ah
	;int	16h		; KEYBOARD - READ CHAR FROM BUFFER, WAIT IF EMPTY
				; Return: AH = scan code, AL = character
	int	32h		; TRDOS 386 Keyboard interrupt 

	call	set_text_mode
cf_terminate:
	sys	_close, [fhandle] ; close file

terminate:
	sys 	_exit			   ; INT 40h
here:
	jmp	short here

open_error:
	push	eax
	mov	esi, msg_open_error
	call	print_msg		   ; INT 31h
	pop	eax
	cmp	eax, 2
	jne	short terminate
	mov	esi, msg_not_found
	call	print_msg		   ; INT 31h
	jmp	short terminate	

read_error:
	call	set_text_mode
	mov	esi, msg_read_error
	call	print_msg		   ; INT 31h
	jmp	short cf_terminate

print_msg:
	mov	ebx, 0Eh       ; yellow characters (bl)
		               ; video page 0 (bh)
	;mov	ah, 0Eh ; teletype output (write tty)
	mov	ah, bl
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

fhandle: dd 0


;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

msg_program:
	db 0Dh, 0Ah
	db "LOADPCX.PRG /// TRDOS 386 sysopen, sysread test program"
	db 0Dh, 0Ah
	db "by Erdogan Tan, 14/10/2016", 0Dh, 0Ah, 0
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
;bss

BUFFER: