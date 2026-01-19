; ****************************************************************************
; writetst.s - TRDOS 386 (TRDOS v2.0) Kernel - SYSWRITE function test
; ----------------------------------------------------------------------------
;
; 24/08/2024
;
; [ Last Modification: 18/09/2024 ]
;
; ****************************************************************************

; 20/08/2024 ; TRDOS 386 v2.0.9 (exit code)
; 20/08/2017
; 01/03/2017
; 16/10/2016
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
_unlink	equ 10 ; _delete
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
_video 	equ 31
_audio	equ 32
_timer	equ 33
_sleep	equ 34
_msg    equ 35
_geterr	equ 36
_fpsave	equ 37
_pri	equ 38
_rele	equ 39
_fff	equ 40
_fnf	equ 41
_alloc	equ 42
_dalloc equ 43
_calbac equ 44
_dma	equ 45
_stdio  equ 46	;  TRDOS 386 v2.0.9

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

;========================================================================
; EXEC TEST - CODE
;========================================================================

[BITS 32] ; 32-bit intructions

[ORG 0] 

START_CODE:
	sys	_msg, program, len0, 0Bh
	sys	_msg, owner, 255, 0Eh

	sys	_msg, sample_text_hdr, 255, 0Ch
	sys	_msg, sample_text, sample_text_size, 0Fh
	sys	_msg, continue_msg, 255, 07h

getchar_@:
	; getchar
	;mov	ah, 10h
	;int	32h

	; stdio getchar (TRDOS 386 Kernel v2.0.9 and later)
	;mov	bl, 6	; read character (ascii and scancode) on stdin
			; -no redirection, wait-
	sys	_stdio, 6
	
	cmp	al, 1Bh ; ESC key
	je	short terminate

	call	write_sample_text

terminate:
	sys	_msg, crlf, 2, 07h

	sys	_exit, 0  ; ebx = exit code
	;sys	_exit
here:
	nop
	jmp	short here

write_sample_text:
	; CL = 0FFh
	; CH = 0 -> 24 bytes buffer
	sys	_fff, sample_file, 0FFh, fff_buffer
	jc	short wst_create

	test	byte [edx], 1Fh ; DVSHR
	jnz	short wsta_error

	sys	_msg, write_option_hdr, 255, 0Eh
	sys	_msg, write_option, 255, 0Fh

wst_get_option:
	sys	_stdio, 6 ; read character (from STDIN)
	cmp	al, 0Dh ; ENTER
	je	short wst_create
	cmp	al, 1Bh ; ESC
	jne	short wst_get_option

	;sys	_delete, sample_file
	sys	_unlink, sample_file
	jnc	short wst_create

wsta_error:
	sys	_msg, access_error, 255, 0Dh
	retn

wst_create:
	; create SAMPLE.TXT or truncate it if it is existing
	sys	_creat, sample_file, 0 ; create normal file
	jnc	short wst_continue_1 ; eax = file handle
wstc_error:
	sys	_msg, create_error, 255, 0Dh
	retn

wst_continue_1:
	mov	[filehandle], eax

	sys	_write, eax, sample_text, sample_text_size
	jnc	short wst_continue_2

	sys	_msg, write_error, 255, 0Dh
	jmp	short wst_close

wst_continue_2:
	and	eax, eax ; written bytes
	jz	short wst_close

	; written message (success/completed msg)
	sys	_msg, msg_written, 255, 07h

wst_close:
	; close SAMPLE.TXT file
	sys	_close, [filehandle]
	retn

;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

program:
		db 0Dh, 0Ah
		db "TRDOS 386 v2.0.9 - SYSWRITE Function Test"
		db 0Dh, 0Ah
len0	equ $-program
		db 0
owner:
		db "Erdogan Tan - 18/09/2024"
		db 0Dh, 0Ah, 0

msg_written:	
		db 0Dh, 0Ah
		db "written ..."
		db 0Dh, 0Ah, 0

sample_text_hdr:
		db 0Dh, 0Ah
		db "Sample Text to be written into SAMPLE.TXT file :"
		db 0Dh, 0Ah, 0
sample_text:
		db "This is a text for TRDOS 386 v2 SYSWRITE system call test."
		db 0Dh, 0Ah
		db 0Dh, 0Ah
		db "I run each teen me? Each team. Catch bar duck each teen?"
		db 0Dh, 0Ah
		db "On bar duck each team. Why high one why!"
		db 0Dh, 0Ah, 0
sample_text_size equ $ - (sample_text+1) ; except 0

continue_msg:
		db 0Dh, 0Ah
		db "Press a key to continue or press ESC to exit."
crlf:
		db 0Dh, 0Ah, 0

sample_file:
		db "SAMPLE.TXT", 0
create_error:
		db 0Dh, 0Ah
		db "SYSCREAT - File creating/open error !"
		db 0Dh, 0Ah,0
access_error:
		db 0Dh, 0Ah
		db "File access/delete error !"
		db 0Dh, 0Ah,0
write_error:
		db 0Dh, 0Ah
		db "SYSWRITE - File write error !"
		db 0Dh, 0Ah,0

write_option_hdr:
		db 0Dh, 0Ah
		db "SAMPLE.TXT file exists! Select owerwrite option:"
		db 0Dh, 0Ah
		db 0
write_option:
		db 0Dh, 0Ah
		db "ENTER - Create/Truncate method (SYSCREAT)"
		db 0Dh, 0Ah
		db "ESC - Delete/Create method (SYSUNLINK+SYSCREAT)"
		db 0Dh, 0Ah, 0

filehandle:	dd 0

fff_buffer:	times 24 db 0FFh