; ****************************************************************************
; ctrlbk.s - TRDOS 386 (TRDOS v2.0) Kernel - CTRL+BRK test
; ----------------------------------------------------------------------------
;
; 23/08/2024
;
; [ Last Modification: 23/08/2024 ]
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
	sys	_msg, owner, 255, 0Ch

	sys	_intr, 0	 ; disable CTRL+BREAK terminate function

	sys	_msg, ctrlbrk_def, len1, 07h
	sys	_msg, ctrlbrk, len2, 0Fh
	sys	_msg, ctrlbrk_skip, len3, 07h

	;sys	_time, 0  ; get time in unix epoch format
	;mov	[time], eax
	jmp	short getchar_@@@

getchar_@:
	cmp	byte [stage], 1
	jne	short getchar_@@@
getchar_@@:
	sys	_quit
	cmp	eax, -1 ; quit
	jne	short getchar_@@@
	cmp	byte [stage], 1
	jb	short getchar_@@@
	jmp	quit_request

getchar_@@@:
	; getchar
	;mov	ah, 10h
	;int	32h

	; stdio getchar (TRTDOS 386 Kernel v2.0.9 and later)
	;mov	bl, 7	; read character (ascii and scancode) on stdin
			; -no redirection, no wait-
	sys	_stdio, 7
	
	and	eax, eax
	jz	short getchar_@@

	cmp	al, 20h
	je	short skip_stage_@

	;sys	_time, 0 ; get time in unix epoch format
	
	;cmp	eax, [time]
	;je	short getchar_@ ; same second
	
	xor	eax, eax
	mov	bl, [counter]
	inc	byte [counter]
	and	bl, 3
	mov	al, len4
	mul	bl
	add	eax, _one_two_three_four
print_1234:
	sys	_msg, eax, len4, 07h
	test	byte [counter], 3
	jnz	short getchar_@
print_crlf:
	sys	_msg, crlf, 2, 07h
	jmp	getchar_@

skip_stage_@:
	inc	byte [stage]
	cmp	byte [stage], 1
	ja	short enable_ctrlbrk
	sys	_msg, sysquit_msg, 255, 0Ah
	sys	_msg, ctrlbrk_skip, len3, 07h
skip_stage_@@:
	test	byte [counter], 3
	mov	byte [counter], 0
	jz	short print_crlf
	jmp	getchar_@@

enable_ctrlbrk:
	cmp	byte [stage], 2
	;ja	short _ok
	jna	short enable_ctrlbrk_@
	jmp	_ok
enable_ctrlbrk_@:
	sys	_intr, 1
	sys	_msg, ctrlbrk_enabled, 255, 0Eh
	sys	_msg, ctrlbrk, len2, 0Fh
	sys	_msg, ctrlbrk_skip, len3, 07h
	jmp	getchar_@@@

quit_request:
	sys	_msg, quit_question, 255, 0Fh
quit_request_@:
	; getchar
	;mov	ah, 10h
	;int	32h

	; stdio getchar (TRTDOS 386 Kernel v2.0.9 and later)
	;mov	bl, 6	; read character (ascii and scancode) on stdin
			; -no redirection, wait-
	sys	_stdio, 6

	and	al, ~20h ; 0DFh ; capitalize letter
	cmp	al, 'Y'
	je	short print_yes
	cmp	al, 'N'
	jne	short quit_request_@
print_no:
	mov	byte [counter], 0
	sys	_msg, msg_no, 4, 07h
	jmp	getchar_@@

print_yes:
	sys	_msg, msg_yes, 5, 07h
	jmp	short terminate

_ok:
	sys	_msg, msg_ok, 255, 07h

terminate:
	sys	_msg, crlf, 2, 07h

	sys	_exit, 0	; ebx = exit code
	;sys	_exit
here:
	nop
	jmp	short here

;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

program:
		db 0Dh, 0Ah
		db "TRDOS 386 v2.0.9 - CTRL+BRK Function Test"
		db 0Dh, 0Ah
len0	equ $-program
		db 0
owner:
		db "Erdogan Tan - 23/08/2024"	
		db 0Dh, 0Ah, 0

ctrlbrk_def:
		db 0Dh, 0Ah
		db "Default CTRL+BRK function disabled."
len1	equ  $-ctrlbrk_def
		db 0
ctrlbrk:
		db 0Dh, 0Ah
		db "Press CTRL+BREAK keys (together) to test."
		db 0Dh, 0Ah
len2	equ  $-ctrlbrk
		db 0
ctrlbrk_skip:	
		db 0Dh, 0Ah
		db "Press SPACEBAR to skip this test stage."
crlf:
		db 0Dh, 0Ah
len3	equ  $-ctrlbrk_skip
		db 0

sysquit_msg:
		db 0Dh, 0Ah
		db "SYSQUIT CTRL+BRK handling test."
		db 0Dh, 0Ah, 0
quit_question:
		db 0Dh, 0Ah
		db "SYSQUIT CTRL+BREAK"
		db 0Dh, 0Ah	
		db "Do you want to quit/break (Y/N)? " 
		db 0
ctrlbrk_enabled:
		db 0Dh, 0Ah
		db "Default CTRL+BRK function ENABLED."
		db 0

time:		dd 0
counter:	db 0
stage:		db 0

_one_two_three_four:
		db "ONE   "
len4	equ $ - _one_two_three_four
		db "TWO   "
		db "THREE "
		db "FOUR  "

msg_yes:	db "Yes", 0Dh, 0Ah, 0
msg_no:		db "No", 0Dh, 0Ah, 0

msg_ok:
		db 0Dh, 0Ah
		db "OK. "
		db 0