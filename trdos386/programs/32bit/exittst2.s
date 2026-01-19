; ****************************************************************************
; exittst2.s - TRDOS 386 (TRDOS v2.0) Kernel - 'sysexit' exit code test 
; ----------------------------------------------------------------------------
;
; 20/08/2024
;
; [ Last Modification: 20/08/2024 ]
;
; Modified from 'TRDOS 386 v2' EXIT TEST source code by Erdogan Tan
; ('exittest.s', 20/08/2024)
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
; EXIT TEST - CODE
;========================================================================

[BITS 32] ; 32-bit intructions

[ORG 0] 
START_CODE:
	mov	byte [color], 0Ch
	mov 	esi, sysexit_test
	call 	print_msg

      	mov 	ebx, chldr
	sys 	_fork
	jc 	short error

	mov	[cpid], eax ; child's process ID

	call 	bin_to_decimal_str
	mov 	[pprocessid], ebx

	mov	byte [color], 07h
pwait:
	mov	ebx, 999 ; this must not return
	sys 	_wait
	;jc 	short error
	jnc	short pwait_ok

error:
	mov 	esi, msg_err
	call 	print_msg
 	;jmp 	short here
here:	
	sys	_exit
	; hlt
	; jmp 	short here

pwait_ok:
	cmp 	eax, [cpid]
	jne 	short pwait	

	; EAX = child's process id (AX)
	; EBX = child's exit code (BL) ; must be 209

	push	ebx  ; exit code

	call 	bin_to_decimal_str
	mov 	[cprocessid], ebx

	mov	byte [color], 0Ah
	mov	esi, child_name
	call	print_msg
	mov	byte [color], 0Eh
	mov	esi, prgfilename
	call	print_msg	

	mov	byte [color], 07h
	mov	esi, CRLF
	call	print_msg

	mov	byte [color], 0Fh
	mov 	esi, child_pid
	call 	print_msg

	pop	eax

	call 	bin_to_decimal_str
	mov 	[exitcode], ebx

	mov 	esi, child_exitcode
	call 	print_msg
_ok:
	mov	byte [color], 07h

	mov 	esi, msg_ok
	call 	print_msg

_here:
	sys	_exit

	; CPU must not come here
	jmp	short _ok

chldr:
	; EAX = parent's process ID
	call 	bin_to_decimal_str
	mov 	[pprocessid], ebx

	mov	byte [color], 0Fh

	mov 	esi, parent_pid
	call 	print_msg

	mov	byte [color], 07h

	mov	esi, CRLF
	call	print_msg

	; EXEC child process (replaced by another program) 
	sys	_exec, prgfilename, prgp

	jnc	short chld_exit2 ; eax = exit code

	cmp	al, 2 ; File Not Found
	jne	short _here

	mov	byte [color], 0Fh

	mov	esi, prgfilename
	call	print_msg

	mov	esi, not_found
	call	print_msg

chld_exit:
	mov	eax, 209
chld_exit2:
	; eax = exit code
	mov	ebx, eax
	sys 	_exit

        jmp     _ok

print_msg:
	sys 	_msg, esi, 255, [color]
			; message with color (max. 255 chars)
	retn

bin_to_decimal_str:
	; eax = binary number
	mov	ecx, 10
	xor	ebx, ebx
btd_@:
	xor	edx, edx
  	div	ecx
	shl	ebx, 8
	add	dl, '0'
	mov	bl, dl
	or	eax, eax
	jnz	short btd_@
btd_@@:
	; ebx = db 'nnn', 0
	retn

;-----------------------------------------------------------------

color:	dd 0

child_name:
	db 0Dh, 0Ah
	db "Child PRG Name: ", 0
prgfilename:
	db "INVALID.PRG",0
;arguments:
	;dd 0

not_found:
	db " not found ! ", 0Dh, 0Ah, 0

prgp:	dd prgfilename
	;dd arguments
	dd 0
	dd 0

;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

cpid:		dd 0

sysexit_test:	db 0Dh, 0Ah
		db 'TRDOS 386 v2.0.9 - <sysexit> exit code -return- test'
		db 0
parent_pid:	db 0Dh, 0Ah
		db 'Parent Process ID: '
pprocessid:	dd 30303030h
		db 0Dh, 0Ah
sizeofparentpid equ $ - parent_pid
		db 0

child_pid:	db 0Dh, 0Ah
		db 'Child Process ID: '
cprocessid:	dd 30303030h
		db 0Dh, 0Ah
sizeofchildpid equ $ - child_pid
		db 0

child_exitcode:	db 0Dh, 0Ah
		db 'Child Exit Code: '
exitcode:	dd 30303030h
		db 0Dh, 0Ah
sizeofchildexit	equ $ - child_exitcode
		db 0
msg_err:
		db 0Dh, 0Ah 
                db 'Error ! '
		db 0Dh, 0Ah, 0
msg_ok:
        	db 0Dh, 0Ah
        	db 'OK. '
CRLF:
        	db 0Dh, 0Ah
sizeof_ok 	equ $ - msg_ok
sizeof_CRLF 	equ $ - CRLF
        	db 0