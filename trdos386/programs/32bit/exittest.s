; ****************************************************************************
; exittest.s - TRDOS 386 (TRDOS v2.0) Kernel - 'sysexit' exit code test
; ----------------------------------------------------------------------------
;
; 20/08/2024
;
; [ Last Modification: 23/08/2024 ]
;
; Derived from 'TRDOS 386 v2' FORK TEST source code by Erdogan Tan
; ('forkstest.s', 12/11/2017)
;
; ****************************************************************************
; forktest.s, 17/09/2015 (Retro UNIX 386 v1, NASM 2.11)

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
; FORK TEST - CODE
;========================================================================

[BITS 32] ; 32-bit intructions

[ORG 0] 
START_CODE:
	mov 	esi, sysexit_test
	call 	print_msg

      	mov 	ebx, chldr
	sys 	_fork
	jc 	short error

	mov	[cpid], eax ; child's process ID

	mov	edi, pprocessid
	call 	bin_to_decimal_str

pwait:
	mov	ebx, 999 ; this must not return
	sys 	_wait
	jc 	short error

	cmp 	eax, [cpid]
	jne 	short pwait	

	; EAX = child's process id (AX)
	; EBX = child's exit code (BL) ; must be 209

	push	ebx  ; exit code

	mov	edi, cprocessid
	call 	bin_to_decimal_str

	mov 	esi, child_pid
	call 	print_msg

	pop	eax

	mov	edi, exitcode
	call 	bin_to_decimal_str

	mov 	esi, child_exitcode
	call 	print_msg
_ok:
	mov 	esi, msg_ok
	call 	print_msg

	jmp 	short here

error:
	mov 	esi, msg_err
	call 	print_msg
 	;jmp 	short here
here:	
	sys	_exit
	; hlt
	; jmp 	short here
	
chldr:
	; EAX = parent's process ID
	mov	edi, pprocessid
	call 	bin_to_decimal_str

	mov 	esi, parent_pid
	call 	print_msg

	mov	ebx, 209
	sys 	_exit

        jmp     short _ok

print_msg:
	sys 	_msg, esi, 255, 0Fh ; message with white color (max. 255 chars)
	retn

bin_to_decimal_str:
	; eax = binary number
	mov	ecx, 10
	mov	ebp, esp
btd_@:
	xor	edx, edx
  	div	ecx
	push	edx
	or	eax, eax
	jnz	short btd_@
btd_@@:
	pop	ecx
	mov	al, cl
	add	al, '0'
	stosb
	cmp	ebp, esp
	jne	short btd_@@
	mov	byte [edi],0
	retn

;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

cpid:		dd 0

sysexit_test:	db 0Dh, 0Ah
		db 'TRDOS 386 - <sysexit> exit code -return- test'
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
        	db 0Dh, 0Ah
sizeof_ok 	equ $ - msg_ok 
        	db 0