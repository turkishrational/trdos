; ****************************************************************************
; invalid.s - TRDOS 386 (TRDOS v2.0) Kernel - 'sysexec' exit code test
; ----------------------------------------------------------------------------
;
; 20/08/2024
;
; [ Last Modification: 20/08/2024 ]
;
; ****************************************************************************
; invalid system call test
; (program will be terminated and exit code will be -1)

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
	sys	_msg, sysexit_test_hdr, 255, 0Bh

	sys	_msg, sysexit_test_msg1, 255, 0Fh	
	sys	_msg, sysexit_test_msg2, 255, 0Fh
	sys	_msg, sysexit_test_msg3, 255, 0Fh
getchar:
	xor	ah, ah
	int	32h

	cmp	al, 1Bh ; ESC key
	je	short child_ok_exit

	cmp	al, 0Dh ; ENTER (CR) key
	jne	short getchar

	_INVALID equ 255 ; > 46 
			 ; (the last system call number for v2.0.9)
	sys	_INVALID

child_ok_exit:
	sys	_msg, msg_ok, 255, 07h
terminate:
	mov	ebx, 24 ; 'programmed in 2024' signature
	sys	_exit

	jmp	short terminate

;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

sysexit_test_hdr:
		db 0Dh, 0Ah
		db "TRDOS 386 v2.0.9 - <sysexit> exit code -return- test"
		db 0Dh, 0Ah, 0
sysexit_test_msg1:
		db 0Dh, 0Ah
		db "Press ENTER to get INVALID SYSTEM CALL response."
		db 0Dh, 0Ah
		db "(Exit code will be 0FFh or 255. It means error.)" 
		db 0Dh, 0Ah, 0
sysexit_test_msg2:
		db 0Dh, 0Ah
		db "Press ESC to skip INVALID SYSTEM CALL test."
		db 0Dh, 0Ah
		db "(User's exit code will be 018h or 24.)"
		db 0Dh, 0Ah, 0
sysexit_test_msg3:
		db 0Dh, 0Ah
		db "NOTE: If this program is run as a child"
		db 0Dh, 0Ah
		db "      by the exit code test program as parent,"
		db 0Dh, 0Ah
		db "      you will see the exit code."
		db 0Dh, 0Ah, 0
msg_ok:
        	db 0Dh, 0Ah
        	db "OK. "
        	db 0Dh, 0Ah
sizeof_ok 	equ $ - msg_ok 
        	db 0