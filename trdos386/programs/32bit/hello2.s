; ****************************************************************************
; hello2.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'hello2.prg')
; ----------------------------------------------------------------------------
; HELLO2.PRG ! TEST program !  INT 31h (Video BIOS) test !
;
; 12/05/2016
;
; Derived from 'hello.s' source code for Retro UNIX 386 v1 & TRDOS 386
;
; [ Last Modification: 13/05/2016 ]
;
; ****************************************************************************
;
; hello.s (01/05/2016, TRDOS v2.0)
; hello.s (28/08/2015, Retro UNIX 386 v1, NASM 2.11, 32 bit version)
; HELLO.ASM, 18/11/2013 (Retro UNIX 8086 v1, MASM 6.11) 

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
	;mov	eax, 417
	;int	30h ; 'INVALID SYSTEM CALL' test!
	;
	sys 	_ver ; get TRDOS version   ; INT 40h
	add	ax, '00'
	mov	[ver_str_major], ah
	mov	[ver_str_minor], al
	
	sys	_msg, msg_Hello, msg_size, msg_color ; INT 40h

	mov	esi, msg_int31h
	call	print_msg		   ; INT 31h

	sys 	_exit			   ; INT 40h
here:
	jmp	short here

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

;-----------------------------------------------------------------
;  message
;-----------------------------------------------------------------

msg_color	equ 0Ah ; ligth green

msg_Hello:	db 07h ; beep
		db 0Dh, 0Ah
		db 'Hello world !'
		db 0Dh, 0Ah
		db 'This is Turkish Rational DOS v'
ver_str_major:	db '0'
		db '.'
ver_str_minor:  db '0'
                db ' test program ! (INT 40h, sysmsg)'
		db 0Dh, 0Ah
msg_size equ    $ - msg_Hello
		db 0
msg_int31h:
		db '(This message has been written by using INT 31h teletype function!)'
		db 0Dh, 0Ah, 0
