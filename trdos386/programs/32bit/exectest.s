; ****************************************************************************
; exectest.s - TRDOS 386 (TRDOS v2.0) Kernel - 'sysexec' test
; ----------------------------------------------------------------------------
;
; 12/11/2017
;
; [ Last Modification: 19/11/2017 ]
;
; ****************************************************************************
; EXECTEST.PRG : 'sysexec' test with 'sysfork' - 19/11/2017
; EXECTST2.PRG : direct sysexec test (without 'sysfork') - 19/11/2017

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
	; clear bss
	;mov	edi, bss_start ;
	mov	di, bss_start
	mov	ecx, (bss_end - bss_start)/4
	;xor	eax, eax
	rep	stosd
GetFileName:
	mov	esi, esp
	lodsd
	cmp	eax, 2 ; two arguments 
		; (program file name & mod file name)
	jb	pmsg_usage ; nothing to do
	ja	short a_0
	mov	byte [maxargslength], 0
a_0:
	; 18/11/2017
	dec	eax
	mov	[argc], al
	;
	lodsd ; name of this program
	lodsd ; program file name address (to be launched)
	mov	ebp, esi ; 18/11/2017
	mov	esi, eax
	mov	edi, prgfilename
ScanName:       
	lodsb
	cmp	al, 20h
	je	short ScanName	; scan start of name.
	jb	pmsg_usage
	stosb
a_1:
	dec	byte [maxfnlength]
	jz	short a_2
	lodsb
	cmp	al, 20h
	jna	short a_2	
	stosb
	jmp	short a_1
a_2:
	;mov	ebx, edi 
	;sub	ebx, prg_file_name ; file name length
	;mov	[maxfnlength], bl

	mov	edi, arguments 
	
	; 18/11/2017
	dec	byte [argc]
	jz	short a_4
	;cmp	byte [maxargslength], 0
	;jna	short a_4
	mov	esi, [ebp]
	add	ebp, 4
a_3:
	lodsb
	cmp	al, 20h
	;jna	short a_4
	jna	short a_6 ; 18/11/2017
	stosb
	dec	byte [maxargslength]
	jnz	short a_3
a_6:
	; 18/11/2017
	dec	byte [argc]
	jz	short a_4
	dec	byte [maxargslength]
	jz	short a_4
	mov	al, 20h
	stosb	
	mov	esi, [ebp]
	;and	esi, esi
	;jz	short a_4
	add	ebp, 4
	jmp	short a_3
a_4:	
	; 18/11/2017
	xor	al, al
	stosb

	;mov	ecx, edi
	;sub	ecx, arguments ; arguments length
	;mov	[maxargslength], cl

	sys	_msg, msg_program1, 255, 0Bh

	mov	ebx, child_exec
	sys	_fork

	sys	_wait

parent_return:
	sys	_msg, msg_program5, 255, 0Bh	
	jmp	terminate

child_exec:
	sys	_msg, msg_program2, 255, 0Ah
	sys	_msg, prgfilename, 255, 0Ch

	cmp	byte [maxargslength], 0
	jna	short a_5

	sys	_msg, msg_program3, 255, 0Ah
	sys	_msg, arguments, 255, 0Ch
a_5:	
	sys	_msg, msg_program4, 255, 0Fh
	
	xor	ah, ah
	int	32h

	cmp	al, 1Bh ; ESC key
	je	short child_cancel

	sys	_exec, prgfilename, prgp 

	sys	_msg, error_msg, 255, 0Eh

terminate:
	sys	_exit

pmsg_usage:      
	sys	_msg, msg_program1, 255, 0Ah
	sys	_msg, msg_usage, 255, 0Fh
	
	jmp	short terminate

child_cancel:
	sys	_msg, msg_program6, 255, 0Eh
	jmp	short terminate

align 4
prgp:	dd prgfilename
	dd arguments
	dd 0

maxfnlength:	db 79
maxargslength:	db 79				

;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

msg_usage:
	db 0Dh, 0Ah
	db 'Usage: exectest <program file name> <arguments>'
	db 0Dh, 0Ah, 0

msg_program1:
	db "EXECTEST.PRG /// TRDOS 386 'sysexec' test program"
	db 0Dh, 0Ah
	db "by Erdogan Tan, 19/11/2017", 0Dh, 0Ah, 0
msg_program2:
	db 0Dh, 0Ah
	db "PROGRAM NAME: ", 0
msg_program3:
	db 0Dh, 0Ah
	db "Argument(s) : ", 0		
msg_program4:
	db 0Dh, 0Ah
	db "Press any key to run the program..."
	db 0Dh, 0Ah, 0
msg_program5:
	db 0Dh, 0Ah
	db "OK."
	db 0Dh, 0Ah, 0

msg_program6:
	db 0Dh, 0Ah
	db "CANCEL."
	db 0Dh, 0Ah, 0

error_msg:
	db 0Dh, 0Ah, 07h
	db "'sysexec' error ! "
	db 0Dh, 0Ah
	db 0

bss_start:

alignb 4  ; 19/11/2017

ABSOLUTE bss_start

;========================================================================
; UNINITIALIZED DATA
;========================================================================

argc:	resb 0 ; 18/11/2017

	resb 3 ; 19/11/2017

;alignb 4

prgfilename:  resb 80
arguments:    resb 80

;alignb4

bss_end:	
