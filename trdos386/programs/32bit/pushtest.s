; ****************************************************************************
; pushtest.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'pushtest.prg')
; ----------------------------------------------------------------------------
; PUSHTEST.PRG ! TEST program !
;
; 10/08/2016
;
; ****************************************************************************

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
	mov	ebp, esp
	mov	bx, bp
	push	bx
	cmp 	word [esp], bx
	jne	short _1

	sys	_msg, msg_ok1, 255, 0Fh	

	jmp	 short _1_ok
_1:	
	sys	_msg, msg_err1, 255, 0Fh	
_1_ok:
	mov	ebx, ebp
	sub	ebx, 4
	cmp	ebx, esp
	jne	short _2

	sys	_msg, msg_ok2, 255, 0Fh	

	jmp	short _2_ok
_2:
	add	ebx, 2
	cmp	ebx, esp
	je	short _2_err

	sys	_msg, msg_err, 255, 0Fh	

	jmp	short _2_ok
_2_err:
	sys	_msg, msg_err2, 255, 0Fh	
_2_ok:
	pop	bx

	push	word [numara]
	cmp	word [esp], 1234
	jne	short _3

	sys	_msg, msg_ok3, 255, 0Fh	

	jmp	short _3_ok
_3:
	sys	_msg, msg_err2, 255, 0Fh
_3_ok:
	mov	ebx, ebp
	sub	ebx, 4
	cmp	ebx, esp
	jne	short _4

	sys	_msg, msg_ok4, 255, 0Fh	

	jmp	short _4_ok
_4:
	add	ebx, 2
	cmp	ebx, esp
	je	short _4_err

	sys	_msg, msg_err, 255, 0Fh	

	jmp	short _4_ok
_4_err:
	sys	_msg, msg_err4, 255, 0Fh	
_4_ok:
	pop	ebx
	cmp	ebx, 1234
	jne	short _5

	sys	_msg, msg_ok5, 255, 0Fh

	jmp	short _5_ok
_5:
	push	ebx
	sys	_msg, msg_err5, 255, 0Fh
	pop	bx
	cmp	bx, 1234
	jne	short _5_ok
	sys	_msg, msg_ok6, 255, 0Fh
_5_ok:
	mov	ah, 0Eh
	mov	al, 0Dh
	int	31h ; Video Interrupt
	mov	al, 0Ah
	int	31h ; Video Interrupt

	sys 	_exit			   ; INT 40h
here:
	jmp	short here

;-----------------------------------------------------------------
;  DATA
;-----------------------------------------------------------------

numara:	
	dw	1234

;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

msg_ok1:
	db	0Dh, 0Ah
	db	'OK ! (push bx : word [esp] = bx)'
	db	0
msg_ok2:
	db	0Dh, 0Ah
	db	'OK ! (push bx : esp = esp - 4)'
	db	0
msg_ok3:
	db	0Dh, 0Ah
	db	'OK ! (push word [numara] : word [esp] = 1234)'
	db	0
msg_ok4:
	db	0Dh, 0Ah
	db	'OK ! (push word [numara] : esp = esp - 4)'
	db	0
msg_ok5:
	db	0Dh, 0Ah
	db	'OK ! (pop ebx  : ebx = 1234 = [numara])'
	db	0Dh, 0Ah, 0

msg_err1:
	db	0Dh, 0Ah
	db	'  Error ! (push bx : word [esp] = bx)'
	db	0

msg_err2:
	db	0Dh, 0Ah
	db	'  Error ! (push bx : esp = esp - 4) : (( esp = esp - 2 ))'
	db	0

msg_err3:
	db	0Dh, 0Ah
	db	'  Error ! (push word [numara] : word [esp] = 1234)'
	db	0

msg_err4:
	db	0Dh, 0Ah
	db	'  Error ! (push word [numara] : esp = esp - 4) : (( esp = esp - 2 ))'
	db	0
msg_err5:
	db	0Dh, 0Ah
	db	'  Error ! (pop ebx : ebx = 1234 = [numara])'
	db	0
msg_ok6:
	db	0Dh, 0Ah
	db	'OK ! (pop bx : bx = 1234 = [numara])'
	db	0Dh, 0Ah, 0
msg_err:
	db	0Dh, 0Ah, '  ERROR !', 0