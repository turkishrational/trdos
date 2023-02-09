; ****************************************************************************
; readfile.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'readfile.prg')
; ----------------------------------------------------------------------------
; READFILE.PRG ! 'sysopen' and 'sysread' TEST program for TRDOS 386 !
;
; 11/10/2016
;
; [ Last Modification: 13/10/2016 ]
;
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
	jb	prg_msg
	lodsd ; program file name address 
	lodsd ; text file name address
	push	eax ; arg2 ; file name

	sys	_open, eax, 0 ; open for reading
	jc	open_error

	mov	[fhandle], eax ; file handle/index number

	sys	_msg, msg_reading, 255, 0Ah ; Green

	pop	eax

	sys	_msg, eax, 255, 0Ch ; Red

	mov	esi, nextline
	call	print_msg


	sys	_read, [fhandle], BUFFER, 0FFFFFFFFh ; read count = 4 Giga Bytes
					       ; read count = file size
 	jc	short read_error ; disk read or memory allocation error			

show_text:
	mov	esi, BUFFER
	call	print_msg

cf_terminate:
	sys	_close, [fhandle] ; close file

terminate:
	sys 	_exit			   ; INT 40h
here:
	jmp	short here

prg_msg:
	sys	_msg, msg_program, 255, 0Fh ; White
	jmp	short terminate

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

fhandle: dd 0

;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

msg_program:
	db 0Dh, 0Ah
	db "READFILE.PRG /// TRDOS 386 sysopen, sysread test program"
	db 0Dh, 0Ah
	db "by Erdogan Tan, 13/10/2016", 0Dh, 0Ah, 0
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
msg_reading:
	db 0Dh, 0Ah
	db 'reading file '
;bss

BUFFER:
	dw 0