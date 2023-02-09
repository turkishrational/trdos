; ****************************************************************************
; flist.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'flist.prg')
; ----------------------------------------------------------------------------
; FLIST.PRG ! 'sysfff' and 'sysfnf' TEST program for TRDOS 386 !
;
; 17/10/2016
;
; [ Last Modification: 17/10/2016 ]
;
; ****************************************************************************

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
_rsvd1	equ 37
_pri	equ 38
_rele	equ 39
_fff	equ 40
_fnf	equ 41

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
	; EAX = arg2 ; file name address

	; EBX = EAX = file name or path address
	; CL = file attributes (archive = 20h, read only = 01h)
	;	(21h = Archived & Read Only files are included)
	; CH = 0 = basic parameters (24 bytes)
	; EDX = DTA = buffer address (24 bytes for basic parameters)
	; EAX = _fff = 'Find First File' system call for TRDOS 386

	push	eax

	mov	esi, msg_program
	call	print_msg

	xor	ah, ah
	int	32h

	pop	eax

	; Find First File
	sys	_fff, eax, 0021h, DTA
_0:
	jc	terminate  ; terminate if there is 
			   ; file not found error or another error

	; check file attributes
	test	byte [DTA], 1Eh ; 10h = directory, 08h = volume label
				; 04h = system, 02h = hidden
	jz	short _2   ; atributes are proper 
		
_1:
	; Find Next File
	sys	_fnf	; if the first file is not proper file
			; check for next file
	jmp	short _0
_2:
	mov	esi, nextline
	call	print_msg

	mov	esi, DTA+10 ; ASCIIZ, capitalized file name
	call	print_msg

	xor	ah, ah
	;int	16h	; KEYBOARD - READ CHAR FROM BUFFER, WAIT IF EMPTY
			; Return: AH = scan code, AL = character
	int	32h	; TRDOS 386 Keyboard interrupt 
		    	; Note: This is not a system call -INT 40h-,
		    	; this is a direct video service like as
		    	; INT 16h ROMBIOS service in real mode.

	sys	_fnf
	
	jmp	_0 	; loop (we use same code for the next file)

terminate:
	mov	esi, nextline
	call	print_msg

	sys 	_exit			   ; INT 40h
here:
	jmp	short here

print_msg:
	mov	ebx, 0Fh       ; white characters (bl)
		               ; video page 0 (bh)
	mov	ah, 0Eh ; teletype output (write tty)
	lodsb
_p_nextchar:
	int	31h
	lodsb
	and	al, al
	jnz	short _p_nextchar
	retn

;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

msg_program:
	db 0Dh, 0Ah
	db "FLIST.PRG /// TRDOS 386 sysfff, sysfnf test program"
	db 0Dh, 0Ah
	db "by Erdogan Tan, 17/10/2016", 0Dh, 0Ah, 0
nextline:
	db 0Dh, 0Ah, 0

bss:

ABSOLUTE bss

alignb 4

DTA:	 resb 24