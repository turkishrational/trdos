; ****************************************************************************
; piano.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'piano.prg')
; ----------------------------------------------------------------------------
; PIANO.PRG ! TEST program !  INT 34h (IOCTL functions) test !
;
; 20/06/2016
;
; [ Last Modification: 20/06/2016 ]
;
; ****************************************************************************

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
	sys	_msg, msg_piano, msg_size, msg_color ; INT 40h
getchar:
	; Getchar by using keyboard interrupt
	mov	ah, 10h
	int 	32h ; TRDOS 386 keyboard interrupt
		    ; (IBM PC/AT ROMBIOS, INT 16h)			
	
	cmp	al, 1Bh
	je	short terminate

	cmp	al, '1'
	jb	short error
	cmp	al, '8'
	ja	short error

	and	al, 0Fh
	dec	al
	shl	al, 1 
	movzx	esi, al
	add	esi, FREQUENCY

	mov	bx, [esi]
	
	mov	ah, 1 ; write port
	mov	al, ah ; 1
	mov	dx, 61h ; I/O port number
	int	34h ; TRDOS 386 IOCTL interrupt
	 
	mov	al, 0B6h
	mov	dx, 43h
	int	34h ; TRDOS 386 IOCTL interrupt

	mov	al, bl
	;mov	dx, 42h
	dec	dx
	int	34h ; TRDOS 386 IOCTL interrupt

	mov	al, bh
	;mov	dx, 42h
	int	34h ; TRDOS 386 IOCTL interrupt

	mov	dx, 61h
	sub	ah, ah ; 0 -> read port
	int	34h ; TRDOS 386 IOCTL interrupt
	
	or	al, 3

	inc 	ah ; mov ah, 1 ; write port
	;mov	dx, 61h
	int	34h ; TRDOS 386 IOCTL interrupt

	call	delay

	mov	dx, 61h
	;sub	ah, ah ; 0 -> read port
	int	34h ; TRDOS 386 IOCTL interrupt	

	and	al, 0FCh
	inc 	ah ; mov ah, 1 ; write port
	;mov	dx, 61h
	int	34h ; TRDOS 386 IOCTL interrupt

	jmp	short getchar 

terminate:
	sys 	_exit			   ; INT 40h
here:
	jmp	short here

error:
	cmp	al, '+'
	jne	short _0
	inc	 byte [delaym]
	jmp	short _1
_0:
	cmp	al, '-'
	jne	short _1
	dec	byte [delaym]
_1:
	mov 	esi, msg_piano_keys
	;
	mov	ebx, 0Fh       ; white characters (bl)
		               ; video page 0 (bh)
	mov	ah, 0Eh  ; teletype output (write tty)
	lodsb
_2:
	int	31h	 ; TRDOS 386 video interrupt
	lodsb
	and	al, al
	jnz	short _2
	;
	jmp	getchar

delay:
	mov	eax, 0FFFFh
	mov	cl, [delaym]
	shl	eax, cl
dl0:	
	nop
	dec	eax
	jnz	short dl0
	retn

FREQUENCY:
	dw	2280, 2031, 1809, 1709, 1521, 1355, 1207, 1139 

delaym:
	db 	4

;-----------------------------------------------------------------
;  message
;-----------------------------------------------------------------

msg_color	equ 0Bh ; ligth cyan

msg_piano:	db 0Dh, 0Ah
		db 'TRDOS 386 IOCTL (INT 34h) Test Program by Erdogan Tan'
		db 0Dh, 0Ah
msg_piano_keys:
		db 'Piano (Beep) keys: "1" to "8" (Press ESC key to exit)'
		db 0Dh, 0Ah
msg_size equ    $ - msg_piano
		db 0
db '20/06/2016'