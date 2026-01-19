; ****************************************************************************
; scancode.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'scancode.prg')
; ----------------------------------------------------------------------------
; SCANCODE.PRG ! TEST program !  INT 32h (Kerboard BIOS) test !
;
; 13/05/2016
;
; Derived from 'scancode.asm' source code for MSDOS (Erdogan Tan, 1998)
;
; [ Last Modification: 13/05/2016 ]
;
; ****************************************************************************
; SCANCODE.ASM (01/02/1998, MSDOS Program by Erdogan Tan) 
; 'scancode' prints character codes

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

start:
	mov     esi, BossMsg
	call    print_msg
again:
	mov     byte [Character], 20h
	mov	ah, 10h		; Keyboard Service - Extended read
	int	32h		; TRDOS 386 Keyboard BIOS interrtupt
				; (INT 16h for Real Mode BIOS)	

	push	eax
	and	al, al 
	jz	short pass_enter
	cmp	al, 0Dh
	je	short pass_enter
	mov	[Character], al
pass_enter:    
	mov	al, ah
	call    bin_to_hex
	mov     [Reg_ScanCode], ax
	mov	eax, [esp]
	call    bin_to_hex
	mov     [Reg_AsciiCode], ax

	mov     esi, ScancodeMsg
        call    print_msg
	pop	eax
	cmp     al, 0Dh ; ENTER (CR) key
	jne     short again

	sys 	_exit		; TRDOS 386 System call
				; (INT 40h, EAX=1)
				; (terminate process)
here:
	jmp	short here

print_msg:
 	; 12/05/2016
	mov	ebx, 07h        ; light gray characters (bl)
				; video page 0 (bh)
	mov	ah, 0Eh  	; teletype output (write tty)
	lodsb
_1:
	int	31h		; TRDOS 386 Video BIOS interrupt
				; (INT 10h for real mode BIOS)
	lodsb
	and	al, al
	jnz	short _1
_2:
	retn

bin_to_hex:
	; binary (byte) to hexadecimal (character) converter
	; by Erdogan Tan (1998)
	;
	; input  -> AL = byte (binary number) to be converted 
	; output -> AL = first character of hexadecimal number
	;	    AH = second character of hexadecimal number

	mov     ah, al
	and     ah, 0Fh
	add     ah, 30h
	cmp     ah, 39h
	jna     short pass1
	add     ah, 07h
pass1:
	shr     al, 4
	add     al, 30h
	cmp     al, 39h
	jna     short pass2
	add     al, 07h
pass2:
	retn

;-----------------------------------------------------------------
;  message
;-----------------------------------------------------------------

BossMsg:
	db 0Dh, 0Ah
	db '[ (c) Erdogan TAN  1998-2016 ]  Press a key to see ASCII and scan code...'
	db 0Dh, 0Ah
	db 0Dh, 0Ah, 0h
ScancodeMsg:
	db 'Character : '
Character:
	db '?'
	db '     Scan Code : '
Reg_ScanCode:
	dw '??'
	db 'h'
	db '     ASCII Code : '
Reg_AsciiCode:
	dw '??'
	db 'h'
	db 0Dh, 0Ah, 0h
