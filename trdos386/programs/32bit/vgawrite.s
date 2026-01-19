; ****************************************************************************
; vgawrite.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'vgawrite.prg')
; ----------------------------------------------------------------------------
; VGA TELETYPE & SCROLL TEST program !  TRDOS 386 VGA Functionality test !
;
; 09/08/2016
;
; [ Last Modification: 09/08/2016 ]
;
; Assembler: NASM 2.11
;
; ****************************************************************************

; 19/05/2016
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
_video	equ 31
_audio	equ 32
_ilgins	equ 33
_sleep	equ 34
_msg    equ 35
_geterr equ 36
_rsrvd1	equ 37
_pri	equ 38
_rele 	equ 39

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

mov     esi, prg_msg
call	print_msg

xor	eax, eax
xor	ebx, ebx

; DIRECT VGA MEMORY ACCESS
;xor	ebx, ebx
mov	bh, 5 ; Direct access/map to VGA memory (0A0000h)
;mov	eax, _video ; 1Fh
mov	al, 1Fh ; sys _video ; TRDOS 386 Video functions
int	40h   ; TRDOS 386 system call

; eax = 0A0000h
and	eax, eax
jz      terminate ; error (eax = 0)

; ah = 0
mov	al, 13h ; set video mode to 13h 
;int	10h		; - VIDEO -
int	31h  ; TRDOS 386 - VIDEO Interrupt
;
;mov	ax, 1124h ; set (load) 8x16 VGA font
;mov	bl, 1	; 14 rows
;;int	10h  ; - VIDEO - TEXT-MODE CHARACTER GENERATOR FUNCTIONS
;int	31h  ; TRDOS 386 - VIDEO Interrupt

getchar:
mov	ah, 10h
int	32h

push	ax
mov	ah, 0Eh
mov	bx, 22h
int	31h	; write tty
pop	ax
cmp	al, 1Bh ; ESC key
jne     short getchar

terminate:
	sys 	_exit   ; INT 40h
here:
	jmp	short here

print_msg:
	mov	bx, 7
        mov     ah, 0Eh
pmsg_loop:
	lodsb
	and	al, al
	jz	short pmsg_ok
	int	31h	; TRDOS 386 video interrupt
	jmp	short pmsg_loop	
pmsg_ok:
	mov	ah, 10h ; Getchar
	int	32h	; TRDOS 386 keyboard interrupt
	retn

db 	0
prg_msg:
db	'ERDOGAN TAN - TRDOS 386 - VGA TELETYPE & SCROLL TEST'
db	0Dh, 0Ah
db	'09/08/2016'
db	0Dh, 0Ah
db 	0
dw	_end
db	0 
_end:
