; ****************************************************************************
; dvmrw.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'dvmrw.prg')
; ----------------------------------------------------------------------------
; DVMRW.PRG ! TEST program !  TRDOS 386 Direct Video Memory Access test !
;
; 14/07/2016
;
; [ Last Modification: 29/07/2016 ]
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

; DIRECT VGA MEMORY ACCESS
;xor	ebx, ebx
mov	bh, 5 ; Direct access/map to VGA memory (0A0000h)
;mov	eax, _video ; 1Fh
mov	al, 1Fh ; sys _video ; TRDOS 386 Video functions
int	40h	 ; TRDOS 386 system call

; eax = 0A0000h
and	eax, eax
jz	short terminate ; error (eax = 0)

;call	read_font_bitmap

mov     esi, prg_msg
call	print_msg

; SET VIDEO MODE TO 13H
mov	ax, 13h  ; set video mode 
;mov	al, 13h	 ; 320x200 linear, 256 colors, 0A0000h
int	31h	 ; TRDOS 386 Video Interrupt

mov	al, 07h ; beep !
mov	ah, 0Eh ; tty write
mov	bx, 0
int	31h	; TRDOS 386 Video Interrupt

; Write rectangle pixels to VGA memory 
; (as it is mapped to user's memory space) 

mov	edi, 0A0000h
;mov	ah, 0FFh
;xor	eax, eax
mov	eax, 0F0F0F0F0h
mov	ecx, 5
L1:
push	ecx
mov	cl, 40
L2:
push	ecx
mov	cl, 4 ; 4*(40+40) = 320
L3:
push	ecx
mov	cl, 10
not	eax ; FFFFFFFFh
rep	stosd
mov	cl, 10
not	eax ; 00000000h
rep	stosd
pop	ecx
loop	L3
pop	ecx
loop	L2
not	eax ; FFFFFFFFh
pop	ecx
loop	L1
L4:

mov	ah, 0	; read char from keyboard buffer
int	32h	; TRDOS 386 - KEYBOARD Interrupt
cmp	al, 1Bh
je	short L5
cmp	al, 0Dh
je	short L5
cmp	al, 20h
je	short L5
mov	al, 07h ; beep !
mov	ah, 0Eh ; tty write
int	31h	; TRDOS 386 Video Interrupt
jmp	short L4 

L5:
;call	save_font_bitmap

mov	ax, 3	; set mode (to 80x25 color, text)
;mov	bx, 0	
int	31h	; TRDOS 386 Video Interrupt

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

read_font_bitmap:
	;in: edi=4k buffer
	;out: buffer filled with font
	;clear even/odd mode
	mov	esi, 0A0000h
	mov	edi, _end
	jmp	short load_save_bitmap
save_font_bitmap:
	;in: esi=4k buffer
	;out: fonts will be loadeds
        mov     esi, _end
	mov	edi, 0A0000h
load_save_bitmap:
	;in: edi=4k buffer
	;out: buffer filled with font
	;clear even/odd mode
	mov	dx, 03ceh
	mov	ax, 5
	call	_outw
	;map VGA memory to 0A0000h
	mov	ax, 0406h
	call	_outw
	;set bitplane 2
	mov	dx, 03c4h
	mov	ax, 0402h
	call	_outw
	;clear even/odd mode (the other way, don't ask why)
	mov	ax, 0604h
	call	_outw
	;copy charmap
        mov     ecx, 256
	;copy 16 bytes to bitmap
_@@:
	movsd
	movsd
	movsd
	movsd
	;skip another 16 bytes
	add	esi, 16
        loop    _@@
	;restore VGA state to normal operation
	mov	ax, 0302h
	call	_outw
	mov	ax, 0204h
	call	_outw
	mov	dx, 03ceh
	mov	ax, 1005h
	call	_outw
	mov	ax, 0E06h
	call	_outw
	retn

;_inb:
;	mov	ah, 0
;	int	34h
;	retn
;_outb:
;	mov	ah, 1
;	int	34h
;	retn	
;_inw:
;	push	ax
;	mov	ah, 2
;	int	34h
;	pop	ax
;	mov	al, ah
;	mov	ah, 2
;	int	34h
;	retn
_outw:
	push	ax
	mov	ah, 3
	int	34h
	pop	ax
	mov	al, ah
	mov	ah, 3
	int	34h
	retn	

db 	0
prg_msg:
db	'ERDOGAN TAN - TRDOS 386 - DVMRW.PRG'
db 	0Dh, 0Ah,
db	'29/07/2016'
db 	0
align 2
dw	_end
_end:
