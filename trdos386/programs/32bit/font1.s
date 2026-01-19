; ****************************************************************************
; font1.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'font1.prg')
; ----------------------------------------------------------------------------
; FONT1.PRG ! TEST program !  TRDOS 386 Video Function (font) test !
;
; 25/07/2016
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

; DIRECT CGA MEMORY ACCESS
;xor	ebx, ebx
mov	bh, 4 ; Direct access/map to CGA memory (0B8000h)
;mov	eax, _video ; 1Fh
mov	al, 1Fh ; sys _video ; TRDOS 386 Video functions
int	40h	; TRDOS 386 system call

; eax = 0B8000h
and	eax, eax
jz	terminate ; error (eax = 0)

mov     esi, prg_msg
call	print_msg

; set cursor position
mov	ah, 2
sub	dx, dx ; row 0, column 0
sub	bh, bh ; video page 0
int	31h

;xor	ebp, ebp

_0:
mov	ebx, color_for_page
mov	edi, 0B8000h
; Fill all pages with characters
_1:
mov	ah, [ebx]
call	full_page
inc	ebx
cmp	ebx, color_for_page + 8
jb	short _1

mov	ebx, 0B8000h
_2:
mov	edi, ebx
mov	esi, [msg_font]
call	write_name
mov	edi, ebx
add	edi, 4000 - (2*msg_vpg_len)		
mov	esi, msg_page
call	write_name
cmp	ebx, 0BFD00h
jnb	short _3
add	ebx, 4000
inc	byte [msg_pg_num]
jmp	short _2

_3:
mov	ah, 10h
int	32h

inc	ebp
mov	esi, msg_font
cmp	ebp, 1
ja	short _5
mov	ax, 1111h ; load and activate vga 8x14 font
mov	dword [esi], msg_vga_font_8_14
_4:
int	31h
mov	byte [msg_pg_num], '0'
jmp	short _0

_5:
cmp	ebp, 3
ja	short _7	
je	short _6
mov	ax, 1112h ; load and activate vga 8x8 font
mov	dword [esi], msg_vga_font_8_8
jmp	short _4

_6:
mov	ax, 1114h ; load and activate vga 8x16 font
mov	dword [esi], msg_vga_font_8_16
jmp	short _4

_7:
; clear video page 0
mov	ax, 0720h
mov	cx, 2000
mov	edi, 0B8000h
rep	stosw

terminate:
	sys 	_exit   ; INT 40h
here:
	jmp	short here

full_page:
	;mov	edi, 0B8000h
	;mov	ah, 4Eh
f_pg_0:
	push	ax
	mov	ax, 0720h
	mov	cx, 360
	rep	stosw
	pop	ax
	mov	dl, 5
f_pg_1:
	mov	ch, 1 ; cx = 256
	xor	al, al
f_pg_2:
	stosw	
	inc	al
	loop	f_pg_2
	dec	dl
	jnz	short f_pg_1
	;
	mov	ax, 720h
	mov	cx, 360
	rep	stosw
f_pg_3:
	retn

write_name:
	; esi = ASCIIZ string address
	; edi = destination address
	mov	ah, 07h
w_n1:
	lodsb
	and	al, al
	jz	short f_pg_3
	stosw
	jmp	short w_n1
	
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

color_for_page:
	db 0Fh, 0Ah, 0Bh, 0Ch, 0Dh, 0Eh, 09h, 07h 	

db 	0
msg_font:
dd	msg_current_font
msg_current_font:
db 	'CURRENT FONT'
db	0
msg_page:
db 	'VIDEO PAGE '
msg_pg_num:
db	'0'
msg_vpg_len	equ $ - msg_page
db	0
msg_vga_font_8_8:
db 	'VGA FONT 8x8'
db	0
msg_vga_font_8_14:
db 	'VGA FONT 8x14'
db	0
msg_vga_font_8_16:
db 	'VGA FONT 8x16'
db	0

prg_msg:
db	'ERDOGAN TAN - TRDOS 386 - VGA FONT Test Program - FONT1.PRG'
db 	0Dh, 0Ah,
db	'29/07/2016'
db	0Dh, 0Ah
db	'Press any key to continue...'
db 	0
align 2
dw	_end
_end: