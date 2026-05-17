; ****************************************************************************
; epoch1.s (TRDOS 386, TRDOS v2.0.11 - sample binary file, 'epoch1.prg')
; ----------------------------------------------------------------------------
; EPOCH1.PRG ! 'sysstime' TEST program for TRDOS 386 !
;
; 17/05/2026
;
; [ Last Modification: 17/05/2026 ]
;
; ****************************************************************************
; nasm epoch1.s -l epoch1.txt -o EPOCH1.PRG

; 30/04/2026
; 14/07/2020
; 31/12/2017
; TRDOS 386 (v2.0) system calls
_ver 	equ 0
_exit 	equ 1
_fork 	equ 2
_read 	equ 3
_write	equ 4
_open	equ 5
_close 	equ 6
_wait 	equ 7
_create	equ 8
_rename	equ 9
_delete	equ 10
_exec	equ 11
_chdir	equ 12
_time 	equ 13
_mkdir 	equ 14
_chmod	equ 15
_rmdir	equ 16
_break	equ 17
_drive	equ 18
_seek	equ 19
_tell 	equ 20
_memory	equ 21
_prompt	equ 22
_path	equ 23
_env	equ 24
_stime	equ 25
_quit	equ 26
_intr	equ 27
_dir	equ 28
_emt 	equ 29
_ldrvt 	equ 30
_video 	equ 31
_audio	equ 32
_timer	equ 33
_sleep	equ 34
_msg    equ 35
_geterr	equ 36
_fpstat	equ 37
_pri	equ 38
_rele	equ 39
_fff	equ 40
_fnf	equ 41
_alloc	equ 42
_dalloc equ 43
_calbac equ 44
_dma	equ 45
_stdio	equ 46

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

[BITS 32] ; 32-bit intructions

[ORG 0] 

START_CODE:
	mov	esi, esp
	lodsd
	cmp	eax, 2 ; two arguments (program file name & date)
	jb	short p_usage ; display usage message
	lodsd ; program file name address
	lodsd ; epoch string address

	; EAX = arg2 ; epoch (text)
	mov	esi, eax
	call	str_to_epoch
	jnc	short p_date

p_usage:
	mov	esi, usage
	call	print_msg

	jmp	terminate

p_date:
	; EAX = Unix (Epoch) Time Ticks/Seconds
	push	eax
	mov	edi, epoch_hex
	call	dword_to_hex
	mov	esi, epoch_txt
	call	print_msg
	pop	ecx

	; convert unix epoch time to msdos packed
	; date&time dword (without saving)
	sys	_stime, 9

	mov	[datetime], eax

	mov	edi, packed_hex
	call	dword_to_hex

	mov	esi, packed_txt
	call	print_msg

	; convert msdos packed date&time dword
	; to unix epoch format (without saving)
	sys	_stime, 8, [datetime]
	
	; eax = unix epoch time (seconds)
	push	eax
	mov	edi, epoch_hex
	call	dword_to_hex
	mov	esi, epoch_txt
	call	print_msg
	pop	ecx
	
	; convert unix epoch time to msdos
	; (trdos386) date&time (without saving)
	sys	_stime, 7

	; eax = time
	; edx = date

	mov	[second], al
	mov	[minute], ah
	shr	eax, 16
	mov	[hour], eax

	mov	[day], dl
	mov	[month], dh
	shr	edx, 16
	mov	[year], edx

	mov	esi, date_time_txt
	call	print_msg

	mov	edi, txt_date

	mov	eax, [day]
	call	bin_to_str_2

	mov	byte [edi],'/'
 	inc	edi

	mov	eax, [month]
	call	bin_to_str_2

	mov	byte [edi],'/'
 	inc	edi

	mov	eax, [year]
	call	bin_to_str_4

	mov	byte [edi],0

	mov	esi, txt_date
	call	print_msg

	mov	esi, _space
	call	print_msg

	mov	edi, txt_time

	mov	eax, [hour]
	call	bin_to_str_2

	mov	byte [edi],':'
 	inc	edi

	mov	eax, [minute]
	call	bin_to_str_2

	mov	byte [edi],':'
 	inc	edi

	mov	eax, [second]
	call	bin_to_str_2

	mov	byte [edi],0

	mov	esi, txt_time
	call	print_msg

p_nextline:
	mov	esi, newline
	call	print_msg

terminate:
	sys	_exit, 0

;-----------------------------------------------------------------

hang:
	jmp	short hang

;-----------------------------------------------------------------

	; 06/05/2026
bin_to_str_4:
	mov	esi, 4
	jmp	short bin_to_str
bin_to_str_2:
	mov	esi, 2
bin_to_str:
	xor	ecx, ecx ; 0
	mov	ebp, esp
	mov	ebx, 10
bin2str_div:
	xor	edx, edx
	div	ebx
	add	dl, '0'
	push	edx
	inc	ecx
	;cmp	eax, 0
	;ja	short bin2str_div
	and	eax, eax
	jnz	short bin2str_div

	mov	ebx, esi
	add	esi, edi
	sub	ebx, ecx
	jng	short skip_zero_prefix
	; eax = 0
	add	al,'0'
zero_prefix:
	stosb
	dec	ebx
	jnz	short zero_prefix

skip_zero_prefix:
pop_next:
	pop	eax
	cmp	edi, esi
	jnb	short skip_stosb
	stosb
skip_stosb:
	cmp	esp, ebp
	jb	short pop_next

bin2str_ok:
	retn

;-----------------------------------------------------------------

print_msg:
	mov	ebx, 0Fh       ; white characters (bl)
		               ; video page 0 (bh)
print_msg_@:
	mov	ah, 0Eh ; teletype output (write tty)
	lodsb
_p_nextchar:
	int	31h
	lodsb
	and	al, al
	jnz	short _p_nextchar
	retn

;-----------------------------------------------------------------

	; 07/05/2026 - Erdogan Tan
str_to_epoch:
	; esi = unix epoch -numeric- string (digits)
	xor	eax, eax
	mov	ecx, 10	; max. 10 digits
get_digit:
	lodsb
	cmp	al, '0'
	jb	short chk_eol
	cmp	al, '9'
	ja	short fail
	sub	al, '0'
	push	eax
	mov	eax, 10
	mul	dword [epoch]
	mov	[epoch], eax
	pop	eax
	add	dword [epoch], eax
	jc	short overflow
	loop	get_digit

	lodsb
chk_eol:
	and	al, al
	jnz	short fail

	mov	eax, [epoch]
	retn
fail:
	stc
overflow:
	retn

;-----------------------------------------------------------------

dword_to_hex:
	; eax = binary number
	mov	ebx, eax
	mov	ecx, 8
dd2hex:
	rol	ebx, 4
	mov	dl, bl
	and	edx, 15
	add	edx, hex_digits
	mov	al, [edx]
	stosb
	loop	dd2hex
	retn

;-----------------------------------------------------------------
; data
;-----------------------------------------------------------------

hex_digits:
	db '0123456789ABCDEF'

;-----------------------------------------------------------------

usage:
	db "TRDOS 386 v2 'sysstime' system call test program",0Dh,0Ah
	db "by Erdogan Tan [May 2026]",0Dh,0Ah
	db 0Dh,0Ah
	db "Usage: epoch1 unix_epoch_time_digits"
newline:
	db 0Dh,0Ah,0

epoch_txt:
	db 0Dh,0Ah
	db "Unix Epoch Time: "
epoch_hex:
	dd 0
	dd 0
	db "h"
	db 0Dh,0Ah,0

	db 0
packed_txt:
	db 0Dh,0Ah
	db "MSDOS/FAT Packed dword: "
packed_hex:
	dd 0
	dd 0
	db "h"
	db 0Dh,0Ah,0

date_time_txt:
	db 0Dh,0Ah
	db "Date&Time:"

_space:	db " ",0

align 4

year:	dd 0
month:	dd 0
day:	dd 0
hour:	dd 0
minute: dd 0
second: dd 0

epoch:	dd 0

;-----------------------------------------------------------------
; uninitialized data
;-----------------------------------------------------------------

bss:

ABSOLUTE bss

datetime:
	resd 1

txt_date:
	resb 12
txt_time:
	resb 10
