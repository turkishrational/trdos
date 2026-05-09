; ****************************************************************************
; ctime.s (TRDOS 386, TRDOS v2.0.11 - sample binary file, 'ctime.prg')
; ----------------------------------------------------------------------------
; CTIME.PRG ! Date & Time - 'systime' TEST program for TRDOS 386 !
;
; 09/05/2026
;
; [ Last Modification: 09/05/2026 ]
;
; ****************************************************************************
; nasm ctime1.s -l ctime1.txt -o CTIME1.PRG

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
	;mov	esi, esp
	;lodsd
	;cmp	eax, 2 ; two arguments (program file name & date)
	;jb	short terminate ; nothing to do
	;lodsd ; program file name address
	;lodsd ; text file name address

	; EAX = arg2 ; date (text)

	;push	eax

	;mov	esi, msg_program
	;call	print_msg

	;xor	ah, ah
	;int	32h

	;pop	eax

	; Get Date&Time in MSDOS (TRDOS 386) format (1980->1980)
	sys	_time, 1

	; EAX = Current System Time (RTC)
	;  AL = Second (DL in MSDOS)
	;  AH = Minute (CL in MSDOS)
	;  HW of EAX = Hour (CH in MSDOS)

	mov 	byte [second], al
	mov 	byte [minute], ah
	shr	eax, 16
	;mov 	byte [hour], al
	mov 	word [hour], ax

	mov	esi, header
	call	print_msg

	mov	edi, txt_time

	mov	eax, [hour]
	call	bin_to_str

	mov	byte [edi],':'
 	inc	edi

	mov	eax, [minute]
	call	bin_to_str

	mov	byte [edi],':'
 	inc	edi

	mov	eax, [second]
	call	bin_to_str

	mov	byte [edi],0

	mov	esi, txt_time
	call	print_msg

	mov	esi, newline
	call	print_msg

	sys	_exit, 0

;-----------------------------------------------------------------

hang:
	jmp	short hang

;-----------------------------------------------------------------

bin_to_str:
	mov	ebx, 2
	mov	ebp, esp
	mov	ecx, 10
bin2str_div:
	xor	edx, edx
	div	ecx
	add	dl, '0'
	push	edx
	cmp	eax, 0
	ja	short bin2str_div
pop_next:
	pop	eax
	or	ebx, ebx
	jz	short skip_stosb
	stosb
	dec	ebx
skip_stosb:
	cmp	esp, ebp
	jb	short pop_next
zero_prefix:
	or	ebx, ebx
	jz	short bin2str_ok
	mov	byte [edi-1],'0'
	stosb
	dec	ebx
	jmp	short zero_prefix
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
;  data
;-----------------------------------------------------------------

	 db 0
header:	 db 0Dh,0Ah
	 db "Current Time: ",0
newline: db 0Dh,0Ah,0

;-----------------------------------------------------------------
;  uninitialized data
;-----------------------------------------------------------------

bss:

ABSOLUTE bss

alignb 4

hour:	resd 1
minute: resd 1
second: resd 1

txt_time:
	resb 10
