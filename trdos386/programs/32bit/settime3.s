; ****************************************************************************
; settime.s (TRDOS 386, TRDOS v2.0.11 - sample binary file, 'settime.prg')
; ----------------------------------------------------------------------------
; SETTIME.PRG ! 'sysstime' TEST program for TRDOS 386 !
;
; 18/05/2026
;
; [ Last Modification: 18/05/2026 ]
;
; ****************************************************************************
; nasm settime3.s -l settime3.txt -o SETTIME3.PRG

; modified from "settime2.s" (14/05/2026) and "epoch2.s" (17/05/2026)

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
	cmp	eax, 2 ; two arguments (program file name & time)
	jb	short p_usage ; display usage message
	lodsd ; program file name address
	lodsd ; time string address

	; EAX = arg2 ; time (text)
	mov	esi, eax
	call	str_to_time
	jnc	short p_time

p_usage:
	mov	esi, usage
	call	print_msg

	jmp	terminate

p_time:
	mov	ecx, [hour]
	shl	ecx, 16
	mov	ch, [minute]
	mov	cl, [second]

	; Set Time in MSDOS format (bl = 1)
	sys	_stime, 1
	jc	short p_nextline

	; updated message
	mov	esi, updated
	call	print_msg

p_nextline:
	;mov	esi, newline
	;call	print_msg

terminate:
	sys	_exit, 0

;-----------------------------------------------------------------

hang:
	jmp	short hang

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

	; 17/05/2026 - Erdogan Tan ('epoch2.s')
str_to_time:
	; esi = time string (hh:mm:ss format)
	xor	eax, eax
	lodsb
	cmp	al, '0'
	jb	short _fail2
	cmp	al, '2'
	ja	short s2t1
	mov	cl, al
	sub	al, '0'
	mov	[hour], eax
	lodsb
	cmp	al, '0'
	jb	short _fail2
	cmp	al, '9'
	ja	short s2t1
	cmp	cl, '2'
	ja	short _fail2_stc
	jb	short s2t0
	cmp	al, '3'
	jna	short s2t0
_fail2_stc:
	stc
_fail2:
	retn
s2t0:
	mov	cl, al
	sub	cl, '0'
	mov	al, 10
	mul	byte [hour]
	mov	[hour], al
	add	[hour], cl
	lodsb
s2t1:
	cmp	al, ':'
	jne	short _fail2
s2t2:
	lodsb
	cmp	al, '0'
	jb	short _fail2
	cmp	al, '9'
	ja	short s2t4
	mov	cl, al
	sub	al, '0'
	mov	[minute], eax
	lodsb
	cmp	al, '0'
	jb	short _fail2
	cmp	al, '9'
	ja	short s2t4
	cmp	cl, '5'
	ja	short _fail2_stc
s2t3:
	mov	cl, al
	sub	cl, '0'
	mov	al, 10
	mul	byte [minute]
	mov	[minute], al
	add	[minute], cl
	lodsb
s2t4:
	cmp	al, ':'
	je	short s2t5

_fail3_stc:
	stc
_fail3:
	retn

s2t5:
	lodsb
	cmp	al, '0'
	jb	short _fail3
	cmp	al, '9'
	ja	short _fail3_stc
	mov	cl, al
	sub	al, '0'
	mov	[second], eax
	lodsb
	cmp	al, '0'
	jb	short s2t6
	cmp	al, '9'
	ja	short _fail3_stc
	cmp	cl, '5'
	ja	short _fail3_stc

	mov	cl, al
	sub	cl, '0'
	mov	al, 10
	mul	byte [second]
	mov	[second], al
	add	[second], cl
s2t5_@:
	lodsb
s2t6:
	and	al, al
	jnz	short _fail3_stc

	; [hour] = 0-23
	; [minute] = 0-59
	; [second] = 0-59

	retn

;-----------------------------------------------------------------
; data
;-----------------------------------------------------------------

usage:
	db "TRDOS 386 v2 'sysstime' system call test program",0Dh,0Ah
	db "by Erdogan Tan [May 2026]",0Dh,0Ah
	db 0Dh,0Ah
	db "Usage: settime hh:mm:ss",0Dh,0Ah,0 

updated:
	db 0Dh,0Ah
	db "OK ...",0Dh,0Ah,0

align 4

hour:	dd 0
minute: dd 0
second: dd 0
