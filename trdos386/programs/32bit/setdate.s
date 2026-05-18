; ****************************************************************************
; setdate.s (TRDOS 386, TRDOS v2.0.11 - sample binary file, 'setdate.prg')
; ----------------------------------------------------------------------------
; SETDATE.PRG ! 'sysstime' TEST program for TRDOS 386 !
;
; 18/05/2026
;
; [ Last Modification: 18/05/2026 ]
;
; ****************************************************************************
; nasm setdate.s -l setdate.txt -o SETDATE.PRG

; modified from "settime3.s" (14/05/2026) and "dow3.s" (07/05/2026)

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
	lodsd ; date string address

	; EAX = arg2 ; date (text)
	mov	esi, eax
	call	str_to_date
	jnc	short p_date

p_usage:
	mov	esi, usage
	call	print_msg

	jmp	terminate

p_date:
	mov	edx, [year]
	shl	edx, 16
	mov	dh, [month]
	mov	dl, [day]

	; Set Date in MSDOS format (bl = 2)
	sys	_stime, 2
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

	; 06/05/2026 - Erdogan Tan
str_to_date:
	; esi = date string (dd/mm/yyyy or dd-mm-yyyy format)
	xor	eax, eax
	lodsb
	cmp	al, '0'
	jb	short _fail
	cmp	al, '9'
	ja	short _fail_stc
	mov	cl, al
	sub	al, '0'
	mov	[day], eax
	lodsb
	cmp	al, '0'
	jb	short s2d1
	cmp	al, '9'
	ja	short _fail_stc
	cmp	cl, '3'
	ja	short _fail_stc
	jb	short s2d0
	cmp	al, '1'
	;ja	short _fail_stc
	jna	short s2d0
_fail_stc:
	stc
_fail:
	retn
s2d0:
	mov	cl, al
	sub	cl, '0'
	mov	al, 10
	mul	byte [day]
	mov	[day], al
	add	[day], cl
	lodsb
s2d1:
	cmp	al, '-'
	je	short s2d2
	cmp	al, '/'
	jne	short fail
s2d2:
	cmp	byte [day], 1
	jb	short _fail

	mov	[separator], al
	;
	lodsb
	cmp	al, '0'
	jb	short fail
	cmp	al, '9'
	ja	short fail_stc
	mov	cl, al
	sub	al, '0'
	mov	[month], eax
	lodsb
	cmp	al, '0'
	jb	short s2d4
	cmp	al, '9'
	ja	short fail_stc
	cmp	cl, '1'
	ja	short fail_stc
	jb	short s2d3
	cmp	al, '2'
	ja	short fail_stc
s2d3:
	mov	cl, al
	sub	cl, '0'
	mov	al, 10
	mul	byte [month]
	mov	[month], al
	add	[month], cl
	lodsb
s2d4:
	cmp	al, [separator]
	jne	short fail_stc
	;
	cmp	byte [month], 1
	jb	short fail

	mov	edx, mdays-1
	add	edx, [month]
	mov	cl, [day]
	cmp	cl, [edx]
	jna	short s2d5

fail_stc:
	stc
fail:
	retn

s2d5:
	lodsb
	cmp	al, '0'
	jb	short fail
	cmp	al, '9'
	ja	short fail_stc
	sub	al, '0'
	mov	[year], eax
	xor	ebx, ebx
	lodsb
	cmp	al, '0'
	jb	short s2d6
	cmp	al, '9'
	ja	short fail_stc
	sub	al, '0'
	mov	ecx, eax
	mov	al, 10
	mul	byte [year]
	mov	[year], al
	add	[year], cl
	inc	ebx
	lodsb
	cmp	al, '0'
	jb	short s2d6
	cmp	al, '9'
	ja	short fail_stc
	sub	al, '0'
	mov	ecx, eax
	mov	al, 10
	mul	dword [year]
	mov	[year], eax
	add	[year], ecx
	inc	ebx
	xor 	eax, eax
	lodsb
	cmp	al, '0'
	jb	short s2d6
	cmp	al, '9'
	ja	short fail_stc
	sub	al, '0'
	mov	ecx, eax
	mov	al, 10
	mul	dword [year]
	mov	[year], eax
	add	[year], ecx
	inc	ebx
	lodsb
s2d6:
	and	al, al
	jnz	short fail_stc
	;

	; dd/mm/yy to dd/mm/20yy
	; correction
	cmp	bl, 1
	jne	short s2d7

	add	dword [year], 2000
s2d7:
	cmp	byte [month], 2
	ja	short s2d9
	jb	short s2d8

	test	byte [year], 3
	jz	short s2d8

	cmp	byte [day], 29
s2d8:
	cmc
s2d9:
	; if cf = 0
	    ; [day] = day
	    ; [month] = month
	    ; [year] = year
	retn

;-----------------------------------------------------------------
; data
;-----------------------------------------------------------------

usage:
	db "TRDOS 386 v2 'sysstime' system call test program",0Dh,0Ah
	db "by Erdogan Tan [May 2026]",0Dh,0Ah
	db 0Dh,0Ah
	db "Usage: setdate dd/mm/yyyy",0Dh,0Ah,0

updated:
	db 0Dh,0Ah
	db "OK ...",0Dh,0Ah,0

mdays:	db 31,29,31,30,31,30,31,31,30,31,30,31

separator:
	db 0

align 4

year:	dd 0
month:	dd 0
day:	dd 0
