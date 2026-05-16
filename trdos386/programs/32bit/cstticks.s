; ****************************************************************************
; cstticks.s (TRDOS 386, TRDOS v2.0.11 - sample binary file, 'cstticks.prg')
; ----------------------------------------------------------------------------
; CSTTICKS.PRG ! 'sysstime' TEST program for TRDOS 386 !
;
; 14/05/2026
;
; [ Last Modification: 14/05/2026 ]
;
; ****************************************************************************
; nasm cstticks.s -l cstticks.txt -o cstticks.PRG

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
	mov	esi, program
	call	print_msg
getchar:
	; TRDOS 386 keyboard input interrupt
	; (ROMBIOS INT 16h simulation)
	xor	ah, ah
	int	32h

	cmp	al, 27	; ESC (1Bh)
	je	short p_nextline
	cmp	al, 13	; ENTER/CR (0Dh)
	jne	short getchar
	
	; Convert/Save current time to/as
	; 18.2 Hz system timer ticks (bl = 5)
	sys	_stime, 5
	jc	short p_nextline

	; eax = (new/current) tick count
	mov	[tick_count], eax

	; print/display current tick count
	mov	esi, ticks_label
	call	print_msg

	mov	eax, [tick_count]
	mov	edi, ticks_txt
	call	bin_to_str
	mov	byte [edi], 20h ; space
	
	mov	esi, ticks_txt
	call	print_msg

	mov	eax, [tick_count]
	mov	edi, ticks_hex
	call	dword_to_hex
	
	mov	esi, ticks_hex_str
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

bin_to_str:
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

dword_to_hex:
	; eax = binary number
	; edi = hexadecimal string buffer (8 bytes)
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

DMonth:
	dw 0
	dw 31
	dw 59
	dw 90
	dw 120
	dw 151
	dw 181
	dw 212
	dw 243
	dw 273
	dw 304
	dw 334

;-----------------------------------------------------------------

program:
	db "TRDOS 386 v2 'sysstime' system call test program",0Dh,0Ah
	db "by Erdogan Tan [May 2026]",0Dh,0Ah
	db 0Dh,0Ah
confirm:
	db "Press ENTER to convert/save current time as tick count",0Dh,0Ah
	db "or press ESC to cancel"
newline:
	db 0Dh,0Ah,0

ticks_label:
	db 0Dh,0Ah
	db "Current Tick Count: ",0
ticks_hex_str:
	db "("
ticks_hex:
	dd 30303030h
	dd 30303030h
	db "h)", 0Dh, 0Ah, 0

;-----------------------------------------------------------------
; uninitialized data
;-----------------------------------------------------------------

bss:

ABSOLUTE bss

alignb 4

tick_count:
	resd 1

ticks_txt:
	resb 10
