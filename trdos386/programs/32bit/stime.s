; ****************************************************************************
; stime.s (TRDOS 386, TRDOS v2.0.11 - sample binary file, 'stime.prg')
; ----------------------------------------------------------------------------
; STIME.PRG ! "Get/Set Time" - "systime" & 'sysstime' TEST program !
;
; 18/05/2026
;
; [ Last Modification: 18/05/2026 ]
;
; ****************************************************************************
; nasm stime.s -l stime.txt -o STIME.PRG

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
	call	show_time
	call	set_time
	
terminate:
	sys	_exit, 0

;-----------------------------------------------------------------

hang:
	jmp	short hang

;-----------------------------------------------------------------

show_time:
	; ref: trdosk3.s  - 19/12/2025
	;      'show_time:'
	;      (26/07/2022 - TRDOS 386 Kernel v2.0.5)

	;;mov	ah, 02h
	;;int	1Ah
	;	; RTC_20 ; GET RTC TIME
	;mov	ah, 02h
	;int	35h

	;mov	al, ch
	;call	bcd_to_ascii
	;mov	[Hour], ax

	;mov	al, cl
	;call	bcd_to_ascii
	;mov	[Minute], ax

	;mov	al, dh
	;call	bcd_to_ascii
	;mov	[Second], ax

	; 18/05/2026
	; Get Time in MSDOS (TRDOS 386) format
	sys	_time, 1

	mov	[second], al
	mov	[minute], ah
	shr	eax, 16
	;mov	[hour], eax

	;mov	eax, [hour]
	mov	edi, Hour
	call	bin_to_str

	mov	eax, [minute]
	mov	edi, Minute
	call	bin_to_str

	mov	eax, [second]
	mov	edi, Second
	call	bin_to_str

	mov	esi, Msg_Show_Time
	;call	print_msg
	;retn
	jmp	print_msg

set_time:
	; ref: trdosk3.s  - 19/12/2025
	;      'set_time:'
	;      (26/07/2022 - TRDOS 386 Kernel v2.0.5)

	mov 	esi, Msg_Enter_Time
	call	print_msg

loc_enter_hour_1:
	xor     ah, ah
	int	32h
	; AL = ASCII Code of the Character
	cmp	al, 13 ; ENTER key
        je	short loc_set_time_retn
	cmp	al, 27 ; ESC key
        je	short loc_set_time_retn
set_time_0:
	mov	[Hour], al
	cmp	al, '0'
        jb	short loc_set_time_stc_0
	cmp	al, '2'
	jna	short set_time_1

loc_set_time_stc_0:
	call	beeper ; BEEP !
	jmp	short loc_enter_hour_1

loc_set_time_stc_1:
	call	check_for_backspace
	je	short loc_set_time_bs_1
	call	beeper ; BEEP !
	jmp	short loc_enter_hour_2
loc_set_time_bs_1:
	call	write_backspace
	jmp	short loc_enter_hour_1

loc_set_time_retn:
	mov 	esi, nextline
	;call	print_msg
	;retn
	jmp	print_msg

set_time_1:
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	; _write_tty
	mov	ah, 0Eh
	int	31h
loc_enter_hour_2:
	xor     ah, ah
	;int	16h
	int	32h
	; AL = ASCII Code of the Character
	cmp	al, 27
	je	short loc_set_time_retn
	mov	[Hour+1], al
	cmp	al, '0'
	jb	short loc_set_time_stc_1
	cmp	al, '9'
	ja	short loc_set_time_stc_1
        cmp     byte [Hour], '2'
	jb	short pass_set_time_24
	cmp	al, '4'
	ja	short loc_set_time_stc_1
pass_set_time_24:
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	; _write_tty
	mov	ah, 0Eh
	int	31h
loc_enter_time_separator_1:
	sub    ah, ah ; 0
	;int	16h
	int	32h
	; AL = ASCII Code of the Character
	cmp	al, 27
	je	short loc_set_time_retn
	cmp	al, ':'
	je	short set_time_2

loc_set_time_stc_2:
	call	check_for_backspace
	je	short loc_set_time_bs_2
	call	beeper ; BEEP !
	jmp	short loc_enter_time_separator_1
loc_set_time_bs_2:
	call	write_backspace
	jmp	short loc_enter_hour_2

set_time_2:
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	; _write_tty
	mov	ah, 0Eh
	int	31h
loc_enter_minute_1:
	xor     ah, ah
	int	32h
	; AL = ASCII Code of the Character
	cmp	al, 27
	je	short loc_set_time_retn
	mov	[Minute], al
	cmp	al, '0'
	jb	short loc_set_time_stc_3
	cmp	al, '5'
	jna	short set_time_3

loc_set_time_stc_3:
	call	check_for_backspace
	je	short loc_set_time_bs_3
	call	beeper ; BEEP !
	jmp	short loc_enter_minute_1
loc_set_time_bs_3:
	call	write_backspace
	jmp	short loc_enter_time_separator_1

set_time_3:
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	; _write_tty
	mov	ah, 0Eh
	int	31h
loc_enter_minute_2:
	xor     ah, ah
	int	32h
	; AL = ASCII Code of the Character
	cmp	al, 27
	jne	short loc_enter_minute_3
	jmp	loc_set_time_retn
loc_enter_minute_3:
	mov	[Minute+1], al
	cmp	al, '0'
	jb	short loc_set_time_stc_4
	cmp	al, '9'
	jna	short set_time_4

loc_set_time_stc_4:
	call	check_for_backspace
	je	short loc_set_time_bs_4
	call	beeper ; BEEP !
	jmp	short loc_enter_minute_2
loc_set_time_bs_4:
	call	write_backspace
	jmp	short loc_enter_minute_1

set_time_4:
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	; _write_tty
	mov	ah, 0Eh
	int	31h
loc_enter_time_separator_2:
	mov	word [Second], 3030h
	sub     ah, ah
	int	32h
	; AL = ASCII Code of the Character
	cmp	al, 13
	jne	short loc_enter_time_separator_3
jmp_loc_set_time_progress:
	jmp	loc_set_time_progress
loc_enter_time_separator_3:
	cmp	al, 27
	jne	short loc_enter_time_separator_4
	jmp	loc_set_time_ok
loc_enter_time_separator_4:
	cmp	al, ':'
        jne	short loc_set_time_stc_5

	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	; _write_tty
	mov	ah, 0Eh
	int	31h
loc_enter_second_1:
	xor     ah, ah
	int	32h
	; AL = ASCII Code of the Character
	cmp	al, 13
	je	short jmp_loc_set_time_progress
	cmp	al, 27
	jne	short loc_enter_second_2
	jmp	loc_set_time_ok
loc_enter_second_2:
	mov	[Second], al
	cmp	al, '0'
	jb	short loc_set_time_stc_6
	cmp	al, '5'
	ja	short loc_set_time_stc_6
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	; _write_tty
	mov	ah, 0Eh
	int	31h
loc_enter_second_3:
	xor     ah, ah
	int	32h
	; AL = ASCII Code of the Character
	cmp	al, 27
	je	short loc_set_time_ok
	cmp	al, '0'
        jb	short loc_set_time_stc_7
	cmp	al, '9'
        ja	short loc_set_time_stc_7
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	; _write_tty
	mov	ah, 0Eh
	int	31h
loc_set_time_get_lchar_again:
	sub	ah, ah ; 0
	int	32h
	; AL = ASCII Code of the Character
	cmp	al, 13
	je	short loc_set_time_progress
	cmp	al, 27
	je	short loc_set_time_ok
	;
	call	check_for_backspace
	jne	short loc_set_time_get_lchar_again

loc_set_time_bs_8:
	call	write_backspace
	jmp	short loc_enter_second_3

loc_set_time_stc_5:
	call	check_for_backspace
	je	short loc_set_time_bs_5
	call	beeper ; BEEP !
	jmp	loc_enter_time_separator_2
loc_set_time_bs_5:
	call	write_backspace
	jmp	loc_enter_minute_2

loc_set_time_ok:
	mov 	esi, nextline
	;call	print_msg
	;retn
	jmp	print_msg

loc_set_time_stc_6:
	call	check_for_backspace
	je	short loc_set_time_bs_6
	call	beeper ; BEEP !
	mov	word [Second], 3030h
	jmp	loc_enter_second_1
loc_set_time_bs_6:
	call	write_backspace
	jmp	loc_enter_time_separator_2
loc_set_time_stc_7:
	call	check_for_backspace
	je	short loc_set_time_bs_7
	call	beeper ; BEEP !
	jmp	loc_enter_second_3
loc_set_time_bs_7:
	call	write_backspace
	jmp	loc_enter_second_1

loc_set_time_progress:
	; Get Current Time
	;;mov	ah, 02h
	;;int	1Ah
		; RTC_20 ; GET RTC TIME
	mov	ah, 02h
	int	35h
	; DL = Daylight Savings Enable option (0-1)

	mov	ax, [Hour]
	sub	ax, '00'
	shl	al, 4 ; * 16
	mov	ch, al
	add	ch, ah
	mov	ax, [Minute]
	sub	ax, '00'
	shl	al, 4 ; * 16
	mov	cl, al
	add	cl, ah
	mov	ax, [Second]
	sub	ax, '00'
	shl	al, 4 ; * 16
	mov	dh, al
	add	dh, ah

	;mov	ah, 03h
	;int	1Ah
		; RTC_30 ; SET RTC TIME
	mov	ah, 03h
	int	35h

	jmp	loc_set_time_ok

;-----------------------------------------------------------------

write_backspace:
	mov	al, 08h ; BACKSPACE
	mov	ebx, 7 ; bl = attribute/color
		      ; bh = video page = 0
	; _write_tty
	mov	ah, 0Eh
	int	31h
	mov	al, 20h ; BLANK/SPACE char
	;mov	ebx, 7 ; attribute/color
	; _write_c_current
	mov	ah, 09h
	mov	ecx, 1
	int	31h
	retn
	
check_for_backspace:
	cmp	ax, 0E08h
	je	short cfbs_retn
	cmp	ax, 4BE0h
	je	short cfbs_retn
	cmp	ax, 4B00h
	je	short cfbs_retn
	cmp	ax, 53E0h
cfbs_retn:
	retn

;-----------------------------------------------------------------

	; 18/05/2026
	; 09/05/2026 (ref: 'ctime1.s')
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
	; 18/05/2026
	;dec	ebx
	;jmp	short zero_prefix
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

	; ref: TRDOS 386 v2.0 Kernel - 'video.s' (10/02/2026)
	;      'beeper:' (04/08/2016)
beeper: 
	mov	ecx, 1331 	; divisor for 886 hz tone
	;mov	bl, 31		; set count for 31/64 second for beep
	mov	ebx, 31

	sys	_audio		; bh = 0 -> Beep (PC Speaker)
	retn

;-----------------------------------------------------------------

;bcd_to_ascii:
;	; ref: TRDOS 386 v2.0 Kernel - 'trdos386.s' (17/05/2026)
;	;      'bcd_to_ascii:' (25/08/2014)
;	; input: al = Packed BCD number
;	; output: ax = ASCII word/number
;	
;	db	0D4h, 10h	; Undocumented inst. AAM
;				; AH = AL / 10h
;				; AL = AL MOD 10h
;	or	ax, '00'	; Make it ASCII based
;	xchg	ah, al
;	                             
;	retn
	
;-----------------------------------------------------------------
;
;-----------------------------------------------------------------

	; ref: 'trdosk9.s' (04/01/2016-17/05/2026)
Msg_Enter_Time:
	db 'Enter new time: '
	db 0
Msg_Show_Time:
	db 'Current time is '
Hour:	db '0'
	db '0'
	db ':'
Minute:	db '0'
	db '0'
	db ':'
Second:	db '0'
	db '0'
nextline:
	db 0Dh, 0Ah, 0

second:	dd 0
minute:	dd 0
;hour:	dd 0
