; ****************************************************************************
; clock.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'clock.s')
; ---------------------------------------------------------------------------
; CLOCK.PRG ! TEST program !
; TRDOS 386 date & time interrupt test program (by Erdogan Tan)
;
; 28/05/2016
;
; [ Last Modification: 31/05/2016 ]
;
; ****************************************************************************

[BITS 32]
        mov     esi, prg_msg
	call	print_msg

	mov 	ah, 0   ; set video mode
	mov 	al, 3   ; 80x25 text mode, CGA colors
	int	31h     ; TRDOS 386 video interrupt
			; (IBM PC/AT ROMBIOS, INT 10h) 
rtc_p_next:
	; Check keyboard buffer
	mov	ah, 11h
	int 	32h ; TRDOS 386 keyboard interrupt
		    ; (IBM PC/AT ROMBIOS, INT 16h)			
	jz	short rtc_p ; keyboard buffer empty

	call	getch
	cmp	al, 1Bh
	ja	short rtc_p

terminate:
	mov	eax, 1	; 'sysexit'
	int	40h	; TRDOS 386 system call
hang:
	nop
	nop
	nop
	jmp	short hang

bcd_to_ascii:
	; INPUT ->
	;	AL = Packed BCD number
	; OUTPUT ->
	;	AX = ASCII word/number
	;
	db	0D4h, 10h	; Undocumented inst. AAM
				; AH = AL / 10h
				; AL = AL MOD 10h
	or	ax, '00'	; Make it ASCII based
        xchg	ah, al 
	retn	

rtc_p:
	; Set cursor position
	xor	dx, dx  ; row 0, column 0
	xor	bh, bh  ; Video page 0
	mov	ah, 2	; set cursor position
	int	31h 	; TRDOS 386 video interrupt

	; Print Real Time Clock content
	;
	mov	ah, 2	; read the time
	int	35h	; TRDOS 386 date&time interrupt
	jc	short terminate

	cmp	dh, [time_second]
        je      short rtc_p_next
	
        mov     [time_second], dh
        ;mov    [time_minute], cl
        ;mov    [time_hour], ch
        mov     [time_minute], cx
	;
	mov	ah, 4	; read the date
	int	35h	; TRDOS 386 date&time interrupt
			; (IBM PC/AT ROMBIOS, INT 1Ah)
        jc      short terminate

	;mov	[date_day], dl
	;mov	[date_month], dh
	mov	[date_day], dx
	;mov	[date_year], cl
	;mov	[date_century], ch
	mov	[date_year], cx
	;
	mov	al, [date_century]
	call	bcd_to_ascii
	mov	[datestr+6], ax
	mov	al, byte [date_year]
	call	bcd_to_ascii
	mov	[datestr+8], ax
	mov	al, byte [date_month]
	call	bcd_to_ascii
	mov	[datestr+3], ax
	mov	al, byte [date_day]
	call	bcd_to_ascii
	mov	[datestr], ax
	;
        mov     al, byte [time_hour]
	call	bcd_to_ascii
	mov	[timestr], ax
        mov     al, byte [time_minute]
	call	bcd_to_ascii
	mov	[timestr+3], ax
        mov     al, byte [time_second]
	call	bcd_to_ascii
	mov	[timestr+6], ax
	;		
	mov	ebx, rtc_msg ; message offset
	mov	ecx, 255 ; message length 
	mov	edx, 0Fh ; white color
	mov	eax, 35  ; 'sysmsg'
	int	40h	 ; TRDOS 386 system call

	jmp	rtc_p_next  

getch:
	; Getchar by using keyboard interrupt
	mov	ah, 10h
	int 	32h ; TRDOS 386 keyboard interrupt
		    ; (IBM PC/AT ROMBIOS, INT 16h)			
	retn

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

; /// MESSAGE - DATA ///

prg_msg:
	db 0Dh, 0Ah, 07h
	db 'TRDOS 386 RTC interrupt test program by Erdogan Tan [31/05/2016]'
	db 0Dh, 0Ah, 0Dh, 0Ah
        db '(Press any key to continue...)'
	db 0Dh, 0Ah, 0

rtc_msg:
	db "Real Time Clock - "
datestr:
	db "00/00/0000"
	db "  "
timestr:	
        db "00:00:00"
	db " "
	db 0 

date_day:     db 30h
date_month:   db 05h
date_year:    db 16h
date_century: db 20h

time_second:  db 99h
time_minute:  db 23h
time_hour:    db 01h

db 0
db 'ISTANBUL'