; ****************************************************************************
; clock2.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'clock2.s')
; ----------------------------------------------------------------------------
; CLOCK2.PRG ! TEST program !
; TRDOS 386 RTC interrupt & timer event test program (by Erdogan Tan)
;
; 02/06/2016
;
; [ Last Modification: 11/06/2016 ]
;
; ****************************************************************************
; clock.s (TRDOS 386, RTC interrupt test program, 31/05/2016)

[BITS 32]
        mov     esi, prg_msg
	call	print_msg

	mov 	ah, 0   ; set video mode
	mov 	al, 3   ; 80x25 text mode, CGA colors
	int	31h     ; TRDOS 386 video interrupt
			; (IBM PC/AT ROMBIOS, INT 10h) 

	; start (Real Time Clock) timer function
        mov     bl, 0FFh ; signal return (response) byte
        mov     bh, 3    ; 1 second (rtc interrupt) 
	mov	ecx, 1
	;mov	cl, 1
	mov	edx, timer_event ; signal return (response) address
	mov	eax, 33	; 'systimer'
	int	40h	; TRDOS 386 system call
	jc	terminate

	mov	[timer_event_number], al 

rtc_p1:
	; Set cursor position
	xor	dx, dx  ; row 0, column 0
	xor	bh, bh  ; Video page 0
	mov	ah, 2	; set cursor position
	int	31h 	; TRDOS 386 video interrupt

	; Print Real Time Clock content
	;
	mov	ah, 2	; read the time
	int	35h	; TRDOS 386 date&time interrupt
	jc	terminate

        mov     [time_second], dh
        ;mov    [time_minute], cl
        ;mov    [time_hour], ch
        mov     [time_minute], cx
	;
	mov	ah, 4	; read the date
	int	35h	; TRDOS 386 date&time interrupt
			; (IBM PC/AT ROMBIOS, INT 1Ah)
        jc      terminate

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

rtc_p2:
	cmp	byte [timer_event], 0FFh
	jne	short rtc_p3

	mov	byte [timer_event], 0
	jmp	rtc_p1

rtc_p3:
	; Check keyboard buffer
	mov	ah, 11h
	int 	32h ; TRDOS 386 keyboard interrupt
		    ; (IBM PC/AT ROMBIOS, INT 16h)			
	jz	short rtc_p2 ; keyboard buffer empty

	call	getch
	cmp	al, 1Bh
	ja	short rtc_p2
	je	short terminate

	; Stop timer event
	movzx	ebx, byte [timer_event_number]
		; bh = 0 -> stop timer event
	mov	eax, 33	; 'systimer'
	int	40h	; TRDOS 386 system call

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
	db 'TRDOS 386 RTC & timer event test program by Erdogan Tan [11/06/2016]'
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

date_day:     db 11h
date_month:   db 06h
date_year:    db 16h
date_century: db 20h

time_second:  db 99h
time_minute:  db 30h
time_hour:    db 02h

timer_event: db 0
timer_event_number: db 0

db 'ISTANBUL', 0