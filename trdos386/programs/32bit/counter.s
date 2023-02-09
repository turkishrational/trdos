; ****************************************************************************
; counter.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'counter.s'
; ----------------------------------------------------------------------------
; COUNTER.PRG ! TEST program !
; TRDOS 386 timer interrupt & callback function test program (by Erdogan Tan)
;
; 19/12/2016
;
; [ Last Modification: 02/01/2017 ]
;
; ****************************************************************************

[BITS 32]
        mov     esi, prg_msg
	call	print_msg

	call	getch

	cmp	al, 1Bh  ; ESC key
	je	terminate ; short olacak !	

	mov	word [color], 22h

	; start (Real Time Clock) timer function
	;;mov	bl, 0
        ;;mov	bh, 84h	 ; Current Timer setup, Callback method 	
        mov	bx, 8400h
	;mov	ecx, 1	 ; 1 tick 
	;mov	bl, 0FFh
	;mov	bh, 4
	mov	cl, 1
	mov	edx, t_callback ; timer callback service address

	;mov	edx, t_event
	mov	eax, 33	; 'systimer'
	int	40h	; TRDOS 386 system call
	jc	terminate

	mov	[timer_event_number], al 

	mov 	ah, 0   ; set video mode
	mov 	al, 13h ; 320x200 graphics mode (256 colors)
	int	31h     ; TRDOS 386 video interrupt
			; (IBM PC/AT ROMBIOS, INT 10h) 
pc_1:
	xor	bh, bh
	cmp	[timer_event_status], bh ; 0
	;cmp	byte [t_event], bh ; 0
	jna	short pc_2

	mov	[timer_event_status], bh ; 0
	;mov	byte [t_event], bh ; 0

	;call	t_callback

	; Set cursor position
	xor	dx, dx  ; row 0, column 0
	;xor	bh, bh  ; Video page 0
	mov	ah, 2	; set cursor position
	int	31h 	; TRDOS 386 video interrupt

	; Print Count
	mov	esi, counter_str
	call	print_msg
pc_2:
	; Check keyboard buffer
	mov	ah, 11h
	int 	32h ; TRDOS 386 keyboard interrupt
		    ; (IBM PC/AT ROMBIOS, INT 16h)			
	jz	short pc_1 ; keyboard buffer empty

	call	getch
	cmp	al, 1Bh  ; ESC key
	ja	short pc_1
	je	short terminate

	; Stop timer event
	mov	bl, [timer_event_number]
	mov	bh, 80h	; stop timer event
			; and cancel callback service	
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

;t_event:
;	db	0

t_callback:
	pushfd

	; save registers
	push	edi
	push	edx
	push	eax
	push	ecx
	
	inc	byte [timer_event_status]

	sub	edx, edx
	movzx	eax, word [tcount]
	mov	edi, counter_str+5
	mov	ecx, 10
_tcb1:
	div	ecx
	add	dl, '0'
	dec 	edi
	mov	[edi], dl
	sub	dl, dl 
	and	eax, eax
	jnz	short _tcb1
	
	mov	ecx, edi
	mov	edi, counter_str
	sub	ecx, edi
	jna	short _tcb3
_tcb2:
	mov	al, '0'
	rep	stosb	
_tcb3:
	inc	word [tcount]

	; restore registers
	pop	ecx
	pop	eax
	pop	edx
	pop	edi

	popfd

	retn

	;iret	; restore EIP, CS, E-FLAGS	
		; return to normal running code

getch:
	; Getchar by using keyboard interrupt
	mov	ah, 10h
	int 	32h ; TRDOS 386 keyboard interrupt
		    ; (IBM PC/AT ROMBIOS, INT 16h)			
	retn

print_msg:
	mov	ah, 0Eh
	mov	bx, [color]
pmsg_loop:
	lodsb
	and	al, al
	jz	short pmsg_ok
	int	31h	; TRDOS 386 video interrupt
	jmp	short pmsg_loop	
pmsg_ok:
	retn

; /// MESSAGE - DATA ///

prg_msg:
	db 0Dh, 0Ah, 07h
	db 'TRDOS 386 timer callback test program by Erdogan Tan [02/01/2017]'
	db 0Dh, 0Ah, 0Dh, 0Ah
        db '(Press any key to continue...)'
	db 0Dh, 0Ah
	db 0Dh, 0Ah, 0
color:
	dw 0Fh
timer_event_number:
	db 0
timer_event_status:
	db 0
tcount:
	dw 0

counter_str:
	db '99999'
	db 0