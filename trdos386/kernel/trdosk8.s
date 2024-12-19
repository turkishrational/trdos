; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel - v2.0.9) - MAIN PROGRAM : trdosk8.s
; ----------------------------------------------------------------------------
; Last Update: 19/12/2024  (Previous: 05/06/2024)
; ----------------------------------------------------------------------------
; Beginning: 24/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ----------------------------------------------------------------------------
; Derived from 'Retro UNIX 386 Kernel - v0.2.1.0' source code by Erdogan Tan
; u0.s (20/11/2015), u4.s (14/10/2015)
; ****************************************************************************
; Derived from TRDOS Operating System v1.0 (8086) source code by Erdogan Tan
; TRDOS2.ASM (09/11/2011)
; ----------------------------------------------------------------------------
; DIR.ASM (c) 2004-2011 Erdogan TAN  [07/01/2004] Last Update: 09/10/2011

set_run_sequence:
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 23/12/2016
	; 10/06/2016
	; 22/05/2016
	; 20/05/2016
	; 19/05/2016 - TRDOS 386 (TRDOS v2.0)
	; TRDOS 386 feature only !
	;
	; INPUT ->
	;	AL = process number (next process)
	;
	;	this process must be added to run sequence
	;
	;	[u.pri] = priority of present process
	;
	;	DL = priority (queue)
	;	     0 = background (low) ; run on background 
	;	     1 = regular (normal) ; run as regular
	;	     2 = event (high)     ; run for event
	;
	;	1) If the requested process is already running:
	;	   a) If present priority is high ([u.pri]=2) 
	;	      and requested priority is also high, 
	;	      there is nothing to do! Because it has been
	;	      done already (before this attempt).
	;	   b) If present priority is high ([u.pri]=2)
	;	      and requested priority is not high, there is
	;	      nothing to do! Because, it's current
	;	      run queue is unspecified, here. (It may be in
	;	      a waiting list or in a run queue; if the new
	;	      priority would be used to add it to relavant
        ;             run queue, this would be wrong, unnecessary
	;	      and destabilizing duplication!)
	;	   c) If present priority is not high ([u.pri]<2)
        ;             and requested priority is high (event),
	;	      process will be added to present priority's
	;	      run queue and then, priority will be changed
	;	      to high ([u.pri]=2).
	;	   d) If present priority is not high ([u.pri]<2)
	;	      and requested priority is not high, [u.pri]
	;	      value will be changed. There is nothing to do
	;	      in addition. (The new priority value will be
	;	      used by 'tswap/tswitch' procedure at 'sysret'
	;	      or 'sysrele' stage.)
	;
	;	2) If the requested process is not running:
	;	   a) If requested priority of the requested
	;	      (next) process is high (event) and priority
	;	      of present process is not high, the requested
	;	      process will be added to ('runq_event') high
	;	      priority run queue and then present (running)
	;	      process will be stopped (swapped/switched out)
	;	      immediately if it is in user mode, or it's
	;	      [u.quant] value will be reset to 0 and (then)
	;	      it will be stopped at 'sysret' stage.
	;	   b) If requested priority of the requested
	;	      (next) process is high (event) and priority
	;	      of present process is also high, the requested
	;	      process will be added to ('runq_event') high
	;	      priority run queue and present (running)
	;	      process will be allowed to run until it's
	;	      time quantum will be elapsed ([u.quant]=0).
	;	   c) If requested priority of the requested
	;	      (next) process is not high ('run for event'),
	;	      there is nothing to do. Because, it's current
	;	      run queue is unspecified, here. (It may be in
	;	      a waiting list or in a run queue; if the new
	;	      priority would be used to add it to relavant
        ;             run queue, this would be wrong, unnecessary
	;	      and destabilizing duplication!)
	;
	; OUTPUT ->
	;	none
	;
	;	[u.pri] = priority of present process
	;
	;	cf = 1, if the request could not be fulfilled.
	;			 	     	  
	;	NOTE: 
        ;          * Processes in 'run as regular' queue can run
	;	     if there is no process in 'run for event' queue
	;	     ('run for event' processes have higher priority)
	;	   * When [u.quant] time quantum of a process is
	;	     elapsed, it's high priority ('run for event')
	;	     status will be disabled, it can be run in sequence
	;	     of it's actual run queue.
	;	   * A 'run on background' process will always be
	;	     sequenced in 'run on background' (low priority)
	;	     queue, it can run only when other priority queues
	;	     are empty. (idle time processes, e.g. printing)
	;
	; Modified registers: eax, ebx, edx
	;

srunseq_0:
        cmp     al, [u.uno]     ; same process ?
	jne	short srunseq_2 ; no

	mov	ah, [u.pri] 	; present/current priority
	cmp	ah, 2       	; 'run for event' priority level
	jb	short srunseq_6 ; no

srunseq_1:
	; there is nothing to do!
	retn

srunseq_2:
	;;this not necessary ! 23/12/2016
        ;;cmp	al, nproc	; number of processes = 16
	;;jnb	short srunseq_5	; error ! invalid process number

	; dl = priority
	cmp	dl, 2 		; event queue
	jb	short srunseq_1 ; requested process is not present
				; process and priority of requested
				; process is not high (event),
				; there is nothing to do!
	
	; requested process is not present process
	; & priority of requested process is high
	cmp	dl, [u.pri] 	; priority of present process
	jna	short srunseq_3 ; is high, also
	;
	; present process will be swapped/switched out
	inc	byte [p_change] ; 1

srunseq_3:
	; add process to 'runq_event' queue for new event
	mov	ebx, runq_event ; high priority run queue

srunseq_4:
	; al = process number
	; ebx = run queue
	;call	putlu
	;retn
	; 29/07/2022
	jmp	putlu

srunseq_5:
	cmc
	retn

srunseq_6:
	; present priority of the process is not high
	
	mov	[u.pri], dl ; new priority 
			    ; (will be used by 'tswap')

	cmp	dl, 2 		; high priority ?
	jb	short srunseq_5 ; no, there is nothing to do
				; in addition

	; process must be added to relevant run queue, here!
	; (new priority is high/event priority and process
	; will not be added to a run queue by 'tswap')

	mov	ebx, runq_normal ; 'run as regular' queue

	and	ah, ah  ;  previous value of [u.pri]
	jnz	short srunseq_4

	inc	ebx
	inc	ebx
	; ebx = runq_background ; 'run on backgroud' queue

	jmp	short srunseq_4
clock:
	; 21/08/2024 - TRDOS 386 v2.0.9
	; 23/05/2016
	; 22/05/2016
	; 20/05/2016
	; 19/05/2016 - TRDOS 386 (TRDOS v2.0)
	; 14/05/2015 - 14/10/2015 (Retro UNIX 386 v1)
	; 07/12/2013 - 10/04/2014 (Retro UNIX 8086 v1)

	;;;
	; 21/08/2024
	;cmp	word [u.quit], 0FFFFh
	cmp	byte [u.quit], 0FFh
	jb	short clk_0
	; CTRL+BRK keys pressed
	;cmp	word [u.intr], 0
	cmp	byte [u.intr], 0
	jna	short clk_0
	mov	byte [p_change], 0FFh ; -1 ; CTRL+BREAK sign
	test	byte [u.quant], 0FFh  ; if [u.quant] number > 0
	jnz	short clk_1 ; decrease [u.quant] number
	; [u.quant] = 0
	retn
	;;;
clk_0:
	cmp	byte [u.quant], 0
	ja	short clk_1
	;
	cmp     byte [u.uno], 1 ; /etc/init ? (for Retro UNIX 8086 & 386 v1)
				; MainProg (Kernel's Command Interpreter)
				; for TRDOS 386.
	jna	short clk_1 ; yes, do not swap out
	;
	cmp     byte [sysflg], 0FFh ; user or system space ?
	jne	short clk_2 	    ; system space (sysflg <> 0FFh)
	;
	; 21/08/2024
	;cmp	word [u.intr], 0
	;jna	short clk_2
	;
	; 23/05/2016
	cmp	byte [multi_tasking], 0
	jna	short clk_2

	; 21/08/2024
	;inc	byte [p_change] ; it is time to change running process
	mov	byte [p_change], 1
	retn
clk_1:
	dec	byte [u.quant]
clk_2:
	retn   ; return to (hardware) timer interrupt routine

; 12/10/2017
; 15/01/2017
; 14/01/2017
; 07/01/2017
; 02/01/2017
; 17/08/2016
; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
int34h: ; #IOCTL# (I/O port access support for ring 3)
	;
	; 23/05/2016
	; 20/06/2016
	; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
	;
	; INPUT ->
	;	AH = 0 -> read port  (physical IO port) -byte-
	;	AH = 1 -> write port (physical IO port) -byte-
	;		AL = data byte
	;	AH = 2 -> read port  (physical IO port) -word-
	;	AH = 3 -> write port (physical IO port) -word-
	;		BX = data word
	;	AH = 4 -> read port  (physical IO port) -dword-
	;	AH = 5 -> write port (physical IO port) -dword-
	;		EBX = data dword
	;	; 12/10/2017
	;	AH = 6 -> read port (physical IO port) twice -byte-
	;	AH = 7 -> write port (physical IO port) twice -byte-
	;		BX = data word	
	;
	;	DX = Port number (<= 0FFFFh)
	;
	; OUTPUT ->
	;	AL = data byte (in al, dx)
	;	AX = data word (in ax, dx)
	;	EAX = data dword (in eax, dx)
	;
	;	(ECX = actual TRANSFER COUNT for string functions)
	;
	;
	; Modified registers: EAX
	;

	;cmp	ah, 5
	;ja	short int34h_5 ; invalid function !

	; 12/10/2017
	cmp	ah, 7
	ja	short int34h_5 ; invalid function !

	;; 15/01/2017
	; 14/01/2017
	; 02/01/2017
	;;mov	byte [ss:intflg], 34h	; IOCTL interrupt
	sti
	
	;sti	; enable interrupts
	and	byte [esp+8], 11111110b	; clear carry bit of eflags register

	cmp	ah, 1
	jb	short int34h_0
	ja	short int34h_1

	out	dx, al
	;iretd
	jmp	short int34h_iret

int34h_0:
	in	al, dx
	;iretd
int34h_iret:
	;cli	; 07/01/2017
	;; 15/01/2017
	;;mov	byte [ss:intflg], 0 ; reset
	iretd 

int34h_1:
	test	ah, 1
	jnz	short int34h_3 ; out

	; in
	cmp	ah, 2
	ja	short int34h_2

	mov	ax, bx
	in	ax, dx
	;iretd
	jmp	short int34h_iret

int34h_2:
	cmp	ah, 4
	ja	short int34h_7	; 12/10/2017
	; ah = 4
	mov	eax, ebx
	in	eax, dx
	;iretd
	jmp	short int34h_iret

int34h_3:
	cmp	ah, 3
	ja	short int34h_4

	mov	ax, bx
	out	dx, ax
	;iretd
	jmp	short int34h_iret

int34h_4:
	cmp	ah, 5
	ja	short int34h_6	; 12/10/2017
	; ah = 5
	mov	eax, ebx
	out	dx, eax
	;iretd
	jmp	short int34h_iret

int34h_5:
	or	byte [esp+8], 1	; set carry bit of eflags register
	iretd

	; 12/10/2017
int34h_6:
	mov	ax, bx
	out	dx, al
	jmp	short $+2
	xchg	ah, al
	out	dx, al
	;xchg	al, ah
	;iretd
	jmp	short int34h_8
int34h_7:
	in	al, dx
	jmp	short $+2
	mov	ah, al
	in	al, dx
int34h_8:
	xchg	al, ah
	iretd

INT4Ah:
	; 24/01/2016
	; this procedure will be called by 'RTC_INT' (in 'timer.s')
	retn

; u0.s
; Retro UNIX 386 v1 Kernel (v0.2) - SYS0.INC
; Last Modification: 20/11/2015

com2_int:
	; 07/11/2015 
	; 24/10/2015
	; 23/10/2015
	; 14/03/2015 (Retro UNIX 386 v1 - Beginning)
	; 28/07/2014 (Retro UNIX 8086 v1)
	; < serial port 2 interrupt handler >
	;
	mov 	[esp], eax ; overwrite call return address
	;push	eax
	mov	ax, 9
	jmp	short comm_int
com1_int:
	; 07/11/2015
	; 24/10/2015
	mov 	[esp], eax ; overwrite call return address
	; 23/10/2015
	;push	eax
	mov	ax, 8
comm_int:
	; 20/11/2015
	; 18/11/2015
	; 17/11/2015
	; 16/11/2015
	; 09/11/2015
	; 08/11/2015
	; 07/11/2015
	; 06/11/2015 (serial4.asm, 'serial')
	; 01/11/2015
	; 26/10/2015
	; 23/10/2015
	push	ebx
	push	esi
	push	edi
	push 	ds
	push 	es
	; 18/11/2015
	mov	ebx, cr3
	push	ebx ; ****
	;
	push	ecx ; ***
	push	edx ; **
	;
	mov	ebx, KDATA
	mov	ds, bx
	mov	es, bx
	;
	mov	ecx, [k_page_dir]
	mov	cr3, ecx
	; 20/11/2015
	; Interrupt identification register
	mov	dx, 2FAh ; COM2
	;
	cmp 	al, 8 
	ja 	short com_i0
	;
	; 20/11/2015
	; 17/11/2015
	; 16/11/2015
	; 15/11/2015
	; 24/10/2015
	; 14/03/2015 (Retro UNIX 386 v1 - Beginning)
	; 28/07/2014 (Retro UNIX 8086 v1)
	; < serial port 1 interrupt handler >
	;
	inc	dh ; 3FAh ; COM1 Interrupt id. register
com_i0:
	;push	eax ; *
	; 07/11/2015
	mov 	byte [ccomport], al
	; 09/11/2015
	movzx	ebx, ax ; 8 or 9
	; 17/11/2015
 	; reset request for response status
	mov	[ebx+req_resp-8], ah ; 0
	;
	; 20/11/2015
	in	al, dx		; read interrupt id. register
	JMP	$+2	   	; I/O DELAY
	and	al, 4		; received data available?
	jz	short com_eoi	; (transmit. holding reg. empty)
	;
	; 20/11/2015
	sub	dl, 3FAh-3F8h	; data register (3F8h, 2F8h)
	in	al, dx     	; read character
	;JMP	$+2	   	; I/O DELAY
	; 08/11/2015
	; 07/11/2015
	mov	esi, ebx 
	mov	edi, ebx
	add 	esi, rchar - 8 ; points to last received char
	add	edi, schar - 8 ; points to last sent char
	mov	[esi], al ; received char (current char)
	; query
	and	al, al
	jnz	short com_i2
   	; response
	; 17/11/2015
	; set request for response status
        inc     byte [ebx+req_resp-8] ; 1
	;
	add	dx, 3FDh-3F8h	; (3FDh, 2FDh)
	in	al, dx	   	; read line status register
	JMP	$+2	   	; I/O DELAY
	and	al, 20h	   	; transmitter holding reg. empty?
	jz	short com_eoi 	; no
	mov 	al, 0FFh   	; response
	sub	dx, 3FDh-3F8h 	; data port (3F8h, 2F8h)
	out	dx, al	   	; send on serial port
	; 17/11/2015
	cmp 	byte [edi], 0   ; query ? (schar)
	jne 	short com_i1    ; no
	mov	[edi], al 	; 0FFh (responded)
com_i1:
	; 17/11/2015
	; reset request for response status (again)
        dec     byte [ebx+req_resp-8] ; 0 
	jmp	short com_eoi
com_i2:	
	; 08/11/2015
	cmp 	al, 0FFh	; (response ?)
	je	short com_i3	; (check for response signal)
	; 07/11/2015
	cmp	al, 04h	; EOT
	jne	short com_i4	
	; EOT = 04h (End of Transmit) - 'CTRL + D'
	;(an EOT char is supposed as a ctrl+brk from the terminal)
	; 08/11/2015
		; ptty -> tty 0 to 7 (pseudo screens)
	xchg	bl, [ptty]  ; tty number (8 or 9)
	call 	ctrlbrk
	xchg	[ptty], bl ; (restore ptty value and BL value)
	;mov	al, 04h ; EOT
	; 08/11/2015
	jmp	short com_i4	
com_i3:
	; 08/11/2015
	; If 0FFh has been received just after a query
	; (schar, ZERO), it is a response signal.
	; 17/11/2015
        cmp     byte [edi], 0 ; query ? (schar)
	ja	short com_i4 ; no
	; reset query status (schar)
	mov	[edi], al ; 0FFh
	inc	al ; 0
com_i4:
	; 27/07/2014
	; 09/07/2014
	shl	bl, 1
	add	ebx, ttychr
	; 23/07/2014 (always overwrite)
	;;cmp	word [ebx], 0
	;;ja	short com_eoi
	;
	mov	[ebx], ax   ; Save ascii code
			    ; scan code = 0
com_eoi:
	;mov	al, 20h
	;out	20h, al	   ; end of interrupt
	;
	; 07/11/2015
      	;pop	eax ; *
	mov	al, byte [ccomport] ; current COM port
	 ; al = tty number (8 or 9)
        call	wakeup
com_iret:
	; 23/10/2015
	pop	edx ; **
	pop	ecx ; ***
	; 18/11/2015
	;pop	eax ; ****
	;mov	cr3, eax
	;jmp	iiret
	jmp	iiretp

;iiretp: ; 01/09/2015
;	; 28/08/2015
;	pop	eax ; (*) page directory
;	mov	cr3, eax
;iiret:
;	; 22/08/2014
;	mov	al, 20h ; END OF INTERRUPT COMMAND TO 8259
;	out	20h, al	; 8259 PORT
;	;
;	pop	es
;	pop	ds
;	pop	edi
;	pop	esi
;	pop	ebx ; 29/08/2014
;	pop 	eax
;	iretd

; 21/11/2023
%if 0

sp_init:
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 07/11/2015
	; 29/10/2015
	; 26/10/2015
	; 23/10/2015
	; 29/06/2015
	; 14/03/2015 (Retro UNIX 386 v1 - 115200 baud)
	; 28/07/2014 (Retro UNIX 8086 v1 - 9600 baud)
	; Initialization of Serial Port Communication Parameters
	; (COM1 base port address = 3F8h, COM1 Interrupt = IRQ 4)
	; (COM2 base port address = 2F8h, COM1 Interrupt = IRQ 3)
	;
	; ((Modified registers: EAX, ECX, EDX, EBX))
	;
	; INPUT:  (29/06/2015)
	;	AL = 0 for COM1
	;	     1 for COM2
	;	AH = Communication parameters	
	;
	;  (*) Communication parameters (except BAUD RATE):
	;	Bit	4	3	2	1	0
	;		-PARITY--   STOP BIT  -WORD LENGTH-
	;  this one -->	00 = none    0 = 1 bit  11 = 8 bits
	;		01 = odd     1 = 2 bits	10 = 7 bits
	;		11 = even
	;  Baud rate setting bits: (29/06/2015)
	;		Retro UNIX 386 v1 feature only !
	;	Bit	7    6    5  | Baud rate
	;		------------------------
	;	value	0    0    0  | Default (Divisor = 1)
	;		0    0    1  | 9600 (12)
	;		0    1    0  | 19200 (6)
	;		0    1	  1  | 38400 (3)
	;		1    0	  0  | 14400 (8)
	;		1    0	  1  | 28800 (4)
	;		1    1    0  | 57600 (2)
	;		1    1    1  | 115200 (1)
	
	; References:	
	; (1) IBM PC-XT Model 286 BIOS Source Code
	;     RS232.ASM --- 10/06/1985 COMMUNICATIONS BIOS (RS232)
	; (2) Award BIOS 1999 - ATORGS.ASM
	; (3) http://wiki.osdev.org/Serial_Ports
	;
	; Set communication parameters for COM1 (= 03h)
	;
	mov	ebx, com1p		; COM1 parameters
	mov	dx, 3F8h		; COM1
	 ; 29/10/2015
	mov	cx, 301h  ; divisor = 1 (115200 baud)
	call	sp_i3	; call A4
	test	al, 80h
	jz	short sp_i0 ; OK..
		; Error !
	;mov	dx, 3F8h
	sub	dl, 5 ; 3FDh -> 3F8h
	mov	cx, 30Eh  ; divisor = 12 (9600 baud)
	call	sp_i3	; call A4
	test	al, 80h
	jnz	short sp_i1
sp_i0:
        ; (Note: Serial port interrupts will be disabled here...)
        ; (INT 14h initialization code disables interrupts.)
	;
	mov	byte [ebx], 0E3h ; 11100011b
	call	sp_i5 ; 29/06/2015
sp_i1:
	inc	ebx
	mov	dx, 2F8h		; COM2
	 ; 29/10/2015
	mov	cx, 301h  ; divisor = 1 (115200 baud)
	call	sp_i3	; call A4
	test	al, 80h
	jz	short sp_i2 ; OK..
		; Error !
	;mov	dx, 2F8h
	sub	dl, 5 ; 2FDh -> 2F8h
	mov	cx, 30Eh  ; divisor = 12 (9600 baud)
	call	sp_i3	; call A4
	test	al, 80h
	jnz	short sp_i7
sp_i2:
	mov	byte [ebx], 0E3h ; 11100011b
sp_i6:
	;; COM2 - enabling IRQ 3
	; 29/07/2022
	; 07/11/2015
	; 26/10/2015
	pushf
	cli
	;
	mov	dx, 2FCh   		; modem control register
	in	al, dx 	   		; read register
	JMP	$+2	   		; I/O DELAY
	or	al, 8      		; enable bit 3 (OUT2)
	out	dx, al     		; write back to register
	JMP	$+2	   		; I/O DELAY
	;mov	dx, 2F9h   		; interrupt enable register
	; 29/07/2022
	mov	dl, 0F9h
	in	al, dx     		; read register
	JMP	$+2	   		; I/O DELAY
	;or	al, 1      		; receiver data interrupt enable and
	or	al, 3	   		; transmitter empty interrupt enable
	out	dx, al 	   		; write back to register
	JMP	$+2        		; I/O DELAY
	in	al, 21h    		; read interrupt mask register
	JMP	$+2	   		; I/O DELAY
	and	al, 0F7h   		; enable IRQ 3 (COM2)
	out	21h, al    		; write back to register
	;
	; 23/10/2015
	mov 	eax, com2_int
	mov	[com2_irq3], eax
	; 26/10/2015
	popf	
sp_i7:
	retn

sp_i3:
;A4:  	;-----	INITIALIZE THE COMMUNICATIONS PORT
	; 28/10/2015
	inc	dl	; 3F9h (2F9h)	; 3F9h, COM1 Interrupt enable register 
	mov	al, 0
	out	dx, al			; disable serial port interrupt
	JMP	$+2			; I/O DELAY
	add	dl, 2 	; 3FBh (2FBh)	; COM1 Line control register (3FBh)
	mov	al, 80h			
	out	dx, al			; SET DLAB=1 ; divisor latch access bit
	;-----	SET BAUD RATE DIVISOR
	; 26/10/2015
	sub 	dl, 3   ; 3F8h (2F8h)	; register for least significant byte
					; of the divisor value
	mov	al, cl	; 1
	out	dx, al			; 1 = 115200 baud (Retro UNIX 386 v1)
					; 2 = 57600 baud
					; 3 = 38400 baud
					; 6 = 19200 baud
					; 12 = 9600 baud (Retro UNIX 8086 v1)
	JMP	$+2			; I/O DELAY
	sub	al, al
	inc	dl      ; 3F9h (2F9h)	; register for most significant byte
					; of the divisor value
	out	dx, al ; 0
	JMP	$+2			; I/O DELAY
	;	
	mov	al, ch ; 3		; 8 data bits, 1 stop bit, no parity
	;and	al, 1Fh ; Bits 0,1,2,3,4	
	add	dl, 2	; 3FBh (2FBh)	; Line control register
	out	dx, al
	JMP	$+2			; I/O DELAY
	; 29/10/2015
	dec 	dl 	; 3FAh (2FAh)	; FIFO Control register (16550/16750)
	xor	al, al			; 0
	out	dx, al			; Disable FIFOs (reset to 8250 mode)
	JMP	$+2	
sp_i4:
;A18:	;-----	COMM PORT STATUS ROUTINE
	; 29/06/2015 (line status after modem status)
	add	dl, 4	; 3FEh (2FEh)	; Modem status register
sp_i4s:
	in	al, dx			; GET MODEM CONTROL STATUS
	JMP	$+2			; I/O DELAY
	mov	ah, al			; PUT IN (AH) FOR RETURN
	dec	dl	; 3FDh (2FDh)	; POINT TO LINE STATUS REGISTER
					; dx = 3FDh for COM1, 2FDh for COM2
	in	al, dx			; GET LINE CONTROL STATUS
	; AL = Line status, AH = Modem status
	retn

sp_status:
	; 29/06/2015
	; 27/06/2015 (Retro UNIX 386 v1)
	; Get serial port status
	mov	dx, 3FEh		; Modem status register (COM1)
	sub	dh, al			; dh = 2 for COM2 (al = 1)
					; dx = 2FEh for COM2
	jmp	short sp_i4s

sp_setp: ; Set serial port communication parameters
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 07/11/2015
	; 29/10/2015
	; 29/06/2015
	; Retro UNIX 386 v1 feature only !
	;
	; INPUT:
	;	AL = 0 for COM1
	;	     1 for COM2
	;	AH = Communication parameters (*)
	; OUTPUT:
	;	CL = Line status
	;	CH = Modem status
	;   If cf = 1 -> Error code in [u.error]
	;		 'invalid parameter !'
	;		 	 or
	;		 'device not ready !' error
	;	
	;  (*) Communication parameters (except BAUD RATE):
	;	Bit	4	3	2	1	0
	;		-PARITY--   STOP BIT  -WORD LENGTH-
	;  this one -->	00 = none    0 = 1 bit  11 = 8 bits
	;		01 = odd     1 = 2 bits	10 = 7 bits
	;		11 = even
	;  Baud rate setting bits: (29/06/2015)
	;		Retro UNIX 386 v1 feature only !
	;	Bit	7    6    5  | Baud rate
	;		------------------------
	;	value	0    0    0  | Default (Divisor = 1)
	;		0    0    1  | 9600 (12)
	;		0    1    0  | 19200 (6) 
	;		0    1	  1  | 38400 (3) 
	;		1    0	  0  | 14400 (8)
	;		1    0	  1  | 28800 (4)
	;		1    1    0  | 57600 (2)
	;		1    1    1  | 115200 (1) 
	;
	; (COM1 base port address = 3F8h, COM1 Interrupt = IRQ 4)
	; (COM2 base port address = 2F8h, COM1 Interrupt = IRQ 3)
	;
	; ((Modified registers: EAX, ECX, EDX, EBX))
	;
	mov	dx, 3F8h
	mov	ebx, com1p ; COM1 control byte offset
	cmp	al, 1
	ja 	short sp_invp_err
	jb	short sp_setp1 ;  COM1 (AL = 0)
	dec	dh ; 2F8h
	inc	ebx ; COM2 control byte offset
sp_setp1:
	; 29/10/2015
	mov	[ebx], ah
	movzx 	ecx, ah
	shr	cl, 5 ; -> baud rate index
	and	ah, 1Fh ; communication parameters except baud rate
	mov	al, [ecx+b_div_tbl]
	mov	cx, ax
	call	sp_i3
	mov	cx, ax ; CL = Line status, CH = Modem status
	test	al, 80h
	jz	short sp_setp2
        mov     byte [ebx], 0E3h ; Reset to initial value (11100011b)
stp_dnr_err:
	mov	dword [u.error], ERR_DEV_NOT_RDY ; 'device not ready !'
	; CL = Line status, CH = Modem status
	stc
	retn
sp_setp2:
	cmp	dh, 2 ; COM2 (2F?h)
	;jna	sp_i6
		      ; COM1 (3F?h)
	; 29/07/2022
	ja	short sp_i5
	jmp	sp_i6
sp_i5: 
	; 29/07/2022
	; 07/11/2015
	; 26/10/2015
	; 29/06/2015
	;
	;; COM1 - enabling IRQ 4
	pushf
	cli
	mov	dx, 3FCh   		; modem control register
	in	al, dx 	   		; read register
	JMP	$+2			; I/O DELAY
	or	al, 8      		; enable bit 3 (OUT2)
	out	dx, al     		; write back to register
	JMP	$+2			; I/O DELAY
	;mov	dx, 3F9h   		; interrupt enable register
	; 29/07/2022
	mov	dl, 0F9h
	in	al, dx     		; read register
	JMP	$+2			; I/O DELAY
	;or	al, 1      		; receiver data interrupt enable and
	or	al, 3	   		; transmitter empty interrupt enable
	out	dx, al 	   		; write back to register
	JMP	$+2        		; I/O DELAY
	in	al, 21h    		; read interrupt mask register
	JMP	$+2			; I/O DELAY
	and	al, 0EFh   		; enable IRQ 4 (COM1)
	out	21h, al    		; write back to register
	;
	; 23/10/2015
	mov 	eax, com1_int
	mov	[com1_irq4], eax
	; 26/10/2015
	popf
	retn

sp_invp_err:
	mov	dword [u.error], ERR_INV_PARAMETER ; 'invalid parameter !'
	xor	ecx, ecx
	dec	ecx ; 0FFFFh
	stc
	retn

; 29/10/2015
b_div_tbl: ; Baud rate divisor table (115200/divisor)
	db 1, 12, 6, 3, 8, 4, 1

%endif

; 23/10/2015
com1_irq4:
	dd dummy_retn
com2_irq3:
	dd dummy_retn

; 21/11/2023
dummy_retn:
	;retn
wakeup:
	; 24/01/2016
	retn

set_working_path_x:
		; 17/10/2016 (TRDOS 386 - FFF & FNF)
		;mov	ax, 1 
			; File name is needed/forced (AL=1)
			; Change directory as temporary (AH=0)
		; 29/07/2022
		xor	eax, eax
		inc	al
		; eax = 1
set_working_path_xx: ; 30/12/2017 (syschdir)
		; This is needed for preventing wrong Find Next File
		; system call after sysopen, syscreate, sysmkdir etc.
		; Find Next File must immediate follow Find First File)

		mov	[FFF_Valid], ah ; 0 ; reset ; 17/10/2016

set_working_path:
		; 08/08/2022
		; 29/07/2022 - TRDOS 386 Kernel v2.0.5
		; 16/10/2016
		; 12/10/2016
		; 10/10/2016
		; 05/10/2016 - TRDOS 386 (TRDOS v2.0)
		;
		; TRDOS v1.0 (DIR.ASM, "proc_set_working_path")
                ; 27/01/2011 - 08/02/2011 
		; Set/Changes current drive, directory and file
		; depending on command tail
		; (procedure is derivated from CMD_INTR.ASM 
		; file or dir locating code of internal commands)
		; (This procedure is prepared for INT 21H file/dir 
		; functions and also to get compact code for 
		; internal mainprog -command interpreter- commands)
		; 
		; INPUT: DS:SI -> Command tail (ASCIIZ string)
		; AL = 0 -> any, AL > 0 -> file name is forced
		; AH = CD -> Change directory permanently 
		; AH <> CD -> Change directory as temporary    
		; 
		; OUTPUT: ES=DS, FindFile structure has been set
		;        RUN_CDRV points previous current drive  
		;        DS:SI = FindFile structure address
		;        (DS=CS)       
		;        AX, BX, CX, DX, DI will be changed
		;   cf = 1 -> Error code in AX (AL)
		;        stc & AX = 0 -> Bad command or path name
		; -----------------------------------------------
		;
		; TRDOS 386 (05/10/2016)
		; INPUT:
		;	ESI = File/Directory Path (ASCIIZ string)
		;             address in user's memory space
		;       AL = 0 -> any
		;       AL > 0 -> file name is forced
		;       AH = CD -> change directory as permanent
		;       AH <> CD -> change directory as temporary
		; 
		; OUTPUT:
		;	FindFile structure has been set
		;       RUN_CDRV points previous current drive
		;       ESI = FindFile_Name address ; 12/10/2016
		;
		;       cf = 1 -> Error code in EAX (AL)
		;       stc & EAX = 0 -> Bad command or path name
		;  
		; Modified registers: EAX, EBX, ECX, EDX, ESI, EDI

		mov	[SWP_Mode], ax
		mov	al, [Current_Drv]
		xor	ah, ah
		mov	[SWP_DRV], ax

		; TRDOS 386 ring 3 (user's page directory)
		; to ring 0 (kernel's page directory)
		; transfer modifications (05/10/2016).

		push	ebp
		mov	ebp, esp
		
		;mov	ecx, 128 ; maximum path length = 128 bytes
		; 29/07/2022
		xor	ecx, ecx
		mov	cl, 128
		sub	esp, ecx ; reserve 128 bytes (buffer) on stack
		mov	edi, esp ; destination address (kernel space)
		; esi = source address (virtual, in user's memory space)
		call	transfer_from_user_buffer
		jc	short loc_swp_xor_retn 
		
		mov	esi, esp ; temporary buffer (the path) on stack
loc_swp_fchar:
		mov	al, [esi]
		cmp	al, 20h
		ja	short loc_swp_parse_path_name
		;je	short loc_swp_fchar_next
		; 29/07/2022
		jb	short loc_swp_xor_retn

loc_swp_fchar_next:
		inc	esi
		jmp	short loc_swp_fchar

loc_swp_xor_retn:
		xor	eax, eax
		stc
loc_swp_retn:
		mov	esp, ebp
		pop	ebp

		;mov	esi, FindFile_Drv
		mov	esi, FindFile_Name ; 12/10/2016
		retn 

;loc_swp_fchar_next:
;		inc	esi
;		jmp	short loc_swp_fchar  

loc_swp_parse_path_name:
		mov	edi, FindFile_Drv
		call	parse_path_name
		jc	short loc_swp_retn

loc_swp_checkfile_name:
		cmp	byte [SWP_Mode], 0
		jna	short loc_swp_drv

		; 10/10/2016 (valid file name checking)
		mov	esi, FindFile_Name
		cmp	byte [esi], 20h
		jna	short loc_swp_xor_retn

		; 16/10/2016
		mov	byte [SWP_inv_fname], 0 ; reset 
		; esi = file name address (ASCIIZ)
		call	check_filename
		jnc	short loc_swp_drv

		inc	byte [SWP_inv_fname] ; set
loc_swp_drv:
		mov	dh, [Current_Drv]
               ;mov	[RUN_CDRV], dh

		mov	dl, [FindFile_Drv]
               ;cmp	dl, dh
		cmp	dl, [Current_Drv]
		je	short loc_swp_change_directory

		inc	byte [SWP_DRV_chg]
		call	change_current_drive
		jc	short loc_swp_retn ; eax = error code
		; eax = 0

loc_swp_change_directory:
		cmp	byte [FindFile_Directory], 21h
		cmc
		jnc	short loc_swp_retn

		inc	byte [SWP_DRV_chg]
		inc	byte [Restore_CDIR]
		mov	esi, FindFile_Directory
		mov	ah, [SWP_Mode+1] 
		call	change_current_directory
		;jc	short loc_swp_retn ; eax = error code
		; 08/08/2022
		jnc	short loc_swp_change_prompt_dir_string
		jmp	loc_swp_retn	

loc_swp_change_prompt_dir_string:
		; esi = PATH_Array
		; eax = Current Directory First Cluster
		; edi = Logical DOS Drive Description Table
		call	change_prompt_dir_str 
		sub	eax, eax ; 0
		jmp	loc_swp_retn 

reset_working_path:
		; 06/10/2016 - TRDOS 386 (TRDOS v2.0)
		;
		; TRDOS v1.0 (DIR.ASM, "proc_reset_working_path")
		; 05/02/2011 - 08/02/2011
		;
		; Restores current drive and directory
		; 
		; INPUT: none
		; OUTPUT: DL = SWP_DRV, EAX = 0 -> OK
		;
		;    AX = 0 -> ESI = Logical Dos Drv Desc. Table
		;
		;    EAX, EBX, ECX, EDX, ESI, EDI will be changed
		;

  
		xor	eax, eax
		dec	eax 

		mov	dx, [SWP_DRV]
		or	dh, dh
		jz	short loc_rwp_return

		cmp	dl, [Current_Drv]
		je	short loc_rwp_restore_cdir
loc_rwp_restore_cdrv:
		call	change_current_drive 
		jmp	short loc_rwp_restore_ok
loc_rwp_restore_cdir:
		xor	ebx, ebx
		mov	bh, dl
		mov	esi, Logical_DOSDisks
		add	esi, ebx

		call	restore_current_directory

loc_rwp_restore_ok:
		mov	dx, [SWP_DRV]
		xor	eax, eax  
		mov	[SWP_DRV_chg], ax
loc_rwp_return:
		retn

get_file_name:
		; 25/08/2024 (TRDOS 386 Kernel v2.0.9)
		; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
		; 15/10/2016 - TRDOS 386 (TRDOS v2.0)
		; Convert file name 
		;	from directory entry format
                ; 	to (8.3) dot file name format
		;
		; TRDOS v1.0 (DIR.ASM, "get_file_name")
                ; 2005 - 09/10/2011
		; INPUT: 
		;	DS:SI -> Directory Entry Format File Name
		;       ES:DI -> DOS Dot File Name Address
		; OUTPUT:
		;	DS:SI -> DOS Dot File Name Address
                ;	ES:DI -> Directory Entry Format File Name
		;	
		; TRDOS 386 (15/10/2016)
		; INPUT:
		;	ESI = File name addr in dir entry format
		;	EDI = Dot file name address (destination)
		; OUTPUT: 
		;	File name is converted and moved
		;	to destination (as 8.3 dot filename)
		;  
		; Modified registers: EAX, ECX

                ; 2005 (TRDOS 8086) - 2016 (TRDOS 386)

		push	edi
		push	esi
		lodsb
		; 25/08/2024
		xor	ecx, ecx ; 0
		cmp	al, 20h
		jna	short pass_gfn_ext
		; 25/08/2024
		;push	esi
		stosb
		; 25/08/2024
		; 29/07/2022
		;xor	ecx, ecx
		; ecx <= 128 ; 25/08/2024
		mov	cl, 7
		; 25/08/2024
		add	esi, ecx ; add esi, 7
		push	esi ; (*)
loc_gfn_next_char:
		lodsb
		cmp	al, 20h
		jna	short pass_gfn_fn
		stosb
		loop	loc_gfn_next_char
pass_gfn_fn:
		;pop	esi
		;add	esi, 7
		; 25/08/2024
		pop	esi ; (*)

		lodsb
		cmp	al, 20h
		jna	short pass_gfn_ext
		mov	ah, '.'
		xchg	ah, al
		stosw
		lodsb
		cmp	al, 20h
		jna	short pass_gfn_ext
		stosb
		lodsb
		cmp	al, 20h
		jna	short pass_gfn_ext
		stosb
pass_gfn_ext:
		xor	al, al
		stosb
		pop	esi
		pop	edi
		; 25/08/2024
		; ecx <= 7
		retn

set_hardware_int_vector:
		; 18/03/2017
		; 03/03/2017
		; 28/02/2017 - TRDOS 386 (TRDOS v2.0)
		;
		; SET/RESET HARDWARE INTERRUPT GATE
		;
		; Changes interrupt gate descriptor table
		; (without changing default interrupt list)
		;
		; INPUT:
		;	AL = IRQ number (0 to 15)
		;	AH > 0 -> set
		;	AH = 0 -> reset
		;	
		; Modified registers: eax, ebx, edx, edi
		;
		
		shl	al, 2 ; IRQ number * 4
		movzx	ebx, al

		or	ah, ah
		jnz	short shintv_1 ; set (for user call service)
		
		; 18/03/2017
		add	ebx, IRQ_list ; reset to default interrupt list
		jmp	short shintv_2
shintv_1:
		add	ebx, IRQ_u_list
shintv_2:	
		mov	edx, [ebx] ; IRQ handler address
		
		; 03/03/2017
		shl	al, 1 ; IRQ number * 8 
		; 18/03/2017
		movzx	edi, al 
		add	edi, idt + (8*32) ; IRQ 0 offset = idt + 256
		
		mov	eax, edx ; IRQ handler address
		mov	ebx, 80000h

		;mov	edx, eax
		mov	dx, 8E00h
		mov	bx, ax
		mov	eax, ebx ; /* selector = 0x0008 = cs */
       			         ; /* interrupt gate - dpl=0, present */
		stosd	; selector & offset bits 0-15 	
		mov	[edi], edx ; attributes & offset bits 16-23

		retn
IRQ_u_list:
		; 28/02/2017
		dd	timer_int
		dd	kb_int
		dd	irq2
		dd	IRQ_service3
		dd	IRQ_service4
		dd	IRQ_service5
		dd	fdc_int	
		dd	IRQ_service7
		dd	rtc_int
		dd	IRQ_service9
		dd	IRQ_service10
		dd	IRQ_service11
		dd	IRQ_service12
		dd	IRQ_service13
		dd	hdc1_int
		dd	hdc2_int

		; 03/03/2017
		; 27/02/2017
IRQ_service3:
		mov	byte [ss:IRQnum], 3
		jmp	short IRQ_service
IRQ_service4:
		mov	byte [ss:IRQnum], 4
		jmp	short IRQ_service
IRQ_service5:
		mov	byte [ss:IRQnum], 5
		jmp	short IRQ_service
IRQ_service7:
		mov	byte [ss:IRQnum], 7
		jmp	short IRQ_service
IRQ_service9:
		mov	byte [ss:IRQnum], 9
		jmp	short IRQ_service
IRQ_service10:
		mov	byte [ss:IRQnum], 10
		jmp	short IRQ_service
IRQ_service11:
		mov	byte [ss:IRQnum], 11
		jmp	short IRQ_service
IRQ_service12:
		mov	byte [ss:IRQnum], 12
		jmp	short IRQ_service
IRQ_service13:
		mov	byte [ss:IRQnum], 13
		;jmp	short IRQ_service
IRQ_service:
		; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
		; 13/06/2017
		; 11/06/2017
		; 10/06/2017
		; 01/03/2017, 04/03/2017
		; 27/02/2017, 28/02/2017
		push	ds
		push	es
		push	fs
		push	gs

		pushad	; eax,ecx,edx,ebx,esp,ebp,esi,edi
		mov     cx, KDATA
        	mov     ds, cx
        	mov     es, cx
        	mov     fs, cx
        	mov     gs, cx

		mov	eax, cr3
		mov	[IRQ_cr3], eax

		mov	eax, [k_page_dir]
		mov	cr3, eax 

		mov	al, [IRQnum]

		;mov	cl, [sysflg]
		;mov	[u.r_mode], cl  ; system (0) or user mode (FFh) 
IRQsrv_0:
		movzx	ebx, al
		mov	bl, [ebx+IRQenum] ; IRQ (available) index number + 1
		; 01/03/2017
		dec	bl  ; IRQ index number, 0 to 8
		;js	IRQsrv_5 ; not available to use here!?
		; 29/07/2022
		js	short IRQsrv_j5  ; (jump to IRQsrv_5)
		;		 
		cmp	byte [ebx+IRQ.method], 80h ; using by a dev or kernel? 
		jb	short IRQsrv_1 ; no

		; If the IRQ service is already owned by TRDOS 386 kernel
		;	 or a Device driver
		; we need to call 'dev_IRQ_service'

		; IRQ number in AL
		call	dev_IRQ_service	 ; IRQ service for device drivers
		; IRQ number in AL
IRQsrv_1:		
		; check user callback service status
		; AL = IRQ number
		; EBX = IRQ (Available) Index number

		mov	[u.irqwait], al ; set waiting IRQ flag

		mov	al, [ebx+IRQ.owner]
		and	al, al
		;jz	IRQsrv_5 ; it is not owned by a user/proc
		; 29/07/2022
		jz	short IRQsrv_j5  ; (jump to IRQsrv_5)

		; 03/03/2017
		mov	edx, ebx
		shl	dl, 2
		mov	edx, [edx+IRQ.addr] ; S.R.B. or Callback service addr
		
		mov	ah, [ebx+IRQ.method]
		test	ah, 1
		jnz	short IRQsrv_4 ; Callback service method

		; Signal Response Byte method
		;mov	edx, [edx+IRQ.addr] ; Signal Response Byte address
		;			    ; (Physical address, non-swappable)
		and	ah, 2 ; bit 1, (S.R.B.) counter (auto increment) method
		mov	ah, [ebx+IRQ.srb] ; Signal Response Byte value
		jz	short IRQsrv_2 ; fixed S.R.B. value
		; counter method (auto increment)
		inc	ah
		mov	[ebx+IRQ.srb], ah ; next (count) number
IRQsrv_2:
		mov	[edx], ah ; put S.R.B. val to the user's S.R.B. addr
		mov	byte [u.irqwait], 0 ; clear waiting IRQ flag

		cmp	al, [u.uno]
		;je	IRQsrv_5 ; the owner is current user/process
		; 29/07/2022
		je	short IRQsrv_j5  ; (jump to IRQsrv_5)
IRQsrv_3:
		; the owner is not current user/process
		; AL = process number
		mov	dl, 2 ; priority, 2 = event (high)
		call	set_run_sequence

		; [u.irqwait] = waiting IRQ number for callback service
IRQsrv_j5:		; 29/07/2022
		jmp	IRQsrv_5
IRQsrv_4:
		cmp	al, [u.uno]  ; is the owner is current user/process?
		jne	short IRQsrv_3 ; no !

		; Check if an IRQ callback service already in progress
		cmp	byte [u.r_lock], 0
		;ja	IRQsrv_5 ; nothing to do !  
				     ; (we need to complete prev callback)
		; 29/07/2022
		ja	short IRQsrv_j5  ; (jump to IRQsrv_5)

		cmp	byte [u.t_lock], 0
		ja	short IRQsrv_5 ; nothing to do !  
				     ; (we need to complete timer callback)

		; 04/03/2017
		mov	byte [u.irqwait], 0 ; reset/clear waiting IRQ flag

		inc	byte [u.r_lock] ; 'IRQ callback service in progress' flag

		mov	cl, [sysflg]   ; (system call) mode flag (kernel/user)
		mov	[u.r_mode], cl ; system mode (0) or user mode (FFh)

		; 
		mov	ebp, [tss.esp0] ; kernel stack address (for ring 0)
		sub	ebp, 20		; eip, cs, eflags, esp, ss
	 	mov	[u.sp], ebp
		mov	[u.usp], esp

		;or	word [ebp+8], 200h ; 22/01/2017, force enabling interrupts

		mov	eax, [esp+28] ; pushed eax
		mov	[u.r0], eax

		call	wswap ; save user's registers & status

		; software int is in ring 0 but IRQ handler must return to ring 3
		; so, ring 3 return address and stack registers
		; (eip, cs, eflags, esp, ss) 
		; must be copied to IRQ handler return
		; eip will be replaced by callback service routine address

		mov	byte [sysflg], 0FFh ; user mode

		; system mode (system call)
		;mov	ebp, [u.sp] ; EIP (u), CS (UCODE), EFLAGS (u),
				    ; ESP (u), SS (UDATA)

		mov	eax, [ebp+16]	; SS (UDATA)
		mov	esi, esp
		push	eax
		push	eax
		mov	edi, esp
		mov	[u.usp], edi
		mov	ecx, ((ESPACE/4) - 4) ; except DS, ES, FS, GS
		rep	movsd
		mov	cl, 4	
		rep	stosd
		mov	[u.sp], edi
		mov	esi, ebp
		mov	cl, 5 ; EIP (u), CS (UCODE), EFLAGS (u), ESP (u), SS (UDATA)
		rep	movsd
		;

		mov	ecx, [u.pgdir]
		mov	[IRQ_cr3], ecx

set_IRQ_callback_addr:
		;
		; This routine sets return address
		; to start of user's interrupt
		; service (callback) address
		;
		; INPUT:
		;	EDX = callback routine/service address
		;	      (virtual, not physical address!)
		;	[u.sp] = kernel stack, points to
		;		 user's EIP,CS,EFLAGS,ESP,SS
		;		 registers.
		; OUTPUT:
		;	EIP (user) = callback (service) address
		;	CS (user) = UCODE
		;	EFLAGS (user) = flags before callback
		;       ESP (user) = ESP-4 (user, before callback)
		;	[ESP](user) = EIP (user) before callback
		;
		; Note: If CPU was in user mode while entering 
		;	the timer interrupt service routine,
		;	'IRET' will get return to callback routine
		;	immediately. If CPU was in system/kernel mode
		;	'iret' will get return to system call and
		;	then, callback routine will be return address
		;	from system call. (User's callback/service code
		;	will be able to return to normal return address
		;	via a 'sysrele' system call at the end.) 
		;
		; Note: User's IRQ callback service code must be ended
		;	with a 'sysrele' system call !
		;
		;	For example:
		;
		;	audio_IRQ_callback:
		;	    ...	 
		;	    <load DMA buffer with audio data>
		;	    ...
		;	    mov eax, 39 ; 'sysrele'
		;	    int 40h ; TRDOS 386 system call (interrupt)
		;
		
		;mov	edx, [edx+IRQ.addr] ; Callback service address
		;			    ; (Virtual address)
		
		mov	ebp, [u.sp]; kernel's stack, points to EIP (user)
		mov	[ebp], edx
IRQsrv_5:
		; EOI & return
		; 01/08/2020
		; 11/06/2017
		; 10/06/2017 
		;mov	al, [IRQnum]
		mov	al, 20h ; 01/08/2020
		cli
		;cmp	al, 7
		cmp	byte [IRQnum], 7 ; 01/08/2020
		jna	short IRQsrv_6
		;
		;;mov	al, EOI	; end of interrupt
		;mov	al, 20h ; 01/08/2020
		;cli		; disable interrupts till stack cleared
		;out	INTB00, al ; For controll2 #2
		out	0A0h, al
IRQsrv_6:
		;mov	byte [IRQnum], 0 ; reset
		;;mov	al, EOI	; end of interrupt
		;mov	al, 20h ; 01/08/2020
		;cli		; disable interrupts till stack cleared
		;out	INTA00, al ; end of interrupt to 8259 - 1
		out	20h, al	
IRQsrv_7:	
		;; 13/06/2017
		;or	word [ebp+8], 200h ; force enabling interrupts
		;
		mov 	ecx, [IRQ_cr3]	; previous content of cr3 register
 		mov	cr3, ecx	; restore cr3 register content
		;
		popad ; edi,esi,ebp,(icrement esp by 4),ebx,edx,ecx,eax
		;
		pop	gs
		pop	fs
		pop	es
		pop	ds
		;
		iretd	; return from interrupt

; 17/04/2021
; ('get_device_number' procedure is disabled as temporary)

;get_device_number:
;		; 08/10/2016
;		; 07/10/2016 - TRDOS 386 (TRDOS v2.0)
;		;
;		; This procedure compares name of requested
;		; device with kernel device names and
;		; installable device names. If names match, 
;		; the relevant device index (entry) number 
;		; will be returned the caller (sysopen) 
;		; for the requested device.
;		;
;		; NOTE: Installable device drivers must
;		; be loaded before using 'sysopen'
;		; (opendev) system call.
;		;
;		; INPUT:
;		;    ESI = device name address (ASCIIZ)
;		;         (in kernel's memory space)  
;  		;    max name length = 8 without '/dev/')
;		;    Device name will be capitalized 
;		;    and if there is, '/dev/' will be
;		;    removed from name before comparising)
;		;
;		; OUTPUT:
;		;    cf = 0 -> 
;		;      EAX (AL) = device entry/index number
;		;    cf = 1 -> device not found (installed)
;		;	       or invalid device name
;		;	       (AL=0)
;		;    device_name = device name address (asciiz)
;			;
;		; Modified registers: EAX, EBX, ESI, EDI
;
;		mov	edi, device_name
;		call 	lodsb_capitalize
;		mov	ah, al
;		cmp	al, '/'
;		jne	short gdn_1
;		mov	edi, device_name
;		call 	lodsb_capitalize
;gdn_0:
;		and	al, al ; 0 ?
;		jz	short gdn_err ; null name after '/'
;gdn_1:
;		cmp	al, 'D'
;		jne	short gdn_2
;		call 	lodsb_capitalize
;		cmp	al, 'E'
;		jne	short gdn_2
;		call 	lodsb_capitalize
;		cmp	al, 'V'
;		jne	short gdn_2
;		lodsb
;		cmp	al, '/'
;		je	short gdn_4
;gdn_2:
;		cmp	ah, '/'
;		jne	short gdn_5
;gdn_err:		
;		; invalid device name or device not found
;		xor	eax, eax ; 0
;		stc
;		retn
;gdn_3:
;		cmp	al, '/'
;		jne	short gdn_5
;gdn_4:
;		mov	edi, device_name
;		jmp	short gdn_6
;gdn_5:
;		cmp	al, 0
;		je	short gdn_7
;gdn_6:
;		call	lodsb_capitalize
;		cmp	edi, device_name + 8
;		jb	short gdn_3
;		cmp	al, 0
;		jne	short gdn_err
;		cmp	edi, device_name + 1
;		jna	short gdn_err ; null name after '/'
;gdn_7:
;		stosb
;		; zero padding ("NAME",0,0,0,0)
;		cmp	edi, device_name + 8
;		jb	short gdn_7
;gdn_8:
;		; search for kernel device names
;		mov	esi, device_name 
;		mov	edi, KDEV_NAME
;		xor	eax, eax
;gdn_9:
;		cmpsd	
;		jne	short gdn_10
;		cmpsd
;		jne	short gdn_11
;		jmp	short gdn_17 ; match
;gdn_10:
;		cmpsd  ; add esi, 4 & add edi, 4
;gdn_11:
;		mov	esi, device_name
;		inc	al
;		cmp	al, NumOfKernelDevNames
;		jb	short gdn_9
;gdn_12:
;		; search for installable device names
;		; esi = offset device_name 
;		mov	edi, IDEV_NAME
;		sub	al, al ; 0
;gdn_13:
;		cmpsd	
;		jne	short gdn_14
;		cmpsd
;		jne	short gdn_15
;		jmp	short gdn_19 ; match
;gdn_14:
;		cmpsd  ; add esi, 4 & add edi, 4
;gdn_15:
;		mov	esi, device_name
;		inc	al
;		cmp	al, NumOfInstallableDevices
;		jb	short gdn_13
;
;gdn_16: 	; error: invalid device name (not found) !
;		xor	al, al
;		stc
;		retn
;
;gdn_17:	; name match (with one of kernel device names)
;		;
;		; convert KDEV_NAME index to 
;		; KDEV_NUMBER index
;		; (different names are used for same devices)
;		; (example: "COM1" & "TTY8" = device number 18)
;		mov	ebx, eax ; < 256
;		mov	al, [KDEV_NUMBER+ebx]
;
;		; check if empty dev entry in the list
;		cmp	byte [DEV_OPENMODE+eax], 0
;		ja	short gdn_18 ; it must be already set
;
;		; (re)set device name and access flags
;		; (remain open work will be easy after that)
;		; (NOTE: here, data will be copied to bss section)
;		mov	bl, al
;		sub	edi, 8 ; kernel device name address (data)
;		shl	bx, 2 
;		mov	[DEV_NAME_PTR+ebx], edi ; (all) device names
;		mov	bl, [KDEV_ACCESS+eax] ; kernel dev list (data)
;		mov	[DEV_ACCESS+eax], bl ; (all) device list (bss)
;gdn_18:
;		inc	al ; 1 to NumOfKernelDevNames (<=7Fh)
;		; eax = device index/entry number
;		retn		
;
;gdn_19:	; name match (with one of installable device names)
;		;
;		; al = 0 to NumOfInstallableDevices - 1 (<=7Fh)
;
;		mov	ebx, eax
;		add	bl, NumOfKernelDevices 	; < NUMOFDEVICES
;
;		; check if empty dev entry in the list
;		cmp	byte [DEV_OPENMODE+ebx], 0
;		ja	short gdn_20 ; it must be already set
;
;		; (re)set device name and access flags
;		; (remain open work will be easy after that)
;		sub	edi, 8 ; installable device name address
;		shl	bx, 2 ;*4
;		mov	[DEV_NAME_PTR+ebx], edi ; (all) device names
;		shr	bx, 2
;		mov	al, [IDEV_FLAGS+eax] ; installable dev list
;		mov	[DEV_ACCESS+ebx], al ; (all) device list
;gdn_20:	
;		mov	al, bl
;		; eax = device index/entry number ; < NUMOFDEVICES
;		retn

;lodsb_capitalize:
;	; 07/10/2016 - TRDOS 386 (TRDOS v2.0)
;	; INPUT -> [esi] = character
;	;          edi = destination
;	; OUTPUT -> AL contains capitalized character
;	;	   esi = esi+1
;	;	   edi = edi+1	
;	; 
;	lodsb	
;	cmp	al, 61h
;    	jb	short lodsb_cap_retn
;     	cmp	al, 7Ah
;     	ja	short lodsb_cap_retn
;     	and	al, 0DFh
;lodsb_cap_retn:
;	stosb
;	retn

; 17/04/2021
; ('device_open' procedure is disabled as temporary)

;device_open:
;	; 08/10/2016 - TRDOS 386 (TRDOS v2.0)
;	; Complete device opening work for sysopen (device)
;	;
;	; INPUT -> 
;	;	EAX = Device Number (AL)
;	;        CL = Open mode (1 = read, 2 = write)
;	;	 CH = Device access byte (bit 0 = 0)
;	; OUTPUT ->
;	;	EAX = Device Number
;	;	CF = 0 -> device has been opened
;	;	CF = 1 -> device could not be opened
;	;
;	;  Modified registers: ebx, (edx, ecx, esi, edi, ebp)
;	;
;
;	mov	ebx, eax
;	shl	bx, 2 ; *4
;
;	test	ch, 80h ; bit 7, installable device driver flag
;	jz	short d_open_2 ; Kernel device
;	; installable device
;d_open_1:
;       jmp	dword [ebx+IDEV_OADDR-4]
;d_open_2:
;	jmp	dword [ebx+KDEV_OADDR-4]

; 17/04/2021
; ('device_close' procedure is disabled as temporary)

;device_close:
;	; 08/10/2016 - TRDOS 386 (TRDOS v2.0)
;	; Complete device closing work for sysclose (device)
;	;
;	; INPUT -> 
;	;	EAX = Device Number (AL)
;	;        CL = Open mode (1 = read, 2 = write)
;	;	 CH = Device access byte (bit 0 = 0)
;	; OUTPUT ->
;	;	EAX = Device Number	
;	;	CF = 0 -> device has been closed
;	;	CF = 1 -> device could not be closed
;	;
;	; Modified registers: ebx, (edx, ecx, esi, edi, ebp)
;	;
;
;	mov	ebx, eax
;	shl	bx, 2 ; *4
;
;	test	ch, 80h ; bit 7, installable device driver flag
;	jz	short d_close_2 ; Kernel device
;	; installable device
;d_close_1:
;       jmp	dword [ebx+IDEV_CADDR-4]
;d_close_2:
;	jmp	dword [ebx+KDEV_CADDR-4]

;rnull:
;	; 07/10/2016 - TRDOS 386 (TRDOS v2.0)
;	; read null (read from null device)
;	retn

;wnull:
;	; 07/10/2016 - TRDOS 386 (TRDOS v2.0)
;	; write null (write to null device)
;	retn

dev_IRQ_service:
	; 12/05/2017
	; 13/04/2017
	; 27/02/2017 - TRDOS 386 (TRDOS v2.0)
	; INPUT ->
	;	AL = IRQ Number (0 to 15)
	;	
	push	ebx
	movzx	ebx, al
	shl	bl, 2 ; * 4
	mov	ebx, [ebx+DEV_INT_HNDLR]
	and 	ebx, ebx
        jz	short dIRQ_s_retn
	push	eax

	call	ebx

	pop	eax
dIRQ_s_retn:
	pop	ebx
	retn

set_dev_IRQ_service:
	; 09/08/2022
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 13/04/2017 - TRDOS 386 (TRDOS v2.0)
	;
	; Set Device Interrupt Service
	;
	; INPUT ->
	;	AL = IRQ Number
	;	EBX = Hardware Interrupt Service Address
	;
	; Note: There is not a validation check here
	;	because this procedure is called by
	;	TRDOS 386 kernel !
	;       (Even if a device driver does not exist
	;	this setting may be used by sysaudio
	;	and other system calls for hardware
	;	components which use IRQ method for I/O.)
	;
	;push	esi
	movzx	esi, al
	;shl	si, 2 ; * 4
	; 09/08/2022
	shl	esi, 2 ; * 4
	mov	[esi+DEV_INT_HNDLR], ebx
	;pop	esi
	retn

sysaudio: ; AUDIO FUNCTIONS
	; 19/12/2024
	; 23/08/2024 (TRDOS 386 v2.0.9) 
	; 05/06/2024
	; 04/06/2024
	; 23/05/2024 (TRDOS 386 v2.0.8)
 	; 19/11/2023 (TRDOS 386 v2.0.7)
	; 29/07/2022 (TRDOS 386 v2.0.5)
	; 12/02/2021 (TRDOS 386 v2.0.3) 
	; 28/07/2020
	; 27/07/2020
	; 10/10/2017
	; 22/06/2017
	; 28/05/2017, 04/06/2017, 05/06/2017, 10/06/2017
	; 01/05/2017, 12/05/2017, 15/05/2017, 20/05/2017
	; 21/04/2017, 22/04/2017, 23/04/2017, 24/04/2017
	; 10/04/2017, 13/04/2017, 14/04/2017, 16/04/2017
	; 03/04/2017 (VIA VT8237R)
	; 01/04/2016 (trdosk6.s -> tdosk8.s)
	; 16/05/2016 - TRDOS 386 (TRDOS v2.0)
	;
	; Inputs:
	;
	;	BH = 0 -> Beep (PC Speaker)
	;	     BL = Duration Counter (1 for 1/64 second)
	;	     CX = Frequency Divisor (1193180/Frequency)
	;		 (1331 for 886 Hz)
	;
	;	01/04/2017	
	;
	; 	BH = 1 -> DETECT (& ENABLE) AUDIO DEVICE
	;	     BL = 0 : PC SPEAKER
	;		  1 : SOUND BLASTER 16
	;		  2 : INTEL AC'97
	;		  3 : VIA VT8237R (VT8233)
	;		  4 : INTEL HDA
	;	      5-FEh : unknown/invalid
	;	        ; 04/06/2017
	;		FFh : Get current audio device id
	;
	; 	BH = 2 -> ALLOCATE AUDIO BUFFER (for user)
	;		ECX = Audio Buffer Size (must be equal to
	;		      the half of DMA buffer size) 
	;		EDX = Virtual Address of the buffer
	;		      (This is not DMA buffer!)
	;
	;	BH = 3 -> INITIALIZE AUDIO DEVICE
	;	     BL = 0,2 -> for Signal Response Byte
	;	     	CL = Signal Response Byte Value (fixed)
	;				if BL = 0
	;	             auto increment of S.R.B. value
	;			 	if BL = 2
	;	        EDX = Signal Response (Return) Byte Address
	;	     	   			
	;	     BL = 1 for CallBack Method
	;	    	EDX = CallBack Service Address (Virtual)
	;
	;	     BL > 2 -> invalid function
	;
	;	    (Audio buffer must be allocated before
	;	     initialization.)
	;
	;	BH = 4 -> START TO PLAY
	;	     BL = Mode
	;		  Bit 0 = mono/stereo (1 = stereo)
	;		  Bit 1 = 8 bit / 16 bit (1 = 16 bit)
	;	     CX = Sampling Rate (Hz)
	;
	;	BH = 5 -> PAUSE
	;	     BL = Any
	;
	;	BH = 6 -> CONTINUE TO PLAY
	;	     BL = Any
	;
	;	BH = 7 -> STOP
	;	     BL = Any
	;
	;	BH = 8 -> RESET 
	;	     BL = Any
	;
	;	BH = 9 -> CANCEL (CALLBACK or S.R.B. SERVICE)
	;	     BL = Any	
	;	
	;	BH = 10 -> DEALLOCATE AUDIO BUFFER (for user)
	;	     BL = Any
	;
	;	BH = 11 -> SET VOLUME LEVEL
	;	     BL: (Bit 0 to 6)
	;		  0 = Master (Playback, Lineout) volume
	;		  1 = PCM out volume ; 23/05/2024
	;	     CL = Left Channel Volume (0 to 31 max)
	;	     CH = Right Channel Volume (0 to 31 max)
	;	
	;	     Note: If BL >= 80h (Bit 7 of BL is set),
	;	     volume level will be set for next playing
	;	     (actual volume level will not be changed
	;	     immediately)
	;
	;	BH = 12 -> DISABLE AUDIO DEVICE
	;	     (reset audio device and unlink dma buffer)
	;	     BL = Any
	;
	;    	12/05/2017
	;	BH = 13 -> MAP DMA BUFFER TO USER
	;	    (for direct access to system's dma buffer)
	;
	;	     ECX = map size in bytes 
	;		  (will be rounded up to page borders)
	;	     EDX = Virtual Address of the buffer
	;		  (Will be rounded up to page borders)
	;
	;	05/06/2017
	;    	04/06/2017
	;	BH = 14 -> GET AUDIO DEVICE INFO
	;	     BL: 0 = Audio Controller Info
	;	     ; 19/11/2023
	;	     BL: 1 = Audio (AC'97) Codec Info
	;	     BL > 1 = Invalid for now! 
	;
	;	22/06/2017	
	;	BH = 15 -> GET CURRENT SOUND DATA (for graphics)
	;	     BL: 0 -> PCM OUT data
	;	       > 0 -> Invalid for now!
	;	     ECX = 0 -> Get DMA Buffer Pointer
	;		 EDX = Not Used
	;	     ECX > 0 -> Byte count for buffer (EDX)
	;	         EDX = Buffer Address (Virtual)
	;
	;	10/10/2017	
	;	BH = 16 -> UPDATE DMA BUFFER DATA
	;		   (by using the Audio Buffer content)
	;	     BL = 0 : Update dma half buffer in sequence
	;		      (automatic destination)	
	;		  1 : Update 1st half of the dma buffer
	;		  2 : Update 2nd half of the dma buffer
	;		  3-FEh: Invalid!
	;		  FFh = Get current flag value
	;			(Half buffer number -1)
	;
	;	24/05/2024
	;	BH = 17 -> GET VOLUME LEVEL
	;	     BL:  0 = Master (Playback, Lineout) volume
	;		  1 = PCM out volume
	;
	; Outputs:
	;
	;	For BH = 0 -> Beep
	;	    None
	;
	;	01/04/2017
	;
	; 	For BH = 1 -> DETECT (& ENABLE) AUDIO DEVICE
	;	    AH = 0 : PC SPEAKER
	;		 1 : SOUND BLASTER 16
	;		 2 : INTEL AC'97
	;		 3 : VIA VT8237R (VT8233)
	;		 4 : INTEL HDA
	;	      5-FFh : unknown/invalid
	;	    AL = mode status
	;		 bit 0 = mono /stereo (1 = stereo)
	;		 bit 1 = 8 bit / 16 bit (1 = 16 bit)
	;	    04/06/2017
	;	    EBX = PCI DEVICE/VENDOR ID (if >0)
	;		 (BX = VENDOR ID) 
	;	    (if CF = 1 -> Error code in EAX)
	;
	; 	For BH = 2 -> ALLOCATE AUDIO BUFFER (for user)
	;	    EAX = Physical Address of the buffer
	;	    (if CF = 1 -> Error code in EAX)
	;
	;	For BH = 3 -> INITIALIZE AUDIO DEVICE
	;	    (if CF = 1 -> Error code in EAX)
	;
	;	For BH = 4 -> START TO PLAY
	;	    none (if CF = 1 -> Error code in EAX)
	;
	;	For BH = 5 -> PAUSE
	;	    none (if CF = 1 -> Error code in EAX)
	;
	;	For BH = 6 -> CONTINUE TO PLAY
	;	    none (if CF = 1 -> Error code in EAX)
	;
	;	For BH = 7 -> STOP
	;	    none (if CF = 1 -> Error code in EAX)
	;
	;	For BH = 8 -> RESET 
	;	    none (if CF = 1 -> Error code in EAX)
	;
	;	For BH = 9 -> CANCEL (CALLBACK or S.R.B. SERVICE)
	;	    none (if CF = 1 -> Error code in EAX)
	;	
	;	For BH = 10 -> DEALLOCATE AUDIO BUFFER (for user)
	;	    none (if CF = 1 -> Error code in EAX)
	;
	;	For BH = 11 -> SET VOLUME LEVEL
	;	    none (if CF = 1 -> Error code in EAX)
	;
	;	For BH = 12 -> DISABLE AUDIO DEVICE
	;	    none (if CF = 1 -> Error code in EAX)
	; 
	;    	12/05/2017
	;	For BH = 13 -> MAP DMA BUFFER TO USER
	;	    EAX = Physical Address of the buffer
	;	    (if CF = 1 -> Error code in EAX)
	;
	;    	04/06/2017
	;	For BH = 14 -> GET AUDIO DEVICE INFO
	;	(for BL = 0) ; 05/06/2017	
	;	    EAX = IRQ Number in AL
	;		  Audio Device Number in AH 
	;	    EBX = DEV/VENDOR ID
	;		 (DDDDDDDDDDDDDDDDVVVVVVVVVVVVVVVV)
	;	    ECX = BUS/DEV/FN 
	;		 (00000000BBBBBBBBDDDDDFFF00000000)
	;	    EDX = NABMBAR/NAMBAR (for AC97)
	;		  (Low word, DX = NAMBAR address)
	;	    EDX = Base IO Addr (DX) for SB16 & VT8233
	;	    (if CF = 1 -> Error code in EAX)
	;	                 (ERR_DEV_NOT_RDY = 15)
	;	(for BL = 1) -for AC97- ; 19/11/2023
	;	    EAX = Extended Audio ID (MX28) in AX
	;	     AX bit 0 - VRA bit
	;	     HW of EAX = PCM output sample rate (HZ)
	;	    EBX = VENDOR ID 1 (BX), VENDOR ID 2 (HW)
	;	    (if CF = 1 -> Error code in EAX)	
	;	                 (ERR_DEV_NOT_RDY = 15)
	;	    Note: EAX & EBX = 0 (for SB16,VIA,HDA)
 	;
	;	22/06/2017	
	;	For BH = 15 -> GET CURRENT SOUND DATA
	;			 (for graphics)
	;	(for BL = 0)
	;	 If ECX input is 0 
	;	    EAX = DMA Buffer Current Position (Offset)
	;	 If ECX input >  0
	;	    EAX = Actual transfer count
	;	    (Sound samples will be copied from 
	;	     Current DMA Buffer Position to EDX
	;	     virtual address as EAX bytes.)
	;	 ((If CF = 1 ->  Error code in EAX))
	;
	;	10/10/2017	
	;	For BH = 16 -> UPDATE DMA BUFFER DATA
	;	    EAX = 0, if the updated (or current)
	;		     half buffer is DMA half buffer 1
	;	    EAX = 1, if the updated (or current)
	; 		     half buffer is DMA half buffer 2
	;	    (If CF = 1 -> Error code in EAX)
	;
	;	24/05/2024
	;	For BH = 17 -> GET VOLUME LEVEL
	;	    IF BL input is 0
	;		Master (Playback, Lineout) volume
	;	    If BL input is 1
	;		PCM out volume
	;	    If BL input > 1
	;		Invalid for now ; 24/05/2024
	;
	;	    CL = Left Channel Volume (0 to 31 max)
	;	    CH = Right Channel Volume (0 to 31 max)
	;	

	cmp	bh, AUDIO1L/4
	jnb	sysret

	shl	bh, 2 ; *4	
	movzx	esi, bh

	; 22/04/2017
	xor	eax, eax
	mov	[u.r0], eax  ; 0
		
	call	dword [esi+AUDIO1]
	;jc	error
	jmp	sysret	

AUDIO1:	dd	_beep ; 12/02/2021
	;dd	beep ; FUNCTION = 0 (bl = Duration Counter
		     ; 		     cx = Frequency Divisor
	dd	soundc_detect
	dd	sound_alloc
	dd	soundc_init
	dd	sound_play
	dd	sound_pause
	dd	sound_continue
	dd	sound_stop
	dd	soundc_reset
	dd	soundc_cancel
	dd	sound_dalloc
	dd	sound_volume
	dd	soundc_disable
	dd	sound_dma_map
	dd	soundc_info
	dd	sound_data
	dd	sound_update
	dd	sound_getvol ; 24/05/2024
	
AUDIO1L	EQU	$ - AUDIO1

soundc_detect:
	; FUNCTION = 1
	; bl = Audio device type number 
	; (0 = pc speaker, 1 = sound blaster 16, 2 = intel ac97
	;  3 = via vt823x, 4 = intel HDA, 0FFh = any)
	
	; 04/06/2017
	mov	ah, [audio_device]
	cmp	bl, 0FFh ; get current audio device id
	je	short sysaudio0

	and	ah, ah
	jz	short soundc_get_dev

	cmp	ah, bl
	jne	short soundc_dev_err

sysaudio0:
	mov	al, [audio_mode]
sysaudio1:
	mov	[u.r0], eax
	mov	ebx, [audio_vendor] ; (DEVICE/VENDOR ID)
	mov	ebp, [u.usp]
	mov	[ebp+16], ebx  ; ebx
	retn

soundc_get_dev:
	; 28/05/2017
	; 03/04/2017, 24/04/2017
	mov	byte [audio_pci], 0
	cmp	bl, 3 ; VIA VT8233 (VT8237R) Audio Controller & AC97 Codec
	;jne	short soundc_get_dev_sb
	; 28/05/2017
	jb	short soundc_get_dev_sb
	ja	short soundc_dev_err  ; temporary (28/05/2017)
	;
	call	DetectVT8233
	jc	short soundc_dev_err
	; eax = 0

	;mov	ebx, [audio_vendor]
	; ebx = DEVICE/VENDOR ID
	;	DDDDDDDDDDDDDDDDVVVVVVVVVVVVVVVV

	mov	al, 3  ; VIA VT8237R (VT3233) Audio Controller
	mov	ah, al

soundc_get_pci_dev_ok: ; 28/05/2017
	inc	byte [audio_pci] ; = 1
soundc_get_dev_ok:

soundc_get_dev_sb16_ok:
	mov	[audio_device], al
	mov	[audio_mode], ah ; stereo (bit0), 16 bit (bit1) capability
	jmp	short sysaudio1

soundc_get_dev_sb:
	; 24/04/2017
	cmp	bl, 1 ; Sound Blaster 16
	jne	short soundc_get_dev_ich ; 28/05/2017
	;
	call	DetectSB
	jc	short soundc_dev_err
	mov	eax, 0301h ; Sound Blaster 16
	jmp	short soundc_get_dev_sb16_ok

soundc_get_dev_ich:
	; 28/05/2017
	;cmp	bl, 2 ; Intel AC'97 Audio Controller (ICH)
	;jne	short soundc_dev_err ; Temporary  (28/05/2017)
	;			     ; (Here will be modified just after
	;			     ; new sound card code will be ready!)
	call	DetectICH
	jc	short soundc_dev_err
	;
	mov	eax, 0302h ; AC'97 (ICH)
	jmp	short soundc_get_pci_dev_ok

soundc_dev_err:
	mov	eax, ERR_DEV_NOT_RDY ; Device not ready !
	; 29/07/2022
	;sub	eax, eax
	;mov	al, ERR_DEV_NOT_RDY
	jmp	short sysaudio_err

soundc_respond_err:
	; ERR_TIME_OUT ; 'time out !' error
	mov	eax, ERR_DEV_NOT_RESP ; 'device not responding !' error
	; 29/07/2022
	;sub	eax, eax
	;mov	al, ERR_DEV_NOT_RESP
sysaudio_err:
	mov	[u.r0], eax
	mov	[u.error], eax
	jmp	error

sound_alloc:
	; FUNCTION = 2
	; ecx = audio buffer size (in bytes)
	; edx = audio buffer address (virtual)
	; 25/11/2023
	; 27/07/2020
	; 28/05/2017
	; 01/05/2017, 15/05/2017
	; 21/04/2017, 24/04/2017
	cmp	byte [audio_pci], 0
	ja	short snd_alloc_0
	; Max. 64KB DMA buffer !!!
	cmp	ecx, 32768
	jna	short snd_alloc_6
	; 25/11/2023
sound_buff_error:
	mov	eax, ERR_BUFFER ; Buffer error !
	; 29/07/2022
	;sub	eax, eax
	;mov	al, ERR_BUFFER
	jmp	short sysaudio_err
snd_alloc_0:	
	; 25/11/2023
	; Max 128KB DMA buffer size (2 half buffers)
	cmp	ecx, 65536
	ja	short sound_buff_error
snd_alloc_6:
	; 15/05/2017
	cmp	ecx, 4096 ; PAGE_SIZE
	jb	short sound_buff_error
	;
	mov	eax, [audio_buffer] ; audio buffer address (current)
	or	eax, eax
	jz	short snd_alloc_2
	; audio buffer exists !
	mov	bl, [u.uno]
	cmp	bl, [audio_user]
	;jne	sndc_owner_error ; not owner !
	; 25/11/2023
	je	short snd_alloc_7
	jmp	sndc_owner_error ; not owner !
snd_alloc_7:
	cmp	eax, edx ; same virtual buffer address ?
	jne	short snd_alloc_1
	cmp	ecx, [audio_buff_size]
	je	short snd_alloc_3 ; Nothing to do !
				  ; Buffer has been set already!
snd_alloc_1:
	push	ecx
	push	edx
	mov	ebx, eax ; audio buffer address (current)
	mov	ecx, [audio_buff_size]
	call	deallocate_user_pages
	pop	edx
	pop	ecx
	xor	eax, eax ; 0
	mov	[audio_buffer], eax  ; 0
 	mov	[audio_p_buffer], eax  ; 0
 	mov	[audio_buff_size], eax
	mov	[audio_user], al ; 0
snd_alloc_2:
	mov	ebx, edx
	; 01/05/2017
	;mov	edx, ~PAGE_OFF ; truncating page offsets
	;		       ; for aligning to page borders
	;;and	eax, edx
	;and	ebx, edx
	; 26/11/2023
	;and	ecx, edx
	and	ebx, ~PAGE_OFF
	mov	edx, ecx
	; 15/05/2017
	; EAX = Beginning address (physical)
	; EAX = 0 -> Allocate mem block from the 1st proper aperture	
	; ECX = Number of bytes to be allocated
	call	allocate_memory_block
	jc	short sound_buff_error

	; EAX = Physical address of the allocated memory block
	; ECX = Allocated bytes (as rounded up to page border)
	; EBX = Virtual address (as truncated to page border)
	push	edx ; 26/11/2023
	push	eax
	push	ebx
	push	ecx
	call	allocate_user_pages
	pop	ecx
	pop	ebx
	pop	eax
	pop	edx ; 26/11/2023
	jc	short snd_alloc_4  ; insufficient memory, buff error
	; eax = physical address of the user's audio buffer
	; ebx = virtual address of the user's audio buffer
	; 26/11/2023
	; ecx = allocated buffer size (in bytes)
	;		 -rounded up to page boundary-
	; edx = requested buffer size (in bytes)
	mov	[audio_p_buffer], eax
	mov	[audio_buffer], ebx
	; 26/11/2023
	;mov	ecx, edx
	;; 25/11/2023
	;cmp	ecx, 65536
	;jb	short snd_alloc_5
	;dec	ecx
	;; ecx = 65535
	;; (DMA half buffer's sample count must be < 65536)
;snd_alloc_5:
 	;mov	[audio_buff_size], ecx
	; 26/11/2023
	mov	[audio_buff_size], edx
	;
	mov	dl, [u.uno]
	mov	[audio_user], dl
	mov	[u.r0], eax
snd_alloc_3:
	; 27/07/2020
	mov	byte [audio_flag], 0 ; clear dma half buffer flag
	;	
	retn
snd_alloc_4:
	; 15/05/2017
	; EAX = Beginning address (physical)
	; ECX = Number of bytes to be deallocated
	call	deallocate_memory_block
	jmp	sound_buff_error ; insufficient memory, buff error

soundc_init:
	; FUNCTION = 3 
	; bl = method (0= s.r.b., 1= callback, 2= auto incr s.r.b.)
	; cl = signal response byte (initial or fixed) value
	; edx = signal response byte or callback address
	; 05/06/2024
	; 04/06/2024
	; 04/12/2023
	; 02/12/2023
	; 07/08/2022
	; 29/07/2022
	; 27/07/2020
	; 28/05/2017
	; 12/05/2017, 20/05/2017
	; 22/04/2017, 23/04/2017, 24/04/2017
	; 13/04/2017, 14/04/2017, 16/04/2017, 21/04/2017
	; 03/04/2017, 10/04/2017

	mov	al, [audio_device]
	and	al, al
	jnz	short sndc_init6
	;
	mov	byte [audio_pci], 0
	push	edx
	push	ebx
	push	ecx
	call	DetectSB
	jc	short sndc_init8
	mov	ax, 0301h ; Sound Blaster 16
	jmp	short sndc_init7

sndc_init11:
	; 28/05/2017
	call	DetectICH ; Detect AC'97 (ICH) Audio Controller
	jc	short sndc_init7
	mov	ax, 0302h ; Intel AC'97 Audio Device
	jmp	short sndc_init12 ; (PCI device)

sndc_init8:
	call	DetectVT8233
	;jc	short sndc_init7
	; 29/07/2022
	jc	short sndc_init11 ; 28/05/2017

	; eax = 0
	mov	al, 3	; VIA VT8237R (VT8233) Audio Controller
	mov	ah, al

sndc_init12:
	inc	byte [audio_pci] ; = 1
sndc_init7:
	pop	ecx
	pop	ebx
	pop	edx
	;jc	soundc_dev_err
	; 29/07/2022
	jnc	short sndc_init14
	jmp	soundc_dev_err
sndc_init14:
	mov	[audio_device], al
	mov	[audio_mode], ah ; stereo (bit0), 16 bit (bit1) capability

sndc_init6:
	cmp	dword [audio_buffer], 0
	;jna	sound_buff_error
	; 29/07/2022
	ja	short sndc_init19
	jmp	sound_buff_error ; 07/08/2022
sndc_init19:
	mov	al, [u.uno]
	mov	ah, [audio_user]
	or	ah, ah
	jz	short sndc_init0
	cmp	al, ah
	je	short sndc_init1

sndc_owner_error:
	mov	eax, ERR_NOT_OWNER ; 'permission denied !' error
	; 29/07/2022
	;sub	eax, eax
	;mov	al, ERR_NOT_OWNER
sndc_perm_error:
	mov	[u.r0], eax
	mov	[u.error], eax
	jmp	error
sndc_init0:
	mov	[audio_user], al
sndc_init1:
	mov	[audio_cb_addr], edx
	mov	[audio_cb_mode], bl
	mov	[audio_srb], cl

	; 27/07/2020
	;mov	byte [audio_flag], 0  ; clear dma half buffer flag

	; 24/04/2017
	cmp	byte [audio_device], 3 ; VT8233 (VT8237R)
	je	short sndc_init9
	;ja	short soundc_respond_err ; temporary (28/05/2017)
	cmp	byte [audio_device], 1 ; SB 16
	jne	short sndc_init13
	mov	ebx, sb16_int_handler
	; Note: 'SbInit' is at 'Start to Play' stage
	; 20/05/2017
	mov	word [audio_master_volume], 0808h ; 2/8
	jmp	short sndc_init10
sndc_init13:
	; 28/05/2017
	cmp	byte [audio_device], 2 ; AC 97 (ICH)
	;jne	soundc_respond_err ; temporary (28/05/2017)
	; 29/07/2022
	je	short sndc_init16
sndc_init15:
	jmp	soundc_respond_err
sndc_init16:
	call	ac97_codec_config
	;jc	soundc_respond_err ; codec error !
	; 29/07/2022
	jc	short sndc_init15

	mov	ebx, ac97_int_handler
	jmp	short sndc_init10

sndc_init9:
	;call	reset_codec
	;; eax = 1
	;call	codec_io_w16 ; w32
	call	init_codec ; 28/05/2017
	;jc	soundc_respond_err ; codec error !
	; 29/07/2022
	jc	short sndc_init15

	call	channel_reset

	; setup the Codec (actually mixer registers)
        call    codec_config  ; unmute codec, set rates.
	;jc	soundc_respond_err ; codec error !
	; 29/07/2022
	jc	short sndc_init15

	mov	ebx, vt8233_int_handler
sndc_init10:
	; 13/04/2017
	mov	al, [audio_intr] ; IRQ number
	call	set_dev_IRQ_service

	; SETUP (audio) INTERRUPT CALLBACK SERVICE
	mov	bl, [audio_intr] ; IRQ number
	mov	bh, [audio_cb_mode]
	inc	bh  ; 1 = Signal Response Byte method (fixed value)
		    ; 2 = Callback service method
		    ; 3 = Auto Increment S.R.B. method
	mov	cl, [audio_srb]
	mov	edx, [audio_cb_addr]
	mov	al, [audio_user]
	; 14/04/2017
 	call	set_irq_callback_service
	; 16/04/2017
	mov	[u.r0], eax
	;jnc	sysret
	jnc	short sndc_init2 ; 21/04/2017
	;
	mov	dword [u.error], eax

	mov	al, [audio_intr] ; IRQ number
	xor	ebx, ebx ; reset IRQ handler address
	call	set_dev_IRQ_service

	jmp	error

sndc_init2:
	; 21/04/2017
	mov	ecx, [audio_buff_size] ; audio buffer size
	; 05/06/2024	
	;shl	ecx, 1 ; *2
	
	; 05/06/2024
	mov	al, [audio_device]
	cmp	al, 1 ; SB16
	jna	short sndc_init22
	and	cl, ~1 ; truncated for word alignment
	cmp	al, 3 ; VT8233  
	jnb	short sndc_init22
	; al = 2 ; AC'97 	
	and	cl, ~7 ; truncated for 8 byte (8x) alignment
sndc_init22:
	; 05/06/2024
	mov	[dma_hbuff_size], ecx  ; DMA half buffer size
	shl	ecx, 1  ; add ecx, ecx ; * 2
	
	;;;
	; 04/06/2024
	add	ecx, 4095  ; PAGE_SIZE - 1
	and	cx, ~4095  ; ~PAGE_OFF	
	; ecx = page border aligned DMA buffer size (required) (*)
	;;;

	mov	eax, [audio_dma_buff]
	and	eax, eax
	jz	short sndc_init3 ; no need to compare dma buff size
	
	mov	edx, [audio_dmabuff_size] ; dma buffer size
	cmp	ecx, edx
	je	short sndc_init5

	; 04/12/2023
	cmp	eax, sb16_dma_buffer	; reserved buffer ?
	je	short sndc_init20 ; it isn't an allocated mem buff

	xchg	ecx, edx
	; 26/11/2023 
	; round up (always -rounded up- page count is allocated)
	; ((so deallocation must be done for the rounded up value))
	;add	ecx, PAGE_SIZE - 1   ; 4095
	;call	deallocate_memory_block
	; 04/06/2024
	;call	deallocate_memory_block_x
	;		; deallocate ((ecx+4095)>>12) pages
	call	deallocate_memory_block	

	xchg	edx, ecx
sndc_init20:
	xor	eax, eax
sndc_init3:
	; 05/06/2024
	mov	[audio_dmabuff_size], ecx ; (*)
	; 12/05/2017
	cmp	byte [audio_device], 1 ; SB 16
	jne	short sndc_init4
	mov	dword [audio_dma_buff], sb16_dma_buffer
	; 05/06/2024
	;mov	dword [audio_dmabuff_size], 65536
	;xor	eax, eax
	;mov	[u.r0], eax ; 0 = no error, successful
sndc_init5:	; 29/07/2022
	retn 

sndc_init4:
	; 02/12/2023 - TRDOS 386 v2.0.7
	cmp	ecx, 65536
	ja	short sndc_init21
	mov	eax, sb16_dma_buffer ; use already reserved buffer
	jmp	short sndc_init17

sndc_init21:
	; EAX = Beginning address (physical)
	; EAX = 0 -> Allocate mem block from the 1st proper aperture
	; ECX = Number of bytes to be allocated	(>0)
	call	allocate_memory_block
	;jc	sound_buff_error
	; 29/07/2022
	jnc	short sndc_init17
	jmp	sound_buff_error

sndc_init17:	; 29/07/2022
	; set dma buffer address and size parameters
	mov	[audio_dma_buff], eax ; dma buffer address
	; 05/06/2024
	;mov	[audio_dmabuff_size], ecx ; dma buffer size

	;;;
	; 05/06/2024
	mov	ecx, [dma_hbuff_size] ; DMA half (1-2) buffer size
	; 04/06/2024
	;mov	ecx, [audio_buff_size] ; audio buffer size in bytes
	;;;

;	; EAX = Beginning (physical) addr of the allocated mem block
;	; ECX = Num of allocated bytes (rounded up to page borders)
;	cmp	byte [audio_pci], 0 ; AC97 audio controller ?
;	ja	short sndc_init4
;
;	; Sound Blaster 16 uses classic DMA
;	mov	edx, eax
;	add	edx, ecx
;	cmp	edx, 1000000h ; 1st 16 MB
;	jna	short sndc_init4
;
;	; error !
;	; restore Memory Allocation Table Content
;	; EAX = Beginning address (physical)
;	; ECX = Number of bytes to be deallocated
;	call	deallocate_memory_block
;	; reset dma buffer address and size parameters
;	xor	eax, eax ; 0
;	mov	[audio_dma_buff], eax ; 0
;	mov	[audio_dmabuff_size], ecx ; 0
;	jmp	sound_buff_error
;
;sndc_init4:
	cmp	byte [audio_device], 3
	;jne	short sndc_init5
	; 29/07/2022
	jne	short sndc_init18 ; 28/05/2017

; 29/07/2022
;	call	set_vt8233_bdl
;sndc_init5:
;	;sub	eax, eax ; 0
;	;mov	[u.r0], eax ; 0 = no error, successful
;	retn

	; 05/06/2024
	;;;
	; 04/06/2024 (use truncated buffer size for BDL setup)
	; NOTE: Round up adds spurious bytes (noise) to the DMA buff;
	;	so, round down the size is better than the round up.
	;	((BDL/SGD feature needs word aligned buffer address))
	;
	;mov	ecx, [audio_buff_size] ; audio buffer size in bytes
	;and	cl, ~1
	; 05/06/2024
	;mov	[dma_hbuff_size], ecx
	; ecx = DMA half buffer size as word aligned
	;	(in fact, half buffer is one of the two DMA buffers)
	;;;

	jmp	set_vt8233_bdl

sndc_init18:
	;call	set_ac97_bdl
	;;jmp	short sndc_init5
	;retn

	; 05/06/2024
	;;;
	;;;
	; 04/06/2024 (use truncated buffer size for BDL setup)
	; NOTE: Round up adds spurious bytes (noise) to the DMA buff;
	;	so, round down the size is better than the round up.
	;	((BDL feature needs 8 byte aligned buffer address))
	;
	;mov	ecx, [audio_buff_size] ; audio buffer size in bytes
	;and	cl, ~7
	; 05/06/2024
	;mov	[dma_hbuff_size], ecx
	; ecx = DMA half buffer size as truncated for 8 byte alignment
	;	(in fact, half buffer is one of the two DMA buffers)
	;;;

	; 29/07/2022
	jmp	set_ac97_bdl

sound_play:
	; FUNCTION = 4 
	; bl = Mode 
	;      bit 0 = mono/stereo (1 = stereo)
	;      bit 1 = 8 bit / 16 bit (1 = 16 bit)
	; cx = Sampling Rate (Hz)

	; 13/06/2017
	; Note: Even if Mode bits are not 11b,
	; 	AC'97 Audio Controller (&Codec)
	;	will play audio samples as 16 bit, stereo
	;	samples.
	;	(Program must fill the audio buffer
	;	as required; 8 bit samples must be converted
	;	to 16 bit samples and mono samples must be
	;	converted to stereo samples...)
	 
	; 19/12/2024 (BugFix)
	; 05/06/2024
	; 04/06/2024
	; 30/07/2022
	; 28/07/2020
	; 27/07/2020
	; 28/05/2017
	; 15/05/2017, 20/05/2017
	; 21/04/2017, 24/04/2017
	; ... device check at first
	mov	al, [audio_device]
	or	al, al ; 0 ; pc speaker or invalid
	;jz	beeper_gfx ; 'video.s' ; temporary
	; 30/07/022
	jnz	short snd_play_7
	jmp	beeper_gfx

snd_play_7:
;	cmp	al, 3 ; VIA VT 8237R (vt8233)
;	je	short snd_play_1
;	cmp	al, 1 ; SB 16
;	jne	soundc_dev_err ; temporary !
;snd_play_0:
	; ... buffer & (buffer) owner check at second
	cmp	dword [audio_buffer], 0
	;jna	sound_buff_error
	; 30/07/2022
	jna	short snd_play_4 ; jmp sound_buff_error
	mov	al, [u.uno]
	cmp	al, [audio_user]
	;jne	sndc_owner_error
	; 30/07/2022
	je	short snd_play_3
	jmp	sndc_owner_error
snd_play_3:
	mov	[audio_freq], cx ; sample frequency (Hertz)
	mov	al, bl
	and	al, 1 ; mono/stereo (1= stereo)
	inc	al ; channels
	mov	[audio_stmo], al ; sound channels (1 or 2)
	mov	al, 8
	test	bl, 2 ; bits per sample (1= 16 bit)
	jz	short snd_play_bps
	shl	al, 1  
snd_play_bps:	
	mov	[audio_bps], al

	; Transfer ring 3 (user's) audio buffer content to dma buffer
	mov	edi, [audio_dma_buff] ; dma buffer (ring 0)
	or	edi, edi
	;jz	sound_buff_error
	; 30/07/2022
	jnz	short snd_play_5
snd_play_4:
	jmp	sound_buff_error
snd_play_5:
	; 27/07/2020
	mov	esi, [audio_p_buffer] ; physical address (ring 3)
	;mov	ecx, [audio_buff_size] ; 15/05/2017

; 04/06/2024
%if 0
	mov	ecx, [audio_dmabuff_size] ; 27/07/2020
	;or	ecx, ecx 
	;jz	sound_buff_error
	; 28/07/2020
	shr	ecx, 1  ; dma half buffer size
%else
	; 05/06/2024
	mov	ecx, [dma_hbuff_size]  ; DMA half buffer size

	; 04/06/2024
	;mov	ecx, [audio_buff_size]
	; 19/12/2024 (BugFix)
	mov	al, [audio_device]
	;
	;cmp	al, 1 ; Sound Blaster 16
	;je	short snd_play_8
	;and	cl, ~1
	;cmp	al, 3 ; VT8233 (VT8237R)
	;je	short snd_play_8
	; AC'97
	;and	cl, ~7
snd_play_8:
%endif

	xor	byte [audio_flag], 1 ;  0 -> 1, 1 -> 0
	jnz	short snd_play_0 ; [audio_flag] = 1
				 ; fill dma half buffer 1
	; [audio_flag] = 0
	
	; fill dma half buffer 2
	add	edi, ecx

snd_play_0:
	; 05/06/2024
	;;rep	movsb
	;shr	ecx, 2 ; convert byte count to dword count
	;rep	movsd  ;	
	rep	movsb  ; SB16, AC97, VT8233

	; here, if [audio_flag] = 0, interrupt handler will update
				; dma half buffer 2 
				; (user's audio buffer data will be
				; copied into dma half buffer 2) 
	;; 20/05/2017
	;mov	byte [audio_flag], 1 ; next half (on next time)

	; 24/04/2017
	;mov	al, [audio_device]
	; 04/06/2024
	; al = [audio_device]

	cmp	al, 3 ; VT8233 (VT8237R)
	je	short snd_play_1
	cmp	al, 1 ; Sound Blaster 16
	jne	short snd_play_2  ; 28/05/2017

; 30/07/2022
;	call	SbInit_play
;	;jc	soundc_respond_err
;	;retn
;	; 30/07/2022
;	jnc	short snd_play_6  ; retn
;	jmp	soundc_respond_err

	; 30/07/2022
	jmp	SbInit_play	; sb16_start_play

snd_play_1:	
	;call	vt8233_start_play
	;retn
	; 30/07/2022
	jmp	vt8233_start_play

snd_play_2:
	; 28/05/2017
	;cmp	al, 2 ; AC'97
	;jne	short snd_play_3

	;call	ac97_start_play
	;retn
	; 30/07/2022
	jmp	ac97_start_play

;snd_play_3:
;	;call	hda_start_play
;	retn
	; 30/07/2022
	;jmp	hda_start_play

; 04/06/2024
;snd_play_6:
;	; 30/07/2022
;	retn

sound_pause:
	; FUNCTION = 5
	; Pause
	; 28/05/2017
	; 24/04/2017
	; 22/04/2017
	call	snd_dev_check
	jc	short snd_nothing ; temporary.
	call	snd_buf_check
	jc	short snd_nothing ; temporary.
	mov	al, [audio_device]
	cmp	al, 3 ; VIA VT 8237R (vt8233)
	je	short snd_pause_1
	cmp	al, 1 ; Sound Blaster 16
	jne	short snd_pause_2 ; 28/05/2017
	jmp	sb16_pause
snd_pause_1:
	jmp	vt8233_pause
snd_pause_2:
	; 28/05/2017
	;cmp	al, 2 ; AC'97
	;jne	short snd_nothing ; temporary.
	jmp	ac97_pause

sound_continue:
	; FUNCTION = 6
	; Continue to play
	; 28/05/2017
	; 22/04/2017
	call	snd_dev_check
	jc	short snd_nothing ; temporary.
	call	snd_buf_check
	jc	short snd_nothing ; temporary.
	mov	al, [audio_device]
	cmp	al, 3 ; VIA VT 8237R (vt8233)
	je	short snd_cont_1
	cmp	al, 1 ; Sound Blaster 16
	jne	short snd_cont_2 ; 28/05/2017
	jmp	sb16_continue
snd_cont_1:
	jmp	vt8233_play
snd_cont_2:
	; 28/05/2017
	;cmp	al, 2 ; AC'97
	;;jne	short snd_nothing ; temporary.
	; 30/07/2022
	;jne	short snd_cont_3
	jmp	ac97_play
;snd_cont_3:
	;jmp	hda_play

sound_stop:
	; FUNCTION = 7
	; Stop playing
	; 30/07/2022
	; 28/05/2017
	; 24/05/2017
	; 21/04/2017, 22/04/2017, 24/04/2017
	call	snd_dev_check
	jc	short snd_nothing ; temporary.
	;call	snd_buf_check
	call	snd_user_check ; 24/05/2017
	jc	short snd_nothing ; temporary.	
	
	mov	al, [audio_device]
	cmp	al, 3 ; VIA VT 8237R (vt8233)
	;je	vt8233_stop
	;; 28/05/2017
	;;ja	short snd_nothing
	; 30/07/2022
	jne	short snd_stop_1
	jmp	vt8233_stop
snd_stop_1:
	cmp	al, 1 ; Sound Blaster 16
	;je	sb16_stop
	; 30/07/2022
	jne	short snd_stop_2
	jmp	sb16_stop
snd_stop_2:
	;cmp	al, 2 
	;;je	short ac97_stop
	; 30/07/2022
	;jne	short snd_stop_3
	jmp	ac97_stop ; temporary.
;snd_stop_3:
	;jmp	hda_stop

sndc_cancel_ok:
	; 30/07/2022
sndc_reset_ok:
	; 30/07/2022
snd_nothing:
	; 21/04/2017
	retn

soundc_reset:
	; FUNCTION = 8
	; Reset Audio Controller
	; 30/07/2022
	; 28/05/2017
	; 22/04/2017
	call	snd_dev_check
	jc	short snd_nothing ; temporary.
	call	snd_buf_check
	jc	short snd_nothing ; temporary.	
	
	mov	al, [audio_device]
	; 30/07/2022
	cmp	al, 3 ; VIA VT 8237R (vt8233)
	jb	short sndc_reset_1
	;je	vt8233_reset
	ja	short snd_nothing ; temporary.
	;;ja	hda_reset
	;ja	short sndc_reset_3
	; 30/07/2022
	jmp	vt8233_reset	
sndc_reset_1:
	cmp	al, 1 ; Sound Blaster 16
	;jne	ac97_reset	
	; 30/07/2022
	jne	short sndc_reset_2
	call	sb16_reset
	;jc	soundc_respond_err
	;retn
	; 30/07/2022
	jnc	short sndc_reset_ok
	jmp	soundc_respond_err
sndc_reset_2:
	; 30/07/2022
	;cmp	al, 2
	;ja	short sndc_reset_3
	jmp	ac97_reset
;sndc_reset_3:
	;jmp	hda_reset

soundc_cancel:
	; FUNCTION = 9
	; Cancel audio callback service
	; 30/07/2022
	; 22/04/2017
	mov	al, [audio_user]
	cmp	al, [u.uno]
	jne	short snd_nothing
	; RESET (audio) INTERRUPT CALLBACK SERVICE
	mov	bl, [audio_intr] ; IRQ number
	mov	al, [u.uno]
	sub	bh, bh ; 0 ; unlink IRQ from user service
 	call	set_irq_callback_service
	;jc	sndc_perm_error ; 'permission denied' error
	;retn
	; 30/07/2022
	jnc	short sndc_cancel_ok
	jmp	sndc_perm_error

sound_dalloc:
	; FUNCTION = 10
	; Deallocate (ring 3) audio buffer
	; 22/04/2017
	mov	al, [audio_user]
	cmp	al, [u.uno]
	jne	short snd_nothing
	mov	ebx, [audio_buffer]
	;or	ebx, ebx
	;jz	short snd_nothing
	mov	ecx, [audio_buff_size]
	call	deallocate_user_pages
	xor	eax, eax
	mov	[audio_buffer], eax ; 0
	mov	[audio_user], al ; 0
;sndc_cancel_ok:
	retn

sound_volume:
	; FUNCTION = 11
	; Set sound volume level
	; 23/05/2024
	; 30/07/2022
	; 28/05/2017
	; 20/05/2017
	; 22/04/2017, 24/04/2017
	; bl = component
	;		0 = master/playback/lineout volume
	;		1 = PCM out volume ; 23/05/2024
	; cl = left channel volume level (0 to 31)
	; ch = right channel volume level (0 to 31)

	cmp	bl, 80h
	jb	short snd_vol_1
	;ja	snd_nothing ; temporary.
	; 30/07/2022
	ja	short snd_vol_0
	; Set volume level for next play (BL>= 80h)
	mov	[audio_master_volume], cx
snd_vol_0:
	retn
snd_vol_1:
	; set volume level immediate (BL< 80h)
	;cmp	bl, 0
	;;ja	snd_nothing ; temporary.
	; 30/07/2022
	;ja	short snd_vol_0
	; 23/05/2024
	cmp	bl, 1
	ja	short snd_vol_0 ; temporary

	call	snd_dev_check
	;jc	snd_nothing ; temporary.
	; 30/07/2022
	jc	short snd_vol_0
	call	snd_buf_check
	;jc	snd_nothing ; temporary.
	; 30/07/2022
	jc	short snd_vol_0	

	mov	al, [audio_device]
	cmp	al, 3 ; VIA VT 8237R (vt8233)
	;je	vt8233_volume
	; 30/07/2022
	jb	short snd_vol_2
	; 28/05/2017
	;ja	snd_nothing ; temporary.
	; 30/07/2022
	ja	short snd_vol_0
	;ja	hda_volume
	; 30/07/2022
	jmp	vt8233_volume
snd_vol_2:	
	; Sound Blaster 16
	cmp	al, 1 ; SB 16
	;je	sb16_volume
	; 30/07/2022
	ja	short snd_vol_3
	jmp	sb16_volume
snd_vol_3:
	; 30/07/2022
	;cmp	al, 2
	;ja	short snd_vol_4
	jmp	ac97_volume
;snd_vol_4:
	;jmp	hda_volume

soundc_disable:
	; FUNCTION = 12 
	; Disable audio device (and unlink DMA memory)
	; 23/08/2024
	; 04/06/2024
	; 30/07/2022
	; 28/05/2017
	; 24/05/2017
	; 22/04/2017
	call	snd_dev_check
	;jc	soundc_dev_err ; temporary.
	; 30/07/2022
	jnc	short snd_disable_4
	jmp	soundc_dev_err
snd_disable_4:
	;call	snd_buf_check
	;;jc	sndc_owner_error ; temporary.
	; 30/07/2022
	;jnc	short snd_disable_5
	;jmp	sndc_owner_error
;snd_disable_5:
	mov	al, [audio_device]
	cmp	al, 3 ; VIA VT 8237R (vt8233)
	je	short snd_disable_1
	;ja	snd_nothing ; temporary.
	; 30/07/2022
	ja	short snd_disable_3 ; retn
	cmp	al, 1 ; Sound Blaster 16
	jne	short snd_disable_0
	call	sb16_stop
	jmp	short snd_disable_2
snd_disable_0:
	call	ac97_stop
	jmp	short snd_disable_2
snd_disable_1:
	call	vt8233_stop
snd_disable_2:
	mov	al, [audio_intr]
	sub	ebx, ebx ; 0 = reset
	call	set_dev_IRQ_service

	;mov	al, [audio_intr]
	sub	ah, ah ; 0 = reset
	call	set_hardware_int_vector

	xor	eax, eax
	mov	byte [audio_device], al
	mov	byte [audio_intr], al
	xchg	eax, [audio_dma_buff]

	; 24/05/2017
	;or	eax, eax
	;jz	short snd_disable_3
	;cmp	eax, sb16_dma_buffer ; default DMA buffer
	;je	short snd_disable_3
	cmp	byte [audio_pci], 0 ; AC97 audio controller ?
	jna	short snd_disable_3
	mov	byte [audio_pci], 0

	; 23/08/2024 - bugfix
	cmp	eax, sb16_dma_buffer	; reserved buffer ?
	je	short snd_disable_3 ; it isn't an allocated mem buff

	;sub	ecx, ecx
	;xchg	ecx, [audio_dmabuff_size]
	mov	ecx, [audio_dmabuff_size]
	; 26/11/2023 
	; round up (always -rounded up- page count is allocated)
	; ((so deallocation must be done for the rounded up value))
	;add	ecx, PAGE_SIZE - 1   ; 4095
	;call	deallocate_memory_block
	; 04/06/2024
	;call	deallocate_memory_block_x 
			; deallocate ((ecx+4095)>>12) pages
	call	deallocate_memory_block
snd_disable_3:
	retn

sound_dma_map:
	; FUNCTION = 13 
	; Map audio dma buff addr to user's buffer addr
	; 30/07/2022
	; 12/05/2017
	and	ecx, ecx
	;jz	sound_buff_error
	; 30/07/2022
	jnz	short snd_dma_map_3
	jmp	sound_buff_error
snd_dma_map_3:
	cmp	byte [audio_device], 1
	jb	short snd_dma_map_1
snd_dma_map_0:
	mov	eax, [audio_dma_buff]
	and	eax, eax
	jz	short snd_dma_map_1
	;
	mov	bl, [audio_user]
	or	bl, bl
	jz	short snd_dma_map_1
	cmp	bl, [u.uno]
	;jne	sndc_owner_error
	; 30/07/2022
	je	short snd_dma_map_4
	jmp	sndc_owner_error
snd_dma_map_4:
	mov	ebx, [audio_dmabuff_size]
	and	ebx, ebx
	jnz	short snd_dma_map_2
snd_dma_map_1:
	mov	eax, sb16_dma_buffer
	mov	ebx, 65536
snd_dma_map_2:	
	add	ecx, PAGE_SIZE-1 ; 4095
	and	cx, ~PAGE_OFF ; not 4095
	cmp	ecx, ebx
	;ja	sound_buff_error
	; 30/07/2022
	jna	short snd_dma_map_6
snd_dma_map_5:
	jmp	sound_buff_error
snd_dma_map_6:
	push	eax
	mov	ebx, edx
	shr	ecx, 12 ; byte count to page count
	; eax = physical address of (audio) dma buffer
	; ebx = virtual address of (audio) dma buffer (user's pgdir)
	; ecx = page count (>0)
	call	direct_memory_access
	pop	eax
	;jc	sound_buff_error
	; 30/07/2022
	jc	short snd_dma_map_5
	mov	[u.r0], eax
	retn

soundc_info:
	; FUNCTION = 14 
	; Get Audio Controller Info
	; 19/11/2023
	; 30/07/2022
	; 10/06/2017
	; 05/06/2017
	
	;and	bl, bl ; 0
	;jz	short sndc_info_0
	
	; 19/11/2023
	cmp	bl, 1
	jna	short sndc_info_0

; invalid parameter !
; 30/07/2022
	;mov	eax, ERR_INV_PARAMETER ; 23
;sndc_inf_error:
;	mov	[u.r0], eax
;	mov	[u.error], eax
;	jmp	error
	; 30/07/2022
	sub	eax, eax
	mov	al, ERR_INV_PARAMETER ; 23
	jmp	sysaudio_err

sndc_info_0:
	call	snd_dev_check
	;jc	soundc_dev_err
	; 30/07/2022
	jnc	short sndc_info_3
snd_data_dev_err:
	jmp	soundc_dev_err
sndc_info_3:
	; 19/11/2023
	;cmp	bl, 1
	;je	short sndc_info_4
	and	bl, bl
	jnz	short sndc_info_4

	mov	ebx, [audio_vendor]
	mov	ecx, [audio_dev_id]
	;mov	al,  [audio_device]
	cmp	al, 2 ; AC'97 (ICH)
	jne	short sndc_info_1
	; Intel AC97 (ICH) Audio Controller (=2)
	mov	dx, [NABMBAR]
	shl	edx, 16
	mov	dx, [NAMBAR]
	jmp	short sndc_info_2
sndc_info_1:
	; 05/06/2017
	; Note: Intel HDA code (here) is not ready yet!
	; !!! SB16 or VT8233 (VT8237R) !!!
	movzx	edx, word [audio_io_base]
sndc_info_2:
	mov	ah, al ; [audio_device]
	mov	al, [audio_intr] 

	; EAX = IRQ Number in AL
	;	Audio Device Number in AH 
	; EBX = DEV/VENDOR ID
	;	(DDDDDDDDDDDDDDDDVVVVVVVVVVVVVVVV)
	; ECX = BUS/DEV/FN 
	;	(00000000BBBBBBBBDDDDDFFF00000000)
	; EDX = NABMBAR/NAMBAR (for AC97)
	;	(Low word, DX = NAMBAR address)
	; EDX = Base IO Addr (DX) for SB16 & VT8233

	; 10/06/2017
	mov	[u.r0], eax
	mov	ebp, [u.usp]
	mov	[ebp+16], ebx  ; ebx
	mov	[ebp+20], edx  ; edx
	mov	[ebp+24], ecx  ; ecx

 	retn

sndc_info_4:
	; 19/11/2023
	cmp	al, 2 ; Intel AC97 (ICH) Audio Controller
	je	short sndc_info_7
sndc_info_5:
	; return ZERO if it is not AC97 audio controller codec
	xor	eax, eax ; 0
	xor	ebx, ebx ; 0
sndc_info_6:
	mov	[u.r0], eax
	mov	ebp, [u.usp]
	mov	[ebp+16], ebx  ; ebx
	retn
sndc_info_7:
	call	ac97_codec_info
	jc	short sndc_info_5

	; 26/11/2023 - temporary
	;and	al, 0FEh	; clear VRA support bit for test

	jmp	short sndc_info_6

sound_data:
	; FUNCTION = 15 
	; Get Current Sound data for graphics
	; 30/07/2022
	; 22/06/2017
	;
	call	snd_dev_check
	;jc	soundc_dev_err ; Device not ready !
	; 30/07/2022
	jc	short snd_data_dev_err

	cmp	bl, 0
	jna	short sound_data_0

	; Only PCM OUT buffer data is valid for now!
	mov	eax, ERR_INV_PARAMETER  ; 23
	jmp	sysaudio_err

sound_data_0:
	mov	eax, [audio_dma_buff]
	or	eax, eax
	;jz	sound_buff_error
	; 30/07/2022
	jz	short sound_data_5

	cmp	byte [audio_device], 4 ; Intel HDA
	je	short sound_data_4 ; temporary ! (22/06/2017)

	and	ecx, ecx
	;jnz	short sound_data_1 ; sample transfer
	
	; Return only DMA Buffer pointer/offset... 
	; (If DMA Buffer has been mapped to user's
	;  memory space; program can get graphics
	;  data by using only this pointer value.)
	   	
	;call	get_dma_buffer_offset
	;; eax = DMA buffer offset
	;;	(!not half buffer offset!)
	;mov	[u.r0], eax
	;retn

	;jz	get_dma_buffer_offset
	; 30/07/2022
	jnz	short sound_data_1
	jmp	get_dma_buffer_offset
	
sound_data_1:
	;mov	eax, [audio_dmabuff_size]
	;shr	eax, 1 ; half buffer size
	;cmp	ecx, eax
	;ja	short sound_buff_error

	cmp	ecx, [audio_dmabuff_size]
	;ja	sound_buff_error
	; 30/07/2022
	jna	short sound_data_6
sound_data_5:
	jmp	sound_buff_error	
sound_data_6:
	mov	eax, edx
	and	eax, PAGE_OFF ; 4095 (0FFFh)
	cmp	ecx, 4096
	jna	short sound_data_2
	;mov	ecx, 4096 ; max. 1 page
	; 30/07/2022
	xor	ecx, ecx
	mov	ch, 16
	; ecx = 4096
sound_data_2:
	add	eax, ecx
	cmp	eax, 4096
	jna	short sound_data_3
	and	ax, PAGE_OFF ; 4095 (0FFFh)
	sub	ecx, eax
	; here, ECX has been adjusted to fit 
	;	in page border..  (<= 4096, >0)
sound_data_3:
	push	ecx
	push	edx
	mov	ebx, edx 
	call	get_physical_addr
	pop	edx
	pop	ecx
	;jc	sound_buff_error
	; 30/07/2022
	jc	short sound_data_5
	
	; eax = physical address of user's buffer
	mov	ebx, eax  
	; ecx = byte (transfer) count
	;call	get_current_sound_data
	;retn
	jmp	get_current_sound_data

sound_data_4:
	; Intel HDA code is not ready yet !
	; 22/06/2017
	xor	eax, eax
	dec	eax
	mov	[u.r0], eax ; 0FFFFFFFFh
	retn 

snd_dev_check:
	; 10/06/2017
	; 05/06/2017
	; 24/05/2017
	; 22/04/2017
	; 21/04/2017
	; ... device check at first
	mov	al, [audio_device]
	cmp	al, 1 ; SB 16
	jb	short snd_dev_chk_retn ; error !
	;cmp	al, 4 ; Intel HDA
	;ja	short snd_dbchk_stc ; invalid !
	; 10/06/2017
	cmp	al, 5
	cmc
snd_dev_chk_retn:
	retn

snd_buf_check:
	; 10/06/2017
	; 22/04/2017
	; 21/04/2017
	; ... buffer & (buffer) owner check at second
	cmp	dword [audio_buffer], 0
	jna	short snd_dbchk_stc
snd_user_check:
	mov	al, [u.uno]
	cmp	al, [audio_user]
	;jne	short snd_dbchk_stc
	;retn
	je	short snd_dev_chk_retn

snd_dbchk_stc:
	stc
	retn

sound_update:
	; FUNCTION = 16 
	; bl = 
	;    0 = automatic (sequental) update (with flag switch!)
	;    1 = update dma half buffer 1 (without flag switch!)
	;    2 = update dma half buffer 2 (without flag switch!)
	;  FFh = get current flag value	
	;      0 = dma half buffer 1 (will be played next)
	;      1 = dma half buffer 2 (will be played next)
	
	; 05/06/2024
	; 30/07/2022
	; 10/10/2017
	; ... device check at first
	mov	al, [audio_device]
	or	al, al ; 0 ; pc speaker or invalid 
	;jz	soundc_dev_err
	; 30/07/2022
	jnz	short snd_update_4
	jmp	soundc_dev_err
snd_update_4:
	; ... buffer & (buffer) owner check at second
	cmp	dword [audio_buffer], 0
	;jna	sound_buff_error
	; 30/07/2022
	jna	short snd_update_6 ; jmp sound_buff_error
	mov	al, [u.uno]
	cmp	al, [audio_user]
	;jne	sndc_owner_error
	; 30/07/2022
	je	short snd_update_5
	jmp	sndc_owner_error
snd_update_5:
	; Transfer ring 3 (user's) audio buffer content to dma buffer
	mov	edi, [audio_dma_buff] ; dma buffer (ring 0)
	or	edi, edi
	;jz	sound_buff_error
	; 30/07/2022
	jnz	short snd_update_7
snd_update_6:
	jmp	sound_buff_error
snd_update_7:
	mov	esi, [audio_p_buffer] ; physical address (ring 3)
	; 05/06/2024
	;mov	ecx, [audio_buff_size]
	;
	;;;
	; 04/06/2024
	;mov	al, [audio_device]
	;cmp	al, 1
	;je	short snd_update_8 ; SB16
	;and	cl, ~1 ; word alignment
	;cmp	al, 3
	;je	short snd_update_8 ; VIA VT8233
	;; al = 2 ; AC97
	;and	cl, ~7 ; 8 byte alignment
;snd_update_8:
	;;;

	; 05/06/2024
	mov	ecx, [dma_hbuff_size] ; DMA half buffer size

	;movzx	eax, byte [audio_flag]
	mov	al, [audio_flag]

	inc	bl
	jz	short snd_update_3 ; bl = 0FFh
	dec	bl 
	jz	short snd_update_0 ; bl = 0

	cmp	bl, 2
	je	short snd_update_1 ; dma half buffer 2
	jb	short snd_update_2 ; dma half buffer 1
 
	; invalid parameter !
; 30/07/2022
	;mov	eax, ERR_INV_PARAMETER ; 23
;	mov	[u.r0], eax
;	mov	[u.error], eax
;	jmp	error

	; 30/07/2022
	sub	eax, eax
	mov	al, ERR_INV_PARAMETER ; 23
	jmp	sysaudio_err
	
snd_update_0:
	xor	byte [audio_flag], 1 ; update flag !!!
	cmp	al, 1
	jb	short snd_update_2 ; dma half buffer 1
snd_update_1:
	; dma half buffer 2
	add	edi, ecx
snd_update_2:
	;rep	movsb
	; 05/06/2024
	;shr	ecx, 2
	;rep	movsd
	rep	movsb ; SB16, AC97, VT8233
snd_update_3:
	mov	[u.r0], eax

	retn

sound_getvol:
	; FUNCTION = 17
	; Get sound volume level
	; 24/05/2024
	; bl = component
	;		0 = master/playback/lineout volume
	;		1 = PCM out volume
	; Return:
	; cl = left channel volume level (0 to 31)
	; ch = right channel volume level (0 to 31)

	cmp	bl, 1
	ja	short snd_gvol_0 ; temporary ; 24/05/2024
	je	short snd_gvol_1
	mov	cx, [audio_master_volume]
snd_gvol_0:
	retn
snd_gvol_1:
	mov	cx, [audio_pcmo_volume]
	retn

set_irq_callback_service:
	; 23/11/2023
	; 20/11/2023 (TRDOS 386 Kernel v2.0.7)
	; 30/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 03/08/2020
	; 10/06/2017
	; 12/05/2017
	; 24/04/2017
	; 22/04/2017
	; caller: 'syscalbac' or 'sysaudio' or ...
	; 13/04/2017, 14/04/2017, 17/04/2017
	; 24/02/2017, 26/02/2017, 28/02/2017
	; 21/02/2017 - TRDOS 386 (TRDOS v2.0)
	;
	; Link or unlink IRQ callback service to/from user (ring 3)
	;
	; INPUT ->
	;	If AL = 0, the caller is 'syscalbac';
	;	   otherwise, the caller is 'sysaudio' or ...
	;	   (AL = user number) 
	;
	;	BL = IRQ number (Hardware interrupt request number)
	;	     (0 to 15 but IRQ 0,1,2,6,8,14,15 are prohibited)
	;	     IRQ numbers 3,4,5,7,9,10,11,12,13 are valid
	;	     (numbers >15 are invalid)
	;
	;	BH = 0 = Unlink IRQ (in BL) from user (ring 3) service
	;	     1 = Link IRQ by using Signal Response Byte method
	;	     2 = Link IRQ by using Callback service method
	;	     3 = Link IRQ by using Auto Increment S.R.B. method
	;	    >3 = invalid 
	;		 (syscallback version will return to user)
	;
	;	CL = Signal Return/Response Byte value 
	;
	;	If BH = 3, kernel will put a counter value ; 03/08/2020
	;	          (into the S.R.B. addr)
	;		  between 0 to 255. (start value = CL+1)
	;	
	;	NOTE: counter value, for example: even and odd numbers
	;	      may be used for -audio- DMA buffer switch 
	;	      within double buffer method, etc.
	;
	;	EDX = Signal return (Response) byte address
	;	       		  - or -
	;	      Interrupt/Callback service/routine address
	;
	;	      (virtual address in user's memory space)
	;
	; OUTPUT ->
	;	CF = 0 & EAX = 0 -> Successful setting
	;	CF = 1 & EAX > 0 -> IRQ is prohibited or locked
	;			by another process
	;		 eax = ERR_PERM_DENIED -> prohibited or locked
	;		 eax = ERR_INV_PARAMETER -> 
	;		       invalid parameter/option or bad address
	;
	; TRDOS 386 - IRQ CALLBACK structures (parameters):
	;
	;	   [u.irqlock]	: 1 word, IRQ flags (0-15) that indicates
	;			which IRQs are locked by (that) user.
	;		        Lock and unlock (by user) will change
	;			these flags or 'terminate process' (sysexit)
	;			will clear these flags and unlock those IRQs.
	;
	;		   	Bit 0 is for IRQ 0 and Bit 15 is for IRQ 15
	;
	;	   IRQ(x).owner	: 1 byte, user, [u.uno], 0 = free (unlocked)
	;
	;	   IRQ(x).method : 1 byte for callback method & status
	;			   0 = Signal Response Byte method
	;			   1 = Callback service method
	;			   >1 = invalid for current 'syscalback'.
	;			or(+) 80h = IRQ is in use by system (ring 0)
	;			            function (audio etc.) or
	;			   	    a device driver.
	;			(system function will ignore the lock/owner)
	;
	;	   IRQ(x).srb	: 1 byte, Signal Return/Response byte value
	;			  (a fixed value by user or a counter value
	;			 from 0 to 255, which is increased by every
	;			 interrupt just before putting it into 
	;			 the Signal Response byte address)
	;			 (This is not used in callback serv method)
	;
	;	   IRQ(x).addr	: 1 dword
	;			  Signal Response Byte address (physical)
	;			  		-or-
	;			  Callback service address (virtual)
	;
	;	   IRQ(x).dev	: 1 byte
	;			  0 = Default device or kernel function
	;			  		-or-
	;			  1-255 = Assigned device driver number
	;
	;	   (x) = 3,4,5,7,9,10,11,12,13
	;
	
	cmp	bl, 15
	ja	short scbs_2
	
	cmp	bh, 3
	ja	short scbs_2  ; invalid parameter

	movzx	edi, bl ; save IRQ number

		; IRQ 0,1,2,6,8,14,15 are prohibited
	;IRQenum: ; 'trdosk9.s'
	;	db  0,0,0,1,2,3,0,4,0,5,6,7,8,9,0,0

	movzx	esi, byte [edi+IRQenum] ; IRQ availability
					; enumeration/index
	; 20/11/2023
	dec	esi
	;dec	si
	js	short scbs_1 ;  0 -> 0FFFFh  

	; ESI = IRQ callback parameters index number (0 to 8)

	or	bh, bh
	jz	short scbs_4 ; unlink the IRQ (in BL)

	dec	bh
	; bh = method (0 = signal response byte, 1 = callback)
	;	      (2 = auto increment of signal response byte)

	cmp	byte [esi+IRQ.owner], 0 ; locked ?
	jna	short scbs_6	; no... OK...

scbs_1:
	; permission denied (prohibited IRQ)
	;mov	eax, ERR_PERM_DENIED
	; 30/07/2022
	sub	eax, eax
	mov	al, ERR_PERM_DENIED
	stc
	retn
scbs_2:
	;stc
scbs_3:
	;mov	eax, ERR_INV_PARAMETER
	; 30/07/2022
	sub	eax, eax
	mov	al, ERR_INV_PARAMETER
	stc
	retn

scbs_4: ; unlink the requested IRQ (if it belongs to current user)
	; 10/06/2017
	; 22/04/2017
	; 14/04/2017
	; If AL = 0 -> The caller is 'syscalbac'
	mov	ah, [esi+IRQ.owner]
	cmp	ah, [u.uno]
	jne	short scbs_1

	dec	byte [u.irqc] ; decrease IRQ count (in use)

	;sub	ah, ah
	;mov	[esi+IRQ.owner], ah ; 0 ; free !!!
	;and	byte [esi+IRQ.method], 80h
	;mov	[esi+IRQ.srb], ah ; 0
	;mov	[esi+IRQ.dev], ah ; 0
	;mov	dword [esi+IRQ.addr], 0
	;mov	dword [u.r0], 0

	;mov	byte [esi+IRQ.owner], 0

	; 22/04/2017
	sub	eax, eax
	mov	[esi+IRQ.owner], al ; 0
	; 10/06/2017
	xchg	al, [esi+IRQ.method]
	and	al, 80h
	jz	short scbs_12 
	; Audio device must be disabled -later- ! ([IRQ.medhod] = 80h)

;	cmp	byte [esi+IRQ.method], 80h ; device drv or kernel extension ?
;	jb	short scbs_12 ; bh = 0 reset to default IRQ handler
;
;	and	al, al
;	jz	short scbs_5  ; the caller is 'syscalbac'
;	; The caller is 'sysaudio' or ...
	xor	al, al
;	mov	[esi+IRQ.method], al ; 0 ; reset kernel extension flag
;scbs_5:
;	sub	ah, ah
	;mov	[u.r0], eax ; 0
	retn

scbs_6:
	; 14/04/2017
	and	al, al
	jz	short scbs_7  ; the caller is 'syscalbac'
	; AL = user number ([u.uno] or [audio.user] or ...)
	; The caller is 'sysaudio' or ...
	;	
	; bh = method (0 = signal response byte, 1 = callback)
	;	      (2 = auto increment of signal response byte) 

	or	bh, 80h		; Kernel extension flag !
	jmp	short scbs_8
scbs_7:	
	mov	al, [esi+IRQ.method] ; >= 80h = kernel is using this IRQ
	and	al, 80h ; use only bit 7 (kernel function flag)
	or	bh, al		 ; method 
				 ; 0 = signal response byte, 1 = callback
				 ; 2 = auto increment of s.r.b.
scbs_8:
	mov	al, [u.uno] ; user (process) number (1 to 16)
	mov	[esi+IRQ.owner], al ; lock the IRQ for user
	mov	[esi+IRQ.method], bh

;	test	bh, 1
;	jnz	short scbs_9 	 ; Callback method, CX will not be used
;
;	test	bh, 2		 ; use auto increment (counter) method
;	jz	short scbs_10	 ; (count can be used for buffer switch)
;scbs_9:
;	xor	ecx, ecx ; 0

scbs_10:
	;mov	[esi+IRQ.method], bh
	mov	[esi+IRQ.srb], cl
	mov	byte [esi+IRQ.dev], 0 ; device number is always 0
				 ; for this system call
	;test	bh, 1
	and	bh, 1 ; 17/04/2017
	jnz	short scbs_11	 ; callback method, use virtual address

	push	ebx ; IRQ number (in BL)
	mov	ebx, edx
	; ebx = virtual address
	; [u.pgdir] = page directory's physical address
	inc	 byte [no_page_swap] ; 1 
			; Do not add this page to swap queue
			; and remove it from swap queue if it is
			; on the queue.
	call	get_physical_addr
	pop	ebx
	jc	short scbs_3 ; invalid address !
	; eax = physical address of the virtual address in user's space
	mov	edx, eax
scbs_11:
	;shl	si, 2		; byte (index) to dword (offset)
	; 23/11/2023
	shl	esi, 2
	mov	[esi+IRQ.addr], edx

	inc	byte [u.irqc]	; increase IRQ (in use) count

	inc	bh ; 17/04/2017
	; bh > 0 -> set to requested IRQ handler (IRQ_u_list)
scbs_12:
	mov	al, bl ; IRQ number
	mov	ah, bh ; 0 = reset, >0 = set
	call	set_hardware_int_vector

	xor	eax, eax
	;mov 	[u.r0], eax ; 0	

	retn	; return with success (cf=0, eax=0)

sysdma: ; DMA FUNCTIONS
	; 08/08/2022
	; 30/07/2022 - TRDOS 386 Kernel v2.0.5
	; 02/09/2017
	; 28/08/2017
	; 20/08/2017 - TRDOS 386 (TRDOS v2.0)
	;
	; Inputs:
	;	BH = 0 -> Allocate DMA buffer
	;	   BL = 0 -> Use the system's default DMA
	;		     (SB16) Buffer
	;	        Buffer Size (max.) = 65536 bytes
	;	   BL > 0 -> Allocate (a new) DMA buffer
 	;	   ECX = DMA Buffer Size in bytes (<=128KB)
	;	   EDX = Virtual Address of DMA buffer
	;
	;	BH = 1 -> Initialize (Start) DMA service
	;	     BL, bit 0 to 3 = Channel Number (0 to 7)
	;	     BL, bit 7 = Auto Initialized Mode
	;			(If bit 7 is set)
	;		 bit 6 = Record (read) mode (0= playback)
	;	     ECX = byte count (0 = use dma buffer size)
	;	     EDX = physical buffer address
	;	     	   (0 = use dma buffer -start- address)
	;
	;	BH = 2 -> Get Current DMA Buffer Offset
	;	     BL = DMA channel number	
	;
	;	BH = 3 -> Get Current DMA count down value
	;	     BL = DMA channel number (0 tO 7)
	;
	;	BH = 4 -> Get Current DMA channel (in progress)
	;
	;	BH = 5 -> Get System's Default DMA Buffer Address
	;
	;	BH = 6 -> Get Current DMA Buffer Address
	;
	;	BH = 7 -> Stop DMA service
	;
	; Outputs:
	;
	;	For BH = 0 ; Allocate DMA buffer
	;	    EAX = Physical address of DMA buffer
	;	    ECX = Allocated buffer size in bytes
	;		  - page count * 4096 -
	;		  (may be bigger than requested)
	;	    If BL input > 0,
	;	       'sysalloc:' system call will be used with
  	;	       EBX (for 'sysalloc') = EDX (for 'sysdma')
	;	       ECX is same, byte count (buffer size)
	;	       EDX = 1024*1024*16 ; 16 MB upper limit
	;	    If BL input = 0,
	;	       Default DMA buffer (SB16 buffer) will be
	;	       checked and if it is free, it's address
	;	       will be returned in EAX and it's size
	;	       will be returned in ECX (as 65536)
	;
	;	    If CF = 1, error code is in EAX
	;	       EAX = -1 ; DMA buffer allocation error!
	;	       EAX = 11 ; 'Permission Denied' error !
	;
	;	       Note: 'sysalloc' error return method
	;		      will be applied if BL input > 0 !
	;
	; 	 For BH = 1 ; Initialize (Start) DMA
	;	     EAX = 0 (Successful)
	;	     If CF = 1, error code is in EAX
	;
	; 	 For BH = 2 ; Get Current DMA Buffer Offset
	;	     EAX = DMA Buffer Offset (in bytes)
	;	     ;
	;	      AX = DMA buffer offset
	;	     EAX bits 16 to 23 = Page register value
	;
	; 	 For BH = 3 ; Get Current DMA count down value
	;	     EAX = Count down value (remain bytes)
	;	
	;	 For BH = 4 ; Get Current DMA channel (in progress)
	;	     EAX = DMA channel number (0 to 7)
	;		AH = 0 if the owner is the caller process
	;		AH > 0 if the dma channel is in use by
	;		       another user/process
	;	     EAX = -1 (0FFFFFFFFh) 
	;		    if DMA service is not in use
	;	 	    (stopped or not initialized/started)
	;
	;	 For BH = 5 ; Get System's Default DMA Buff Addr
	;	     EAX = Default DMA Buffer Address (Physical)
	;		 = offset 'sb16_dma_buffer:'
	;	     ECX = Buffer size
	;		 = 65536
	;
	;	 For BH = 6 ; Get Current DMA Buffer Address
	;	     EAX = Current DMA buffer address (Physical)
	;	     ECX = Current DMA buffer size (setting value)
	;	     Note: These values are for current dma channel
	;		   settings for the user/process
	;	     ** For now (for current TRDOS 386 version)
	;		only one user/process can use only one
	;		dma channel & one dma buffer at same time
	;		(no multi tasking on DMA service) !!! **
	;	     (Once, current DMA user must stop it's own DMA
	;	      DMA service, than another user/program 
	;	      can use DMA service with same dma channel
	;	      or with another DMA channel.)
	;
	;	 For BH = 7 ; Stop DMA service (for current user
	;	     and current DMA channel)
	;	     EAX = 0 ; successful
	;	     CF = 1 & EAX > 0 (= -1) -> Error

	cmp	bh, 7
	jna	short sysdma_0

sysdma_err:
	xor	eax, eax
	dec	eax ; -1
sysdma_perm_err:
	mov	[u.r0], eax
	mov	[u.error], eax ; DMA service error !
	jmp	error

sysdma_0:
	or	bh, bh
	jnz	short sysdma_1 ; 30/07/2022
	
	and	bl, bl
	jz	short sysdma_01

	; redirect system call to 'sysalloc'
	mov	ebx, edx ; virtual address of DMA buffer
	;ecx = Buffer size in bytes
	; DMA buffer address <= 16MB upper limit
	mov	edx, 1024*1024*16 ; 16MB limit for DMA buff

	mov	dword [dma_addr], 0FFFFFFFFh ; -1

	jmp	sysalloc

sysdma_01:
	mov	eax, sb16_dma_buffer

	cmp	byte [audio_device], 1
	jb	short sysdma_03

	cmp	eax, [audio_dma_buff]
	jne	short sysdma_02

sysdma_0_err:
	mov	eax, ERR_PERM_DENIED
	jmp	short sysdma_perm_err

	; 30/07/2022
sysdma_1:
	cmp	bh, 1
	;ja	sysdma_5
	; 30/07/2022
	jna	short sysdma_10
	jmp	sysdma_5

sysdma_10:
	test	bl, 40h	; record (read) mode -BL, bit 6-
	;jnz	sysdma_err ; not ready yet!
	; 30/07/2022
	jnz	short sysdma_06 ; jmp sysdma_err

	mov	eax, [dma_addr] ; physical address of dma buffer
	and	eax, eax
	;jz	sysdma_err
	; 30/07/2022
	jz	short sysdma_06 ; jmp sysdma_err

	or	edx, edx
	jnz	short sysdma_11

	mov	edx, eax
	jmp	short sysdma_12

sysdma_02:
	; Only one user is permitted for audio/dma functions

	cmp	dword [audio_dma_buff], 0
	jna	short sysdma_03

	mov	bl, [audio_user]
	or	bl, bl
	jz	short sysdma_03

	cmp	bl, [u.uno]
	jne	short sysdma_0_err

sysdma_03:
	mov	bl, [dma_user]
	and	bl, bl
	jnz	short sysdma_04
	
	mov	bl, [u.uno]
	mov	[dma_user], bl

	jmp	short sysdma_05

sysdma_04:
	mov	esi, [dma_addr]
	and	esi, esi
	jz	short sysdma_05

	inc	esi ; -1 -> 0
	jz	short sysdma_05

	cmp	bl, [u.uno]
	jne	short sysdma_0_err
	
sysdma_05:
	; edx = virtual address (user's buffer address)
	; 
	cmp	ecx, 65536   ; byte count (buffer size)
	;ja	sysdma_err
	; 30/07/2022
	ja	short sysdma_06 ; jmp sysdma_err
	;
	add	ecx, PAGE_SIZE-1 ; 4095
	and	cx, ~PAGE_OFF ; not 4095
	;cmp	ecx, 65536
	;ja	sysdma_err
	push	ecx	; buffer size (allocated pages * 4096)
	push	eax	; offset sb16_dma_buffer
	mov	ebx, edx
	shr	ecx, 12 ; byte count to page count
	; eax = physical address of (audio) dma buffer
	; ebx = virtual address of (audio) dma buffer (user's pgdir)
	; ecx = page count (>0)
	call	direct_memory_access
	pop	eax
	pop	ecx
	;jc	sysdma_err
	; 30/07/2022
	jnc	short sysdma_07
sysdma_06:
	jmp	sysdma_err

sysdma_11:
	cmp	edx, eax
	;jb	sysdma_err
	; 30/07/2022
	jb	short sysdma_06 ; jmp sysdma_err
sysdma_12:
	and	ecx, ecx
	jnz	short sysdma_13

	mov	ecx, [dma_size]
	jmp	short sysdma_14

sysdma_07:
	mov	[dma_addr], eax
	mov	[dma_size], ecx ; dma buffer size (in bytes)

	;mov	[u.r0], eax ; DMA Buffer Address (Physical)

	;mov	ebp, [u.usp]  ; ebp points to user's registers
	;mov	[ebp+24], ecx ; return to user with ecx value

	;jmp	sysret

	; 28/08/2017
	jmp	sysdma_51

sysdma_13:
	cmp	ecx, [dma_size]
	;ja	sysdma_err
	; 30/07/2022
	ja	short sysdma_06 ; jmp sysdma_err
sysdma_14:
	mov	esi, eax
	add	esi, [dma_size]

	mov	eax, edx
	add	eax, ecx
	;jc	sysdma_err ; 02/09/2017
	; 30/07/2022
	jc	short sysdma_06 ; jmp sysdma_err
		
	cmp	eax, esi
	;ja	sysdma_err
	; 30/07/2022
	ja	short sysdma_06 ; jmp sysdma_err

	mov	edi, [audio_dma_buff]
	mov	esi, [dma_addr]

	or	edi, edi
	jz	short sysdma_16
	
	cmp	byte [audio_device], 1
	jb	short sysdma_15

	; Sound Blaster 16
	cmp	esi, edi
	;je	sysdma_0_err ; permmission denied !
	; 30/07/2022
	jne	short sysdma_15
	jmp	sysdma_0_err
sysdma_15:
	mov	byte [dma_mode], 48h ; single mode playback

	test	bl, 80h ; DMA mode - BL, bit 7, auto init -
	jz	short sysdma_16	
	; Auto initialized playback (write) mode
	add	byte [dma_mode], 10h ; = 58h
sysdma_16:
	and	bl, 07h
	mov	[dma_channel], bl
	mov	[dma_start], edx
	mov	[dma_count], ecx
	 
	; 28/08/2017
	;call	dma_init
	;jmp	sysret
	jmp	dma_init

sysdma_5:
	cmp	bh, 5
	jb	short sysdma_3
	ja	short sysdma_6

	; Get the system's default dma buffer addr and size
	mov	eax, sb16_dma_buffer
	mov	ecx, 65536 ; Buffer size in bytes

sysdma_51:
	; 0 = there is not a dma buffer (in use or available)
	mov	[u.r0], eax

	mov	ebp, [u.usp]  ; ebp points to user's registers
	mov	[ebp+24], ecx ; return to user with ecx value

	jmp	sysret

sysdma_2:
	; Get current dma buffer offset (& page)
	; 28/08/2017
	movzx	esi, byte [dma_channel]
	movzx	edx, byte [dma_flip+esi]
	out	dx, al			; flip-flop clear
	mov	dl, [dma_adr+esi]
	in	al, dx			; get dma position
	movzx	ebx, al
	in	al, dx
	mov	bh, al

	cmp	si, 4	; channel number ?
	jb	short sysdma_21 ; 8 bit dma channel

	shl	ebx, 1	; word offset to byte offset

sysdma_21:
	mov	[u.r0], ebx

	mov	dl, [dma_page+esi]
	in	al, dx			; get dma page

	;add	[u.ro+2], al
	or	[u.r0+2], al

	jmp	sysret

sysdma_6:
	cmp	bh, 6
	ja	short sysdma_7

	; 28/08/2017
	; Get current DMA buffer addr and size
	mov	eax, [dma_addr] ; dma buffer address
	mov	ecx, [dma_size] ; dma buffer size (in bytes)

	jmp	short sysdma_51

sysdma_3:
	cmp	bh, 3
	jb	short sysdma_2
	ja	short sysdma_4

	; Get current dma count down value (remain bytes)
	; 28/08/2017
	movzx	esi, byte [dma_channel]
	movzx	edx, byte [dma_flip+esi]
	out	dx, al			; flip-flop clear
	mov	dl, [dma_cnt+esi] ; dma count register addr
	in	al, dx
	movzx	ebx, al
	in	al, dx
	mov     bh, al
	
	cmp	si, 4	; channel number ?
	jb	short sysdma_31 ; 8 bit dma channel

	shl	ebx, 1	; word count to byte count

sysdma_31:
	mov	[u.r0], ebx

	jmp	sysret

sysdma_4:
	; Get current DMA channel number
	; 28/08/2017
	mov	ah, [dma_user]
	and	ah, ah
	jnz	short sysdma_42

sysdma_41:	
	; Not a valid dma channel (in use)
	mov	dword [u.r0], -1 ; 0FFFFFFFFh
	jmp	sysret

sysdma_7:
	; DMA service STOP
	mov	al, [u.uno]
	cmp	al, [dma_user]
	jne	short sysdma_72
	
	sub	al, al ; 0

	mov	[dma_user], al ; clear user

	xchg	al, [dma_mode]
	and	al, al
	;jz	short sysdma_err
	jnz	short sysdma_73	

sysdma_71:
	xor	eax, eax
	mov	[u.r0], eax; 0
	jmp	sysret

sysdma_42:
	mov	esi, [dma_addr]
	and	esi, esi
	jz	short sysdma_41

	inc	esi ; -1 -> 0
	jz	short sysdma_41

	mov	al, [dma_channel]

	cmp	ah, [u.uno]
	jne	short sysdma_43

	xor	ah, ah ; DMA channel in use by current user

sysdma_43:
	mov	[u.r0], eax ; AL = dma channel number
			    ; AH > 0 if the the channel
			    ; in use by another user/process
	jmp	sysret

sysdma_72:
	; 28/08/2017
	cmp	byte [dma_user], 0
	jna	short sysdma_71 ; Nothing to do !

	cmp	dword [dma_addr], 0
	;ja	sysdma_0_err
	; 30/07/2022
	jna	short sysdma_74
	jmp	sysdma_0_err

sysdma_74:	
	mov	[dma_user], al ; reset to current user

sysdma_73:
	; 28/08/2017
	movzx	esi, byte [dma_channel]
	movzx	edx, byte [dma_mask+esi]
	mov	al, [dma_channel]
	or	al, 4
	out	dx, al

	jmp	short sysdma_71

dma_init:
	; 30/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 28/08/2017
	; 20/08/2017
	; DMA initialization
	; 14/08/2017
	; 03/08/2017, 06/08/2017, 08/08/2017
	; 02/07/2017, 13/07/2017, 16/07/2017, 30/07/2017
	; (Derived from 'DMA_INIT' procedure in SB16MOD.ASM)
	; Modified for TRDOS 386 DMA buffer allocation & initialization !

	mov	ebx, [dma_start]
	mov	ecx, [dma_count]

	movzx	esi, byte [dma_channel]

	cmp	si, 4
	jb	short gdmi1
	; 08/08/2017
	;shr	cx, 1 ; word count
	; 30/07/2022
	shr	ecx, 1
	shr	ebx, 1 ; convert byte offset to word offset
gdmi1:
	;mov	[dma_poff], bx ; 08/08/2017
	;dec	cx			; dma size = block size - 1
	; 30/07/2022
	dec	ecx	

	movzx	edx, byte [dma_mask+esi] ; 30/07/2017
	mov	al, [dma_channel]
	or	al, 4
	out	dx, al			; dma channel mask

	xor	al, al ; 0 ; any value ! 08/08/2017
	mov	dl, [dma_flip+esi]
	out	dx, al			; flip-flop clear
	
	mov	dl, [dma_mod+esi]
	mov	al, [dma_channel]  ; 13/07/2017
	and	al, 3
	; 08/08/2017
	or	al, [dma_mode] ; 58h    ; dma mode for SB16
	out	dx, al

	mov	dl, [dma_adr+esi]
	mov	al, bl
	out	dx, al			; offset low

	mov	al, bh
	out	dx, al			; offset high

	mov	dl, [dma_cnt+esi]
	mov	al, cl
	out	dx, al			; size low

	mov	al, ch
	out	dx, al			; size high

	mov	dl, [dma_page+esi]
	; 14/08/2017 
	cmp	si, 4
	jnb	short gdmi2
	shr	ebx, 16
	jmp	short gdmi3
gdmi2:
	; 09/08/2017
	shr	ebx, 15	 ; complete 16 bit shift
	and	bl, 0FEh ; clear bit 0 (not necessary)
gdmi3:	
	mov	al, bl
	out	dx, al			; page
	
	mov	dl, [dma_mask+esi]
	mov	al, [dma_channel]  ; 13/07/2017
	and	al, 3
	out	dx, al			; dma channel unmask

	;retn
	; 28/08/2017
	jmp	sysret

sysstdio: ; STDIN/STDOUT/STDERR functions
	; 18/09/2024
	; 07/09/2024
	;	(STDAUX/STDPRN functions, pre-definions)
	;	(!these functions are not ready!)
	; 24/08/2024
	; 23/08/2024
	; 20/08/2024 - TRDOS 386 Kernel v2.0.9
	;
	; Inputs:
	;	BL = 0 -> read a character on stdin (wait)
	;	BL = 1 -> read a character on stdin (no wait)
	;	BL = 2 -> write a character onto stdout (redirection)
	;	BL = 3 -> write a character onto stderr (no redirection)
	;	BL = 4 -> redirect stdin to file (if cl > 0)
	;	BL = 5 -> redirect stdout to file (if cl > 0)
	;	BL = 6 -> read character (ascii and scancode) on stdin
	;				-no redirection, wait-
	;	BL = 7 -> read character (ascii and scancode) on stdin
	;				-no redirection, no wait-
	;	BL = 8 -> write character and color onto stdout
	;				-no redirection-
	;	BL = 9 -> ungetchar (put back the ascii code in u.getc)
	;
	;	For BL=2,3,8,9
	;	    CL = character (ascii) code
	;	For BL=8
	;	    CH = color (Attribute) -CGA-
	;	For BL=4,5
	;	    CL = file descriptor number + 1
	;	    (File must be open and that number is 'u.fp' index)
	;
	;	07/09/2024 (these subfunctions will not be handled
	;		   by kernel v2.0.9 for now)
	;	 -I am writing them here for C compiler compatibility,
	;	  for now. Ref: SCC STDIO.H-
	;	BL = 10 -> get STDAUX status
	;	BL = 11 -> get/select STDAUX (COM) port
	;				(clears redirection)
	;	BL = 12 -> redirect STDAUX
	;	BL = 13 -> STDAUX IOCTL (control functions)
	;	BL = 14 -> read byte/character from STDAUX
	;	BL = 15 -> write byte/character to STDAUX
	;	BL = 16 -> get STDPRN status (LPT printer only)
	;	BL = 17 -> redirect STDPRN
	;	BL = 18 -> STDPRN IOCTL (init/control functions)
	;	BL = 19 -> write byte/character to STDPRN
	;
	;	For BL = 11
	;	    If	CL = 0 -> get STDAUX (COM) port number
	;		CL = 1 -> COM1
	;		CL = 2 -> COM2
	;		CL > 4 -> invalid
	;	For BL = 12
	;	    If ECX = 0 -> clear redirection
	;	    If  CH & 7Fh = 0 -> redirect INPUT
	;	        CH & 7Fh > 0 -> redirect OUTPUT
	;	    If  CH & 80h = 80h 
	;		CL = file handle (descriptor index)
	;	    If  CH & 80h = 0
	;		CL = console (pseudo) tty number,
	;		     1 to 8 for tty0 to tty7.
	;	For BL = 13
	;	    If CL = 0 -> get control byte
	;	    If CL > 1 -> set control byte (in CL)
	;		bit 0 = 1
	;		bit 1 = data bits (set=8, clear=7)
	;		bit 2 = stop bit (set=2, clear=1)
	;		bit 3 = parity bit (set=yes, clear=no)
	;		bit 4 = parity bit (set=even, clear=odd)
	;		bit 5-6-7 = data rate
	;			000 = 9600
	;		        001 = 19200
	;			010 = 38400
	;			011 = 57600
	;			100 = 115200
	;	For BL = 15, 19
	;	    CL = byte/character to r/w
	;	For BL = 17
	;	    If ECX = 0 -> clear redirection	
	;	    If  CH & 80h = 80h 
	;		CL = file handle (descriptor index)
	;	    If  CH & 80h = 0
	;		CL = console (pseudo) tty number,
	;		     1 to 8 for tty0 to tty7.
	;	For BL = 18
	;	    If CL = 0 -> initialize printer (command=0)
	;	       CL > 0 -> configuration/command byte
	;			 (reserved, not used)		
	;
	; Outputs:
	;
	;	For BL=0,1,2,3,6,7,8,9
	;	    AL = character (ascii) code
	;	For BL=6,7
	;	    AH = scan code
	;	For BL=4,5
	;	    AL = file descriptor (CL input - 1)	
	;
	;	If CF=1
	;	   AL (EAX) = error code
	;	   If EAX = 0 -> EOF ; 18/09/2024
	;
	;	07/09/2024
	;	For BL = 10 to 19
	;	    IF CF = 1, EAX/AL = error code 		
	;    .. If CF = 0 ...
	;	For BL = 10 & 16
	;	    AL (EAX) = status
	;	For BL = 11
	;	    EAX/AL = COM (serial) port (1 to 4)
	;	For BL = 12, 15, 17, 18, 19
	;	    EAX/AL = 0
	;	For BL = 13
	;	    If CL input = 0
	;		  EAX/AL = current ctrl/cfg byte
	;           If CL input > 0
	;		  EAX/AL = ctrl/cfg byte
	;	For BL = 14
	;	    EAX/AL = byte/character
	;			

	cmp	bl, (end_of_stdiofuncs-stdiofuncs)>>2
	jb	short sysstdio_0
	
	; invalid parameter (invalid sub function number)
	mov	eax, ERR_INV_PARAMETER
sysstdio_err:
	mov	[u.r0], eax
	mov	[u.error], eax
	jmp	error

sysstdio_0:
	xor	eax, eax ; 0
	mov	[u.error], eax ; clear previous error code	
	mov	[u.r0], eax ; clear previous return code

	movzx	esi, bl
	shl	esi, 2
	jmp	dword [esi+stdiofuncs]

stdiofuncs:
	dd	readstdinw
	dd	readstdinnw
	dd	writestdout
	dd	writestderr
	dd	redirstdin
	dd	redirstdout
stdinacsc:
	dd	readstdin2w
	dd	readstdin2nw
stdoutcc:
	dd	writestdoutcc
	dd	ungetchar
end_of_stdiofuncs:

redirstdin:
	; CL = file handle (descriptor/index) + 1
	or	cl, cl
	jnz	short redirstdin_0
	mov	[u.stdin], cl ; 0 ; reset STDIN to keyboard
	jmp	short redirstdin_3
redirstdin_0: 
	mov	al, cl
	call	getf
	; eax = first cluster
	; ebx = system (not user) open file index
	cmp	byte [ebx+OF_MODE], 1 ; open for read
	je	short redirstdin_1
redir_acc_denied:
	mov	eax, ERR_ACCESS_DENIED
	jmp	short sysstdio_err
redirstdin_1:
	mov	[u.stdin], cl
redirstdout_2:
	dec	ecx
	mov	[u.r0], cl ; return file descriptor
redirstdin_3:
redirstdout_3:
	mov	word [u.ungetc], 0
		; reset u.ungetc and u.getc
	jmp	sysret

redirstdout:
	; CL = file handle (descriptor/index) + 1
	or	cl, cl
	jnz	short redirstdout_0
	mov	[u.stdout], cl ; 0 ; reset STDOUT to keyboard
	jmp	short redirstdout_3
redirstdout_0: 
	mov	al, cl
	call	getf
	; eax = first cluster
	; ebx = system (not user) open file index
	cmp	byte [ebx+OF_MODE], 2 ; open for write
	jne	short redir_acc_denied
redirstdout_1:
	mov	[u.stdout], cl
	jmp	short redirstdout_2
	
readstdinw:
readstdinnw:
	; read a character on stdin (wait)
	;sub	eax, eax
	cmp	byte [u.ungetc], al ; 0
	jna	short readstdinw_0
	; read the character in u.getc buffer (put by ungetchar)
	mov	[u.ungetc], al ; 0
	xchg	al, [u.getc]
readstdinw_retn:
	mov	[u.r0], al
	jmp	sysret

readstdinw_3:
	; 23/08/2024
	mov	ah, 10h
	call	int16h
	jmp	short readstdinw_retn

readstdinw_0:
	mov	al, [u.stdin]
	or	al, al
	jz	short readstdinw_1

readstdinf:
	; 24/08/2024
	dec	eax

	; file
	mov	ebx, eax ; File handle (descriptor/index)
	; 18/09/2024
	mov	ecx, charbuf ; buffer address
	mov	edx, 1  ; 1 character only

	; 18/09/2024
	;;;
	call	getf1
	jc	short redirect_fno_err
	
readstdinf_@:
	; eax = first cluster
	; ebx = file descriptor (system, open files)

	call	rw1
	jc	short redirection_err

	inc	byte [u.kcall]
	;mov	byte [u.kcall], 1 ; >0 means system buffer
			      ; (buff addr is not virtual)
	call	readi

redirected_rw_OK:
	xor	eax, eax
	cmp	[u.nread], eax ; 0
	jna	short readstdinf_EOF

	mov	al, [charbuf]
;redirected_w_not_OK:
	mov	[u.r0], eax
	jmp	sysret

redirect_fno_err:
	mov 	eax, ERR_FILE_NOT_OPEN ; file not open !
redirection_err:
	mov	[u.error], eax
readstdinf_EOF_@:
	mov	[u.r0], eax ; error code ; 0 = EOF
	jmp	error

readstdinf_EOF:
	; 'end of file !' error 
	mov	dword [u.error], ERR_FILE_EOF ; 16
	jmp	short readstdinf_EOF_@
	;;;

readstdinw_1:
	; 18/09/2024
	and	bl, bl
	jz	short readstdinw_3 ; wait (int16h, 10h)
	; no wait (int16h, 11h)
	mov	ah,11h	; EXTENDED ASCII STATUS
	call	int16h
	; ah = scan code, al = ascii code
	;jnz	short readstdinw_retn
	; 23/08/2024
	jnz	short readstdinw_3
readstdinw_2:

; 23/08/2024
;	; if zf=1 at here
;	; it means 'no code available' for function 11h
;	and	bl, bl	; 0 ?
;	jz	short  short readstdinw_retn ; function 10h
;	; function 11h
;	; [u.r0] = 0 = eax return
readstdin2w@_retn:
	jmp	sysret

writestdoutf:
	; 24/08/2024
	dec	eax

	; file
	mov	ebx, eax ; File handle (descriptor/index)
	; 18/09/2024
	mov	byte [charbuf], cl
	mov	ecx, charbuf
	mov	edx, 1  ; 1 character only

	; 18/09/2024
	;;;
	call	getf1
	jc	short redirect_fno_err

writestdoutf_@:
	; eax = first cluster
	; ebx = file descriptor (system, open files)

	call	rw1
	jc	short redirection_err

	inc	byte [u.kcall]
	;mov	byte [u.kcall], 1 ; >0 means system buffer
			      ; (buff addr is not virtual)
	call	writei

	;xor	eax, eax
	;cmp	[u.nread], eax ; 0
	;jna	short redirected_w_not_OK
	jmp	short redirected_rw_OK
	;;;

writestderr:	; skip redirection
		; STDERR = STDOUT (as file redirection disabled)
	mov	al, cl  ; character
	mov	[u.r0], al
	mov	ah, 0Eh	; write a character (as tty write)
	mov	ebx, 07h ; video page 0 (and color/attrib 07h)
	; 23/08/2024
	mov	bh, [ptty] ; ACTIVE_PAGE
	call	_int10h
	jmp	sysret

readstdin2w:
readstdin2nw:
	mov	word [u.ungetc], 0
		; reset u.ungetc and u.getc
	mov	ah, 10h  ; Keyboard, EXTENDED READ
	;cmp	bl, 6
	cmp	bl, (stdinacsc-stdiofuncs)>>2
	je	short readstdin2w_1 ; wait (int16h, 10h)
	; no wait (int16h, 11h)
	inc	ah  ; function 11h ; EXTENDED ASCII STATUS

	; 24/08/2024
	call	int16h
	; ah = scan code, al = ascii code
	;jnz	short readstdin2w_retn
	; 23/08/2024
	jz	short readstdin2w@_retn
;readstdin2w_1:

; 23/08/2024
;	; if zf=1 at here
;	; it means 'no code available' for function 11h
;	;cmp	bl, 6
;	cmp	bl, (stdinacsc-stdiofuncs)>>2
;	;je	short readstdin2w_retn ; function 10h
;	; function 11h
;	; [u.r0] = 0 = eax return
;	;jmp	sysret
;	jne	short readstdin2w@_retn

	; 23/08/2024
	mov	ah, 10h
readstdin2w_1:	; 18/09/2024
	call	int16h

readstdin2w_retn:
	mov	[u.r0], ax
	jmp	sysret

writestdout:
	; 18/09/2024
	mov	al, [u.stdout]
	and	al, al
	jnz	short writestdoutf ; 18/09/2024

writestdout_1:
	;mov	byte [ccolor], 07h ; default color (CGA) 
			; (black background, light gray character) 
	; 18/09/2024
	mov	ch, 07h
writestdoutcc:	
	mov	esi, u.r0 ; buffer
	; 18/09/2024
	mov	[esi], cl ; character
	;;cmp	bl, 8 ; write char and color
	;cmp	bl, (stdoutcc-stdiofuncs)>>2
	;jne	short writestdout_2
	mov	byte [ccolor], ch ; color/attribute (CGA)
writestdout_2:
	call	print_cmsg
	; 18/09/2024
	;mov	byte [ccolor], 07h ; set default color again

	; [u.r0] = written character (AL)
	jmp	sysret

ungetchar:
	; put a character back in u.getc STDIN buffer
	mov	byte [u.ungetc], 1 
	mov	[u.getc], cl
	mov	[u.r0],cl
	jmp	sysret

	; 18/09/2024
charbuf: db 0

otty:
sret:
ocvt:
ctty:
cret:
ccvt:
rtty:
wtty:
rmem:
wmem:
rfd:
rhd:
wfd:
whd:
rlpt:
wlpt:
rcvt:
xmtt:
	retn