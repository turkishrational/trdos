; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel - v2.0.10) - MAIN PROGRAM : trdosk6.s
; ----------------------------------------------------------------------------
; Last Update: 20/05/2025  (Previous: 27/09/2024, v2.0.9)
; ----------------------------------------------------------------------------
; Beginning: 24/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ----------------------------------------------------------------------------
; Derived from 'Retro UNIX 386 Kernel - v0.2.1.0' source code by Erdogan Tan
; u1.s (27/17/2015), u2.s (03/01/2016)
; ****************************************************************************
; Derived from TRDOS Operating System v1.0 (8086) source code by Erdogan Tan
; TRDOS2.ASM (09/11/2011)
; ----------------------------------------------------------------------------
; INT_21H.ASM (c) 2009-2011 Erdogan TAN  [14/11/2009] Last Update: 08/11/2011

; Ref: Retro UNIX 386 v1.2 Kernel (v0.2.2.3) - ux.s - 15/07/2022

sysent: ; < enter to system call >
	; 23/07/2022 - TRDOS 386 Kernel v2.0.5
	; 17/03/2017
	; 03/03/2017
	; 19/02/2017
	; 13/01/2017
	; 06/06/2016
	; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
	; 16/04/2015 - 19/10/2015 (Retro UNIX 386 v1)
	; 10/04/2013 - 18/01/2014 (Retro UNIX 8086 v1)
	;
	; 'unkni' or 'sysent' is sytem entry from various traps. 
	; The trap type is determined and an indirect jump is made to 
	; the appropriate system call handler. If there is a trap inside
	; the system a jump to panic is made. All user registers are saved
	; and u.sp points to the end of the users stack. The sys (trap)
	; instructor is decoded to get the the system code part (see
	; trap instruction in the PDP-11 handbook) and from this
	; the indirect jump address is calculated. If a bad system call is
	; made, i.e., the limits of the jump table are exceeded, 'badsys'
	; is called. If the call is legitimate control passes to the
	; appropriate system routine.
	;
	; Calling sequence:
	;	Through a trap caused by any sys call outside the system.
	; Arguments:
	;	Arguments of particular system call.
	; ...............................................................
	;
	; Retro UNIX 8086 v1 modification: 
	;       System call number is in EAX register.
	;
	;       Other parameters are in EDX, EBX, ECX, ESI, EDI, EBP
	;	registers depending of function details.
  	;
	; 16/04/2015
        mov     [ss:u.sp], esp ; Kernel stack points to return address

	; save user registers
	push	ds
	push	es
	push	fs
	push	gs
	pushad  ; eax, ecx, edx, ebx, esp -before pushad-, ebp, esi, edi
	;
	; ESPACE = [ss:u.sp] - esp ; 4*12 = 48 ; 17/09/2015 ; 06/06/2016
	; 	(ESPACE is size of space in kernel stack 
	;	for saving/restoring user registers.)
	;
	push	eax ; 01/07/2015
	mov     ax, KDATA
        mov     ds, ax
        mov     es, ax
        mov     fs, ax
        mov     gs, ax
	mov	eax, [k_page_dir]
	mov	cr3, eax
	pop	eax ; 01/07/2015
	; 19/10/2015
	cld
	;
	inc	byte [sysflg]
		; incb sysflg / indicate a system routine is in progress
        sti 	; 18/01/2014
	;jnz	panic ; 24/05/2013
		; beq 1f
		; jmp panic ; / called if trap inside system
	; 23/07/2022
	jz	short sysent0
	jmp	panic
;1:
sysent0:
	; 17/03/2017
	and	byte [esp+ESPACE+8], ~1 ; clear carry flag

	; 16/04/2015
	mov	[u.r0], eax
	mov	[u.usp], esp ; kernel stack points to user's registers

	; 13/01/2017 (TRDOS 386 Feature only !)
	cmp	byte [u.t_lock], 0 ; timer interrupt lock ?
	;ja	sysrele		   ; yes, sys release only !!!
	; 23/07/2022
	jna	short sysent1
	jmp	sysrele
		; mov $s.syst+2,clockp
		; mov r0,-(sp) / save user registers 
		; mov sp,u.r0 / pointer to bottom of users stack
			   ; / in u.r0
		; mov r1,-(sp)
		; mov r2,-(sp)
		; mov r3,-(sp)
		; mov r4,-(sp)
		; mov r5,-(sp)
		; mov ac,-(sp) / "accumulator" register for extended
		             ; / arithmetic unit
		; mov mq,-(sp) / "multiplier quotient" register for the
		             ; / extended arithmetic unit
		; mov sc,-(sp) / "step count" register for the extended
		             ; / arithmetic unit
		; mov sp,u.sp / u.sp points to top of users stack
		; mov 18.(sp),r0 / store pc in r0
		; mov -(r0),r0 / sys inst in r0
		; sub $sys,r0 / get xxx code
sysent1:
	shl	eax, 2
		; asl r0 / multiply by 2 to jump indirect in bytes
	cmp	eax, end_of_syscalls - syscalls
		; cmp r0,$2f-1f / limit of table (35) exceeded
	;jnb	short badsys
		; bhis badsys / yes, bad system call
	cmc
	pushf
	push	eax
 	mov 	ebp, [u.sp] ; Kernel stack at the beginning of sys call
	mov	al, 0FEh ; 11111110b
	adc	al, 0 ; al = al + cf
	and	[ebp+8], al ; flags (reset carry flag)
		; bic $341,20.(sp) / set users processor priority to 0
				 ; / and clear carry bit
	pop	ebp ; eax
	popf
        jc      short badsys ; 23/07/2022
	mov	eax, [u.r0]
	; system call registers: EAX, EDX, ECX, EBX, ESI, EDI
	jmp	dword [ebp+syscalls]
		; jmp *1f(r0) / jump indirect thru table of addresses
		            ; / to proper system routine.

	; 20/08/2024 (exit code)
	; 30/07/2022
	; 23/07/2022
badsys:
	; 25/12/2016
	; 18/04/2016 (TRDOS 386 = TRDOS v2.0)
	; 17/04/2011 (TRDOS v1.0, 'IFC.ASM')
	; 03/02/2011 ('trdos_ifc_routine')
	;
	; 16/04/2015 (Retro UNIX 386 v1, 'badsys')
	; (EIP, EAX values will be shown on screen with error message)
	; (EIP = 'CD 40h' instruction address -INT 40h-)
	; (EAX = Function number)  
	;
	inc	byte [u.bsys]
	;
	mov	ebx, [u.sp] ; esp at the beginning of 'sysent'
	mov	eax, [ebx] ; EIP (return address, not 'INT 30h' address)
	;sub	eax, 2 ; CDh, ##h
	; 30/07/2022
	dec	eax
	dec	eax
	call	dwordtohex
	mov	[eip_str], edx
	mov	[eip_str+4], eax
	mov	eax, [u.r0]
	call	dwordtohex
	mov	[eax_str], edx
	mov	[eax_str+4], eax

	mov	word [int_num_str], SYSCALL_INT_NUM ; 25/12/2016

	mov	esi, ifc_msg ; "invalid function call !" msg (trdosk9.s)

	call	print_msg

;; 20/08/2024 - temporary
;	mov	ebx, 07h
;	mov	ah, 0Eh
;p_fc_msg:
;	lodsb
;	and	al, al
;	jz	short p_fc_ms_ok
;	call	_int10h
;	jmp	short p_fc_msg
;p_fc_ms_ok:

	;jmp	sysexit

	; 20/08/2024 (exit code)
	;mov	bl, 0FFh ; -1
	;jmp	sysexit
	;
	jmp	sysexit_0FFh

syscalls: ; 1:
	; 20/08/2024 - TRDOS 386 v2.0.9
	; 31/12/2017
	; 28/02/2017
	; 20/02/2017
	; 19/02/2017
	; 15/10/2016
	; 20/05/2016
	; 19/05/2016
	; 16/05/2016
	; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
	; 21/09/2015
	; 01/07/2015
	; 16/04/2015 (32 bit address modification) 
	dd sysver	; 0 ; Get TRDOS 386 version number (v2.0)
	dd sysexit 	; 1
	dd sysfork 	; 2
	dd sysread 	; 3
	dd syswrite 	; 4
	dd sysopen 	; 5
	dd sysclose 	; 6
	dd syswait 	; 7
	dd syscreat 	; 8
	dd sysrename	; 9  ; TRDOS 386, Rename File (31/12/2017)
	dd sysdelete	; 10 ; TRDOS 386, Delete File (29/12/2017)
	dd sysexec 	; 11
	dd syschdir 	; 12
	dd systime 	; 13 ; TRDOS 386, Get Sys Date&Time (30/12/2017)
	dd sysmkdir 	; 14
	dd syschmod 	; 15 ; TRDOS 386, Change Attributes (30/12/2017)
	dd sysrmdir 	; 16 ; TRDOS 386, Remove Directory (29/12/2017)
	dd sysbreak 	; 17
	dd sysdrive 	; 18 ; TRDOS 386, Get/Set Current Drv (30/12/2017)
	dd sysseek 	; 19
	dd systell 	; 20
	dd sysmem 	; 21 ; TRDOS 386, Get Total&Free Mem (31/12/2017)
	dd sysprompt 	; 22 ; TRDOS 386, Change Cmd Prompt (31/12/2017)
	dd syspath 	; 23 ; TRDOS 386, Get/Set Run Path (31/12/2017)
	dd sysenv 	; 24 ; TRDOS 386, Get/Set Env Vars (31/12/2017)
	dd sysstime 	; 25 ; TRDOS 386, Set Sys Date&Time (30/12/2017)
	dd sysquit 	; 26
	dd sysintr 	; 27
	dd sysdir 	; 28 ; TRDOS 386, Get Curr Drive&Dir (30/12/2017)
	dd sysemt 	; 29
	dd sysldrvt	; 30 ; TRDOS 386, Get Logical DOS DDT (30/12/2017)
	dd sysvideo 	; 31 ; TRDOS 386 Video Functions (16/05/2016)
	dd sysaudio 	; 32 ; TRDOS 386 Audio Functions (16/05/2016)
	dd systimer 	; 33 ; TRDOS 386 Timer Functions (18/05/2016)
	dd syssleep 	; 34 ; Retro UNIX 8086 v1 feature only !
			     ; 11/06/2014
	dd sysmsg	; 35 ; Retro UNIX 386 v1 feature only !
			     ; 01/07/2015
	dd sysgeterr	; 36 ; Retro UNIX 386 v1 feature only !
			     ; 21/09/2015 - get last error number
	dd sysfpstat	; 37 ; TRDOS 386 FPU state option (28/02/2017)
	dd syspri 	; 38 ; change priority - TRDOS 386 (20/05/2016)
	dd sysrele	; 39 ; TRDOS 386 (19/05/2016) (0 -> 39)
	dd sysfff	; 40 ; Find First File - TRDOS 386 (15/10/2016)
	dd sysfnf	; 41 ; Find Next File - TRDOS 386 (15/10/2016)
	dd sysalloc	; 42 ; Allocate contiguous memory block/pages
			     ; TRDOS 386 (19/02/2017) DMA buff fuctions
	dd sysdalloc	; 43 ; Deallocate contiguous memory block/pages
			     ; TRDOS 386 (19/02/2017) DMA buff fuctions
	dd syscalbac	; 44 ; IRQ Callback and Signal Response Byte
			     ; service setup - TRDOS 386 (20/02/2017)
			     ; 28/08/2017 (20/08/2017)
	dd sysdma	; 45 ; TRDOS 386 - (ISA) DMA service
	dd sysstdio	; 46 ; TRDOS 386 v2.0.9 (STDIN/STDOUT functions)

end_of_syscalls:

sysemt: ; enable (or disable) multi tasking -time sharing-
	;
	; 08/08/2022
	; 23/07/2022 - TRDOS 386 v2.0.5
	; 23/05/2016 - TRDOS 386 (TRDOS v2.0)
	; 14/05/2015 (Retro UNIX 386 v1)
	; 10/12/2013 - 20/04/2014 (Retro UNIX 8086 v1)
	;
	; Retro UNIX 8086 v1 modification:
	;	'Enable Multi Tasking'  system call instead
	;	of 'Emulator Trap' in original UNIX v1 for PDP-11.
	;
	; Retro UNIX 8086 v1 feature only!
	;	Using purpose: Kernel will start without time-out
	;	(internal clock/timer) functionality.
	;	Then etc/init will enable clock/timer for
	;	multi tasking.
	;
	; INPUT ->
	;	BL = 0 -> disable multi tasking
	;	BL > 1 -> enable multi tasking (time sharing)
	; OUTPUT ->
	;	none
	;
	;  Note: Multi tasking is disabled during system
	;	 initialization, it must be enabled by using
	;	 this system call. (Otherwise, running proces 
	;	 will not be changed by another process within
	;	 run time sequence/schedule, if running process
	;	 will not 'release' itself. Only 'wakeup' procedure
	;	 for waiting processes and programmed timer events
	;	 for other processes can change running process 
	;	 while multi tasking is disabled.) ** 23/05/2016 **

	cmp	byte [u.uid], 0 ; root ?
	;ja	short error
	; 23/07/2022
	;ja	short badsys ; 14/05/2015
	; 08/08/2022
	jna	short sysemt_root
	jmp	badsys
sysemt_root:	; 08/08/2022
	cli
	mov	[multi_tasking], bl ; 0 to disable, >0 to enable
	jmp	sysret

error:
	; 18/05/2016
	; 13/05/2016
	; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
	; 16/04/2015 - 17/09/2015 (Retro UNIX 386 v1)
	; 10/04/2013 - 07/08/2013 (Retro UNIX 8086 v1)
	;
	; 'error' merely sets the error bit off the processor status (c-bit)
	; then falls right into the 'sysret', 'sysrele' return sequence.
	;
	; INPUTS -> none
	; OUTPUTS ->
	;	processor status - carry (c) bit is set (means error)
	;
	; 26/05/2013 (Stack pointer must be reset here!
	; 	      Because, jumps to error procedure
	;	      disrupts push-pop nesting balance)
	;
	mov	ebp, [u.sp] ; interrupt (system call) return (iretd) address
	or	byte [ebp+8], 1  ; set carry bit of flags register
				 ; (system call will return with cf = 1)
		; bis $1,20.(r1) / set c bit in processor status word below
		               ; / users stack
	; 17/09/2015
	sub	ebp, ESPACE ; 48 ; total size of stack frame ('sysdefs.inc')
				 ; for saving/restoring user registers
	;cmp	ebp, [u.usp]
	;je	short err0
	mov	[u.usp], ebp
;err0:
	; 01/09/2015
	mov	esp, [u.usp] 	    ; Retro Unix 8086 v1 modification!
				    ; 10/04/2013
				    ; (If an I/O error occurs during disk I/O,
				    ; related procedures will jump to 'error'
				    ; procedure directly without returning to
				    ; the caller procedure. So, stack pointer
                                    ; must be restored here.)
	; 13/05/2016
	; NOTE: (The last) error code is in 'u.error', it can be retrieved by
	;	'get last error' system call later.

	; 03/09/2015 - 09/06/2015 - 07/08/2013
	mov 	byte [u.kcall], 0 ; namei_r, mkdir_w reset

sysret: ; < return from system call>
	; 28/08/2024 - TRDOS 386 Kernel v2.0.9
	; 23/07/2022 - TRDOS 386 Kernel v2.0.5
	; 01/03/2017
	; 28/02/2017
	; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
	; 16/04/2015 - 10/09/2015 (Retro UNIX 386 v1)
	; 10/04/2013 - 23/02/2014 (Retro UNIX 8086 v1)
	;
	; 'sysret' first checks to see if process is about to be
	; terminated (u.bsys). If it is, 'sysexit' is called.
	; If not, following happens:
	; 	1) The user's stack pointer is restored.
	;	2) r1=0 and 'iget' is called to see if last mentioned
	;	   i-node has been modified. If it has, it is written out
	;	   via 'ppoke'.
	;	3) If the super block has been modified, it is written out
	;	   via 'ppoke'.
	;	4) If the dismountable file system's super block has been
	;	   modified, it is written out to the specified device
	;	   via 'ppoke'.
	;	5) A check is made if user's time quantum (uquant) ran out
	;	   during his execution. If so, 'tswap' is called to give
	;	   another user a chance to run.
	;	6) 'sysret' now goes into 'sysrele'.
	;	    (See 'sysrele' for conclusion.)
	;
	; Calling sequence:
	;	jump table or 'br sysret'
	; Arguments: 
	;	-
	; ...............................................................
	;
	; ((AX=r1 for 'iget' input))
	;
	xor	eax, eax ; 28/02/2017
sysret0: ; 29/07/2015 (eax = 0, jump from sysexec)
	inc	al ; 04/05/2013
	cmp	[u.bsys], al ; 1
		; tstb u.bsys / is a process about to be terminated because
        ;jnb	sysexit ; 04/05/2013
		; bne sysexit / of an error? yes, go to sysexit
	; 23/07/2022
	jb	short sysret1
	;jmp	sysexit
	; 22/08/2024
	jmp	sysexit_@ ; BL = 0FFh ; -1
sysret1:	; 23/07/2022
	;mov	esp, [u.usp] ; 24/05/2013 (that is not needed here)
		; mov u.sp,sp / no point stack to users stack
	dec 	al ; mov ax, 0
		; clr r1 / zero r1 to check last mentioned i-node
	call	iget
		; jsr r0,iget / if last mentioned i-node has been modified
		            ; / it is written out
	; 10/01/2017
	; 09/01/2017
;sysrele: ; < release >
	; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
	; 16/04/2015 - 14/10/2015 (Retro UNIX 386 v1)
	; 10/04/2013 - 07/03/2014 (Retro UNIX 8086 v1)
	;
	; 'sysrele' first calls 'tswap' if the time quantum for a user is
	;  zero (see 'sysret'). It then restores the user's registers and
	; turns off the system flag. It then checked to see if there is
	; an interrupt from the user by calling 'isintr'. If there is,
	; the output gets flashed (see isintr) and interrupt action is
	; taken by a branch to 'intract'. If there is no interrupt from
	; the user, a rti is made.
	;
	; Calling sequence:
	;	Fall through a 'bne' in 'sysret' & ?
	; Arguments:
	;	-
	; ...............................................................
	;
	; 23/02/2014 (swapret)
	; 22/09/2013
sysrel0: ;1:
	cmp	byte [u.quant], 0 ; 16/05/2013
		; tstb uquant / is the time quantum 0?
        ja      short swapret
		; bne 1f / no, don't swap it out
sysrelease: ; 07/12/2013 (jump from 'clock')
	call	tswap
		; jsr r0,tswap / yes, swap it out

; Retro Unix 8086 v1 feature: return from 'swap' to 'swapret' address.
swapret: ;1:
	; 10/09/2015
	; 01/09/2015
	; 14/05/2015
	; 16/04/2015 (Retro UNIX 386 v1 - 32 bit, pm modifications)
	; 26/05/2013 (Retro UNIX 8086 v1)
	; cli
	; 24/07/2015
	;
	;; 'esp' must be already equal to '[u.usp]' here !
	;; mov	esp, [u.usp]

	; 22/09/2013
	call	isintr
	; 20/10/2013
	jz	short sysrel1
	call	intract
		; jsr r0,isintr / is there an interrupt from the user
		;     br intract / yes, output gets flushed, take interrupt
		               ; / action
sysrel1:
	cli	; 14/10/2015
sysrel2:
	; 28/02/2017
	; Check if there is a (delayed) callback for current user/process
	mov	al, [u.irqwait]
	and	al, 0Fh ; is there a waiting IRQ callback service ?
	jz	short sysrel8 ; no

	; Set return to IRQ callback service and return from the service
	movzx	ebx, al
	mov 	[u.irqwait], bh ; 0 ; reset
	mov	bl, [ebx+IRQenum] ; (available) IRQ index +1 (1 to 9)
	; 01/03/2017
	dec	bl ; IRQ index number, 0 to 8
	js	short sysrel8 ; 0 -> FFh (not in use!?) 
	;
	mov 	al, [u.uno] ; current process (user) number 
	cmp	[ebx+IRQ.owner], al
	jne	short sysrel8 ; it is not the current user/process !?
	test	byte [ebx+IRQ.method], 1 ; callback ?
	jz	short sysrel8 ; not a callback method !?

	mov	edx, [ebx+IRQ.addr] ; IRQ callback service address (virtual)
	mov	byte [u.r_lock], 1 ; IRQ callback service in progress flag

	call	wswap ; save user's registers & status 
		      ;	(for return from IRQ callback service)

	mov	ebp, [u.sp]; kernel's stack, points to EIP (user)
	mov	[ebp], edx ; IRQ call back service address
sysrel8:
	dec	byte [sysflg]
		; decb sysflg / turn system flag off

	mov	eax, [u.pgdir]
	mov	cr3, eax  ; 1st PDE points to Kernel Page Table 0 (1st 4 MB)
			  ; (others are different than kernel page tables)
	; 10/09/2015
	popad ; edi, esi, ebp, temp (icrement esp by 4), ebx, edx, ecx, eax
		; mov (sp)+,sc / restore user registers
		; mov (sp)+,mq
		; mov (sp)+,ac
		; mov (sp)+,r5
		; mov (sp)+,r4
		; mov (sp)+,r3
		; mov (sp)+,r2
	;
	mov	eax, [u.r0]  ; ((return value in EAX))
	pop	gs
	pop	fs
	pop	es
	pop	ds
	;or	word [esp+8], 200h ; 22/01/2017 ; force enabling interrupts
	iretd
		; rti / no, return from interrupt

sysrele:
	; 23/07/2022 - TRDOS 386 v2.0.5
	; 24/03/2017
	; 28/02/2017
	; 27/02/2017
	; 29/01/2017
	; 14/01/2017
	; 13/01/2017
	; 09/01/2017 - 10/01/2017 - 12/01/2017
	; Major modification for TRDOS 386 (CallBack return)
	;
	; 'sysrele' system call restores previously saved
	; registers and addresses of the process
	; (Main purpose -in TRDOS 386- is to return from
	; timer callback service routine in ring 3 -user mode-.)
	;
	; check if the process is in timer callback phase
	cmp	byte [u.t_lock], 0 ; TIMER INT LOCK
	;je	short sysrel0 ; classic (Retro UNIX 386 type) sysrele
	ja	short sysrel3
	; 27/02/2017
	cmp	byte [u.r_lock], 0 ; IRQ callback lock	
	;jna	sysrel0 ; classic sysrele ; 24/03/2017
	; 23/07/2022
	ja	short sysrel9
	jmp	sysrel0
sysrel9:	; 23/07/2022
	call	sysrel7
	cmp	byte [u.r_lock], 0 ; IRQ callback service lock
	jna	short sysrel4
	mov	byte [u.r_lock], 0 ; reset
	;mov	byte [u.irqwait], 0 ; reset ; 28/02/2017
	mov	al, [u.r_mode]
	or	al, al
	jnz	short sysrel4
	dec	al
	mov	[u.r_mode], al ; 0FFh ; not necessary !?
	jmp	short sysrel6
sysrel3:
	; 27/02/2017
	call	sysrel7
	; 14/01/2017
	sub	al, al
	cmp	[u.t_lock], al ; 0 ; TIMER INT LOCK
	ja	short sysrel5 ; yes
sysrel4:
	; 29/01/2017
	mov	eax, [esp+28] ; eax
	mov	[u.r0], eax
	jmp	sysrel2
sysrel5:
	mov	[u.t_lock], al ; 0 ; reset
	mov	al, [u.t_mode]
	and	al, al
	;jnz	short sysrel2 ; 0FFh ; user mode
	jnz	short sysrel4 ; 29/01/2017
	dec	al
	mov	[u.t_mode], al ; 0FFh ; not necessary !?
sysrel6:
	; cpu will continue from the interrupted sytem call addr
	popad		; edi, esi, ebp, esp, ebx, edx, ecx, eax
	add	esp, 16	; pass segment segisters: ds, es, fs, gs
	iretd		; eip, cs, eflags

sysrel7:
	movzx	ebx, byte [u.uno] ; current process number
	;shl	bx, 2
	; 23/07/2022
	shl	ebx, 2
	;cmp	[ebx+p.tcb-4], eax ; 0 ; is there callback address ?
	;jna	short sysrel0 
	; yes, reset callback address then restore process registers
	;mov	[ebx+p.tcb-4], eax ; 0 ; reset
	mov     eax, [ebx+p.upage-4] ; UPAGE address
	cli	; disable interrupts till 'iretd'
	jmp	rswap ; restore process 'u' structure

intract: ; / interrupt action
	; 14/10/2015
	; 16/04/2015 (Retro UNIX 386 v1 - Beginning)
	; 09/05/2013 - 07/12/2013 (Retro UNIX 8086 v1)
	;
	; Retro UNIX 8086 v1 modification !
	; (Process/task switching and quit routine by using
	; Retro UNIX 8086 v1 keyboard interrupt output.))
	;
	; input -> 'u.quit' (also value of 'u.intr' > 0)
	; output -> If value of 'u.quit' = FFFFh ('ctrl+brk' sign)
	;		'intract' will jump to 'sysexit'.
	;	    Intract will return to the caller
	;		if value of 'u.quit' <> FFFFh.
	; 14/10/2015
	sti
	; 07/12/2013
	inc 	word [u.quit]
	jz	short intrct0 ; FFFFh -> 0
	dec	word [u.quit]
	; 16/04/2015
	retn
intrct0:
	pop	eax ; call intract -> retn
	;
	; 20/08/2024
	;xor 	eax, eax
	;inc	al  ; mov ax, 1
;;;
	; UNIX v1 original 'intract' routine...
	; / interrupt action
		;cmp *(sp),$rti / are you in a clock interrupt?
		; bne 1f / no, 1f
		; cmp (sp)+,(sp)+ / pop clock pointer
	; 1: / now in user area
		; mov r1,-(sp) / save r1
		; mov u.ttyp,r1 
			; / pointer to tty buffer in control-to r1
		; cmpb 6(r1),$177
			; / is the interrupt char equal to "del"
		; beq 1f / yes, 1f
		; clrb 6(r1) 
		        ; / no, clear the byte 
			; / (must be a quit character)
		; mov (sp)+,r1 / restore r1
		; clr u.quit / clear quit flag
		; bis $20,2(sp) 
		    	; / set trace for quit (sets t bit of
			; / ps-trace trap)
		; rti   ;  / return from interrupt
	; 1: / interrupt char = del
		; clrb 6(r1) / clear the interrupt byte
			   ; / in the buffer
		; mov (sp)+,r1 / restore r1
		; cmp u.intr,$core / should control be
				; / transferred to loc core?
		; blo 1f
		; jmp *u.intr / user to do rti yes, 
				; / transfer to loc core
	; 1:
		; sys 1 / exit

	; 20/08/2024
	; ctrl+break -> exit code = -1
sysexit_0FFh:
	; 20/08/2024
	xor 	eax, eax
	inc	eax	; mov eax, 1
sysexit_@:	; 22/08/2024
	mov	bl, 0FFh ;  exit code ; -1

sysexit: ; <terminate process>
	; 22/08/2024
	; 20/08/2024
	; 18/08/2024 - TRDOS 386 v2.0.9 
	; 30/07/2022
	; 23/07/2022 - TRDOS 386 v2.0.5
	; 14/11/2017
	; 27/05/2017
	; 10/04/2017
	; 26/02/2017 - 28/02/2017
	; 02/01/2017 - 23/01/2017
	; 06/06/2016 - 10/06/2016
	; 19/05/2016 - 23/05/2016
	; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
	; 16/04/2015 - 01/09/2015 (Retro UNIX 386 v1)
	; 19/04/2013 - 14/02/2014 (Retro UNIX 8086 v1)
	;
	; 'sysexit' terminates a process. First each file that
	; the process has opened is closed by 'flose'. The process
	; status is then set to unused. The 'p.pid' table is then
	; searched to find children of the dying process. If any of
	; children are zombies (died by not waited for), they are
	; set free. The 'p.pid' table is then searched to find the
	; dying process's parent. When the parent is found, it is
	; checked to see if it is free or it is a zombie. If it is
	; one of these, the dying process just dies. If it is waiting
	; for a child process to die, it notified that it doesn't 
	; have to wait anymore by setting it's status from 2 to 1
	; (waiting to active). It is awakened and put on runq by
	; 'putlu'. The dying process enters a zombie state in which
	; it will never be run again but stays around until a 'wait'
	; is completed by it's parent process. If the parent is not
	; found, process just dies. This means 'swap' is called with
	; 'u.uno=0'. What this does is the 'wswap' is not called
	; to write out the process and 'rswap' reads the new process
	; over the one that dies..i.e., the dying process is 
	; overwritten and destroyed.
 	;
	; Calling sequence:
	;	sysexit or conditional branch.
	; Arguments:
	;	-
	; ...............................................................
	;
	; Retro UNIX 8086 v1 modification: 
	;       System call number (=1) is in EAX register.
	;
	;       Other parameters are in EDX, EBX, ECX, ESI, EDI, EBP
	;       registers depending of function details.
	;
	; ('swap' procedure is mostly different than original UNIX v1.)
	;
; / terminate process
	; AX = 1
	;dec 	ax ; 0
	; 18/08/2024
	dec	eax
	mov	[u.intr], ax ; 0
		; clr u.intr / clear interrupt control word
		; clr r1 / clear r1
	; 18/08/2024 - exit code (in EBX, BL)
	mov	[u.exit], bl
sysexit_0:
	; 30/07/2022
	; 23/01/2017
	; 02/01/2017
	; 10/06/2016
	; 06/06/2016
	; 23/05/2016
	; 19/05/2016 - TRDOS 386 (TRDOS v2.0)
	; Check and stop/clear timer event(s) of this (dying) process
	; if there is.

	; 02/01/2017
	cli	; disable interrupts
	; 23/01/2017 - reset timer frequency (to 18.2Hz)
	mov	al, 00110110b ; 36h
 	out	43h, al
	sub	al, al ; 0
	out	40h, al ; LB
	out	40h, al ; HB
 	; 
	movzx	ebx, byte [u.uno]
	;mov	bl, [u.uno] ; process number of dying process
	cmp	byte [ebx+p.timer-1], al ; 0
	jna	short sysexit_12 ; no timer events for this process
	mov	byte [ebx+p.timer-1], al ; 0 ; reset
	;mov	al, [timer_events]
	;or	al, al
 	;jz	short sysexit_12 ; no timer events
	;mov	cl, al
	mov	cl, [timer_events] ; 14/11/2017
	;cli	; disable interrupts 
	mov	ah, 16 ; number of available timer events
	mov	esi, timer_set ; beginning address of timer events
sysexit_7:
	mov	al, [esi] ; process number (of timer event)
	cmp	al, bl ; process number comparison
	je	short sysexit_10
	and	al, al
	jz	short sysexit_9
sysexit_8:
	dec	cl
	jz	short sysexit_11
sysexit_9:
	dec	ah
	jz	short sysexit_12
	add	esi, 16
	jmp	short sysexit_7

sysexit_10:
	;mov	byte [esi], 0
	mov	word [esi], 0
	;mov	dword [esi+12], 0
	;
	dec	byte [timer_events] ; 02/01/2017
	;
	jmp	short sysexit_8

sysexit_11:
	;sub	ax, ax ; 0 ; 26/02/2017
	; 30/07/2022
	sub	eax, eax ; 0
sysexit_12:
	; 26/02/2017 (Unlink IRQ callbacks belong to the user)
	cmp	byte [u.irqc], 0 ; Count of IRQ callbacks
	jng	short sysexit_16 ; zero or invalid
	; 28/02/2017
	; clear IRQ callback flags (for 'sysrele' and 'sysret')
	mov	[u.irqwait], al ; 0 ; force to clear waiting flag
	mov	[u.r_lock], al ; 0 ; force to clear busy flag
	mov	esi, IRQ.owner
sysexit_13:
	lodsb
	cmp	al, [u.uno] ; owner = current user ?
	jne	short sysexit_14
	mov	byte [esi-1], 0 ; owner = 0 : Free

	;;;
	; 22/08/2024 - Reset Hardware Interrupt Handler
	mov	ecx, esi
	sub	ecx, IRQ.owner
	; CL = (user configurable) IRQ index + 1 (1 to 9)
	mov	eax, IRQenum
sysexit_18:
	cmp	cl, [eax]
	je	short sysexit_19
	cmp	eax, IRQenum+15
	jnb	short sysexit_20
	inc	eax
	jmp	short sysexit_18
sysexit_19:
	sub	eax, IRQenum
	; AL = IRQ number (will be reset to system's IRQ handler)
	; AH = 0
	;mov	ah, 0 ; reset
	call	set_hardware_int_vector
sysexit_20:
	;;;

	dec	byte [u.irqc]
	jz	short sysexit_15
sysexit_14:
	cmp	esi, IRQ.owner + 8 ; the last IRQ index number ?
	jna	short sysexit_13 ; no
sysexit_15:
	;xor	al, al ; 0
	; 22/08/2024
	xor	eax, eax
sysexit_16: ; 2:
	sti	; enable interrupts
	;
	; AX = 0
sysexit_1: ; 1:
	; AX = File descriptor
		; / r1 has file descriptor (index to u.fp list)
		; / Search the whole list
	call	fclose
		; jsr r0,fclose / close all files the process opened
	;; ignore error return
		; br .+2 / ignore error return
	;;inc	ax
	;inc	al
	; 22/08/2024
	inc	eax
		; inc r1 / increment file descriptor
	;cmp	ax, 10
	cmp	al, 10
		; cmp r1,$10. / end of u.fp list?
	jb	short sysexit_1
		; blt 1b / no, go back

	; 22/08/2024
	; reset IRQs if owned by the current (dying) process
	mov	esi, IRQ.owner

	;movzx	ebx, byte [u.uno]
	mov	bl, [u.uno] ; 02/01/2017
		; movb	u.uno,r1 / yes, move dying process's number to r1
	mov	[ebx+p.stat-1], ah ; 0, SFREE
		; clrb p.stat-1(r1) / free the process
	; 10/04/2017
	cmp	[audio_user], bl
	jne	short sysexit_17
	; reset audio device (current) owner and 'initializated' flag
	mov	[audio_user], bh ; 0
	; 27/05/2017
	mov	ecx, [audio_buffer]
	or	ecx, ecx
	jz	short sysexit_17
	; 'deallocate_user_pages' is not necessary in sysexit !!!
	;push	ebx
	;mov	ebx, ecx
	;mov	ecx, [audio_buff_size]
	;call	deallocate_user_pages
	;; (Modified Registers -> EAX, EDX, ESI, EDI, EBX, ECX, EBP)
	sub	ecx, ecx
	mov	[audio_buffer], ecx ; 0
	;pop	ebx
sysexit_17:
	;shl	bx, 1
	shl	bl, 1
	; 22/08/2024
	;shl	ebx, 1
		; asl r1 / use r1 for index into the below tables
	mov	cx, [ebx+p.pid-2]
		; mov p.pid-2(r1),r3 / move dying process's name to r3
	mov	dx, [ebx+p.ppid-2]
		; mov p.ppid-2(r1),r4 / move its parents name to r4
	;xor 	bx, bx ; 0
	xor	bl, bl ; 0
		; clr r2
	xor	esi, esi ; 0
		; clr r5 / initialize reg
sysexit_2: ; 1:
	        ; / find children of this dying process,
		; / if they are zombies, free them
	;add	bx, 2
	add	bl, 2
		; add $2,r2 / search parent process table
		          ; / for dying process's name
	cmp	[ebx+p.ppid-2], cx
		; cmp p.ppid-2(r2),r3 / found it?
	jne	short sysexit_4
		; bne 3f / no
	;shr	bx, 1
	shr	bl, 1
		; asr r2 / yes, it is a parent
	cmp	byte [ebx+p.stat-1], 3 ; SZOMB
		; cmpb p.stat-1(r2),$3 / is the child of this
				     ; / dying process a zombie
	jne	short sysexit_3
		; bne 2f / no
	mov	[ebx+p.stat-1], ah ; 0, SFREE
		; clrb p.stat-1(r2) / yes, free the child process
sysexit_3: ; 2:
	;shr	bx, 1
	shl	bl, 1
		; asl r2
sysexit_4: ; 3:
		; / search the process name table 
		; / for the dying process's parent
	cmp	[ebx+p.pid-2], dx
		; cmp p.pid-2(r2),r4 / found it?
	jne	short sysexit_5
		; bne 3f / no
	mov	esi, ebx
		; mov r2,r5 / yes, put index to p.pid table (parents
		          ; / process # x2) in r5
sysexit_5: ; 3:
	;cmp	bx, nproc + nproc
	cmp	bl, nproc + nproc
		; cmp r2,$nproc+nproc / has whole table been searched?
	jb	short sysexit_2
		; blt 1b / no, go back
		; mov r5,r1 / yes, r1 now has parents process # x2
	and	esi, esi ; r5=r1
	jz	short sysexit_6
		; beq 2f / no parent has been found.
		       ; / The process just dies
	;shr	si, 1
	; 23/07/2022
	shr	esi, 1
		; asr r1 / set up index to p.stat
	mov	al, [esi+p.stat-1]
		; movb p.stat-1(r1),r2 / move status of parent to r2
	and	al, al
	jz	short sysexit_6
		; beq 2f / if its been freed, 2f
	cmp	al, 3
		; cmp r2,$3 / is parent a zombie?
	je	short sysexit_6
		; beq 2f / yes, 2f

	; BH = 0
	mov	bl, [u.uno]
		; movb u.uno,r3 / move dying process's number to r3
	mov	byte [ebx+p.stat-1], 3 ; SZOMB
		; movb $3,p.stat-1(r3) / make the process a zombie

	cmp	al, 1 ; SRUN
	je	short sysexit_6
	;cmp	al, 2
		; cmp r2,$2 / is the parent waiting for 
			  ; / this child to die
	;jne	short sysexit_6	
		; bne 2f / yes, notify parent not to wait any more
	; p.stat = 2 --> waiting
	; p.stat = 4 --> sleeping
	mov	byte [esi+p.stat-1], 1 ; SRUN
	;;;
	; 18/08/2024 - exit code
	mov	al, [u.exit] ; exit code of the child
	; 20/08/2024
	mov	[ebx+p.exitc-1], al ; save it to use by the parent
	;;;
	;dec	byte [esi+p.stat-1]
		; decb	p.stat-1(r1) / awaken it by putting it (parent)
	;;;
	;mov	ax, si ; r1  (process number in AL)
	; 18/08/2024
	mov	eax, esi
	;;;
	;mov	ebx, runq + 4
		; mov $runq+4,r2 / on the runq
	mov	ebx, runq+2 ; normal run queue ; 02/01/2017
	call	putlu
		; jsr r0, putlu
sysexit_6:
		; / the process dies
	mov	byte [u.uno], 0
		; clrb u.uno / put zero as the process number,
	           ; / so "swap" will
	call	swap
		; jsr r0,swap / overwrite process with another process
hlt_sys:
	;sti
hlts0:
	hlt
	jmp	short hlts0
		; 0 / and thereby kill it; halt?

syswait: ; < wait for a processs to die >
	; 20/08/2024
	; 30/07/2022
	; 23/07/2022 - TRDOS 386 v2.0.5
	; 17/09/2015
	; 02/09/2015
	; 01/09/2015
	; 16/04/2015 (Retro UNIX 386 v1 - Beginning)
	; 24/05/2013 - 05/02/2014 (Retro UNIX 8086 v1)
	;
	; 'syswait' waits for a process die.
	; It works in following way:
	;    1) From the parent process number, the parent's 
	; 	process name is found. The p.ppid table of parent
	;	names is then searched for this process name.
	;	If a match occurs, r2 contains child's process
	;	number. The child status is checked to see if it is
	;	a zombie, i.e; dead but not waited for (p.stat=3)
	;	If it is, the child process is freed and it's name
	;	is put in (u.r0). A return is then made via 'sysret'.
	;	If the child is not a zombie, nothing happens and
	;	the search goes on through the p.ppid table until
	;	all processes are checked or a zombie is found.
	;    2) If no zombies are found, a check is made to see if
	;	there are any children at all. If there are none,
	;	an error return is made. If there are, the parent's
	;	status is set to 2 (waiting for child to die),
	;	the parent is swapped out, and a branch to 'syswait'
	;	is made to wait on the next process.
	;
	; Calling sequence:
	;	?
	; Arguments:
	;	-
	; Inputs: - 
	; Outputs: if zombie found, it's name put in u.r0.
	; ...............................................................
	;

; / wait for a process to die

syswait_0:
	movzx	ebx, byte [u.uno] ; 01/09/2015
		; movb u.uno,r1 / put parents process number in r1
	shl	bl, 1
	;shl	bx, 1
		; asl r1 / x2 to get index into p.pid table
	mov	ax, [ebx+p.pid-2]
		; mov p.pid-2(r1),r1 / get the name of this process
	xor	esi, esi
		; clr r2
	xor	ecx, ecx ; 30/10/2013
	;xor 	cl, cl
		; clr r3 / initialize reg 3
syswait_1: ; 1:
	;add	si, 2
	; 23/07/2022
	inc	esi
	inc	esi
		; add $2,r2 / use r2 for index into p.ppid table
			  ; / search table of parent processes
			  ; / for this process name
	cmp	ax, [esi+p.ppid-2]
		; cmp p.ppid-2(r2),r1 / r2 will contain the childs
			            ; / process number
	jne	short syswait_3
		;bne 3f / branch if no match of parent process name
	;inc	cx
	inc	cl
		;inc r3 / yes, a match, r3 indicates number of children
	;shr	si, 1
	; 23/07/2022
	shr	esi, 1
		; asr r2 / r2/2 to get index to p.stat table
	; The possible states ('p.stat' values) of a process are:
	;	0 = free or unused
	;	1 = active
	;	2 = waiting for a child process to die
	;	3 = terminated, but not yet waited for (zombie).
	cmp	byte [esi+p.stat-1], 3 ; SZOMB, 05/02/2014
		; cmpb p.stat-1(r2),$3 / is the child process a zombie?
	jne	short syswait_2
		; bne 2f / no, skip it
	mov	[esi+p.stat-1], bh ; 0
		; clrb p.stat-1(r2) / yes, free it

	; 20/08/2024 (ebx = child's exit code)
	xor	eax, eax
	mov	al, [esi+p.exitc-1] ; exit code
	mov	ebp, [u.usp]
	; 22/08/2024
	mov	[ebp+16], eax ; ebx

	;shl	si, 1
	; 23/07/2022
	shl	esi, 1
		; asl r2 / r2x2 to get index into p.pid table
	;movzx	eax, word [esi+p.pid-2]
	; 20/08/2024
	mov	ax, [esi+p.pid-2]
	mov	[u.r0], eax
		; mov p.pid-2(r2),*u.r0 
			      ; / put childs process name in (u.r0)
	;
	; Retro UNIX 386 v1 modification ! (17/09/2015)
	;
	; Parent process ID -p.ppid- field (of the child process)
	; must be cleared in order to prevent infinitive 'syswait'
	; system call loop from the application/program if it calls
	; 'syswait' again (mistakenly) while there is not a zombie
	; or running child process to wait. ('forktest.s', 17/09/2015)
	;
	; Note: syswait will return with error if there is not a
	;       zombie or running process to wait.

	;sub	ax, ax
	; 30/07/2022
	sub	eax, eax ; 0
	mov 	[esi+p.ppid-2], ax ; 0 ; 17/09/2015
	jmp	sysret0 ; ax = 0
	;
	;jmp	sysret
		; br sysret1 / return cause child is dead
syswait_2: ; 2:
	;shl	si, 1
	; 23/07/2022
	shl	esi, 1
		; asl r2 / r2x2 to get index into p.ppid table
syswait_3: ; 3:
	cmp	si, nproc+nproc
		; cmp r2,$nproc+nproc / have all processes been checked?
	jb	short syswait_1
		; blt 1b / no, continue search
	;and	cx, cx
	and	cl, cl
		; tst r3 / one gets here if there are no children
		       ; / or children that are still active
	; 30/10/2013
	jnz	short syswait_4
	;jz	error
		; beq error1 / there are no children, error
	mov	[u.r0], ecx ; 0

	; 20/08/2024 (ebx = child's exit code)
	mov	ebp, [u.usp]
	dec	ecx ; -1 ; 0FFFFFFFFh
	; 22/08/2024
	mov	[ebp+16], ecx ; ebx

	jmp	error
syswait_4:
	mov	bl, [u.uno]
		; movb u.uno,r1 / there are children so put 
			      ; / parent process number in r1
	inc	byte [ebx+p.stat-1] ; 2, SWAIT, 05/02/2014
		; incb p.stat-1(r1) / it is waiting for
				  ; / other children to die
	; 04/11/2013
	call	swap
		; jsr r0,swap / swap it out, because it's waiting

	; 23/08/2024 - bugfix
	; restore [u.usp]
	; (swap above changes [u.usp] just before wswap)
	mov	[u.usp], esp
		; points to user's regs on top ofn system stack

	jmp	syswait_0
		; br syswait / wait on next process

sysfork: ; < create a new process >
	; 19/08/2024 - TRDOS 386 Kernel v2.0.9 (STDIN/STDOUT)
	; 23/07/2022 - TRDOS 386 Kernel v2.0.5
	; 02/01/2017 (TRDOS 386 modification)
	; 04/09/2015 - 18/05/2015
	; 28/08/2015 - 01/09/2015 - 02/09/2015
	; 09/05/2015 - 10/05/2015 - 14/05/2015
	; 06/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 24/05/2013 - 14/02/2014 (Retro UNIX 8086 v1)
	;
	; 'sysfork' creates a new process. This process is referred
	; to as the child process. This new process core image is
	; a copy of that of the caller of 'sysfork'. The only
	; distinction is the return location and the fact that (u.r0)
	; in the old process (parent) contains the process id (p.pid)
	; of the new process (child). This id is used by 'syswait'.
	; 'sysfork' works in the following manner:
	;    1) The process status table (p.stat) is searched to find
	;	a process number that is unused. If none are found
	;	an error occurs.
	;    2) when one is found, it becomes the child process number
	;	and it's status (p.stat) is set to active.
	;    3) If the parent had a control tty, the interrupt
	;	character in that tty buffer is cleared.
	;    4) The child process is put on the lowest priority run
	;	queue via 'putlu'.
	;    5) A new process name is gotten from 'mpid' (actually
	;	it is a unique number) and is put in the child's unique
	;	identifier; process id (p.pid).
	;    6) The process name of the parent is then obtained and
	;	placed in the unique identifier of the parent process
	;	name is then put in 'u.r0'.
	;    7) The child process is then written out on disk by
	;	'wswap',i.e., the parent process is copied onto disk
	;	and the child is born. (The child process is written
	;	out on disk/drum with 'u.uno' being the child process
	;	number.)
	;    8) The parent process number is then restored to 'u.uno'.
	;    9) The child process name is put in 'u.r0'.
	;   10) The pc on the stack sp + 18 is incremented by 2 to
	;	create the return address for the parent process.
	;   11) The 'u.fp' list as then searched to see what files
	;	the parent has opened. For each file the parent has
	;	opened, the corresponding 'fsp' entry must be updated
	;	to indicate that the child process also has opened
	;	the file. A branch to 'sysret' is then made.
	;
	; Calling sequence:
	;	from shell ?
	; Arguments:
	;	-
	; Inputs: -
	; Outputs: *u.r0 - child process name
	; ...............................................................
	;	
	; Retro UNIX 8086 v1 modification: 
	;	AX = r0 = PID (>0) (at the return of 'sysfork')
	;	= process id of child a parent process returns
	;	= process id of parent when a child process returns
	;
	;       In original UNIX v1, sysfork is called and returns as
	;	in following manner: (with an example: c library, fork)
	;
	;	1:
	;		sys	fork
	;			br 1f  / child process returns here
	;		bes	2f     / parent process returns here
	;		/ pid of new process in r0
	;		rts	pc
	;	2: / parent process condionally branches here
	;		mov	$-1,r0 / pid = -1 means error return
	;		rts	pc
	;
	;	1: / child process brances here
	;		clr	r0   / pid = 0 in child process
	;		rts	pc
	;
	;	In UNIX v7x86 (386) by Robert Nordier (1999)
	;		// pid = fork();
	;		//
	;		// pid == 0 in child process; 
	;		// pid == -1 means error return
	;		// in child, 
	;		//	parents id is in par_uid if needed
	;
	;		_fork:
	;			mov	$.fork,eax
	;			int	$0x30
	;			jmp	1f
	;			jnc	2f
	;			jmp	cerror
	;		1:
	;			mov	eax,_par_uid
	;			xor	eax,eax
	;		2:
	;			ret
	;
	;	In Retro UNIX 8086 v1,
	;	'sysfork' returns in following manner:
	;
	;		mov	ax, sys_fork
	;		mov	bx, offset @f ; routine for child
	;		int	20h
	;		jc	error
	;
	;	; Routine for parent process here (just after 'jc')
	;		mov	word ptr [pid_of_child], ax
	;		jmp	next_routine_for_parent	
	;
	;	@@: ; routine for child process here
	;		....
	;	NOTE: 'sysfork' returns to specified offset
	;	       for child process by using BX input.
	;	      (at first, parent process will return then
	;	      child process will return -after swapped in-
	;	      'syswait' is needed in parent process
	;	      if return from child process will be waited for.)
	;

; / create a new process
	; EBX = return address for child process
	     ; (Retro UNIX 8086 v1 modification !)
	xor 	esi, esi
		; clr r1
sysfork_1: ; 1: / search p.stat table for unused process number
	inc	esi
		; inc r1
	cmp	byte [esi+p.stat-1], 0 ; SFREE, 05/02/2014
		; tstb p.stat-1(r1) / is process active, unused, dead
	jna	short sysfork_2	
		; beq 1f / it's unused so branch
	cmp	si, nproc
		; cmp r1,$nproc / all processes checked
	jb	short sysfork_1
		; blt 1b / no, branch back
	;
	; Retro UNIX 8086 v1. modification:
	;	Parent process returns from 'sysfork' to address 
	;	which is just after 'sysfork' system call in parent
	;	process. Child process returns to address which is put
	;	in BX register by parent process for 'sysfork'. 
	;
		; add $2,18.(sp) / add 2 to pc when trap occured, points
		             ; / to old process return
		; br error1 / no room for a new process
sysfork_err:
	jmp	error
sysfork_2: ; 1:
	call	allocate_page
	;jc	error
	; 23/07/2022
	jc	short sysfork_err
	push	eax   ; UPAGE (user structure page) address
	; Retro UNIX 386 v1 modification!
	call	duplicate_page_dir
		; EAX = New page directory
	jnc	short sysfork_3
	pop	eax   ; UPAGE (user structure page) address
	call 	deallocate_page
	jmp	error
sysfork_3:
	; Retro UNIX 386 v1 modification !
	push	esi
	call	wswap ; save current user (u) structure, user registers
		      ; and interrupt return components (for IRET)
	xchg	eax, [u.pgdir] ; page directory of the child process
	mov	[u.ppgdir], eax ; page directory of the parent process
	pop	esi
	pop	eax   ; UPAGE (user structure page) address
		; [u.usp] = esp
	mov	edi, esi
	;shl	di, 2
	; 23/07/2022
	shl	edi, 2
	mov	[edi+p.upage-4], eax ; memory page for 'user' struct
	mov	[u.upage], eax ; memory page for 'user' struct (child)
	;;;
	; 19/08/2024 - reset STDIN/STDOUT redirections (and ungetchar)
	xor	eax, eax ; 0
	; 19/08/2024
	;mov	[u.stdin], al ; 0
	;mov	[u.stdout], al ; 0
	;mov	[u.ungetc], al ; 0
	;mov	[u.getc], al ; 0
	mov	[u.stdin], eax ; 0
	;;;

	; 28/08/2015
	;movzx	eax, byte [u.uno] ; parent process number
	; 19/08/2024
	; eax = 0
	mov	al, [u.uno] ; parent process number
		; movb u.uno,-(sp) / save parent process number
	mov	edi, eax
        push	eax ; **
	mov     al, [edi+p.ttyc-1] ; console tty (parent)
	; 18/09/2015
	;;mov	[esi+p.ttyc-1], al ; set child's console tty
	;;mov	[esi+p.waitc-1], ah ; 0 ; reset child's wait channel
	;mov    [esi+p.ttyc-1], ax ; al - set child's console tty
				   ; ah - reset child's wait channel
	; 23/07/2022
	mov	[esi+p.ttyc-1], al ; set child's console tty
	mov	eax, esi
	mov	[u.uno], al ; child process number
		;movb r1,u.uno / set child process number to r1
        inc     byte [esi+p.stat-1] ; 1, SRUN, 05/02/2014
		; incb p.stat-1(r1) / set p.stat entry for child 
				; / process to active status
		; mov u.ttyp,r2 / put pointer to parent process' 
			      ; / control tty buffer in r2
                ; beq 2f / branch, if no such tty assigned
		; clrb 6(r2) / clear interrupt character in tty buffer
	; 2:
	push	ebx  ; * return address for the child process
		     ; * Retro UNIX 8086 v1 feature only !
	; (Retro UNIX 8086 v1 modification!)
		; mov $runq+4,r2
	mov	ebx, runq+2 ; normal run queue ; 02/01/2017
	call	putlu
 		; jsr r0,putlu / put child process on lowest priority
			   ; / run queue
	; 23/07/2022
	shl	esi, 1
	;shl	si, 1
		; asl r1 / multiply r1 by 2 to get index 
		       ; / into p.pid table
	inc	word [mpid]
		; inc mpid / increment m.pid; get a new process name
	mov	ax, [mpid]
	mov	[esi+p.pid-2], ax
		;mov mpid,p.pid-2(r1) / put new process name 
				    ; / in child process' name slot
	pop	edx  ; * return address for the child process
		     ; * Retro UNIX 8086 v1 feature only !
  	pop	ebx  ; **
	;mov	ebx, [esp] ; ** parent process number
		; movb (sp),r2 / put parent process number in r2
	; 23/07/2022
	shl	ebx, 1
	;shl 	bx, 1
		;asl r2 / multiply by 2 to get index into below tables
	;movzx eax, word [ebx+p.pid-2]
	mov	ax, [ebx+p.pid-2]
		; mov p.pid-2(r2),r2 / get process name of parent
				   ; / process
	mov	[esi+p.ppid-2], ax
		; mov r2,p.ppid-2(r1) / put parent process name
			  ; / in parent process slot for child
	mov	[u.r0], eax
		; mov r2,*u.r0 / put parent process name on stack
			     ; / at location where r0 was saved
	mov 	ebp, [u.sp] ; points to return address (EIP for IRET)
	mov	[ebp], edx ; *, CS:EIP -> EIP
			   ; * return address for the child process
		; mov $sysret1,-(sp) /
		; mov sp,u.usp / contents of sp at the time when 
			      ; / user is swapped out
		; mov $sstack,sp / point sp to swapping stack space
	; 04/09/2015 - 01/09/2015
	; [u.usp] = esp
	push	sysret ; ***
	mov	[u.usp], esp ; points to 'sysret' address (***)
			     ; (for child process)
	xor 	eax, eax
	mov 	[u.ttyp], ax ; 0
	;
	call	wswap ; Retro UNIX 8086 v1 modification !
		; jsr r0,wswap / put child process out on drum
		; jsr r0,unpack / unpack user stack
		; mov u.usp,sp / restore user stack pointer
		; tst (sp)+ / bump stack pointer
	; Retro UNIX 386 v1 modification !
	pop	eax ; ***
	;shl	bx, 1
	; 23/07/2022
	shl	ebx, 1
	mov     eax, [ebx+p.upage-4] ; UPAGE address ; 14/05/2015
	call	rswap ; restore parent process 'u' structure,
		      ; registers and return address (for IRET)
		;movb (sp)+,u.uno / put parent process number in u.uno
        movzx   eax, word [mpid]
	mov	[u.r0], eax
		; mov mpid,*u.r0 / put child process name on stack
			       ; / where r0 was saved
		; add $2,18.(sp) / add 2 to pc on stack; gives parent
			          ; / process return
	;xor	ebx, ebx
	xor     esi, esi
		;clr r1
sysfork_4: ; 1: / search u.fp list to find the files
	      ; / opened by the parent process
	; 01/09/2015
	;xor	bh, bh
	;mov 	bl, [esi+u.fp]
	mov 	al, [esi+u.fp]
		; movb u.fp(r1),r2 / get an open file for this process
        ;or	bl, bl
	or	al, al
	jz	short sysfork_5	
		; beq 2f / file has not been opened by parent,
		       ; / so branch
	;mov	ah, 10 ; Retro UNIX 386 v1 fsp structure size = 10 bytes
	;mul	ah
	; 23/07/2022
	; Retro UNIX 386 v2 & TRDOS 386 v2.0.5 fsp struc size = 16 bytes
	;mov	ebx, eax
	;shl	ebx, 4 ; * 16
	;inc	byte [ebx+fsp-10]

	; 23/07/2022 (BugFix)
	inc	byte [OF_OPENCOUNT+eax]

	;;movzx	ebx, ax
	;mov	bx, ax
	;shl	bx, 3
		; asl r2 / multiply by 8
       		; asl r2 / to get index into fsp table
       		; asl r2
  	;inc	byte [ebx+fsp-2]
		; incb fsp-2(r2) / increment number of processes
			     ; / using file, because child will now be
			     ; / using this file
sysfork_5: ; 2:
        inc     esi
		; inc r1 / get next open file
	; 23/07/2022
	cmp	si, OPENFILES ; = 10
	;cmp	si, 10
		; cmp r1,$10. / 10. files is the maximum number which
			    ; / can be opened
	jb	short sysfork_4	
		; blt 1b / check next entry
	jmp	sysret
		; br sysret1

syscreat: ; < create file >
	; 29/08/2024
	; 25/08/2024 - TRDOS 386 v2.0.9	
	; 08/08/2022
	; 23/07/2022 - TRDOS 386 Kernel v2.0.5
	; 13/11/2017
	; 27/10/2016
	; 25/10/2016 - 26/10/2016
	; 15/10/2016 - 16/10/2016 - 17/10/2016
	; 10/10/2016 (TRDOS 386 = TRDOS v2.0)
	;	     -derived from INT_21H.ASM-
	;            ("loc_INT21h_create_file")
        ; 	10/07/2011 (12/03/2011)
        ;	INT 21h Function AH = 3Ch
        ;	Create File
        ;	INPUT
        ;	   CX = Attributes
        ;          DS:DX= Address of zero terminaned path name
        ;
	; 27/12/2015 (Retro UNIX 386 v1.1)
	; 14/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 27/05/2013 (Retro UNIX 8086 v1)
	;
	; 'syscreat' called with two arguments; name and mode.
	; u.namep points to name of the file and mode is put
	; on the stack. 'namei' is called to get i-number of the file.
	; If the file aready exists, it's mode and owner remain 
	; unchanged, but it is truncated to zero length. If the file
	; did not exist, an i-node is created with the new mode via
	; 'maknod' whether or not the file already existed, it is
	; open for writing. The fsp table is then searched for a free
	; entry. When a free entry is found, proper data is placed
	; in it and the number of this entry is put in the u.fp list.
	; The index to the u.fp (also know as the file descriptor)
	; is put in the user's r0.
	;
	; Calling sequence:
	;	syscreate; name; mode
	; Arguments:
	;	name - name of the file to be created
	;	mode - mode of the file to be created
	; Inputs: (arguments)
	; Outputs: *u.r0 - index to u.fp list 
	;		   (the file descriptor of new file)
	; ...............................................................
	;
	; Retro UNIX 8086 v1 modification:
	;       'syscreate' system call has two arguments; so,
	;	* 1st argument, name is pointed to by BX register
	;	* 2nd argument, mode is in CX register
	;
	;	AX register (will be restored via 'u.r0') will return
	;	to the user with the file descriptor/number 
	;	(index to u.fp list).
	;
	;call	arg2
	; * name - 'u.namep' points to address of file/path name
	;          in the user's program segment ('u.segmnt')
	;          with offset in BX register (as sysopen argument 1).
	; * mode - sysopen argument 2 is in CX register 
	;          which is on top of stack.
	;
	; TRDOS 386 (10/10/2016)
	;
        ; INPUT ->
        ;	   CL = File Attributes
	;     	      bit 0 (1) - Read only file (R)
	;             bit 1 (1) - Hidden file (H)
        ;             bit 2 (1) - System file (R)
	;             bit 3 (1) - Volume label/name (V)
        ;             bit 4 (1) - Subdirectory (D)
	;	      bit 5 (1) - File has been archived (A)
        ;          EBX = Pointer to filename (ASCIIZ) -path-
	;
	; OUTPUT ->
	;          eax = File/Device Handle/Number (index) (AL)
	;          cf = 1 -> Error code in AL
	;
	; Modified Registers: EAX (at the return of system call)
	; 
	; Note: If the file is existing and it has not any one
	;	of S,H,R,V,D attributes, it will be truncated
	;	to zero length; otherwise, access error will be
	;	returned.

sysmkdir_0:
	test	cl, 08h ; Volume name
	jz	short syscreat_0

	; Volume name or long name creation
	; is not permitted (in TRDOS 386)!
	mov	eax, ERR_FILE_ACCESS  ; 11 ; 'permission denied !'
        jmp	sysopen_dev_err ; 08/08/2022
syscreat_0:
        ;mov	[u.namep], ebx
	push	ecx
	mov	esi, ebx
	; file name is forced, change directory as temporary
	;mov	ax, 1
	;mov	[FFF_Valid], ah ; 0 ; reset ; 17/10/2016
	;call	set_working_path 
	call	set_working_path_x ; 17/10/2016	
	;jc	short syscreat_err
	; 23/07/2022
	jnc	short syscreat_3
	jmp	syscreat_err

syscreat_3:
	; 16/10/2016
	cmp	byte [SWP_inv_fname], 0
	ja	short syscreat_inv_fname ; invalid file name !

	; Here, we have a valid path and also a valid file name
	; (Working dir has been changed if the path
	;  -file name string- had contained a dir name.)

	;xor	ax, ax
	; 25/08/2024
	xor	eax, eax
	;mov	esi, FindFile_Name
	call	find_first_file
	pop	ecx
		; ESI = Directory Entry (FindFile_DirEntry) Location
		; EDI = Directory Buffer Directory Entry Location
		; EAX = File Size
		;  BL = Attributes of The File/Directory
		;  BH = Long Name Yes/No Status (>0 is YES)
		;  DX > 0 : Ambiguous filename chars are used
	jc	short syscreat_1 ; file not found (the good!)
				 ; or another error (the bad!)

	; (& the uggly!) truncate file to zero length before open

	;'*' and '?' already checked at 'set_working_path' stage
	;and	dx, dx
	;jnz	short sysmkdir_err ; permission denied
				   ; invalid filename chars

	;test	cl, 10h ; subdirectory ?
	;jnz	short sysmkdir_err

	; BL = File Attributes:
	;     	      bit 0 (1) - Read only file (R)
	;             bit 1 (1) - Hidden file (H)
        ;             bit 2 (1) - System file (R)
	;             bit 3 (1) - Volume label/name (V)
        ;             bit 4 (1) - Subdirectory (D)
	;	      bit 5 (1) - File has been archived

	; * existing directory must not be truncated
	;   (we don't know it is empty or not, at this stage)
	; * existing volume name (or a long name) can not be
	;   re-created or truncated by 'syscreat'
	; * A file with S, H, R attributes must not be truncated
	;   (change attributes to normal, if you need truncate it)

	test	bl, 00011111b  ; check attributes of existing file
	jnz	short sysmkdir_err

	;; normal file, OK to continue...

	; ESI = FindFile_DirEntry
	mov	ax, [esi+DirEntry_FstClusHI] ; 20
	shl	eax, 16 ; 13/11/2017
	mov	ax, [esi+DirEntry_FstClusLO] ; 26

	; 24/08/2024
	and	eax, eax
	jz	short skip_truncate ; first cluster is 0 !

	; EAX = First cluster to be truncated/unlinked
	push	edi
	push	ecx
	mov	esi, Logical_DOSDisks
	sub	ecx, ecx
	;mov	ch, [Current_Drv] ; = [FindFile_Drv]
	; 03/09/2024
	mov	ch, [FindFile_Drv]
	add	esi, ecx
	; ESI = Logical dos drive description table address
	call	truncate_cluster_chain
	pop	ecx
	pop	edi
	jc	short syscreate_truncate_err

	; 03/09/2024
	mov	word [edi+DirEntry_FstClusHI], 0
	mov	word [edi+DirEntry_FstClusLO], 0

	; 24/08/2024
skip_truncate:
	; 26/10/2016
	; EDI = Directory entry address in directory buffer
	; Update directory entry
	call	convert_current_date_time
	; OUTPUT -> DX = Date in dos dir entry format
        ; 	    AX = Time in dos dir entry format
	mov	[edi+DirEntry_WrtTime], ax
	mov	[edi+DirEntry_WrtDate], dx
	mov	[edi+DirEntry_LastAccDate], dx
	xor	eax, eax ; file size = 0 
	mov	[edi+DirEntry_FileSize], eax ; 0
	mov	byte [DirBuff_ValidData], 2 ; data changed sign	
	;mov	esi, FindFile_DirEntry
	; 03/09/2024
	mov	dl, 1 ; open file for writing
	jmp	sysopen_2 ; 08/08/2022

sysmkdir_err:
	; 1 = write, 2 = read & write, >2 = invalid
        mov	eax, ERR_FILE_ACCESS  ; 11 ; 'permission denied !'
        jmp	short sysopen_err

syscreate_truncate_err:
	mov	eax, ERR_DRV_WRITE ; 18 ; 'disk write error !'
        jmp	short sysopen_err

syscreat_inv_fname:  ; invalid file name chars
	; 16/10/2016
	mov	eax, ERR_INV_FILE_NAME  ; 26 ; invalid file name chars 
	pop	ecx
	jmp	short sysopen_err

syscreat_1:
	; Error code in EAX
        cmp	al, 02h ; 'File not found' error
        jne	short sysopen_err

	test	cl, 10h ; Directory
	;jnz	sysmkdir_2
	; 23/07/2022
	jz	short syscreat_2
	jmp	sysmkdir_2

syscreat_2:
	mov	esi, FindFile_Name
        ;xor	edx, edx
        xor	eax, eax ; File Size  = 0
	xor	ebx, ebx
	dec 	ebx ; FFFFFFFFh -> create empty file
	            ;              (only for FAT fs)
	; CL = File Attributes
	call	create_file
	jc	short sysopen_err
		; EAX = New file's first cluster
		; ESI = Logical Dos Drv Descr. Table Addr.
		; EBX = offset CreateFile_Size
		; ECX = Sectors per cluster (<256) 
		; EDX = Directory entry index/number (<65536)
		; 29/08/2024
		; EBX = File Size (0 for a new, empty file)
		; ECX = Directory Entry Index/Number (<2048)
		;      (in directory cluster, not in directory)
		; EDX = Directory Cluster Number (of the file)

	; 26/10/2016
	;mov	esi, Directory_Buffer
	;shl	dx, 5 ; *32
	;add	esi, edx
	;; esi = directory entry address in directory buffer

	; Here, directory entry has been created but last
	; modification date & time of the parent dir has not
	; been updated, yet! 
	; (Note: Directory and FAT buffers have been updated...)

	call	update_parent_dir_lmdt ; now, it is OK too!

	; 25/10/2016
	mov	ax, 1800h
	mov	esi, FindFile_Name
	call	find_first_file
	jc	short sysopen_err

	; Only possible error after here is
	; "too many open files !" error.
	;
	; If "syscreat" will return with that error,
	; (the file has been created but it could not be opened)
	; the user must retry to open this file again
	; or must close another file before using 
	; "sysopen" system call.

	mov	dl, 1 ; open file for writing
	; ESI = Directory Entry (FindFile_DirEntry) Location
	; EAX = File Size (= 0)
	jmp	short sysopen_2

sysopen: ;<open file>
	; 03/09/2024
	; 19/08/2024 - TRDOS 386 v2.0.9
	; 23/07/2022 - TRDOS 386 v2.0.5
	; 17/04/2021 - TRDOS 386 v2.0.4
	;	(temporary modifications)
	; 26/10/2016
	; 24/10/2016
	; 17/10/2016
	; 15/10/2016
	; 06/10/2016, 07/10/2016, 08/10/2016
	; 05/10/2016 (TRDOS 386 = TRDOS v2.0)
	;	     -derived from INT_21H.ASM-
	;            ("loc_INT21h_open_file")
        ; 	26/02/2011 
        ;	INT 21h Function AH = 3Dh
        ;	Open File
        ;	INPUT
        ;	   AL= File Access Value
	;     	     0- Open for reading
	;            1- Open for writing
        ;            2- Open for reading and writing
        ;          DS:DX= Pointer to filename (ASCIIZ)
        ;
	; 14/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 22/05/2013 - 27/05/2013 (Retro UNIX 8086 v1)
	;
	; 'sysopen' opens a file in following manner:
	;    1) The second argument in a sysopen says whether to
	;	open the file ro read (0) or write (>0).
	;    2) I-node of the particular file is obtained via 'namei'.
	;    3) The file is opened by 'iopen'.
	;    4) Next housekeeping is performed on the fsp table
	;	and the user's open file list - u.fp.
	;	a) u.fp and fsp are scanned for the next available slot.
	;	b) An entry for the file is created in the fsp table.
	;	c) The number of this entry is put on u.fp list.
	;	d) The file descriptor index to u.fp list is pointed
	;	   to by u.r0.
	;
	; Calling sequence:
	;	sysopen; name; mode
	; Arguments:
	;	name - file name or path name
	;	mode - 0 to open for reading
	;	       1 to open for writing
	; Inputs: (arguments)
	; Outputs: *u.r0 - index to u.fp list (the file descriptor)
	;		  is put into r0's location on the stack.
	; ...............................................................
	;
	; Retro UNIX 8086 v1 modification:
	;       'sysopen' system call has two arguments; so,
	;	* 1st argument, name is pointed to by BX register
	;	* 2nd argument, mode is in CX register
	;
	;	AX register (will be restored via 'u.r0') will return
	;	to the user with the file descriptor/number 
	;	(index to u.fp list).
	;
	;call	arg2
	; * name - 'u.namep' points to address of file/path name
	;          in the user's program segment ('u.segmnt')
	;          with offset in BX register (as sysopen argument 1).
	; * mode - sysopen argument 2 is in CX register 
	;          which is on top of stack.
	;
	; jsr r0,arg2 / get sys args into u.namep and on stack
	;
       	; system call registers: ebx, ecx (through 'sysenter')
	;
	; TRDOS 386 (05/10/2016)
	;
        ; INPUT ->
        ;	   CL = File Access Value (Open Mode)
	;     	      0 - Open file for reading
	;             1 - Open file for writing
        ;             2 - Open device for reading
	;	      3 - Open device for writing
        ;          EBX = Pointer to filename/devicename (ASCIIZ)
	; OUTPUT ->
	;          eax = File/Device Handle/Number (index) (AL)
	;          cf = 1 -> Error code in AL
	;
	; Modified Registers: EAX (at the return of system call)
	;

	cmp	cl, 1 ; read file (0), write file (1)
	jna	short sysopen_0

	; 17/04/2021 (temporary)
	;cmp	cl, 3
	;jna	sysopen_device

	; Invalid access code
	mov	eax, ERR_INV_PARAMETER
	;jmp	sysopen_dev_err

sysopen_dev_err:
	mov	[u.r0], eax
	mov	[u.error], eax
	jmp	error

sysopen_0:
	;mov	[u.namep], ebx
	push	ecx
	mov	esi, ebx
	; file name is forced, change directory as temporary
	;mov	ax, 1
	;mov	[FFF_Valid], ah ; 0 ; reset ; 17/10/2016
	;call	set_working_path
	call	set_working_path_x ; 17/10/2016	
	jnc	short sysopen_1

syscreat_err: ; ecx = file attributes (for 'syscreat')
	pop	ecx ; open mode  
	and	eax, eax  ; 0 -> Bad Path!
	jnz	short sysopen_err
	; eax = 0
	mov	eax, ERR_DIR_NOT_FOUND ; Directory not found !
sysopen_err:
	mov	[u.r0], eax
	mov	[u.error], eax
	call 	reset_working_path
	jmp	error

sysopen_1:
	;mov	esi, FindFile_Name
        mov	ax, 1800h ; Only files
	call	find_first_file
	pop	edx
	jc	short sysopen_err ; eax = 2 (File not found !)

	; check_open_file_attr_access_code

        test	bl, 7  ; system, hidden, readonly
        jz	short sysopen_2

	and	dl, dl ; 0 = read mode
	jz	short sysopen_2

sysopen_access_err:
	; 1 = write, 2 = read & write, >2 = invalid
        mov	eax, ERR_FILE_ACCESS ; 11 = 'permission denied !'
        jmp	short sysopen_err

sysopen_2:
	; esi = Directory Entry (FindFile_DirEntry) Location
	;mov	ebx, esi
        ; 03/09/2024
	mov	ebx, edi ; Directory entry in directory buffer
	xor     esi, esi ; 0
        xor     edi, edi ; 0
sysopen_3: ; scan the list of entries in fsp table
        cmp     byte [esi+u.fp], 0
        jna     short sysopen_4 ; empty slot
        ;inc	si
	; 19/08/2024
	inc	esi
        cmp     si, 10
	jb	short sysopen_3
toomanyf:
	mov	eax, ERR_TOO_MANY_FILES ; too many open files !
	jmp	short sysopen_err

sysopen_4: 
        cmp     byte [edi+OF_MODE], 0 ; Scan open files table
	jna     short sysopen_5

	;inc	di
	; 19/08/2024
	inc	edi
	cmp     di, OPENFILES ; max. number of open files (sytem)
	jb	short sysopen_4
	jmp	short toomanyf

sysopen_5:
	; 19/08/2024
	;;;
	push	eax ; size
        mov 	ax, [ebx+DirEntry_FstClusHI]
	shl	eax, 16
	mov 	ax, [ebx+DirEntry_FstClusLO]
	pop	ebx ; size
	; eax = First Cluster
	; ebx = File Size
	cmp	dl, 1 ; is open mode = open for write ?
	jne	short sysopen_8
	; check if the file is already open for write
	push	edi
	sub	edi, edi
	mov	dl, [FindFile_Drv]
sysopen_5_@:
	cmp 	byte [edi+OF_MODE], 2 ; open for write
	jne	short sysopen_5_@@@
	cmp	dl, [edi+OF_DRIVE]
	jne	short sysopen_5_@@@
	shl 	edi, 2
	cmp	eax, [edi+OF_FCLUSTER]
	jne	short sysopen_5_@@
	pop	edi
	jmp	short sysopen_access_err
sysopen_5_@@:
	shr 	edi, 2
	;;;
sysopen_5_@@@:
	inc	edi
	cmp	di, OPENFILES
	jb	short sysopen_5_@
	pop	edi
	mov	dl, 1 ; open for write
sysopen_8:
	inc	dl
        mov     [edi+OF_MODE], dl
	mov	dl, [FindFile_Drv]
        mov     [edi+OF_DRIVE], dl ; Logical DOS drive number
	;shl	di, 2 ; *4 (dword offset)
	; 23/07/2022
	shl	edi, 2

	; 19/08/2024
	;mov	[edi+OF_SIZE], eax ; File size in bytes
	mov	[edi+OF_SIZE], ebx ; File size in bytes

	; 19/08/2024
	; eax = Fist Cluster
        ;mov 	ax, [ebx+DirEntry_FstClusHI]
	;shl	eax, 16
	;mov 	ax, [ebx+DirEntry_FstClusLO]

	mov     [edi+OF_FCLUSTER], eax ; First cluster
	mov     [edi+OF_CCLUSTER], eax ; Current cluster

        xor	ebx, ebx
        mov     [edi+OF_POINTER], ebx ; offset pointer (0)
        mov     [edi+OF_CCINDEX], ebx ; cluster index (0)

	mov	eax, [FindFile_DirFirstCluster]
	mov	[edi+OF_DIRFCLUSTER], eax

	mov	eax, [FindFile_DirCluster]
	mov	[edi+OF_DIRCLUSTER], eax

	; Get (& Save) Volume ID
	; Important for files of removable drives
	; (In order to check the drive has same volume/disk)
	mov	bh, dl
        add	ebx, Logical_DOSDisks
        mov	al, [ebx+LD_FATType]
        cmp	al, 1
        jb	short sysopen_6_fs
        cmp	al, 2
        ja	short sysopen_6_fat32
sysopen_6_fat:
        mov	eax, [ebx+LD_BPB+VolumeID]
        jmp	short sysopen_7
sysopen_6_fs:
        mov	eax, [ebx+LD_FS_VolumeSerial]
        jmp	short sysopen_7
sysopen_6_fat32:
        mov	eax, [ebx+LD_BPB+FAT32_VolID]
sysopen_7:
        mov	[Current_VolSerial], eax

	mov	[edi+OF_VOLUMEID], eax

	; 24/10/2016
	;shr	di, 1 ; 4/2, word offset
	; 23/07/2022
	shr	edi, 1
	mov	bx, [FindFile_DirEntryNumber]
	mov	[edi+OF_DIRENTRY], bx

	xor	edx, edx
	;;shr	di, 2 ; /4 (byte offset)
	;shr	di, 1 ; 2/2, byte offset
	; 23/07/2022
	shr	edi, 1
	mov	byte [edi+OF_OPENCOUNT], dl ; 0
	mov	byte [edi+OF_STATUS], dl ; 0

	mov	ebx, edi
	inc	bl

        mov     [esi+u.fp], bl ; Open File Entry Number
	mov     [u.r0], esi ; move index to u.fp list
			    ; into eax on stack

	call 	reset_working_path

	jmp	sysret


; fsp table (original UNIX v1)
;
;Entry
;          15                                      0
;  1     |---|---------------------------------------|
;        |r/w|       i-number of open file           |
;        |---|---------------------------------------|
;        |               device number               |
;        |-------------------------------------------|
;    (*) | offset pointer, i.e., r/w pointer to file |
;        |-------------------------------------------|
;        |  flag that says    | number of processes  |
;        |   file deleted     | that have file open  |
;        |-------------------------------------------|
;  2     |                                           |
;        |-------------------------------------------|
;        |                                           |
;        |-------------------------------------------|
;        |                                           |
;        |-------------------------------------------|
;        |                                           |
;        |-------------------------------------------|
;  3     |                                           |
;        |                                           |
;
; (*) Retro UNIX 386 v1 modification: 32 bit offset pointer

; 27/03/2020 - Retro UNIX 386 v2 - FSP (OPEN FILES) TABLE

;Entry
;         15                    7                   0
;  1     |-------------------------------------------|
;        |   	     i-number of open file           |
;        |-------------------------------------------|
;        |        high word of 32 bit i-number       |
;        |-------------------------------------------|
;        | open mode & status  |   device number     |
;        |-------------------------------------------|
;        |    reserved byte    |     open count      |
;        |-------------------------------------------|
;        | offset pointer, i.e., r/w pointer to file |
;        |-------------------------------------------|
;        |   64 bit file offset pointer (bit 16-31)  |
;        |-------------------------------------------|
;        |   64 bit file offset pointer (bit 32-47)  |
;        |-------------------------------------------|
;        |   64 bit file offset pointer (bit 48-63)  |
;        |-------------------------------------------|
;  2     |                                           |
;        |-------------------------------------------|
;        |                                           |
;        |-------------------------------------------|
;        |                                           |
;        |-------------------------------------------|
;        |                                           |
;        |-------------------------------------------|
;        |                                           |

; 23/07/2022 - TRDOS 386 Kernel v2.0.5
; OPENFILES equ 10 (sysdefs.s)
;; 06/10/2016
;; Open File Parameters (trdoskx.s)
;OF_FCLUSTER:	resd OPENFILES  ; First clusters of open files
;OF_DRIVE:	resb OPENFILES  ; Logical DOS drive numbers of open files
;OF_MODE:	resb OPENFILES  ; Open mode (1 = read, 2 = write, 3 = r&w)
;OF_STATUS:	resb OPENFILES  ; (bit 0 = read, bit 1 = write)
;OF_OPENCOUNT:	resb OPENFILES  ; Open counts of open files
;OF_POINTER:	resd OPENFILES	; File seek/read/write pointer
;OF_SIZE:	resd OPENFILES	; File sizes of open files (in bytes)
;OF_DIRFCLUSTER: resd OPENFILES  ; Directory First Clusters of open files
;OF_DIRCLUSTER:	resd OPENFILES  ; Directory (Entry) Clusters of open files
;OF_VOLUMEID:	resd OPENFILES  ; Vol ID for removable drives of open files
;OF_CCLUSTER:	resd OPENFILES  ; Current clusters of open files
;OF_CCINDEX:	resd OPENFILES  ; Cluster index numbers of current clusters
;; 24/10/2016
;OF_DIRENTRY:	resw OPENFILES  ; Directory entry index no. in dir cluster

; 17/04/2021
; ('sysopen_device' procedure is disabled as temporary)

;sysopen_device:
;	; 15/10/2016
;	; 08/10/2016
;	; 07/10/2016 (TRDOS 386 = TRDOS v2.0)
;	push	ecx ; open mode
;	mov	ebp, esp
;	mov	ecx, 16 ; transfer length = 16 bytes
;	sub	esp, ecx
;	mov	edi, esp ; destination address
;	mov 	esi, ebx ; dev name in user's memory space
;	call	transfer_from_user_buffer
;	jnc	short sysopen_dev_0
;	; eax = ERR_OUT_OF_MEMORY = 4 = ERR_MINOR_IM
;	pop	ecx
;sysopen_dev_err:
;	mov	[u.r0], eax
;	mov	[u.error], eax
;	jmp	error
;sysopen_dev_0:
;	mov	esi, edi ; Device name addr (max. 16 bytes, ASCIIZ)
;			 ; for example: "tty, TTY, /dev/tty"
;	call	get_device_number
;	mov	esp, ebp
;	pop	ecx
;	jnc	short sysopen_dev_1
;	mov	eax, ERR_INV_DEV_NAME ; 24 ; 'invalid device name !'
;	jmp	short sysopen_dev_err
;sysopen_dev_1:
;	; eax = Device Number (AL)
;	;  cl = Open mode (2 = device read, 3 = device write)
;       xor     ebx, ebx ; 0
;sysopen_dev_2: ; scan the list of entries
;       cmp     [ebx+u.fp], bl ; 0
;       jna     short sysopen_dev_3 ; empty slot
;       inc     bl
;       cmp     bl, 10
;	jb	short sysopen_dev_2
;	;
;	mov	eax, ERR_TOO_MANY_FILES ; too many open files !
;	jmp	short sysopen_dev_err
;sysopen_dev_3:
;	mov 	[u.r0], ebx ; File/Device index/handle/descriptor
;	; eax = device number (entry offset)
;	mov	ch, [eax+DEV_ACCESS] ; bit 0 = accessable by users
;				     ; bit 1 = read access perm
;				     ; bit 2 = write access perm
;				     ; bit 3 = IOCTL permit to users
;				     ; bit 4 = block device if set
;				     ; bit 5 = 16 bit or 1024 byte
;				     ; bit 6 = 32 bit or 2048 byte
;				     ; bit 7 = installable device drv
;	test 	ch, 1 ; accessable by normal users (except root)
;	jnz	short sysopen_dev_4 ; yes, permission has been given
;	cmp	byte [u.uid], 0 ; root?
;	jna	short sysopen_dev_4 ; superuser can open all devices
;sysopen_dev_perm_err:
;	mov	eax, ERR_DEV_ACCESS  ; 11 = 'permission denied !'
;	jmp	short sysopen_dev_err
;sysopen_dev_4:
;	shr	ch, 1 ; result: 1 = read, 2 = write, 3 = r & w
;	dec	cl  ; result: 1 = read, 2 = write
;	test	cl, ch
;	jz	short sysopen_dev_perm_err
;
;	shl	ch, 1 ; bit 0 = 0
;	; eax = device number (entry offset)
;	call	device_open
;	jc	short sysopen_dev_perm_err
;
;	; eax = device number (entry offset)
;	or	al, 80h ; set device bit (set bit 7 to 1)
;	mov	ebx, [u.r0]
;	mov	[ebx+u.fp], al	; bit 7 (=1) points to device
;
;	jmp	sysret

sysmkdir: ; < make directory >
	; 23/07/2022 - TRDOS 386 Kernel v2.0.5
	; 15/10/2016
	; 10/10/2016 (TRDOS 386 = TRDOS v2.0)
	;	     -derived from INT_21H.ASM-
	;            ("loc_INT21h_create_file")
        ; 	10/07/2011 (12/03/2011)
        ;	INT 21h Function AH = 3Ch
        ;	Create File
        ;	INPUT
        ;	   CX = Attributes
        ;          DS:DX= Address of zero terminaned path name
        ;
        ;
	; 14/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 27/05/2013 - 02/08/2013 (Retro UNIX 8086 v1)
	;
	; 'sysmkdir' creates an empty directory whose name is
	; pointed to by arg 1. The mode of the directory is arg 2.
	; The special entries '.' and '..' are not present.
	; Errors are indicated if the directory already exists or
	; user is not the super user. 
	;
	; Calling sequence:
	;	sysmkdir; name; mode
	; Arguments:
	;	name - points to the name of the directory
	;	mode - mode of the directory
	; Inputs: (arguments)
	; Outputs: -
	;    (sets 'directory' flag to 1; 
	;    'set user id on execution' and 'executable' flags to 0)
	; ...............................................................
	;
	; Retro UNIX 8086 v1 modification: 
	;       'sysmkdir' system call has two arguments; so,
	;	* 1st argument, name is pointed to by BX register
	;	* 2nd argument, mode is in CX register
	;
	; TRDOS 386 (10/10/2016)
	;
        ; INPUT ->
        ;	   CL = Directory Attributes
	;     	      bit 0 (1) - Read only file/dir (R)
	;             bit 1 (1) - Hidden file/dir (H)
        ;             bit 2 (1) - System file/dir (R)
	;             bit 3 (1) - Volume label/name (V)
        ;             bit 4 (1) - Subdirectory (D)
	;	      bit 5 (1) - File/Dir has been archived (A)
	;	   CX = 0 -> create normal directory
        ;          EBX = Pointer to directory name (ASCIIZ) -path-
	;
	; OUTPUT ->
	;          eax = First cluster of the new directory
	;          cf = 1 -> Error code in AL
	;
	; Modified Registers: EAX (at the return of system call)
	;
	; Note: If the file or directory is existing
	;	an access error will be returned.

	and	cx, cx ; if cx = 0 -> create a normal subdir
	jz	short sysmkdir_1

	test	cl, 10h ; if dir flags set, also use other flags
	;jnz	sysmkdir_0 ; jump to head of 'syscreat'
	; 23/07/2022
	jz	short sysmkdir_invf
sysmkdir_3:
	jmp	sysmkdir_0

sysmkdir_invf:
	; CX has wrong flags
	mov 	eax, ERR_INV_FLAGS
	jmp	sysopen_dev_err

sysmkdir_1:
	mov	cl, 10h ; set subdir flag and reset other flags
	;jmp	sysmkdir_0
	; 23/07/2022
	jmp	short sysmkdir_3 ; jump to head of 'syscreat'
sysmkdir_2: 
	; jump from 'syscreat' ; from 'syscreat_1'
	;  CL = Directory attributes/flags
	mov	esi, FindFile_Name 
	call	make_sub_directory
	;jc	sysopen_err       ; NOTE: Old type (TRDOS 8086)
				  ; error codes must be modified
				  ; for next TRDOS 386 versions
				  ; (10/10/2016)
				  ; Old (MSDOS type)
				  ; error codes (2011):
				  ;  2 = file not found
				  ;  3 = directory not found
				  ;  5 = access denied
				  ; 12 = no more files
			          ; 19 = disk write protected
				  ; 39 = insufficient disk space
				  ; 'sysdefs.s' ; 10/10/2016
	; 23/07/2022
	jnc	short sysmkdir_4
	jmp	sysopen_err

sysmkdir_4:	
	mov	[u.r0], eax ; New sub dir's first cluster
        call 	reset_working_path
	jmp	sysret

sysclose: ;<close file>
	; 24/04/2025 - TRDOS 386 v2.0.10
	; 23/07/2022 - TRDOS 386 v2.0.5
	; 06/10/2016 (TRDOS 386 = TRDOS v2.0)
	;
	; 14/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 22/05/2013 - 26/05/2013 (Retro UNIX 8086 v1)
	;
	; 'sysclose', given a file descriptor in 'u.r0', closes the
	; associated file. The file descriptor (index to 'u.fp' list)
	; is put in r1 and 'fclose' is called.
	;
	; Calling sequence:
	;	sysclose
	; Arguments:
	;	-  
	; Inputs: *u.r0 - file descriptor
	; Outputs: -
	; ...............................................................
	;
	; Retro UNIX 8086 v1 modification:
	;	 The user/application program puts file descriptor
	;        in BX register as 'sysclose' system call argument.
	; 	 (argument transfer method 1)

	; TRDOS 386 (06/10/2016)
	;
        ; INPUT ->
        ;	   EBX = File Handle/Number (file index) (AL)
	; OUTPUT ->
	;          cf = 0 -> EAX = 0
	;          cf = 1 -> Error code in EAX (ERR_FILE_NOT_OPEN)
	;
	; Modified Registers: EAX (at the return of system call)
	;

	mov 	eax, ebx
	xor	ebx, ebx
	mov	[u.r0], ebx ; 0  ; return value of EAX
	call 	fclose
	;jnc	sysret
	; 23/07/2022
	jc	short sysclose_err
	jmp	sysret
sysclose_err:
	; 24/04/2025
	;mov	eax, ERR_FILE_NOT_OPEN ; file not open !
	;mov	[u.error], eax ;
	mov	eax, [u.error]
	; eax = [u.error] = error code
	mov	[u.r0], eax
	jmp	error

sysread: ; < read from file >
	; 27/09/2024
	; 18/09/2024
	; 03/09/2024 (TRDOS v2.0.9)
	; 11/10/2016 (TRDOS 386 = TRDOS v2.0)
	;	     -derived from INT_21H.ASM-
	;            ("loc_INT21h_read_file")
        ; 	13/03/2011 (05/03/2011)
        ;	INT 21h Function AH = 3Fh
        ;	Read from a File
        ;	INPUT
	;	   BX = File Handle
        ;	   CX = Number of bytes to read
        ;          DS:DX= Buffer address
        ;
	; Note: TRDOS 386 'sysread' has been derived from 
	;	Retro UNIX 386 v1 'sysread', except a few 
	;	code modifications.
	;
	; 13/05/2015 (Retro UNIX 386 v1)
	; 11/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 23/05/2013 (Retro UNIX 8086 v1)
	;
	; 'sysread' is given a buffer to read into and the number of
	; characters to be read. If finds the file from the file
	; descriptor located in *u.r0 (r0). This file descriptor
	; is returned from a successful open call (sysopen).
	; The i-number of file is obtained via 'rw1' and the data
	; is read into core via 'readi'.
	;
	; Calling sequence:
	;	sysread; buffer; nchars
	; Arguments:
	;	buffer - location of contiguous bytes where 
	;		 input will be placed.
	;	nchars - number of bytes or characters to be read.
	; Inputs: *u.r0 - file descriptor (& arguments)
	; Outputs: *u.r0 - number of bytes read.
	; ...............................................................
	;				
	; Retro UNIX 8086 v1 modification: 
	;       'sysread' system call has three arguments; so,
	;	* 1st argument, file descriptor is in BX register
	;	* 2nd argument, buffer address/offset in CX register
	;	* 3rd argument, number of bytes is in DX register
	;
	;	AX register (will be restored via 'u.r0') will return
	;	to the user with number of bytes read. 
	;
	; TRDOS 386 (05/10/2016)
	;
        ; INPUT ->
        ;	   EBX = File handle (descriptor/index)
	;	   ECX = Buffer address
        ;          EDX = Number of bytes
	; OUTPUT ->
	;          EAX = Number of bytes have been read
	;          cf = 1 -> Error code in AL
	;
	; Modified Registers: EAX (at the return of system call)
	;

	; EBX = File descriptor
	call	getf1 
	jc	short device_read ; read data from device

	; EAX = First cluster of the file

	call	rw1	; 03/09/2024 (major modification)
	jnc	short sysread_0

sysrw_err:	; 03/09/2024
device_rw_error: ; 03/05/2025	
	mov	[u.r0], eax ; error code
	jmp	error
	 
sysread_0:
	call	readi
	jmp	short rw0

syswrite: ; < write to file >
	; 27/09/2024
	; 18/09/2024
	; 03/09/2024
	; 25/08/2024 - TRDOS 386 v2.0.9
	; 23/10/2016
	; 11/10/2016 (TRDOS 386 = TRDOS v2.0)
	;	     -derived from INT_21H.ASM-
	;            ("loc_INT21h_write_file")
        ; 	13/03/2011 (05/03/2011)
        ;	INT 21h Function AH = 40h
        ;	Write to a File
        ;	INPUT
	;	   BX = File Handle
        ;	   CX = Number of bytes to write
        ;          DS:DX= Buffer address
        ;
	; Note: TRDOS 386 'sysrwrite' has been derived from
	;	Retro UNIX 386 v1 'syswrite', except a few
	;	code modifications.
	;

	; 13/05/2015 (Retro UNIX 386 v1)
	; 11/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 23/05/2013 (Retro UNIX 8086 v1)
	;
	; 'syswrite' is given a buffer to write onto an output file
	; and the number of characters to write. If finds the file
	; from the file descriptor located in *u.r0 (r0). This file
	; descriptor is returned from a successful open or create call
	; (sysopen or syscreat). The i-number of file is obtained via
	; 'rw1' and buffer is written on the output file via 'write'.
	;
	; Calling sequence:
	;	syswrite; buffer; nchars
	; Arguments:
	;	buffer - location of contiguous bytes to be writtten.
	;	nchars - number of characters to be written.
	; Inputs: *u.r0 - file descriptor (& arguments)
	; Outputs: *u.r0 - number of bytes written.
	; ...............................................................
	;
	; Retro UNIX 8086 v1 modification: 
	;       'syswrite' system call has three arguments; so,
	;	* 1st argument, file descriptor is in BX register
	;	* 2nd argument, buffer address/offset in CX register
	;	* 3rd argument, number of bytes is in DX register
	;
	;	AX register (will be restored via 'u.r0') will return
	;	to the user with number of bytes written. 
	;
	; INPUT ->
        ;	   EBX = File handle (descriptor/index)
	;	   ECX = Buffer address
        ;          EDX = Number of bytes
	; OUTPUT ->
	;          EAX = Number of bytes have been written
	;          cf = 1 -> Error code in AL
	;
	; Modified Registers: EAX (at the return of system call)
	;

	; EBX = File descriptor
	call	getf1
	jc	short device_write ; write data to device
	; EAX = First cluster of the file
	; EBX = File number (Open file number) ; 23/10/2016

	call	rw1	; 03/09/2024 (major modification)
	; 03/09/2024
	jc	short sysrw_err

	; 25/08/2024 (*)
	;jnc	short syswrite_0
	;mov	[u.r0], eax ; error code
	;jmp	error

	; 25/08/2024 - TRDOS 386 v2.0.9 (bugfix) (*)
	; (if eax = 0, lets add a cluster at 'mget_w_0')
	; ('mget_w' and 'add_new_cluster' procs are modified)

syswrite_0:
	call	writei	; 24/08/2024 ('mget_w' modification)
rw0: ; 1:
        mov	eax, [u.nread]
	mov	[u.r0], eax
	jmp	sysret

	; 17/04/2021 (temporary)
device_write:
device_read:
	; 03/05/2025
	; 24/04/2025 - TRDOS 386 v2.0.10
	; 26/09/2024
	; 18/09/2024 - TRDOS 386 v2.0.9
	; 17/04/2021 - TRDOS 386 v2.0.4
	;	(temporary modifications)
	;
	; 11/10/2016 (TRDOS 386 = TRDOS v2.0)
	; cl = DEV_OPENMODE ; open mode
	; ch = DEV_ACCESS   ; access flags
	; al = DEV_DRIVER   ; device number (eax)

	; 24/04/2025 (temporary)
	;; 18/09/2024 (temporary)
	;call	rw2 ; file not open ; cf = 1
	;;jmp	error
	;; 26/09/2024
	;jmp	short sysrw_err
	; 03/05/2025 - Temporary !
	mov	eax, ERR_DEV_ACCESS ; 11
			; 'permission denied !' error 
	jmp	device_rw_error

;	test	cl, 1 ; 1 = read, 2 = write, 3 = read&write
;	jz	short rw3
;
;	mov	ebx, eax
;	shl	bx, 2 ; *4
;
;	test	ch, 80h ; bit 7, installable device driver flag
;	jz	short d_read_2 ; Kernel device
;	; installable device
;d_read_1:
;       jmp	dword [ebx+IDEV_RADDR-4]
;d_read_2:
;	jmp	dword [ebx+KDEV_RADDR-4]

;device_write:
	; 17/04/2021 - TRDOS 386 v2.0.4
	;	(temporary modifications)
	;
	; 11/10/2016 (TRDOS 386 = TRDOS v2.0)
	; cl = DEV_OPENMODE ; open mode
	; ch = DEV_ACCESS   ; access flags
	; al = DEV_DRIVER   ; device number (eax)

	; 17/04/2021 (temporary)
	;jmp	short rw2 ; file not open

;	test	cl, 2 ; 1 = read, 2 = write, 3 = read&write
;	jz	short rw3
;
;	mov	ebx, eax
;	shl	bx, 2 ; *4
;
;	test	ch, 80h ; bit 7, installable device driver flag
;	jz	short d_write_2 ; Kernel device
;	; installable device
;d_write_1:
;       jmp	dword [ebx+IDEV_WADDR-4]
;d_write_2:
;	jmp	dword [ebx+KDEV_WADDR-4]

rw1:
	; 27/09/2024	
	; 03/09/2024 (TRDOS 386 v2.0.9)
	; 17/04/2021 (TRDOS 386 v2.0.4)
	; 11/10/2016 (TRDOS 386 = TRDOS v2.0)
	; 14/05/2015 (Retro UNIX 386 v1)
	; 11/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 23/05/2013 - 24/05/2013 (Retro UNIX 8086 v1)
	; System call registers: ebx, ecx, edx (through 'sysenter')
	;
	; EBX = File descriptor
	;call	getf1 ; calling point in 'getf' from 'rw1'
	;jc	short device_rw ; read/write data from/to device
	; EAX = First cluster of the file

	; 03/09/2024
	or	eax, eax
	jz	short rw7 ; eax = 0 -> empty file (OK for now)

	cmp 	eax, 2	  ; is it valid cluster number ?
	;jb	short rw2
	; 03/09/2024
	jnb	short rw6 ; yes, check upper limit

	;;;
	; eax = 1 -> invalid cluster number
rw5:
	mov	eax, ERR_CLUSTER ; 35 ; 'cluster not available !'
	jmp	short rw4 ; cf = 1

rw6:	; 03/09/2024 (check cluster number is valid or not)
	push	ebx
	; ebx <= OPENFILES-1 ; 0-31
	mov	bh, [ebx+OF_DRIVE] ; drive number * 256
	sub	bl, bl ; 27/09/2024
	mov	ebx, [ebx+Logical_DOSDisks+LD_Clusters]
	inc	ebx ; cluster count + 1 = last cluster number
	cmp	ebx, eax
	pop	ebx
	jb	short rw5
rw7:
	;;;

	mov	[u.base], ecx 	; buffer address/offset 
				;(in the user's virtual memory space)
	mov	[u.count], edx 
        mov	dword [u.error], 0 ; reset the last error code
	retn

; 24/04/2025 - TRDOS 386 v2.0.10
%if 0

rw2:
	mov	eax, ERR_FILE_NOT_OPEN ; file not open !
	;mov	dword [u.error], eax
	;retn

%endif

	; 03/09/2024
	; 17/04/2021
	;jmp	short rw4

	; 03/09/2024
rw3: 
;	mov	eax, ERR_FILE_ACCESS ; permission denied !
;	stc

rw4:	; 17/04/2021
	mov	dword [u.error], eax
	retn

systimer:
	; 23/07/2022 - TRDOS 386 Kernel v2.0.5
	; 02/01/2017
	; 21/12/2016
	; 19/12/2016
	; 10/12/2016 (callback)
	; 10/06/2016
	; 07/06/2016
	; 06/06/2016
	; 21/05/2016
	; 19/05/2016
	; 18/05/2016 - TRDOS 386 (TRDOS v2.0)
	; (TRDOS 386 feature only!)
	;
	; (start or stop timer event(s))
	;
	; INPUT ->
	;	BL = Signal return byte (response byte)
	;	     (Any requested value between 0 and 255)
	;	     (Kernel will put it at the requested address)
	;	BH = Time count unit
	;	     0 = Stop timer event
	;	     1 = 18.2 ticks per second
	;	     2 = 10 milliseconds
	;	     3 = 1 second (for real time clock interrupt)
	;	     4 = time/tick count in current time count unit
	;	    // 10/12/2016
	;	    80h = Stop timer event (callback method)
	;	    81h = 18.2 ticks per second, callback method
	;	    82h = 10 milliseconds, callback method
	;	    83h = 1 second (for RTC int), callback method
	;	    84h = current time count unit, callback method
	;
	;	    Note: Only 03h or 83h will set real time clock
	;		  (RTC) events (Others are for PIT events)!
	;	
	;	NOTE: If callback (user service) method is used,
	;	    EDX will point to the return address (of service
	;	    procedure) in user's space instead of signal 
	;	    response byte address. (TRDOS 386 kernel will
	;	    direct the cpu to that address -in user's space-
	;	    at the return of system call or interrupt 
	;	    just after the adjusted count/time is elapsed.)
	;	    User's sevice routine must be ended with a
	;	    'iret'. Normal return addresses from system 
	;	    calls or and interrupts will be kept same except
	;	    the timer returns.
     	;
	;	BH = 0 -> Stop timer event
	;	BL = Timer event number (1 to 255) if BH = 0
	;	     If BL = 0, all timer events (which are belongs
	;	      to running process) will be stopped
	;	ECX = Time/Tick count (depending on time count unit)
	;	EDX = Signal return (Response) byte address
	;	      (virtual address in user's memory space)
	; OUTPUT ->
	;	AL = Timer event number	(1 to 255) (max. value = 16)
	;	IF BH Input = 0 & CF = 0 & AL = 0 -> 
	;	     timer event(s) has/have been stopped/finished
	;	CF = 1 & AL = 0 -> no timer setting space to set
	;	CF = 1 & AL > 0 -> timer count unit is not usable
	;
	;	NOTE: To modify a time count for a user function,
	;	      at first, current timer event must be stopped
	;	      then a new timer event (which is related with
	;	      same user function) must be started.
	;		
	;	      Signal return (response) byte may be used for
	;	      several purposes. Kernel will put this value
	;	      to requested address during timer interrupt,
	;	      program/user can check this value to understand
	;	      which event has been occurred and what is changed.
	;	      (Multi timer events can share same signal address)
	;	
	;	NOTE: If the process is running while the time count
	;	      is reached, kernel will put signal return (response)
	;	      byte value at requested address during timer
	;	      interrupt and the process will continue to run.
	;	      Program/process must call (jump to) it's timer event
	;	      function as required, for checking the timer event
	;	      status via signal return (response) byte address.
	;
	;	      If the process is not running (waiting or sleeping
	;	      or released) while the time count is reached,
	;	      it is restarted from where it left, to ensure
	;	      proper multi media (video, audio, clock, timer)
	;	      functionality.
	;
	;	      (It is better to use 'syswait' or 'syssleep',
	;	      or 'sysrele' system call just after the timer
	;	      function. Otherwise, timer events may block other
	;	      processes which are not using timer events.)
	;
	; Timer Event Structure: (max. 16 timer events, 16*16 bytes)
	;       Owner:	        resb 1 ; 0 = free
	;		  	       ;>0 = process number (u.uno)
	;	Calback:	resb 1 ; 1 = callback, 0 = response byte
	;	Interrupt:      resb 1 ; 0 = Timer interrupt (or none)
	;		   	       ; 1 = Real Time Clock interrupt
	;	Response:       resb 1 ; 0 to 255, signal return value
	;	Count Limit:	resd 1 ; count of ticks (total/set)
	;	Current Count: 	resd 1 ; count of ticks (current)
	;	Response Addr:  resd 1 ; response byte (pointer) address
	;

	; 19/12/2016 (timer callback)
	mov	byte [tcallback], 0
	mov	byte [trtc], 0
	mov	dword [u.tcb], 0 ; this is not necessary...

	cmp	bh, 80h
	jb	short systimer_cb2
	ja	short systimer_cb0

	xor	edx, edx ; 0, reset callback address
	jmp	short systimer_cb1

systimer_cb0:
	cmp	bh, 84h	
	ja	short systimer_5 ;  undefined, error

	;mov	byte [tcallback], 1 ; 19/12/2016
	inc	byte [tcallback]

systimer_cb1:
	movzx	esi, byte [u.uno] ; process number
	;shl	si, 2	
	; 23/07/2022
	shl	esi, 2
	mov	[esi+p.tcb-4], edx ; set process timer callback address
				   ; (overwrite prev value if it is set!)
	and	bh, 7Fh

systimer_cb2:
	cmp	bh, 2
        je      short systimer_5  ; only 18.2 ticks per second is usable
				  ; 10 milliseconds (100 Hertz) timer 
				  ; will be set later (18/05/2016)
        ja      short systimer_6 

	and	bh, bh
	;jz	systimer_9        ; stop timer event(s)
	; 23/07/2022
	jnz	short systimer_19
	jmp	systimer_9

	; bh = 1 (timer interrupt, 18.2 Hz, IBM PC/AT ROMBIOS default)

systimer_19:
	mov	al, 10 ; (*)

systimer_0:
	mov	bh, 16
	;
	cmp	[timer_events], bh ; 16 ; 07/06/2016
	jnb 	short systimer_3  ; max. 16 timer events
	;
	push	eax ; (*)

	mov	edi, timer_set  ; beginning address of timer events
				; setting space
	xor	al, al ; 0
systimer_1:
	inc	al
	cmp	byte [edi], 0 	; is it free space ?
	jna	short systimer_7 ; yes
	dec	bh
	jz	short systimer_2
	add	edi, 16
	jmp	short  systimer_1 ; next event space

systimer_2:
	pop	eax ; (*) discard
systimer_3:
	mov	byte [u.r0], 0
systimer_4:
        mov     dword [u.error], ERR_MISC
                                ; one of miscellaneous/other errors
	jmp	error ; cf -> 1

systimer_5:
	mov	[u.r0], bh ; Time count unit (=2 or >3)
	jmp	short systimer_4 ; 07/06/2016

systimer_6:
	cmp	bh, 4
        ja      short systimer_5  ; undefined time count unit
	;jb	short systimer_16

	;mov	al, 1	; default (use current timer unit)
			; countdown value is in ECX !
			; max. value of ecx = 4294967296/10
        ;jmp    short systimer_0
	;jmp	short systimer_19
	je	short systimer_19

systimer_16:
	; bh = 3
	; timer event via real time clock interrupt
	; interrupt/update frequency: 1 Hz (1 tick per second)
	
	mov	al, 182 ; (*) ; 18.2 * 10
	inc	byte [trtc] ; timer event via real time clock
        jmp     short systimer_0

systimer_7:
	mov	[u.r0], al ; timer event number
	;
	; edi = address of empty timer event area
	mov	al, [u.uno]
	cli 	; disable interrupts 
	stosb	; process number
	mov	al, [tcallback] ; timer callback flag
	stosb 	; 1= callback method, 0= signal response byte method
	mov	al, [trtc] ; timer interrupt type
	stosb	; 1= real time clock, 0= programmable interval timer
	mov	al, bl ; Signal return (Response) value
	stosb   ; response byte
	pop	eax ; (*) ; 10 or 182
	mov	ebx, edx ; virtual address for response/signal byte
	mul	ecx
	; (eax = 10 * count of 18.2 Hz timer ticks)
	; (count down step = 10)
	stosd  ; count limit (reset value)
	stosd  ; current count value

	; 19/12/2016
	cmp	byte [tcallback], 0 ; timer callback method ?
	jna	short systimer_17 ; no
	mov	eax, ebx ; virtual address for callback routine
	jmp	short systimer_18

systimer_17: ; signal response byte method
	; ebx = virtual address
	; [u.pgdir] = page directory's physical address
	; 20/02/2017
	inc	 byte [no_page_swap] ; 1 
			; Do not add this page to swap queue
			; and remove it from swap queue if it is
			; on the queue.
	call	get_physical_addr
	jc	short systimer_8 ; 07/06/2016
	; eax = physical address of the virtual address in user's space
systimer_18:
	stosd	; response addr (physical) or callback addr (virtual)
	inc	byte [timer_events] ; 07/06/201
	; 02/01/2017
	movzx	eax, byte [u.uno]
	inc	byte [eax+p.timer-1]
	;
	sti 	; enable interrupts
	jmp	sysret

systimer_8:
	; 10/06/2016
	; 07/06/2016
	sub	al, al ; 0
	mov	[edi-12], al ; clear process number (free timer event)
	;mov	dword [edi], eax ; 0
	sti
	mov	[u.r0], al ; 0
	jmp	error

systimer_9:
	; 10/06/2016
	; 07/06/2016
	sub	al, al
	mov	byte [u.r0], al ; 0
	cmp     byte [timer_events], al ;  0
	jna	short systimer_12

	; Note: ecx and edx are undefined here
	;	(for stop timer function)

	mov	esi, timer_set  ; beginning address of timer events
				; setting space
	mov	al, [u.uno]

	mov	bh, 16

	or	bl, bl
	jnz	short systimer_15

	; clear timer event areas belong to current process
	; (for stopping all timer events belong to current process) 
	cli 	; disable interrupts
systimer_10:
	; 10/06/2016
	; 07/06/2016 	
	mov	ah, [esi]
	or	ah, ah ; 0 ?
	jz	short systimer_11
	cmp	ah, al ; is the process number (owner) same ?
        jne     short systimer_11 ; no

	;mov	byte [esi], 0
	mov	word [esi], 0 ; clear
	;mov	dword [esi+12], 0 ; clear

	dec	byte [timer_events]
	jz	short systimer_12

systimer_11:
	dec	bh
	jz	short systimer_12
	add	esi, 16
	jmp	short systimer_10

systimer_12:
	movzx	esi, byte [u.uno]
	or	bl, bl ; all timer events or one timer event ?
	jz	short systimer_13
	mov	bl, [esi+p.timer-1]
	and	bl, bl	; previous number of timer events for the process
	jz	short systimer_14
	dec	bl  ; previous number of timer events for the process - 1
systimer_13:
	mov	[esi+p.timer-1], bl ; 0 ; no timer events for process
systimer_14:
	sti	; enable interrupts
	jmp	sysret

systimer_15:
	cmp	bl, bh ; 16
        ;ja	systimer_4      ; max. 16 timer events !
	; 23/07/2022
	jna	short systimer_21
systimer_20:
	jmp	systimer_4
systimer_21:	; 23/07/2022
	mov	dl, bl
	dec	dl  ; 16 -> 15 ... 1 -> 0
	shl	dl, 4 ; * 16
	movzx	edi, dl
	add	edi, esi ; timer_set 
	
	cmp	al, [edi] ; process number
        ;jne	systimer_4
	; 23/07/2022
	jne	short systimer_20 ; jmp systimer_4
	
	; same process ID
	cli	; disable interrupts
 	; 10/06/2016 ; 02/01/2017
	;mov	byte [edi], 0 
	mov	word [edi], 0 ; clear
	;mov	dword [edi+12], 0 ; clear
	dec	byte [timer_events]
	jmp	short systimer_12

sysvideo: ; VIDEO DATA TRANSFER FUNCTIONS
	; 11/08/2022
	; 08/08/2022
	; 23/07/2022 (TRDOS 386 v2.0.5)
	; 06/03/2021
	; 02/03/2021
	; 28/02/2021
	; 27/02/2021
	; 26/02/2021
	; 25/02/2021
	; 21/02/2021, 22/02/2021, 23/02/2021
	; 15/02/2021, 16/02/2021, 18/02/2021
	; 10/02/2021, 11/02/2021, 12/02/2021
	; 07/02/2021, 08/02/2021
	; 01/02/2021, 02/02/2021, 05/02/2021
	; 29/01/2021, 30/01/2021, 31/01/2021
	; 23/01/2021, 24/01/2021, 28/01/2021
	; 18/01/2021, 19/01/2021, 22/01/2021
	; 04/01/2021, 10/01/2021, 11/01/2021
	; 01/01/2021, 02/01/2021, 03/01/2021
	; 28/12/2020, 29/12/2020, 30/12/2020
	; 25/12/2020, 26/12/2020
	; 21/12/2020, 23/12/2020
	; 12/12/2020, 14/12/2020
	; 10/12/2020, 11/12/2020
	; 03/12/2020, 04/12/2020
	; 22/11/2020, 23/11/2020
	; 21/11/2020 (TRDOS 386 v2.0.3)
	; 12/05/2017
	; 11/07/2016
	; 13/06/2016
	; 16/05/2016 - TRDOS 386 (TRDOS v2.0)
	;
	; VIDEO DATA TRANSFER FUNCTIONS:
	;
	; Inputs:
	;		; 07/02/2021
	;	BH = 0 = VIDEO BIOS Mode 3, tty/text mode data transfers
	;	     BL = 
	;		Bits 0&1, Transfer direction
	;	     	 	0 - System to system
	;			1 - User to system
	;			2 - System to user
	;			3 - Exhange (Swap) - 28/01/2021
	;		Bits 2, Transfer Type
	;			0 - Display page (complete) transfer
	;	     		1 - Display page window (col,row) transfer
	;		; 28/01/2021
	;		Bits 3..7 - Reserved, undefined (must be 0)
	;	        ; 28/01/2021	
	;	     /// BL = 0 -> System to system (display page) transfer
	;		 CL = Source page (0FFh = current video page)
	;		 DL = Destination page (0FFh = current video page)
	;		 (Note: Nothing to do if src & dest are same page)
	;	     /// BL = 1&2 -> user to system & system to user transfer
	;		 ECX = User's buffer address
	;		 DL = Video page (0FFh = current video page)
	;	     /// BL = 3 -> exchange (swap) display page ; 28/01/2021
	;		 ECX = User's buffer address
	;		 DL = Video page (0FFh = current video page)
	;		 EDI = Swap address in user's memory (must be > 0)
	;	     /// BL = 5&6&7 -> user to system, system to user transfer 
	;		(system window is in current/active display page)
	;		 ESI = User's buffer address
	;		 ECX Low 16 bits = Top left column (X1 position)
	;		 ECX High 16 bits = Top row (Y1 position)
	;		 EDX Low 16 bits = Bottom right column (X2 position)
	;		 EDX High 16 bits = Bottom row (Y2 position)
        ;                If BL = 5 or BL bit 0 & bit 1 are 1 ; 28/01/2021
	;		 EDI = Swap address (in user's memory space)
	;		 (If swap address > 0, previous content of the window
	;		 will be saved into swap area in user's memory space)
	;	     /// BL = 4 -> system to system transfer
	;		 ESI = System's source buffer (video page) address
	;		 ECX Low 16 bits = Top left column (X1 position)
	;		 ECX High 16 bits = Top row (Y1 position)
	;		 EDX Low 16 bits = Bottom right column (X2 position)
	;		 EDX High 16 bits = Bottom row (Y2 position)
	;		 EDI = System's destination buffer (video page) addr
	;
	;		; 06/02/2021
	;		; 05/02/2021
	;		; 01/02/2021, 02/02/2021
	;		; 30/01/2021, 31/02/2021
	;		; 29/01/2021 (major modification)
	;		; 23/11/2020 (major modification)
	;		; 22/11/2020 (bugfixes and extensions)
	;	BH = 1 = VGA Graphics (0A0000h) data transfers
	;		BL bit 7
	;		   resolution (screen width) option
	;		   0 = 320 pixels
	;		   1 = 640 pixels
	;		.. followings are same with SVGA transfer function
	;		BL bit 6
	;		   direction option
	;		   0 = user to system (video memory)
	;		   1 = system to user
	;		BL bit 5
	;		   masked/direct (non-masked) operations
	;		   1 = masked, 0 = non-masked (direct)
	;		BL bit 4
	;		   page/window option
	;		   1 = window, 0 = display page (screen)
	;		BL bit 0 to 3 (pixel operation types)
	;		   0 = Copy pixels (colors) ((mask color))
	;		   1 = Change (New, Fill) color
	;		   2 = Add color
	;		   3 = Sub color
	;		   4 = OR color
	;		   5 = AND color
	;		   6 = XOR color
	;		   7 = NOT color
	;		   8 = NEG color
	;		   9 = INC color
	;		  10 = DEC color
	;		  11 = Mix (Average) colors
	;		  12 = Replace pixel colors
	;		  13 = Copy pixel block(s)
	;		  14 = Write line(s)
	;		  15 = Write character (font)
	;
	;	   Input Registers for pixel operations:
	;		 Same with LFB data transfer function below
	;
	;		; 25/02/2021
	;		; 05/02/2021, 06/02/2021
	;		; 01/02/2021, 02/02/2021
	;		; 30/01/2021, 31/02/2021
	; 		; 29/01/2021 (major modification)
	;		; 23/11/2020 (major modification)
	;		; 22/11/2020 (bugfixes and extensions)
	;	BH = 2 = Super VGA, LINEAR FRAME BUFFER data transfers
	;		BL bit 7
	;		   unused (invalid), must be 0
	;		BL bit 6
	;		   direction option
	;		   0 = user to system (video memory)
	;		   1 = system to user
	;		BL bit 5
	;		   masked/direct (non-masked) operations
	;		   1 = masked, 0 = non-masked (direct)
	;		BL bit 4
	;		   page/window option
	;		   1 = window, 0 = display page (screen)
	;		BL bit 0 to 3 (pixel operation types)
	;		   0 = Copy pixels (colors) ((mask color))
	;		   1 = Change (New, Fill) color
	;		   2 = Add color
	;		   3 = Sub color
	;		   4 = OR color
	;		   5 = AND color
	;		   6 = XOR color
	;		   7 = NOT color
	;		   8 = NEG color
	;		   9 = INC color
	;		  10 = DEC color
	;		  11 = Mix (Average) colors
	;		  12 = Replace pixel colors
	;		  13 = Copy pixel block(s)
	;		  14 = Write line(s)
	;		  15 = Write character (font)
	;
	;	   Note: If HW of EBX > 0, it is VESA VBE mode number
	;		 otherwise, function will be applied
	;		 to current (VESA VBE) video mode.
	;
	;	   Input Registers for pixel operations:
	;		-- user to system & system to system --
	;		-- (BL = 0 to 0Fh) -- non-masked, screen --
	;		-- (BL = 10h to 1Fh) -- non-masked, window --
	;		-- (BL = 20h to 2Fh) -- masked, screen --
	;		-- (BL = 30h to 3Fh) -- masked, window --
	;		(*) window, (**) masked (***) sys to sys
	;		for BL bit 0 to 3
	;		00h: COPY PIXELS
	;		  If BL bit 4 = 0 ; 21/02/2021
	;		     full screen copy	
   	;		     ECX & EDX will not be used
	;		    (user buffer must fit to display page)
	;		  If BL bit 4 = 1 ; 21/02/2021
	;		    ECX = start position (row, column) (*)
	;			  (HW = row, CX = column)
	;		    EDX = size (rows, colums) (*)
	;			  (HW = rows, DX = columns)
	;		          (0 -> invalid)
	;		          (1 -> horizontal or vertical line)
	;		    ESI = user's buffer address
	;		    EDI = mask color (**) ; 25/02/2021
	;			  (this color will be excluded)
	;		01h: CHANGE PIXEL COLORS
	;		02h: ADD PIXEL COLORS
	;		03h: SUB PIXEL COLORS
	;		04h: OR PIXEL COLORS
	;		05h: AND PIXEL COLORS
	;		06h: XOR PIXEL COLORS
	;		0Bh: MIX PIXEL COLORS
	;		     CL = color (8 bit, 256 colors)
	;		    ECX = color (16 bit and true colors)
	;		    EDX = start position (row, column) (*)
	;			 (HW = row, DX = column)
	;		    ESI = size (rows, colums) (*)
	;			  (HW = rows, SI = columns)
	;		    EDI = mask color (**) ; 25/02/2021
	;			  (this color will be excluded)
	;		07h: NOT PIXEL COLORS
	;		08h: NEG PIXEL COLORS
	;		09h: INC PIXEL COLORS
	;		0Ah: DEC PIXEL COLORS
	;		    ECX = start position (row, column) (*)
	;			  (HW = row, CX = column)
	;		    EDX = size (rows, colums) (*)
	;			  (HW = rows, DX = columns)
	;		          (0 -> invalid)
	;		          (1 -> horizontal or vertical line)
	;		    EDI = mask color (**) ; 25/02/2021
	;			  (this color will be excluded)
	;		0Ch: REPLACE PIXEL COLORS
	;		     CL = current color (8 bit, 256 colors)
	;		    ECX = current color (16 bit and true colors)
	;		     DL = new color (8 bit, 256 colors)
	;		    EDX = new color (16 bit and true colors)
	;		    ESI = start position (row, column) (*)
	;			  (HW = row, SI = column)
	;		    EDI = size (rows, colums) (*)
	;			  (HW = rows, DI = columns)
	;		0Dh: COPY PIXEL BLOCK(S) -full screen-
	;		   -If BL bit 5 is 0-
	;		    ECX = start position (row, column) (*)
	;			  (HW = row, CX = column)
	;		    EDX = size (rows, colums) (*)
	;			  (HW = rows, DX = columns)
	;		          (0 -> invalid)
	;		          (1 -> horizontal or vertical line)
	;		    ESI = destination (row, column) (***)
	;		   -If BL bit 5 is 1-
	;	               CL = color (8 bit, 256 colors)
	;		      ECX = color (16 bit and true colors)
	;	   	      EDX = count of blocks (not bytes)
	;			    (limit: 2048 blocks)
	;		      ESI = user's buffer address 
	;			  contains 64 bits block data
	;			  BLOCK ADDRESS - (row, col), dword
	;			  (first 32 bits)
	;			  BLOCK SIZE - (rows, cols), dword
	;			  (second 32 bits)
	;		; 10/02/2021
	;		0Eh: WRITE LINE(s) -full screen- 
	;		   -If BL bit 5 is 0-	
	; 		    CL = color (8 bit, 256 colors)
	; 		   ECX = color (16 bit and true colors)
	; 		    DX = low 12 bits - size (length)
	;			 high 4 bits - direction or type
	;			     0 - Horizontal line
	;			     1 - Vertical line
	;			   > 1 - undefined, invalid
	; 		   ESI = start position (row, column)
	;			 (HW = row, SI = column)
	;		   -If BL bit 5 is 1-
	;		     CL = color (8 bit, 256 colors)
	;		    ECX = color (16 bit and true colors)
	;		     DX = number of lines (in user buffer)
	;			   (limit: 2048 lines)	
	;		    ESI = user's buffer
	;		          contains 64 bit data for lines
	;			   START POINT: 32 bit (row, col)
	;			   LENGTH: 32 bit
	;			   high 16 bits - 0
	;			   bit 0-11 - length
	;			   bit 12-15 - type (length)
	;		0Fh: WRITE CHARACTER (FONT)
	;		     CL = char's color (8 bit, 256 colors)
	;		    ECX = char's color (16 bit and true colors)
	;		     DL = Character's ASCII code
	;		     DH bit 0 -> font height
	;			  0 -> 8x16 character font
	;			  1 -> 8x8 character font
	;		     DH bit 1 & 2 -> scale
	;			  0 = 1/1 (8 pixels per char row)
	;			  1 = 2/1 (16 pixels per char row)
	;			  2 = 3/1 (24 pixels per char row)
	;			  3 = 4/1 (32 pixels per char row) 
	;		     DH bit 6 -> [ufont] option (1 = use [ufont])
	;		     If DH bit 7 = 1
	;			 USER FONT (from user buffer)
	;			    DL = 0 -> 8x8 (width: 1 byte per row)
	;			    DL = 1 -> 8x16
	;			    DL = 2 -> 16x16 (width: 2 bytes)
	;			    DL = 3 -> 16x32
	;			    DL = 4 -> 24x24 (width: 3 bytes)
	;			    DL = 5 -> 24x48
	;			    DL = 6 -> 32x32 (width: 4 bytes)
	;			    DL = 7 -> 32x64
	;			    DL > 7 -> invalid (unused)
	;			 EDI = user's font buffer address
	;			    (NOTE: byte order is as row0,row1,row2..)
	; 		    ESI = start position (row, column) (*)
	;			     (HW = row, SI = column)
	;
	;		-- system to user --
	;		   BL (bit 0 to 7)
	;	 	40h: COPY PIXELS (full screen, display page)
	;		    EDI = user's buffer address
	;		41h: COPY PIXELS (window)
	;		    ECX = start position (row, column) (*)
	;			  (HW = row, CX = column)
	;		    EDX = size (rows, colums) (*)
	;			  (HW = rows, DX = columns)
	;		          (<=1 -> horizontal or vertical line)
	;		    EDI = user's buffer address
	;
	;		Example: (29/01/2021)
	;		    ecx = 00400064h (start at row 64, column 100)
	;		    edx = 00320048h (size: 50 rows, 72 columns)
	;				    (end at row 114, column 172)
	;		    If video memory starts at 0A0000h
	;		    and if resolution is 320x200 (256 colors) ..
	;		    window start offset: (64*320)+100 = 20580
	;		    window size: 16072 bytes (pixels) 
	;		    window end offset: 20580+16072 = 36652
	;		    window start address: 0A0000h+564h = 0A5064h
	; Outputs:
	;	EAX = transfer/byte count
	;
	;	NOTE: If the source or destination address passes out of
	;	video pages (display memory limits), data will not be transferred
	;	and EAX will return as 0.
	;
	; 08/02/2021
	; 07/02/2021
	; 04/01/2021
	; PIXEL READ/WRITE (in current/active video mode)
	;
	;	BH = 3 = Read/Write pixel(s) -for all graphics modes-
	;	     BL = 
	;		0 = Read pixel
	;		1 = Write pixel
	;	     	2 = swap pixel colors
	;		3 = mix pixel colors
	;	     29/01/2021	
	;		4 = read pixels from user defined positions 
	;		5 = write single color pixels to user defined positions
	;		6 = write multi color pixels to user defined positions 
	;
	;	      > 6 = invalid/unimplemented
	;
	;	     .. for BL = 0 to 5
	;	     CL = color for writing pixel(s) or
 	;	     ECX = color for writing pixel(s) in true color modes
	;
	;	     EDX = Offset from start of video memory (0A0000h)
	;		   or start of linear frame buffer
	;
	;	     07/02/2021	
	;	     .. for BL = 4
	;	     EDI = user's destination buffer address for pixel colors
	;	     29/01/2021
	;	     .. for BL = 4 & 5
	;	     ESI = user's source buffer address for BL = 4 & 5
	;	     (buffer contains dword offset positions for pixels)
	;	     EDX = number of pixels
	;	     .. for BL = 6
	;	     ESI = user's buffer address for BL = 6
	;	     (buffer contains dword offset position and dword color
	;	      value for each pixel)
	;	     EDX = number of pixels
	;
	; Note:
	;	Pixel read/write will be performed in current video mode.
	;	If [CRT_MODE] < 0FFh, 0A0000h will be used 
	;	   as video memory and limit will be 65536
	;	   (new/mix pixel color will be in CL)
	;	if [CRT_MODE] = 0FFh (VESA VBE video mode)
	;	   LFB base address will be used as video memory
	;	   and limit will be video page size
	;	   (new/mix pixel color will be in CL)
	;
	; Outputs:
	;	EAX = pixel color (according to BL and ECX input)
	;	EAX = 0 (pixel color is 0 or there is an error)
	;	     (BL will return as 0FFh if there is an error)
	;	; 29/01/2021
	;	EAX = number of pixels (for BL input = 4&5&6)
	;
	; DIRECT (STANDARD VGA/CGA) DISPLAY MEMORY ACCESS FUNCTIONS:
	;
	;	BH = 4 = CGA direct video memory (0B8000h, 32K) access
	;		Page directory & page tables of the user's
	;		program will be updated to direct access to
	;		0B8000h (32K) video (CGA, color) memory; if
	;		there is not a permission conflict or lock!
	;	        (User's program/process will have permission to
	;		access locked display memory if the owner is
	;		it's parent.)
        ;
	;	    Screen width = 320
	;
	;	BH = 5 = VGA direct video memory (0A0000h, 64K) access
	;		Page directory & page tables of the user's
	;		program will be updated to direct access to
	;		0A0000h (64K) video (VGA) memory; if there is not
	;		a permission conflict or lock!  
	;	        (User's program/process will have permission to
	;		access locked display memory if the owner is
	;		it's parent.)
	;
	;	    ; 23/11/2020
	;	    Screen width options = 320, 640, 800
	;
	; Outputs:
	;	EAX = Display memory address for direct access
	;	      0A0000h for VGA, 0B8000h for CGA
	;	(Display memory size: 32K for CGA, 64K for VGA)
	; 	EAX = 0 if display page access permission has been denied.
	;	      (Locked!)
	;
	; LINEAR FRAME BUFFER ACCESS FUNCTIONS:
	;
	;	BH = 6 = Linear Frame Buffer direct video memory access
	;
	;		Page directory & page tables of the user's
	;		program will be updated to direct access to
	;		the configured LFB (Linear Frame Buffer) address,
	;		if there is not a permission conflict or lock!
	;	        (User's program/process will have permission to
	;		access locked display memory if the owner is
	;		it's parent.)
	;
	;	   ; 10/12/2020
	;	   BL = 0FFh -> Direct LFB access for current video mode
	;	   BL = XXh < 0FFh -> Direct LFB access
	; 			      for VESA video mode 1XXh
	;
	;		Return: EAX = Linear Frame Buffer address
	;			(EAX = 0 -> error)
	;		     If EAX > 0
	;			EDX = Frame Buffer Size in bytes
	;		         BH = Requested Video Mode - 100h
	;		              (VESA VBE video modes)	
	;		         BL = bits per pixel
	;			      8 = 256 colors, 8
	;		             16 = 65536 colors, 5-6(G)-5 
	;		             24 = RGB, 16M colors, 8-8-8
	;		             32 = RGB + alpha bytes, 8-8-8-8
	;		     If BH = 0FFh
	;		        BL = VGA/CGA video mode (also EAX = 0)
	;
	;		** Function will return with EAX = 0 if the mode
	;		is not a valid VESA VBE video mode as 1??h **
	;
	;		ECX = Pixel resolution
	;		      CX = Width (320, 640, 800, 1024, 1366, 1920)
	;		      High 16 bits of ECX = Height
	;
	; 	23/11/2020
	; ***	GET VIDEO MODE & LINEAR FRAME BUFFER INFO
	;	(This function -7- also is used for VGA and CGA modes)
	;
	;	BH = 7 = Get Linear Frame Buffer info
	;
	;	   ; 22/01/2021
	;	   ; 10/12/2020
	;	   BL = any -not used- (22/01/2021)
	;
	;		Return:
	;		EAX = Frame Buffer Address (0 = is not in use)
	;		EDX = Frame Buffer Size in bytes
	;		 BH = Current Video Mode - 100h  ; 22/01/2021
	;		     (VESA VBE video modes)
	;		 BL = bits per pixel
	;			8 = 256 colors, 8
	;		       16 = 65536 colors, 5-6(G)-5
	;		       24 = RGB, 16M colors, 8-8-8
	;		       32 = RGB + alpha bytes, 8-8-8-8
	;		If BH = 0FFh
	;		   BL = VGA/CGA video mode (also EAX = 0)
	;
	;		Note:
	;		Alpha byte will be used as virtual color index.
	;		(32 bit pixel colors.. byte 0,1,2 rgb and 3 alpha)
	;
	;		** Function will return with EAX = 0 if the mode
	;		is not a valid VESA VBE video mode as 1??h **
	;
	;		ECX = Pixel resolution
	;		      CX = Width (320, 640, 800, 1024, 1366, 1920)
	;		      High 16 bits of ECX = Height  
	;
	;	NOTE: Each process will have it's own frame buffer
	;	      address and resolution parameters in 'u' area.
	;	      Then, if the current frame buffer & resolution
	;	      is different, frame buffer r/w functions
	;	      will use scale factor to convert process's
        ;             pixel coordinates to actual screen coordinates.
	;	      resolution -> dimensional scale
	;	      color size -> color scale
	;	     * RGB (TRUE) colors to 256 colors conversion:
        ;             TRUE Colors -> 8,8,8 (R,G,B; byte 0 is R)
	;	      256 colors -> 2,2,2,2 (R,G,B,L; bit 0&1 is R)
	; 		  bit 6&7 -> luminosity base level (0,1,2,3)
	;		  bit 4&5 -> blue level (0,1,2,3)
	;		  bit 2%3 -> green level (0,1,2,3)
	;		  bit 0&1 -> red level (0,1,2,3)
	;	      Example: total red level : luminosity + red level
	;	      Luminosity base level: 0 -> 16
	;		 		     1 -> 32
	;				     2 -> 64
	;				     3 -> 128
	;	      Color level:
	;				    0 -> 0
	;				    1 -> luminosity level
	;				    2 -> luminosity level + 64
	;				    3 -> 255
	;	     Luminosity base level = min (R,G,B)
	;			if it is <16, it will be set to 16
	;	     Color levels: Color values are fixed to (nearest)
	;		   one of all possible set level (step) values
	;		   (according to luminosity base level); then
	;		   color levels are set to R-L, G-L, B-L.
	;	 For example: If luminosity base level is 32
	;		  all possible set values are 0, 32, 96, 255.
	;
	;	    * RGB (TRUE) colors to 16 colors conversion:
	;	    16 colors: R,B,G,L bits (4 bits)
	;	    	   If any one of R,G,B >= 128 L = 1
 	;		   If max. value of (R,G,B) >= 32, it is 1
	;		      else all color bits (R&G&B&L) are 0
	;		   If the second value >= max. value / 2
	;		      it is 1
	;		   If the third value >= max. value / 2
	;		      it is 1
	;	    Example: R = 132, G = 64, B = 78
	;		     L = 1, R = 1
	;		     G < 66 --> G = 0
	;		     B >= 66 --> B = 1
	;
	; 10/12/2020
	; SET VIDEO MODE (& RETURN LFB INFO for VESA VBE VIDEO MODES)
	;	
	;	BH = 8 = Set Video Mode
	;		
	;		BL = Requested Video Mode (method)
	;		If BL = 0FFh
	;		   CX = VESA VBE Video Mode
	;		   ; 11/12/2020
	;		   If EDX > 0 -> LFB INFO (user) buffer addr
	;		If BL < 0FFh, it is VGA/CGA video mode and
	;			CX & EDX will not be used
	;
	;	    NOTE: The last VESA VBE video mode is 11Bh but
	;	    TRDOS 386 will permit to set video mode upto 11Fh.
	;	    Above 11Fh, from 140h to 1FEh, it will be accepted
	;	    as Bochs/Plex86 emulator video mode and it will be
	;	    used only if [vbe3] = 2 and detected
	;	    video bios is BOCHS/PLEX86/QEMU/VIRTUALBOX vbios.
	;
	; Outputs:
	;	EAX = Requested (Proper) video mode number + 1
	;	      ("dec eax" by user will give requested video mode),
	;
	;	If BL input is 0FFh
	;	   EDX = LFBINFO table/structure (in user's buffer addr)
	;	   EDX = 0 -> Invalid LFBINFO (do not use it)
	;
	;	EAX = 0 -> Error (but EDX will not be changed)
	;
	; 03/12/2020
	; VESA VBE3 VIDEO BIOS (32 bit) PROTECTED MODE INTERFACE SETTINGS
	;
	;	BH = 9 = set/get VBE3 Protected Mode Interface parameters
	;
	;		BL = 0 - Disable protected mode interface
	;			([pmi32] = 0)
	;		 	Return: AL = 1
	;		BL = 1 - Enable protected mode Interface
	;			([pmi32] = 1)
	;			Return: AL = 2
	;		BL = 2 - Get protected mode interface status
	;			Return: AL = [pmi32] + 1 (AL = 1 or 2)
	;
	;		If [vbe3] <> 3 --> AL = 0
	;
	;		; 17/01/2021
	;		BL = 3 - Disable/Cancel restore permission to user
	;			Return: AL = 1 (if disabled) or 0
	;		BL = 4 - Enable/Give restore permission to user
	;			Return: AL = 2 (if enabled) or 0
	;		BL = 5 - Get video state save/restore status
	;			 (permission status)
	;		        Return: AL = Status (enabled = 1)
	;				; 22/01/2021
	;				AH = state options ([srvso])
	;		BL = 6 - Return VESA VBE number/status
	;		        Return: AX = status
	;			      if AH =  2, AL > 0 : Emulator
	;				 AH =  3, VESA VBE3 video bios
	;		; 28/02/2021
	;		BL = 7 - Set true color mode as 32bpp (default)
	;			Return: AX = 32 (if VBE3) 
	;		NOTE: Initial/default value is 32bpp for vbe3.
	;			Return: AX = 0 -> error
	;		BL = 8 - Set true color mode as 24bpp (default)
	;			Return: AX = 24
	;			;Return: AX = 0 -> error
	;		BL = 9 - Return default/current true color mode
	;			Return: AX = 32 (32 bpp)
	;			Return: AX = 24 (24 bpp)
	;			Return: AX = 0 -> error (not VESA bios)
	;
	;		BL > 9 : not implemented (28/02/2021)
	;
	;	; 19/01/2021 ([u.uid] check)
	;	Note: Enabling/Disabling are done by root ([u.uid] = 0)
	;	      while [multi_tasking] = 0.
	;
    	; 23/12/2020
	; VIDEO MEMORY MAPPING:
	;	BH = 10 = Map video memory to user's buffer
	;
	;	   BL = 0 : CGA memory (0B8000h) map (32K)
	;	   BL = 1 : VGA memory (0A0000h) map (64K)
	;	   BL = 2 : SVGA memory (LFB) map to user's buffer
	;
	;	ECX = User's buffer addr (low 12 bits will be cleared)
	;	EDX = Buffer size in bytes (if BL = 2)
	;		(will be trimmed if LFB size < EDX)
	; Return:
        ;	EAX = physical address of video memory (buffer)
	;	EBX = mapped (actual) size of video memory (bytes)
	;	ECX = virtual start address of user's video buffer 
	;	EDX is same with EDX input
	;
	;	(Note: Memory page boundaries will be applied 
	;	 to buffer size and buff start addr by rounding down.
	;	 Rounded size & address values must not be zero.) 
	;	-Normally, it is expected to request mapping by using
	;	 correct buffer size of current or desired video mode-
	;
	;	EAX = 0 -> error ! memory can not mapped to user
	;
	; 04/01/2021
	; SET/READ COLOR PALETTE (set/read DAC color registers)
	;	((256 colors (8bpp) VGA/CGA video hardware feature))
	;
	;	BH = 11 = Set/Read DAC color registers
	;
	;	   (BL<4 Original method for std VGA video hardware)
	;	   BL = 0 : Read all DAC color registers (256 colors)
	;	       (6 bit colors, in RGB order)
	;	   BL = 1 : Set all DAC color registers (256 colors)
	;	       (6 bit colors, in RGB order)
	;	   BL = 2 : Read single DAC color register
	;	       (6 bit color, in RGB order)
	;		((EAX will return with color value))
	;		CL = DAC color register (index)
	;	   BL = 3 : Set/Write single DAC color register
	;	       (6 bit color, in RGB order, bit 6&7 are 0)
	;		ECX byte 0 - DAC color register
	;		ECX byte 1 - Red (6 bit)
	;		ECX byte 2 - Green (6 bit)
	;		ECX byte 3 - Blue (6 bit)
	;	   (BL>3 Alternative method for BMP files etc.)	
	;	   BL = 4 : Read all DAC color registers (256 colors)
	;	       (8 bit colors, in BGR order, bit 0&1 is 0)
	;	   BL = 5 : Set all DAC color registers (256 colors)
	;	       (8 bit colors, in BGR order, bit 0&1 is 0)
	;	   BL = 6 : Read single DAC color register
	;	       (8 bit color, in BGR order, bit 0&1 is 0)
	;		((EAX will return with color value))
	;		CL = DAC color register (index)
	;	   BL = 7 : Set/Write single DAC color register
	;	       (8 bit color, bit 0&1 are 0)
	;		ECX byte 0 - DAC color register
	;		ECX byte 1 - Blue (8 bit)
	;		ECX byte 2 - Green (8 bit)
	;		ECX byte 3 - Red (8 bit)
	;
	;	  BL > 7 : invalid (not implemented)
	;
	;   if BL bit 2 is 1, 6 bit colors converted to 8 bit colors
	;	(low two bits of color bytes will be 0)
	;     ((color byte 00111111b will be converted to 11111100b))
	;	and RGB byte order will be 
	;		byte 0 - Blue (low 2 bits are 0)
	;		byte 1 - Green (low 2 bits are 0)
	;		byte 2 - Red (low 2 bits are 0)
	;		byte 3 - pad (or zero byte)
	;	and 256 colors buffer size must be 256*4 = 1024 bytes
	;   if BL bit 2 is 0, 6 bit colors will be used directly
	;	  (high two bits of 8 bit color bytes will be 0)
	;     ((dac color 111111b will be converted to 00111111b))
	;		byte 0 - Red (high 2 bits are 0)
	;		byte 1 - Green (high 2 bits are 0)
	;		byte 2 - Blue (high 2 bits are 0)
	;	and 256 colors buffer size must be 256*3 = 768 bytes
	;
	;	ECX = User's buffer addr (256*3 = 768 bytes) or
	;	      Color
	;
	; Return:
        ;	EAX = buffer size (for BL input = 0,1,4,5)
	;	      or color value (for BL input = 2,3,6,7)
	;
	; 10/01/2021
	; SET/READ FONT DATA
	;
	;	BH = 12 = Set/Read Character Font Data
	;
	;	   BL = 0 : Disable system font overwrite
	;	   BL = 1 : Enable system font overwrite
	;	   BL = 2 : Read system font 8x8
	;	   BL = 3 : Read system font 8x14
	;	   BL = 4 : Read system font 8x16
	;	   BL = 5 : Read user defined font 8x8
	;	   BL = 6 : Read user defined font 8x16
	;	   BL = 7 : Write system font 8x8
	;	   BL = 8 : Write system font 8x14
	;	   BL = 9 : Write system font 8x16
	;	   BL = 10 : Write user defined font 8x8
	;	   BL = 11 : Write user defined font 8x16
	;
	;	  BL > 11 : invalid (not implemented)
	;
	;	For BL = 1 to 11
	;	   ECX = number of characters (<= 256)
	;	   EDX = first character (ascii code in DL)
	;	   ESI = user's buffer address
	;
	; Return:
        ;	EAX = number of characters (ecx input)
	;	EAX = 0 -> error
	;	(EAX = 256 for BL = 0 and 1 if successful)
	;
	; Note: system font overwrite permission will be
	;       given if [multi_tasking] = 0
	;	and [u.uid] = 0 (BL = 1) ; 19/01/2021
	;       and if [ufont] bit 7 is 1 (BL = 7,8,9)
	;
	; 18/01/2021
	; SAVE/RESTORE STANDARD VGA VIDEO STATE
	;
	;	BH = 13 = Save/Restore std VGA video state
	;
	;	   BL = 0 : Save VGA state (without DAC regs)
	;		Return: EAX = VideoStateID (>0)
	;			EAX = 0 (failed!)
	;	       (size: 110 bytes for TRDOS 386 v2.0.3)
	;	   BL = 1 : Restore VGA state (without DAC regs)
	;		ECX = VideoStateID (to be verified)
	;	        Return: EAX = Restore size (>0)
	;	   BL = 2 : Save VGA state (complete)
	;		Return: EAX = VideoStateID (>0)
	;			EAX = 0 (failed!)
	;	       (size: 882 bytes for TRDOS 386 v2.0.3)
	;	   BL = 3 : Restore VGA state (complete)
	;		ECX = VideoStateID (to be verified)
	;	        Return: EAX = Restore size (>0)
	;
	;	* Above options are for saving
	;	*       video state to system memory
	;	*	(location: VBE3VIDEOSTATE, 2048 bytes)
	;
	;	   BL = 4 : Save VGA state (without DAC regs)
	;		ECX = buffer address
	;		Return: EAX = transfer count
	;	       (size: 110 bytes for TRDOS 386 v2.0.3)
	;		ECX = buffer address
	;	   BL = 5 : Restore VGA state (without DAC regs)
	;		ECX = buffer address
	;	        Return: EAX = transfer count
	;	   BL = 6 : Save VGA state (complete)
	;		ECX = buffer address
	;		Return: EAX = transfer count
	;	       (size: 882 bytes for TRDOS 386 v2.0.3)
	;	   BL = 7 : Restore VGA state (complete)
	;		ECX = buffer address
	;	        Return: EAX = transfer count
	;
	;	* Above options are for saving
	;	*       video state to user's buffer
	;	*	(buffer size: 110 bytes or 882 bytes)
	;
	;	  BL > 7 : invalid (not implemented)
	;
	; 18/01/2021
	; SAVE/RESTORE SUPER VGA (VESA VBE 2/3) VIDEO STATE
	;
	;	BH = 14 = Save/Restore SVGA video state
	;
	;	   BL = options
	;		bit 0 - Save (0) or Restore (1)
	;		bit 1 - controller hardware state
	;		bit 2 - BIOS data state
	;		bit 3 - DAC state
	;		bit 4 - (extended) Register state
	;		bit 5 - system (0) or user (1) memory
	;		bit 6 - verify without transfer
	;		bit 7 - not used (must be 0)
	;
	;	     if bit 0 = 0 and bit 5 = 0
	;		Return:	EAX = VideoStateID (>0)
	;	     if bit 0 = 1
	;		ECX = VideoStateID (bit 5 = 0)
	;		Return: EAX = restore (transfer) size
	;	     if bit 5 = 1
	;		ECX = Buffer address
	;		Return: EAX = transfer count (size)
	;
	;	     ECX = Buffer address or VideoStateID
	;
	;	  BL > 127 : invalid (not implemented)
	;
	;  Note: Required buffer size may be > 2048 bytes
	;	 (function fails when buff size > 2048 bytes)
	;	 proper option must be used
	;
	; 18/01/2021
	; READ VESA EDID (EXTENDED DISPLAY IDENTIFICATION DATA)
	;
	;	BH = 15 = Read VESA EDID for connected monitor
	;		  (copy EDID to user)
	;
	;	   BL = any
	;
	;	Input:
	;	    ECX = user's (EDID) buffer address 
	;		 (buffer size: 128 bytes)
	;	Output:
	;	    EAX = 128 (EDID size)
	;	    or EAX = 0 -> Error!
	;	       (EDID not ready or buffer addr error)
	;

	; 16/05/2016
	xor	eax, eax
	mov	[u.r0], eax
	and	bh, bh
	;jnz	sysvideo_13 ; 11/07/2016
	; 23/07/2022
	jz	short sysvideo_40
	jmp	sysvideo_13

sysvideo_40:	; 23/07/2022
	;; 21/11/2020 (TRDOS 386 v2.0.3)
	;; tty/text mode transfers are only for video mode 3

	; 22/11/2020
	;cmp	byte [CRT_MODE], 3 ; 80x25 text, 16 colors
	;jne	sysret ; invalid (nothing to do), [u.r0] = 0
	
	; 23/11/2020
	; bit 7,6,5,4 of BL are reserved and it must be 0
	;	 	 for current 'sysvideo' version
	;test	bl, 0F0h
	;;jnz	sysret ; invalid (undefined) !
	;; 28/01/2021
	;jnz	short sysvideo_1_2 ; invalid (undefined) !
	; 28/01/2021
	cmp	bl, 7
	ja	short sysvideo_1_2 ; invalid (undefined) !

	; Video mode 0, 80*25 text mode, CGA 16 colors  
	; [CRT_MODE] = 3
	;mov	bh, bl
	;shr	bh, 2 ; 4..7 -> 1, 8..11 -> 2, 12..15 -> 3
	;; 21/11/2020
	;;and	bh, bh
        ;jnz	sysvideo_4  ; Display page window transfer etc.

	; 28/01/2021
	test	bl, 4 ; bit 2
	;jnz	sysvideo_4 ; Display page window transfer
	; 23/07/2022
	jz	short sysvideo_41
	jmp	sysvideo_4
sysvideo_41:	; 23/07/2022
	; Display page (complete) transfer
	cmp	dl, 7
	;jnz	sysret ; invalid (nothing to do), [u.r0] = 0
	jna	short sysvideo_0 ; 28/01/2021
	inc	dl ; 0FFh -> 0 ("use current video page")
	jnz	short sysvideo_1_2  ; invalid
	; dl = 0 -> use current current page
	mov	dl, [ACTIVE_PAGE]
sysvideo_0:
	; 28/01/2021
	cmp	bl, 3
	jb	short sysvideo_0_0
	or	edi, edi
	jz	short sysvideo_1_2  ; invalid
	mov	esi, edi ; save swap/exchange buffer addr
	; ecx = user buffer for new video page content
	; esi = user (swap) buffer for saving current video page
sysvideo_0_0:
	mov	edi, 0B8000h
	; dl = display page number, destination
	mov	ax, 4096 ; 21/11/2020
	and	dl, dl
	jz	short sysvideo_1
	; 07/02/2021
	mov	dh, dl
sysvideo_0_1:
	; page length = 4096 bytes (but page content is 80*25*2 bytes)
	add	edi, eax ; 21//11/2020 ([CRT_LEN] = 1000h for mode 3)
	dec	dh
	jnz	short sysvideo_0_1
sysvideo_1:
	and	bl, 3
	jnz	short sysvideo_2 ; user to system display page transfer
	; system to system video page (content) transfer
	; cl = display page number, source
	cmp	cl, 7
	;ja	sysret ; invalid (nothing to do), [u.r0] = 0
	jna	short sysvideo_1_0
	inc	cl ; 0FFh -> 0 ("use current video page")
	jnz	short sysvideo_1_2  ; invalid
	mov	cl, [ACTIVE_PAGE]
sysvideo_1_0:
	; 28/01/2021
	cmp	cl, dl
	je	short sysvideo_1_2  ; same video page !

	; system to system video/display page transfer (mode 0)
	; 21/11/2020
	;mov	esi, 0B8000h
	;movzx	eax, cl
	;mov	edx, 4096 ; [CRT_LEN] = 1000h for video mode 3
	;mov	ecx, edx
	;mul	edx
	;add	esi, eax
	; 28/01/2021
	;movzx	esi, cl
	;shl	si, 12 ; * 4096
	;add	esi, 0B8000h
	
	; 28/01/2021
	mov	[u.r0], eax ; 4096
	mov	esi, 0B8000h
	or	cl, cl
	jz	short sysvideo_1_1
	; 07/02/2021
	;mov	al, cl ; display/video page
	;xor	ah, ah
	;shl	ax, 12 ; * 4096
	; 23/07/2022
 	xor	al, al
	;xor	eax, eax
	mov	ah, cl ; CL * 256
	shl	eax, 4 ; * 16 = CL * 4096
	add	esi, eax
sysvideo_1_1:
	; 21/11/2020
	;;mov	ecx, 4096
	;mov	ecx, eax ; 4096
	;;mov	[u.r0], ecx ; 4096 bytes
	; 28/01/2021
	;mov	[u.r0], cx
	xor	ecx, ecx
	mov	ch, 4 ; mov ecx, 1024
	;shr	cx, 2 ; / 4
	rep	movsd
sysvideo_1_2:
	jmp	sysret
sysvideo_2:
	cmp	bl, 2
        ;ja	sysret; invalid (user to user), [u.r0] = 0
	; 28/01/2021
	jb	short sysvideo_3 ; user to system
	je	short sysvideo_2_0 ; system to user
	; bl = 3
	mov	edx, ecx ; save user's buffer addr
	mov	ecx, esi ; save swap address
sysvideo_2_0:
	; bl = 2 (or bl = 3, stage 1)
	; system to user video/display page transfer (mode 0)
	mov	esi, edi
	mov	edi, ecx ; user buffer ; 28/01/2021
	; 21/11/2020
	mov	ecx, eax ; 4096
	call	transfer_to_user_buffer ; fast transfer
	;jc	sysret ; [u.r0] = 0
	jc	short sysvideo_1_2 ; 28/01/2021
	; 28/01/2021
	cmp	bl, 3
	je	short sysvideo_2_2
sysvideo_2_1:
	; 21/11/2020
	mov	[u.r0], ecx
	;;mov	[u.r0], cx
	;jmp	sysret
	jmp	short sysvideo_1_2

sysvideo_2_2:
	; bl = 3 (exchange/swap) complete display page
	; esi = video page start address
	; edx = user's buffer address
	
	;mov	ecx, 4096
	mov	edi, esi ; video page start address
	mov	esi, edx ; user's (new page) buffer address
	jmp	short sysvideo_2_3
sysvideo_3: 
	; bl = 1 (or bl = 3, stage 2)
	; user to system video/display page transfer (mode 0)
	mov	esi, ecx ; user buffer
	; edi = video page address
	; 21/11/2020
	mov	ecx, eax ; 4096
sysvideo_2_3:
	call	transfer_from_user_buffer ; fast transfer
	;jc	sysret ; [u.r0] = 0
	jc	short sysvideo_1_2 ; 28/01/2021
	jmp	short sysvideo_2_1

	; 21/11/2020
	;mov	[u.r0], ecx
	;;mov	[u.r0], cx
	;;jmp	sysret
	;jmp	short sysvideo_1_2 ; 28/01/2021

sysvideo_4:
	; 23/07/2022
	; 23/11/2020 (TRDOS 386 v2.0.3)

	; Display page window transfer etc.
	and	bl, 3
        ;jnz	sysvideo_9 ; user to system or system to user
	; 23/07/2022
	jz	short sysvideo_4_0
	jmp	sysvideo_9
sysvideo_4_0:
	; 21/11/2020
	; system to system video/display page window transfer (mode 0)
	cmp	esi, 0B8000h	; source buffer address (system)
	;jb	sysret
	; 23/07/2022
	jb	short sysvideo_4_3  ; jmp sysret
	cmp	esi, 0B8000h+(4096*8)
	; 23/07/2022
	;jnb	sysret
	jnb	short sysvideo_4_3  ; jmp sysret
	cmp	edi, 0B8000h	; destination buffer address (system)
	;jb	sysret
	; 23/07/2022
	jb	short sysvideo_4_3  ; jmp sysret
        cmp     edi, 0B8000h+(4096*8)
	; 23/07/2022
	;jnb	sysret
	jnb	short sysvideo_4_3  ; jmp sysret
	;
	push	ecx ; X1 and Y1 position - top left column, row
	movzx	eax, cx ; top left column
	; 21/11/2020
	shr	ecx, 16 ; top row
	jz	short sysvideo_4_1 ; bypass following code
	push	edx ; X2 and Y2 position - bottom right column, row
	push	eax
	mov	ax, 80*2 ; 80 colums, 160 bytes per row
	mul	ecx
		; eax = offset for start row number 
	add	esi, eax
	add	edi, eax
	pop	eax
	pop	edx
sysvideo_4_1:
	;shl	ax, 1 ; * 2 ; convert start column number to offset
	; 23/07/2022
	shl	eax, 1
	jz	short sysvideo_4_2
	add	esi, eax
	add	edi, eax
		; esi = source page window start offset
		; edi = destination page window start offset
sysvideo_4_2:
	pop	ecx
	;mov	eax, 0B8000h+(80*25*2*8)
	; 21/11/2020
	mov	eax, 0B8000h+(4096*8)
	cmp	esi, eax
	;jnb	sysret ; out of video page
	; 23/07/2022
	jnb	short sysvideo_4_3  ; jmp sysret
	cmp	esi, eax
	;jnb	sysret  ;out of video page
	; 23/07/2022
	jb	short sysvideo_4_4
sysvideo_4_3:
	jmp	sysret
sysvideo_4_4:
	push	esi ; ****
	push	edi ; ***
	push	edx ; **
	push	ecx ; *
	shr	ecx, 16 ; top row
	shr	edx, 16 ; bottom row
	; 21/11/2020
	;cmp	ecx, 24 ; max. 25 rows
	cmp	cx, 24
	ja	short sysvideo_6 ; invalid, [u.r0] = 0
	;cmp	edx, 24 ; max. 25 rows
	cmp	dx, 24
	ja	short sysvideo_6 ; invalid, [u.r0] = 0
	sub	dl, cl ; end >= start
	jc	short sysvideo_6 ; invalid, [u.r0] = 0
	; 21/11/2020
	mov	ebx, edx ; row count - 1
	jz	short sysvideo_4_5
	push	eax ; *****
	mov	eax, 80*2 ; bytes per row
	mul	ebx ; 21/11/2020
		; eax = window end offset 
		; (for the last row, before adding column bytes)
	add	esi, eax
	add	edi, eax
	pop	eax ; ***** ; mode 3 video memory end (0C000h)
	cmp	esi, eax
	;ja	short sysvideo_6 ; invalid, [u.r0] = 0
	jnb	short sysvideo_6 ; 21/11/2020
	cmp	edi, eax
	;ja	short sysvideo_6 ; invalid, [u.r0] = 0
	jnb	short sysvideo_6 ; 21/11/2020
sysvideo_4_5:
	pop	ecx ; *
	pop	edx ; **
	and	ecx, 0FFFFh
	and	edx, 0FFFFh
	; 21/11/2020
	;cmp	ecx, 79 ; max. 80 columns
	cmp	cx, 79
	ja	short sysvideo_7 ; invalid, [u.r0] = 0
	;cmp	edx, 79 ; max. 80 columns
	cmp	dx, 79 
	ja	short sysvideo_7 ; invalid, [u.r0] = 0
	sub	dl, cl
	jna	short sysvideo_7 ; invalid, [u.r0] = 0
	; 21/11/2020
	jz	short sysvideo_4_6 
	; edx = column count (width) - 1
	shl	dl, 1 ; * 2 ; byte offset (in end row)
	add	esi, edx
	add	edi, edx
		; esi = source page window end offset
		; edi = destination page window end offset
	cmp	esi, eax ; video memory end
	;ja	short sysvideo_7
	jnb	short sysvideo_7 ; 21/11/2020
	cmp	edi, eax ; video memory end
	;ja	short sysvideo_7
	jnb	short sysvideo_7 ; 21/11/2020
sysvideo_4_6:
	pop	edi ; ***
	pop	esi ; ****
	inc	bl ; row count - 1 -> row count
	inc	dl ; column count
	mov	bh, dl
	shl	dl, 1 ; convert column count to byte offset
	; 21/11/2020
	; esi = source page window start offset
	; edi = destination page window start offset
	mov	eax, 80*2 ; bytes per row
	; Note: 160 bytes per row (even if move count < 160)
sysvideo_5:
	mov	cl, bh ; move/transfer -word- count per row
	add	[u.r0], edx ; transfer count in bytes
	rep	movsw
	add	esi, eax ; + 160 bytes to next row
	add	edi, eax ; + 160 bytes to next row
	dec	bl ; remain count of rows
	jnz	short sysvideo_5
	jmp	sysret

sysvideo_6:
	pop	ecx ; *
	pop	edx ; **
sysvideo_7:
	pop	edi ; ***
	pop	esi ; ****
sysvideo_8:
	jmp	sysret

sysvideo_9:
	; user to system or system to user window transfer
	; 28/01/2021 (bl = 3 -> swap/exchange)
	;cmp	bl, 2
	;;ja	sysret	; user to user transfer is invalid
	;		; [u.r0] = 0
	;ja	short sysvideo_8 ; 26/12/2020

	; 28/01/2021
	cmp	bl, 2
	jna	short sysvideo_9_8 

	; swap/ exchange video memory and user mem windows
	; edi = swap address in user's memory space
	and	edi, edi
	jz	short sysvideo_8 ; invalid ; 28/01/2021

sysvideo_9_8:
	push	esi ; ****
	push	edi ; ***
	push	edx ; **
	push	ecx ; *

	shr	ecx, 16 ; top row
	shr	edx, 16 ; bottom row

	; 21/11/2020
	;cmp	ecx, 24 ; max. 25 rows
	cmp	cx, 24
	ja	short sysvideo_6 ; invalid, [u.r0] = 0
	;cmp	edx, 24 ; max. 25 rows
	cmp	dx, 24
	ja	short sysvideo_6 ; invalid, [u.r0] = 0
	sub	dl, cl
	jc	short sysvideo_6 ; invalid, [u.r0] = 0

	;mov	ch, cl ; top row
	;mov	cl, [ACTIVE_PAGE]

	;mov	edi, 80*25*2 ; 4000
	; 21/11/2020
	;mov	edi, 4096 ; [CRT_LEN = 4096 for video mode 3
	;shl	edi, cl  ; ! wrong for page 2 to page 7 !
	;;add	edi, 0B8000h - 80*25*2
	;add	edi, 0B8000h - 1000h ; - 4096

	; 21/11/2020
	;xor	eax, eax
	;mov	edi, 0B8000h
	;and	cl, cl ; is video page = 0 ?
	;jz	short sysvideo_9_1 ; yes
	; eax = 0

;sysvideo_9_0:
	;add	ax, 4096
	;dec	cl
	;jnz	short sysvideo_9_0
	;add	edi, eax
	;	; edi = video page start address

	; 21/11/2020
	mov	edi, 0B8000h
	cmp	byte [ACTIVE_PAGE], 0
	jna	short sysvideo_9_1
stsvideo_9_0:
	mov	al, 16 ; 4096/256
	mul	byte [ACTIVE_PAGE]
	xchg	ah, al ; * 256
	add	edi, eax
		; edi = video page start address
sysvideo_9_1:
	; bl = transfer direction 
	;     (1 = from user, 2 = to user)
	;     (3 = swap) ; 28/01/2021
	mov	bh, dl ; row count - 1
	;mov	dl, ch ; top row
	; 21/11/2020
	or	cl, cl ; top row number
	jz	short sysvideo_9_2

	;mov	eax, 80*2
	mov	ax, 80*2 ; 160, bytes per row
	mul	ecx ; 22/11/2020
	add	edi, eax
		; edi = window start address for top row
sysvideo_9_2:
	pop	ecx ; *
	pop	edx ; **
	and	ecx, 0FFFFh
	and	edx, 0FFFFh
	; 21/11/2020
	;cmp	ecx, 79 ; max. 80 columns
	cmp	cx, 79
	ja	short sysvideo_7 ; invalid, [u.r0] = 0
	;cmp	edx, 79 ; max. 80 columns
	cmp	dx, 79
	ja	short sysvideo_7 ; invalid, [u.r0] = 0

	sub	dl, cl
	jc	short sysvideo_7 ; invalid, [u.r0] = 0

	or	cl, cl ; left column 
	jz	short sysvideo_9_3 ; 0

	; 21/11/2020
	shl	cl, 1  ; column * 2
	add	edi, ecx
		; edi = window start addr for top left column
sysvideo_9_3:
	mov	cl, dl
	inc	cl ; column count
	shl	cl, 1 ; column count * 2
		; ecx = transfer count per row

	pop	eax ; *** (swap address)
	pop	esi ; ****

	inc	bh  ; row count
	
	;mov	edx, 80*2
	mov	dl, 80*2  ; bytes per row
	;
	;cmp	bl, 1 ; transfer direction
	;ja	short sysvideo_11 ; system to user transfer
	; 28/01/2021
	test	bl, 1
	jz	short sysvideo_11 ; system to user transfer

	; user to system video/display page window transfer (mode 0)	
	and	eax, eax ; swap address
	jz	short sysvideo_10 ; no window swap
sysvideo_9_7: ; 28/01/2021
	; save previous window content in user's buffer (swap address)
	push	esi ; user buffer
	push	edi ; beginning address of the window
	; 21/11/2020
	push	ebx ; save bh
	mov	esi, edi
	mov	edi, eax
sysvideo_9_4:
	call	transfer_to_user_buffer ; fast transfer
	jc	short sysvideo_9_5
	; ecx = actual transfer count (must be same with input)
	add	esi, edx ; next row address of (video page) window 
	add	edi, ecx ; next row address of user's window
		; Note: ecx may be less than row length of video page
		; user's window uses offset according to window width
	dec	bh
	jnz	short sysvideo_9_4 ; repeat for next row
sysvideo_9_5:
	pop	ebx ; restore bh
	pop	edi
	pop	esi
	;jnc	short sysvideo_10
	jc	short sysvideo_9_6 ; 28/01/2021
;sysvideo_9_6:
;	jmp	sysret ; [u.r0] = 0

sysvideo_10:
	; user to system video/display page window transfer (mode 0)
	; esi =	user buffer
	call	transfer_from_user_buffer ; fast transfer
	;jc	sysret
	jc	short sysvideo_9_6 ; 28/01/2021
	; ecx = actual transfer count (must be same with input)
	add	[u.r0], ecx ; actual transfer count
	add	edi, edx ; next row address of (video page) window 
	add	esi, ecx ; next row address of user's window
		; Note: ecx may be less than row length of video page
		; user's window uses offset according to window width
	dec	bh
	jnz	short sysvideo_10 ; repeat for next row
	;jmp	sysret
sysvideo_9_6:
	jmp	sysret
	
sysvideo_11:
	; system to user video/display page window transfer (mode 0)
	xchg	edi, esi
sysvideo_12:
	; esi = beginning addr of the (screen, video page) window
	; edi =	user's buffer	
	call	transfer_to_user_buffer ; fast transfer
	;jc	sysret
	; 23/07/2022
	jc	short sysvideo_9_6 ; jmp sysret	

	; ecx = actual transfer count (must be same with input)
	add	[u.r0], ecx
	add	esi, edx ; next row (edx = 160)
	add	edi, ecx ; next row of the user's window
			 ; (ecx <= 160)
	dec	bh
	jnz	short sysvideo_12
sysvideo_12_0:
	jmp	sysret

sysvideo_13:
	; 28/12/2020
	cmp	bh, 1
        ja      short sysvideo_15 ; 23/11/2020

	; 25/02/2021
	; 12/02/2021
	; 29/01/2021, 31/01/2021
	; 23/11/2020 (TRDOS 386 v2.0.3)
	; (major modification, from mode 13h to all VGA modes,
	;  except super VGA modes and liner frame buffer method)

	; BH = 1 = VGA Graphics mode (0A0000h) data transfers

	; 29/01/2021
	mov	ax, 320	; 320 pixels
	test	bl, 80h	; bit 7 (screen width, 640 pixels)
	jz	short sysvideo_13_0
	;shl	ax, 1	; 640 pixels
	; 23/07/2022
	shl	eax, 1
	;
	and	bl, 7Fh
	jz	short sysvideo_14
sysvideo_13_0:
	; 29/01/2021
	cmp	bl, 41h
	ja	short sysvideo_12_0 ; invalid (unknown) sub function
sysvideo_14:
	mov	[v_width], ax	; save screen width
	mov	dword [v_mem], 0A0000h  ; save video memory address
	mov	dword [v_siz], 65536	; save video memory size
	mov	dword [v_end], 0B0000h	; save end of video page
	mov	bh, 8
	mov	[v_bpp], bh ; 8	; bits per pixel (256 colors)
	mov	[v_ops], bl	; VGA data transfer options
	;mov	[maskbuff], edi ; 25/02/2021 
	mov	[maskcolor], edi ; 25/02/2021
			; save mask color or bitmask buffer address
	jmp	sysvideo_15_7

sysvideo_15:
	; 23/07/2022
	; 28/12/2020
	cmp	bh, 2
        ;ja	sysvideo_16
	; 23/07/2022
	jna	short sysvideo_15_17
	jmp	sysvideo_16
sysvideo_15_17:	; 23/07/2022
	; 25/02/2021
	; 12/02/2021
	; 30/01/2021 - 31/01/2021
	; 01/01/2021 - 29/01/2021
	; 26/12/2020 - 27/12/2020
	; 25/12/2020 (TRDOS 386 v2.0.3)
	;
	; BH = 2 = SVGA (VESA VBE) Graphics mode (LFB) data transfers

	; 25/12/2020
	; resolution table entry will be saved into EBP register

	cmp	byte [vbe3], 2 ; VESA VBE 3 video bios 
			  ; or BOCHS/QEMU/VIRTUALBOX emu video bios
	jb	short sysvideo_15_4 ; no, nothing to do !
	ja	short sysvideo_15_0 ; yes

	; Only Bochs/Plex86 (emu) vbe2 video bios is usable in pmid
	; (if [vbe3] = 2) 
	mov	al, [vbe2bios] ; Bochs vbios sign is from C0h to C5h
	and	al, 0F0h
	cmp	al, 0C0h
	jne	short sysvideo_15_4 ; unknown (vbe2) video bios
sysvideo_15_0:
	; 29/01/2021
	cmp	bl, 41h
	ja	short sysvideo_15_4 ; invalid (unknown) sub function
	; 29/01/2021
	mov	[v_ops], bl	; SVGA data transfer options

	mov	eax, ebx ; hw of ebx is vesa vbe video mode
	shr	eax, 16 ; ax = vesa vbe video mode
	jnz	short sysvideo_15_2 
	; ax = 0

	; check & use current video mode
	cmp	byte [CRT_MODE], 0FFh ; extended (SVGA) mode ?
	jne	short sysvideo_15_4 ; no
sysvideo_15_1:
	; use current vbe (svga) video mode
	mov	ax, [video_mode] ; extended (SVGA, VESA VBE) mode
	and	ax, 1FFh ; vesa vbe video mode: 1XXh
sysvideo_15_2:
	; 29/01/2021
	;mov	[maskbuff], edi ; 25/02/2021
	mov	[maskcolor], edi ; 25/02/2021
			; save mask color or bitmask buffer address
	mov	ebp, b_vbe_modes ; vbe mode table (in 'vidata.s')
sysvideo_15_3:
	cmp	ax, [ebp]
	je	short sysvideo_15_5
	add	ebp, 8 ; vbe mode table entry size
	cmp	ebp, end_of_b_vbe_modes
 	jb	short sysvideo_15_3
sysvideo_15_4:
	; desired video mode is not a valid (implemented)
	;	  extended (VESA VBE, SVGA) video mode
	;
	; nothing to do !

 	; [u.r0] = 0  ; return value of EAX
 	jmp	sysret

sysvideo_15_5:
	; get LFB address
	mov	eax, [LFB_ADDR] ; [LFB_Info+LFBINFO.LFB_addr]
	or	eax, eax
	jnz	short sysvideo_15_6
	mov	ax, [def_LFB_addr] ; default LFB addr 
				   ; (for vbe mode 118h)
	shl	eax, 16
	; 27/12/2020
	;jz	short sysvideo_15_4
sysvideo_15_6:
	; 29/01/2021
	mov	[v_mem], eax ; save video memory address
	
	; 27/12/2020
	; 26/12/2020
	mov	eax, [ebp+2] ; width, height
	; 29/01/2021
	mov	[v_width], ax ; save screen width
	; 28/12/2020
	mov	bh, [ebp+6] ; bpp
	; 28/02/2021
	; check default truecolor bpp value and use
	; 32bpp instead of 24bpp if the default value
	; has been set to 32bpp.
	cmp	bh, 24
	jne	short sysvideo_15_16
	cmp	byte [truecolor], 32
		; Default truecolor bpp value,
		; it is 32 for VBE3 video bios
		; (it can be set to 32 or 24)
	jne	short sysvideo_15_16 ; not VBE3 !
				; or it is set to 24
	mov	bh, 32
	; 28/02/2021
sysvideo_15_16:
	; 29/01/2021
	mov	[v_bpp], bh ; bits per pixel

	push	edx ; *
	movzx	edx, ax  ; width
	shr	eax, 16  ; height
	mul	edx
	; eax = linear frame buffer size (pixels)
	; 29/01/2021
	mov	[v_siz], eax ; save video page size
	call	pixels_to_byte_count
	add	eax, [v_mem]
	mov	[v_end], eax ; save end of video page 
	pop	edx ; *

	; bh = bits per pixel
	; (bh will not be used after here, 29/01/2021)

	; bl = pixel operations & options
	; ecx, edx, esi, edi input parameters
	; [maskcolor] = edi input ; 25/02/2021

sysvideo_15_7:
	; 29/01/2021
	;test	byte [v_ops], 40h  ; system to user ?
	test	bl, 40h
	jnz	short sysvideo_15_9

	xor	eax, eax
	mov	al, bl
	mov	ebx, pixel_ops
	and	al, 0Fh ; isolate 16 pixel operations
	shl	al, 2 ; * 4 for dword table pointers 
	add	ebx, eax

	; ebx = subroutine address

	; ecx, edx, esi, edi input parameters
	; [maskbuff] = edi input
	; [maskcolor] = edi input ; 25/02/2021

	call	[ebx]
sysvideo_15_8:
	jmp	sysret

sysvideo_15_9:
	; system to user display page or window copy
	;test	byte [v_ops], 1 ; window copy ?
	test	bl, 1	
	jnz	short sysvideo_15_10

	; display page (full screen copy)
	mov	esi, [v_mem] ; LFB start address
	mov	eax, [v_siz]
	call	pixels_to_byte_count
	mov	ecx, eax ; transfer count in bytes
	;edi = user's buffer address
	call	transfer_to_user_buffer
	jc	short sysvideo_15_8
	mov	[u.r0], ecx
	jmp	short sysvideo_15_8

sysvideo_15_10:
	call	sysvideo_15_12 ; window preparations
	jc	short sysvideo_15_8

	mov	esi, [v_str]
sysvideo_15_11:
	; esi = window's current row address (video mem)
	; edi = current row (virtual) addr in user's buff
	; ecx = transfer count per row
	call	transfer_to_user_buffer
	jc	short sysvideo_15_8
 	add	[u.r0], ecx
	dec	ebx
	jz	short sysvideo_15_8 ; ok.
	; next row
	add	edi, ecx ; next row in user's buffer
	add	esi, edx ; next row of window (system)
	jmp	short sysvideo_15_11

sysvideo_15_14:
	stc	 ; error !
sysvideo_15_15:
	retn

sysvideo_15_12:
	; 30/01/2021
	; 29/01/2021
	; Window address preparations for window copy
	and	dx, dx
	jz	short sysvideo_15_14 ; invalid (zero columns)
	;test	edx, 0FFFF0000h
	;jz	short sysvideo_15_14 ; invalid (zero rows)
	cmp	edx, 65536
	jb	short sysvideo_15_15 ; invalid (zero rows)
	mov	eax, ecx ; start position (row, column)
	call	calc_pixel_offset
	cmp	eax, [v_siz]
	jnb	short sysvideo_15_14 ; out of display page
				; nothing to do
	call	pixels_to_byte_count
	add	eax, [v_mem]
	mov	[v_str], eax ; window start address
			     ; (addr of top left corner)
	; check column limit
	mov	eax, ecx
	add	ax, dx  ; add columns to start column
	jc	short sysvideo_15_15 ; cf = 1
	cmp	ax, [v_width]
	ja	short sysvideo_15_14

	mov	eax, edx ; size
	sub	eax, 65536 ; row count -> 0 based row #
	call	calc_pixel_offset
	cmp	eax, [v_siz] ; video (display) page size
	ja	short sysvideo_15_14 ; out of display page
				; nothing to do
	call	pixels_to_byte_count
	add	eax, [v_str] ; window start address
	cmp	eax, [v_end] ; window end address (+1)
			 ; (addr of bottom right corner +1)
	ja	short sysvideo_15_14 ; out of display page
				; nothing to do
	mov	ebx, edx
	shr	ebx, 16
	; ebx = row count
	and	edx, 0FFFFh
	; edx = transfer count per row (from user's buffer)
	;	(in pixels, window width)
	mov	eax, edx
	mov	[pixcount], eax ; 27/02/2021
	call	pixels_to_byte_count
	mov	ecx, eax
	; ecx = transfer count per row (from user's buffer)
	;	(in bytes, window width)
	mov	ax, [v_width]
	call	pixels_to_byte_count
	mov	edx, eax
	; edx = byte count per row
	retn ; cf = 0

pixels_to_byte_count:
	; 29/01/2021
	; INPUT:
	;   eax = pixel count
	; OUTPUT:
	;   eax = byte count
	;
	cmp	byte [v_bpp], 8
	jna	short pixtobc_3 ; 8 bit colors
	cmp	byte [v_bpp], 24
	jb	short pixtobc_1 ; 16 bit colors
	ja	short pixtobc_2 ; 32 bit colors
	; 24 bit pixels
	; eax = eax * 3
	;push	edx
	;mov	edx, eax
	;shl	eax, 1
	;add	eax, edx
	;pop	edx
	push	eax
	shl	eax, 1
	add	[esp], eax
	pop	eax
	retn
pixtobc_1:
	; 32 bit pixels
	; eax = eax * 2
	shl	eax, 1
	retn
pixtobc_2:
	; 16 bit pixels
	; eax = eax * 4
	shl	eax, 2
pixtobc_3:
	retn

calc_pixel_offset:
	; 29/01/2021
	; INPUT:
	;   eax = pixel position (row, column)
	; OUTPUT:
	;   eax = pixel offset (linear address)
	;
	push	edx
	push	eax
	shr	eax, 16
	jz	short cpixo_0
	; eax = row 
	movzx	edx, word [v_width]
	mul	edx
cpixo_0:
	; eax = row * screen width
	pop	edx
	and	edx, 0FFFFh
	; edx = column
	add	eax, edx
	; eax = (row * screen width) + column
	pop	edx
	retn

	; 02/02/2021
	; 29/01/2021
pixel_ops:
	dd	pix_op_cpy ; copy pixels (user to system)
	dd	pix_op_new ; change (new, fill) color
	dd	pix_op_add ; add color (up to 0FFh)
	dd	pix_op_sub ; sub color (down to 0)
	dd	pix_op_orc ; or color
	dd	pix_op_and ; and color
	dd	pix_op_xor ; xor color
	dd	pix_op_not ; not color
	dd	pix_op_neg ; neg color
	dd	pix_op_inc ; inc color
	dd	pix_op_dec ; dec color
	dd	pix_op_mix ; mix color
	dd	pix_op_rpl ; replace color
	dd	pix_op_blk ; copy pixel block(s) (sys)
	dd	pix_op_lin ; write line(s)
	dd	pix_op_chr ; write character (font)

pix_op_cpy:
	; 21/02/2021
	; 06/02/2021
	; 30/01/2021
	; COPY PIXELS
	;
	; INPUT:
	;  If bit 4 of BL or [v_ops] = 1 -window copy-
	;  ECX = start position (row, column)
	;        (HW = row, CX = column)
	;  EDX = size (rows, colums)
	;        (HW = rows, DX = columns)
	;	 (0 -> invalid 	
	;        (1 -> horizontal or vertical line)
	;  If bit 4 of BL or [v_ops] = 0 -full screen-
	;     ECX and EDX will not be used
  	;  ESI = user's buffer address
	;  [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	test	byte [v_ops], 10h ; display page or window ?
	jnz	short pix_op_cpy_w ; window

	mov	edi, [v_mem] ; 21/02/2021
	
	; Copy user's buffer content do display page
	; (full screen copy)
	mov	eax, [v_siz] ; video page size
	call	pixels_to_byte_count
	mov	ecx, eax ; transfer count
	; esi = user's buffer address (virtual)
	test	byte [v_ops], 20h ; masked copy ?
	jz	short pix_op_cpy_0 ; no	
	jmp	m_pix_op_cpy ; copy pixels except mask color
pix_op_cpy_0:
	; esi = user buffer for full screen copy
	; edi = start of video memory 
	;	(start of display page)
	; ecx = byte count (display page size in bytes)
	call	transfer_from_user_buffer
	jc	short pix_op_cpy_1
	mov	[u.r0], ecx
pix_op_cpy_1:
	retn	; 06/02/2021

pix_op_cpy_w:
	call	sysvideo_15_12 ; window preparations
	jc	short pix_op_cpy_1
	; ecx = bytes per row (to be applied)
	; edx = screen width in bytes
	; ebx = row count
	mov	edi, [v_str]
	test	byte [v_ops], 20h ; masked copy ?
	jz	short pix_op_cpy_w_0 ; no
	jmp	m_pix_op_cpy_w ; window copy except mask color
pix_op_cpy_w_0:
	; esi = current row (virtual) addr in user's buff
	; edi = window's current row address (video mem)
	; ecx = transfer count per row
	call	transfer_from_user_buffer
	jc	short pix_op_cpy_1
 	add	[u.r0], ecx
	dec	ebx
	jz	short pix_op_cpy_1 ; ok.
	; next row
	add	esi, ecx ; next row in user's buffer
	add	edi, edx ; next row of window (system)
	jmp	short pix_op_cpy_w_0

pix_op_add:
	; 31/01/2021
	; 30/01/2021
	; ADD COLOR
	;
	; INPUT:
	;   CL = color (8 bit, 256 colors)
	;  ECX = color (16 bit and true colors)
	;  EDX = start position (row, column)
	;        (HW = row, DX = column)
	;  ESI = size (rows, colums)
	;        (HW = rows, SI = columns)
	;
	;  [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	test	byte [v_ops], 10h ; display page or window ?
	jnz	short pix_op_add_w ; window

	mov	edi, [v_mem]
	mov	esi, edi
	; ecx = color (CL, CX, ECX)
	mov	eax, ecx
	mov	ecx, [v_siz] ; display page pixel count

	test	byte [v_ops], 20h ; masked color adding ?
	jz	short pix_op_add_0 ; no
	jmp	m_pix_op_add ; add color except mask color
pix_op_add_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_add_1

	; 256 colors (8bpp)
	call	pix_op_add_8
	jmp	short pix_op_add_4

pix_op_add_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_add_3 ; 32bpp
	jb	short pix_op_add_2 ; 16bpp

	; 24 bit true colors
	call	pix_op_add_24
	jmp	short pix_op_add_4

	; 65536 colors (16bpp)
pix_op_add_2:
	call	pix_op_add_16
	jmp	short pix_op_add_4

	; 32 bit true colors
pix_op_add_3:
	call	pix_op_add_32
pix_op_add_4:
	sub	edi, esi
	mov	[u.r0], edi
pix_op_add_5:
	retn

pix_op_add_w:
	; 31/01/2021
	push	ecx ; * ; color
	mov	ecx, edx ; win start pos
	mov	edx, esi ; size (rows, cols)
	call	sysvideo_15_12 ; window preparations
	pop	eax ; * ; color
	jc	short pix_op_add_5

	test	byte [v_ops], 20h ; masked color adding ?
	jz	short pix_op_add_w_0 ; no
	jmp	m_pix_op_add_w
			; window add color except mask color
pix_op_add_w_0:
	; ecx = bytes per row (to be applied)
	; edx = screen width in bytes
	; ebx = row count
	; eax = color

	mov	edi, [v_str]
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_add_w_1

	; 256 colors (8bpp)
	mov	ebp, pix_op_add_8
	jmp	short pix_op_add_w_4

pix_op_add_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_add_w_3 ; 32bpp
	jb	short pix_op_add_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, pix_op_add_24
	jmp	short pix_op_add_w_4

	; 65536 colors (16bpp)
pix_op_add_w_2:
	mov	ebp, pix_op_add_16
	jmp	short pix_op_add_w_4

	; 32 bit true colors
pix_op_add_w_3:
	mov	ebp, pix_op_add_32
pix_op_add_w_4:
	jmp	pix_op_add_w_x

pix_op_sub:
	; 31/01/2021
	; SUB COLOR
	;
	; INPUT:
	;   CL = color (8 bit, 256 colors)
	;  ECX = color (16 bit and true colors)
	;  EDX = start position (row, column)
	;        (HW = row, DX = column)
	;  ESI = size (rows, colums)
	;        (HW = rows, SI = columns)
	;
	;  [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	test	byte [v_ops], 10h ; display page or window ?
	jnz	short pix_op_sub_w ; window

	mov	edi, [v_mem]
	mov	esi, edi
	; ecx = color (CL, CX, ECX)
	mov	eax, ecx
	mov	ecx, [v_siz] ; display page pixel count

	test	byte [v_ops], 20h ; masked color subtract ?
	jz	short pix_op_sub_0 ; no
	jmp	m_pix_op_sub ; sub color except mask color
pix_op_sub_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_sub_1

	; 256 colors (8bpp)
	call	pix_op_sub_8
	jmp	short pix_op_sub_4

pix_op_sub_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_sub_3 ; 32bpp
	jb	short pix_op_sub_2 ; 16bpp

	; 24 bit true colors
	call	pix_op_sub_24
	jmp	short pix_op_sub_4

	; 65536 colors (16bpp)
pix_op_sub_2:
	call	pix_op_sub_16
	jmp	short pix_op_sub_4

	; 32 bit true colors
pix_op_sub_3:
	call	pix_op_sub_32
pix_op_sub_4:
	sub	edi, esi
	mov	[u.r0], edi
pix_op_sub_5:
	retn

pix_op_sub_w:
	; 31/01/2021
	push	ecx ; * ; color
	mov	ecx, edx ; win start pos
	mov	edx, esi ; size (rows, cols)
	call	sysvideo_15_12 ; window preparations
	pop	eax ; * ; color
	jc	short pix_op_sub_5

	test	byte [v_ops], 20h ; masked color subtract ?
	jz	short pix_op_sub_w_0 ; no
	jmp	m_pix_op_sub_w 
			; window sub color except mask color
pix_op_sub_w_0:
	; ecx = bytes per row (to be applied)
	; edx = screen width in bytes
	; ebx = row count
	; eax = color

	mov	edi, [v_str]
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_sub_w_1

	; 256 colors (8bpp)
	mov	ebp, pix_op_sub_8
	jmp	short pix_op_sub_w_4

pix_op_sub_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_sub_w_3 ; 32bpp
	jb	short pix_op_sub_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, pix_op_sub_24
	jmp	short pix_op_sub_w_4

	; 65536 colors (16bpp)
pix_op_sub_w_2:
	mov	ebp, pix_op_sub_16
	jmp	short pix_op_sub_w_4

	; 32 bit true colors
pix_op_sub_w_3:
	mov	ebp, pix_op_sub_32
pix_op_sub_w_4:
	jmp	pix_op_sub_w_x

pix_op_mix:
	; 31/01/2021
	; MIX COLOR
	;
	; INPUT:
	;   CL = color (8 bit, 256 colors)
	;  ECX = color (16 bit and true colors)
	;  EDX = start position (row, column)
	;        (HW = row, DX = column)
	;  ESI = size (rows, colums)
	;        (HW = rows, SI = columns)
	;
	;  [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	test	byte [v_ops], 10h ; display page or window ?
	jnz	short pix_op_mix_w ; window

	mov	edi, [v_mem]
	mov	esi, edi
	; ecx = color (CL, CX, ECX)
	mov	eax, ecx
	mov	ecx, [v_siz] ; display page pixel count

	test	byte [v_ops], 20h ; masked color mix ?
	jz	short pix_op_mix_0 ; no
	jmp	m_pix_op_mix ; mix colors except mask color
pix_op_mix_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_mix_1

	; 256 colors (8bpp)
	call	pix_op_mix_8
	jmp	short pix_op_mix_4

pix_op_mix_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_mix_3 ; 32bpp
	jb	short pix_op_mix_2 ; 16bpp

	; 24 bit true colors
	call	pix_op_mix_24
	jmp	short pix_op_mix_4

	; 65536 colors (16bpp)
pix_op_mix_2:
	call	pix_op_mix_16
	jmp	short pix_op_mix_4

	; 32 bit true colors
pix_op_mix_3:
	call	pix_op_mix_32
pix_op_mix_4:
	sub	edi, esi
	mov	[u.r0], edi
pix_op_mix_5:
	retn

pix_op_mix_w:
	; 31/01/2021
	push	ecx ; * ; color
	mov	ecx, edx ; win start pos
	mov	edx, esi ; size (rows, cols)
	call	sysvideo_15_12 ; window preparations
	pop	eax ; * ; color
	jc	short pix_op_mix_5

	test	byte [v_ops], 20h ; masked color mix ?
	jz	short pix_op_mix_w_0 ; no
	jmp	m_pix_op_mix_w 
			; window mix colors except mask color
pix_op_mix_w_0:
	; ecx = bytes per row (to be applied)
	; edx = screen width in bytes
	; ebx = row count
	; eax = color

	mov	edi, [v_str]
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_mix_w_1

	; 256 colors (8bpp)
	mov	ebp, pix_op_mix_8
	jmp	short pix_op_mix_w_x

pix_op_mix_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_mix_w_3 ; 32bpp
	jb	short pix_op_mix_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, pix_op_mix_24
	jmp	short pix_op_mix_w_x

	; 65536 colors (16bpp)
pix_op_mix_w_2:
	mov	ebp, pix_op_mix_16
	jmp	short pix_op_mix_w_x

	; 32 bit true colors
pix_op_mix_w_3:
	mov	ebp, pix_op_mix_32
	;jmp	short pix_op_mix_w_x

pix_op_mix_w_x:
pix_op_add_w_x:
pix_op_sub_w_x:
pix_op_rpl_w_x:
pix_op_orc_w_x:
pix_op_and_w_x:
pix_op_xor_w_x:
	; 27/02/2021
	; 31/01/2021
	; ecx = bytes per row (to be applied)
	; edx = windows (screen) width in bytes
	; ebx = row count
	; eax = color
	; ebp = pixel operation subroutine address
	push	edx
	push	ecx
	push	edi
	mov	ecx, [pixcount] ; 27/02/2021
	call	ebp ; call pixel-row operation
	pop	edi
	pop	ecx ; bytes per row
	add	[u.r0], ecx
	pop	edx
	add	edi, edx ; next row
	dec	ebx
	jnz	short pix_op_mix_w_x
	retn

pix_op_rpl:
	; 01/02/2021
	; REPLACE COLOR
	;
	; INPUT:
	;   CL = old/current color (8 bit, 256 colors)
	;  ECX = old/current color (16 bit and true colors)
	;   DL = new color (8 bit, 256 colors)
	;  EDX = new color (16 bit and true colors)
	;  ESI = start position (row, column)
	;        (HW = row, DX = column)
	;  EDI = size (rows, colums)
	;        (HW = rows, SI = columns)
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful
	
	test	byte [v_ops], 10h ; display page or window ?
	jnz	short pix_op_rpl_w ; window
	
	mov	edi, [v_mem]
	mov	esi, edi
	; ecx = old color (CL, CX, ECX) -to be replaced with-
	; edx = new color (CL, CX, ECX) -new one-
	mov	eax, edx ; new color
	mov	[maskcolor], ecx ; old color
	mov	ecx, [v_siz] ; display page pixel count
pix_op_rpl_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_rpl_1

	; 256 colors (8bpp)
	call	pix_op_rpl_8
	jmp	short pix_op_rpl_4

pix_op_rpl_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_rpl_3 ; 32bpp
	jb	short pix_op_rpl_2 ; 16bpp

	; 24 bit true colors
	call	pix_op_rpl_24
	jmp	short pix_op_rpl_4

	; 65536 colors (16bpp)
pix_op_rpl_2:
	call	pix_op_rpl_16
	jmp	short pix_op_rpl_4

	; 32 bit true colors
pix_op_rpl_3:
	call	pix_op_rpl_32
pix_op_rpl_4:
	sub	edi, esi
	mov	[u.r0], edi
pix_op_rpl_5:
	retn

pix_op_rpl_w:
	; 01/02/2021
	mov	[maskcolor], ecx ; old color
	push	edx ; * ; new color
	mov	ecx, esi ; win start pos
	mov	edx, edi ; size (rows, cols)
	call	sysvideo_15_12 ; window preparations
	pop	eax ; * ; new color
	jc	short pix_op_rpl_5

	; replace window color
pix_op_rpl_w_0:
	; ecx = bytes per row (to be applied)
	; edx = screen width in bytes
	; ebx = row count
	; eax = new color
	; [maskcolor] = old color

	mov	edi, [v_str]

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_rpl_w_1

	; 256 colors (8bpp)
	mov	ebp, pix_op_rpl_8
	jmp	short pix_op_rpl_w_4

pix_op_rpl_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_rpl_w_3 ; 32bpp
	jb	short pix_op_rpl_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, pix_op_rpl_24
	jmp	short pix_op_rpl_w_4

	; 65536 colors (16bpp)
pix_op_rpl_w_2:
	mov	ebp, pix_op_rpl_16
	jmp	short pix_op_rpl_w_4

	; 32 bit true colors
pix_op_rpl_w_3:
	mov	ebp, pix_op_rpl_32
pix_op_rpl_w_4:
	jmp	pix_op_rpl_w_x

pix_op_orc:
	; 31/01/2021
	; OR COLOR
	;
	; INPUT:
	;   CL = color (8 bit, 256 colors)
	;  ECX = color (16 bit and true colors)
	;  EDX = start position (row, column)
	;        (HW = row, DX = column)
	;  ESI = size (rows, colums)
	;        (HW = rows, SI = columns)
	;
	;  [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful
	
	test	byte [v_ops], 10h ; display page or window ?
	jnz	short pix_op_or_w ; window
	
	mov	edi, [v_mem]
	mov	esi, edi
	; ecx = color (CL, CX, ECX)
	mov	eax, ecx
	mov	ecx, [v_siz] ; display page pixel count

	test	byte [v_ops], 20h ; masked color 'or' ?
	jz	short pix_op_or_0 ; no
	jmp	m_pix_op_or ; 'or' color except mask color
pix_op_or_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_or_1

	; 256 colors (8bpp)
	call	pix_op_or_8
	jmp	short pix_op_or_4

pix_op_or_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_or_3 ; 32bpp
	jb	short pix_op_or_2 ; 16bpp

	; 24 bit true colors
	call	pix_op_or_24
	jmp	short pix_op_or_4

	; 65536 colors (16bpp)
pix_op_or_2:
	call	pix_op_or_16
	jmp	short pix_op_or_4

	; 32 bit true colors
pix_op_or_3:
	call	pix_op_or_32
pix_op_or_4:
	sub	edi, esi
	mov	[u.r0], edi
pix_op_or_5:
	retn

pix_op_or_w:
	; 31/01/2021
	push	ecx ; * ; color
	mov	ecx, edx ; win start pos
	mov	edx, esi ; size (rows, cols)
	call	sysvideo_15_12 ; window preparations
	pop	eax ; * ; color
	jc	short pix_op_or_5

	test	byte [v_ops], 20h ; masked color 'or' ?
	jz	short pix_op_or_w_0 ; no
	jmp	m_pix_op_or_w 
			; window 'or' color except mask color
pix_op_or_w_0:
	; ecx = bytes per row (to be applied)
	; edx = screen width in bytes
	; ebx = row count
	; eax = color

	mov	edi, [v_str]
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_or_w_1

	; 256 colors (8bpp)
	mov	ebp, pix_op_or_8
	jmp	short pix_op_or_w_4

pix_op_or_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_or_w_3 ; 32bpp
	jb	short pix_op_or_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, pix_op_or_24
	jmp	short pix_op_or_w_4

	; 65536 colors (16bpp)
pix_op_or_w_2:
	mov	ebp, pix_op_or_16
	jmp	short pix_op_or_w_4

	; 32 bit true colors
pix_op_or_w_3:
	mov	ebp, pix_op_or_32
pix_op_or_w_4:
	jmp	pix_op_orc_w_x

pix_op_and:
	; 31/01/2021
	; AND COLOR
	;
	; INPUT:
	;   CL = color (8 bit, 256 colors)
	;  ECX = color (16 bit and true colors)
	;  EDX = start position (row, column)
	;        (HW = row, DX = column)
	;  ESI = size (rows, colums)
	;        (HW = rows, SI = columns)
	;
	;  [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful
	
	test	byte [v_ops], 10h ; display page or window ?
	jnz	short pix_op_and_w ; window
	
	mov	edi, [v_mem]
	mov	esi, edi
	; ecx = color (CL, CX, ECX)
	mov	eax, ecx
	mov	ecx, [v_siz] ; display page pixel count

	test	byte [v_ops], 20h ; masked color 'and' ?
	jz	short pix_op_and_0 ; no
	jmp	m_pix_op_and ; 'and' color except mask color
pix_op_and_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_and_1

	; 256 colors (8bpp)
	call	pix_op_and_8
	jmp	short pix_op_and_4

pix_op_and_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_and_3 ; 32bpp
	jb	short pix_op_and_2 ; 16bpp

	; 24 bit true colors
	call	pix_op_and_24
	jmp	short pix_op_and_4

	; 65536 colors (16bpp)
pix_op_and_2:
	call	pix_op_and_16
	jmp	short pix_op_and_4

	; 32 bit true colors
pix_op_and_3:
	call	pix_op_and_32
pix_op_and_4:
	sub	edi, esi
	mov	[u.r0], edi
pix_op_and_5:
	retn

pix_op_and_w:
	; 31/01/2021
	push	ecx ; * ; color
	mov	ecx, edx ; win start pos
	mov	edx, esi ; size (rows, cols)
	call	sysvideo_15_12 ; window preparations
	pop	eax ; * ; color
	jc	short pix_op_and_5

	test	byte [v_ops], 20h ; masked color 'and' ?
	jz	short pix_op_and_w_0 ; no
	jmp	m_pix_op_and_w 
			; window 'and' color except mask color
pix_op_and_w_0:
	; ecx = bytes per row (to be applied)
	; edx = screen width in bytes
	; ebx = row count
	; eax = color

	mov	edi, [v_str]
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_and_w_1

	; 256 colors (8bpp)
	mov	ebp, pix_op_and_8
	jmp	short pix_op_and_w_4

pix_op_and_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_and_w_3 ; 32bpp
	jb	short pix_op_and_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, pix_op_and_24
	jmp	short pix_op_and_w_4

	; 65536 colors (16bpp)
pix_op_and_w_2:
	mov	ebp, pix_op_and_16
	jmp	short pix_op_and_w_4

	; 32 bit true colors
pix_op_and_w_3:
	mov	ebp, pix_op_and_32
pix_op_and_w_4:
	jmp	pix_op_and_w_x

pix_op_xor:
	; 31/01/2021
	; XOR COLOR
	;
	; INPUT:
	;   CL = color (8 bit, 256 colors)
	;  ECX = color (16 bit and true colors)
	;  EDX = start position (row, column)
	;        (HW = row, DX = column)
	;  ESI = size (rows, colums)
	;        (HW = rows, SI = columns)
	;
	;  [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful
	
	test	byte [v_ops], 10h ; display page or window ?
	jnz	short pix_op_xor_w ; window

	mov	edi, [v_mem]
	mov	esi, edi
	; ecx = color (CL, CX, ECX)
	mov	eax, ecx
	mov	ecx, [v_siz] ; display page pixel count

	test	byte [v_ops], 20h ; masked color 'xor' ?
	jz	short pix_op_xor_0 ; no
	jmp	m_pix_op_xor ; 'xor' color except mask color
pix_op_xor_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_xor_1

	; 256 colors (8bpp)
	call	pix_op_xor_8
	jmp	short pix_op_xor_4

pix_op_xor_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_xor_3 ; 32bpp
	jb	short pix_op_xor_2 ; 16bpp

	; 24 bit true colors
	call	pix_op_xor_24
	jmp	short pix_op_xor_4

	; 65536 colors (16bpp)
pix_op_xor_2:
	call	pix_op_xor_16
	jmp	short pix_op_xor_4

	; 32 bit true colors
pix_op_xor_3:
	call	pix_op_xor_32
pix_op_xor_4:
	sub	edi, esi
	mov	[u.r0], edi
pix_op_xor_5:
	retn

pix_op_xor_w:
	; 31/01/2021
	push	ecx ; * ; color
	mov	ecx, edx ; win start pos
	mov	edx, esi ; size (rows, cols)
	call	sysvideo_15_12 ; window preparations
	pop	eax ; * ; color
	jc	short pix_op_xor_5

	test	byte [v_ops], 20h ; masked color 'xor' ?
	jz	short pix_op_xor_w_0 ; no
	jmp	m_pix_op_xor_w 
			; window 'xor' color except mask color
pix_op_xor_w_0:
	; ecx = bytes per row (to be applied)
	; edx = screen width in bytes
	; ebx = row count
	; eax = color

	mov	edi, [v_str]
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_xor_w_1

	; 256 colors (8bpp)
	mov	ebp, pix_op_xor_8
	jmp	short pix_op_xor_w_4

pix_op_xor_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_xor_w_3 ; 32bpp
	jb	short pix_op_xor_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, pix_op_xor_24
	jmp	short pix_op_xor_w_4

	; 65536 colors (16bpp)
pix_op_xor_w_2:
	mov	ebp, pix_op_xor_16
	jmp	short pix_op_xor_w_4

	; 32 bit true colors
pix_op_xor_w_3:
	mov	ebp, pix_op_xor_32
pix_op_xor_w_4:
	jmp	pix_op_xor_w_x

pix_op_new:
	; 31/01/2021
	; 30/01/2021
	; CHANGE COLOR
	;
	; INPUT:
	;   CL = color (8 bit, 256 colors)
	;  ECX = color (16 bit and true colors)
	;  EDX = start position (row, column)
	;        (HW = row, DX = column)
	;  ESI = size (rows, colums)
	;        (HW = rows, SI = columns)
	;
	;  [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful
	
	test	byte [v_ops], 10h ; display page or window ?
	jnz	short pix_op_new_w ; window
	
	mov	edi, [v_mem]
	mov	esi, edi
	; ecx = color (CL, CX, ECX)
	mov	eax, ecx
	mov	ecx, [v_siz] ; display page pixel count

	test	byte [v_ops], 20h ; masked color change ?
	jz	short pix_op_new_0 ; no
	jmp	m_pix_op_new ; change color except mask color
pix_op_new_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_new_2

	; 256 colors (8bpp)
pix_op_new_1:
	mov	ah, al
	shr	ecx, 1
	jmp	short pix_op_new_3

pix_op_new_2:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_new_4 ; 32bpp
	jb	short pix_op_new_3 ; 16bpp

	; 31/01/2021
	
	; 24 bit true colors
	call	pix_op_new_24

	jmp	short pix_op_new_5

	; 65536 colors (16bpp)
pix_op_new_3:
	mov	edx, eax
	shl	eax, 16
	mov	ax, dx
	shr	ecx, 1 ; dword counts
	; 32 bit true colors
pix_op_new_4:
	rep	stosd
pix_op_new_5:
	sub	edi, esi
	mov	[u.r0], edi
pix_op_new_6:
	retn

pix_op_new_w:
	; 31/01/2021
	; 30/01/2021
	push	ecx ; * ; color
	mov	ecx, edx ; win start pos
	mov	edx, esi ; size (rows, cols)
	call	sysvideo_15_12 ; window preparations
	pop	eax ; * ; color
	jc	short pix_op_new_6

	test	byte [v_ops], 20h ; masked color change ?
	jz	short pix_op_new_w_0 ; no
	jmp	m_pix_op_new_w 
			; window chg color except mask color
pix_op_new_w_0:
	; ecx = bytes per row (to be applied)
	; edx = screen width in bytes
	; ebx = row count
	; eax = color

	mov	edi, [v_str]

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_new_w_1

	; 256 colors (8bpp)
	mov	ebp, pix_op_new_8
	jmp	short pix_op_new_w_x

pix_op_new_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_new_w_3 ; 32bpp
	jb	short pix_op_new_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, pix_op_new_24
	jmp	short pix_op_new_w_x

	; 65536 colors (16bpp)
pix_op_new_w_2:
	mov	ebp, pix_op_new_16
	jmp	short pix_op_new_w_x

	; 32 bit true colors
pix_op_new_w_3:
	mov	ebp, pix_op_new_32
	;jmp	short pix_op_new_w_x

pix_op_new_w_x:
pix_op_not_w_x:
pix_op_neg_w_x:
pix_op_inc_w_x:
pix_op_dec_w_x:
	; 27/02/2021
	; 01/02/2021
	; 31/01/2021
	; ecx = bytes per row (to be applied)
	; edx = windows (screen) width in bytes
	; ebx = row count
	; eax = color
	; ebp = pixel operation subroutine address
	;push	edx ; 01/02/2021
	push	ecx
	push	edi
	mov	ecx, [pixcount] ; 27/02/2021
	call	ebp ; call pixel-row operation
	pop	edi
	pop	ecx ; bytes per row
	add	[u.r0], ecx
	;pop	edx ; 01/02/2021
	add	edi, edx ; next row
	dec	ebx
	jnz	short pix_op_new_w_x
	retn

pix_op_not:
	; 31/01/2021
	; NOT COLOR
	;
	; INPUT:
	;  ECX = start position (row, column)
	;        (HW = row, CX = column)
	;  EDX = size (rows, colums)
	;        (HW = rows, DX = columns)
	;	 (0 -> invalid 	
	;        (1 -> horizontal or vertical line)
	;  [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful
	
	test	byte [v_ops], 10h ; display page or window ?
	jnz	short pix_op_not_w ; window
	
	mov	edi, [v_mem]
	mov	esi, edi
	mov	ecx, [v_siz] ; display page pixel count

	test	byte [v_ops], 20h ; masked color 'not' ?
	jz	short pix_op_not_0 ; no
	jmp	m_pix_op_not ; 'not' color except mask color
pix_op_not_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_not_1

	; 256 colors (8bpp)
	call	pix_op_not_8
	jmp	short pix_op_not_4

pix_op_not_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_not_3 ; 32bpp
	jb	short pix_op_not_2 ; 16bpp

	; 24 bit true colors
	call	pix_op_not_24
	jmp	short pix_op_not_4

	; 65536 colors (16bpp)
pix_op_not_2:
	call	pix_op_not_16
	jmp	short pix_op_not_4

	; 32 bit true colors
pix_op_not_3:
	call	pix_op_not_32
pix_op_not_4:
	sub	edi, esi
	mov	[u.r0], edi
pix_op_not_5:
	retn

pix_op_not_w:
	; 31/01/2021
	; ecx = win start pos (row, column)
	; edx = size (rows, columns)
	call	sysvideo_15_12 ; window preparations
	jc	short pix_op_not_5

	test	byte [v_ops], 20h ; masked color 'not' ?
	jz	short pix_op_not_w_0 ; no
	jmp	m_pix_op_not_w 
			; window 'not' color except mask color
pix_op_not_w_0:
	; ecx = bytes per row (to be applied)
	; edx = screen width in bytes
	; ebx = row count

	mov	edi, [v_str]

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_not_w_1

	; 256 colors (8bpp)
	mov	ebp, pix_op_not_8
	jmp	short pix_op_not_w_4

pix_op_not_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_not_w_3 ; 32bpp
	jb	short pix_op_not_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, pix_op_not_24
	jmp	short pix_op_not_w_4

	; 65536 colors (16bpp)
pix_op_not_w_2:
	mov	ebp, pix_op_not_16
	jmp	short pix_op_not_w_4

	; 32 bit true colors
pix_op_not_w_3:
	mov	ebp, pix_op_not_32
pix_op_not_w_4:
	jmp	pix_op_not_w_x

pix_op_neg:
	; 31/01/2021
	; NEGATE COLOR
	;
	; INPUT:
	;  ECX = start position (row, column)
	;        (HW = row, CX = column)
	;  EDX = size (rows, colums)
	;        (HW = rows, DX = columns)
	;	 (0 -> invalid 	
	;        (1 -> horizontal or vertical line)
	;  [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	test	byte [v_ops], 10h ; display page or window ?
	jnz	short pix_op_neg_w ; window

	mov	edi, [v_mem]
	mov	esi, edi
	mov	ecx, [v_siz] ; display page pixel count

	test	byte [v_ops], 20h ; masked negate color ?
	jz	short pix_op_neg_0 ; no
	jmp	m_pix_op_neg ; 'neg' color except mask color
pix_op_neg_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_neg_1

	; 256 colors (8bpp)
	call	pix_op_neg_8
	jmp	short pix_op_neg_4

pix_op_neg_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_neg_3 ; 32bpp
	jb	short pix_op_neg_2 ; 16bpp

	; 24 bit true colors
	call	pix_op_neg_24
	jmp	short pix_op_neg_4

	; 65536 colors (16bpp)
pix_op_neg_2:
	call	pix_op_neg_16
	jmp	short pix_op_neg_4

	; 32 bit true colors
pix_op_neg_3:
	call	pix_op_neg_32
pix_op_neg_4:
	sub	edi, esi
	mov	[u.r0], edi
pix_op_neg_5:
	retn

pix_op_neg_w:
	; 31/01/2021
	; ecx = win start pos (row, column)
	; edx = size (rows, columns)
	call	sysvideo_15_12 ; window preparations
	jc	short pix_op_neg_5

	test	byte [v_ops], 20h ; masked negate color ?
	jz	short pix_op_neg_w_0 ; no
	jmp	m_pix_op_neg_w 
			; window 'neg' color except mask color
pix_op_neg_w_0:
	; ecx = bytes per row (to be applied)
	; edx = screen width in bytes
	; ebx = row count

	mov	edi, [v_str]

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_neg_w_1

	; 256 colors (8bpp)
	mov	ebp, pix_op_neg_8
	jmp	short pix_op_neg_w_4

pix_op_neg_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_neg_w_3 ; 32bpp
	jb	short pix_op_neg_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, pix_op_neg_24
	jmp	short pix_op_neg_w_4

	; 65536 colors (16bpp)
pix_op_neg_w_2:
	mov	ebp, pix_op_neg_16
	jmp	short pix_op_neg_w_4

	; 32 bit true colors
pix_op_neg_w_3:
	mov	ebp, pix_op_neg_32
pix_op_neg_w_4:
	jmp	pix_op_neg_w_x

pix_op_inc:
	; 31/01/2021
	; INCREASE COLOR
	;
	; INPUT:
	;  ECX = start position (row, column)
	;        (HW = row, CX = column)
	;  EDX = size (rows, colums)
	;        (HW = rows, DX = columns)
	;	 (0 -> invalid 	
	;        (1 -> horizontal or vertical line)
	;  [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful
	
	test	byte [v_ops], 10h ; display page or window ?
	jnz	short pix_op_inc_w ; window
	
	mov	edi, [v_mem]
	mov	esi, edi
	mov	ecx, [v_siz] ; display page pixel count

	test	byte [v_ops], 20h ; masked increase color ?
	jz	short pix_op_inc_0 ; no
	jmp	m_pix_op_inc ; 'inc' color except mask color
pix_op_inc_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_inc_1

	; 256 colors (8bpp)
	call	pix_op_inc_8
	jmp	short pix_op_inc_4

pix_op_inc_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_inc_3 ; 32bpp
	jb	short pix_op_inc_2 ; 16bpp

	; 24 bit true colors
	call	pix_op_inc_24
	jmp	short pix_op_inc_4

	; 65536 colors (16bpp)
pix_op_inc_2:
	call	pix_op_inc_16
	jmp	short pix_op_inc_4

	; 32 bit true colors
pix_op_inc_3:
	call	pix_op_inc_32
pix_op_inc_4:
	sub	edi, esi
	mov	[u.r0], edi
pix_op_inc_5:
	retn

pix_op_inc_w:
	; 31/01/2021
	; ecx = win start pos (row, column)
	; edx = size (rows, columns)
	call	sysvideo_15_12 ; window preparations
	jc	short pix_op_inc_5

	test	byte [v_ops], 20h ; masked increase color ?
	jz	short pix_op_inc_w_0 ; no
	jmp	m_pix_op_inc_w 
			; window 'inc' color except mask color
pix_op_inc_w_0:
	; ecx = bytes per row (to be applied)
	; edx = screen width in bytes
	; ebx = row count

	mov	edi, [v_str]

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_inc_w_1

	; 256 colors (8bpp)
	mov	ebp, pix_op_inc_8
	jmp	short pix_op_inc_w_4

pix_op_inc_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_inc_w_3 ; 32bpp
	jb	short pix_op_inc_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, pix_op_inc_24
	jmp	short pix_op_inc_w_4

	; 65536 colors (16bpp)
pix_op_inc_w_2:
	mov	ebp, pix_op_inc_16
	jmp	short pix_op_inc_w_4

	; 32 bit true colors
pix_op_inc_w_3:
	mov	ebp, pix_op_inc_32
pix_op_inc_w_4:
	jmp	pix_op_inc_w_x

pix_op_dec:
	; 31/01/2021
	; DECREASE COLOR
	;
	; INPUT:
	;  ECX = start position (row, column)
	;        (HW = row, CX = column)
	;  EDX = size (rows, colums)
	;        (HW = rows, DX = columns)
	;	 (0 -> invalid 	
	;        (1 -> horizontal or vertical line)
	;  [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful
	
	test	byte [v_ops], 10h ; display page or window ?
	jnz	short pix_op_dec_w ; window
	
	mov	edi, [v_mem]
	mov	esi, edi
	mov	ecx, [v_siz] ; display page pixel count

	test	byte [v_ops], 20h ; masked decrease color ?
	jz	short pix_op_dec_0 ; no
	jmp	m_pix_op_dec ; 'dec' color except mask color
pix_op_dec_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_dec_1

	; 256 colors (8bpp)
	call	pix_op_dec_8
	jmp	short pix_op_dec_4

pix_op_dec_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_dec_3 ; 32bpp
	jb	short pix_op_dec_2 ; 16bpp

	; 24 bit true colors
	call	pix_op_dec_24
	jmp	short pix_op_dec_4

	; 65536 colors (16bpp)
pix_op_dec_2:
	call	pix_op_dec_16
	jmp	short pix_op_dec_4

	; 32 bit true colors
pix_op_dec_3:
	call	pix_op_dec_32
pix_op_dec_4:
	sub	edi, esi
	mov	[u.r0], edi
pix_op_dec_5:
	retn

pix_op_dec_w:
	; 31/01/2021
	; ecx = win start pos (row, column)
	; edx = size (rows, columns)
	call	sysvideo_15_12 ; window preparations
	jc	short pix_op_dec_5

	test	byte [v_ops], 20h ; masked decrease color ?
	jz	short pix_op_dec_w_0 ; no
	jmp	m_pix_op_dec_w 
			; window 'dec' color except mask color
pix_op_dec_w_0:
	; ecx = bytes per row (to be applied)
	; edx = screen width in bytes
	; ebx = row count

	mov	edi, [v_str]

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_dec_w_1

	; 256 colors (8bpp)
	mov	ebp, pix_op_dec_8
	jmp	short pix_op_dec_w_4

pix_op_dec_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_dec_w_3 ; 32bpp
	jb	short pix_op_dec_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, pix_op_dec_24
	jmp	short pix_op_dec_w_4

	; 65536 colors (16bpp)
pix_op_dec_w_2:
	mov	ebp, pix_op_dec_16
	jmp	short pix_op_dec_w_4

	; 32 bit true colors
pix_op_dec_w_3:
	mov	ebp, pix_op_dec_32
pix_op_dec_w_4:
	jmp	pix_op_dec_w_x

pix_op_blk:
	; 11/08/2022 (TRDOS 386 Kernel v2.0.5)
	; 23/01/2021
	; 22/02/2021
	; 02/02/2021
	; COPY PIXEL BLOCK -system to system-
	; WRITE PIXEL BLOCKS -user to system-
	;
	; INPUT:
	;   -If BL bit 5 is 0-
	;    ECX = start position (row, column) (*)
	;    (HW = row, CX = column)
	;    EDX = size (rows, colums) (*)
	;    (HW = rows, DX = columns)
	;         (0 -> invalid)
	;         (1 -> horizontal or vertical line)
	;    ESI = destination (row, column) (***)
	;   -If BL bit 5 is 1-	
	;     CL = color (8 bit, 256 colors)
	;    ECX = color (16 bit and true colors)
	;    EDX = count of blocks (not bytes)
	;	 (limit: 2048 blocks/windows)
	;    ESI = user's buffer address 
	;	  contains 64 bit block data
	;	  BLOCK ADDRESS - (row, col), dword
	;	  (first 32 bits)
	;	  BLOCK SIZE - (rows, cols), dword
	;	  (second 32 bits)
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; Window option ([v_ops] bit 4) will be ignored
	; (Function is used for display page coordinates)

	test	byte [v_ops], 20h ; masked or direct ?
	jnz	short pix_op_blk_u ; blocks from user's buffer

	mov	eax, esi ; destination position (row, col)
	call	calc_pixel_offset
	cmp	eax, [v_siz]
	jnb	short pix_op_blk_retn ; out of display page
	mov	esi, eax
	call	pixels_to_byte_count
	mov	edi, eax
	mov	eax, edx  ; size
	call	calc_pixel_offset
	; 22/02/2021
	cmp	eax, [v_siz]
	ja	short pix_op_blk_retn ; out of display page
	add	esi, eax
	cmp	esi, [v_siz]
	ja	short pix_op_blk_retn ; out of display page

	add	edi, [v_mem] ; destination address

	; 23/01/2021
	;call	pixels_to_byte_count
	;add	edi, eax
	;jc	short pix_op_blk_retn ; out of display page
	;cmp	edi, [v_end]
	;ja	short pix_op_blk_retn ; out of display page
	;sub	edi, eax

	call	sysvideo_15_12 ; window preparations
	jc	short pix_op_blk_retn ; something wrong !?
	; ecx = bytes per row (to be applied)
	; edx = screen width in bytes
	; ebx = row count

	mov	esi, [v_str] ; source address

	; Note:
	; ecx & edx are already adjusted for pixel sizes
	; so, following code is proper all pixel sizes

	sub	edx, ecx ; screen width - window width
pix_op_blk_0:
	mov	eax, ecx
	add	[u.r0], eax
	rep	movsb
	mov	ecx, eax
	add	esi, edx ; next row
	add	edi, edx ; next row
	dec	ebx
	jnz	short pix_op_blk_0
pix_op_blk_retn:
	retn

pix_op_blk_u:
	; fill blocks (windows) with desired color
	; according to block definitions in user's buffer
	cmp	edx, 2048
	jna	short pix_op_blk_u_0
	; Maximum 2048 blocks
	mov	edx, 2048
pix_op_blk_u_0:
	and	byte [v_ops], ~20h ; clear masked bit
	mov	[maskcolor], ecx ; save pixel color
	; 22/02/2021
	;mov	ebp, edx ; save blocks count
	;push	ebp
pix_op_blk_u_next:
	push	edx
	;mov	ecx, 8
	; 11/08/2022
	sub	ecx, ecx
	mov	cl, 8
	mov	edi, buffer8 ; 8 bytes small buffer
	; esi = user's buffer address
	call	transfer_from_user_buffer
	jc	short pix_op_blk_retn
	add	esi, ecx ; 22/02/2021
	push	esi
	mov	edx, [buffer8] ; block start pos (row,col)
	mov	esi, [buffer8+4] ; block size (rows,cols)
	mov	ecx, [maskcolor]
	call	pix_op_new_w ; new (change) color (window)
	pop	esi
	;pop	ebp
	;dec	ebp
	pop	edx
	dec	edx
	jnz	short pix_op_blk_u_next
	retn

pix_op_lin:
	; 11/08/2022
	; 23/07/2022 - TRDOS 386 v2.0.5
	; 12/02/2021
	; 11/02/2021
	; 10/02/2021
	; 05/02/2021
	; 02/02/2021
	; WRITE LINE -direct-
	; WRITE LINE(S) -via user's buffer-
	;
	; INPUT:
	;   -If BL bit 5 is 0-	
	;     CL = color (8 bit, 256 colors)
	;    ECX = color (16 bit and true colors)
	;     DX = low 12 bits - size (length)
	;	   high 4 bits - direction or type
	;	     0 - Horizontal line
	;	     1 - Vertical line
	;	   > 1 - undefined, invalid
	;    ESI = start position (row, column)
	;	  (HW = row, SI = column)
	;   -If BL bit 5 is 1-
	;     CL = color (8 bit, 256 colors)
	;    ECX = color (16 bit and true colors)
	;     DX = number of lines (in user buffer)
	;	   (limit: 2048 lines)	
	;    ESI = user's buffer
	;          contains 64 bit data for lines
	;	   START POINT: 32 bit (row, col)
	;	   LENGTH: 32 bit
	;		   high 16 bits - 0
	;		   bit 0-11 - length
	;	   	   bit 12-15 - type (length)
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; Window option ([v_ops] bit 4) will be ignored
	; (Function is used for display page coordinates)

	; 10/02/2021
	test	byte [v_ops], 20h ; masked or direct ?
	jz	short pix_op_lin_vh ; direct (v/h lines)

	; lines from user's buffer
pix_op_lin_u:
	; draw lines with desired color
	; according to line definitions in user's buffer
	cmp	edx, 2048
	jna	short pix_op_lin_u_0
	; Maximum 2048 lines
	mov	edx, 2048
pix_op_lin_u_0:
	mov	[maskcolor], ecx ; save pixel color
	mov	ebp, edx ; save line count
pix_op_lin_u_next:
	;mov	ecx, 8
	; 11/08/2022
	sub	ecx, ecx
	mov	cl, 8
	mov	edi, buffer8 ; 8 bytes small buffer
	; esi = user's buffer address
	call	transfer_from_user_buffer
	jc	short pix_op_lin_retn
	add	esi, ecx ; 11/02/2021
	push	esi
	mov	esi, [buffer8] ; line start pos (row,col)
	mov	edx, [buffer8+4] ; line length
	mov	ecx, [maskcolor]
	call	pix_op_lin_vh ; new (change) color (window)
	pop	esi
	dec	ebp
	jnz	short pix_op_lin_u_next
pix_op_lin_retn:
	retn

pix_op_lin_vh:
	cmp	edx, 1438h ; 1920*1080 (780hx438h) limit
	ja	short pix_op_lin_err1 ; invalid type 
			    	; (for current version)
	test	dx, 0FFFh
	jz	short pix_op_lin_err1 ; zero length!

	mov	eax, esi ; start point (row, col)
	call	calc_pixel_offset
	cmp	eax, [v_siz]
	jnb	short pix_op_lin_err1 ; out of display page!
	call	pixels_to_byte_count
	mov	edi, eax ; start point offset
	add	edi, [v_mem] ; LFB start address
	mov	eax, ecx ; color

	test	dh, 10h
	;jz	pix_op_lin_h ; Horizontal line
	; 23/07/2022
	jnz	short pix_op_lin_v
	jmp	pix_op_lin_h

pix_op_lin_v:
	; Vertical line
	and 	dh, 0Fh ; low 12 bits
	push	ecx ; color
	mov	ecx, edx
	movzx	eax, word [v_width]
	mov	ebx, eax
	; 12/02/2021
	mul	edx ; rows * [v_width]
	add	eax, edi
	cmp	eax, [v_end]
	pop	eax ; color
	ja	short pix_op_lin_err1 ; out of display page
	; ecx = rows	
	mov	edx, ecx

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_lin_v_2
	; 256 colors (1 byte per pixel)
	add	[u.r0], ecx ; byte count
pix_op_lin_v_1:
	mov	[edi], al
	add	edi, ebx  ; next row
	loop	pix_op_lin_v_1
pix_op_lin_err1:
	retn

pix_op_lin_v_2:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_lin_v_6 ; 32bpp
	jb	short pix_op_lin_v_4 ; 16bpp

	; 24 bit true colors
	; * 3
	push	ebx ; screen width in pixels
	shl	ebx, 1
	add	[esp], ebx
	pop	ebx ; screen width in bytes
	add	[u.r0], ecx
	shl	edx, 1
	add	[u.r0], edx ; byte count
pix_op_lin_v_3:
	mov	[edi], ax
	ror	eax, 16
	mov	[edi+2], al
	rol	eax, 16
	add	edi, ebx  ; next row
	loop	pix_op_lin_v_3
	retn

pix_op_lin_v_4:
	; 16 bit (65536) colors
	shl	ebx, 1
	shl	edx, 1
	add	[u.r0], edx
pix_op_lin_v_5:
	mov	[edi], ax
	add	edi, ebx  ; next row
	loop	pix_op_lin_v_5
	retn

pix_op_lin_v_6:
	; 32 bit true colors
	shl	ebx, 2
	shl	edx, 2
	add	[u.r0], edx ; byte count
pix_op_lin_v_7:
	mov	[edi], eax
	add	edi, ebx  ; next row
	loop	pix_op_lin_v_7
	retn

pix_op_lin_h:
	; Horizontal line
	and 	dh, 0Fh ; low 12 bits
  	mov	ecx, edx
	add	si, dx ; start column + columns
	cmp	si, [v_width] ; screen width
	ja	short pix_op_lin_err2 ; out of columns limit

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_lin_h_1
	; 256 colors (1 byte per pixel)
	add	[u.r0], ecx
	rep	stosb
pix_op_lin_err2:
	retn

pix_op_lin_h_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_lin_h_4 ; 32bpp
	jb	short pix_op_lin_h_3 ; 16bpp
	
	; 24 bit true colors
	; * 3
	add	[u.r0], edx
	shl	edx, 1
	add	[u.r0], edx
pix_op_lin_h_2:
	stosw
	ror	eax, 16
	stosb	
	rol	eax, 16
	loop	pix_op_lin_h_2
	retn

pix_op_lin_h_3:
	; 16 bit (65536) colors
	shl	edx, 1
	add	[u.r0], edx
	rep	stosw
	retn

pix_op_lin_h_4:
	; 32 bit true colors
	shl	edx, 2
	add	[u.r0], edx
	rep	stosd
	retn

pix_op_new_8:
	; 8 bit colors (256 colors)
	; CHANGE PIXEL COLOR
	; ecx = pixel count per row
	;  al = color
	; edi = start pixel address

	rep	stosb
	retn

pix_op_new_16:
	; 16 bit colors (65536 colors)
	; CHANGE PIXEL COLOR
	; ecx = pixel count per row
	;  ax = color
	; edi = start pixel address

	rep	stosw
	retn

pix_op_new_24:
	; 24 bit true colors
	; CHANGE PIXEL COLOR
	; ecx = pixel count per row
	; eax = color
	; edi = start pixel address

	stosw
	ror	eax, 16	
	stosb
	rol	eax, 16
	loop	pix_op_new_24
	retn

pix_op_new_32:
	; 32 bit true colors
	; CHANGE PIXEL COLOR
	; ecx = pixel count per row
	; eax = color
	; edi = start pixel address 

	rep	stosd
	retn

pix_op_add_8:
	; 8 bit colors (256 colors)
	; ADD PIXEL COLOR
	; ecx = pixel count per row
	;  al = color
	; edi = start pixel address

	mov	ah, al
pix_op_add_8_0:
	add	al, [edi]
	jnc	short pix_op_add_8_1
	mov	al, 0FFh ; Max. value
pix_op_add_8_1:
	stosb
	mov	al, ah
	loop	pix_op_add_8_0
	retn

pix_op_add_16:
	; 16 bit colors (65536 colors)
	; ADD PIXEL COLOR
	; ecx = pixel count per row
	;  ax = color
	; edi = start pixel address

	mov	edx, eax
pix_op_add_16_0:
	add	ax, [edi]
	jnc	short pix_op_add_16_1
	mov	ax, 0FFFFh ; Max. value
pix_op_add_16_1:
	stosw
	mov	eax, edx
	loop	pix_op_add_16_0
	retn

pix_op_add_24:
	; 24 bit true colors
	; ADD PIXEL COLOR
	; ecx = pixel count per row
	; eax = color
	; edi = start pixel address

	push	ebx
	mov	ebx, 0FFFFFFh
	;and	eax, ebx ; 0FFFFFFh
	mov	edx, eax
pix_op_add_24_0:
	mov	eax, [edi]
	and	eax, ebx ; 0FFFFFFh
	add	eax, edx
	cmp	eax, ebx
	jna	short pix_op_add_24_1
	mov	eax, ebx ; 0FFFFFFh  ; Max. value
pix_op_add_24_1:
	stosw
	shr	eax, 16
	stosb
	loop	pix_op_add_24_0
	mov	eax, edx
	pop	ebx
	retn

pix_op_add_32:
	; 32 bit true colors
	; ADD PIXEL COLOR
	; ecx = pixel count per row
	; eax = color
	; edi = start pixel address

	mov	edx, eax
pix_op_add_32_0:
	add	eax, [edi]
	jnc	short pix_op_add_32_1
	;mov	eax, 0FFFFFFFFh ; Max. value
	sub	eax, eax
	dec	eax
pix_op_add_32_1:
	stosd
	mov	eax, edx
	loop	pix_op_add_32_0
	retn

pix_op_sub_8:
	; 8 bit colors (256 colors)
	; SUBTRACT PIXEL COLOR
	; ecx = pixel count per row
	;  al = color
	; edi = start pixel address 

	mov	ah, al
pix_op_sub_8_0:
	mov	al, [edi]
	sub	al, ah
	jnb	short pix_op_sub_8_1
	xor	al, al ; 0 ; Min. value
pix_op_sub_8_1:
	stosb
	loop	pix_op_sub_8_0
	mov	al, ah
	retn

pix_op_sub_16:
	; 16 bit colors (65536 colors)
	; SUBTRACT PIXEL COLOR
	; ecx = pixel count per row
	;  ax = color
	; edi = start pixel address

	mov	edx, eax
pix_op_sub_16_0:
	mov	ax, [edi]
	sub	ax, dx
	jnb	short pix_op_sub_16_1
	xor	eax, eax ; 0 ; Min. value
pix_op_sub_16_1:
	stosw
	loop	pix_op_sub_16_0
	mov	eax, edx
	retn

pix_op_sub_24:
	; 24 bit true colors
	; SUBTRACT PIXEL COLOR
	; ecx = pixel count per row
	; eax = color
	; edi = start pixel address

	;and	eax, 0FFFFFFh
	mov	edx, eax
pix_op_sub_24_0:
	mov	eax, [edi]
	; 27/02/2021
	and	eax, 0FFFFFFh
	sub	eax, edx
	jnb	short pix_op_sub_24_1
	xor	eax, eax ; 0 ; Min. value
pix_op_sub_24_1: 
	stosw
	shr	eax, 16
	stosb
	loop	pix_op_sub_24_0
	mov	eax, edx
	retn

pix_op_sub_32:
	; 32 bit true colors
	; SUBTRACT PIXEL COLOR
	; ecx = pixel count per row
	; eax = color
	; edi = start pixel address

	mov	edx, eax
pix_op_sub_32_0:
	mov	eax, [edi]
	sub	eax, edx
	jnb	short pix_op_sub_32_1
	xor	eax, eax ; 0 ; Min. value
pix_op_sub_32_1:
	stosd
	loop	pix_op_sub_32_0
	mov	eax, edx
	retn

pix_op_or_8:
	; 8 bit colors (256 colors)
	; OR PIXEL COLOR
	; ecx = pixel count per row
	;  al = color
	; edi = start pixel address

pix_op_or_8_0:
	or	[edi], al
	inc	edi
	loop	pix_op_or_8_0
	retn

pix_op_or_16:
	; 16 bit colors (65536 colors)
	; OR PIXEL COLOR
	; ecx = pixel count per row
	;  ax = color
	; edi = start pixel address 

pix_op_or_16_0:
	or	[edi], ax
	inc	edi
	inc	edi
	loop	pix_op_or_16_0
	retn

pix_op_or_24:
	; 24 bit true colors
	; OR PIXEL COLOR
	; ecx = pixel count per row
	; eax = color
	; edi = start pixel address

	mov	edx, eax
pix_op_or_24_0:
	or	eax, [edi]
	stosw
	shr	eax, 16
	stosb	
	mov	eax, edx
	loop	pix_op_or_24_0
	retn

pix_op_or_32:
	; 32 bit true colors
	; OR PIXEL COLOR
	; ecx = pixel count per row
	; eax = color
	; edi = start pixel address

	;mov	edx, eax
pix_op_or_32_0:
	;or	eax, [edi]
	;stosd
	;mov	eax, edx
	or	[edi], eax
	add	edi, 4
	loop	pix_op_or_32_0
	retn

pix_op_and_8:
	; 8 bit colors (256 colors)
	; AND PIXEL COLOR
	; ecx = pixel count per row
	;  al = color
	; edi = start pixel address

pix_op_and_8_0:
	and	[edi], al
	inc	edi
	loop	pix_op_and_8_0
	retn

pix_op_and_16:
	; 16 bit colors (65536 colors)
	; AND PIXEL COLOR
	; ecx = pixel count per row
	;  ax = color
	; edi = start pixel address

pix_op_and_16_0:
	and	[edi], ax
	inc	edi
	inc	edi
	loop	pix_op_and_16_0
	retn

pix_op_and_24:
	; 24 bit true colors
	; AND PIXEL COLOR
	; ecx = pixel count per row
	; eax = color
	; edi = start pixel address

	mov	edx, eax
pix_op_and_24_0:
	and	eax, [edi]
	stosw
	shr	eax, 16
	stosb	
	mov	eax, edx
	loop	pix_op_and_24_0
	retn

pix_op_and_32:
	; 32 bit true colors
	; AND PIXEL COLOR
	; ecx = pixel count per row
	; eax = color
	; edi = start pixel address

	;mov	edx, eax
pix_op_and_32_0:
	;and	eax, [edi]
	;stosd
	;mov	eax, edx
	and	[edi], eax
	add	edi, 4
	loop	pix_op_and_32_0
	retn

pix_op_xor_8:
	; 8 bit colors (256 colors)
	; XOR PIXEL COLOR
	; ecx = pixel count per row
	;  al = color
	; edi = start pixel address

pix_op_xor_8_0:
	xor	[edi], al
	inc	edi
	loop	pix_op_xor_8_0
	retn

pix_op_xor_16:
	; 16 bit colors (65536 colors)
	; XOR PIXEL COLOR
	; ecx = pixel count per row
	;  ax = color
	; edi = start pixel address

pix_op_xor_16_0:
	xor	[edi], ax
	inc	edi
	inc	edi
	loop	pix_op_xor_16_0
	retn

pix_op_xor_24:
	; 24 bit true colors
	; XOR PIXEL COLOR
	; ecx = pixel count per row
	; eax = color
	; edi = start pixel address

	mov	edx, eax
pix_op_xor_24_0:
	xor	eax, [edi]
	stosw
	shr	eax, 16
	stosb	
	mov	eax, edx
	loop	pix_op_xor_24_0
	retn

pix_op_xor_32:
	; 32 bit true colors
	; XOR PIXEL COLOR
	; ecx = pixel count per row
	; eax = color
	; edi = start pixel address

	;mov	edx, eax
pix_op_xor_32_0:
	;xor	eax, [edi]
	;stosd
	;mov	eax, edx
	xor	[edi], eax
	add	edi, 4
	loop	pix_op_xor_32_0
	retn

pix_op_mix_8:
	; 8 bit colors (256 colors)
	; MIX (AVERAGE) PIXEL COLORS
	; ecx = pixel count per row
	;  al = color
	; edi = start pixel address

	mov	ah, al
pix_op_mix_8_0:
	add	al, [edi]
	rcr	al, 1
	stosb
	mov	al, ah
	loop	pix_op_mix_8_0
	retn

pix_op_mix_16:
	; 16 bit colors (65536 colors)
	; MIX (AVERAGE) PIXEL COLORS
	; ecx = pixel count per row
	;  ax = color
	; edi = start pixel address

	mov	edx, eax
pix_op_mix_16_0:
	add	ax, [edi]
	rcr	ax, 1
	stosw
	mov	eax, edx
	loop	pix_op_mix_16_0
	retn

pix_op_mix_24:
	; 24 bit true colors
	; MIX (AVERAGE) PIXEL COLORS
	; ecx = pixel count per row
	; eax = color
	; edi = start pixel address

	push	ebx
	mov	ebx, 0FFFFFFh
	;and	eax, ebx ; 0FFFFFFh
	mov	edx, eax
pix_op_mix_24_0:
	mov	eax, [edi]
	and	eax, ebx ; 0FFFFFFh
	add	eax, edx
	shr	eax, 1
	;rcr	eax, 1
	stosw
	shr	eax, 16
	stosb
	loop	pix_op_mix_24_0
	mov	eax, edx
	pop	ebx
	retn

pix_op_mix_32:
	; 32 bit true colors
	; MIX (AVERAGE) PIXEL COLORS
	; ecx = pixel count per row
	; eax = color
	; edi = start pixel address

	mov	edx, eax
pix_op_mix_32_0:
	add	eax, [edi]
	rcr	eax, 1	
	stosd
	mov	eax, edx
	loop	pix_op_mix_32_0
	retn

pix_op_not_8:
	; 8 bit colors (256 colors)
	; NOT PIXEL COLOR
	; ecx = pixel count per row
	; edi = start pixel address

pix_op_not_8_0:
	not	byte [edi]
	inc	edi
	loop	pix_op_not_8_0
	retn

pix_op_not_16:
	; 16 bit colors (65536 colors)
	; NOT PIXEL COLOR
	; ecx = pixel count per row
	; edi = start pixel address

pix_op_not_16_0:
	not	word [edi]
	inc	edi
	inc	edi
	loop	pix_op_not_16_0
	retn

pix_op_not_24:
	; 24 bit true colors
	; NOT PIXEL COLOR
	; ecx = pixel count per row
	; edi = start pixel address

pix_op_not_24_0:
	not	word [edi]
	inc	edi
	inc	edi
	not	byte [edi]
	inc	edi
	loop	pix_op_not_24_0
	retn

pix_op_not_32:
	; 32 bit true colors
	; NOT PIXEL COLOR
	; ecx = pixel count per row
	; eax = color
	; edi = start pixel address
pix_op_not_32_0:
	not	dword [edi]
	add	edi, 4
	loop	pix_op_not_32_0
	retn

pix_op_neg_8:
	; 8 bit colors (256 colors)
	; NEG PIXEL COLOR
	; ecx = pixel count per row
	; edi = start pixel address

pix_op_neg_8_0:
	neg	byte [edi]
	inc	edi
	loop	pix_op_neg_8_0
	retn

pix_op_neg_16:
	; 16 bit colors (65536 colors)
	; NEG PIXEL COLOR
	; ecx = pixel count per row
	; edi = start pixel address

pix_op_neg_16_0:
	neg	word [edi]
	inc	edi
	inc	edi
	loop	pix_op_neg_16_0
	retn

pix_op_neg_24:
	; 24 bit true colors
	; NEG PIXEL COLOR
	; ecx = pixel count per row
	; edi = start pixel address

pix_op_neg_24_0:
	mov	eax, [edi]
	and	eax, 0FFFFFFh
	neg	eax
	stosw
	shr	eax, 16
	stosb
	loop	pix_op_neg_24_0
	retn

pix_op_neg_32:
	; 32 bit true colors
	; NEG PIXEL COLOR
	; ecx = pixel count per row
	; eax = color
	; edi = start pixel address
pix_op_neg_32_0:
	neg	dword [edi]
	add	edi, 4
	loop	pix_op_neg_32_0
	retn

pix_op_inc_8:
	; 8 bit colors (256 colors)
	; INCREASE PIXEL COLOR
	; ecx = pixel count per row
	; edi = start pixel address

pix_op_inc_8_0:
	inc	byte [edi]
	jnz	short pix_op_inc_8_1
	;mov	[edi], 0FFh ; Max. value
	dec	byte [edi]
pix_op_inc_8_1:
	inc	edi
	loop	pix_op_inc_8_0
	retn

pix_op_inc_16:
	; 16 bit colors (65536 colors)
	; INCREASE PIXEL COLOR
	; ecx = pixel count per row
	; edi = start pixel address

pix_op_inc_16_0:
	inc	word [edi]
	jnz	short pix_op_inc_16_1
	;mov	word [edi], 0FFFFh ; Max. value
	dec	word [edi]
pix_op_inc_16_1:
	inc	edi
	inc	edi
	loop	pix_op_inc_16_0
	retn

pix_op_inc_24:
	; 24 bit true colors
	; INCREASE PIXEL COLOR
	; ecx = pixel count per row
	; edi = start pixel address

pix_op_inc_24_0:
	mov	eax, [edi]
	inc	eax
	and	eax, 0FFFFFFh
	jnz	short pix_op_inc_24_1
	;mov	eax, 0FFFFFFh  ; Max. value
	dec	eax ; 0FFFFFFFFh
pix_op_inc_24_1: 
	stosw
	shr	eax, 16
	stosb
	loop	pix_op_inc_24_0
	retn

pix_op_inc_32:
	; 32 bit true colors
	; INCREASE PIXEL COLOR
	; ecx = pixel count per row
	; edi = start pixel address

pix_op_inc_32_0:
	inc	dword [edi]
	jnz	short pix_op_inc_32_1
	;mov	dword [edi], 0FFFFFFFFh ; Max. value
	dec	dword [edi]
pix_op_inc_32_1:
	add	edi, 4
	loop	pix_op_inc_32_0
	retn

pix_op_dec_8:
	; 8 bit colors (256 colors)
	; DECREASE PIXEL COLOR
	; ecx = pixel count per row
	; edi = start pixel address

pix_op_dec_8_0:
	dec	byte [edi]
	jns	short pix_op_dec_8_1
	inc	byte [edi] ; 0 ; Min. value
pix_op_dec_8_1:
	inc	edi
	loop	pix_op_dec_8_0
	retn

pix_op_dec_16:
	; 16 bit colors (65536 colors)
	; DECREASE PIXEL COLOR
	; ecx = pixel count per row
	; edi = start pixel address

pix_op_dec_16_0:
	dec	word [edi]
	jns	short pix_op_dec_16_1
	inc	word [edi] ; 0 ; Min. value
pix_op_dec_16_1:
	inc edi
	inc edi
	loop	pix_op_dec_16_0
	retn

pix_op_dec_24:
	; 24 bit true colors
	; DECREASE PIXEL COLOR
	; ecx = pixel count per row
	; edi = start pixel address

pix_op_dec_24_0:
	mov	eax, [edi]
	and	eax, 0FFFFFFh
	jz	short pix_op_dec_24_1
			; 0 ; Min. value
	dec	eax
pix_op_dec_24_1:
	stosw
	shr	eax, 16
	stosb
	loop	pix_op_inc_24_0
	retn

pix_op_dec_32:
	; 32 bit true colors
	; DECREASE PIXEL COLOR
	; ecx = pixel count per row
	; edi = start pixel address

pix_op_dec_32_0:
	dec	dword [edi]
	jns	short pix_op_dec_32_1
	inc	dword [edi] ; 0 ; Min. value
pix_op_dec_32_1:
	add	edi, 4
	loop	pix_op_dec_32_0
	mov	eax, edx
	retn

pix_op_rpl_8:
	; 8 bit colors (256 colors)
	; REPLACE PIXEL COLORS
	; ecx = pixel count per row
	;  al = new color
	; byte [maskcolor] = old color
	; edi = start pixel address

	mov	ah, [maskcolor]
pix_op_rpl_8_0:
	cmp	ah, [edi]
	jne	short pix_op_rpl_8_1
	mov	[edi], al
pix_op_rpl_8_1:
	inc	edi
	loop	pix_op_rpl_8_0
	retn

pix_op_rpl_16:
	; 16 bit colors (65536 colors)
	; REPLACE PIXEL COLORS
	; ecx = pixel count per row
	;  ax = new color
	; word [maskcolor] = old color
	; edi = start pixel address

	mov	edx, [maskcolor]
pix_op_rpl_16_0:
	cmp	dx, [edi]
	jne	short pix_op_rpl_16_1
	mov	[edi], ax
pix_op_rpl_16_1:
	inc	edi
	inc	edi
	loop	pix_op_rpl_16_0
	retn

pix_op_rpl_24:
	; 24 bit true colors
	; REPLACE PIXEL COLORS
	; ecx = pixel count per row
	; eax = new color
	; [maskcolor] = old color
	; edi = start pixel address

pix_op_rpl_24_0:
	mov	edx, [edi]
	and	edx, 0FFFFFFh
	cmp	edx, [maskcolor]
	je	short pix_op_rpl_24_1
	add	edi, 3
	loop	pix_op_rpl_24_0
	retn
pix_op_rpl_24_1:
	stosb
	ror	eax, 8
	stosw
	rol	eax, 8
	loop	pix_op_rpl_24_0
	retn

pix_op_rpl_32:
	; 32 bit true colors
	; REPLACE PIXEL COLORS
	; ecx = pixel count per row
	; eax = new color
	; [maskcolor] = old color
	; edi = start pixel address

	mov	edx, [maskcolor]
pix_op_rpl_32_0:
	cmp	edx, [edi]
	jne	short pix_op_rpl_32_2
	stosd
	loop	pix_op_rpl_32_0
	retn
pix_op_rpl_32_2:
	add	edi, 4
	loop	pix_op_rpl_32_0
	retn

pix_op_chr:
	; 23/07/2022 - TRDOS 386 v2.0.5
	; 15/02/2021
	; 05/02/2021
	; WRITE CHARACTER (FONT)
	; 05/01/2021 ([ufont])
	; 01/01/2021
	;     CL = char's color (8 bit, 256 colors)
	;    ECX = char's color (16 bit and true colors)
	;     DL = Character's ASCII code
	;     DH bit 0 -> font height
	;	  0 -> 8x16 character font
	;	  1 -> 8x8 character font
	;     DH bit 1 & 2 -> scale
	;	  0 = 1/1 (8 pixels per char row)
	;	  1 = 2/1 (16 pixels per char row)
	;	  2 = 3/1 (24 pixels per char row)
	;	  3 = 4/1 (32 pixels per char row)
	;     DH bit 6 -> [ufont] option (1 = use [ufont])
	;     If DH bit 7 = 1
	;	 USER FONT (from user buffer)
	;	    DL = 0 -> 8x8 (width: 1 byte per row)
	;	    DL = 1 -> 8x16
	;	    DL = 2 -> 16x16 (width: 2 bytes)
	;	    DL = 3 -> 16x32
	;	    DL = 4 -> 24x24 (width: 3 bytes)
	;	    DL = 5 -> 24x48
	;	    DL = 6 -> 32x32 (width: 4 bytes)
	;	    DL = 7 -> 32x64
	;	    DL > 7 -> invalid (unused)
	;	 EDI = user's font buffer address
	;	    (NOTE: byte order is as row0,row1,row2..)
	;     ESI = start position (row, column) (*)
	;	     (HW = row, SI = column)

	mov	eax, esi ; start position
	call	calc_pixel_offset
	cmp	eax, [v_siz]
	jnb	short pix_op_chr_err ; out of display page!
	call	pixels_to_byte_count
	; eax = font start offset
	add	eax, [v_mem] ; LFB start address
	mov	[v_str], eax ; font start address

	mov	[maskcolor], ecx ; save char's color

	mov	[v_ops], dh

	and	esi, 0FFFFh
	mov	[buffer8], esi ; start column

	xor	ebx, ebx ; 0
	xor	eax, eax ; 15/02/2021

	test	dh, 80h
	jnz	short pix_op_chr_u ; user font

	and	dh, 3Fh ; clear bit 6, [UFONT] option bit
	jz	short pix_op_chr_0

	cmp	dh, 7
	ja	short pix_op_chr_err
			 ; invalid (undefined) option
	mov	ah, dh
	shr	ah, 1
	; ah = 0 to 3, scale
	;jmp	short pix_op_chr_font_pixels

pix_op_chr_font_pixels:
	; 05/02/2021
	; write scaled font to buffer

	; DL = ASCII code of character
	; AH = scale
	; EDI = buffer address (kernel)

pix_op_chr_0:
	mov	bl, dl ; 15/02/2021
	xor	ecx, ecx
	mov	dh, 16
	test	byte [v_ops], 1 ; 8x8 font ?
	jz	short pix_op_chr_2 ; 8x16 font
	mov	dh, 8
	shl	ebx, 3 ; * 8
	test	byte [v_ops], 40h ; [ufont] option
	jz	short pix_op_chr_1  ; no
	; test 8x8 user font is ready flag
	test	byte [ufont], 1
	jz	short pix_op_chr_1 ; no
	add	ebx, VGAFONT8USER
	jmp	short pix_op_chr_fpos_0
pix_op_chr_err:
	retn
pix_op_chr_1:
	add	ebx, vgafont8 ; system font (8x8)
	jmp	short pix_op_chr_fpos_0
pix_op_chr_2:
	shl	ebx, 4 ; * 16
	test	byte [v_ops], 40h ; [ufont] option
	jz	short pix_op_chr_3  ; no
	; test 8x16 user font is ready flag
	test	byte [ufont], 2
	jz	short pix_op_chr_3 ; no
	add	ebx, VGAFONT16USER
	jmp	short pix_op_chr_fpos_0
pix_op_chr_3:
	add	ebx, vgafont16 ; system font (8x16)
pix_op_chr_fpos_0:
	and	ah, ah
	jnz	short pix_op_chr_fpos_1 ; scale > 1
	; no scale (scale = 1)
	mov	esi, ebx ; 15/02/2021
	mov	cl, dh ; rows/height (16 or 8)
	mov	dh, 8  ; columns/width
	jmp	pix_op_chr_f2p
pix_op_chr_u:
	; write user defined font 
	cmp	dh, 80h
	jne	short pix_op_chr_err
	cmp	dl, 7
	ja	short pix_op_chr_err

	; 16/02/2021
	mov	esi, edi ; user's font buffer
	
	;xor	eax, eax
	; eax = 0 ; 15/02/2021
	mov	ah, dl
	shr	ah, 1
	inc	ah
	; ah =  1 to 4
	mov	al, ah
	shl	al, 3 ; * 8
	; al = 8,16,24,32
	mov	bl, al
	mov	bh, al
	mul	ah
	; ax = 8,32,72,128 bytes
	test	dl, 1
	jz	short pix_op_chr_u_0
	;shl	ax, 1 ; *2
	; 23/07/2022
	shl	eax, 1
	; ax = 16,32,144,256 bytes
	shl	bh, 1
pix_op_chr_u_0:
	; eax = byte count
	mov	ecx, eax
	mov	edi, VBE3SAVERESTOREBLOCK
	; esi = user buffer
	call	transfer_from_user_buffer
	jc	short pix_op_chr_err

	mov	cl, bh ; rows/height
	mov	dh, bl ; columns (width)
	mov	esi, edi ; VBE3SAVERESTOREBLOCK
	jmp	pix_op_chr_f2p

pix_op_chr_fpos_1:
	; 18/02/2021
	; scale > 1
	mov	ch, dh ; 16 or 8
	mov	edi, VBE3SAVERESTOREBLOCK
	mov	esi, edi
	dec	ah
	jnz	short pix_op_chr_fpos_5 ; scale > 2
	; scale = 2
pix_op_chr_fpos_2:
	mov	cl, 8
	mov	dl, [ebx]
pix_op_chr_fpos_3:
	;shl 	ax, 2
	; 23/07/2022
	shl	eax, 2
	shl	dl, 1
	jnc	short pix_op_chr_fpos_4
	or	al, 3
pix_op_chr_fpos_4:
	dec	cl
	jnz	short pix_op_chr_fpos_3
	stosw
	; 18/02/2021
	stosw
	inc	ebx
	dec	ch
	jnz	short pix_op_chr_fpos_2
	; scale = 2
	mov	cl, dh ; 16 or 8 (height/rows)
	shl	cl, 1  ; 32 or 16 rows
	mov	dh, 16 ; columns (width)
	jmp	short pix_op_chr_f2p
pix_op_chr_fpos_5:
	dec	ah
	jnz	short pix_op_chr_fpos_9 ; scale = 4
	; scale = 3
pix_op_chr_fpos_6:
	mov	cl, 8
	mov	dl, [ebx]
pix_op_chr_fpos_7:
	shl 	eax, 3
	shl	dl, 1 ; 18/02/2021
	jnc	short pix_op_chr_fpos_8
	or	al, 7
pix_op_chr_fpos_8:
	dec	cl
	jnz	short pix_op_chr_fpos_7
	stosw
	; 18/02/2021
	ror	eax, 16
	stosb
	rol	eax, 16
	stosw
	ror	eax, 16
	stosb
	rol	eax, 16
	stosw
	shr	eax, 16 ; 27/02/2021
	stosb
	inc	ebx
	; 18/02/2021
	dec	ch
	jnz	short pix_op_chr_fpos_6
	; scale = 3
	mov	cl, dh ; 16 or 8 (height/rows)
	shl	cl, 1 
	add	cl, dh ; 48 or 24 rows
	mov	dh, 24 ; columns (width)
	jmp	short pix_op_chr_f2p

pix_op_chr_fpos_9:
	; scale = 4
	mov	cl, 8
	mov	dl, [ebx]
pix_op_chr_fpos_10:
	; 18/02/2021
	shl 	eax, 4
	shl	dl, 1 ; 18/02/2021
	jnc	short pix_op_chr_fpos_11
	or	al, 0Fh ; or al, 15
pix_op_chr_fpos_11:
	dec	cl
	jnz	short pix_op_chr_fpos_10
	stosd
	; 18/02/2021
	stosd
	stosd
	stosd
	inc	ebx
	dec	ch
	jnz	short pix_op_chr_fpos_9
	; scale = 4
	mov	cl, dh ; 16 or 8 (height/rows)
	shl	cl, 2 ; 64 or 32 rows
	mov	dh, 32 ; columns (width)
	;jmp	short pix_op_chr_f2p
 	
pix_op_chr_f2p:
	; write font pixels
	mov	edi, [v_str]
	; 15/02/2021
pix_op_chr_f2p_next:
	cmp	dh, 8
	ja	short pix_op_chr_f2p_24
pix_op_chr_f2p_8:
	lodsb
	shl	eax, 24 ; 15/02/2021
	jmp	short pix_op_chr_f2p_0
pix_op_chr_f2p_24:
	cmp	dh, 24
	ja	short pix_op_chr_f2p_32
	jb	short pix_op_chr_f2p_16
	; 27/02/2021
	;mov	eax, [esi]
	lodsd
	shl	eax, 8
	;add	esi, 3
	dec	esi
	jmp	short pix_op_chr_f2p_0
pix_op_chr_f2p_16:
	lodsw
	shl	eax, 16 ; 15/02/2021
	jmp	short pix_op_chr_f2p_0
pix_op_chr_f2p_32:
	lodsd
pix_op_chr_f2p_0:
	; EAX = font row (8,16,24,32 pixels)
	;	(bits are shifted to left)
	; CL = rows
	; DH = bits per row (8,16,24,32)
	mov	ebx, [buffer8] ; start column
	push	edi ; *
	push	edx ; **
pix_op_chr_f2p_1:
	call	pix_op_chr_w_pixel
pix_op_chr_f2p_2:
	cmp	bx, [v_width] ; current column
	jnb	short pix_op_chr_f2p_3
	dec	dh
	jnz	short pix_op_chr_f2p_1 ; next bit
pix_op_chr_f2p_3:
	;mov	ebx, [buffer8]
	pop	edx ; **
	pop	eax ; *
	cmp	edi, [v_end]
	jnb	short pix_op_chr_f2p_4
	dec	cl
	jz	short pix_op_chr_f2p_4
	; 27/02/2021
	mov	edi, eax
	movzx	eax, word [v_width]
	call	pixels_to_byte_count
	add	edi, eax ; next position
	jmp	short pix_op_chr_f2p_next
pix_op_chr_f2p_4:
	retn

pix_op_chr_w_pixel:
	; 15/02/2021
	mov	ebp, eax
	mov	eax, [maskcolor]
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short pix_op_chr_wp_2
	; 256 colors (1 byte per pixel)
	shl	ebp, 1
	jnc	short pix_op_chr_wp_0
	mov	[edi], al
pix_op_chr_wp_0:
	inc	edi
	inc	dword [u.r0] ; +1
pix_op_chr_wp_1:
	inc	ebx
	mov	eax, ebp
	retn
pix_op_chr_wp_2:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short pix_op_chr_wp_6 ; 32bpp
	jb	short pix_op_chr_wp_4 ; 16bpp
	; 24 bit true colors
	; * 3
	shl	ebp, 1
	jnc	short pix_op_chr_wp_3
	mov	[edi], ax
	shr	eax, 16
	mov	[edi+2], al
pix_op_chr_wp_3:
	mov	eax, 3 ; 27/02/2021
	add	edi, eax  ; add edi, 3
	add	[u.r0], eax ; +3
	
	jmp	short pix_op_chr_wp_1

pix_op_chr_wp_4:
	; 16 bit (65536) colors
	shl	ebp, 1
	jnc	short pix_op_chr_wp_5
	mov	[edi], ax
pix_op_chr_wp_5:
	inc	edi
	inc	dword [u.r0] ; +1 
	jmp	short pix_op_chr_wp_0

pix_op_chr_wp_6:
	; 32 bit true colors
	shl	ebp, 1
	jnc	short pix_op_chr_wp_7
	mov	[edi], eax
pix_op_chr_wp_7:
	xor	eax, eax
	mov	al, 4
	add	edi, eax ; add edi, 4
	add	[u.r0], eax ; +4
	jmp	short pix_op_chr_wp_1

m_pix_op_cpy:
	; 26/02/2021
	; 06/02/2021
	; MASKED COPY PIXELS (full screen)
	;
	; jump from pix_op_cpy
	;
	; INPUT:
	;   ecx = transfer count (bytes)
	;   edi = [v_mem] = start address of LFB 
	;   esi = user's buffer address (virtual)
	;
	; OUTPUT:
	;   [u.r0] will be > 0 if succesful

	; Full screen masked copy

	; Modified regs: eax, ebx, edx, esi, edi, ecx

m_pix_op_cpy_0:
	;push	ebx ; *** ; 26/02/2021
	push	edi ; **
	push	ecx ; *
	cmp	ecx, 2040 ; (3*680) ; 26/02/2021
	jna	short m_pix_op_cpy_1
	mov	ecx, 2040
m_pix_op_cpy_1:
	mov	edi, VBE3SAVERESTOREBLOCK ; temporary buff
	call	transfer_from_user_buffer
	jc	short m_pix_op_cpy_3
	add	esi, ecx
	mov	ebp, esi  ; save user's buffer address
	mov	esi, edi
	mov	ebx, ecx
	pop	ecx ; *
	sub	ecx, ebx
	pop	edi ; **
	xor	edx, edx ; 26/02/2021
	cmp	byte [v_bpp], 8
	je	short m_pix_op_cpy_1_8
	cmp	byte [v_bpp], 24
	ja	short m_pix_op_cpy_1_32
	jb	short m_pix_op_cpy_1_16
m_pix_op_cpy_1_24:
	; 24 bit masked copy
	;mov	edx, 3
	mov	dl, 3 ; 26/02/2021
m_pix_op_cpy_1_24_0:
	lodsw
	shl	eax, 16
	lodsb
	rol	eax, 16
	cmp	eax, [maskcolor]
	je	short m_pix_op_cpy_1_24_1 ; exclude
	mov	[edi], ax
	shr	eax, 16
	mov	[edi+2], al
	add	[u.r0], edx ; +3
m_pix_op_cpy_1_24_1:
	add	edi, edx ; +3
	sub	ebx, edx ; sub ebx, 3
	ja	short m_pix_op_cpy_1_24_0
 	jmp	short m_pix_op_cpy_2

m_pix_op_cpy_1_8:
	; 8 bit masked copy
	lodsb
	cmp	al, [maskcolor]
	je	short m_pix_op_cpy_1_8_1 ; exclude
	mov	[edi], al
	inc	dword [u.r0] ; +1
m_pix_op_cpy_1_8_1:
	inc	edi ; +1
	dec	ebx
	jnz	short m_pix_op_cpy_1_8
m_pix_op_cpy_2:
	mov	esi, ebp ; restore user's buffer addr
	or	ecx, ecx
	; 26/02/2021
	jz	short m_pix_of_cpy_4
	jmp	m_pix_op_cpy_0
m_pix_op_cpy_3:
	pop	ecx ; *
	pop	edi ; **
m_pix_of_cpy_4:
	;pop	ebx ; *** ; 26/02/2021
	retn

m_pix_op_cpy_1_16:
	; 16 bit masked copy
	;mov	edx, 2
	mov	dl, 2 ; 26/02/2021
m_pix_op_cpy_1_16_0:
	lodsw
	cmp	ax, [maskcolor]
	je	short m_pix_op_cpy_1_16_1 ; exclude
	mov	[edi], ax
	add	[u.r0], edx ; +2
m_pix_op_cpy_1_16_1:
	add	edi, edx ; +2
	sub	ebx, edx ; sub ebx, 2
	ja	short m_pix_op_cpy_1_16_0
 	jmp	short m_pix_op_cpy_2

m_pix_op_cpy_1_32:
	; 32 bit masked copy
	;mov	edx, 4
	mov	dl, 4 ; 26/02/2021
m_pix_op_cpy_1_32_0:
	lodsd
	cmp	eax, [maskcolor]
	je	short m_pix_op_cpy_1_32_1 ; exclude
	mov	[edi], eax
	add	[u.r0], edx ; +4
m_pix_op_cpy_1_32_1:
	add	edi, edx ; +4
	sub	ebx, edx ; sub ebx, 4
	ja	short m_pix_op_cpy_1_32_0
 	jmp	short m_pix_op_cpy_2

m_pix_op_cpy_w:
	; 26/02/2021
	; 06/02/2021
	; MASKED COPY PIXELS (window)
	;
	; jump from pix_op_cpy_w
	;
	; INPUT:
	;   ecx = bytes per row (to be applied)
	;   edx = screen width in bytes
	;   ebx = row count
	;   esi = user's buffer address
	;   [v_str] = window start address
	;
	; OUTPUT:
	;   [u.r0] will be > 0 if succesful

	; Window masked copy

m_pix_op_cpy_w_0:
	mov	edi, [v_str]
m_pix_op_cpy_w_1:
	push	edi
	push	esi
	push	ebx
	push	edx
	push	ecx
	call	m_pix_op_cpy ; 26/02/2021
	pop	ecx
	pop	edx
	pop	ebx
	pop	esi
	pop	edi
	jc	short m_pix_op_cpy_w_2
	dec	ebx
	jz	short m_pix_op_cpy_w_2 ; ok.
	; next row
	add	esi, ecx ; next row in user's buffer
	add	edi, edx ; next row of window (system)
	jmp	short m_pix_op_cpy_w_1
m_pix_op_cpy_w_2:
	retn

m_pix_op_new:
	; 06/02/2021
	; CHANGE COLOR (MASKED, full screen)
	;
	; jump from pix_op_new
	;
	; INPUT:
	;   eax = color (AL, AX, EAX)
	;   ecx = [v_siz] ; display page pixel count
	;   esi = edi = [v_mem] ; LFB start address
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; Full screen
m_pix_op_new_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_new_1
	; 256 colors (8bpp)
	;jmp	short m_pix_op_new_8
m_pix_op_new_8:
	; 8 bit colors (256 colors)
	mov	dl, al ; new color
m_pix_op_new_8_0:
	lodsb 
	cmp	al, [maskcolor]
	je	short m_pix_op_new_8_1 ; exclude
	mov	[edi], dl
	inc	dword [u.r0]
m_pix_op_new_8_1:
	inc	edi
	loop	m_pix_op_new_8_0
	retn
m_pix_op_new_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_new_3 ; 32bpp
	jb	short m_pix_op_new_2 ; 16bpp
	; 24 bit true colors
	;jmp	short m_pix_op_new_24
m_pix_op_new_24:
	; 24 bit true colors
	mov	edx, eax ; new color
m_pix_op_new_24_0:
	lodsw
	shl	eax, 16
	lodsb
	rol	eax, 16	
	cmp	eax, [maskcolor]
	je	short m_pix_op_new_24_1 ; exclude
	mov	[edi], dx
	ror	edx, 16
	mov	[edi+2], dl
	rol	edx, 16
	add	dword [u.r0], 3
m_pix_op_new_24_1:
	add	edi, 3
	loop	m_pix_op_new_24_0
	retn
	; 65536 colors (16bpp)
m_pix_op_new_2:
	;jmp	short m_pix_op_new_16
m_pix_op_new_16:
	; 16 bit colors (65536 colors)
	mov	edx, eax ; new color
m_pix_op_new_16_0:
	lodsw
	cmp	ax, [maskcolor]
	je	short m_pix_op_new_16_1 ; exclude
	mov	[edi], dx
	add	dword [u.r0], 2
m_pix_op_new_16_1:
	add	edi, 2
	loop	m_pix_op_new_16_0
	retn
m_pix_op_new_3:
	; 32 bit true colors
	;jmp	short m_pix_op_new_32
m_pix_op_new_32:
	; 32 bit true colors
	mov	edx, eax ; new color
m_pix_op_new_32_0:
	lodsd 
	cmp	eax, [maskcolor]
	je	short m_pix_op_new_32_1 ; exclude
	mov	[edi], edx
	add	dword [u.r0], 4
m_pix_op_new_32_1:
	add	edi, 4
	loop	m_pix_op_new_32_0
	retn

m_pix_op_new_w:
	; 06/02/2021
	; CHANGE COLOR (MASKED, window)
	;
	; jump from pix_op_new_w
	;
	; INPUT:
	;   ecx = bytes per row (to be applied)
	;   edx = screen width in bytes
	;   ebx = row count
	;   eax = color
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; Window
	;mov	edi, [v_str] ; LFB start address
	;mov	esi, edi 

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_new_w_1

	; 256 colors (8bpp)
	mov	ebp, m_pix_op_new_8
	jmp	short m_pix_op_new_w_x

m_pix_op_new_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_new_w_3 ; 32bpp
	jb	short m_pix_op_new_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, m_pix_op_new_24
	jmp	short m_pix_op_new_w_x

	; 65536 colors (16bpp)
m_pix_op_new_w_2:
	mov	ebp, m_pix_op_new_16
	jmp	short m_pix_op_new_w_x

	; 32 bit true colors
m_pix_op_new_w_3:
	mov	ebp, m_pix_op_new_32
	;jmp	short m_pix_op_new_w_x

m_pix_op_new_w_x:
m_pix_op_add_w_x:
m_pix_op_sub_w_x:
m_pix_op_mix_w_x:
m_pix_op_and_w_x:
m_pix_op_orc_w_x:
m_pix_op_xor_w_x:
m_pix_op_not_w_x:
m_pix_op_neg_w_x:
m_pix_op_inc_w_x:
m_pix_op_dec_w_x:
	; 27/02/2021
	; 26/02/2021
	; 06/02/2021
	; ecx = bytes per row (to be applied)
	; edx = windows (screen) width in bytes
	; ebx = row count
	; eax = color
	; ebp = pixel operation subroutine address
	; edi = esi = window start address

	mov	edi, [v_str] ; LFB start address
	mov	esi, edi
m_pix_op_w_x_next:
	push	edx
	push	ecx
	push	esi
	push	edi
	push	eax ; 26/02/2021
	mov	ecx, [pixcount] ; 27/02/2021
	call	ebp ; call masked pixel-row operation
	pop	eax ; 26/02/2021
	pop	edi
	pop	esi
	pop	ecx
	pop	edx
	add	esi, edx ; next row
	add	edi, edx ; next row
	dec	ebx
	jnz	short m_pix_op_w_x_next
	retn

m_pix_op_add:
	; 06/02/2021
	; ADD COLOR (MASKED, full screen)
	;
	; jump from pix_op_add
	;
	; INPUT:
	;   eax = color (AL, AX, EAX)
	;   ecx = [v_siz] ; display page pixel count
	;   esi = edi = [v_mem] ; LFB start address
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; Full screen
m_pix_op_add_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_add_1
	; 256 colors (8bpp)
	;jmp	short m_pix_op_add_8
m_pix_op_add_8:
	; 8 bit colors (256 colors)
	mov	dl, al ; new color
m_pix_op_add_8_0:
	lodsb 
	cmp	al, [maskcolor]
	je	short m_pix_op_add_8_1 ; exclude
	inc	dword [u.r0] ; +1
	add	[edi], dl
	jnc	short m_pix_op_add_8_1
	mov	byte [edi], 0FFh 
m_pix_op_add_8_1:
	inc	edi
	loop	m_pix_op_add_8_0
	retn
m_pix_op_add_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_add_3 ; 32bpp
	jb	short m_pix_op_add_2 ; 16bpp
	; 24 bit true colors
	;jmp	short m_pix_op_add_24
m_pix_op_add_24:
	; 24 bit true colors
	mov	edx, eax ; new color
	or	edx, 0FF000000h
m_pix_op_add_24_0:
	lodsw
	shl	eax, 16
	lodsb
	rol	eax, 16	
	cmp	eax, [maskcolor]
	je	short m_pix_op_add_24_2 ; exclude
	add	dword [u.r0], 3 ; +3
	add	eax, edx
	jnc	short m_pix_op_add_24_1
	mov	eax, 0FFFFFFh
m_pix_op_add_24_1:	
	mov	[edi], ax
	shr	eax, 16
	mov	[edi+2], al
m_pix_op_add_24_2:
	add	edi, 3 ; +3
	loop	m_pix_op_add_24_0
	retn
	; 65536 colors (16bpp)
m_pix_op_add_2:
	;jmp	short m_pix_op_add_16
m_pix_op_add_16:
	; 16 bit colors (65536 colors)
	mov	edx, eax ; new color
m_pix_op_add_16_0:
	lodsw
	cmp	ax, [maskcolor]
	je	short m_pix_op_add_16_1 ; exclude
	add	dword [u.r0], 2 ; +2
	add	[edi], dx
	jnc	short m_pix_op_add_16_1
	mov	word [edi], 0FFFFh
m_pix_op_add_16_1:
	add	edi, 2 ; +2
	loop	m_pix_op_add_16_0
	retn
m_pix_op_add_3:
	; 32 bit true colors
	;jmp	short m_pix_op_add_32
m_pix_op_add_32:
	; 32 bit true colors
	mov	edx, eax ; new color
m_pix_op_add_32_0:
	lodsd
	cmp	eax, [maskcolor]
	je	short m_pix_op_add_32_1 ; exclude
	add	dword [u.r0], 4 ; +4
	add	[edi], edx
	jnc	short m_pix_op_add_32_1
	mov	dword [edi], 0FFFFFFFFh
m_pix_op_add_32_1:
	add	edi, 4 ; +4
	loop	m_pix_op_add_32_0
	retn

m_pix_op_add_w:
	; 06/02/2021
	; ADD COLOR (MASKED, window)
	;
	; jump from pix_op_add_w
	;
	; INPUT:
	;   ecx = bytes per row (to be applied)
	;   edx = screen width in bytes
	;   ebx = row count
	;   eax = color
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; window
	;mov	edi, [v_str] ; LFB start address
	;mov	esi, edi 

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_add_w_1

	; 256 colors (8bpp)
	mov	ebp, m_pix_op_add_8
	jmp	short m_pix_op_add_w_4

m_pix_op_add_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_add_w_3 ; 32bpp
	jb	short m_pix_op_add_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, m_pix_op_add_24
	jmp	short m_pix_op_add_w_4

	; 65536 colors (16bpp)
m_pix_op_add_w_2:
	mov	ebp, m_pix_op_add_16
	jmp	short m_pix_op_add_w_4

	; 32 bit true colors
m_pix_op_add_w_3:
	mov	ebp, m_pix_op_add_32
m_pix_op_add_w_4:
	jmp	m_pix_op_add_w_x

m_pix_op_sub:
	; 02/03/2021
	; 06/02/2021
	; SUBTRACT COLOR (MASKED, full screen)
	;
	; jump from pix_op_sub
	;
	; INPUT:
	;   eax = color (AL, AX, EAX)
	;   ecx = [v_siz] ; display page pixel count
	;   esi = edi = [v_mem] ; LFB start address
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; Full screen
m_pix_op_sub_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_sub_1
	; 256 colors (8bpp)
	;jmp	short m_pix_op_sub_8
m_pix_op_sub_8:
	; 8 bit colors (256 colors)
	mov	dl, al ; new color
m_pix_op_sub_8_0:
	lodsb
	cmp	al, [maskcolor]
	je	short m_pix_op_sub_8_1 ; exclude
	inc	dword [u.r0] ; +1
	sub	[edi], dl
	jnb	short m_pix_op_sub_8_1
	mov	byte [edi], 0 
m_pix_op_sub_8_1:
	inc	edi
	loop	m_pix_op_sub_8_0
	retn
m_pix_op_sub_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_sub_3 ; 32bpp
	jb	short m_pix_op_sub_2 ; 16bpp
	; 24 bit true colors
	;jmp	short m_pix_op_sub_24
m_pix_op_sub_24:
	; 24 bit true colors
	mov	edx, eax ; new color
	; 02/03/2021
m_pix_op_sub_24_0:
	lodsw
	shl	eax, 16
	lodsb
	rol	eax, 16
	cmp	eax, [maskcolor]
	je	short m_pix_op_sub_24_2 ; exclude
	add	dword [u.r0], 3 ; +3
	sub	eax, edx
	jnb	short m_pix_op_sub_24_1
	xor	eax, eax ; 0
m_pix_op_sub_24_1:
	mov	[edi], ax
	shr	eax, 16
	mov	[edi+2], al
m_pix_op_sub_24_2:
	add	edi, 3 ; +3
	loop	m_pix_op_sub_24_0
	retn
	; 65536 colors (16bpp)
m_pix_op_sub_2:
	;jmp	short m_pix_op_sub_16
m_pix_op_sub_16:
	; 16 bit colors (65536 colors)
	mov	edx, eax ; new color
m_pix_op_sub_16_0:
	lodsw
	cmp	ax, [maskcolor]
	je	short m_pix_op_sub_16_1 ; exclude
	add	dword [u.r0], 2 ; +2
	sub	[edi], dx
	jnb	short m_pix_op_sub_16_1
	xor	eax, eax
	mov	[edi], ax ; 0
m_pix_op_sub_16_1:
	add	edi, 2 ; +2
	loop	m_pix_op_sub_16_0
	retn
m_pix_op_sub_3:
	; 32 bit true colors
	;jmp	short m_pix_op_sub_32
m_pix_op_sub_32:
	; 32 bit true colors
	mov	edx, eax ; new color
m_pix_op_sub_32_0:
	lodsd 
	cmp	eax, [maskcolor]
	je	short m_pix_op_sub_32_1 ; exclude
	add	dword [u.r0], 4 ; +4
	sub	[edi], edx
	jnb	short m_pix_op_sub_32_1
	xor	eax, eax
	mov	[edi], eax ; 0
m_pix_op_sub_32_1:
	add	edi, 4 ; +4
	loop	m_pix_op_sub_32_0
	retn

m_pix_op_sub_w:
	; 06/02/2021
	; SUBTRACT COLOR (MASKED, window)
	;
	; jump from pix_op_sub_w
	;
	; INPUT:
	;   ecx = bytes per row (to be applied)
	;   edx = screen width in bytes
	;   ebx = row count
	;   eax = color
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; window
	;mov	edi, [v_str] ; LFB start address
	;mov	esi, edi 

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_sub_w_1

	; 256 colors (8bpp)
	mov	ebp, m_pix_op_sub_8
	jmp	short m_pix_op_sub_w_4

m_pix_op_sub_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_sub_w_3 ; 32bpp
	jb	short m_pix_op_sub_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, m_pix_op_sub_24
	jmp	short m_pix_op_sub_w_4

	; 65536 colors (16bpp)
m_pix_op_sub_w_2:
	mov	ebp, m_pix_op_sub_16
	jmp	short m_pix_op_sub_w_4

	; 32 bit true colors
m_pix_op_sub_w_3:
	mov	ebp, m_pix_op_sub_32
m_pix_op_sub_w_4:
	jmp	m_pix_op_sub_w_x

m_pix_op_mix:
	; 25/02/2021
	; 06/02/2021
	; MIX COLOR (MASKED, full screen)
	;
	; jump from pix_op_mix
	;
	; INPUT:
	;   eax = color (AL, AX, EAX)
	;   ecx = [v_siz] ; display page pixel count
	;   esi = edi = [v_mem] ; LFB start address
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; Full screen
m_pix_op_mix_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_mix_1
	; 256 colors (8bpp)
	;jmp	short m_pix_op_mix_8
m_pix_op_mix_8:
	; 8 bit colors (256 colors)
	mov	dl, al ; new (mixing) color
m_pix_op_mix_8_0:
	lodsb 
	cmp	al, [maskcolor]
	je	short m_pix_op_mix_8_1 ; exclude
	add	al, dl ; 25/02/2021
	rcr	al, 1	
	mov	[edi], al
	inc	dword [u.r0] ; +1
m_pix_op_mix_8_1:
	inc	edi
	loop	m_pix_op_mix_8_0
	retn
m_pix_op_mix_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_mix_3 ; 32bpp
	jb	short m_pix_op_mix_2 ; 16bpp
	; 24 bit true colors
	;jmp	short m_pix_op_mix_24
m_pix_op_mix_24:
	; 24 bit true colors
	mov	edx, eax ; new color
	;and	edx, 0FFFFFFh
m_pix_op_mix_24_0:
	lodsw
	shl	eax, 16
	lodsb
	rol	eax, 16	
	cmp	eax, [maskcolor]
	je	short m_pix_op_mix_24_1 ; exclude
	add	eax, edx
	shr	eax, 1
	mov	[edi], ax
	shr	eax, 16
	mov	[edi+2], al
	add	dword [u.r0], 3 ; +3
m_pix_op_mix_24_1:
	add	edi, 3 ; +3
	loop	m_pix_op_mix_24_0
	retn
	; 65536 colors (16bpp)
m_pix_op_mix_2:
	;jmp	short m_pix_op_mix_16
m_pix_op_mix_16:
	; 16 bit colors (65536 colors)
	mov	edx, eax ; new color
	;and	edx, 0FFFFh
m_pix_op_mix_16_0:
	lodsw
	cmp	ax, [maskcolor]
	je	short m_pix_op_mix_16_1 ; exclude
	add	ax, dx
	rcr	ax, 1
	mov	[edi], ax
	add	dword [u.r0], 2 ; +2
m_pix_op_mix_16_1:
	add	edi, 2 ; +2
	loop	m_pix_op_mix_16_0
	retn
m_pix_op_mix_3:
	; 32 bit true colors
	;jmp	short m_pix_op_mix_32
m_pix_op_mix_32:
	; 32 bit true colors
	mov	edx, eax ; new color
m_pix_op_mix_32_0:
	lodsd 
	cmp	eax, [maskcolor]
	je	short m_pix_op_mix_32_1 ; exclude
	add	eax, edx
	; 02/03/2021
	rcr	eax, 1
	mov	[edi], eax
	add	dword [u.r0], 4 ; +4
m_pix_op_mix_32_1:
	add	edi, 4 ; +4
	loop	m_pix_op_mix_32_0
	retn

m_pix_op_mix_w:
	; 06/02/2021
	; MIX COLOR (MASKED, window)
	;
	; jump from pix_op_mix_w
	;
	; INPUT:
	;   ecx = bytes per row (to be applied)
	;   edx = screen width in bytes
	;   ebx = row count
	;   eax = color
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; window
	;mov	edi, [v_str] ; LFB start address
	;mov	esi, edi

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_mix_w_1

	; 256 colors (8bpp)
	mov	ebp, m_pix_op_mix_8
	jmp	short m_pix_op_mix_w_4

m_pix_op_mix_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_mix_w_3 ; 32bpp
	jb	short m_pix_op_mix_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, m_pix_op_mix_24
	jmp	short m_pix_op_mix_w_4

	; 65536 colors (16bpp)
m_pix_op_mix_w_2:
	mov	ebp, m_pix_op_mix_16
	jmp	short m_pix_op_mix_w_4

	; 32 bit true colors
m_pix_op_mix_w_3:
	mov	ebp, m_pix_op_mix_32
m_pix_op_mix_w_4:
	jmp	m_pix_op_mix_w_x

m_pix_op_and:
	; 06/02/2021
	; AND COLOR (MASKED, full screen)
	;
	; jump from pix_op_and
	;
	; INPUT:
	;   eax = color (AL, AX, EAX)
	;   ecx = [v_siz] ; display page pixel count
	;   esi = edi = [v_mem] ; LFB start address
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; Full screen
m_pix_op_and_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_and_1
	; 256 colors (8bpp)
	;jmp	short m_pix_op_and_8
m_pix_op_and_8:
	; 8 bit colors (256 colors)
	mov	dl, al ; new color
m_pix_op_and_8_0:
	lodsb
	cmp	al, [maskcolor]
	je	short m_pix_op_and_8_1 ; exclude
	and	[edi], dl
	inc	dword [u.r0] ; +1
m_pix_op_and_8_1:
	inc	edi
	loop	m_pix_op_and_8_0
	retn
m_pix_op_and_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_and_3 ; 32bpp
	jb	short m_pix_op_and_2 ; 16bpp
	; 24 bit true colors
	;jmp	short m_pix_op_and_24
m_pix_op_and_24:
	; 24 bit true colors
	mov	edx, eax ; new color
	;and	edx, 0FFFFFFh
m_pix_op_and_24_0:
	lodsw
	shl	eax, 16
	lodsb
	rol	eax, 16	
	cmp	eax, [maskcolor]
	je	short m_pix_op_and_24_1 ; exclude
	and	eax, edx
	mov	[edi], ax
	shr	eax, 16
	mov	[edi+2], al
	add	dword [u.r0], 3 ; +3
m_pix_op_and_24_1:
	add	edi, 3 ; +3
	loop	m_pix_op_and_24_0
	retn
	; 65536 colors (16bpp)
m_pix_op_and_2:
	;jmp	short m_pix_op_and_16
m_pix_op_and_16:
	; 16 bit colors (65536 colors)
	mov	edx, eax ; new color
	;and	edx, 0FFFFh
m_pix_op_and_16_0:
	lodsw
	cmp	ax, [maskcolor]
	je	short m_pix_op_and_16_1 ; exclude
	and	[edi], dx
	add	dword [u.r0], 2 ; +2
m_pix_op_and_16_1:
	add	edi, 2 ; +2
	loop	m_pix_op_and_16_0
	retn
m_pix_op_and_3:
	; 32 bit true colors
	;jmp	short m_pix_op_and_32
m_pix_op_and_32:
	; 32 bit true colors
	mov	edx, eax ; new color
m_pix_op_and_32_0:
	lodsd 
	cmp	eax, [maskcolor]
	je	short m_pix_op_and_32_1 ; exclude
	and	[edi], edx ; 25/02/2021
	add	dword [u.r0], 4 ; +4
m_pix_op_and_32_1:
	add	edi, 4 ; +4
	loop	m_pix_op_and_32_0
	retn

m_pix_op_and_w:
	; 06/02/2021
	; AND COLOR (MASKED, window)
	;
	; jump from pix_op_and_w
	;
	; INPUT:
	;   ecx = bytes per row (to be applied)
	;   edx = screen width in bytes
	;   ebx = row count
	;   eax = color
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; window
	;mov	edi, [v_str] ; LFB start address
	;mov	esi, edi 

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_and_w_1

	; 256 colors (8bpp)
	mov	ebp, m_pix_op_and_8
	jmp	short m_pix_op_and_w_4

m_pix_op_and_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_and_w_3 ; 32bpp
	jb	short m_pix_op_and_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, m_pix_op_and_24
	jmp	short m_pix_op_and_w_4

	; 65536 colors (16bpp)
m_pix_op_and_w_2:
	mov	ebp, m_pix_op_and_16
	jmp	short m_pix_op_and_w_4

	; 32 bit true colors
m_pix_op_and_w_3:
	mov	ebp, m_pix_op_and_32
m_pix_op_and_w_4:
	jmp	m_pix_op_and_w_x

m_pix_op_or:
	; 06/02/2021
	; OR COLOR (MASKED, full screen)
	;
	; jump from pix_op_orc
	;
	; INPUT:
	;   eax = color (AL, AX, EAX)
	;   ecx = [v_siz] ; display page pixel count
	;   esi = edi = [v_mem] ; LFB start address
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; Full screen
m_pix_op_or_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_or_1
	; 256 colors (8bpp)
	;jmp	short m_pix_op_or_8
m_pix_op_or_8:
	; 8 bit colors (256 colors)
	mov	dl, al ; new color
m_pix_op_or_8_0:
	lodsb 
	cmp	al, [maskcolor]
	je	short m_pix_op_or_8_1 ; exclude
	or	[edi], dl
	inc	dword [u.r0] ; +1
m_pix_op_or_8_1:
	inc	edi
	loop	m_pix_op_or_8_0
	retn
m_pix_op_or_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_or_3 ; 32bpp
	jb	short m_pix_op_or_2 ; 16bpp
	; 24 bit true colors
	;jmp	short m_pix_op_or_24
m_pix_op_or_24:
	; 24 bit true colors
	mov	edx, eax ; new color
	;and	edx, 0FFFFFFh
m_pix_op_or_24_0:
	lodsw
	shl	eax, 16
	lodsb
	rol	eax, 16	
	cmp	eax, [maskcolor]
	je	short m_pix_op_or_24_1 ; exclude
	or	eax, edx
	mov	[edi], ax
	shr	eax, 16
	mov	[edi+2], al
	add	dword [u.r0], 3 ; +3
m_pix_op_or_24_1:
	add	edi, 3 ; +3
	loop	m_pix_op_or_24_0
	retn
	; 65536 colors (16bpp)
m_pix_op_or_2:
	;jmp	short m_pix_op_or_16
m_pix_op_or_16:
	; 16 bit colors (65536 colors)
	mov	edx, eax ; new color
	;and	edx, 0FFFFh
m_pix_op_or_16_0:
	lodsw
	cmp	ax, [maskcolor]
	je	short m_pix_op_or_16_1 ; exclude
	or	[edi], dx
	add	dword [u.r0], 2 ; +2
m_pix_op_or_16_1:
	add	edi, 2 ; +2
	loop	m_pix_op_or_16_0
	retn
m_pix_op_or_3:
	; 32 bit true colors
	;jmp	short m_pix_op_or_32
m_pix_op_or_32:
	; 32 bit true colors
	mov	edx, eax ; new color
m_pix_op_or_32_0:
	lodsd 
	cmp	eax, [maskcolor]
	je	short m_pix_op_or_32_1 ; exclude
	or	[edi], edx ; 25/02/2021
	add	dword [u.r0], 4 ; +4
m_pix_op_or_32_1:
	add	edi, 4 ; +4
	loop	m_pix_op_or_32_0
	retn

m_pix_op_or_w:
	; 06/02/2021
	; MIX COLOR (MASKED, window)
	;
	; jump from pix_op_or_w
	;
	; INPUT:
	;   ecx = bytes per row (to be applied)
	;   edx = screen width in bytes
	;   ebx = row count
	;   eax = color
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; window
	;mov	edi, [v_str] ; LFB start address
	;mov	esi, edi 

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_or_w_1

	; 256 colors (8bpp)
	mov	ebp, m_pix_op_or_8
	jmp	short m_pix_op_or_w_4

m_pix_op_or_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_or_w_3 ; 32bpp
	jb	short m_pix_op_or_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, m_pix_op_or_24
	jmp	short m_pix_op_or_w_4

	; 65536 colors (16bpp)
m_pix_op_or_w_2:
	mov	ebp, m_pix_op_or_16
	jmp	short m_pix_op_or_w_4

	; 32 bit true colors
m_pix_op_or_w_3:
	mov	ebp, m_pix_op_or_32
m_pix_op_or_w_4:
	jmp	m_pix_op_orc_w_x

m_pix_op_xor:
	; 06/02/2021
	; XOR COLOR (MASKED, full screen)
	;
	; jump from pix_op_xor
	;
	; INPUT:
	;   eax = color (AL, AX, EAX)
	;   ecx = [v_siz] ; display page pixel count
	;   esi = edi = [v_mem] ; LFB start address
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; Full screen
m_pix_op_xor_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_xor_1
	; 256 colors (8bpp)
	;jmp	short m_pix_op_xor_8
m_pix_op_xor_8:
	; 8 bit colors (256 colors)
	mov	dl, al ; new color
m_pix_op_xor_8_0:
	lodsb 
	cmp	al, [maskcolor]
	je	short m_pix_op_xor_8_1 ; exclude
	xor	[edi], dl
	inc	dword [u.r0] ; +1
m_pix_op_xor_8_1:
	inc	edi
	loop	m_pix_op_xor_8_0
	retn
m_pix_op_xor_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_xor_3 ; 32bpp
	jb	short m_pix_op_xor_2 ; 16bpp
	; 24 bit true colors
	;jmp	short m_pix_op_xor_24
m_pix_op_xor_24:
	; 24 bit true colors
	mov	edx, eax ; new color
	;and	edx, 0FFFFFFh
m_pix_op_xor_24_0:
	lodsw
	shl	eax, 16
	lodsb
	rol	eax, 16	
	cmp	eax, [maskcolor]
	je	short m_pix_op_xor_24_1 ; exclude
	xor	eax, edx
	mov	[edi], ax
	shr	eax, 16
	mov	[edi+2], al
	add	dword [u.r0], 3 ; +3
m_pix_op_xor_24_1:
	add	edi, 3 ; +3
	loop	m_pix_op_xor_24_0
	retn
	; 65536 colors (16bpp)
m_pix_op_xor_2:
	;jmp	short m_pix_op_xor_16
m_pix_op_xor_16:
	; 16 bit colors (65536 colors)
	mov	edx, eax ; new color
	;and	edx, 0FFFFh
m_pix_op_xor_16_0:
	lodsw
	cmp	ax, [maskcolor]
	je	short m_pix_op_xor_16_1 ; exclude
	xor	[edi], dx
	add	dword [u.r0], 2 ; +2
m_pix_op_xor_16_1:
	add	edi, 2 ; +2
	loop	m_pix_op_xor_16_0
	retn
m_pix_op_xor_3:
	; 32 bit true colors
	;jmp	short m_pix_op_xor_32
m_pix_op_xor_32:
	; 32 bit true colors
	mov	edx, eax ; new color
m_pix_op_xor_32_0:
	lodsd
	cmp	eax, [maskcolor]
	je	short m_pix_op_xor_32_1 ; exclude
	xor	[edi], edx ; 25/02/2021
	add	dword [u.r0], 4 ; +4
m_pix_op_xor_32_1:
	add	edi, 4 ; +4
	loop	m_pix_op_xor_32_0
	retn

m_pix_op_xor_w:
	; 06/02/2021
	; XOR COLOR (MASKED, window)
	;
	; jump from pix_op_xor_w
	;
	; INPUT:
	;   ecx = bytes per row (to be applied)
	;   edx = screen width in bytes
	;   ebx = row count
	;   eax = color
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; window
	;mov	edi, [v_str] ; LFB start address
	;mov	esi, edi 

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_xor_w_1

	; 256 colors (8bpp)
	mov	ebp, m_pix_op_xor_8
	jmp	short m_pix_op_xor_w_4

m_pix_op_xor_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_xor_w_3 ; 32bpp
	jb	short m_pix_op_xor_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, m_pix_op_xor_24
	jmp	short m_pix_op_xor_w_4

	; 65536 colors (16bpp)
m_pix_op_xor_w_2:
	mov	ebp, m_pix_op_xor_16
	jmp	short m_pix_op_xor_w_4

	; 32 bit true colors
m_pix_op_xor_w_3:
	mov	ebp, m_pix_op_xor_32
m_pix_op_xor_w_4:
	jmp	m_pix_op_xor_w_x

m_pix_op_not:
	; 06/02/2021
	; NOT COLOR (MASKED, full screen)
	;
	; jump from pix_op_not
	;
	; INPUT:
	;   ecx = [v_siz] ; display page pixel count
	;   esi = edi = [v_mem] ; LFB start address
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; Full screen
m_pix_op_not_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_not_1
	; 256 colors (8bpp)
	;jmp	short m_pix_op_not_8
m_pix_op_not_8:
	; 8 bit colors (256 colors)
	lodsb 
	cmp	al, [maskcolor]
	je	short m_pix_op_not_8_1 ; exclude
	not	byte [edi]
	inc	dword [u.r0] ; +1
m_pix_op_not_8_1:
	inc	edi
	loop	m_pix_op_not_8
	retn
m_pix_op_not_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_not_3 ; 32bpp
	jb	short m_pix_op_not_2 ; 16bpp
	; 24 bit true colors
	;jmp	short m_pix_op_not_24
m_pix_op_not_24:
	; 24 bit true colors
	lodsw
	shl	eax, 16
	lodsb
	rol	eax, 16	
	cmp	eax, [maskcolor]
	je	short m_pix_op_not_24_1 ; exclude
	not	eax
	mov	[edi], ax
	shr	eax, 16
	mov	[edi+2], al
	add	dword [u.r0], 3 ; +3
m_pix_op_not_24_1:
	add	edi, 3 ; +3
	loop	m_pix_op_not_24
	retn
	; 65536 colors (16bpp)
m_pix_op_not_2:
	;jmp	short m_pix_op_not_16
m_pix_op_not_16:
	; 16 bit colors (65536 colors)
	lodsw
	cmp	ax, [maskcolor]
	je	short m_pix_op_not_16_1 ; exclude
	not	word [edi]
	add	dword [u.r0], 2 ; +2
m_pix_op_not_16_1:
	add	edi, 2 ; +2
	loop	m_pix_op_not_16
	retn
m_pix_op_not_3:
	; 32 bit true colors
	;jmp	short m_pix_op_not_32
m_pix_op_not_32:
	; 32 bit true colors
	lodsd 
	cmp	eax, [maskcolor]
	je	short m_pix_op_not_32_1 ; exclude
	not	dword [edi]	
	add	dword [u.r0], 4 ; +4
m_pix_op_not_32_1:
	add	edi, 4 ; +4
	loop	m_pix_op_not_32
	retn

m_pix_op_not_w:
	; 06/02/2021
	; NOT COLOR (MASKED, window)
	;
	; jump from pix_op_not_w
	;
	; INPUT:
	;   ecx = bytes per row (to be applied)
	;   edx = screen width in bytes
	;   ebx = row count
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; window
	;mov	edi, [v_str] ; LFB start address
	;mov	esi, edi 

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_not_w_1

	; 256 colors (8bpp)
	mov	ebp, m_pix_op_not_8
	jmp	short m_pix_op_not_w_4

m_pix_op_not_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_not_w_3 ; 32bpp
	jb	short m_pix_op_not_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, m_pix_op_not_24
	jmp	short m_pix_op_not_w_4

	; 65536 colors (16bpp)
m_pix_op_not_w_2:
	mov	ebp, m_pix_op_not_16
	jmp	short m_pix_op_not_w_4

	; 32 bit true colors
m_pix_op_not_w_3:
	mov	ebp, m_pix_op_not_32
m_pix_op_not_w_4:
	jmp	m_pix_op_not_w_x

m_pix_op_neg:
	; 06/02/2021
	; NEGATIVE COLOR (MASKED, full screen)
	;
	; jump from pix_op_neg
	;
	; INPUT:
	;   ecx = [v_siz] ; display page pixel count
	;   esi = edi = [v_mem] ; LFB start address
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; Full screen
m_pix_op_neg_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_neg_1
	; 256 colors (8bpp)
	;jmp	short m_pix_op_neg_8
m_pix_op_neg_8:
	; 8 bit colors (256 colors)
	lodsb 
	cmp	al, [maskcolor]
	je	short m_pix_op_neg_8_1 ; exclude
	neg	byte [edi]
	inc	dword [u.r0] ; +1
m_pix_op_neg_8_1:
	inc	edi
	loop	m_pix_op_neg_8
	retn
m_pix_op_neg_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_neg_3 ; 32bpp
	jb	short m_pix_op_neg_2 ; 16bpp
	; 24 bit true colors
	;jmp	short m_pix_op_neg_24
m_pix_op_neg_24:
	; 24 bit true colors
	lodsw
	shl	eax, 16
	lodsb
	rol	eax, 16	
	cmp	eax, [maskcolor]
	je	short m_pix_op_neg_24_1 ; exclude
	neg	eax
	mov	[edi], ax
	shr	eax, 16
	mov	[edi+2], al
	add	dword [u.r0], 3 ; +3
m_pix_op_neg_24_1:
	add	edi, 3 ; +3
	loop	m_pix_op_neg_24
	retn
	; 65536 colors (16bpp)
m_pix_op_neg_2:
	;jmp	short m_pix_op_neg_16
m_pix_op_neg_16:
	; 16 bit colors (65536 colors)
	lodsw
	cmp	ax, [maskcolor]
	je	short m_pix_op_neg_16_1 ; exclude
	neg	word [edi]
	add	dword [u.r0], 2 ; +2
m_pix_op_neg_16_1:
	add	edi, 2 ; +2
	loop	m_pix_op_neg_16
	retn
m_pix_op_neg_3:
	; 32 bit true colors
	;jmp	short m_pix_op_neg_32
m_pix_op_neg_32:
	; 32 bit true colors
	lodsd 
	cmp	eax, [maskcolor]
	je	short m_pix_op_neg_32_1 ; exclude
	neg	dword [edi]
	add	dword [u.r0], 4 ; +4
m_pix_op_neg_32_1:
	add	edi, 4 ; +4
	loop	m_pix_op_neg_32
	retn

m_pix_op_neg_w:
	; 06/02/2021
	; NEGATIVE COLOR (MASKED, window)
	;
	; jump from pix_op_neg_w
	;
	; INPUT:
	;   ecx = bytes per row (to be applied)
	;   edx = screen width in bytes
	;   ebx = row count
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; window
	;mov	edi, [v_str] ; LFB start address
	;mov	esi, edi 

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_neg_w_1

	; 256 colors (8bpp)
	mov	ebp, m_pix_op_neg_8
	jmp	short m_pix_op_neg_w_4
			
m_pix_op_neg_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_neg_w_3 ; 32bpp
	jb	short m_pix_op_neg_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, m_pix_op_neg_24
	jmp	short m_pix_op_neg_w_4

	; 65536 colors (16bpp)
m_pix_op_neg_w_2:
	mov	ebp, m_pix_op_neg_16
	jmp	short m_pix_op_neg_w_4

	; 32 bit true colors
m_pix_op_neg_w_3:
	mov	ebp, m_pix_op_neg_32
m_pix_op_neg_w_4:
	jmp	m_pix_op_neg_w_x

m_pix_op_inc:
	; 06/02/2021
	; INCREASE COLOR (MASKED, full screen)
	;
	; jump from pix_op_inc
	;
	; INPUT:
	;   ecx = [v_siz] ; display page pixel count
	;   esi = edi = [v_mem] ; LFB start address
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; Full screen
m_pix_op_inc_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_inc_1
	; 256 colors (8bpp)
	;jmp	short m_pix_op_inc_8
m_pix_op_inc_8:
	; 8 bit colors (256 colors)
	lodsb
	cmp	al, [maskcolor]
	je	short m_pix_op_inc_8_1 ; exclude
	inc	byte [edi]
	jnz	short m_pix_op_inc_8_0
	dec	byte [edi]
m_pix_op_inc_8_0:
	inc	dword [u.r0] ; +1
m_pix_op_inc_8_1:
	inc	edi
	loop	m_pix_op_inc_8
	retn
m_pix_op_inc_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_inc_3 ; 32bpp
	jb	short m_pix_op_inc_2 ; 16bpp
	; 24 bit true colors
	;jmp	short m_pix_op_inc_24
m_pix_op_inc_24:
	; 24 bit true colors
	lodsw
	shl	eax, 16
	lodsb
	rol	eax, 16	
	cmp	eax, [maskcolor]
	je	short m_pix_op_inc_24_1 ; exclude
	inc	eax
	cmp	eax, 0FFFFFFh
	jna	short m_pix_op_inc_24_0
	dec	eax
m_pix_op_inc_24_0:
	mov	[edi], ax
	shr	eax, 16
	mov	[edi+2], al
	add	dword [u.r0], 3 ; +3
m_pix_op_inc_24_1:
	add	edi, 3 ; +3
	loop	m_pix_op_inc_24
	retn
	; 65536 colors (16bpp)
m_pix_op_inc_2:
	;jmp	short m_pix_op_inc_16
m_pix_op_inc_16:
	; 16 bit colors (65536 colors)
	lodsw
	cmp	ax, [maskcolor]
	je	short m_pix_op_inc_16_1 ; exclude
	inc	word [edi]
	jnz	short m_pix_op_inc_16_0
	dec	word [edi]
m_pix_op_inc_16_0:
	add	dword [u.r0], 2 ; +2
m_pix_op_inc_16_1:
	add	edi, 2 ; +2
	loop	m_pix_op_inc_16
	retn
m_pix_op_inc_3:
	; 32 bit true colors
	;jmp	short m_pix_op_inc_32
m_pix_op_inc_32:
	; 32 bit true colors
	lodsd
	cmp	eax, [maskcolor]
	je	short m_pix_op_inc_32_1 ; exclude
	inc	dword [edi]
	jnz	short m_pix_op_inc_32_0
	dec	dword [edi]
m_pix_op_inc_32_0:
	add	dword [u.r0], 4 ; +4
m_pix_op_inc_32_1:
	add	edi, 4 ; +4
	loop	m_pix_op_inc_32
	retn

m_pix_op_inc_w:
	; 06/02/2021
	; INCREASE COLOR (MASKED, window)
	;
	; jump from pix_op_inc_w
	;
	; INPUT:
	;   ecx = bytes per row (to be applied)
	;   edx = screen width in bytes
	;   ebx = row count
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; window
	;mov	edi, [v_str] ; LFB start address
	;mov	esi, edi 

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_inc_w_1

	; 256 colors (8bpp)
	mov	ebp, m_pix_op_inc_8
	jmp	short m_pix_op_inc_w_4

m_pix_op_inc_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_inc_w_3 ; 32bpp
	jb	short m_pix_op_inc_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, m_pix_op_inc_24
	jmp	short m_pix_op_inc_w_4

	; 65536 colors (16bpp)
m_pix_op_inc_w_2:
	mov	ebp, m_pix_op_inc_16
	jmp	short m_pix_op_inc_w_4

	; 32 bit true colors
m_pix_op_inc_w_3:
	mov	ebp, m_pix_op_inc_32
m_pix_op_inc_w_4:
	jmp	m_pix_op_inc_w_x

m_pix_op_dec:
	; 06/02/2021
	; DECREASE COLOR (MASKED, full screen)
	;
	; jump from pix_op_dec
	;
	; INPUT:
	;   ecx = [v_siz] ; display page pixel count
	;   esi = edi = [v_mem] ; LFB start address
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; Full screen
m_pix_op_dec_0:
	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_dec_1
	; 256 colors (8bpp)
	;jmp	short m_pix_op_dec_8
m_pix_op_dec_8:
	; 8 bit colors (256 colors)
	lodsb 
	cmp	al, [maskcolor]
	je	short m_pix_op_dec_8_1 ; exclude
	dec	byte [edi]
	jns	short m_pix_op_dec_8_0
	inc	byte [edi]
m_pix_op_dec_8_0:
	inc	dword [u.r0] ; +1
m_pix_op_dec_8_1:
	inc	edi
	loop	m_pix_op_dec_8
	retn
m_pix_op_dec_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_dec_3 ; 32bpp
	jb	short m_pix_op_dec_2 ; 16bpp
	; 24 bit true colors
	;jmp	short m_pix_op_dec_24
m_pix_op_dec_24:
	; 24 bit true colors
	lodsw
	shl	eax, 16
	lodsb
	rol	eax, 16	
	cmp	eax, [maskcolor]
	je	short m_pix_op_dec_24_1 ; exclude
	dec	eax
	jns	short m_pix_op_dec_24_0
	inc	eax 
m_pix_op_dec_24_0:
	mov	[edi], ax
	shr	eax, 16
	mov	[edi+2], al
	add	dword [u.r0], 3 ; +3
m_pix_op_dec_24_1:
	add	edi, 3 ; +3
	loop	m_pix_op_dec_24
	retn
	; 65536 colors (16bpp)
m_pix_op_dec_2:
	;jmp	short m_pix_op_dec_16
m_pix_op_dec_16:
	; 16 bit colors (65536 colors)
	lodsw
	cmp	ax, [maskcolor]
	je	short m_pix_op_dec_16_1 ; exclude
	dec	word [edi]
	jns	short m_pix_op_dec_16_0
	inc	word [edi]
m_pix_op_dec_16_0:
	add	dword [u.r0], 2 ; +2
m_pix_op_dec_16_1:
	add	edi, 2 ; +2
	loop	m_pix_op_dec_16
	retn
m_pix_op_dec_3:
	; 32 bit true colors
	;jmp	short m_pix_op_dec_32
m_pix_op_dec_32:
	; 32 bit true colors
	lodsd 
	cmp	eax, [maskcolor]
	je	short m_pix_op_dec_32_1 ; exclude
	dec	dword [edi]
	jns	short m_pix_op_dec_32_0
	inc	dword [edi]
m_pix_op_dec_32_0:
	add	dword [u.r0], 4 ; +4
m_pix_op_dec_32_1:
	add	edi, 4 ; +4
	loop	m_pix_op_dec_32
	retn

m_pix_op_dec_w:
	; 06/02/2021
	; DECREASE COLOR (MASKED, window)
	;
	; jump from pix_op_dec_w
	;
	; INPUT:
	;   ecx = bytes per row (to be applied)
	;   edx = screen width in bytes
	;   ebx = row count
	;
	;   [maskcolor] = mask color (to be excluded)
	;
	; OUTPUT:
	; 	[u.r0] will be > 0 if succesful

	; window
	;mov	edi, [v_str] ; LFB start address
	;mov	esi, edi 

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short m_pix_op_dec_w_1

	; 256 colors (8bpp)
	mov	ebp, m_pix_op_dec_8
	jmp	short m_pix_op_dec_w_4

m_pix_op_dec_w_1:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short m_pix_op_dec_w_3 ; 32bpp
	jb	short m_pix_op_dec_w_2 ; 16bpp

	; 24 bit true colors
	mov	ebp, m_pix_op_dec_24
	jmp	short m_pix_op_dec_w_4

	; 65536 colors (16bpp)
m_pix_op_dec_w_2:
	mov	ebp, m_pix_op_dec_16
	jmp	short m_pix_op_dec_w_4

	; 32 bit true colors
m_pix_op_dec_w_3:
	mov	ebp, m_pix_op_dec_32
m_pix_op_dec_w_4:
	jmp	m_pix_op_dec_w_x

sysvideo_39:
	; 15/02/2021
	; 07/02/2021, 08/02/2021
	; 03/01/2021, 04/01/2021
	; 23/11/2020
	; BH = 3
	; PIXEL READ/WRITE
	
	; 07/02/2021
	; 04/01/2021 (TRDOS 386 v2.0.3)
	cmp	bl, 3
	jna	short sysvideo_39_1
	; 07/02/2021
	cmp	bl, 6
	ja	short sysvideo_39_0
	jmp	sysvideo_39_31
sysvideo_39_0:
	; error
	mov	bl, 0FFh
	mov	ebp, [u.usp]  ; ebp points to user's registers
	mov	[ebp+16], ebx ; EBX
	jmp	sysret
sysvideo_39_1:
	cmp	byte [CRT_MODE], 0FFh
	jnb	short sysvideo_39_2 ; SVGA (VESA VBE) video mode

	; Std VGA or CGA mode
	add	edx, 0A0000h
	jc	short sysvideo_39_0
	cmp	edx, 0AFFFFh
	ja	short sysvideo_39_0
	jmp	short sysvideo_39_3 ; 8bpp

sysvideo_39_2:
	; use current vbe (svga) video mode

	; get LFB address
	mov	eax, [LFB_ADDR] ; [LFB_Info+LFBINFO.LFB_addr]
	or	eax, eax
	jz	short sysvideo_39_0
	cmp	edx, [LFB_SIZE] 
	jnb	short sysvideo_39_0

	add	edx, eax
	;jc	short sysvideo_39_0

	; Pixel read/write in VESA VBE (2/3) video mode
	; Video memory at Linear Frame Buffer base address

	mov	bh, [LFB_Info+LFBINFO.bpp]

	cmp	bh, 8 ; 8bpp
	ja	short sysvideo_39_17

	; 8 bits per pixel
sysvideo_39_3:
	cmp	bl, 1 ; 1 = write pixel
	je	short sysvideo_39_5
	ja	short sysvideo_39_8
sysvideo_39_4:
	; read pixel (8bpp)
	mov	al, [edx]
	;mov	[u.r0], al
	;jmp	sysret
	jmp	short sysvideo_39_7
sysvideo_39_5:
	; write pixel (8bpp)
	mov	al, cl
sysvideo_39_6:
	mov	[edx], al
sysvideo_39_7:
	mov	[u.r0], al
	jmp	sysret
sysvideo_39_8:
	cmp	bl, 3 ; mix
	jb	short sysvideo_39_9
	; mix  pixel colors (8bpp)
	mov	al, [edx]
	add	al, cl
	rcr	al, 1
	jmp	short sysvideo_39_6
sysvideo_39_9:
	 ; swap pixel colors (8bpp)
	mov	al, cl
	xchg	[edx], al
	jmp	short sysvideo_39_7

	; 16 bits per pixel
sysvideo_39_10:
	cmp	bl, 1 ; 1 = write pixel
	je	short sysvideo_39_12
	ja	short sysvideo_39_15
sysvideo_39_11:
	; read pixel (16bpp)
	mov	eax, [edx]
	;mov	[u.r0], ax
	;jmp	sysret
	jmp	short sysvideo_39_14
sysvideo_39_12:
	; write pixel (16bpp)
	mov	eax, ecx
sysvideo_39_13:
	mov	[edx], ax
sysvideo_39_14:
	mov	[u.r0], ax
	jmp	sysret
sysvideo_39_15:
	cmp	bl, 3 ; mix
	jb	short sysvideo_39_16
	; mix  pixel colors (16bpp)
	mov	eax, [edx]
	add	ax, cx
	rcr	ax, 1
	jmp	short sysvideo_39_13
sysvideo_39_16:
	 ; swap pixel colors (16bpp)
	mov	eax, ecx
	xchg	[edx], ax
	jmp	short sysvideo_39_14
sysvideo_39_17:
	cmp	bh, 24
	ja	short sysvideo_39_24
	jb	short sysvideo_39_10  
	
	; 24 bits per pixel
	and	ecx, 0FFFFFFh
	cmp	bl, 1 ; 1 = write pixel
	je	short sysvideo_39_19
	ja	short sysvideo_39_22
sysvideo_39_18:
	; read pixel (24bpp)
	mov	eax, [edx]
	;and	eax, 0FFFFFFh
	;mov	[u.r0], eax
	;jmp	sysret
	jmp	short sysvideo_39_21
sysvideo_39_19:
	; write pixel (24bpp)
	mov	eax, ecx
sysvideo_39_20:
	;and	eax, 0FFFFFFh
	mov	[edx], eax
sysvideo_39_21:
	mov	[u.r0], eax
	jmp	sysret
sysvideo_39_22:
	cmp	bl, 3 ; mix
	jb	short sysvideo_39_23
	; mix  pixel colors (24bpp)
	mov	eax, [edx]
	and	eax, 0FFFFFFh
	;and	ecx, 0FFFFFFh
	add	eax, ecx
	rcr	eax, 1
	jmp	short sysvideo_39_20
sysvideo_39_23:
	 ; swap pixel colors (24bpp)
	mov	eax, ecx
	;and	eax, 0FFFFFFh
	xchg	[edx], ax
	ror	eax, 16
	mov	[edx+2], al
	rol	eax, 16
	jmp	short sysvideo_39_21

	; 32 bits per pixel
sysvideo_39_24:
	cmp	bl, 1 ; 1 = write pixel
	je	short sysvideo_39_26
	ja	short sysvideo_39_29
sysvideo_39_25:
	; read pixel (32bpp)
	mov	eax, [edx]
	;mov	[u.r0], eax
	;jmp	sysret
	jmp	short sysvideo_39_28
sysvideo_39_26:
	; write pixel (32bpp)
	mov	eax, ecx
sysvideo_39_27:
	mov	[edx], eax
sysvideo_39_28:
	mov	[u.r0], eax
	jmp	sysret
sysvideo_39_29:
	cmp	bl, 3 ; mix
	jb	short sysvideo_39_30
	; mix  pixel colors (32bpp)
	mov	eax, [edx]
	add	eax, ecx
	rcr	eax, 1
	jmp	short sysvideo_39_27
sysvideo_39_30:
	 ; swap pixel colors (32bpp)
	mov	eax, ecx
	xchg	[edx], eax
	jmp	short sysvideo_39_28

sysvideo_39_31:
	; 06/03/2021
	; 08/02/2021
	; 07/02/2021
	; BL = 4 -> read pixels from user defined positions
	; BL = 5 -> write single color pixels to user defined pos.
	; BL = 6 -> write multi color pixels to user defined pos.
	; ECX = color (CL, CX, ECX)
	; EDX = number of pixels
	; ESI = user buffer contains dword pixel positions
	;	 (and dword colors for BL input = 6)
	; EDI = user's pixel color buff (destination) for BL = 4

	mov	[maskcolor], ecx
	mov	ebp, edx ; number of pixels    
	cmp	byte [CRT_MODE], 0FFh ; SVGA flag
	jnb	short sysvideo_39_33 ; SVGA (VESA VBE mode)
	; Standard VGA mode
	mov	ecx, 65536 ; Video page size (maximum)
	cmp	edx, ecx
	ja	short sysvideo_39_32 ; abnormal value !
	mov	eax, 0A0000h ; Video page start address
	mov	bh, 8  ; 8 bits per pixel (256 colors)
	jmp	short sysvideo_39_34
sysvideo_39_32:
	; nonsense! (edx has abnormal value)
	jmp	sysret
sysvideo_39_33:
	; 06/03/2021
	mov	bh, [LFB_Info+LFBINFO.bpp]
	cmp	bh, 8
	je	short sysvideo_39_81 ; 8bpp
	mov	eax, edx
	cmp	bh, 16
	je	short sysvideo_39_80 ; 16bpp
	shl	edx, 1
	cmp	bh, 32
	jne	short sysvideo_39_80 ; 24bpp
	shl	eax, 1
sysvideo_39_80:
	add	edx, eax
	; edx = number of bytes
sysvideo_39_81:
	; get LFB address
	mov	eax, [LFB_ADDR] ; [LFB_Info+LFBINFO.LFB_addr]
	or	eax, eax
	jz	short sysvideo_39_32 ; LFB is not ready !
	mov	ecx, [LFB_SIZE]
	cmp	edx, ecx 
	ja	short sysvideo_39_32 ; abnormal value !

	; 02/03/2021
	; 08/02/2021
	;mov	ebp, edx ; pixel count  
	;shl	ebp, 2 ; byte count (pixel pos: 4 bytes)

	; 06/03/2021
	; bits per pixel (pixel color size)
	;mov	bh, [LFB_Info+LFBINFO.bpp]
sysvideo_39_34:
	shl	ebp, 2 ; 15/02/2021 (byte count)
	mov	[v_mem], eax ; Save video page start address
	mov	dh, bl ; sub function
	; 06/03/2021
	mov	[v_bpp], bh ; bits per pixel (color size)
	;mov	ebx, [LFB_SIZE]
	mov	ebx, ecx ; [LFB_SIZE]

	mov	ecx, 2048
	cmp	ebp, ecx
	jnb	short sysvideo_39_35
	mov	ecx, ebp ; fix to requested byte count
sysvideo_39_35:
	cmp	dh, 4 ; 08/02/2021
	;cmp	bl, 4 ; read pixels from user defined positions
	jna	short sysvideo_39_36
	jmp	sysvideo_39_52
	; 08/02/2021
	;mov	[buffer8], edi  ; user's destination buff addr
sysvideo_39_36:
	; 08/02/2021
	; read pixel positions
	; as defined in user's source buffer
	mov	[buffer8], edi  ; user's destination buff addr
	mov	edi, VBE3SAVERESTOREBLOCK ; kernel buffer for
					  ; 2028 byte data
	; esi = user's source buffer for pixel positions
	; ecx = byte count
	call	transfer_from_user_buffer
	jc	short sysvideo_39_32 ; error
	; ecx  = transfer count (bytes)

	push	edi ; *
	push	esi ; **
	push	ecx ; ***

	mov	esi, edi ; kernel buffer
	mov	edx, [v_mem] ; video memory
	shr	ecx, 2 ; pixel count (within buffer capacity)

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short sysvideo_39_49
sysvideo_39_37:
	; 8bpp
	lodsd
	cmp	eax, ebx ; < [LFB_SIZE]
	jnb	short sysvideo_39_39
	movzx	eax, byte [edx+eax]
sysvideo_39_38:
	stosd
	loop	sysvideo_39_37
	jmp	short sysvideo_39_50
sysvideo_39_39:
	; write black color for improper positions
	xor	eax, eax
	jmp	short sysvideo_39_38
sysvideo_39_40:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short sysvideo_39_47 ; 32bpp
	jb	short sysvideo_39_44 ; 16bpp
sysvideo_39_41:
	; 24bpp
	lodsd
	cmp	eax, ebx ; < [LFB_SIZE]
	jnb	short sysvideo_39_43
	mov	eax, [edx+eax]
	and	eax, 0FFFFFFh
sysvideo_39_42:
	stosd
	loop	sysvideo_39_41
	jmp	short sysvideo_39_50
sysvideo_39_43:
	; write black color for improper positions
	xor	eax, eax
	jmp	short sysvideo_39_42
sysvideo_39_44:
	; 16bpp
	lodsd
	cmp	eax, ebx ; < [LFB_SIZE]
	jnb	short sysvideo_39_46
	movzx	eax, word [edx+eax]
sysvideo_39_45:
	stosd
	loop	sysvideo_39_44
	jmp	short sysvideo_39_50
sysvideo_39_46:
	; write black color for improper positions
	xor	eax, eax
	jmp	short sysvideo_39_45
sysvideo_39_47:
	; 32bpp
	lodsd
	cmp	eax, ebx ; < [LFB_SIZE]
	jnb	short sysvideo_39_49
	movzx	eax, word [edx+eax]
sysvideo_39_48:
	stosd
	loop	sysvideo_39_47
	jmp	short sysvideo_39_50
sysvideo_39_49:
	; write black color for improper positions
	xor	eax, eax
	jmp	short sysvideo_39_48
sysvideo_39_50:
	pop	ecx ; transfer count in bytes
	pop	esi ; ** ; kernel buffer
 	;mov	esi, VBE3SAVERESTOREBLOCK ; kernel buffer for
					  ; 2048 byte data
	mov	edi, [buffer8]
	; edi = user's destination buffer for pixel colors
	; ecx = byte count
	call	transfer_to_user_buffer
	pop	esi ; *
	jc	short sysvideo_39_56 ; error
	; ecx  = transfer count (bytes)
	mov	eax, ecx
	shr	eax, 2
	add	[u.r0], eax ; transfer count (in pixels)
		
	sub	ebp, ecx
	jna	short sysvideo_39_56 ; completed/finished
	add	esi, ecx ; next position in source buffer
	;add	[buffer8], ecx ; next pos in destination buff
	add	edi, ecx
	mov	cx, 2048 ; new count, limit: kernel buff size
	cmp	ebp, ecx ; remain >= limit ?
	jnb	short sysvideo_39_51 ; yes
	mov	ecx, ebp ; fix byte count to remain bytes
sysvideo_39_51:
	jmp	sysvideo_39_36

sysvideo_39_52:
	cmp	dh, 5 ; 08/02/2021
	;cmp	bl, 5 ; write pixels to user defined positions 
	jna	short sysvideo_39_53
	jmp	sysvideo_39_66
sysvideo_39_53:
	; single color pixel writing
	mov	edi, VBE3SAVERESTOREBLOCK ; kernel buffer for
					  ; 2028 byte data
	; esi = user's source buffer for pixel positions
	; ecx = byte count
	call	transfer_from_user_buffer
	jc	short sysvideo_39_56 ; error
	; ecx = transfer count (bytes)

	; write pixels by using (user) defined positions
	; ecx = byte count (1,2,3,4 times pixel count)	
	; edi = system buffer address

	push	esi ; *
	push	ecx ; **

	mov	esi, edi
	mov	edi, [v_mem]

	; 08/02/2021
	shr	ecx, 2 ; pixel count
	mov	edx, [maskcolor]
	;mov	ebx, [v_siz]

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short sysvideo_39_57
sysvideo_39_54:
	; 8bpp
	lodsd
	cmp	eax, ebx ; < [v_siz]
	jnb	short sysvideo_39_55
	mov	[edi+eax], dl
	; 06/03/2021
	inc	dword [u.r0]
sysvideo_39_55:
	loop	sysvideo_39_54
	jmp	short sysvideo_39_64
sysvideo_39_56:
	jmp	sysret
sysvideo_39_57:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short sysvideo_39_62 ; 32bpp
	jb	short sysvideo_39_60 ; 16bpp
sysvideo_39_58:
	; 24bpp
	lodsd
	cmp	eax, ebx ; < [v_siz]
	jnb	short sysvideo_39_59
	mov	[edi+eax], dl
	inc	eax
	ror	edx, 8
	mov	[edi+eax], dx
	rol	edx, 8
	inc	dword [u.r0]
sysvideo_39_59:
	loop	sysvideo_39_58
	jmp	short sysvideo_39_64
sysvideo_39_60:
	; 16bpp
	lodsd
	cmp	eax, ebx ; < [v_siz]
	jnb	short sysvideo_39_61
	mov	[edi+eax], dx
	inc	dword [u.r0]
sysvideo_39_61:	
	loop	sysvideo_39_60
	jmp	short sysvideo_39_64
sysvideo_39_62:
	; 32bpp
	lodsd
	cmp	eax, ebx ; < [v_siz]
	jnb	short sysvideo_39_63
	mov	[edi+eax], edx
	inc	dword [u.r0]
sysvideo_39_63:
	loop	sysvideo_39_62
sysvideo_39_64:
	pop	ecx ; **
	pop	esi ; *	
	sub	ebp, ecx
	jna	short sysvideo_39_56
	add	esi, ecx
	mov	cx, 2048
	cmp	ebp, ecx
	jnb	short sysvideo_39_65
	mov	ecx, ebp
sysvideo_39_65:
	jmp	sysvideo_39_53

sysvideo_39_66:
	; 15/02/2021
	shl	ebp, 1 ; 8 bytes per pixel (position&color)
sysvideo_39_67:
	mov	cx, 2048
	cmp	ebp, ecx
	jnb	short sysvideo_39_68
	mov	ecx, ebp
sysvideo_39_68:
	; multi colors pixel writing
	mov	edi, VBE3SAVERESTOREBLOCK ; kernel buffer for
					  ; 2048 byte data
	; esi = user's source buffer for pixel positions
	; ecx = byte count
	call	transfer_from_user_buffer
	jc	short sysvideo_39_56 ; error
	; ecx  = transfer count
	
	; write pixels & colors as defined in user buffer
	; ecx = byte count (2,4,6,8 times pixel count)
	; edi = system buffer address

	push	esi ; **
	push	ecx ; *

	mov	esi, edi
	mov	edi, [v_mem]

	; 08/02/2021
	shr	ecx, 3 ; pixel count

	;mov	ebx, [v_siz]

	cmp	byte [v_bpp], 8 ; 8bpp
	ja	short sysvideo_39_71
sysvideo_39_69:
	; 8bpp
	lodsd	; position
	mov	edx, eax
	lodsd	; color
	cmp	edx, ebx ; < [v_siz]
	jnb	short sysvideo_39_70
	mov	[edi+edx], al
	; 06/03/2021
	inc	dword [u.r0]
sysvideo_39_70:
	loop	sysvideo_39_69
	jmp	short sysvideo_39_78
sysvideo_39_71:
	cmp	byte [v_bpp], 24 ; 24bpp
	ja	short sysvideo_39_76 ; 32bpp
	jb	short sysvideo_39_74 ; 16bpp
sysvideo_39_72:
	; 24bpp
	lodsd	; position
	mov	edx, eax
	lodsd	; color
	cmp	edx, ebx ; < [v_siz]
	jnb	short sysvideo_39_73
	mov	[edi+edx], al
	inc	edx
	shr	eax, 8
	mov	[edi+edx], ax
	inc	dword [u.r0]
sysvideo_39_73:	
	loop	sysvideo_39_72
	jmp	short sysvideo_39_78
sysvideo_39_74:
	; 16bpp
	lodsd	; position
	mov	edx, eax
	lodsd	; color
	cmp	edx, ebx ; < [v_siz]
	jnb	short sysvideo_39_75
	mov	[edi+edx], ax
	inc	dword [u.r0]
sysvideo_39_75:	
	loop	sysvideo_39_74
	jmp	short sysvideo_39_78
sysvideo_39_76:
	; 32bpp
	lodsd	; position
	mov	edx, eax
	lodsd	; color
	cmp	edx, ebx ; < [v_siz]
	jnb	short sysvideo_39_77
	mov	[edi+edx], eax
	inc	dword [u.r0]
sysvideo_39_77:	
	loop	sysvideo_39_76
sysvideo_39_78:
	pop	ecx ; *
	pop	esi ; **

	sub	ebp, ecx
	jna	short sysvideo_39_79
	add	esi, ecx
	jmp	sysvideo_39_67
;sysvideo_39_79:
;	jmp	sysret

sysvideo_16:
	; 11/08/2022
	; 23/07/2022
	; 06/03/2021
	; 23/11/2020
	cmp	bh, 4
	;jb	sysvideo_39 ; bh = 3, pixel r/w
	;ja	short sysvideo_17
	; 23/07/2022
	je	short sysvideo_16_0
	ja	short sysvideo_17
	jmp	sysvideo_39
sysvideo_16_0:
	; BH = 4
	; Direct User Access for CGA video memory.
	; Setup user's page tables for direct access to 0B8000h.
	;
	; Permission checks are not implemented yet !
	; (11/07/2016)

	mov	eax, 0B8000h
	;mov	ecx, 8 ; 8 pages (8*4K=32K)
	; 11/08/2022
	xor	ecx, ecx
	mov	cl, 8
	mov	ebx, eax ; 12/05/2017 ; virtual = physical
	call	direct_memory_access
	;jc	sysret
	jc	short sysvideo_39_79 ; 06/03/2021
	; eax = 0B8000h if there is not an error
	mov	[u.r0], eax
sysvideo_39_79: ; 08/01/2021
	jmp	sysret

sysvideo_17:
	; 23/07/2022
	; 23/12/2020
	; 11/12/2020
	; 10/12/2020
	; 23/11/2020
	cmp	bh, 6
	je	short sysvideo_17_0 ; 23/07/2022
	jb	short sysvideo_18 ; 23/07/2022
	jmp	sysvideo_20 ; ja

	; 23/07/2022
sysvideo_18:
	; BH = 5
	; Direct User Access for VGA video memory.
	; Setup user's page tables for direct access to 0A0000h.
	;
	; Permission checks are not implemented yet !
	; (11/07/2016)

	mov	eax, 0A0000h
	mov	ecx, 16 ; 16 pages (16*4K=64K)
	mov	ebx, eax ; 12/05/2017 ; virtual = physical
	call	direct_memory_access	
	;jc	sysret
	; 23/07/2022
	jc	short sysvideo_18_0
	; eax = 0A0000h if there is not an error
	mov	[u.r0], eax
sysvideo_18_0:
	jmp	sysret

sysvideo_17_0:
	; BH = 6
	; Direct User Access to Linear Frame Buffer.
	; Setup user's page tables for direct access to LFB.
	;
	; Permission checks are not implemented yet !
	; (10/12/2020)

	cmp	bl, 0FFh ; current video mode
	jb	short sysvideo_17_2 ; for desired video mode

	cmp	[CRT_MODE], bl ; VESA VBE video mode ?
	jne	short sysvideo_17_1
	mov	cx, [video_mode]
	and	cx, 1FFh
	jmp	short sysvideo_17_3
sysvideo_17_1:
	; 11/12/2020
	mov	bh, bl ; 0FFh
	mov	bl, [CRT_MODE] ; VGA/CGA video mode
	mov	ebp, [u.usp]  ; ebp points to user's registers
	; 23/12/2020
	mov	[ebp+16], ebx ; return to user with EBX value
	jmp	sysret ; return to user with EAX = 0
sysvideo_17_2:
	; bl = VESA video mode - 100h
	mov	bh, 1 ; bx = 1XXh
	push	ebx ; requested vesa video mode
	call	vbe_biosfn_return_current_mode
	pop	ecx ; requested vesa video mode
	and	bx, 1FFh
	cmp	cx, bx
	jne	short sysvideo_17_8
sysvideo_17_3:
	cmp	cx, [LFB_Info+LFBINFO.mode]
	jne	short sysvideo_17_8
sysvideo_17_4:
	; 11/12/2020
	mov	eax, [LFB_Info+LFBINFO.LFB_addr]
	; 21/12/2020
	or	eax, eax
	jz	short sysvideo_17_7
	;
	mov	ecx, [LFB_Info+LFBINFO.LFB_size] ; buff size
	mov 	ebx, eax ; user's address = physical address
	;push	ebx
	push	ecx
	; 21/12/2020
	add	ecx, 4095  ; PAGESIZE - 1
	; 14/12/2020
	shr	ecx, 12  ; convert bytes to pages
	call	direct_memory_access
	pop	edx  ; linear frame buffer size in bytes
	;pop	eax  ; linear frame buffer address (physical)
	jc	short sysvideo_17_7 ; [u.r0] = eax = 0
sysvideo_17_5:
	mov	cx, [LFB_Info+LFBINFO.Y_res] ; screen height
	shl	ecx, 16
	mov	cx,  [LFB_Info+LFBINFO.X_res] ; screen width
	xor	ebx, ebx
	mov	bl, [LFB_Info+LFBINFO.bpp] ; bits per pixel
	mov	bh, [LFB_Info+LFBINFO.mode] ; XX part of 1XXh
sysvideo_26_4: ; 23/12/2020	
	mov	ebp, [u.usp]  ; ebp points to user's registers
	mov	[ebp+20], edx ; return to user with EDX value
	mov	[ebp+16], ebx ; EBX
	mov	[ebp+24], ecx ; ECX
sysvideo_17_6:
	mov	[u.r0], eax ; LFB address
sysvideo_17_7:
	jmp	sysret 
sysvideo_17_8:
	; cx = mode
	; 21/12/2020
	or	ch, 40h  ; Linear frame buffer flag
	call	_vbe_biosfn_return_mode_info
	jc	short sysvideo_17_7
	jmp	short sysvideo_17_4

sysvideo_19:
	; 23/07/2022
	; 22/01/2021
	; 12/12/2020
	; 11/12/2020
	; 23/11/2020
	; BH = 7
	; Get (Super/Extended VGA) mode
	; and Linear Frame Buffer info.
	
	; 22/01/2021
	mov	bl, 0FFh
	; 11/12/2020
	;cmp	byte [CRT_MODE], 0FFh ; (extended mode?)
	; 22/01/2021
	cmp	[CRT_MODE], bl  ; 0FFh
	;jb	short sysvideo_17_1 ; not a VESA VBE mode
	; 23/07/2022
	; 12/12/2020
	jnb	short sysvideo_19_0
	; 23/07/2022
	jmp	sysvideo_17_1

sysvideo_19_0:
	call	vbe_biosfn_return_current_mode
	and	bx, 1FFh
	cmp	bx, [LFB_Info+LFBINFO.mode]
	jne	short sysvideo_19_2
sysvideo_19_1:
	mov	eax, [LFB_Info+LFBINFO.LFB_addr]
	mov	edx, [LFB_Info+LFBINFO.LFB_size]
	jmp	sysvideo_17_5
sysvideo_19_2:
	call	_vbe_biosfn_return_mode_info
	jnc	short sysvideo_19_1
	jmp	sysret

sysvideo_20_1:
	; cx = vesa video mode
	mov	ax, cx
	cmp	ax, 100h
	jb	short sysvideo_20_0 ; VGA/CGA mode
	cmp	ax, 1FFh
	;ja	short sysvideo_20_4 ; not valid
	ja	short sysvideo_20_3
	push	eax
	mov	bx, ax
	mov	ax, 4F02h

	; simulate _int10h (int 31h) for func 4F02h
	;pushfd
	;push	cs
	;push	sysvideo_20_1_retn 
	;push	es ; *
	;push	ds ; **	; SAVE WORK AND PARAMETER REGISTERS
	;jmp	VBE_func
;sysvideo_20_1_retn:

	call	_int10h ; simulate int 10h (int 31h)

	cmp	ax, 004Fh
	pop	eax
	jne	short sysvideo_20_3 ; error
	;pop	eax
	inc	eax
	mov	[u.r0], eax ; video mode + 1
	or	edx, edx ; is LFBINFO requested by user ?
	;jz	short sysvideo_20_4
	jz	short sysvideo_20_3 ; no

	; 11/12/2020
	; Check LFBINFO table/structure
	; (it is set by vbe2 'vbe_biosfn_set_mode'
	; but if vbe3 vbios pmi is in use,
	; it will not set LFBINFO table)

	push	edx
	dec	eax  ; video mode
	mov	esi, LFB_Info
	cmp	ax, [esi+LFBINFO.mode]
	je	short sysvideo_20_2

	call	_vbe_biosfn_return_mode_info
	;jnc	short sysvideo_20_2
	jc	short sysvideo_20_4 ; edx = 0

	;; clear LFBINFO table for invalidating	
	;mov	ecx, LFBINFO.size ; 16
	;mov	edi, esi ; LFB_Info table address
	;xor	eax, eax
	;rep	stosb

sysvideo_20_2:
	;pop	ecx
	;mov	edi, ecx ; user buffer
	pop	edi
	mov	ecx, LFBINFO.size ; 16
	call	transfer_to_user_buffer ; fast transfer
	jc	short sysvideo_20_5

	;jmp	sysret
sysvideo_20_3:	
	;pop	eax ; [u.r0] = 0
;sysvideo_20_4:
	jmp	sysret

	; 23/07/2022
sysvideo_20:
	; 11/12/2020
	; 23/11/2020
	cmp	bh, 8
	; 08/08/2022
	;jb	short sysvideo_19 ; video mode & lfb info
	je	short sysvideo_20_6
	; 23/07/2022
	ja	short sysvideo_21 ; 12/12/2020
	; 08/08/2022 - jb
	jmp	sysvideo_19
sysvideo_20_6:
	; BH = 8
	; Set (Super/Extended VGA) mode & return LFB info

	; 11/12/2020
	cmp	bl, 0FFh  ; CGA/VGA mode ?
	jnb	short sysvideo_20_1

	;xor	ah, ah
	mov	al, bl
sysvideo_20_0:
	call	_int10h  ; uses vbe3 pmi32 option
	cmp	eax, 0FFFFFFFFh ; -1
	je	short sysvideo_20_3 ; error
	
	; 11/12/2020
	; alternative (it does not use vbe3 pmi32)
	;push	eax
	;call	_set_mode
	;pop	eax
	;jc	short sysvideo_20_3

	;inc	eax
	inc	al
	;mov	[u.r0], ax ; video mode + 1
	mov	[u.r0], al
	jmp	sysret

sysvideo_20_4:
	pop	edx
sysvideo_20_5:
	xor	edx, edx ; 0
	; edx = 0 -> invalid LFBINFO data
	mov	ebp, [u.usp]  ; ebp points to user's registers
	mov	[ebp+20], edx ; return to user with EDX value
	jmp	sysret

sysvideo_21:
	; 23/07/2022
	; 04/01/2021
	; 03/12/2020
	cmp	bh, 10
	;jb	sysvideo_22 ; VESA VBE3 pmi parms
	; 23/07/2022
	jb	short sysvideo_21_19
	; 23/12/2020
	;je	sysvideo_26 ; Video memory mapping
	; 23/07/2022
	je	short sysvideo_21_20

	; 04/01/2020
	cmp	bh, 11
	;ja	sysvideo_27
	; 23/07/2022
	ja	short sysvideo_21_21
		
	; BH = 11
	; set/read DAC color registers (for 8bpp)

	cmp	bl, 4
	;jnb	sysvideo_21_7	; BMP file type palette
				; handling
	; 23/07/2022
	;jnb	short sysvideo_21_7
	; 08/08/2022
	jb	short sysvideo_21_18
	jmp	sysvideo_21_7
sysvideo_21_18:	
	test	bl, 1
	jnz	short sysvideo_21_4 ; set/write DAC colors
		
	; Read DAC color register or all DAC color registers
	test	bl, 2  ; read single DAC color register
	jz	short sysvideo_21_2 ; read all DAC color regs

	; read single DAC color register
	; CL = DAC color register (index)
 	
	mov	dx, 3C7h ; VGAREG_DAC_READ_ADDRESS
	mov	al, cl	; DAC color register
	xor	ecx, ecx ; (this may not be necessary)
	out	dx, al
	;mov	dx, 3C9h ; VGAREG_DAC_DATA
	mov	dl, 0C9h
	in	al, dx
	mov	ah, al	; red
	in	al, dx
	mov	cl, al	; green
	in	al, dx
	mov	ch, al	; blue
	shl	ecx, 8
	mov	cl, ah	; red
	; CL = Red, CH = Green, byte 3 = Blue, byte 4 = 0
sysvideo_21_0:
	mov	[u.r0], ecx
sysvideo_21_1:
	jmp	sysret
sysvideo_21_19:
	; 23/07/2022
	jmp	sysvideo_22
sysvideo_21_20:
	; 23/07/2022
	jmp	sysvideo_26
sysvideo_21_21:
	; 23/07/2022
	jmp	sysvideo_27
sysvideo_21_2:
	; read all DAC color registers
	mov	ebx, ecx ; user's buffer address
	mov	edi, VBE3STACKADDR
	mov	esi, edi
	;mov	ecx, 768 ; 256*3
	; 08/08/2022
	sub	ecx, ecx
	mov	ch, 3
	; ecx = 768 = 300h
	;push	ecx
	mov	dx, 3C7h ; VGAREG_DAC_READ_ADDRESS
	sub	al, al ; 0
	out	dx, al
	;mov	dx, 3C9h ; VGAREG_DAC_DATA
	mov	dl, 0C9h
sysvideo_21_3:
	in	al, dx
	stosb
	in	al, dx
	stosb
	in	al, dx
	stosb
	loop	sysvideo_21_3
	;pop	ecx
	; 18/08/2022
	mov	ch, 3
	; ecx = 768 = 300h

	mov	edi, ebx ; user's buffer address
	;mov	esi, VBE3STACKADDR
	;mov	ecx, 256*3 = 768
	call	transfer_to_user_buffer
	jc	short sysvideo_21_1
	;mov	[u.r0], ecx ; actual transfer count
	jmp	short sysvideo_21_0

sysvideo_21_4:
	; Set/Write DAC color register or all registers
	test	bl, 2 ; write/set single DAC color register
	jz	short sysvideo_21_5 ; set all DAC color regs

	; set single DAC color register
	; CL = DAC color register (index)
	; (byte 1 = Red, byte 2 = Green, byte 3 = Blue)

	mov	dx, 3C8h ; VGAREG_DAC_WRITE_ADDRESS
	mov	eax, ecx ; DAC color register (index)
	shr	ecx, 16  ; CL = green, AH = Red
	out	dx, al
	;mov	dx, 3C9h ; VGAREG_DAC_DATA
	inc	dl
	mov	al, ah	; Red
	out	dx, al
	mov	al, cl	; Green
	out	dx, al
	mov	al, ch	; Blue
	out	dx, al
	;rol	ecx, 8
	shl	ecx, 8	; 21/02/2021
	mov	cl, ah	; Red
		; ecx = 00BBGGRRh
	jmp	short sysvideo_21_0

	; 23/07/2022
sysvideo_21_7:	
	; BMP file type palette handling

	test	bl, 1
	;jnz	short sysvideo_21_12 ; set/write DAC colors
	; 08/08/2022
	jz	short sysvideo_21_16
	jmp	sysvideo_21_12

sysvideo_21_16:
	; Read DAC color register or all DAC color registers
	test	bl, 2  ; read single DAC color register
	jz	short sysvideo_21_10 ; read all DAC color regs

	; read single DAC color register
	; CL = DAC color register (index)
 	
	mov	dx, 3C7h ; VGAREG_DAC_READ_ADDRESS
	mov	al, cl	; DAC color register
	xor	ecx, ecx
	out	dx, al
	;mov	dx, 3C9h ; VGAREG_DAC_DATA
	mov	dl, 0C9h
	in	al, dx
	shl	al, 2
	mov	ch, al	; red
	in	al, dx
	shl	al, 2
	mov	cl, al	; green
	in	al, dx
	shl	al, 2
	; 21/02/2021
	shl	ecx, 8
	mov	cl, al	; blue
	; CL = Blue, CH = Green, byte 3 = Red, byte 4 = 0
sysvideo_21_8:
	mov	[u.r0], ecx
sysvideo_21_9:
	jmp	sysret

sysvideo_21_5:
	; write/set all DAC color registers
	mov	esi, ecx ; user's buffer address
	mov	edi, VBE3STACKADDR
	mov	ebx, edi
	mov	ecx, 768 ; 256*3
	call	transfer_from_user_buffer
	;jc	short sysvideo_21_1
	; 08/08/2022
	jnc	short sysvideo_21_17
	jmp	sysvideo_21_1
sysvideo_21_17:
	mov	[u.r0], ecx ; actual transfer count

	mov	esi, ebx ; VBE3STACKADDR
	mov	dx, 3C8h ; VGAREG_DAC_WRITE_ADDRESS
	sub	al, al ; 0
	out	dx, al
	;mov	dx, 3C9h ; VGAREG_DAC_DATA
	inc	dl
sysvideo_21_6:
	lodsb
	out	dx, al
	lodsb
	out	dx, al
	lodsb
	out	dx, al
	loop	sysvideo_21_6
	jmp	short sysvideo_21_9

sysvideo_21_10:
	; read all DAC color registers
	mov	ebp, ecx ; user's buffer address
	mov	edi, VBE3STACKADDR
	mov	esi, edi
	mov	ecx, 1024 ; 256*4
	push	ecx
	mov	dx, 3C7h ; VGAREG_DAC_READ_ADDRESS
	sub	al, al ; 0
	out	dx, al
	;mov	dx, 3C9h ; VGAREG_DAC_DATA
	mov	dl, 0C9h
sysvideo_21_11:
	xor	ebx, ebx
	in	al, dx	; Red
	shl	al, 2
	mov	bh, al
	in	al, dx	; Green
	shl	al, 2
	mov	bl, al
	in	al, dx	; Blue
	shl	al, 2
	shl	ebx, 8
	mov	eax, ebx ; 00RRGGBBh
	stosd	
	loop	sysvideo_21_11
	pop	ecx

	mov	edi, ebp ; user's buffer address
	;mov	esi, VBE3STACKADDR
	;mov	ecx, 1024 = 4*256
	call	transfer_to_user_buffer
	jc	short sysvideo_21_9
	;mov	[u.r0], ecx ; actual transfer count
	jmp	short sysvideo_21_8

sysvideo_21_12:
	; Set/Write DAC color register or all registers
	test	bl, 2 ; write/set single DAC color register
	jz	short sysvideo_21_13 ; set all DAC color regs

	; set single DAC color register
	; CL = DAC color register (index)
	; (byte 1 = Blue, byte 2 = Green, byte 3 = Red)

	mov	dx, 3C8h ; VGAREG_DAC_WRITE_ADDRESS
	mov	al, cl	; DAC color register (index)
	mov	ah, ch	; Blue
	shr	ecx, 16
	out	dx, al
	;mov	dx, 3C9h ; VGAREG_DAC_DATA
	inc	dl
	mov	al, ch	; Red
	shr	al, 2
	out	dx, al
	mov	al, cl	; Green
	shr	al, 2
	out	dx, al
	mov	al, ah	; Blue
	shr	al, 2
	out	dx, al
	;rol	ecx, 8
	shl	ecx, 8 ; 21/02/2021
	mov	cl, ah
	jmp	sysvideo_21_8 ; 08/08/2022

sysvideo_21_13:
	; write/set all DAC color registers
	mov	esi, ecx ; user's buffer address
	mov	edi, VBE3STACKADDR
	mov	ebx, edi
	mov	ecx, 1024 ; 256*4
	call	transfer_from_user_buffer
	jc	short sysvideo_21_15
	mov	[u.r0], ecx ; actual transfer count

	mov	esi, ebx ; VBE3STACKADDR
	mov	dx, 3C8h ; VGAREG_DAC_WRITE_ADDRESS
	sub	al, al ; 0
	out	dx, al
	;mov	dx, 3C9h ; VGAREG_DAC_DATA
	inc	dl
sysvideo_21_14:
	lodsd
	; byte 0 = Blue, byte 1 = Green, byte 2 = Red
	; 21/02/2021
	mov	ebx, eax ; BL = Blue, BH = Green
	ror	ebx, 8 ; BL = Green, BH = Red
	mov	al, bh
	shr	al, 2
	out	dx, al ; Red
	mov	al, bl
	shr	al, 2	
	out	dx, al ; Green
	rol	ebx, 8 ; BL = Blue
	mov	al, bl
	shr	al, 2
	out	dx, al ; Blue 
	loop	sysvideo_21_14
sysvideo_21_15:
	jmp	sysret

sysvideo_22:
	; 28/02/2021
	; 22/01/2021
	; 17/01/2021
	; 04/12/2020
	; 03/12/2020
	; BH = 9
	; Set/Get VESA VBE3 protected mode interface params

	; 22/01/2021
	;cmp	byte [vbe3], 3
	;jne	short sysvideo_25 ; not applicable if
				; vbe3 compatible video bios
				; is not detected by kernel
	cmp	bl, 2
	;ja	short sysvideo_25 ; bl > 2 not implemented
	; 17/01/2021
	ja	short sysvideo_22_0 ; srvs flag sub function
	;jb	short sysvideo_23

	; 21/01/2021
	cmp	byte [vbe3], 3
	;jne	short sysvideo_25 ; not applicable if
				; vbe3 compatible video bios
				; is not detected by kernel
	jne	short sysvideo_21_15 ; 28/02/2021
	
	cmp	bl, 1
	jna	short sysvideo_23

	mov	bl, [pmi32] ; Video bios 32 bit PMI functions
	jmp	short sysvideo_24

sysvideo_22_0:
	; 17/01/2021
	; save/restore video state user permission
	cmp	bl, 5
	ja	short sysvideo_22_2
	jb	short sysvideo_22_1
	; get srvs flag value/status
	mov	bl, [srvsf] ; 0 = disabled, 1 = enabled
	jmp	short sysvideo_22_3

sysvideo_22_1:
	; permission (root and multi tasking) check
	call	sysvideo_22_4
	jnc	short sysvideo_25 ; not permitted !
	; cf = 1
	sub	bl, 3 ; disable = 0, enable = 1
	; 22/01/2021
	mov	[srvsf], bl
	inc	bl ; 1 = disabled, 2 = enabled
	jmp	short sysvideo_22_3

sysvideo_22_2:
	cmp	bl, 6
	;ja	short sysvideo_25 ; invalid/unimplemented
	; 28/02/2021
	ja	short sysvideo_22_6
	; get VESA VBE number/status
	mov	ah, [vbe3] ; vbe3 = 3, vbe2 = 2, others = 0
	mov	al, [vbe2bios] ; bochs/qemu/vbox emulator status
	mov	[u.r0], ax
	jmp	short sysvideo_25

sysvideo_22_3:
	; 22/01/2021
	mov	bh, [srvso] ; state options (> 80h -> svga)
	mov	[u.r0], bx ; function result is return value 
	jmp	short sysvideo_25 

sysvideo_22_4:
	; 17/01/2021 - permission will be given by root only
	cmp	byte [multi_tasking], 0 ; in single user mode
	ja	short sysvideo_22_5
	; 19/01/2021
 	cmp	byte [u.uid], 1 ; ([u.uid] = 0 -> root)
sysvideo_22_5:
	; [multi_tasking] = 0 & [u.uid] = 0 -> CF = 1
	; otherwise -> CF = 0 
	retn

sysvideo_22_6:
	; 28/02/2021
	cmp	bl, 9
	ja	short sysvideo_25 ; invalid/unimplemented
	je	short sysvideo_22_9
	cmp	bl, 8
	jb	short sysvideo_22_7

	; BL = 8
	; Set default true color bpp to 24

	mov	bl, 24
	;mov	[truecolor], al	; 24bpp (RRGGBBh)
	;mov	[u.r0], al
	;jmp	short sysvideo_25
	jmp	short sysvideo_22_8

sysvideo_23:
	; 17/01/2021
	; permission (root and multi tasking) check
	call	sysvideo_22_4
	jnc	short sysvideo_25 ; not permitted !

	mov	[pmi32], bl ; 1 = enabled, 0 = disabled
sysvideo_24:
	inc	bl
sysvideo_22_10:	; 28/02/2021
	mov	[u.r0], bl ; function result is return value
sysvideo_25:
	jmp	sysret

sysvideo_22_7:
	; BL = 7
	; Set default true color bpp to 32
	; (it will set if [VBE3]=3)

	; Note: This sub function is used to set 24bpp
	; VESA VBE video modes to 32bpp.. because,
	; old hardware uses 24 bpp but new video hardware
	; uses 32bpp for same VESA VBE truecolor modes.
	; (For example: VBE mode 112h is 640*480, 24bpp but
	; new hardware uses/apply it as 640*480, 32bpp.)
	; So, TRDOS 386 v2.0.3 kernel will check [truecolor]
	; status is 32 bpp or not and it will change 24bpp
	; to 32bpp if default [truecolor] value is 32, for
	; same video mode number.

	cmp	byte [vbe3], 3
	jne	short sysvideo_25 ; Only applicable 
				  ; for VBE3 video hardware!
	mov	bl, 32
sysvideo_22_8:
	mov	[truecolor], bl	; 32bpp (00RRGGBBh)
	;mov	[u.r0], bl
	;jmp	short sysvideo_25
	jmp	short sysvideo_22_10

sysvideo_22_9:
	; BL = 9
	; Return default true color bpp
	mov	bl, [truecolor]
	jmp	short sysvideo_22_10
;sysvideo_22_10:
	;mov	[u.r0], bl
	;jmp	sysret

sysvideo_26:
	; 23/12/2020
	; BH = 10
	; Map video memory to user's buffer
	; (multiuser/owner r/w permisions are ignored
	;  for current TRDOS 386 version !)

	and	cx, ~4095  ; clear low 12 bits
	or	ecx, ecx ; start address of user's buffer
	jz	short sysvideo_25 ; error !

	cmp	bl, 1  ; VGA memory mapping ?
	je	short sysvideo_26_1
	ja	short sysvideo_26_2
sysvideo_26_0:
	; BL = 0 : CGA memory (0B8000h) map (32K)
	mov	eax, 0B8000h
	mov	ebx, 32768
	jmp	short sysvideo_26_3
sysvideo_26_1:
	; BL = 1 : VGA memory (0A0000h) map (64K)
	mov	eax, 0A0000h
	mov	ebx, 65536
	jmp	short sysvideo_26_3
sysvideo_26_2:
	; BL = 2 : SVGA memory (LFB) map to user's buffer
	cmp	byte [vbe3], 2  ; VESA VBE 2/3 vbios ready ?
	jb	short sysvideo_25  ; no, error !
	and	dx, ~4095 ; clear low 12 bits
	or	edx, edx  ; buffer size in bytes
	jz	short sysvideo_25  ; error
	mov	ebx, edx
	mov	eax, [LFB_ADDR] ; [LFB_Info+LFBINFO.LFB_addr]
	and	eax, eax
	jz	short sysvideo_26_5
				; (LFB parms are not set yet)
	cmp	ebx, [LFB_SIZE] ; [LFB_Info+LFBINFO.LFB_size]
	jna	short sysvideo_26_3
	mov	ebx, [LFB_SIZE]
sysvideo_26_3: 
	push	edx
	push	ebx	; buffer size in bytes
	push	ecx	; user's buffer address
	xchg	ebx, ecx
	shr	ecx, 12 ; convert buffer size to page count
	call	direct_memory_access
	pop	ecx	; user's buffer address
	pop	ebx	; buffer size
	pop	edx
	;jc	short sysvideo_25 ; error !
				  ; [u.r0] = 0
	; 28/02/2021
	jc	short sysvideo_27_0 ; error !

;sysvideo_26_4:
	;mov	ebp, [u.usp] ; ebp points to user's registers
	;mov	[ebp+20], edx ; return to user with EDX value
	;mov	[ebp+16], ebx ; EBX
	;mov	[ebp+24], ecx ; ECX
	; eax = physical address of video memory (LFB)
	;mov	[u.r0], eax
	;jmp	sysret
	jmp	sysvideo_26_4

sysvideo_26_5:
	mov	ax, [def_LFB_addr] ; default LFB for mode 118h
	; ah must be 0C0h or 0D0h or E0h
	; others are nonsence !?
 	or	ah, ah
	;jz	short sysvideo_25 ; invalid lfb addr or
			  	; it is not a vbe2 -bochs emu-
			  	; or vbe3 -real- video bios
	; 28/02/2021
	jz	short sysvideo_27_0 ; invalid LFB address
 
	cmp	ah, 0F0h  
	;jnb	short sysvideo_25 ; nonsence !?
	; 28/02/2021
	jnb	short sysvideo_27_0 ; nonsence !?

	shl	eax, 16
	;jz	short sysvideo_25 ; eax = 0 

	cmp	ebx, 1920*1080*4 ; maximum value of possible
				 ; buffer sizes
	jna	short sysvideo_26_3  ; buffer size is proper
	; resize buffer to fit 4GB limit
	mov	ebx, 1920*1080*4
	jmp	short sysvideo_26_3

sysvideo_27:
	; 23/07/2022
	; 16/02/2021
	; 18/01/2021
	cmp	bh, 12
	;ja	sysvideo_28 ; 19/01/2021
	; 23/07/2022
	jna	short sysvideo_27_24
	jmp	sysvideo_28

sysvideo_27_24:	; 23/07/2022
	; BH = 12
	; Font sub functions.
	; 12/02/2021
	; 11/01/2021
	; 10/01/2021
	;   BL = 0 : Disable system font overwrite
	;   BL = 1 : Enable system font overwrite
	;   BL = 2 : Read system font 8x8
	;   BL = 3 : Read system font 8x14
	;   BL = 4 : Read system font 8x16
	;   BL = 5 : Read user defined font 8x8
	;   BL = 6 : Read user defined font 8x16
	;   BL = 7 : Write system font 8x8
	;   BL = 8 : Write system font 8x14
	;   BL = 9 : Write system font 8x16
	;   BL = 10 : Write user defined font 8x8
	;   BL = 11 : Write user defined font 8x16
	;
	;  BL > 11 : invalid (not implemented)
	;
	; For BL = 1 to 11
	;   ECX = number of characters (<= 256)
	;   EDX = first character (ascii code in DL)
	;   ESI = user's buffer address
	;
	; Return: EAX = character count 

	cmp	bl, 11
	jna	short sysvideo_27_1
sysvideo_27_0:
	jmp	sysret ; not implemented yet !
sysvideo_27_1:
	mov	ax, 256
	or	bl, bl
 	jnz	short sysvideo_27_3
	
	; bl = 0
	; disable system font overwrite 

	and	byte [ufont], 7Fh ; clear bit 7
sysvideo_27_2:
	;mov	word [u.r0], 256  ; > 0 -> successful
	mov	[u.r0], eax ; 256
	jmp	short sysvideo_27_0
sysvideo_27_3:
	cmp	bl, 1
	ja	short sysvideo_27_4

	; bl = 1
	; enable system font overwrite 
	;	if [multi_tasking]= 0 and [u.uid] = 0

	;cmp	byte [multi_tasking], 0 
	;			; multi tasking enabled ?
	;ja	short sysvideo_27_0 ; yes
	;; 19/01/2021 
	;; system maintenance or single user mode
	;cmp	byte [u.uid], 0  ; root ?
	;ja	short sysvideo_27_0 ; no

	; 19/01/2021
	; multi tasking & root check
	call	sysvideo_22_4
	jnc	short sysvideo_27_0 ; not permitted

	; [multi_tasking]= 0 and [u.uid] = 0

	or	byte [ufont], 80h ; set bit 7
		
 	jmp	short sysvideo_27_2

sysvideo_27_4:
	or	ecx, ecx
	jz	short sysvideo_27_0
	and	edx, edx
	jz	short sysvideo_27_4_0
	;mov	ax, 256
	cmp	ecx, eax ; 256
	ja	short sysvideo_27_0
	dec	eax
	cmp	edx, eax ; 255
	ja	short sysvideo_27_0
	inc	eax
	sub	eax, edx ; 256 - DX
	cmp	eax, ecx
	jb	short sysvideo_27_0

sysvideo_27_4_0:
	mov	ebp, esi

	cmp	bl, 6
	ja	short sysvideo_27_13
	jb	short sysvideo_27_5
	; bl = 6
	test	byte [ufont], 16 ; 8x16 user font loaded ?
	jz	short sysvideo_27_0
	; read 8x16 user defined font
	mov	esi, VGAFONT16USER
	jmp	short sysvideo_27_6
sysvideo_27_5:
	cmp	bl, 4
	jb	short sysvideo_27_11
	ja	short sysvideo_27_9
	; bl = 4
	; read 8x16 system font
	mov	esi, vgafont16
sysvideo_27_6:
	; read 8x16 font
	;shl	dx, 4 ; * 16
	;shl	cx, 4 ; * 16 ; 16 bytes per char
	; 23/07/2022
	shl	edx, 4 ; * 16
	shl	ecx, 4 ; * 16
sysvideo_27_7:
	mov	edi, ebp
	;add	edi, edx ; 16/02/2021
	add	esi, edx
	; ecx = byte count
	; esi = source (in system memory)
	; edi = destination (in user memory)
	call	transfer_to_user_buffer
	jc	short sysvideo_27_8
	mov	[u.r0], ecx
sysvideo_27_8:
	jmp	sysret
sysvideo_27_9:
	; bl = 5
	test	byte [ufont], 8 ; 8x8 user font loaded ?
	jz	short sysvideo_27_8
	; read 8x8 user defined font
	mov	esi, VGAFONT8USER
sysvideo_27_10:
	; read 8x8 font
	;shl	dx, 3 ; * 8
	;shl	cx, 3 ; * 8 ; 8 bytes per char
	; 23/07/2022
	shl	edx, 3 ; * 8
	shl	ecx, 3 ; * 8
	jmp	short sysvideo_27_7

sysvideo_27_11:
	cmp	bl, 3  ; 8x14 system font
	jb	short sysvideo_27_12 ; 8x8 system font
	; bl = 3
	; read 8x14 system font
	;mov	al, 14
	;mul	dl
	;mov	dx, ax
	;push	edx
	;mov	ax, 14
	;mul	cx
	;mov	cx, ax
	;pop	edx
	call	sysvideo_27_14
	mov	esi, vgafont14
	jmp	short sysvideo_27_7
	
sysvideo_27_12:
	; bl = 2
	; read 8x8 system font
	mov	esi, vgafont8
	jmp	short sysvideo_27_10

sysvideo_27_13:
	; overwrite font
	cmp	bl, 10
	ja	short sysvideo_27_22 ; 8x16 user font
	jb	short sysvideo_27_15
	; bl = 10
	mov	edi, VGAFONT8USER
	test	byte [ufont], 8 ; 8x8 user font loaded ?
	jnz	short sysvideo_27_21 ; yes
	or	ch, ch ; cx = 256
	;jnz	short sysvideo_27_21 ; 256 chars
	jz	short sysvideo_27_13_0
	mov	cx, 8*256
	jmp	short sysvideo_27_18_0
sysvideo_27_13_0:
	; copy system font to user font before overwrite
	mov	esi, vgafont8
	;push	edi
	;push	ecx
	;mov	cl, 64
	;rep	movsd
	;pop	ecx
	;pop	edi
	;mov	esi, ebp ; user's font buffer
	call	sysvideo_27_23
	jmp	short sysvideo_27_21

sysvideo_27_15:
	; check system font overwrite permission
	test	byte [ufont], 80h
	jz	short sysvideo_27_8

	cmp	bl, 8
	ja	short sysvideo_27_22 ; 8x16 system font
	jb	short sysvideo_27_20 ; 8x8 system font
	; bl = 8
	; overwrite 8x14 system font
	;mov	al, 14
	;mul	dl
	;mov	dx, ax
	;push	edx
	;mov	ax, 14
	;mul	cx
	;mov	cx, ax
	;pop	edx
	call	sysvideo_27_14
	mov	edi, vgafont14
	jmp	short sysvideo_27_18
sysvideo_27_16:
	; bl = 9
	; overwrite 8x16 system font
	mov	edi, vgafont16
sysvideo_27_17:
	; overwrite 8x16 font
	;shl	dx, 4 ; * 16
	;shl	cx, 4 ; * 16 ; 16 bytes per char
	; 23/07/2022
	shl	edx, 4 ; * 16
	shl	ecx, 4 ; * 16
sysvideo_27_18:
	add	edi, edx
	;add	esi, edx ; 16/02/2021
sysvideo_27_18_0:
	; ecx = byte count
	; esi = source (in user memory)
	; edi = destination (in system memory)
	call	transfer_from_user_buffer
	jc	short sysvideo_27_19
	mov	[u.r0], ecx
sysvideo_27_19:
	jmp	sysret
sysvideo_27_20:
	; bl = 7
	; overwrite 8x8 system font
	mov	edi, vgafont8
sysvideo_27_21:
	; overwrite 8x8 font
	;shl	dx, 3 ; * 8
	;shl	cx, 3 ; * 8 ; 8 bytes per char
	; 23/07/2022
	shl	edx, 3 ; * 8
	shl	ecx, 3 ; * 8
	jmp	short sysvideo_27_18
sysvideo_27_22:
	; bl = 11
	; overwrite 8x16 user defined font
	mov	edi, VGAFONT16USER
	test	byte [ufont], 16 ; 8x16 user font loaded ?
	jnz	short sysvideo_27_17 ; yes
	or	ch, ch ; cx = 256
	;jnz	short sysvideo_27_17 ; 256 chars
	jz	short sysvideo_27_22_0
	mov	cx, 16*256
	jmp	short sysvideo_27_18_0
sysvideo_27_22_0:
	; copy system font to user font before overwrite
	mov	esi, vgafont16
	;push	edi
	;push	ecx
	;mov	cl, 64
	;rep	movsd
	;pop	ecx
	;pop	edi
	;mov	esi, ebp ; user's font buffer
	call	sysvideo_27_23
	jmp	short sysvideo_27_17

sysvideo_27_14:
	; 16/02/2021
	push	edx
	mov	ax, 14
	mul	cx
	mov	ecx, eax
	pop	edx
	mov	al, 14
	mul	dl
	mov	edx, eax
	retn

	;mov	al, 14
	;mul	dl
	;mov	dx, ax
	;push	edx
	;; 12/02/2021
	;mov	ax, 14
	;;mov	eax, 14
	;;mul	cx
	;mul	ecx
	;;mov	cx, ax
	;mov	ecx, eax
	;pop	edx
	;retn

sysvideo_27_23:
	push	edi
	push	ecx
	mov	cl, 64
	rep	movsd
	pop	ecx
	pop	edi
	mov	esi, ebp ; user's font buffer
	retn

sysvideo_28:
	; 23/07/2022
	; 24/01/2021
	; 23/01/2021
	; 18/01/2021
	cmp	bh, 14
	;jb	sysvideo_29
	; 23/07/2022
	jb	short sysvideo_28_29
	;ja	sysvideo_30
	; 23/07/2022
	ja	short sysvideo_28_30

	; BH = 14
	; Save/Restore Super VGA video state
	
	; BL = options
	;	bit 0 - Save (0) or Restore (1)	
	;	bit 1 - controller hardware state
	;	bit 2 - BIOS data state
	;	bit 3 - DAC state
	;	bit 4 - (extended) Register state
	;	bit 5 - system (0) or user (1) memory
	;	bit 6 - verify without transfer
	;	bit 7 - not used (must be 0)

	; ECX = Buffer address or VideoStateID

	cmp	byte [vbe3], 2 ;  VESA VBE2 or VBE3 ?
	ja	short sysvideo_28_0 ; yes
	jb	short sysvideo_28_16 ; not a SVGA sys !

	; == VBE2 ==
	; Check Bochs/Qemu/VirtualBox PC emulator
	; (vbe2 is usable only for emulator's vbios)
	mov	ah, [vbe2bios]
	cmp	ah, 0C0h
	jb	short sysvideo_28_16 ; unknown vbios !
	cmp	ah, 0C5h
	jna	short sysvideo_28_0
		; Use kernel's vbios functions (video.s)
sysvideo_28_16:
	; unknown vbios !
	jmp	sysret

sysvideo_28_29:
	; 23/07/2022
	jmp	sysvideo_29
sysvideo_28_30:
	; 23/07/2022
	jmp	sysvideo_30

sysvideo_28_0:
	cmp	bl, 7Fh
	ja	short sysvideo_28_16 ; unknown options

	mov	dl, bl
	and	dl, 1Fh 
	shr	dl, 1
	jz	short sysvideo_28_16 ; invalid !
	; DL = VBE Function 4F04h Save/Restore options
	;  bit 0 : controller hardware state
	;  bit 1 : BIOS data state
	;  bit 2 : DAC state
	;  bit 3 : (extended) Register state
	
	test	bl, 32 ; bit 5
	;jnz	sysvideo_28_7 ; user buffer
	; 23/07/2022
	jz	short sysvideo_28_17
	jmp	sysvideo_28_7

sysvideo_28_17:	 ; 23/07/2022
	; source or destination is kernel/system buffer

	cmp	byte [srvsf], 0 ; srs permission flag
	jna	short sysvideo_28_16 ; not permitted

	test	bl, 1
	jz	short sysvideo_28_4 ; Save

	; Restore
	cmp	ecx, [VideoStateID]
	jne	short sysvideo_28_16 ; not correct ID !

	movzx	ecx, dl
	or	dl, 80h
	cmp	dl, [srvso]
	jne	short sysvideo_28_16 ; not correct !

	mov	dl, bl

	; ecx = cl = options
	call	vbe_srs_gbs
	; ebx = state buffer size (data size)

	mov	[u.r0], ebx

	test	dl, 64 ; verify without transfer
	jnz	short sysvideo_28_16 ; yes

	mov	esi, VBE3VIDEOSTATE
	mov	edi, VBE3SAVERESTOREBLOCK
	xchg	ecx, ebx
	rep	movsb

	mov	cl, bl

	; 23/01/2021
	jmp	short sysvideo_28_10

sysvideo_28_4:
	push	ebx
	; 24/01/2021
	xor	ebx, ebx ; 0 ; use kernel's buffer
	mov	[srvso], bl ; 0 ; invalidate
	mov	[VideoStateID], ebx ; 0 ; invalidate
	movzx	ecx, dl ; options
	mov	dl, 1 ; save state
	call	sysvideo_28_11 ; 23/01/2021
	; Note: VBE3 BIOS data save option will be
	;	disabled.. ; 24/01/2021
	mov	edx, ecx ; state (save) options
	pop	ebx

	cmp	ax, 4Fh ; successful ?
	jne	short sysvideo_28_3 ; no !

	test	bl, 64 ; verify without transfer
	jnz	short sysvideo_28_6 ; yes

	; ecx = cl = options
	call	vbe_srs_gbs
	; ebx = state buffer size (data size)

	mov	esi, VBE3SAVERESTOREBLOCK
	mov	edi, VBE3VIDEOSTATE
	mov	ecx, ebx
	rep	movsb

	mov	cl, dl
	or	cl, 80h ; SVGA (VESA VBE) flag
	;mov	[srvso], dl

	jmp	sysvideo_28_15

	; 23/01/2021
sysvideo_28_10:
	; CL = VESA VBE3 Save/Restore options

	mov	dl, 2 ; restore state

	call	sysvideo_28_1

	cmp	ax, 4Fh ; successful ?
	je	short sysvideo_28_3
	;jmp	short sysvideo_28_9

sysvideo_28_9:
	; return zero size (error) to user
	sub	eax, eax
sysvideo_28_5:
	mov	[u.r0], eax
sysvideo_28_3:
	jmp	sysret

sysvideo_28_6:
	; use timer ticks as VideoStateID
	mov	eax, [TIMER_LH]
	or 	eax, eax
	jnz	short sysvideo_28_5
	inc	eax
	jmp	short sysvideo_28_5

sysvideo_28_7:
	; save/restore to/from user buffer

	; 23/01/2021
	mov	esi, ecx ; user's vstate buffer
	mov	edi, VBE3SAVERESTOREBLOCK

	movzx	ecx, dl ; VESA VBE func 4F04h options

	; source or destination is user buffer
	test	bl, 1
	jz	short sysvideo_28_12  ; Save

	; Restore
	cmp	byte [srvsf], 0 ; srs permission flag
	jna	short sysvideo_28_14 ; not permitted

	mov	dl, bl ; 'sysvideo' options

	; ecx = cl = options
	call	vbe_srs_gbs
	; ebx = state buffer size (data size)

	mov	[u.r0], ebx ; transfer count

	test	dl, 64 ; verify without transfer
	jnz	short sysvideo_28_14 ; yes

	cmp	bx, 2048
	jnb	short sysvideo_28_9 ; invalid

	xchg	ecx, ebx
	; esi = user buffer
	; edi = VBE3SAVERESTOREBLOCK

	call	transfer_from_user_buffer
	jc	short sysvideo_28_9 ; error 

	mov	ecx, ebx ; Function 4F04h options
	jmp	short sysvideo_28_10 ; 23/01/2021

sysvideo_28_1:
	xor	ebx, ebx ; 0 ; use kernel's buffer
sysvideo_28_11:
	; 24/01/2021
	cmp	byte [vbe3], 3
	je	short sysvideo_28_2

	; VESA VBE2 (BOCHS/QEMU/VBOX) video bios
	jmp	_vbe_biosfn_save_restore_state
sysvideo_28_2:
	;24/01/2021
	;mov	eax, 4F04h  ; Save/Restore vstate
	; VESA VBE3 video bios
	jmp	_vbe3_pmfn_save_restore_state

sysvideo_28_12:
	; Save
	;mov	edi, VBE3SAVERESTOREBLOCK

	;movzx	ecx, dl ; options
	push	esi
	push	ebx
	; 23/01/2021
	mov	dl, 1 ; save state
	call	sysvideo_28_1
	pop	edx ; 'sysvideo' options
	pop	edi ; user's video state buffer

	cmp	ax, 4Fh ; successful ?
	jne	short sysvideo_28_14 ; no !

	; ecx = cl = options
	call	vbe_srs_gbs
	; ebx = state buffer size (data size)

	mov	ecx, ebx ; transfer count

	test	dl, 64 ; verify without transfer
	jnz	short sysvideo_28_13 ; yes
		
	;mov	edi, esi
	mov	esi, VBE3SAVERESTOREBLOCK
	call	transfer_to_user_buffer
	jc	short sysvideo_28_14
sysvideo_28_13:
	mov	[u.r0], ecx
sysvideo_28_14:
	jmp	sysret

sysvideo_29:
	; 18/01/2021
	; BH = 13
	; Save/Restore std VGA video state

	; bl = 0..3
	;	save to or restore from
	;	system buffer, VBE3VIDEOSTATE
	;	ECX = VideoStateID for restoring
	; bl = 4..7
	;	save to or restore from
	;	user buffer pointed by ECX

	cmp	bl, 3
	ja	short sysvideo_29_6

	; source or destination is kernel/system buffer

	cmp	byte [srvsf], 0 ; srs permission flag
	jna	short sysvideo_29_5 ; not permitted

	test	bl, 1
	jz	short sysvideo_29_2  ; Save

	; Restore
	cmp	ecx, [VideoStateID]
	jne	short sysvideo_29_5 ; not correct ID !
	cmp	bl, 1
	ja	short sysvideo_29_0
	; bl = 1
	mov	ebx, 110 
	mov	cl, 3 ; ctrl, vbios data
	jmp	short sysvideo_29_1
sysvideo_29_0:
 	; bl  = 3
	mov	ebx, 882 
	mov	cl, 7 ; ctrl, vbios data, dac
sysvideo_29_1:
	cmp	cl, [srvso]
	jne	short sysvideo_29_5 ; not correct !

	mov	esi, VBE3VIDEOSTATE ; 22/01/2021
	call	biosfn_restore_video_state
	mov	[u.r0], ebx ; video state size (bytes)
	;jmp	sysret
	jmp	short sysvideo_29_5
sysvideo_29_2:
	;mov	esi, ecx
	mov	edi, VBE3VIDEOSTATE

	mov	cl, 7 ; ctrl, vbios data, dac
	or	bl, bl
	jnz	short sysvideo_29_3 ; bl = 2
	; bl = 0
	mov	cl, 3 ; ctrl, vbios data
sysvideo_29_3:
	call	biosfn_save_video_state
sysvideo_28_15:
	; use timer ticks as VideoStateID
	mov	eax, [TIMER_LH]
	and 	eax, eax
	jnz	short sysvideo_29_4
	inc	eax
sysvideo_29_4:
	mov	[srvso], cl
	mov	[VideoStateID], eax
	mov	[u.r0], eax
sysvideo_29_5:
	jmp	sysret

sysvideo_29_6:
	cmp	bl, 7
	ja	short sysvideo_29_5 ; invalid sub function 

	mov	esi, ecx
	mov	edi, VBE3SAVERESTOREBLOCK

	; source or destination is user buffer
	test	bl, 1
	jz	short sysvideo_29_9  ; Save

	; Restore
	cmp	byte [srvsf], 0 ; srs permission flag
	jna	short sysvideo_29_5 ; not permitted
	
	;mov	esi, ecx
	;mov	edi, VBE3SAVERESTOREBLOCK
	
	cmp	bl, 7
	je	short sysvideo_29_7
	; bl = 5 
	mov	bl, 3
	mov	ecx, 110
	jmp	short sysvideo_29_8
sysvideo_29_7:
	; bl = 7
	mov	ecx, 882
sysvideo_29_8:
	call	transfer_from_user_buffer
	jc	short sysvideo_29_5
	mov	[u.r0], ecx
	mov	cl, bl ; mov cl,7 (mov cl,3)
	mov	esi, edi ; VBE3SAVERESTOREBLOCK
	; cl = 3 or 7
	call	biosfn_restore_video_state
	jmp	sysvideo_29_5
	;jmp	sysret
sysvideo_29_9:
	; Save
	;mov	edi, VBE3SAVERESTOREBLOCK

	cmp	bl, 6
	je	short sysvideo_29_10
	; bl = 4
	mov	ebx, 110
	mov	cl, 3 ; ctrl, vbios data
	jmp	short sysvideo_29_11
sysvideo_29_10:
	; bl = 6
	mov	ebx, 882
	mov	cl, 7 ; ctrl, vbios data, dac
sysvideo_29_11:
	call	biosfn_save_video_state

	mov	ecx, ebx ; transfer count
	mov	edi, esi
	mov	esi, VBE3SAVERESTOREBLOCK

	;call	transfer_to_user_buffer
	;jc	short sysvideo_29_5
	;mov	[u.r0], ecx ; transfer count
	;;jmp	sysret
	;jmp	short sysvideo_29_5
 
	jmp	short sysvideo_29_12

sysvideo_30:
	cmp	bh, 15
	ja	short sysvideo_31 ; invalid function

	; BH = 15
	; Copy VESA EDID to user's buffer

	cmp	byte [edid], 4Fh
	jne	short sysvideo_31 ; not ready !

	;and	ecx, ecx
	;jz	short sysvideo_31

	; ecx = user's buffer address
	mov	edi, ecx
	mov	esi, edid_info
	mov	ecx, 128 ; 128 bytes
sysvideo_29_12:
	call	transfer_to_user_buffer
	jc	short sysvideo_31

	mov	[u.r0], ecx ; EDID size, 128 bytes
sysvideo_31:
	jmp	sysret

mkdir:
	; 04/12/2015 (14 byte directory names)
	; 12/10/2015
	; 17/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 29/04/2013 - 01/08/2013 (Retro UNIX 8086 v1)
	;
	; 'mkdir' makes a directory entry from the name pointed to
	; by u.namep into the current directory.
	;
	; INPUTS ->
	;    u.namep - points to a file name 
	;	           that is about to be a directory entry.
	;    ii - current directory's i-number.	
	; OUTPUTS ->
	;    u.dirbuf+2 - u.dirbuf+10 - contains file name.
	;    u.off - points to entry to be filled
	;	     in the current directory
	;    u.base - points to start of u.dirbuf.
	;    r1 - contains i-number of current directory
	;
	; ((AX = R1)) output
	;
	;    (Retro UNIX Prototype : 11/11/2012, UNIXCOPY.ASM)
        ;    ((Modified registers: EAX, EDX, EBX, ECX, ESI, EDI, EBP))
	;

	; 17/06/2015 - 32 bit modifications (Retro UNIX 386 v1)
	xor 	eax, eax
	mov     edi, u.dirbuf+2
	mov	esi, edi
	stosd
	stosd
	; 04/12/2015 (14 byte directory names)
	stosd
	stosw
		; jsr r0,copyz; u.dirbuf+2; u.dirbuf+10. / clear this
	mov	edi, esi ; offset to u.dirbuf
	; 12/10/2015 ([u.namep] -> ebp)
	;mov 	ebp, [u.namep]
	call	trans_addr_nmbp ; convert virtual address to physical
		; esi = physical address (page start + offset)
		; ecx = byte count in the page (1 - 4096)
	; edi = offset to u.dirbuf (edi is not modified in trans_addr_nm)
		; mov u.namep,r2 / r2 points to name of directory entry
		; mov $u.dirbuf+2,r3 / r3 points to u.dirbuf+2
mkdir_1: ; 1:
	inc	ebp ; 12/10/2015
	;
	; / put characters in the directory name in u.dirbuf+2 - u.dirbuf+10
	 ; 01/08/2013
	lodsb
		; movb (r2)+,r1 / move character in name to r1
	and 	al, al
	jz 	short mkdir_3
		; beq 1f / if null, done
	cmp	al, '/'
		; cmp r1,$'/ / is it a "/"?
	je	short mkdir_err
	;je	error
		; beq error9 / yes, error
	; 12/10/2015
	dec	cx
	jnz	short mkdir_2
	; 12/10/2015 ([u.namep] -> ebp)
	call	trans_addr_nm ; convert virtual address to physical
		; esi = physical address (page start + offset)
		; ecx = byte count in the page
	; edi = offset to u.dirbuf (edi is not modified in trans_addr_nm)
mkdir_2:
	cmp     edi, u.dirbuf+16 ; ; 04/12/2015 (10 -> 16)
		; cmp r3,$u.dirbuf+10. / have we reached the last slot for
				     ; / a char?
	je	short mkdir_1
		; beq 1b / yes, go back
	stosb
		; movb r1,(r3)+ / no, put the char in the u.dirbuf
	jmp 	short mkdir_1
		; br 1b / get next char
mkdir_err:
	; 17/06/2015
	mov	dword [u.error], ERR_NOT_DIR ; 'not a valid directory !'
	jmp	error

mkdir_3: ; 1:
	mov	eax, [u.dirp]
	mov	[u.off], eax
		; mov u.dirp,u.off / pointer to empty current directory
				 ; / slot to u.off
wdir: ; 29/04/2013
        mov     dword [u.base], u.dirbuf
		; mov $u.dirbuf,u.base / u.base points to created file name
        mov     dword [u.count], 16 ; 04/12/2015 (10 -> 16) 
		; mov $10.,u.count / u.count = 10
	mov	ax, [ii] 
		; mov ii,r1 / r1 has i-number of current directory
	mov	dl, 1 ; owner flag mask ; RETRO UNIX 8086 v1 modification !
	call 	access
		; jsr r0,access; 1 / get i-node and set its file up
				 ; / for writing
	; AX = i-number of current directory
	; 01/08/2013
	inc     byte [u.kcall] ; the caller is 'mkdir' sign
	call	writei
		; jsr r0,writei / write into directory
	retn
		; rts r0

sysexec:
	; 21/08/2024 - TRDOS 386 v2.0.9
	; 23/07/2022 - TRDOS 386 v2.0.5
	; 06/02/2022 - Retro UNIX 386 v1.2
	; 18/11/2017
	; 14/11/2017
	; 13/11/2017
	; 24/10/2016 - 04/01/2017
	; 24/04/2016 - TRDOS 386 (TRDOS v2.0)
	; 23/06/2015 - 23/10/2015 (Retro UNIX 386 v1)
	; 03/06/2013 - 06/12/2013 (Retro UNIX 8086 v1)
	;
	; 'sysexec' initiates execution of a file whose path name if
	; pointed to by 'name' in the sysexec call. 
	; 'sysexec' performs the following operations:
	;    1. obtains i-number of file to be executed via 'namei'.
	;    2. obtains i-node of file to be exceuted via 'iget'.
	;    3. sets trap vectors to system routines.
	;    4. loads arguments to be passed to executing file into
	;	highest locations of user's core
	;    5. puts pointers to arguments in locations immediately
	;	following arguments.
	;    6.	saves number of arguments in next location.
	;    7. initializes user's stack area so that all registers
	;	will be zeroed and the PS is cleared and the PC set
	;	to core when 'sysret' restores registers
	;	and does an rti.
	;    8. initializes u.r0 and u.sp
	;    9. zeros user's core down to u.r0
	;   10.	reads executable file from storage device into core
	;	starting at location 'core'.
	;   11.	sets u.break to point to end of user's code with
	;	data area appended.
	;   12.	calls 'sysret' which returns control at location
	;	'core' via 'rti' instruction.
	;
	; Calling sequence:
	;	sysexec; namep; argp
	; Arguments:
	;	namep - points to pathname of file to be executed
	;	argp  - address of table of argument pointers
	;	argp1... argpn - table of argument pointers
	;	argp1:<...0> ... argpn:<...0> - argument strings
	; Inputs: (arguments)
	; Outputs: -	
	; ...............................................................
	;
	; Retro UNIX 386 v1 modification:
	;	User application runs in it's own virtual space 
	;	which is izolated from kernel memory (and other
	;	memory pages) via 80386	paging in ring 3 
	;	privilige mode. Virtual start address is always 0.
	;	User's core memory starts at linear address 400000h
	;	(the end of the 1st 4MB).
	;
	; Retro UNIX 8086 v1 modification:
	;	user/application segment and system/kernel segment
	;	are different and sysenter/sysret/sysrele routines
	;	are different (user's registers are saved to 
	;	and then restored from system's stack.)
	;
	;	NOTE: Retro UNIX 8086 v1 'arg2' routine gets these
	;	      arguments which were in these registers;
	;	      but, it returns by putting the 1st argument
	;	      in 'u.namep' and the 2nd argument
	;	      on top of stack. (1st argument is offset of the
	;	      file/path name in the user's program segment.)
	
	;call	arg2
	; * name - 'u.namep' points to address of file/path name
	;          in the user's program segment ('u.segmnt')
	;          with offset in BX register (as sysopen argument 1).
	; * argp - sysexec argument 2 is in CX register 
	;          which is on top of stack.
	;
		; jsr r0,arg2 / arg0 in u.namep,arg1 on top of stack

	; 23/06/2015 (32 bit modifications)

	;; 13/11/2017
	;;mov	[u.namep], ebx ; argument 1
        ; 18/10/2015
	mov     [argv], ecx  ; * ; argument 2

	; 13/11/2017
	mov	esi, ebx
	call	set_working_path_x
	jnc	short sysexec_0

	;; 'bad command or file name'
	;mov	eax, ERR_BAD_CMD_ARG ; 01h ; TRDOS 8086
	
	; 'file not found !' error
	mov	eax, ERR_NOT_FOUND ; 02h ; TRDOS 8086
sysexec_not_found_err:
sysexec_access_error:
sysexec_ext_error:
	mov	[u.r0], eax
	mov	[u.error], eax
	call 	reset_working_path
	jmp	error

sysexec_0:
	; 13/11/2017
	;mov	esi, FindFile_Name
        mov	ax, 1800h ; Only files
	call	find_first_file
	jc	short sysexec_not_found_err ; eax = 2

	; check_ file attributes
	; (attribute bits = 00ADVSHR) ; 18h = Directory+Volume
	; BL = Attributes byte
	
        test	bl, 6  ; system file or hidden file (S+H) 
	;jz	short sysexec_0ext
	jz	short sysexec_1 ; yes

	; 13/11/2017 
	; /// TRDOS386 permission check for multiuser mode ///
	; SYSTEM file or HIDDEN file !!
	; (Only super user has permission to run this file.)
	
	; ([u.uid]=0 for super user or root in multiuser mode)
	; ([u.uid]=0 for any users in singleuser mode)
	cmp 	byte [u.uid], 0 ; Super User ([u.uid]=0) ?
	;jna	short sysexec_0ext
	jna	short sysexec_1 ; yes

	; 'permission denied !' error
        mov	eax, ERR_FILE_ACCESS  ; 11 = ERR_PERM_DENIED
        jmp	short sysexec_access_error

sysexec_not_exf:
	; 'not executable file !' error
	mov	eax, ERR_NOT_EXECUTABLE
	jmp	sysexec_ext_error

;sysexec_0ext:
sysexec_1:
	; 18/11/2017
	mov	esi, FindFile_Name
	; 13/11/2017
	; check program file name extension
	; ('.PRG' for current TRDOS version)
	call	check_prg_filename_ext
	jc	short sysexec_not_exf
	
	; 18/11/2017
	cmp	al, 'P'
	jne	short sysexec_not_exf

	; '.PRG' extension is OK.
	; Only '.PRG' files are valid program files
	; for current TRDOS 386 version.

	mov	edx, [FindFile_DirEntry+DirEntry_FileSize]
	mov	ax, [FindFile_DirEntry+DirEntry_FstClusHI]
	shl	eax, 16
	mov	ax, [FindFile_DirEntry+DirEntry_FstClusLO]
	; EAX = First Cluster number
	; EDX = File Size

	mov	[ii], eax
	mov	[i.size], edx

;sysexec_1:
	; 23/07/2022 - TRDOS 386 Kernel v2.0.5
	; 06/02/2022 - Retro UNIX 386 v1.2
	; 13/11/2017 - TRDOS 386 (TRDOS v2.0)
	; 24/06/2015 - 23/10/2015 (Retro UNIX 386 v1)
        ; Moving arguments to the end of [u.upage]
	; (by regarding page borders in user's memory space)
	;
	; 10/10/2015
	; 21/07/2015
	;mov	ebp, esp ; (**)
	; 18/10/2015
	;mov 	edi, ebp
	; 23/07/2022
	mov	edi, esp ; (**)
	mov 	ecx, MAX_ARG_LEN ; 256
	;sub	edi, MAX_ARG_LEN ; 256
	sub	edi, ecx
	mov	esp, edi ; *!*
	xor	eax, eax
	mov 	[u.nread], eax ; 0
	; ([argc] must be cleared because previous 'sysexec'
	; may leave it with any value after an error))
	;mov	[argc], ax ; 0 ; 13/11/2017
	; 23/07/2022
	mov	[argc], eax ; 0
	dec	ecx ; 256 - 1
	mov 	[u.count], ecx ; MAX_ARG_LEN - 1 ; 255
	;mov 	dword [u.count], MAX_ARG_LEN - 1 ; 255
sysexec_2:
	mov	esi, [argv] ; 18/10/2015 
	call	get_argp
	;mov	ecx, 4
	; 23/07/2022
	xor	ecx, ecx
	mov	cl, 4
sysexec_3:
	and	eax, eax
        jz	short sysexec_6 ; 23/07/2022
	; 18/10/2015
	add	[argv], ecx ; 4
	;;inc	word [argc]
	; 23/07/2022
	;inc	dword [argc]
	inc	byte [argc]
	;
	mov	[u.base], eax
 	; 23/10/2015
	mov	word [u.pcount], 0
sysexec_4:
	call	cpass ; get a character from user's core memory
        jnz	short sysexec_5
		; (max. 255 chars + null)
	; 18/10/2015
	sub 	al, al
	stosb
	inc	dword [u.nread]
	; 23/07/2022
	jmp	short sysexec_6 ; 24/04/2016
sysexec_5:
	stosb
	and 	al, al
	jnz	short sysexec_4
	;mov	ecx, 4
	; 23/07/2022
	xor	ecx, ecx
	mov	cl, 4
	cmp	[ncount], ecx ; 4
	jb	short sysexec_2
	mov	esi, [nbase]
	add	[nbase], ecx ; 4
	;sub	[ncount], cx 
	; 23/07/2022
	sub	[ncount], ecx
	mov	eax, [esi]
	jmp	short sysexec_3

sysexec_6:
	; 23/07/2022 - TRDOS 386 Kernel v2.0.5
	; 19/11/2017
	; 18/11/2017
	; 14/11/2017
	; 13/11/2017
	mov	[argv], esp ; *!* ; start address of argument list

	; 04/01/2017
	; 24/10/2016
	;;02/05/2016
	; 23/04/2016 (TRDOS 386)
	; 18/10/2015 ('sysexec_6')
	; 23/06/2015
	mov	eax, [u.pgdir] ; physical address of page directory
	;cmp 	eax, [k_page_dir] ; TRDOS MainProg ?
	;je	short sysexec_7
	; 19/11/2017
	mov	ebx, [u.ppgdir] ; phy addr of the parent's page dir
	call	deallocate_page_dir
sysexec_7:
	call	make_page_dir
	;jc	panic  ; allocation error 
	;	       ; after a deallocation would be nonsence !?
	; 23/07/2022
	jc	short sysexec_panic

	; 24/07/2015
	; map kernel pages (1st 4MB) to PDE 0
	;     of the user's page directory
	;     (It is needed for interrupts!)
	; 18/10/2015
	mov	edx, [k_page_dir] ; Kernel's page directory
	mov	eax, [edx] ; physical address of
			   ; kernel's first page table (1st 4 MB)
			   ; (PDE 0 of kernel's page directory)
	mov 	edx, [u.pgdir]
	mov	[edx], eax ; PDE 0 (1st 4MB)
	;
	; 20/07/2015
	mov	ebx, CORE ; start address = 0 (virtual) + CORE
	; 18/10/2015
	mov	esi, pcore ; physical start address
sysexec_8:	
	mov	ecx, PDE_A_USER + PDE_A_WRITE + PDE_A_PRESENT
	call	make_page_table
	;jc	panic
	; 23/07/2022
	jc	short sysexec_panic
	;
	;mov	ecx, PTE_A_USER + PTE_A_WRITE + PTE_A_PRESENT
	call	make_page ; make new page, clear and set the pte
	;jc	panic
	; 23/07/2022
	jc	short sysexec_panic
	;
	mov	[esi], eax ; 24/06/2015
	; ebx = virtual address (24/07/2015)
	; 23/07/2022
	;call 	add_to_swap_queue
	; 18/10/2015
	cmp	esi, ecore ; user's stack (last) page ?
	je	short sysexec_9 ; yes
	mov	esi, ecore  ; physical address of the last page
	; 20/07/2015
	mov	ebx, (ECORE - PAGE_SIZE) + CORE
	; ebx = virtual end address + segment base address - 4K
        jmp     short sysexec_8
sysexec_panic:
	; 23/07/2022
	jmp	panic

sysexec_9:
	; 23/07/2022 - TRDOS 386 Kernel v2.0.5
	; 19/11/2017 
	; 24/04/2016 (TRDOS 386 = TRDOS v2.0)
	; 25/06/2015 - 26/08/2015 - 18/10/2015
	; move arguments from kernel stack to [ecore]
	; (argument list/line will be copied from kernel stack
	; frame to the last (stack) page of user's core memory)
	; 18/10/2015
	mov	edi, [ecore]
	add	edi, PAGE_SIZE
	; 19/11/2017
	;sub	edi, 4
	;mov	dword [edi], 0
	;mov	ebx, edi
	;
	;movzx	eax, word [argc]
	;or	eax, eax
	;jz	short sysexec_13 ; 19/11/2017
	;;jnz	short sysexec_10
	;;mov 	ebx, edi
	;;sub	ebx, 4 
	;;mov	[ebx], eax ; 0
	;;jmp 	short sysexec_13
	; 23/07/2022
	; [argc] < 32
	mov	eax, [argc]
	or	eax, eax
	jnz	short sysexec_10
	mov 	ebx, edi
	sub	ebx, 4 
	mov	[ebx], eax ; 0
	jmp 	short sysexec_13

sysexec_10:
	mov	ecx, [u.nread]
	; 13/11/2017
	;;mov	esi, TextBuffer ; 'load_and_execute_file'
	;mov	esi, esp  	; 'sysexec'
	mov	esi, [argv] ; 24/04/2016 (TRDOS 386  = TRDOS v2.0)
	; 23/07/2022
	sub	edi, ecx ; page end address - argument list length
	;sub	ebx, ecx ; 19/11/2017

	;;;;
	; 23/07/2022
	; (move edi -backward- to dword boundary)
	; ((this will prevent 'general protection fault' error
	;  as result of a lodsd or dword move instruction
	;  at the end of argument list))
	sub	edi, 3
	and	edi, ~3 ; (*)
	;;;

	mov	edx, eax
	inc	dl ; argument count + 1 for argc value
	shl 	dl, 2  ; 4 * (argument count + 1)
	; edx <= 128
	mov	ebx, edi
	;mov	edi, ebx ; 19//11/2017
	; 23/07/2022 (*) - edi is already dword aligned -
	;and	bl, 0FCh ; 32 bit (dword) alignment
	sub 	ebx, edx
	mov	edx, edi
	rep	movsb
	mov 	esi, edx
	mov 	edi, ebx
	mov	edx, ECORE - PAGE_SIZE ; virtual addr. of the last page
	sub 	edx, [ecore] ; difference (virtual - physical)
	stosd	; eax = argument count
sysexec_11:
	mov	eax, esi
	add	eax, edx
	stosd  ; eax = virtual address
	; 23/07/2022
	dec	byte [argc]
	;dec	dword [argc]
	;;dec	word [argc] ; 14/11/2017
	jz	short sysexec_13
sysexec_12:
	lodsb
	and	al, al
	jnz	short sysexec_12
	jmp	short sysexec_11
sysexec_13:
	; 24/10/2016
	; 24/04/2016 - TRDOS 386 (TRDOS v2.0)
	; 23/06/2015 - 19/10/2015 (Retro UNIX 386 v1, 'sysexec_13')
	;
	; moving arguments to [ecore] is OK here..
	;
	; ebx = beginning addres of argument list pointers
		;	in user's stack
	sub 	ebx, [ecore]
	add     ebx, (ECORE - PAGE_SIZE)
			; end of core - 4096 (last page)
			; (virtual address)
	mov	[argv], ebx
	mov	[u.break], ebx ; available user memory
	;
	sub	eax, eax
	mov	dword [u.count], 32 ; Executable file header size
	mov	dword [u.fofp], u.off
	mov	[u.off], eax ; 0
	mov	[u.base], eax ; 0, start of user's core (virtual)
	; 24/10/2016
	mov	al, [Current_Drv]
	mov	[cdev], al
	;
	mov	eax, [ii] ; Fist Cluster of the Program (PRG) file
	; EAX = First cluster of the executable file
	call	readi

	mov	ecx, [u.break] ; top of user's stack (physical addr.)
	mov	[u.count], ecx ; save for overrun check
	;
	mov	ecx, [u.nread]
	mov	[u.break], ecx ; virtual address (offset from start)
	cmp	cl, 32
        jne     short sysexec_15
	;:
	; Retro UNIX 386 v1 (32 bit) executable file header format
	mov	esi, [pcore] ; start address of user's core memory 
		             ; (phys. start addr. of the exec. file)
	lodsd
	cmp	ax, 1EEBh ; EBH, 1Eh -> jump to +32
	jne	short sysexec_15
	lodsd
	mov	ecx, eax ; text (code) section size
	lodsd
	add	ecx, eax ; + data section size (initialized data)
	mov	ebx, ecx
	lodsd	
	add	ebx, eax ; + bss section size (for overrun checking)
	cmp	ebx, [u.count]
	ja	short sysexec_14  ; program overruns stack !
	;
	; add bss section size to [u.break]
	add 	[u.break], eax
	;
	sub	ecx, 32  ; header size (already loaded)
	;cmp	ecx, [u.count]
	;jnb	short sysexec_16
	mov	[u.count], ecx ; required read count
	jmp	short sysexec_16
sysexec_14:
	; insufficient (out of) memory
	mov	dword [u.error], ERR_MINOR_IM ; 1
	jmp	error
sysexec_15:
        mov	edx, [i.size] ; file size
	sub	edx, ecx ; file size - loaded bytes
	jna	short sysexec_17 ; no need to next read
	add	ecx, edx ; [i.size]
	cmp	ecx, [u.count] ; overrun check (!)
	ja	short sysexec_14
	mov	[u.count], edx
sysexec_16:
	mov	eax, [ii] ; first cluster
	call	readi
	mov	ecx, [u.nread]
	add	[u.break], ecx
sysexec_17:
	mov	eax, [ii] ; first cluster
	call	iclose
	xor     eax, eax
	
	; 21/08/2024
	;inc	al
	;mov	[u.intr], ax ; 1 (interrupt/time-out is enabled)
	;mov	[u.quit], ax ; 1 ('crtl+brk' signal is enabled)
	; 23/07/2022
	;dec	al
	; 21/08/2024
	dec	eax
	mov	[u.intr], ax ; -1 ; 0FFFFh ; enable CTRL+CRK
	inc	eax
	mov	[u.quit], ax ; 0 ; reset CTRL+BRK flag

	;cmp	dword [u.ppgdir], 0  ; is the caller MainProg (kernel) ?
	cmp	[u.ppgdir], eax ; 0 ; 23/07/2022
	ja	short sysexec_18 ; no, the caller is user process
	; If the caller is kernel (MainProg), 'sysexec' will come here
	mov	edx, [k_page_dir] ; kernel's page directory
	mov	[u.ppgdir], edx ; next time 'sysexec' must not come here
sysexec_18:
	; 02/05/2016
	; 24/04/2016 (TRDOS 386 = TRDOS v2.0)
	; 18/10/2015 (Retro UNIX 386 v1)
	; 05/08/2015
	; 29/07/2015

;	; **** arguments list test start - 19/11/2017
;	mov	ebp, [argv]
;	sub	ebp, ECORE - 4096
;	add	ebp, [ecore]
;
;	mov	ebx, [ebp]
;	mov	[argc], bx
;	add	ebp, 4
;	mov	byte [ccolor], 1Fh
;_zx0:
;	cmp	word [argc], 0
;	jna	short _zx2
;_zx1:
;	push	ebp
;	mov	esi, [ebp]
;
;	sub	esi, ECORE - 4096
;	add	esi, [ecore]
;
;	call	print_cmsg
;
;	dec	word [argc]
;	jz	short _zx2
;
;	mov	al, '.'
;	mov	bl, 07h
;	mov	bh, [u.ttyn]
;	call 	_write_tty 
;
;	pop	ebp
;	add	ebp, 4
;	jmp	short _zx1
;_zx2:
;	pop	ebp
;	mov	byte [ccolor], 07h
;	mov	eax, 1
;	; **** arguments list test stop
;	Test result is OK! (there is not a wrong thing) - 19/11/2017

	mov	ebp, [argv] ; user's stack pointer must point to argument
			    ; list pointers (argument count)
	cli
        mov     esp, [tss.esp0] ; ring 0 (kernel) stack pointer
	;mov   	esp, [u.sp] ; Restore Kernel stack
			    ; for this process
	;add	esp, 20 ; --> EIP, CS, EFLAGS, ESP, SS
	;;xor	eax, eax ; 0
	; 23/07/2022
	;dec	al ; eax = 0
	; eax = 0

	;mov	edx, UDATA
	; 18/11/2017
	push	UDATA ; user's stack segment
	;push	edx
	push	ebp ; user's stack pointer
		    ; (points to number of arguments)
	
	; 04/01/2017
	; MainProg comes here while [sysflg]= 0FFh
	; (but sysexec comes here while [sysflg]= 0)
	mov	byte [sysflg], 0 ; 04/01/2017
				 ; (timer_int sysflg control)
	sti
	pushfd	; EFLAGS
		; Set IF for enabling interrupts in user mode
	;or	dword [esp], 200h 
	;
	;mov	bx, UCODE
	;push	bx ; user's code segment
	push	UCODE
	;push	0
	push	eax ; EIP (=0) - start address -
	mov	[u.sp], esp ; 29/07/2015
	; 05/08/2015
	; Remedy of a General Protection Fault during 'iretd' is here !
	; ('push dx' would cause to general protection fault, 
	; after 'pop ds' etc.)
	;
	;; push dx ; ds (UDATA)
	;; push dx ; es (UDATA)
	;; push dx ; fs (UDATA)
	;; push dx ; gs (UDATA)
	;
	; This is a trick to prevent general protection fault
	; during 'iretd' intruction at the end of 'sysrele' (in u1.s):
	mov	dx, UDATA ; 19/11/2017
	mov 	es, dx ; UDATA
	push 	es ; ds (UDATA)
	push 	es ; es (UDATA)
	push 	es ; fs (UDATA)
	push	es ; gs (UDATA)
	mov	dx, KDATA
	mov	es, dx
	;
	;; pushad simulation
	mov	ebp, esp ; esp before pushad
	push	eax ; eax (0)
	push	eax ; ecx (0)
	push	eax ; edx (0)
	push	eax ; ebx (0)
	push	ebp ; esp before pushad
	push	eax ; ebp (0)
	push	eax ; esi (0)
	push	eax ; edi (0)
	;
	mov	[u.r0], eax ; eax = 0
	mov	[u.usp], esp

	; 14/11/2017
	jmp	sysret0

get_argp:
	; 08/08/2022
	; 23/07/2022 - TRDOS 386 Kernel v2.0.5
	; 11/12/2021 - Retro UNIX 386 v1.2 
	; 14/11/2017 - TRDOS 386 (TRDOS v2.0)
	; 18/10/2015 (nbase, ncount)
	; 21/07/2015
	; 24/06/2015 (Retro UNIX 386 v1)
	; Get (virtual) address of argument from user's core memory
	;
	; INPUT:
	;	esi = virtual address of argument pointer
	; OUTPUT:
	;	eax = virtual address of argument
	;
	; Modified registers: EAX, EBX, ECX, EDX, ESI 
	;
 	cmp     dword [u.ppgdir], 0 ; /etc/init ?
				    ; (the caller is kernel)
	;jna	short get_argpk
	; 08/08/2022
	ja	short get_argp5
	jmp	get_argpk
get_argp5:
     	mov	ebx, esi
	call	get_physical_addr ; get physical address
        jc      short get_argp_err ; 23/07/2022
	mov 	[nbase], eax ; physical address
	;mov	[ncount], cx ; remain byte count in page (1-4096)
	; 23/07/2022
	mov	[ncount], ecx
	;mov	eax, 4 ; 21/07/2015
	xor	eax, eax
	mov	al, 4
	;cmp	cx, ax ; 4
	; 23/07/2022
	cmp	ecx, eax ; 4
	jnb	short get_argp2
	mov	ebx, esi
	add	ebx, ecx
	call	get_physical_addr ; get physical address
	jc	short get_argp_err
	;push	esi
	mov	esi, eax
	;xchg	cx, [ncount]
	; 23/07/2022
	xchg	ecx, [ncount]
	xchg	esi, [nbase]
	mov	ch, 4
	sub	ch, cl
get_argp0:
	lodsb
	;push	ax
	; 23/07/2022
	push	eax
	dec	cl
        jnz     short get_argp0
	mov	esi, [nbase]
	; 21/07/2015
	movzx	eax, ch
	add	[nbase], eax
	;sub	[ncount], ax
	; 23/07/2022
	sub	[ncount], eax
get_argp1:
	lodsb
	dec	ch
        jz      short get_argp3
        ;push	ax
	; 23/07/2022
	push	eax
	jmp     short get_argp1
get_argp_err:
	mov	[u.error], eax
	; 14/11/2017
	mov	eax, ERR_BAD_CMD_ARG ; 01h ; TRDOS 8086
	mov	[u.r0], eax
	jmp	error
get_argp2:
	; 21/07/2015
	;mov	eax, 4
	mov 	edx, [nbase] ; 18/10/2015
	add	[nbase], eax
	;sub	[ncount], ax
	; 23/07/2022
	sub	[ncount], eax
	;
	mov	eax, [edx]
	retn
get_argpk:
	; Argument is in kernel's memory space
	mov	word [ncount], PAGE_SIZE ; 4096
	mov	[nbase], esi
	add	dword [nbase], 4
	mov	eax, [esi] ; virtual addr. = physical addr.
	retn
get_argp3:
	mov	cl, 3
get_argp4:
	shl	eax, 8
	;pop	dx
	; 23/07/2022
	pop	edx
	mov 	al, dl
        loop    get_argp4
	;pop	esi
	retn

	; 23/07/2022
%if 0	

sysstat: 
	; 13/01/2017 - TRDOS 386 (TRDOS v2.0)
	; temporary !
	mov	eax, ERR_INV_FNUMBER ; 'invalid function number !'
        mov     [u.error], eax
        mov     [u.r0], eax 
	jmp	error

sysfstat: 
	; 13/01/2017 - TRDOS 386 (TRDOS v2.0)
	; temporary !
	mov	eax, ERR_INV_FNUMBER ; 'invalid function number !'
        mov     [u.error], eax
        mov     [u.r0], eax 
	jmp	error

%endif

fclose:
	; 24/04/2025
	; 21/04/2025 - TRDOS 386 Kernel v2.0.10 (v2.1 pre-work)
	; 18/09/2024 - TRDOS 386 Kernel v2.0.9
	; 23/07/2022 - TRDOS 386 Kernel v2.0.5
	; 06/10/2016 (TRDOS 386 = TRDOS v2.0)
	;
	; 18/06/2015 (Retro UNIX 386 v1 - Beginning)
	;            (32 bit offset pointer modification)
	; 19/04/2013 - 12/01/2014 (Retro UNIX 8086 v1)
	;
	; Given the file descriptor (index to the u.fp list)
	; 'fclose' first gets the i-number of the file via 'getf'.
	; If i-node is active (i-number > 0) the entry in 
	; u.fp list is cleared. If all the processes that opened
	; that file close it, then fsp etry is freed and the file
	; is closed. If not a return is taken. 
	; If the file has been deleted while open, 'anyi' is called
	; to see anyone else has it open, i.e., see if it is appears
	; in another entry in the fsp table. Upon return from 'anyi'
	; a check is made to see if the file is special.
	;
	; INPUTS ->
	;    r1 - contains the file descriptor (value=0,1,2...)
	;    u.fp - list of entries in the fsp table
	;    fsp - table of entries (4 words/entry) of open files.
	; OUTPUTS ->
	;    r1 - contains the same file descriptor
	;    r2 - contains i-number
	;
	; ((AX = R1))
	; ((Modified registers: EDX, EBX, ECX, ESI, EDI, EBP))
	;
	; Retro UNIX 8086 v1 modification : CF = 1
	;              if i-number of the file is 0. (error)
	;
	; TRDOS 386 (06/10/2016)
	; 
	; INPUT:
	;	EAX = File Handle (File Descriptor, File Index)
	;
	; OUTPUT:
	;	CF = 1 -> File not open !
	;	CF = 0 -> OK!
	;	     EBX = File Number (System)
	;	     [cdev] = Logical DOS Drive Number
	;	     EAX = File Handle/Number (user)
	;
	; Modified Registers: EBX, ECX, EDX, ESI, EDI, EBP

	push	eax ; File handle

	call	getf
	;jc	device_close ; eax = device number
	; 17/04/2021 (temporary)
	; 24/04/2025
	jnc	short fclose_1
fclose_0:
	pop	eax
	; 24/04/2025 (BugFix for 'sysexit')
	;jmp	rw2 ; file not open !
	; eax = file handle
	mov	dword [u.error], ERR_FILE_NOT_OPEN ; file not open !
	; cf = 1
	retn
fclose_1:
	cmp	byte [ebx+OF_MODE], 1 ; open mode ; 0 = empty entry
	; 24/04/2025
	jb	short fclose_0	      ; 1 = read, 2 = write

	; 24/04/2025
	;; 18/09/2024 (EMPTY FILE BugFix)
	;;cmp	eax, 1 ; is the first cluster number > 0
	;;jb	short fclose_0 ; no, this is empty entry

	; 24/04/2025
	call	convert_current_date_time ; (MSDOS -> DATE16)
	; ax = Time in dos dir entry format (HHHHHMMMMMMSSSSS)
	; dx = Date in dos dir entry format (YYYYYYYMMMMDDDDD)
	; modified registers: eax, edx, ecx

	; update last modification date&time of the open file
	shl	ebx, 2
	mov	[ebx+OF_DATETIME], ax
	mov	[ebx+OF_DATETIME+2], dx
	shr	ebx, 2

	; 21/04/2025 - TRDOS 386 v2.0.10
	; ebx = System File Number (MSDOS -> SFT entry number)
	call	update_directory_entry
	; 24/04/2025
	pushf
	; if cf = 1 -> eax = error code 

; ****
	dec	byte [ebx+OF_OPENCOUNT] ; decrement the number of processes
			                ; that have opened the file
	jns	short fclose_2 ; jump if not negative (jump if bit 7 is 0)
			; if all processes haven't closed the file, return
	;
	; 24/04/2025 (ecx)
	; 23/07/2022 (eax)
	xor	ecx, ecx ; 0
	mov	[ebx+OF_MODE], cl ; 0 = empty entry
	;mov	[ebx+OF_STATUS], cl ; 0 = empty entry
	shl	ebx, 2
	mov	[ebx+OF_FCLUSTER], ecx ; 0
	mov	[ebx+OF_CCLUSTER], ecx ; 0
	;mov	[ebx+OF_CCINDEX], ecx ; 0
	;mov	[ebx+OF_OPENCOUNT], cl ; 0
	mov	[u.fofp], ecx ; 0
	shr	ebx, 2

fclose_2: ; 1:
	; 24/04/2025
	popf
	jnc	short fclose_3
	mov	[u.error], eax
fclose_3:
	pop	eax ; File handle (File Descriptor, File Index)
	mov	byte [eax+u.fp], 0 ; clear that entry in the u.fp list
	retn

getf:
	; 03/09/2024 - TRDOS 386 v2.0.9
	; 23/07/2022 - TRDOS 386 v2.0.5
	; 17/04/2021 - TRDOS 386 v2.0.4
	;	(temporary modifications)
	; 12/10/2016
	; 11/10/2016
	; 08/10/2016
	; 06/10/2016 (TRDOS 386 = TRDOS v2.0)
	; / get the device number and the i-number of an open file
	; 13/05/2015
	; 11/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 19/04/2013 - 18/11/2013 (Retro UNIX 8086 v1)
	;
	mov	ebx, eax
getf1:
	cmp	ebx, 10
        jnb	short getf2
	mov	bl, [ebx+u.fp]
	or	bl, bl
	jnz	short getf3
getf2:
	; 'File not open !' error (ax=0)
	sub	eax, eax
	; 03/09/2024
	stc
	retn
getf3:
	; 23/07/2022
	;test	bl, 80h
	;jnz	short getf5 ; device
	dec	bl ; 0 based
	mov	al, [ebx+OF_DRIVE]
	mov	[cdev], al
	shl	bl, 2 ; *4 (dword offset)
	; 23/07/2022
	;shl	ebx, 2
	mov	eax, [ebx+OF_SIZE]
	mov	[i.size], eax ; file size
	lea	eax, [ebx+OF_POINTER] ; 12/10/2016
	mov	[u.fofp], eax
	mov	eax, [ebx+OF_FCLUSTER]
	shr	bl, 2 ; /4 (byte offset)
	; 23/07/2022
	;shr	ebx, 2
	; 17/04/2021
	clc 
getf4:
	retn
;getf5: 
	; 17/04/2021
	; (following code is disabled as temporary)
	;
	;; get device number
	;and	bl, 7Fh ; 1 to 7Fh
	;dec	bl ; 0 based (0 to 7Eh)
	;mov	al, [ebx+DEV_DRIVER]
	;mov	ch, [ebx+DEV_ACCESS]
	;mov	cl, [ebx+DEV_OPENMODE]
	;and	ch, 0FEh ; reset bit 0 ; dev_close
	;
	; 23/07/2022
	;stc ; cf = 1
	;retn

trans_addr_nmbp:
	; 18/10/2015
	; 12/10/2015
	mov 	ebp, [u.namep]
trans_addr_nm: 
	; Convert virtual (pathname) address to physical address
	; (Retro UNIX 386 v1 feature only !)
	; 18/10/2015
	; 12/10/2015 (u.pnbase & u.pncount has been removed from code)
	; 02/07/2015
	; 17/06/2015
	; 16/06/2015
	;
	; INPUTS: 
	;	ebp = pathname address (virtual) ; [u.namep]
	;	[u.pgdir] = user's page directory
	; OUTPUT:
	;       esi = physical address of the pathname
	;	ecx = remain byte count in the page
	;
	; (Modified registers: EAX, EBX, ECX, EDX, ESI)
	;
        cmp     dword [u.ppgdir], 0  ; /etc/init ? (sysexec)
	jna	short trans_addr_nmk ; the caller is os kernel;
				     ; it is already physical address
   	push	eax
	mov	ebx, ebp ; [u.namep] ; pathname address (virtual)
       	call	get_physical_addr ; get physical address
	jc	short tr_addr_nm_err
	; 18/10/2015
	; eax = physical address 
	; cx = remain byte count in page (1-4096)
		; 12/10/2015 (cx = [u.pncount])
	mov	esi, eax ; 12/10/2015 (esi=[u.pnbase])
	pop	eax
	retn

tr_addr_nm_err:
	mov	[u.error], eax
	;pop 	eax
	jmp	error

trans_addr_nmk:
	; 12/10/2015
	; 02/07/2015
	mov	esi, [u.namep]  ; [u.pnbase]
	mov	cx, PAGE_SIZE ; 4096 ; [u.pncount]
	retn

sysbreak:
	; 06/09/2024 - TRDOS 386 v2.0.9
	; 23/07/2022 - TRDOS 386 v2.0.5
	; 18/10/2015
	; 07/10/2015
	; 23/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 20/06/2013 - 24/03/2014 (Retro UNIX 8086 v1)
	;
	; 'sysbreak' sets the programs break points. 
	; It checks the current break point (u.break) to see if it is
	; between "core" and the stack (sp). If it is, it is made an
	; even address (if it was odd) and the area between u.break
	; and the stack is cleared. The new breakpoint is then put
	; in u.break and control is passed to 'sysret'.
	;
	; Calling sequence:
	;	sysbreak; addr
	; Arguments: -
	;	
	; Inputs: u.break - current breakpoint
	; Outputs: u.break - new breakpoint 
	;	area between old u.break and the stack (sp) is cleared.
	; 
	; ...............................................................
	; 06/09/2024 - TRDOS 386 v2.0.9 - Major Modification -
	;
	; 	This system call performs "malloc" function as in C.
	; Input:
	;    EBX = new u.break address (virtual, user's space)
	; (Note: Default/Initial u.break address is the start of the BSS
	;	 section. Or it is file size for PRG -flat image- files.)
	; If EBX = 0FFFFFFFFh ; -1
	;    Return current u.break in EAX
	; Output:
	;    If EBX input is -1, EAX = current u.break address (virtual)
	;	otherwise, EAX = new u.break address
	; Area between the new u.break and the old u.break is cleared.
	; (Note: If the new break address cross overs user's esp,
	;	 this area <new break - old break> is not cleared.)
	; If CF=1 at return, it means Memory Allocation Error.
	;		(or "Insufficient Memory" error)
	;	EAX = 0
	; ...............................................................
	;	
	; Retro UNIX 8086 v1 modification:
	;	The user/application program puts breakpoint address
	;       in BX register as 'sysbreak' system call argument.
	; 	(argument transfer method 1)
	;
	;  NOTE: Beginning of core is 0 in Retro UNIX 8086 v1 !
	; 	((!'sysbreak' is not needed in Retro UNIX 8086 v1!))
	;  NOTE:
	; 	'sysbreak' clears extended part (beyond of previous
	;	'u.break' address) of user's memory for original unix's
	;	'bss' compatibility with Retro UNIX 8086 v1 (19/11/2013)
	
		; mov u.break,r1 / move users break point to r1
		; cmp r1,$core / is it the same or lower than core?
		; blos 1f / yes, 1f
	; 23/06/2015
	mov	ebp, [u.break] ; virtual address (offset)
	
	; 06/09/2024
	xor	eax, eax
	mov	[u.r0], eax ; 0 ; default ('memory allocation error')
	dec	eax ; -1

	; 06/09/2024 - TRDOS 386 v2.0.9
	; get u.break address (for 'malloc' in c compiler)
	; for PRG files:
	; default/initial u.break address is file size
	cmp	ebx, eax ; -1 ; 0FFFFFFFFh
	jb	short sysbreak_@

sysbreak_4:
	mov	ebx, ebp
	;(allocates a new page for user if it is not present)
	call	get_physical_addr ; get physical address
	jc	short tr_addr_nm_err

	mov	[u.r0], ebp  ; start of bss space (to be allocated)
	jmp	sysret

sysbreak_@:
	;and	ebp, ebp
	;jz	short sysbreak_3 
	; Retro UNIX 386 v1 NOTE: u.break points to virtual address !!!
	; (Even break point address is not needed for Retro UNIX 386 v1)
	mov	edx, [u.sp] ; kernel stack at the beginning of sys call
	add	edx, 12 ; EIP -4-> CS -4-> EFLAGS -4-> ESP (user) 
	; 07/10/2015
	;mov	[u.break], ebx ; virtual address !!!
	;
	; 06/09/2024
	; ebp = old/current u.break

	cmp	ebx, [edx] ; compare new break point with 
			   ; with top of user's stack (virtual!)
	jnb	short sysbreak_3
		; cmp r1,sp / is it the same or higher 
			  ; / than the stack?
		; bhis 1f / yes, 1f
	mov	esi, ebx
	sub	esi, ebp ; new break point - old break point
	;jna	short sysbreak_3 
	; 06/09/2024
	ja	short sysbreak_1
	jz	short sysbreak_4
	neg	esi ; convert to positive number (big-small)
	mov	ebp, ebx ; move small number (address) to ebp

sysbreak_1:
	; 06/09/2024
	push	ebx
	mov	ebx, ebp
	call	get_physical_addr ; get physical address
	pop	ebx
	jc	short tr_addr_nm_err ; 23/07/2022
	; 18/10/2015
	mov	edi, eax
	sub	eax, eax ; 0
		 ; ECX = remain byte count in page (1-4096)
	cmp	esi, ecx
	jnb	short sysbreak_2
	mov	ecx, esi
sysbreak_2:
	sub	esi, ecx
	add	ebp, ecx
	rep 	stosb
	or	esi, esi
	jnz	short sysbreak_1
		; bit $1,r1 / is it an odd address
		; beq 2f / no, its even
		; clrb (r1)+ / yes, make it even
	; 2: / clear area between the break point and the stack
		; cmp r1,sp / is it higher or same than the stack
		; bhis 1f / yes, quit
		; clr (r1)+ / clear word
		; br 2b / go back
	;pop	ebx
sysbreak_3: ; 1:
	; 06/09/2024
	mov	[u.break], ebx ; virtual address !!!
		; jsr r0,arg; u.break / put the "address"
			; / in u.break (set new break point)
		; br sysret4 / br sysret
	; 06/09/2024
	mov	[u.r0], ebx ; return new break point in eax
	jmp	sysret

sysseek: ; / moves read write pointer in an fsp entry
	; 06/11/2016 - TRDOS 386 (TRDOS v2.0)
	; 22/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 07/07/2013 - 05/08/2013 (Retro UNIX 8086 v1)
	;
	; 'sysseek' changes the r/w pointer of (3rd word of in an
	; fsp entry) of an open file whose file descriptor is in u.r0.
	; The file descriptor refers to a file open for reading or
	; writing. The read (or write) pointer is set as follows:
	;	* if 'ptrname' is 0, the pointer is set to offset.
	;	* if 'ptrname' is 1, the pointer is set to its
	;	  current location plus offset.
	;	* if 'ptrname' is 2, the pointer is set to the
	;	  size of file plus offset.
	; The error bit (e-bit) is set for an undefined descriptor.
	;
	; Calling sequence:
	;	sysseek; offset; ptrname
	; Arguments:
	;	offset - number of bytes desired to move
	;		 the r/w pointer
	;	ptrname - a switch indicated above
	;
	; Inputs: r0 - file descriptor 
	; Outputs: -
	; ...............................................................
	;	
	; Retro UNIX 8086 v1 modification: 
	;       'sysseek' system call has three arguments; so,
	;	* 1st argument, file descriptor is in BX (BL) register
	;	* 2nd argument, offset is in CX register
	;	* 3rd argument, ptrname/switch is in DX (DL) register

	call	seektell
	; EAX = Current R/W pointer of the file
	; EBX = [u.fofp]
	; [u.base] = offset (ECX input)

	add	eax, [u.base]
	mov	[ebx], eax
	jmp	sysret

systell: ; / get the r/w pointer
	; 06/11/2016 - TRDOS 386 (TRDOS v2.0) - temporary !-
	; 22/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 07/07/2013 - 05/08/2013 (Retro UNIX 8086 v1)
	;
	; Retro UNIX 8086 v1 modification:
	; ! 'systell' does not work in original UNIX v1,
	; 	    it returns with error !
	; Inputs: r0 - file descriptor 
	; Outputs: r0 - file r/w pointer

	;xor	ecx, ecx ; 0
	mov	edx, 1 ; 05/08/2013
	;call 	seektell
	call 	seektell0 ; 05/08/2013
	;; 06/11/2016
	;; mov	eax, [ebx]
	mov	[u.r0], eax
	jmp	sysret

; Original unix v1 'systell' system call:
		; jsr r0,seektell
		; br error4

seektell:
	; 06/11/2016 - TRDOS 386 (TRDOS v2.0)
	; 03/01/2016
	; 22/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 07/07/2013 - 05/08/2013 (Retro UNIX 8086 v1)
	;
	; 'seektell' puts the arguments from sysseek and systell
	; call in u.base and u.count. It then gets the i-number of
	; the file from the file descriptor in u.r0 and by calling
	; getf. The i-node is brought into core and then u.count
	; is checked to see it is a 0, 1, or 2.
	; If it is 0 - u.count stays the same
	;          1 - u.count = offset (u.fofp)
	;	   2 - u.count = i.size (size of file)
	;
	; !! Retro UNIX 8086 v1 modification:
	;	Argument 1, file descriptor is in BX;
	;	Argument 2, offset is in CX;
	;	Argument 3, ptrname/switch is in DX register.
	;
	; ((Return -> eax = base for offset (position= base+offset))
	;
	mov 	[u.base], ecx ; offset
seektell0:
	mov 	[u.count], edx
	; EBX = file descriptor (file number)
	call	getf1
	; EAX = First cluster of the file
	; EBX = File number (Open file number)
	; [u.fofp] = Pointer to File pointer
	; [i.size] = File size

	or	eax, eax
	jnz	short seektell1

seektell_err:
	; 24/04/2025
device_rw_err:
	mov	eax, ERR_FILE_NOT_OPEN
	mov	[u.r0], eax
	mov	[u.error], eax ; 'file not open !'
	jmp	error

seektell1:
        mov     ebx, [u.fofp]
	cmp	byte [u.count], 1
	ja	short seektell2
	je	short seektell3
	xor	eax, eax
	retn

seektell2:
        mov   	eax, [i.size]
	retn

seektell3:
	mov	eax, [ebx]
	retn

sysintr: ; / set interrupt handling
	; 21/08/2024 - TRDOS 386 v2.0.9 modification
	; 22/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 07/07/2013 (Retro UNIX 8086 v1)
	;
	; 'sysintr' sets the interrupt handling value. It puts
	; argument of its call in u.intr then branches into 'sysquit'
	; routine. u.tty is checked if to see if a control tty exists.
	; If one does the interrupt character in the tty buffer is
	; cleared and 'sysret'is called. If one does not exits
	; 'sysret' is just called.
	;
	; Calling sequence:
	;	sysintr; arg
	; Argument:
	;	arg - if 0, interrupts (ASCII DELETE) are ignored.
	;	    - if 1, interrupts cause their normal result
	;		 i.e force an exit.
	;	    - if arg is a location within the program,
	;		control is passed to that location when
	;		an interrupt occurs.
	; Inputs: -
	; Outputs: -
	; ...............................................................
	;	
	; Retro UNIX 8086 v1 modification: 
	;       'sysintr' system call sets u.intr to value of BX
	;	then branches into sysquit.

	; 21/08/2024 - TRDOS 386
	; enable/disable terminate (CTRL+BREAK) interrupt 
	;
	; INPUT:
	;	bx = 0 -> disable CTRL+BREAK
	;		-u.intr will be set to 0, u.quit will be ignored-
	;	bx > 0 -> enable CTRL+BREAK (also default at start)
	;		-u.intr will be set to 0FFFFh, u.quit will be used-
	;	NOTE: u.quit is flag for CTRL+BREAK key press status
	;	      -1 = pressed -termination request-, 0 = not pressed 
	; OUTPUT:
	;	none

	;;;
	; 21/08/2024
	test	ebx, 0FFFFh
	jz	short sysintr_@ ; 0
	;mov	bx, 0FFFFh
	xor	ebx, ebx
	dec	ebx ; -1
sysintr_@:
	;;;
	mov	[u.intr], bx
		; jsr r0,arg; u.intr / put the argument in u.intr
		; br 1f / go into quit routine
	jmp	sysret

sysquit:
	; 21/08/2024 - TRDOS 386 v2.0.9 modification
	;	get/reset QUIT (u.quit) status
	;
	; 22/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 07/07/2013 (Retro UNIX 8086 v1)
	;
	; 'sysquit' turns off the quit signal. it puts the argument of
	; the call in u.quit. u.tty is checked if to see if a control 
	; tty exists. If one does the interrupt character in the tty
	; buffer is cleared and 'sysret'is called. If one does not exits
	; 'sysret' is just called.
	;
	; Calling sequence:
	;	sysquit; arg
	; Argument:
	;	arg - if 0, this call diables quit signals from the
	;		typewriter (ASCII FS)
	;	    - if 1, quits are re-enabled and cause execution to
	;		cease and a core image to be produced.
	;		 i.e force an exit.
	;	    - if arg is an addres in the program,
	;		a quit causes control to sent to that
	;		location.
	; Inputs: -
	; Outputs: -
	; ...............................................................
	;	
	; Retro UNIX 8086 v1 modification: 
	;       'sysquit' system call sets u.quit to value of BX
	;	then branches into 'sysret'.

	; 21/08/2024
	;mov	[u.quit], bx
	;jmp	sysret
		; jsr r0,arg; u.quit / put argument in u.quit
	;1:
		; mov u.ttyp,r1 / move pointer to control tty buffer
			      ; / to r1
		; beq sysret4 / return to user
		; clrb 6(r1) / clear the interrupt character
			   ; / in the tty buffer
		; br sysret4 / return to user

	; 21/08/2024 - TRDOS 386
	; get/reset QUIT (u.quit) status
	;
	; INPUT:
	;	none
	; OUTPUT:
	;	eax = 0 -> no CTRL+BREAK
	;	eax = -1 -> CTRL+BREAK status (but not applied)
	;		-u.intr may be 0-
	;	NOTE: u.quit will be reset to 0

	xor	eax, eax ; 0
	cmp	[u.quit], ax
	jz	short sysquit_@ ; already 0
	mov	[u.quit], ax ; 0 ; reset
	dec	eax ; -1
sysquit_@:
	mov	[u.r0], eax
	jmp	sysret	

%if 0

anyi: 
	; 23/07/2022
	; 06/10/2016 (TRDOS 386 = TRDOS v2.0)
	; Major Modification!
	; TRDOS 386 does not permit to delete a file while it is open 
	; The role of 'anyi' procedure has beeen changed to ensure that.
	; 	
	; 22/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 25/04/2013 (Retro UNIX 8086 v1)
	;
	; 'anyi' is called if a file deleted while open.
	; "anyi" checks to see if someone else has opened this file.
	;
	; INPUTS ->
	;    r1 - contains an i-number
	;    fsp - start of table containing open files
	;
	; OUTPUTS ->
	;    "deleted" flag set in fsp entry of another occurrence of
	;	   this file and r2 points 1st word of this fsp entry.
	;    if file not found - bit in i-node map is cleared
	;    			 (i-node is freed)
	;               all blocks related to i-node are freed
	;	        all flags in i-node are cleared
	; ((AX = R1)) input
	;
	;    (Retro UNIX Prototype : 02/12/2012, UNIXCOPY.ASM)
        ;    ((Modified registers: EDX, ECX, EBX, ESI, EDI, EBP))
	;
	; / r1 contains an i-number

	; TRDOS 386 (06/10/2016)
	; 
	; INPUT:
	;	EAX = First Cluster
	;	 DL = Logical DOS Drive Number
	;
	; OUTPUT:
	;	CF = 1 -> EBX = File Handle/Number/Index
	;	CF = 0 -> EBX = 0
	;
	; Modified Registers: EBX

	xor	ebx, ebx
anyi_0: 
	cmp	byte [ebx+OF_MODE], 0 ; 0 = empty entry
	ja	short anyi_2 ; 1 (r), 2 (w) or 3 (r&w)
anyi_1:
	inc	bl
	cmp	bl, OPENFILES ; max. count of open files
	jb	short anyi_0
	xor	eax, eax
	retn 
anyi_2:
	cmp	dl, [ebx+OF_DRIVE]
	jne	short anyi_1
	;shl	bx, 2 ; *4 (dword offset)
	shl	ebx, 2 ; 23/07/2022
	cmp	eax, [ebx+OF_FCLUSTER]
	je	short anyi_3
	;shr	bx, 2 ; /4 (byte offset)
	shr	ebx, 2 ; 23/07/2022
	jmp	short anyi_1 	
anyi_3:
	;shr	bx, 2 ; /4 (bytes offset) (index)
	shr	ebx, 2 ; 23/07/2022
	stc
	retn

%endif

; Retro UNIX 386 v1 Kernel (v0.2) - SYS9.INC
; Last Modification: 09/12/2015

syssleep:
	; 28/08/2024 - TRDOS 386 v2.0.9
	; 24/07/2022 - TRDOS 386 v2.0.5
	; 29/06/2015 - (Retro UNIX 386 v1)
	; 11/06/2014 - (Retro UNIX 8086 v1)
	;
	; Retro UNIX 8086 v1 feature only
	; (INPUT -> none)
	
	; Temporary - 24/07/2022
	mov	[u.r0], ebx
	;
	movzx	ebx, byte [u.uno] ; process number
	mov	ah, [ebx+p.ttyc-1] ; current/console tty
	call	sleep

	; 23/08/2024 - restore [u.usp]
	; swap changes [u.usp] to esp which points to
	; return of sleep
	mov	[u.usp], esp
		; points to user's regs on top ofn system stack

	; 24/07/2022
	inc	dword [u.r0] ; Temporary !
	jmp	sysret

_vp_clr:
	; Reset/Clear Video Page
	;
	; 24/07/2022 - TRDOS 386 v2.0.5
	; 30/06/2015 - (Retro UNIX 386 v1)
	; 21/05/2013 - 30/10/2013(Retro UNIX 8086 v1) (U0.ASM)
	;
	; Retro UNIX 8086 v1 feature only !
	;
	; INPUTS -> 
	;   BH = video page number
	;
	; OUTPUT ->
	;   none
	; ((Modified registers: EAX, BH, ECX, EDX, ESI, EDI))
	;
	; 04/12/2013
	sub	al, al
	; al = 0 (clear video page)
	; bh = video page ; 13/05/2016
	mov	ah, 07h
	; ah = 7 (attribute/color)
	;xor 	cx, cx ; 0, left upper column (cl) & row (cl)
	;mov	dx, 184Fh ; right lower column & row (dl=24, dh=79)
	; 24/07/2022
	xor	ecx, ecx
	mov	edx, 184Fh
	call	_scroll_up
	; bh = video page
	;xor	dx, dx ; 0 (cursor position)
	; 24/07/2022
	xor	edx, edx
	jmp 	_set_cpos

sysmsg:
	; 28/08/2024 - TRDOS 386 v2.0.9
	; 23/07/2022 - TRDOS 386 v2.0.5
	; 07/12/2020
	; 05/12/2020
	; 13/05/2016
	; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
	; 01/07/2015 - 11/11/2015 (Retro UNIX 386 v1)
	; Print user-application message on user's console tty
	;
	; Input -> EBX = Message address
	;	   ECX = Message length (max. 255)
	;	   DL = Color (IBM PC Rombios color attributes)
	;
	cmp	ecx, MAX_MSG_LEN ; 255
	;ja	sysret ; nothing to do with big message size
	; 23/07/2022
	jna	short sysmsg8
sysmsg7:
	jmp	sysret
sysmsg8:	; 23/07/2022
	or	cl, cl
	;jz	sysret
	; 23/07/2022
	jz	short sysmsg7
	and	dl, dl
	jnz	short sysmsg0
	mov	dl, 07h ; default color
		; (black background, light gray character)
sysmsg0:
	mov	[u.base], ebx
	mov	[ccolor], dl ; color attributes
	mov	ebp, esp ; save stack pointer
	xor	ebx, ebx ; 0
	mov	[u.nread], ebx ; 0
	;
	cmp	[u.kcall], bl ; 0
	ja	short sysmsgk ; Temporary (01/07/2015)
	;
	mov	[u.count], ecx
	;inc	ecx ; + 00h ; ASCIIZ
	;
	; 07/12/2020
	;add	ecx, 3
	add	cx, 3
	and	cl, ~3  ; not 3
	;
	;;;
	; 20/08/2024
	; for safety, against a possibility which if ecx+1
	; chars (without a 0 in ecx chars) overs stack frame
	; (for example 255 chars without 0, safe stack frame
	; size is 260.)
	; !!! 'sysmsg' will put a 0 at ecx+1 position.!!!
	push	ecx
	;;;
	;
	sub	esp, ecx
	mov	edi, esp
	mov	esi, esp
	mov	[u.pcount], bx ; reset page (phy. addr.) counter
	; 11/11/2015
	mov 	ah, [u.ttyp] ; recent open tty
	; 0 = none
	dec	ah
	jns	short sysmsg1
	mov	bl, [u.uno] ; process number
	mov	ah, [ebx+p.ttyc-1] ; user's (process's) console tty
sysmsg1:
	mov	[u.ttyn], ah
sysmsg2:
	call	cpass
	jz	short sysmsg5
	stosb
	and	al, al
	jnz	short sysmsg2
sysmsg3:
	cmp	ah, 7 ; tty number
	ja	short sysmsg6 ; serial port
	call	print_cmsg ; 05/12/2020
sysmsg4:
	mov	esp, ebp ; restore stack pointer
	jmp	sysret
sysmsg5:
	mov	byte [edi], 0
	jmp	short sysmsg3
sysmsg6:
	mov	al, [esi]
	call	sndc
	jc	short sysmsg4
	cmp	byte [esi], 0  ; 0 is stop character
	jna	short sysmsg4
	inc 	esi
	mov	ah, [u.ttyn]
	jmp	short sysmsg6

sysmsgk: ; Temporary (01/07/2015)
	; The message has been sent by Kernel (ASCIIZ string)
	; (ECX -character count- will not be considered)
	mov	esi, [u.base]
	mov	ah, [ptty] ; present/current screen (video page)
	mov	[u.ttyn], ah
	mov	byte [u.kcall], 0
	jmp	short sysmsg3

print_cmsg:
	; 08/12/2020
	; 07/12/2020
	; 05/12/2020
	; 18/11/2017
	; 13/05/2016 - TRDOS 386 (TRDOS v2.0)
	; 01/07/2015 (Retro UNIX 386 v1)
	;
	; print message (on user's console tty)
	;	with requested color
	;
	; INPUTS:
	;	esi = message address
	;	[u.ttyn] = tty number (0 to 7)
	;	[ccolor] = color attributes (IBM PC BIOS colors)
	;
	; Modified registers: eax, ebx, ecx, edx, esi, edi
	; (ebp must be preserved)

	;mov	bh, ah
	mov	bh, [u.ttyn]
	mov	bl, [ccolor] ; * ; 05/12/2020

	; 05/12/2020
	cmp	byte [pmi32], 0 ; is vbios's 32 bit pmi enabled ?
	ja	short pcmsg5 ; yes
pcmsg1:
	; 08/12/2020
	mov	bl, [ccolor] ; * (video.s 'u11'&'beep' change BL)
	
	lodsb
	and 	al, al  ; 0
	jz 	short pcmsg2
pcmsg7:
	push 	esi
	;mov	bl, [ccolor] ; * (video.s 'u11'&'beep' change BL)
	; 05/12/2020
	;;mov	bh, [u.ttyn]
	;call 	_write_tty
	;pop	esi
	;jmp	short pcmsg1
;pcmsg2:
	;retn

	; 07/12/2020
	cmp     byte [CRT_MODE], 3
	ja 	short pcmsg4
pcmsg3:
	call	_write_tty_m3
	pop	esi
	jmp	short pcmsg1
pcmsg4:
        cmp     byte [CRT_MODE], 7
	jna 	short pcmsg3
	call	vga_write_teletype
	pop	esi
	jmp	short pcmsg1
pcmsg5:
	; 07/12/2020
        cmp     byte [CRT_MODE], 7
	jna 	short pcmsg1

	; 05/12/2020
	; writing message by using
	; VESA VBE3 video bios protected mode interface
	
	mov	ah, 0Eh
pcmsg6:
	lodsb
	and 	al, al  ; 0
	jz 	short pcmsg2
	; bh = video page
	; ah = 0Eh 
	; al = character
	; bl = color
	call	int10h_32bit_pmi
	jmp	short pcmsg6
pcmsg2:
	retn

sysgeterr:
	; 09/12/2015
	; 21/09/2015 - (Retro UNIX 386 v1 feature only!)
	; Get last error number or page fault count
	; (for debugging)
	;
	; Input -> EBX = return type
	;	   0 = last error code (which is in 'u.error')
	;	   FFFFFFFFh = page fault count for running process
	;	   FFFFFFFEh = total page fault count
	;	   1 .. FFFFFFFDh = undefined 
	;
	; Output -> EAX = last error number or page fault count
	;	   (depending on EBX input)
	;
	and 	ebx, ebx
	jnz	short glerr_2
glerr_0:
	mov	eax, [u.error]
glerr_1:
	mov	[u.r0], eax
 	retn
glerr_2:
	inc	ebx ; FFFFFFFFh -> 0, FFFFFFFEh -> FFFFFFFFh
	jz	short glerr_2 ; page fault count for process
	inc	ebx ; FFFFFFFFh -> 0	
	jnz	short glerr_0
	mov	eax, [PF_Count] ; total page fault count
        jmp     short glerr_1
glerr_3:
	mov 	eax, [u.pfcount]
	jmp	short glerr_1

load_and_run_file:
	; 23/07/2022 - TRDOS 386 Kernel v2.0.5
	; 18/11/2017
	; 22/01/2017
	; 04/01/2017 - 07/01/2017
	; 24/10/2016
	; 24/04/2016 - 02/05/2016 - 03/05/2016 - 06/05/2016
	; 23/04/2016 (TRDOS 386 = TRDOS v2.0)
	; 23/10/2015 (Retro UNIX 386 v1, 'sysexec')
	; 23/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 03/06/2013 - 06/12/2013 (Retro UNIX 8086 v1)
	; EAX = First Cluster number
	; EDX = File Size
	; ESI = Argument list address
	; [argc] = argument count
	; [u.nread] = argument list length
	; [esp] = return address to the caller (*)
	;
	mov	[argv], esi
	mov	[i.size], edx
	mov	[ii], eax

	;sti	; 07/01/2017
	;mov	eax, [k_page_dir]
	;mov	[u.pgdir], eax
	xor 	eax, eax ; clc ; *** ; 04/01/2017
	;mov	[u.r0], eax ; 0 ; 07/01/2017

	; 06/05/2016
	; Set 'sysexit' return order to MainProg
	;
	pop	eax ; * 'loc_load_and_run_file_8:' address
	;; 22/01/2017
	;;cli ; 07/01/2017
	mov	esp, [tss.esp0]
	;
	; 'loc_load_run_file_8' address has 
	; 'jmp loc_file_rw_restore_retn' instruction
	; 'loc_file_rw_restore_retn:' will return to
	; [mainprog_return_addr] 
	; just after 'call command_interpreter'
	;
	push	_end_of_mainprog ; we must not return to here !
	push	dword [mainprog_return_addr]
	mov	ebp, esp ; **
	;	
	pushfd  ; EFLAGS      ; IRETD ; ***
	push	KCODE ; cs    ; IRETD
	push	eax ; * (eip) ; IRETD
	mov	[u.sp], esp
	;mov	byte [u.quant], time_count
	push	ds
	push	es
	push	fs
	push	gs
	;mov	eax, [u.r0]
	sub	eax, eax
	pushad
	push	sysret
	;push	sysrel1 ; 07/01/2017
	mov	[u.usp], esp
	;
	call	wswap ; Save MainProg (process 1) 'u' structure
		      ; and registers for return (from program)	
	mov	esp, ebp ; **
	;;22/01/2017
	;;sti ; 07/01/2017
	; 23/07/2022
	;push	eax  ; * 'loc_load_and_run_file_8:' address
	;
	;;; 02/05/2016
	;;; Create a new process (parent: MainProg)
	xor 	esi, esi
cnpm_1: ; search p.stat table for unused process number
	inc	esi
	cmp	byte [esi+p.stat-1], 0 ; SFREE
				; is process active, unused, dead
	jna	short cnpm_2	; it's unused so branch
	cmp	si, nproc 	; all processes checked
	jb	short cnpm_1    ; no, branch back
cnpm_panic:
	jmp	panic 
cnpm_2:
	mov	eax, [u.pgdir] ; page directory of MainProg
	mov	[u.ppgdir], eax ; parent's page directory
	call	allocate_page
	;jc	panic
	; 23/07/2022
	jc	short cnpm_panic
	
	; EAX = UPAGE (user structure page) address
	mov	[u.upage], eax ; memory page for 'user' struct (child)
	mov	edi, esi
	;shl	di, 2
	; 23/07/2022
	shl	edi, 2
	mov	[edi+p.upage-4], eax ; memory page for 'user' struct
	call	clear_page ; 03/05/2016
	;;movzx	eax, byte [p.ttyc] ; console tty (for MainProg)
	;sub	ax, ax ; 0
	; 23/07/2022
	sub	eax, eax
	mov     [esi+p.ttyc-1], ax ; al - set child's console tty
				   ; ah - reset child's wait channel
	mov 	[u.ttyp], ax ; 0
	
	mov	edx, esi
	mov	[u.uno], dl ; child process number
        inc     byte [esi+p.stat-1] ; 1, SRUN
	;shl	si, 1 ; multiply si by 2 to get index into p.pid table
	; 23/07/2022
	shl	esi, 1
	inc	word [mpid] ; increment m.pid; get a new process name

	; 23/07/2022	
	;mov	ax, [p.pid]  ; get process name of MainProg
	;mov	ax, 1
	inc	al ; eax = 1
	mov	[esi+p.ppid-2], ax ; put parent process name 
			           ; in parent process slot for child
	mov	[u.pri], al ; 1	; normal priority
	
	;dec	ax ; 0
	;mov 	[u.ttyp], ax ; 0

	mov	ax, [mpid]
	mov	[esi+p.pid-2], ax ; put new process name 
				  ; in child process' name slot
	;;;
	mov 	eax, [ii]
	; 23/07/2022
	; Retro UNIX 386 v1, 'sysexec' (u2.s)
	;call	iopen
	; 06/06/2016
	;mov	byte [u.pri], 1 ; normal priority
	;
	;jmp	short sysexec_7 ; 02/05/2016
	; 23/07/2022
	jmp	sysexec_7

;	; 02/05/2016
;	;inc	byte [sysflg] ; 0FFh -> 0
;	;mov	byte [sysflg], 0 ; 04/01/2017
;	movzx	ebx, byte [u.uno]
;	shl	bl, 1 ; 13/11/2017
;	cmp	word [ebx+p.ppid-2], 1 ; MainProg
;	ja	sysret0 ; 03/05/2016
;	push	sysret ; * 
;	mov	[u.usp], esp
;	call	wswap ; save child process 'u' structure and
;		      ; registers
;	add	dword [u.usp], 4 ; 03/05/2016 
;sysexec_19: ; 02/05/2016
;	retn ; * 'sysret' ; byte [sysflg] -> 0FFh

readi:
	; 09/08/2022
	; 23/07/2022 - TRDOS 386 Kernel v2.0.5
	; 01/05/2016
	; 25/04/2016 - TRDOS 386 (TRDOS v2.0)
	; 20/05/2015 - Retro UNIX 386 v1
	; 11/03/2013 - 31/07/2013 (Retro UNIX 8086 v1)
	;
	; Reads from a file whose the first cluster number in EAX
	; 
	; INPUTS ->
	;    EAX - First cluster number of the file
	;    u.count - byte count user desires
	;    u.base - points to user buffer
	;    u.fofp - points to dword with current file offset
	;    i.size - file size
	;    cdev - logical dos drive number of the file
	; OUTPUTS ->
	;    u.count - cleared
	;    u.nread - accumulates total bytes passed back
	;
	; ((EAX)) input/output
	; (Retro UNIX Prototype : 14/12/2012 - 01/03/2013, UNIXCOPY.ASM)
        ; ((Modified registers: edx, ebx, ecx, esi, edi))

	xor	edx, edx ; 0
	mov 	[u.nread], edx ; 0
	mov	[u.pcount], dx ; 19/05/2015
	cmp 	[u.count], edx ; 0
	;ja 	short readi_1
	;retn
	; 09/08/2022
	jna	short dskr_5 ; retn
;readi_1:
dskr:
	; 01/05/2016
	; 25/04/2016 - TRDOS 386 (TRDOS v2.0)
	; 24/05/2015 - 12/10/2015 (Retro UNIX 386 v1)
	; 26/04/2013 - 03/08/2013 (Retro UNIX 8086 v1)
dskr_0:
        mov	edx, [i.size]
	mov	ebx, [u.fofp]
	sub	edx, [ebx]
	jna	short dskr_4
	;
	push	eax ; 01/05/2016
	cmp     edx, [u.count] 
	jnb	short dskr_1
	mov	[u.count], edx
dskr_1:
	; EAX = First Cluster
	; [Current_Drv] = Physical drive number 
	call	mget_r
	; NOTE: in 'mget_r', relevant sector will be read in buffer
	; if it is not already in buffer !
	mov	ebx, readi_buffer
	cmp	byte [u.kcall], 0 ; the caller is 'namei' sign (=1)
	ja	short dskr_3	  ; zf=0 -> the caller is 'namei'
	cmp	word [u.pcount], 0
	ja	short dskr_3
dskr_2:
	; [u.base] = virtual address to transfer (as destination address)
	call	trans_addr_w ; translate virtual address to physical (w)
dskr_3:
	; EBX (r5) = system (I/O) buffer address -physical-
	call	sioreg
	xchg	esi, edi
	; EDI = file (user data) offset
	; ESI = sector (I/O) buffer offset
	; ECX = byte count
	rep	movsb
	; eax = remain bytes in buffer
        ;       (check if remain bytes in the buffer > [u.pcount])
	or	eax, eax
	jnz	short dskr_2 ; (page end before system buffer end!)
	pop	eax  ; (first cluster number)
	cmp	[u.count], ecx ; 0
	ja	short dskr_0
dskr_4:
	mov	byte [u.kcall], 0
dskr_5:		; 23/07/2022
	retn

mget_r:
	; 29/08/2023
	; 30/07/2022
	; 23/07/2022 - TRDOS 386 Kernel v2.0.5
	; 24/10/2016
	; 22/10/2016
	; 12/10/2016
	; 29/04/2016
	; 25/04/2016 - TRDOS 386 (TRDOS v2.0)
	; 03/06/2015 (Retro UNIX 386 v1, 'mget', u.5s)
	; 22/03/2013 - 31/07/2013 (Retro UNIX 8086 v1)
	;
	; Get existing or (allocate) a new disk block for file
	; 
	; INPUTS ->
	;    [u.fofp] = file offset pointer
	;    EAX = First Cluster
	;    [cdev] = Logical dos drive number
	;    ([u.off] = file offset)
	; OUTPUTS ->
	;    EAX = logical sector number
	;    ESI = Logical Dos Drive Description Table address
	;
	; Modified registers: EDX, EBX, ECX, ESI, EDI

	mov     esi, [u.fofp]
	mov	ebx, [esi] ; (u.off)

	sub	ecx, ecx
	mov	ch, [cdev]

	mov	esi, Logical_DOSDisks
	add	esi, ecx

	cmp	[readi.valid], cl ; 0
	jna	short mget_r_0
	
	cmp	ch, [readi.drv]
	jne	short mget_r_0

	cmp	eax, [readi.fclust]
	jne	short mget_r_3
	
	mov	eax, ebx ; file offset
	mov	cx, [readi.bpc]
	inc	ecx ; <= 65536
	sub	edx, edx
	div	ecx

	mov	edi, [readi.c_index] ; cluster index

	cmp	eax, edi
        jne     short mget_r_4  ; (*)

	; edx = byte offset in cluster (<= 65535)
	mov	[readi.offset], dx
	; 23/07/2022
	;shr	dx, 9 ; / 512
	shr	edx, 9
	mov	[readi.s_index], dl ; sector index in cluster (0 to spc -1)

	mov	eax, [readi.cluster]  ; > 0 if [readi.valid] = 1
	mov	edx, [readi.fs_index]
        jmp     mget_r_7

mget_r_0:
	mov	[readi.drv], ch ; physical drive number
	cmp	byte [esi+LD_FATType], 0
	ja	short mget_r_1
	mov	cl, [esi+LD_FS_BytesPerSec+1]
	shr	cl, 1 ;  ; 1 for 512 bytes, 4 for 2048 bytes
	jmp	short mget_r_2	
mget_r_1:
	mov	cl, [esi+LD_BPB+BPB_SecPerClust]
mget_r_2:
	mov	[readi.spc], cl  ; sectors per cluster
	; NOTE: readi bytes per sector value is always 512 !
	; 23/07/2022
	;xor	ch, ch
	; 29/08/2023
	shl	ecx, 9 
	;shl	cx, 9 ; * 512
	;dec	cx ; bytes per cluster - 1
	; 23/07/2022
	dec	ecx
	mov	[readi.bpc], cx
	;sub	cx, cx
	; 23/07/2022
	sub	ecx, ecx
mget_r_3:
	mov	[readi.fclust], eax ; first cluster (or FDT address)
	mov	[readi.valid], cl ; 0 
	;mov	[readi.s_index], cl ; 0
	;mov	[readi.offset], cx ; 0
	mov	[readi.c_index], ecx ; 0
	mov	[readi.cluster], ecx ; 0
	mov	[readi.sector], ecx ; 0

	mov	eax, ebx ; file offset
	mov	cx, [readi.bpc]
	inc	ecx ; <= 65536
	sub	edx, edx
	div	ecx
	;mov	edi, [readi.c_index] ; previous cluster index
	sub	edi, edi
mget_r_4:
	mov	[readi.c_index], eax ; cluster index
	; edx = byte offset in cluster (<= 65535)
	mov	[readi.offset], dx
	; 23/07/2022
	;shr	dx, 9 ; / 512
	shr	edx, 9
	mov	[readi.s_index], dl ; sector index in cluster (0 to spc -1)

	mov	ecx, eax ; current cluster index
	mov	eax, [readi.fclust]
	or	ecx, ecx ; cluster index
	jz	short mget_r_6

	cmp	edi, ecx
	ja	short mget_r_5 ; old cluster index is higher
	mov	edx, [readi.cluster]
	and	edx, edx
	jz	short mget_r_5
	; valid 'readi' parameters (*)
	mov	eax, edx
	sub	ecx, edi
	jz	short mget_r_7
mget_r_5:
	; EAX = Beginning cluster
	; EDX = Sector index in disk/file section
	;	(Only for SINGLIX file system!)
	; ECX = Cluster sequence number after the beginning cluster
	; ESI = Logical DOS Drive Description Table address
	call	get_cluster_by_index
	jc	short mget_r_err
	; EAX = Cluster number
mget_r_6:
	mov	[readi.cluster], eax ; FDT number for Singlix File System
mget_r_7:
	cmp	byte [esi+LD_FATType], 0
	jna	short mget_r_12

	;sub	eax, 2
	; 30/07/2022
	dec	eax
	dec	eax
	movzx	edx, byte [readi.spc]
	mul	edx

	add	eax, [esi+LD_DATABegin]
	mov	dl, [readi.s_index]
	add	eax, edx
mget_r_8:
	; eax = logical sector number
	cmp	byte [readi.valid], 0
	jna	short mget_r_9
	cmp	eax, [readi.sector]
	je	short mget_r_11 ; sector is already in 'readi' buffer
mget_r_9:
	mov	[readi.sector], eax
	mov	ebx, readi_buffer ; buffer address
	;mov	ecx, 1
	; 30/07/2022
	xor	ecx, ecx
	inc	cl
	; ecx = 1	

	; 29/04/2016
	;xor	dl, dl

	; EAX = Logical sector number
	; ECX = Sector count
	; EBX = Buffer address
	; (EDX = 0)
	; ESI = Logical DOS drive description table address

	call	disk_read
	jnc	short mget_r_10

	; 22/10/2016 (15h -> 17)
	mov	eax, 17 ; Drive not ready or read error !
mget_r_err:
	mov	[u.error], eax
	; 12/10/2016
	mov	[u.r0], eax
	jmp	error
mget_r_10:
	mov	byte [readi.valid], 1 ; 24/10/2016
	mov	eax, [readi.sector]
mget_r_11:
	retn
mget_r_12:
	; EAX = FDT number
	; EDX = Sector index from FDT sector (0,1,2,3,4...)
	inc	eax ; the first data sector in FS disk section
	mov	[readi.fs_index], edx
	add	eax, edx
	jmp	short mget_r_8

trans_addr_r:
	; 12/10/2016
	; 02/05/2016 - TRDOS 386 (TRDOS v2.0)
	; Translate virtual address to physical address 
	; for reading from user's memory space
	; 04/06/2015 - 18/10/2015 (Retro UNIX 386 v1)

	xor	edx, edx ; 0 (read access sign)
	jmp 	short trans_addr_rw

trans_addr_w:
	; 12/10/2016
	; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
	; Translate virtual address to physical address
	; for writing to user's memory space
	; 04/06/2015 - 18/10/2015 (Retro UNIX 386 v1)
	
	sub	edx, edx
	inc	dl ; 1 (write access sign)
trans_addr_rw:
	push	eax
	push	ebx
	push 	edx ; r/w sign (in DL)
	;
	mov	ebx, [u.base]
	call	get_physical_addr ; get physical address
	jnc	short passc_0
	mov	[u.error], eax
	mov	[u.r0], eax ; 12/10/2016
	;pop	edx
	;pop 	ebx
	;pop	eax
	jmp	error
passc_0:
	test	dl, PTE_A_WRITE ; writable page
	pop	edx
	jnz	short passc_1
	
	and 	dl, dl
	jz	short passc_1
	; read only (duplicated) page -must be copied to a new page-
	; EBX = linear address
	push 	ecx
	call 	copy_page
	pop	ecx
	jc	short passc_2
	push	eax ; physical address of the new/allocated page
	call	add_to_swap_queue
	pop	eax
	and 	ebx, PAGE_OFF ; 0FFFh
	;mov 	ecx, PAGE_SIZE
	;sub	ecx, ebx
	add	eax, ebx
passc_1: 
	mov 	[u.pbase], eax ; physical address
	mov	[u.pcount], cx ; remain byte count in page (1-4096)
	pop	ebx
	pop	eax
	retn
passc_2:
	mov	eax, ERR_MINOR_IM ; "Insufficient memory !" error
	mov	[u.r0], eax ; 12/10/2016
	mov	[u.error], eax
	;pop 	ebx
	;pop	eax
	jmp	error

sioreg: 
	; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
	; 19/05/2015 - 25/07/2015 (Retro UNIX 386 v1)
	; 12/03/2013 - 22/07/2013 (Retro UNIX 8086 v1)
	; INPUTS -> 
	;     EBX = system buffer (data) address (r5)
	;     [u.fofp] = pointer to file offset pointer
	;     [u.base] = virtual address of the user buffer
	;     [u.pbase] = physical address of the user buffer
	;     [u.count] = byte count
	;     [u.pcount] = byte count within page frame
	; OUTPUTS -> 
	;     ESI = user data offset (r1)
	;     EDI = system (I/O) buffer offset (r2)
	;     ECX = byte count (r3)
	;     EAX = remain bytes after byte count within page frame
	;	(If EAX > 0, transfer will continue from the next page)
        ;
	; ((Modified registers:  EDX))
 
        mov     esi, [u.fofp]
        mov     edi, [esi]
	mov	ecx, edi
	or	ecx, 0FFFFFE00h
	and	edi, 1FFh
	add	edi, ebx ; EBX = system buffer (data) address
	neg	ecx
	cmp	ecx, [u.count]
	jna	short sioreg_0
	mov	ecx, [u.count]
sioreg_0:
	cmp	byte [u.kcall], 0
	jna	short sioreg_1
	 ; the caller is 'mkdir' or 'namei'
	mov	eax, [u.base]
	mov 	[u.pbase], eax ; physical address = virtual address
	mov	word [u.pcount], cx ; remain bytes in buffer (1 sector)
	jmp	short sioreg_2
sioreg_1:
	movzx	edx, word [u.pcount]
	cmp	ecx, edx
	ja	short sioreg_4 ; transfer count > [u.pcount]
sioreg_2: ; 2:
	xor 	eax, eax
sioreg_3:
	add 	[u.nread], ecx
	sub 	[u.count], ecx
	add 	[u.base], ecx
        add 	[esi], ecx 
	mov	esi, [u.pbase]
	sub	[u.pcount], cx
	add	[u.pbase], ecx
        retn
sioreg_4:
	; transfer count > [u.pcount]
	; (ecx > edx)
	mov	eax, ecx
	sub	eax, edx ; remain bytes for 1 sector (block) transfer
	mov	ecx, edx ; current transfer count = [u.pcount]
	jmp	short sioreg_3

tswitch: ; Retro UNIX 386 v1
tswap:
	; 16/01/2017
	; 21/05/2016 - TRDOS 386 (TRDOS v2.0)
	; 10/05/2015 - 01/09/2015 (Retro UNIX 386 v1)
	; 14/04/2013 - 14/02/2014 (Retro UNIX 8086 v1)
	; time out swap, called when a user times out.
	; the user is put on the low priority queue.
	; This is done by making a link from the last user
	; on the low priority queue to him via a call to 'putlu'.
	; then he is swapped out.

	; TRDOS 386 (TRDOS v2.0) modification ->  ** 21/05/2016 **
	;     * when a high priority (event) process will be stopped
	;	(swapped out, swithched out/off), 'tswap/tswitch' will
	;	not add it to a run queue. 
	;	/// What for: Process may be already in a run queue, 
	;	it is unspeficied state because process might be started
	;	by a timer event which does not regard previous priority
	;	level and run queue of the process (for fast executing!).
	;	After the 'run for event', process will be sequenced
	;	to run by it's actual run queue. ///
	;
	; Retro UNIX 386 v1 modification ->
	;       swap (software task switch) is performed by changing
	;	user's page directory (u.pgdir) instead of segment change
	;	as in Retro UNIX 8086 v1.
	;
	; RETRO UNIX 8086 v1 modification ->
	;       'swap to disk' is replaced with 'change running segment'
	;	according to 8086 cpu (x86 real mode) architecture.
	;	pdp-11 was using 64KB uniform memory while IBM PC
	;	compatibles was using 1MB segmented memory
	;	in 8086/8088 times.
	;
	; INPUTS ->
	;    u.uno - users process number
	;    runq+4 - lowest priority queue
	; OUTPUTS ->
	;    r0 - users process number
	;    r2 - lowest priority queue address
	;
	; ((AX = R0, BX = R2)) output
	; ((Modified registers: EDX, EBX, ECX, ESI, EDI))
	;

	NOTE:
	;* [u.pri] priority level is specified by run queue which is process 
	;  comes to run from.
	;* Initial [u.pri] is 1 ('normal/regular') for programs 
	;  (which are launched by MainProg or 'sysexec'), it is changed
	;  to 2 ('high') by timer event, if program uses 'systimer' system call.
	;* Program (Process) also can change it's running priority 
	;  from 1 to 0 or up to 2 by using 'syspri' system call; but,
	;  if program selects priority level 2 (high) for running, next time
	;  it is reduced to 1 (normal/regular) because 'syspri' adds this
	;  program to 'run for normal' queue while running duration is a bit
	;  protected from swap/switch out immediate, behalf of other high 
	;  priority process in sequence. Program (with high priority) will not
	;  be swapped/switched out (by timer event) before it's time quantum
	;  will be elapsed, but, this will be temporary if program is not using
	;  timer event function.

	;For example:
	;If a process frequently gets a timer event, it runs at high priority
	;level but when it returns from running it returns to actual run queue,
	;not to 'run for event' queue again.
	;'tswap' will not change the sequence at return/stop(swap out) stage.
	;But if priority level not high (=2, 'run for event'), 'tswap/tswitch'
	;will add the stopping process to relevant run queue according to
	;[u.pri] priority level.

	; 16/01/2017
	mov	ebx, runq+2 	; 'runq_normal' ; normal/regular priority
	; 21/05/2016
	;cmp	byte [u.pri], 2	; high priority (run for event) ?
	;jnb	short swap
	; 16/01/2017
	; (Normal and also high/event priority processes will be added to
	; normal priority run queue for ensuring circular running sequence!)
	; (Timer interrupt or 'syspri' system call may change priority and run
	; queue to high/event level.)
	cmp	byte [u.pri], 0
	ja	short tswap_1	; normal priority run queue
	;
	inc	ebx
	inc	ebx		; runq+4, 'runq_background', low priority
tswap_1:
	mov 	al, [u.uno]
	       	; movb u.uno,r1 / move users process number to r1
		; mov  $runq+4,r2 
			; / move lowest priority queue address to r2
      	; ebx = run queue
	call 	putlu
		; jsr r0,putlu / create link from last user on Q to 
		             ; / u.uno's user

switch: ; Retro UNIX 386 v1
swap:
	; 20/08/2024
	; 02/01/2017
	; 21/05/2016
	; 20/05/2016
	; 02/05/2016
	; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
	; 10/05/2015 - 02/09/2015 (Retro UNIX 386 v1)
	; 14/04/2013 - 08/03/2014 (Retro UNIX 8086 v1)
	;
	; 'swap' is routine that controls the swapping of processes
	; in and out of core.
	;
	; TRDOS 386 (TRDOS v2.0) modification ->  ** 20/05/2016 **
	;     * 3 different priority level is applied
	;	(just as original unix v1)
	;	1) high priority (event) run queue, 'runq_event'
	;	2) normal priority (regular) run queue, 'runq_normal'
	;	3) low priority (background) run queue, 'runq_backgroud'
	;	'swap' code will run a process which has max. priority
        ;       (for earliest event at first)
	;
	; Retro UNIX 386 v1 modification ->
	;       swap (software task switch) is performed by changing
	;	user's page directory (u.pgdir) instead of segment change
	;	as in Retro UNIX 8086 v1.
	;
	; RETRO UNIX 8086 v1 modification ->
	;       'swap to disk' is replaced with 'change running segment'
	;	according to 8086 cpu (x86 real mode) architecture.
	;	pdp-11 was using 64KB uniform memory while IBM PC
	;	compatibles was using 1MB segmented memory
	;	in 8086/8088 times.
	;
	; INPUTS ->
	;    runq table - contains processes to run.
	;    p.link - contains next process in line to be run.
	;    u.uno - process number of process in core
	;    s.stack - swap stack used as an internal stack for swapping.
	; OUTPUTS ->
	;    (original unix v1 -> present process to its disk block)
	;    (original unix v1 -> new process into core -> 
	;	   Retro Unix 8086 v1 -> segment registers changed 
	;	   for new process)
	;    u.quant = 3 (Time quantum for a process)
	; 	((INT 1Ch count down speed -> 18.2 times per second)
	;    RETRO UNIX 8086 v1 will use INT 1Ch (18.2 times per second)
	;	 for now, it will swap the process if there is not
	;	 a keyboard event (keystroke) (Int 15h, function 4Fh)
	;	 or will count down from 3 to 0 even if there is a
	;        keyboard event locking due to repetitive key strokes.
	;	 u.quant will be reset to 3 for RETRO UNIX 8086 v1.
	;
	; ((Modified registers: EAX, EDX, EBX, ECX, ESI, EDI))

	;NOTE:
	;High priority queue is the first for selecting a process to run.
	;If there is not a process in high priority level run queue,
	;a process in normal priority run queue will be selected
	;or a proces in low priority run queue will be selected if normal
	;priority level run queue is empty.
	
	; 21/05/2016 -(3 priority levels, 3 run queues)
	mov	esi, runq ; 'runq_event' ; high priority, 'run for event'
	mov	byte [priority], 3 ; high priority + 1
	xor	ebx, ebx ; 02/01/2017
swap_0: ; 1: / search runq table for highest priority process
	lodsw  ; mov ax, [esi], add esi+2
	;xor	ebx, ebx ; 02/05/2016
	and 	ax, ax ; are there any processes to run in this Q entry
	jnz	short swap_2
	; 21/05/2026
	; runq_normal = runq+2, runq_background = runq+4
	dec	byte [priority] ; 3 -> 3, 2 -> 1, 1-> 0
	jnz	short swap_0
	;cmp	esi, runq+6  ; if zero compare address to end of table
	;jb	short swap_0 ; if not at end, go back
swap_1:
	; 02/05/2016
	; 29/04/2016 (TRDOS 386 = TRDOS v2.0)
	; No user process to run...
	; Run the kernel process... MainProg: Internal Command Interpreter
	inc	al ; mov al, 1  ; process number of MainProg
	inc	bl ; mov bl, al ; 1
	jmp	short swap_4
swap_2:
	; 21/05/2016
	dec	byte [priority] ; priority level of present user/process
			        ; 0, 1, 2
	dec	esi
        dec     esi
	;
	mov	bl, al
	cmp	al, ah ; is there only 1 process in the queue to be run
	je	short swap_3 ; yes
	mov	ah, [ebx+p.link-1]
       	mov	[esi], ah ; move next process in line into run queue
	jmp	short swap_4
swap_3:
	;xor	dx, dx
	; 20/08/2024
	xor	edx, edx
	mov	[esi], dx ; zero the entry; no processes on the Q
swap_4:
	mov 	ah, [u.uno]
	cmp	ah, al ; is this process the same as the process in core?
       	je	short swap_8 ; yes, don't have to swap
	or	ah, ah ; is the process # = 0
       	jz	short swap_6 ; 'sysexit'
	;cmp	ah, al ; is this process the same as the process in core?
       	;je	short swap_8 ; yes, don't have to swap
	mov	[u.usp], esp ; return address for 'syswait' & 'sleep'
	call	wswap  ; write out core to disk
	jmp 	short swap_7
swap_6:
	; Deallocate memory pages belong to the process
	; which is being terminated.
	; (Retro UNIX 386 v1 modification !)
	;
	push	ebx
	mov 	eax, [u.pgdir]  ; page directory of the process
	mov	ebx, [u.ppgdir] ; page directory of the parent process
	call	deallocate_page_dir
	mov	eax, [u.upage] ; 'user' structure page of the process
	call	deallocate_page
	pop	ebx
swap_7:
	shl	bl, 2 ; * 4
	;;;
	mov	eax, [ebx+p.upage-4] ; the 'u' page of the new process
	call	rswap ; read new process into core
swap_8:
	; Retro UNIX  8086 v1 modification !
	mov	byte [u.quant], time_count
	retn

wswap:  ; < swap out, swap to disk >
	; 28/02/2017 (fnsave)
	; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
	; 09/05/2015 (Retro UNIX 386 v1)
	; 26/05/2013 - 08/03/2014 (Retro UNIX 8086 v1)
	; 'wswap' writes out the process that is in core onto its
	; appropriate disk area.
	;
	; Retro UNIX 386 v1 modification ->
	;       User (u) structure content and the user's register content
	;	will be copied to the process's/user's UPAGE (a page for
	;	saving 'u' structure and user registers for task switching).
	;	u.usp - points to kernel stack address which contains
	;		user's registers while entering system call.
	;	u.sp  - points to kernel stack address 
	;		to return from system call -for IRET-.
	;	[u.usp]+32+16 = [u.sp] 
	;	[u.usp] -> edi, esi, ebp, esp (= [u.usp]+32), ebx, 
	;		edx, ecx, eax, gs, fs, es, ds, -> [u.sp].
	;
	; Retro UNIX 8086 v1 modification ->
	;       'swap to disk' is replaced with 'change running segment'
	;	according to 8086 cpu (x86 real mode) architecture.
	;	pdp-11 was using 64KB uniform memory while IBM PC
	;	compatibles was using 1MB segmented memory 
	;	in 8086/8088 times.
	;
	; INPUTS ->
	;    u.break - points to end of program
	;    u.usp - stack pointer at the moment of swap
	;    core - beginning of process program
	;    ecore - end of core 	
	;    user - start of user parameter area
	;    u.uno - user process number
	;    p.dska - holds block number of process
	; OUTPUTS ->
	;    swp I/O queue
	;    p.break - negative word count of process 
	;    r1 - process disk address
	;    r2 - negative word count
	;
	; RETRO UNIX 8086 v1 input/output:
	;
	; INPUTS ->
	;    u.uno - process number (to be swapped out)
	; OUTPUTS ->
	;    none
	;
	;   ((Modified registers: ECX, ESI, EDI))
	;

	; 28/02/2017
	;cmp	byte [multi_tasking], 0  ; Musti tasking mode ?
	;jna	short wswp
	cmp	byte [u.fpsave], 0 ; 28/02/2017
	jna	short wswp
	fnsave	[u.fpregs] ; save floating point registers (94 bytes)
wswp:
	mov	edi, [u.upage] ; process's user (u) structure page addr
	mov	ecx, (U_SIZE + 3) / 4
	mov	esi, user ; active user (u) structure
	rep	movsd
	;
	mov	esi, [u.usp] ; esp (system stack pointer,
			     ;      points to user registers)
	mov	ecx, [u.sp]  ; return address from the system call
			     ; (for IRET)
			     ; [u.sp] -> EIP (user)
			     ; [u.sp+4]-> CS (user)
			     ; [u.sp+8] -> EFLAGS (user)
			     ; [u.sp+12] -> ESP (user)
			     ; [u.sp+16] -> SS (user)
	sub	ecx, esi     ; required space for user registers
	add	ecx, 20	     ; +5 dwords to return from system call
			     ; (for IRET)
	shr	ecx, 2
	rep	movsd
	retn

rswap:  ; < swap in, swap from disk >
	; 28/02/2017 (frstor)
	; 15/01/2017
	; 14/01/2017
	; 21/05/2016
	; 03/05/2016
	; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
	; 09/05/2015 - 15/09/2015 (Retro UNIX 386 v1)
	; 26/05/2013 - 08/03/2014 (Retro UNIX 8086 v1)
	; 'rswap' reads a process whose number is in r1,
	; from disk into core.
	;
	; Retro UNIX 386 v1 modification ->
	;       User (u) structure content and the user's register content
	;	will be restored from process's/user's UPAGE (a page for
	;	saving 'u' structure and user registers for task switching).
	;	u.usp - points to kernel stack address which contains
	;		user's registers while entering system call.
	;	u.sp  - points to kernel stack address 
	;		to return from system call -for IRET-.
	;	[u.usp]+32+16 = [u.sp] 
	;	[u.usp] -> edi, esi, ebp, esp (= [u.usp]+32), ebx,
	;		edx, ecx, eax, gs, fs, es, ds, -> [u.sp].
	;
	; RETRO UNIX 8086 v1 modification ->
	;       'swap to disk' is replaced with 'change running segment'
	;	according to 8086 cpu (x86 real mode) architecture.
	;	pdp-11 was using 64KB uniform memory while IBM PC
	;	compatibles was using 1MB segmented memory
	;	in 8086/8088 times.
	;
	; INPUTS ->
	;    r1 - process number of process to be read in
	;    p.break - negative of word count of process
	;    p.dska - disk address of the process
	;    u.emt - determines handling of emt's
	;    u.ilgins - determines handling of illegal instructions
	; OUTPUTS ->
	;    8 = (u.ilgins)
	;    24 = (u.emt)
	;    swp - bit 10 is set to indicate read 
	;		(bit 15=0 when reading is done)
	;    swp+2 - disk block address
	;    swp+4 - negative word count
	;      ((swp+6 - address of user structure))
	;
	; RETRO UNIX 8086 v1 input/output:
	;
	; INPUTS ->
	;    AL	- new process number (to be swapped in)
	; OUTPUTS ->
	;    none
	;
	;   ((Modified registers: EAX, ECX, ESI, EDI, ESP))
	;
	; Retro UNIX 386 v1 - modification ! 14/05/2015
	mov	esi, eax  ; process's user (u) structure page addr
	mov	ecx, (U_SIZE + 3) / 4
	mov	edi, user ; active user (u) structure
	rep	movsd
	pop	eax	; 'rswap' return address
	;
	;cli
	mov	edi, [u.usp] ; esp (system stack pointer,
			     ;     points to user registers)
	mov	esp, edi     ; 14/01/2017
	mov	ecx, [u.sp]  ; return address from the system call
			     ; (for IRET)
			     ; [u.sp] -> EIP (user)
			     ; [u.sp+4]-> CS (user)
			     ; [u.sp+8] -> EFLAGS (user)
			     ; [u.sp+12] -> ESP (user)
			     ; [u.sp+16] -> SS (user)
	sub	ecx, edi     ; required space for user registers
	add	ecx, 20	     ; +5 dwords to return from system call
			     ; (for IRET)
	shr	ecx, 2
	rep	movsd
	;mov	esp, [u.usp] ; 15/09/2015
	;sti
	; 28/02/2017
	;cmp	byte [multi_tasking], 0  ; Multi tasking mode ?
	;jna	short rswp_retn
	cmp	byte [u.fpsave], 0
	jna	short rswp_retn
	frstor	[u.fpregs] ; restore floating point regs (94 bytes)
			; 108 bytes (22/08/2024)	
rswp_retn:
	push	eax	; 'rswap' return address
	retn

putlu: 
	; 20/05/2016
	; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
	; 10/05/2015 - 12/09/2015 (Retro UNIX 386 v1)
	; 15/04/2013 - 23/02/2014 (Retro UNIX 8086 v1)
	; 'putlu' is called with a process number in r1 and a pointer
	; to lowest priority Q (runq+4) in r2. A link is created from
	; the last process on the queue to process in r1 by putting
	; the process number in r1 into the last process's link.
	;
	; INPUTS ->
	;    r1 - user process number
	;    r2 - points to lowest priority queue
	;    p.dska - disk address of the process
	;    u.emt - determines handling of emt's
	;    u.ilgins - determines handling of illegal instructions
	; OUTPUTS ->
	;    r3 - process number of last process on the queue upon
	;	  entering putlu
	;    p.link-1 + r3 - process number in r1
	;    r2 - points to lowest priority queue
	;
	; ((Modified registers: EDX, EBX))
	;
	; / r1 = user process no.; r2 points to lowest priority queue

	; EBX = r2
	; EAX = r1 (AL=r1b)

	; 20/05/2016
	; AL = process number (1 to 16) // Retro UNIX 8086, 386 v1 //
	;     (max. 16 processes available for current kernel version)
	; EBX = run queue address ; 20/05/2016 (TRDOS 386)
		; which is one of following addresses:
		;  1) 'runq_event' high priority run queue
		;  2) 'runq_normal' normal/regular priority run queue
		;  3) 'runq_background' low priority run queue

	;mov	ebx, runq
	movzx  	edx, byte [ebx]
	inc	ebx
	and	dl, dl
		; tstb (r2)+ / is queue empty?
       	jz	short putlu_1
		; beq 1f / yes, branch
	mov 	dl, [ebx] ; 12/09/2015
		; movb (r2),r3 / no, save the "last user" process number
			     ; / in r3
       	mov	[edx+p.link-1], al
		; movb r1,p.link-1(r3) / put pointer to user on
			     ; / "last users" link
	jmp	short putlu_2
		; br 2f /
putlu_1: ; 1:
	mov	[ebx-1], al
       		; movb r1,-1(r2) / user is only user; 
			    ; / put process no. at beginning and at end
putlu_2: ; 2: 
	mov	[ebx], al
       		; movb r1,(r2) / user process in r1 is now the last entry
			     ; / on the queue
	mov	dl, al
        mov     [edx+p.link-1], dh ; 0
		; dec r2 / restore r2
        retn
		; rts r0

sysver:
	; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
	mov	dword [u.r0], 200h ; AH = major version, AL = minor version 
	jmp	sysret

syspri: ; change running priority (of the process)
	; 23/07/2022 - TRDOS 386 v2.0.5
	; 21/05/2016
	; 20/05/2026 - TRDOS 386 (TRDOS v2.0)
	; INPUT ->
	;	BL = priority level
	;	   0 = low running priority (running on background)
	;	   1 = normal/regular priority (running as regular)
	;	   2 = high/event priority (running for event)
	;	   >2 = invalid, it will accepted as 2 (event)
	;	   0FFh = get/return current running priority only
	; OUTPUT ->
	;	* if current [u.pri] < 2
	;	  if BL input < 0FFh ->
	;	     [u.pri] is updated as in BL input (0,1,2)
	;	  if BL input = 0FFh -> AL = [u.pri] (current)
	;
	;	* if current [u.pri] = 2
	;	  if BL input < 0FFh -> cf = 1 & AL = 2
	;	  if BL input = 0FFh -> cf = 0 & AL = 2
	;
	;	NOTE:
	;	If [u.pri] = 2, it can not be changed to 1 or 0;
	;	because, run queue of the running process is unspecified
	;	at this	stage. Process might be started by a timer event
	;	or priority might be changed to high by previous
	;	'syspri' system	call. In both cases, the process is in
	;	'runq_normal' or 'runq_background' queue.
	;	As result of this fact, when the [u.quant] time quantum
	;	of the process is elapsed or 'sysrele' system call is
	;	instructed by the process, 'tswap' ('tswitch') procedure
	;	will be called (to 'swap' or 'switch' out the procedure)
	;	and it will not call 'putlu' to add the (stopping)
	;	process to relevant run queue when [u.pri] = 2.
	;	(Otherwise, it would be possible to add process to
	;	a run queue while it is already in a run queue, wrongly.)
  	;
	;	If [u.pri]< 2, 'tswap/tswitch' procedure will call
	;	'putlu' to add process to relevant run queue
	;	according to [u.pri] value. ('runq_normal' for 1,
	;	'runq_background' for 0).
	;
	;	If BL input >= 2 and < 0FFh while [u.pri] < 2,
	;	process will be added to 'runq_normal' queue and
	;	[u.pri] will be set to 2. (in 'syspri' system call)
	;

	sub	eax, eax ; 0
	mov	[u.error], eax

	mov	al, [u.pri]
	mov	[u.r0], eax

	inc	bl
	;jz	sysret ; 0FFh -> 0, get priority level
	; 23/07/2022
	jz	short syspri_2	; jmp sysret

	cmp	al, 2
	;jnb	error ; CF = 1 & AL = 2 (& last error = 0)
	; 23/07/2022
	jb	short syspri_0
	jmp	error
syspri_0:
	dec	bl
	cmp	bl, 2
	jna	short syspri_1
	mov	bl, 2
syspri_1:
	mov	[u.pri], bl
	cmp	bl, 2
	;jb	sysret
	; 23/07/2022
	jb	short syspri_2	; jmp sysret

	; here...
	; Priority of current process has been changed to high
	; ('run for event') but current process will be added to
	; 'run as normal' queue. ('run for event' high priority
	; queue is under control of timer -& RTC- interrupt only!)
	;
	; (Otherwise, process can fall into black hole!
	; e.g. if it is not in waiting list and it has not got
	; a timer event and it is not in a run queue!
	; Because, when [u.pri] is 2, 'tswap/tswitch' will not
	; add the stopping process to a run queue.)

	mov	al, [u.uno]
	mov	ebx, runq_normal ; normal priority !
				 ; [u.pri] is set to high
				 ; but 'runq_event' queue is set
				 ; only by the kernel's timer
				 ; event function (timer interrupt). 
	call	putlu
syspri_2:
	jmp	sysret

cpass: ; / get next character from user area of core and put it in AL (r1)
	; 30/07/2022 - TRDOS 386 Kernel v2.0.5
	; 02/05/2016 - TRDOS 386 (TRDOS v2.0)
	; 19/05/2015 - 18/10/2015 (Retro UNIX 386 v1)
	; 14/08/2013 - 20/09/2013 (Retro UNIX 8086 v1)
	; INPUTS -> 
	;     [u.base] = virtual address in user area
	;     [u.count] = byte count (max.)
	;     [u.pcount] = byte count in page (0 = reset)
	; OUTPUTS -> 
	;     AL = the character which is pointed by [u.base]
	;     zf = 1 -> transfer count has been completed
        ;
	; ((Modified registers: EAX, EDX, ECX))
	
	; 30/07/2022
	sub	eax, eax
	
	cmp	[u.count], eax ; 0
	;cmp 	dword [u.count], 0  ; have all the characters been transferred
			    	    ; i.e., u.count, # of chars. left
	jna	short cpass_3	    ; to be transferred = 0?) yes, branch
	dec	dword [u.count]	    ; no, decrement u.count
        ; 19/05/2015 
	;(Retro UNIX 386 v1 - translation from user's virtual address
	;		      to physical address
	; 30/07/2022
	cmp	[u.pcount], ax ; 0
	;cmp	word [u.pcount], 0 ; byte count in page = 0 (initial value)
			     ; 1-4095 --> use previous physical base address
			     ; in [u.pbase]
	ja	short cpass_1
	; 30/07/2022
	cmp	[u.ppgdir], eax ; 0
	;cmp	dword [u.ppgdir], 0 ; is the caller os kernel
        je      short cpass_k       ; (sysexec, '/etc/init') ?  (MainProg)
	call	trans_addr_r
cpass_1:
	dec	word [u.pcount]
cpass_2: 
	mov	edx, [u.pbase]
	mov	al, [edx]	; take the character pointed to
				; by u.base and put it in r1
	inc	dword [u.nread] ; increment no. of bytes transferred
	inc	dword [u.base]  ; increment the buffer address to point to the
			        ; next byte
	inc	dword [u.pbase]
cpass_3:
	retn
cpass_k:
	; 02/07/2015
	; The caller is os kernel
	; (get sysexec arguments from kernel's memory space)
	mov	ebx, [u.base]
        mov     word [u.pcount], PAGE_SIZE ; 4096
	mov	[u.pbase], ebx
	jmp	short cpass_2

transfer_to_user_buffer: ; fast transfer
	; 27/05/2016
	; 16/05/2016 - TRDOS 386 (TRDOS v2.0)
	;
	; INPUT ->
	;	ESI = source address in system space
	;	EDI = user's buffer address
	;	ECX = transfer (byte) count
	;	[u.pgdir] = user's page directory
	; OUTPUT ->
	;	ECX = actual transfer count
	;	cf = 1 -> error
	;	[u.count] = remain byte count
	;
	; Modified registers: eax, ecx
	;

	and	ecx, ecx
	jz	short ttub_4

	mov	[u.count], ecx
	
	push	edi
	push	esi
	push	ebx
	push	edx
	push	ecx

	mov	ebx, edi
	add	ebx, CORE ; 27/05/2016
ttub_1:
	; ebx = virtual (linear) address
	; [u.pgdir] = user's page directory
       	call	get_physical_addr_x ; get physical address
	jc	short ttub_5
	; eax = physical address 
	; ecx = remain byte count in page (1-4096)
	mov	edi, eax
	mov	eax, [u.count]
	cmp	ecx, eax
	jna	short ttub_2
	mov	ecx, eax
ttub_2:	
	sub	eax, ecx
	add	ebx, ecx
	rep	movsb
	mov	[u.count], eax
	or	eax, eax
	jnz	short ttub_1
ttub_retn:
tfub_retn:
	pop	ecx ; transfer count = actual transfer count
ttub_3:
	pop	edx
	pop	ebx
	pop	esi
	pop	edi
ttub_4:
	retn
ttub_5:
	pop	ecx
	sub	ecx, [u.count] ; actual transfer count
	stc
	jmp	short ttub_3

transfer_from_user_buffer: ; fast transfer
	; 27/05/2016
	; 16/05/2016 - TRDOS 386 (TRDOS v2.0)
	;
	; INPUT ->
	;	ESI = user's buffer address
	;	EDI = destination address in system space
	;	ECX = transfer (byte) count
	;	[u.pgdir] = user's page directory
	; OUTPUT ->
	;	ecx = actual transfer count
	;	cf = 1 -> error
	;	[u.count] = remain byte count
	;
	; Modified registers: eax, ecx
	;

	and	ecx, ecx
	;jz	short tfub_4
	jz	short ttub_4

	mov	[u.count], ecx

	push	edi
	push	esi
	push	ebx
	push	edx
	push	ecx

	mov	ebx, esi
	add	ebx, CORE ; 27/05/2016
tfub_1:
	; ebx = virtual (linear) address
	; [u.pgdir] = user's page directory
       	call	get_physical_addr_x ; get physical address
	;jc	short tfub_5
	jc	short ttub_5
	; eax = physical address 
	; ecx = remain byte count in page (1-4096)
	mov	esi, eax
	mov	eax, [u.count]
	cmp	ecx, eax
	jna	short tfub_2
	mov	ecx, eax
tfub_2:	
	sub	eax, ecx
	add	ebx, ecx
	rep	movsb
	mov	[u.count], eax
	or	eax, eax
	jnz	short tfub_1

	jmp	short tfub_retn

;tfub_retn:
;	pop	ecx ; transfer count = actual transfer count
;tfub_3:
;	pop	edx
;	pop	ebx
;	pop	esi
;	pop	edi
;tfub_4:
;	retn
;tfub_5:
;	pop	ecx
;	sub	ecx, [u.count] ; actual transfer count
;	stc
;	jmp	short tfub_3

sysfff: ; <Find First File>
	; 18/05/2025 (TRDOS 386 Kernel v2.0.10)
	; 25/08/2024 (TRDOS 386 Kernel v2.0.9)
	; 08/08/2022
	; 30/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 17/10/2016
	; 16/10/2016
	; 15/10/2016 TRDOS 386 (TRDOS v2.0) feature only !
	;           -derived from TRDOS v1.0, INT_21H.ASM-
	;            ("loc_INT21h_find_first_file")
	; TRDOS 8086 (v1.0)
        ; 	07/08/2011 
        ;	Find First File
	;	INPUT:
        ;	    CX= Attributes
        ;	    DS:DX= Pointer to filename
	;	MSDOS OUTPUT:
	;	    DTA: (Default address: PSP offset 80h)
	;	    Offset  Descrription
	;	    0	    Reserved for use find next file
	;	    21	    Attribute of file found
	;	    22	    Time stamp of file
	;	    24	    Date stamp of file
	;	    26	    File size in bytes
	;	    30	    Filename and extension (zero terminated)
	;	If cf = 1:
	;	    Error Codes: (in AX)
	;	    	2 - File not found
	;	       18 - No more files
        ;
	; TRDOS 386 (v2.0)
	; 15/10/2016
	;
        ; INPUT ->
        ;	   CL = File attributes
	;     	      bit 0 (1) - Read only file (R)
	;             bit 1 (1) - Hidden file (H)
        ;             bit 2 (1) - System file (R)
	;             bit 3 (1) - Volume label/name (V)
        ;             bit 4 (1) - Subdirectory (D)
	;	      bit 5 (1) - File has been archived (A)
	;	   CH = 0 -> Return basic parameters (24 bytes)
	;	   CH > 0 -> Return FindFile structure/table (128 bytes)
        ;          EBX = Pointer to filename (ASCIIZ) -path-
	;	   EDX = File parameters buffer address
	;		(buffer size = 24 bytes if CH input = 0)
	;		(buffer size = 128 bytes if CH input > 0)
	;
	; OUTPUT ->
	;	   EAX = 0 if CH input > 0
	;	   EAX = First cluster number of file if CH input = 0
	;	   EDX = File parameters table/structure address
	;	   Basic Parameters:
	;		Offset  Description
	;		------	---------------
	;		0	File Attributes
	;		1	Ambiguous filename chars are used sign
	;			(0 = filename fits exactly with request)
	;			(>0 = ambiguous filename chars are used)
	;	      	2	Time stamp of file
	;		4	Date stamp of file
	;		6	File size in bytes
	;		10	Short Filename (ASCIIZ, max. 13 bytes)
	;		23	Longname Length (1-255) if existing
	;
	;          cf = 1 -> Error code in AL
	;
	; Modified Registers: EAX (at the return of system call)
	;
	; TR-DOS FindFile (FFF) Structure (128 bytes):
	; 09/10/2011 (DIR.ASM) - 10/02/2016 (trdoskx.s)
	;
	; Offset	Parameter		Size
	; ------	------------------	--------
	; 0		FindFile_Drv		1 byte
	; 1		FindFile_Directory	65 bytes
	; 66		FindFile_Name		13 bytes
	; 79		FindFile_LongNameEntryLength 1 byte
	;Above 80 bytes form
	;TR-DOS Source/Destination File FullName Format/Structure
	; 80		FindFile_AttributesMask 1 word
	; 82		FindFile_DirEntry	32 bytes (*)
	; 114		FindFile_DirFirstCluster 1 double word
	; 118		FindFile_DirCluster	1 double word
	; 122		FindFile_DirEntryNumber 1 word
	; 124		FindFile_MatchCounter	1 word
	; 126		FindFile_Reserved	1 word
   	; (*) MS-DOS, FAT 12-16-32 classic directory entry (32 bytes)
	;
	; ***********************************************************
	;
	; 18/05/2025 (Major Modification)
	; ------ TR-DOS 386 v2.0.10 --------
	; FindFile - SysFFF/SysFNF Structure (<=281 bytes):
	;
	; Offset	Parameter		Size
	; ------	------------------	--------
	; 0		FindFile_DirEntry	32 bytes (*)
	; 32		FindFile_Drv		1 byte
	; 33		FindFile_Directory	65 bytes (104 bytes)
	; 98		FindFile_Name		13 bytes
	; 111		FindFile_LongNameEntryLength 1 byte
	; 112		FindFile_Longname	128 bytes (max.)
	;
	; Parameters table/structure size: max. 279 bytes
	;
	; Return Options:
	;	CH = 0 -> basic (24 bytes)
	;	CH bits
	;		bit 0 - FindFile_DirEntry
	;		bit 1 - FindFile_Drv
	;		bit 2 - FindFile_Directory (65 bytes)
	;		bit 3 - FindFile_Name
	;		bit 4 - FindFile_LongNameEntryLength
	;		bit 5 - FindFile_Longname (66 bytes)
	;		bit 6 -	use 104 bytes buf for directory
	;		bit 7 - use 128 bytes buf for longname
	; 	examples:
	;	  1Fh -	00011111b needs 112 bytes buffer
	;	  0Fh -	01001111b needs 150 bytes buffer
	;	 0EFh -	11101111b needs 278 bytes buffer
	;	 0FFh -	11111111b needs 279 bytes buffer
	; Note:
	;	if EDX input is 0
	;		Only File Size will be returned in EAX (*)
	;	if CH > 0 but
	;	   CH & 3Fh is 0, buffer will not used (*)
	;
	; ***********************************************************
	;

	;mov	[u.namep], ebx
	; 16/10/2016
	mov	[FFF_UBuffer], edx
	mov	[FFF_Attrib], cx ; [FFF_RType] = ch
		    ; Attributes in CL, return data type in CH
	mov	esi, ebx
	; file name is forced, change directory as temporary
	;mov	ax, 1
	;mov	[FFF_Valid], ah ; 0 ; reset ; 17/10/2016
	;call	set_working_path
	call	set_working_path_x ; 17/10/2016
	jnc	short sysfff_0  ; esi = FindFile_Name

	and	eax, eax  ; 0 -> Bad Path!
	jnz	short sysfff_err

	; eax = 0
	mov	eax, ERR_DIR_NOT_FOUND ; Directory not found !
sysfff_err:
	mov	[u.r0], eax
	mov	[u.error], eax
        call 	reset_working_path
	jmp	error

sysfff_0:
	; 18/05/2025 - TRDOS 386 v2.0.10
	; [FFF_valid] = 0 (from set_working_path_x)
	mov	ax, [mpid]
	mov	[FFF_mpid], ax	; save user process id/number
			 	; (will be used by SysFNF)
	mov	ah, 0

	; 19/05/2025
	mov	al, [Current_FATType]
	mov	[FFF_FATtype], al
	cmp	al, ah ; 0
	ja	short sysfff_@

	; SysFFF singlix fs procedure(s)
	call	find_first_fs_file
	jc	short sysfff_err

	; all of data has been transferred and eax return
	; value has been set in SysFFF singlix fs proc

sysfff_fs_ok:
sysfnf_fs_ok:	; 19/05/2025
	mov	[u.r0], eax
        call 	reset_working_path
	jmp	sysret

sysfff_@:
	;;;
	; 25/08/2024 (bugfix)
	;mov	al, [esp] ; ???
	mov	al, [FFF_Attrib]
	;;;
	or	al, al
	jz	short sysfff_2
	mov	ah, 10h
	test	al, 08h
	jnz	short sysfff_1
	or	ah, 08h
sysfff_1:
	and	al, 10h ; Directory
	jz	short sysfff_2
	and	ah, 08h
	xor	al, al ; When a directory is searched,
		       ; filename will be returned even if
		       ; it is not a directory!
		       ; Because: (in order to prevent
		       ; creating a dir with existing file name)
		       ; Dir and file names must not be same!
		       ; (return attribute must be checked)
sysfff_2:
	; AX = Attributes mask
		; AL = AND mask (result must be equal to AL)
		; AH = Negative AND mask (result must be ZERO)
 	; ESI = FindFile_Name address

	call	find_first_file
	jc	short sysfff_err ; eax = 2 (File not found !)

	; ESI = Directory Entry (FindFile_DirEntry) Location
	; EDI = Directory Buffer Directory Entry Location
	; EAX = File Size
	;  BL = Attributes of The File/Directory
	;  BH = Long Name Yes/No Status (>0 is YES)
	;; DX > 0 : Ambiguous filename chars are used
	; 18/05/2025
	;  DL > 0 : Ambiguous filename chars are used

sysfff_3:
	; 16/10/2016
	mov	cx, [FFF_Attrib]
	; Attribs in CL, return data type in CH

	;or	cl, cl
	;jz	short sysfff_4 ; 0 = No filter
	xor	cl, 0FFh
	; cl = negative attributes ; 25/08/2024
	and	cl, bl
	jz	short sysfff_4

	;mov	eax, 2 ; 'file not found !' error
	;jmp	short sysfff_err_1

	; 16/10/2016
	call	find_next_file
	jc	short sysfff_err ; eax = 12 (no more files !)
	jmp	short sysfff_3

sysfff_4:
	; 18/05/2025 - TRDOS 386.v2.0.10
	cmp	dword [FFF_UBuffer], 0
	ja	short sysfff_8
sysfff_24:
	; eax = file size
	;mov	[u.r0], eax
	mov	ecx, eax
	mov	byte [FFF_valid], 24 ; basic/default
	mov	byte [FFF_RType], 0  ; basic/default
	jmp	short sysfff_9 ; reset working path & sysret
sysfff_8:
	and	ch, ch ; [FFF_RType]
	;jz	short sysfff_5
	; 25/08/2024
	jnz	short sysfff_7

	;mov	ecx, 128 ; ; transfer length
	; 30/07/2022
	;sub	ecx, ecx
	;mov	cl, 128
	;mov	[FFF_Valid], cl
	; 25/08/2024
	;mov	byte [FFF_Valid], 128 ; (*)
	;jmp	short sysfff_7 ; sysfnf_11

sysfff_5:
	;;mov	esi, FindFile_DirEntry
	;mov	ecx, 24  ; transfer length
	; 30/07/2022
	;sub	ecx, ecx
	;mov	cl, 24
	;mov	[FFF_Valid], cl
	mov	byte [FFF_Valid], 24 ; (*)
;sysfnf_12:
sysfnf_4:	; 19/05/2025
	mov	edi, DTA ; FFF data transfer address
	;mov	al, [esi+DirEntry_Attr] ; 11
	mov	al, bl ; File/Dir Attributes
	mov	[edi+23], bh ; Longname length (0= none)
	stosb
	; 18/05/2025
	;mov	al, dl ; DL is for '?'
	;add	al, dh ; DH is for '*'
	mov	al, dl ; bit 0,1 for '?' and bit 2,3 for '*'
	; AL > 0 if ambiguous file name wildcards are used
	stosb
	mov	eax, [esi+DirEntry_WrtTime] ; 22
        stosd	; DirEntry_WrtTime & DirEntry_WrtDate
        mov	eax, [esi+DirEntry_FileSize] ; 28
        stosd
	mov	ax, [esi+DirEntry_FstClusHI] ; 20
	;shl	ax, 16
	; 23/07/2022 (BugFix)
	shl	eax, 16 
	mov	ax, [esi+DirEntry_FstClusLO] ; 26
	mov	[u.r0], eax ; First Cluster

        ;mov	esi, FindFile_DirEntry
	call	get_file_name

	; 25/08/2024
	; ecx <= 7 (from 'get_file_name')
	;mov	cl, [FFF_Valid] ; (*)
       	mov	esi, DTA ; FFF data transfer address

	; 18/05/2025
;;sysfff_6:
	; 25/08/2024
	;sub	ecx, ecx
	mov	cl, [FFF_Valid] ; (*) ecx <= 128
	; 18/05/2025
	; cl = 24 here (basic FFF/FNF data)
;sysfff_6:	; cl > 0
	mov	edi, [FFF_UBuffer] ; user's buffer address (edx)
	; 20/05/2025 (jmp from 'sysfff_23:')
sysfff_6:
	; ecx = transfer byte count = byte [FFF_valid]

	call	transfer_to_user_buffer

	; 18/05/2025
	test	byte [FFF_RType], 1 ; bit 0 - FindFile_DirEntry
	jz	short sysfff_9
	; ecx = actual transfer count
	add	ecx, 32 ; + Directory Entry size
sysfff_9:	; 18/05/2025
	mov	[u.r0], ecx ; actual transfer count
        call 	reset_working_path
	jmp	sysret

sysfff_7:
	; 18/05/2025
	; (bit 6 & bit 7 are valid if other bits are not zero)
	test	ch, 3Fh ; bit 0 to bit 5 ; wrong flags ?
	jz	short sysfff_24 ; perform as zero buffer size

	; 19/05/2025
sysfnf_5:
	; 25/08/2024
	;mov	byte [FFF_Valid], 128 ; (*)
	;;;;
	; 18/05/2025 - TRDOS 386 v2.0.10
	test	ch, 1 ; bit 0 - FindFile_DirEntry
	jz	short sysfff_10
	push	ecx
	; 20/05/2025
	mov	edi, [FFF_UBuffer]
	mov	esi, FindFile_DirEntry
	mov	ecx, 32
	call	transfer_to_user_buffer
	add	edi, 32
	; 20/05/2025 ; (*!*)
	;mov	[FFF_UBuffer], edi
	mov	[FFF_valid], al
	pop	ecx
sysfff_10:
	; 20/05/2025
	push	edi ; *!*
	mov	edi, TextBuffer ; temporary buffer
			; max. 279-32 bytes
	test	ch, 2	; bit 1 - FindFile_Drv
	jz	short sysfff_11 ; skip
	mov	esi, FindFile_Drv
	movsb
sysfff_11:
	test	ch, 4	; bit 2 - FindFile_Directory
	jz	short sysfff_18 ; skip

	push	ecx ; *
	test	ch, 40h	; bit 6 - use 104 bytes
	jnz	short sysfff_16

	; shorten it if > 65 bytes (asciiz)
	xor	ecx, ecx
	mov	esi, FindFile_Directory
	push	esi
sysfff_12:
	inc	ecx
	lodsb
	;cmp	al, 0
	;ja	short sysfff_12
	or	al, al
	jnz	short sysfff_12
	cmp	ecx, 65
	ja	short sysfff_13
	pop	esi ; FindFile_Directory
	rep	movsb
	jmp	short sysfff_17
sysfff_13:
	sub	esi, 65-4 ; '.../' + 61
	; look for the 1st '/'
	mov	cl, 66
sysfff_14:
	dec	ecx
	lodsb
	cmp	al, '/'
	jne	short sysfff_14
	sub	esi, 4
	mov	dword [esi], '.../'
	push	edi
	rep	movsb
	pop	edi
	add	edi, 65
	jmp	short sysfff_17
sysfff_16:
	mov	esi, FindFile_Directory
	mov	ecx, 104/4
	rep	movsd
sysfff_17:
	pop	ecx ; *
sysfff_18:
	test	ch, 8	; bit 3 - FindFile_Name
	jz	short sysfff_19 ; skip
	mov	esi, FindFile_Name
	movsd
	movsd
	movsd
	movsb
sysfff_19:
	; 20/05/2025
	test	ch, 48 ; 32+16
	jz	short sysfff_23

	push	ecx
	; condition:
	; same checksum & [LongNameFound] = 1
	;		  (last LDIR_Ord & NOT 40h)
	mov	esi, FindFile_DirEntry ; checksum
	call	validate_long_name 
			; (in 'find_longname')
	pop	ecx
	jc	short sysfff_25 ; cf = 1 & eax = 0

	mov	esi, FindFile_LongNameEntryLength
	lodsb
sysfff_25:	; 20/05/2025
	test	ch, 16	; bit 4 - FindFile_LNEL
	jz	short sysfff_20 ; skip

	stosb
sysfff_20:
	test	ch, 32	; bit 5 - FindFile_Longname
	jz	short sysfff_23 ;  skip

	and	al, al  ; zero ?
	jz	short sysfff_22 ; yes

	;mov	esi, FindFile_LongName
	; 20/05/2025
	mov	esi, LongFileName ; asciiz
			; (130 bytes)
			; ((space: 132 bytes))

	test	ch, 80h	; bit 6 - use 128 bytes
	mov	ecx, 127
	jnz	short sysfff_21
	mov	cl, 65
sysfff_21:
	rep	movsb
	sub	al, al ; 0
	; 66th or 128th byte is zero
sysfff_22:
	stosb

sysfff_23:
	; 18/05/2025 - TRDOS 386.v2.0.10
	mov	ecx, edi
	mov	esi, TextBuffer
	sub	ecx, esi
	; ecx  <= 247 (279-32)
	mov	[FFF_valid], cl
	; 20/05/2025
	pop	edi ; *!*
	jmp	short sysfff_6
	;;;;
	
; 18/05/2025
%if 0
sysfnf_11:
	; ecx = 128
	; 25/08/2024 (bugfix)
	; 08/08/2022
	mov	esi, FindFile_Drv
	; 25/08/2024
	sub	ecx, ecx ; 0
	jmp	short sysfff_6
%endif

sysfnf: ; <Find Next File>
	; 19/05/2025
	; 18/05/2025 (TRDOS 386 Kernel v2.0.10)
	; 25/08/2024 (TRDOS 386 Kernel v2.0.9)
	; 29/08/2023 (TRDOS 386 Kernel v2.0.6)
	; 08/08/2022
	; 30/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 16/10/2016 TRDOS 386 (TRDOS v2.0) feature only !
	;           -derived from TRDOS v1.0, INT_21H.ASM-
	;            ("loc_INT21h_find_next_file")
	; TRDOS 8086 (v1.0)
        ; 	07/08/2011 
        ;	Find First File
	;	INPUT:
        ;	    none
	;	MSDOS OUTPUT:
	;	    DTA: (Default address: PSP offset 80h)
	;	    Offset  Descrription
	;	    0	    Reserved for use find next file
	;	    21	    Attribute of file found
	;	    22	    Time stamp of file
	;	    24	    Date stamp of file
	;	    26	    File size in bytes
	;	    30	    Filename and extension (zero terminated)
	;	If cf = 1:
	;	    Error Codes: (in AX)
	;	       18 - No more files
        ;
	; TRDOS 386 (v2.0)
	; 16/10/2016
	;
        ; INPUT ->
       	; 	   none
	; OUTPUT ->
	;	   EAX = 0 if CH input of 'Find First File' > 0
	;	   EAX = First cluster number of file
	;		 if CH input of 'Find First File' = 0
	;	   EDX = File parameters table/structure address
	;
	;          cf = 1 -> Error code in AL
	;
 	; Modified Registers: EAX (at the return of system call)
	;
	; Note: If byte [FFF_Valid] = 0
	;	'sysfnf' will return with 'no more files' error.
	;	If byte [FFF_Valid] = 24
	;	'sysfnf' will return with 24 bytes basic parameters
	;	at the address which is in EDX.
	;	If byte [FFF_Valid] = 128
	;	'sysfnf' will return with 128 bytes Find File
	;	Structure/Table at the address which is in EDX.
	;
	; ***********************************************************
	;
	; 18/05/2025 (Major Modification)
	; ------ TR-DOS 386 v2.0.10 --------
	; FindFile - SysFFF/SysFNF Structure (<=281 bytes):
	;
	; Offset	Parameter		Size
	; ------	------------------	--------
	; 0		FindFile_DirEntry	32 bytes (*)
	; 32		FindFile_Drv		1 byte
	; 33		FindFile_Directory	65 bytes (104 bytes)
	; 98		FindFile_Name		13 bytes
	; 111		FindFile_LongNameEntryLength 1 byte
	; 112		FindFile_Longname	128 bytes (max.)
	;
	; Parameters table/structure size: max. 279 bytes
	;
	; Return Options:
	;	CH = 0 -> basic (24 bytes)
	;	CH bits
	;		bit 0 - FindFile_DirEntry
	;		bit 1 - FindFile_Drv
	;		bit 2 - FindFile_Directory (65 bytes)
	;		bit 3 - FindFile_Name
	;		bit 4 - FindFile_LongNameEntryLength
	;		bit 5 - FindFile_Longname (66 bytes)
	;		bit 6 -	use 104 bytes buf for directory
	;		bit 7 - use 128 bytes buf for longname
	; 	examples:
	;	  1Fh -	00011111b needs 112 bytes buffer
	;	  0Fh -	01001111b needs 150 bytes buffer
	;	 0EFh -	11101111b needs 278 bytes buffer
	;	 0FFh -	11111111b needs 279 bytes buffer
	; Note:
	;	if EDX input is 0
	;		Only File Size will be returned in EAX (*)
	;	if CH > 0 but
	;	   CH & 3Fh is 0, buffer will not used (*)
	;
	; ***********************************************************
	;

	cmp	byte [FFF_Valid], 0
	ja	short sysfnf_0

sysfnf_nmf:
 	; 'no more files !' error
	;mov	eax, ERR_NO_MORE_FILES ; 12
	; 30/07/2022
	sub	eax, eax
	mov	al, ERR_NO_MORE_FILES ; 12
sysfnf_err:	; 19/05/2025
	mov	[u.r0], eax
	mov	[u.error], eax
	jmp	error

sysfnf_0:
	; 18/05/2025 - TRDOS 386 v2.0.10
	mov	ax, [mpid]
	cmp	ax, [FFF_mpid] ; same user process ?
	jne	short sysfnf_nmf ; no ; it is improper sys call!

; 19/05/2025
%if 0
	;cmp	byte [FFF_Valid], 128
	;je	short sysfnf_1
	;cmp	byte [FFF_Valid], 24
	;je	short sysfnf_1
	;mov	[FFF_Valid], 24 ; Default
sysfnf_1:
	movzx	ebx, byte [Current_Drv]
	mov	[SWP_DRV], bx
	mov	dl, [FindFile_Drv]
	cmp	dl, bl
	jne	short sysfnf_2
	xchg	bh, bl
	mov	esi, Logical_DOSDisks
	add	esi, ebx
	jmp	short sysfnf_3

sysfnf_8:
	call	load_FAT_sub_directory
	jc	short sysfnf_err_1 ; read error (no FNF stop)

sysfnf_9:
	call	find_next_file
	jc	short sysfnf_5
	; 25/08/2024
	; esi = Directory Entry (FindFile_DirEntry) Location
	;; 08/08/2022
	;; esi = FindFile_Drv ; wrong ! ; 25/08/2024

	mov	al, [FFF_Attrib]
	;or	al, al
	;jz	short sysfnf_10 ; 0 = No filter
	xor	al, 0FFh
	and	al, bl
	jnz	short sysfnf_9 ; search for next file until
			       ; an error return from
			       ; find_next_file procedure
sysfnf_10:
        ;movzx	ecx, byte [FFF_Valid]
	;cmp	cl, 128 ; complete FindFile structure/table
	;je	sysfnf_11
	;;cmp	cl, 24  ; basic parameters
	;;je	sysfnf_12
	;jmp	sysfnf_12
	; 30/07/2022
	;movzx	ecx, byte [FFF_Valid]
	; 25/08/2024
	;cmp	cl, 128
	cmp	byte [FFF_Valid], 128
	;jne	short sysfnf_12
	;jmp	short sysfnf_11 (*)
	;; 08/08/2022
	;;je	short sysfnf_6 ; esi = FindFile_Drv
	; 29/08/2023 (BugFix)
	;je	short sysfff_6 ; esi = FindFile_Drv
	; 25/08/2024 (BugFix of BugFix) (*)
	je	short sysfnf_11 ; esi <> FindFile_Drv

	jmp	sysfnf_12

sysfnf_2:
	inc	byte [SWP_DRV_chg]

	call	change_current_drive
	jc	short sysfnf_err_1 ; read error !
				   ; (do not stop, because
				   ; we don't have a
				   ; 'no more files'
				   ; -file not found- error,
				   ; next sysfnf system call
				   ; may solve the problem,
				   ; after re-placing the disk)
sysfnf_3:
	mov	eax, [FindFile_DirCluster]
	and	eax, eax
	jnz	short sysfnf_6
        
	cmp	byte [Current_FATType], 2
	ja	short sysfnf_err_0 ; invalid, we neeed to stop !?
	cmp	byte [Current_FATType], 1
	jb	short sysfnf_err_0 ; invalid, we neeed to stop !?

	cmp	byte [DirBuff_ValidData], al ; 0
	jna	short sysfnf_4 

	cmp	eax, [DirBuff_Cluster] ; 0 ?
	je	short sysfnf_9

	;cmp	byte [Current_Dir_Level], 0
        ;ja	short sysfnf_4  
        ;jna	short sysfnf_9 

sysfnf_4:
	inc	byte [SWP_DRV_chg]
	call 	load_FAT_root_directory
	jnc	short sysfnf_9
	; eax = error code (17, 'drv not ready or read error')
	jmp	short sysfnf_err_1 ; read error ! (no FNF stop)
				   ; (if you want, try again,
				   ;  after re-placing the disk)
sysfnf_5:
	cmp	al, 12 ; 'no more files' error
	jne	short sysfnf_err_1 ; (no FNF stop -sysfnf will try
				   ;  to read the directory again,
				   ;  if the user calls sysfnf
				   ;  just after this error return-)
	; (FNF stop -sysfnf will not try
	;  to read the directory again-)

sysfnf_err_0:
	mov	byte [FFF_Valid], 0 ; FNF stop sign
sysfnf_err_1:
	mov	[u.r0], eax
	mov	[u.error], eax
	call	reset_working_path
	jmp	error

sysfnf_6:
	cmp	byte [DirBuff_ValidData], 0
	jna	short sysfnf_7

	cmp	eax, [DirBuff_Cluster]
	;je	short sysfnf_9
	; 08/08/2022
	jne	short sysfnf_7
	jmp	sysfnf_9
sysfnf_7:
	inc	byte [SWP_DRV_chg]
	cmp	byte [Current_FATType], 1
	;jnb	short sysfnf_8
	; 08/08/2022
	jb	short sysfnf_13
	jmp	sysfnf_8
sysfnf_13:
	; Singlix (TRFS) File System 
	; (access via compatibility buffer)
	call	load_FS_sub_directory
	;jnc	short sysfnf_9
	;jmp	short sysfnf_err_1 ; read error (no FNF stop)
	; 08/08/2022
	jc	short sysfnf_err_1
	jmp	sysfnf_9
%else
	; 19/05/2025 - TRDOS 386 v2.0.10
	; here:
	; [FindFile_MatchCounter] = number of files
	;			    previously found
	; [FindFile_DirEntryNumber] = dir entry index
	;			    in current sector
	; 16 = last entry index number + 1
	; [FindFile_DirSector] = current phy sector addr
	; [FindFile_Drv] = logical drive number
	; [FindFile_DirSectorCount] = remain sectors
	;			    in current cluster
	; [FindFile_DirCluster] = current cluster
	;			  is 0 for root dir
	;			  (extept FAT32 fs)
	; FindFile_DirEntryName = file/dir name
	;			  to be searched
	;		 (may contain '?' wildcards)

	; 19/05/2025
	cmp	[FFF_FATtype], 0
	ja	short sysfnf_1

	; SysFFF singlix fs procedure(s)
	call	find_next_fs_file
	jc	short sysfnf_err

	; all of data has been transferred and eax return
	; value has been set in SysFNF singlix fs proc

	;mov	[u.r0], eax
	;call 	reset_working_path
	;jmp	sysret
	; 19/05/2025
	jmp	sysfnf_fs_ok

sysfnf_1:
	call	find_next_file
	jc	short sysfnf_err ; eax = 12 (no more files !)

	; esi = Directory Entry (FindFile_DirEntry) Location
	; edi = Directory Buffer Directory Entry Location
	; eax = File Size
	;  bl = Attributes of The File/Directory
	;  bh = Long Name Yes/No Status (>0 is YES)
	;  dl > 0 : Ambiguous filename chars are used

	mov	cx, [FFF_Attrib]
	; Attribs in CL, return data type in CH

	xor	cl, 0FFh
	; cl = negative attributes
	and	cl, bl
	jnz	short sysfnf_1 ; it doesn't fit
			       ;    to conditions
	cmp	dword [FFF_UBuffer], 0
	jna	short sysfnf_3 ; only file size
	; return data according to
	;	ch = [FFF_RType]

	and	ch, ch
	jnz	short sysfnf_2

	jmp	sysfnf_4

sysfnf_2:
	jmp	sysfnf_5

sysfnf_3:
	; eax = file size
	mov	ecx, eax
	jmp	sysfff_9
%endif

find_first_fs_file:
find_next_fs_file:
	; 19/05/2025 (TRDOS 386 Kernel v2.0.10)
	; 19/05/2025 - temporary !
	mov	eax, ERR_CLUSTER ; 35 ; 'cluster not available !'
	stc
	retn

writei:
	; 02/09/2024
	; 27/08/2024 - TRDOS 386 v2.0.9 
	; 08/08/2022
	; 30/07/2022
	; 23/07/2022 - TRDOS 386 Kernel v2.0.5
	; 26/10/2016
	; 25/10/2016
	; 23/10/2016
	; 22/10/2016
	; 19/10/2016 - TRDOS 386 (TRDOS v2.0)
	; 19/05/2015 - 20/05/2015 (Retro UNIX 386 v1)
	; 12/03/2013 - 31/07/2013 (Retro UNIX 8086 v1)
	;
	; Write data to file with first cluster number in EAX
	; 
	; INPUTS ->
	;    EAX - First cluster number of the file
	;    EBX - File number (Open file index number)
	;    u.count - byte count to be written
	;    u.base - points to user buffer
	;    u.fofp - points to dword with current file offset
	;    i.size - file size
	;    cdev - logical dos drive number of the file
	; OUTPUTS ->
	;    u.count - cleared
	;    u.nread - accumulates total bytes passed back
	;    i.size - new file size (if file byte offset overs file size)
	;    u.fofp - points to u.off (with new offset value)
	;
	; (Retro UNIX Prototype : 11/11/2012 - 18/11/2012, UNIXCOPY.ASM)
	; ((Modified registers: eax, edx, ebx, ecx, esi, edi, ebp))

	xor	ecx, ecx
	mov 	[u.nread], ecx  ; 0
	mov	[u.pcount], cx ; 19/05/2015
	cmp 	[u.count], ecx
	ja 	short writei_1 ; 08/08/2022
	retn
	; 23/07/2022
	;jna	short dskw_8 ; retn
writei_1:
	; 02/09/2024
	mov	dword [writei.fclust], 0FFFFFFFFh ; -1 ; reset
dskw:
	mov	[writei.ofn], bl ; Open file number
	mov	[setfmod], cl ; 0 ; reset 'update lm date&time' sign
dskw_0: 
	; 26/10/2016
	; 22/10/2016, 23/10/2016, 25/10/2016
	; 19/10/2016 - TRDOS 386 (TRDOS v2.0)
	; 31/05/2015 - 25/07/2015 (Retro UNIX 386 v1)
	; 26/04/2013 - 20/09/2013 (Retro UNIX 8086 v1)
	;
	; 01/08/2013 (mkdir_w check)
 	call	mget_w
	; eax = sector/block number

	mov     ebx, [u.fofp]
	mov	edx, [ebx]
	and	edx, 1FFh  ; / test the lower 9 bits of the file offset
	jnz	short dskw_1 ; / if its non-zero, branch
			     ; if zero, file offset = 0,
		       	     ; / 512, 1024,...(i.e., start of new block)
	cmp	dword [u.count], 512
				; / if zero, is there enough data to fill
				; / an entire block? (i.e., no. of
	jnb	short dskw_2 ; / bytes to be written greater than 512.?
			     ; / Yes, branch. Don't have to read block
dskw_1: ; in as no past info. is to be saved 
	; (the entire block will be overwritten).
	; 23/10/2016

	mov	ebx, writei_buffer
	; esi = logical dos drive description table address
	; eax = sector number
	; ebx = buffer address (in kernel's memory space)
	; ecx = sector count
	; 30/07/2022
	;mov	ecx, 1
	xor	ecx, ecx
	inc	cl
	; ecx = 1
	call	disk_read
	;call	dskrd 	; / no, must retain old info.. 
		       	; / Hence, read block 'r1' into an I/O buffer
	jnc	short dskw_2

	; disk read error
	; 30/07/2022
	;mov	eax, 17 ; drive not ready or READ ERROR !
	sub	eax, eax
	mov	al, 17
dskw_err: ; jump from disk write error
	mov	[u.r0], eax
	mov	[u.error], eax

	cmp	byte [setfmod], 0
	;jna	error
	; 23/07/2022
	jna	short writei_err

	call	update_file_lmdt ; update last modif. date&time of the file
	;mov	byte [setfmod], 0
writei_err:
	jmp	error

dskw_2: ; 3:
	; 23/10/2016
	mov	byte [writei.valid], 1 ; writei buffer contains valid data
	push	esi ; logical dos drive description table address
	; EAX (r1) = block/sector number
	;call	wslot
		; jsr r0,wslot / set write and inhibit bits in I/O queue,
			   ; / proc. status=0, r5 points to 1st word of data
	cmp	byte [u.kcall], 0
	ja	short dskw_4 ; zf=0 -> the caller is 'mkdir'
	;
	cmp	word [u.pcount], 0
	ja	short dskw_4
dskw_3:
	; [u.base] = virtual address to transfer (as source address)
	call	trans_addr_r ; translate virtual address to physical (r)
dskw_4:
	mov	ebx, writei_buffer
	; EBX (r5) = system (I/O) buffer address
	call	sioreg
	; ESI = file (user data) offset
	; EDI = sector (I/O) buffer offset
	; ECX = byte count
	;
  	rep	movsb
	; 25/07/2015
	; eax = remain bytes in buffer
        ;       (check if remain bytes in the buffer > [u.pcount])
	or	eax, eax
	jnz	short dskw_3 ; (page end before system buffer end!)

	; 23/10/2016
	mov	cl, 1
	pop	esi
	mov	eax, [writei.sector]
	; esi = logical dos drive description table address
	; eax = sector number
	; ebx = writei buffer address
	; ecx = sector count
	call	disk_write ; / yes, write the block
	jnc	short dskw_5

	;mov	eax, 18 ; drive not ready or WRITE ERROR !
	; 30/08/2022
	sub	eax, eax
	mov	al, 18
	jmp	short dskw_err

dskw_7:
 	; update last modif. date&time of the file
	; (also updates file size as OF_SIZE)
	call	update_file_lmdt
	;mov	byte [setfmod], 0

	; 03/08/2013
	mov	byte [u.kcall], 0
	; 23/10/2016
	;mov	eax, [writei.fclust]
dskw_8:		; 23/07/2022
	retn

dskw_5:
	; 26/10/2016
	movzx	ebx, byte [writei.ofn] ; open file number
	shl	bl, 2 ; *4
	mov	eax, [ebx+OF_POINTER]
	cmp	eax, [ebx+OF_SIZE]
	jna	short dskw_6
	mov	[ebx+OF_SIZE], eax
dskw_6:
	;shr	bl, 2
        cmp     dword [u.count], 0 ; / any more data to write?
	jna	short dskw_7
	mov	eax, [writei.fclust]
	jmp	dskw_0 ; / yes, branch

mget_w:
	; 03/09/2024
	; 02/09/2024
	; 25/08/2024 - TRDOS 386 v2.0.9
	; 08/08/2022
	; 25/07/2022
	; 23/07/2022 - TRDOS 386 Kernel v2.0.5
	; 02/11/2016
	; 01/11/2016
	; 23/10/2016, 31/10/2016
	; 22/10/2016 - TRDOS 386 (TRDOS v2.0)
	; 03/06/2015 (Retro UNIX 386 v1, 'mget', u.5s)
	; 22/03/2013 - 31/07/2013 (Retro UNIX 8086 v1)
	;
	; Get existing or (allocate) a new disk block for file
	;
	; INPUTS ->
	;    [u.fofp] = file offset pointer
	;    [i.size] = file size
	;    [u.count] = byte count
	;    EAX = First cluster
	;    [cdev] = Logical dos drive number
	;    [writei.ofn] = File Number
	;		   (Open file index, 0 based)
	;    ([u.off] = file offset)
	; OUTPUTS ->
	;    EAX = logical sector number
	;    ESI = Logical Dos Drive Description Table address
	;
	; Modified registers: EDX, EBX, ECX, ESI, EDI, EBP

        mov     esi, [u.fofp]
	mov	ebp, [esi] ; u.off (or EBX*4+OF_POINTER)

	sub	ecx, ecx
	mov	ch, [cdev]

	mov	esi, Logical_DOSDisks
	add	esi, ecx

	; 31/10/2016
	mov	ebx, eax ; First Cluster or FDT address

	cmp	byte [esi+LD_FATType], 0
	;jna	mget_w_14 ; Singlix FS
	; 23/07/2022
	ja	short mget_w_20
	jmp	mget_w_14  ; Singlix FS

mget_w_20:
	movzx	eax, word [esi+LD_BPB+BytesPerSec]
	movzx	edx, byte [esi+LD_BPB+SecPerClust]
	mov	[writei.spc], dl  ; sectors per cluster
	mul	edx
	; edx = 0
	; eax = bytes per cluster (<= 65536)

	; 02/11/2016
	mov	ecx, eax
	dec	eax
	mov	[writei.bpc], ax

	mov	eax, ebp
	add	eax, [u.count] ; next file position
	cmp	eax, [i.size] ; <= file size ?
	;jna	mget_w_4 ; no
	; 23/07/2022
	ja	short mget_w_21
	; 02/09/2024
	; ebx  = first cluster (input)
	jmp	mget_w_4
mget_w_21:
	div	ecx
	mov	[writei.c_index], eax ; cluster index
	; edx = byte offset in cluster (<= 65535)
	;mov	[writei.offset], dx
	;shr	dx, 9 ; / 512
	;mov	[writei.s_index], dl ; sector index in cluster (0 to spc -1)

	sub	edx, edx ; 01/11/2016
	mov 	[writei.sector], edx ; 0
	mov	[writei.offset], dx  ; byte offset in cluster
	mov	[writei.s_index], dl ; sector index in cluster (0 to spc -1)

	mov	eax, ebx ; First Cluster

	; is this the 1st mget_w or a next mget_w call ? (by 'writei')
	cmp	byte [writei.valid], dl ; 0
	jna	short mget_w_0

	mov 	byte [writei.valid], dl ; 0 ; reset ('writei' will set it) 

	cmp	eax, [writei.fclust]
	jne	short mget_w_0

	mov	cl, [cdev]
	cmp	cl, [writei.drv]
	jne	short mget_w_0
 	; [writei.l_clust] & [writei.l_index] are valid,
	;  we don't need to get last cluster & last cluster index
	mov	ecx, [writei.l_index]
	jmp	short mget_w_2
mget_w_0:
	mov	[writei.fclust], eax ; first cluster
	; edx = 0
	mov	[writei.cluster], eax ; first cluster ; 01/11/2016
	mov 	[writei.fs_index], edx ; 0 ; current cluster index

	;;;
	; 25/08/2024 - TRDOS 386 v2.0.9
	or	eax, eax ; is first cluster number = 0 ?
	jnz	short mget_w_27 ; no

	; eax = 0 (*)
	; ((a directory entry with zero file size)) (*)
	; (*) ((('syswrite' may be used for writing to an empty file.
	; Before this modification, 'syscreat' and 'syswrite' system calls
	; did wrong things together... 'writetest.s' file, 24/08/2024.)))

	call	add_new_cluster ; (*) 2024 (v2.0.9) modification
	jc	short mget_w_errj ; eax = error code
	; eax = (new) first cluster
	; skip 'get_last_cluster' and set ecx to -1
	;
	; 02/09/2024
	mov	[writei.fclust], eax ; first cluster
	mov	[writei.cluster], eax
	; 03/09/2024
	mov	[setfclust], eax
	;
	xor	ecx, ecx
	jmp	short mget_w_28
	;;;

mget_w_27:
	; FAT file system (FAT12, FAT16, FAT32)
	call	get_last_cluster
	;jc	short mget_w_err ; eax = error code
	; 08/08/2022
	jnc	short mget_w_25
mget_w_errj:	; 25/08/2024
	jmp	mget_w_err

mget_w_25:
	; 25/08/2024
	mov	ecx, [glc_index] ; last cluster index
mget_w_28:	; 02/09/2024
	mov	[writei.l_index], ecx
	mov	[writei.lclust], eax ; last cluster

	mov	al, [writei.ofn]
	inc	al
	mov	[setfmod], al ; update lm date&time sign

mget_w_1:
	cmp	ecx, [writei.c_index] ; last cluster index
	jnb	short mget_w_2 ; 01/11/2016

	mov	eax, [writei.lclust]
	; EAX = Last cluster
	call	add_new_cluster
	jc	short mget_w_err ; eax = error code
	; edx = 0
	mov	[writei.lclust], eax ; (new) last cluster
	mov	ecx, [writei.l_index]
	inc	ecx ; add 1 to last cluster index
	mov	[writei.l_index], ecx ; current last cluster index

	jmp	short mget_w_1

mget_w_2:
	mov	ecx, ebp
	add	ecx, [u.count]
	mov	[i.size], ecx ; save new file size
	;sub	edx, edx ; 0

	mov	al, [cdev]
	mov	[writei.drv], al ; physical drive number
	; edx = 0
	mov	eax, ebp ; file offset
	movzx	ecx, word [writei.bpc] ; bytes per cluster - 1
	inc	ecx ; bytes per cluster
	div	ecx
	; edx = byte offset in cluster (<= 65535)
	; eax = cluster index
	mov	[writei.c_index], eax
	mov	[writei.offset], dx
	;shr	dx, 9 ; / 512
	; 23/07/2022
	shr	edx, 9
	mov	[writei.s_index], dl ; sector index in cluster (0 to spc -1)

mget_w_3:
	cmp	eax, [writei.l_index] ; last cluster index
	jne	short mget_w_5

	mov	[writei.fs_index], eax ; cluster index (for next check)
	mov	eax, [writei.lclust] ; last cluster
	jmp	short mget_w_10

mget_w_err:
	mov	[u.error], eax
	mov	[u.r0], eax
	jmp	error

mget_w_4: ; 02/11/2016
	; eax = next file position
	sub	eax, [u.count] ; current file position
	; edx = 0
	; ecx = bytes per cluster
	div	ecx
	mov	[writei.c_index], eax ; cluster index
	mov	[writei.offset], dx
	;shr	dx, 9 ; / 512
	; 23/07/2022
	shr	edx, 9
	mov	[writei.s_index], dl ; sector index in cluster (0 to spc -1)

	; 02/09/2024
	mov	 [writei.fclust], ebx

mget_w_5:
	and	eax, eax ; 0 = First Cluster's index number
	jnz	short mget_w_6

	mov	[writei.fs_index], eax ; cluster index (for next check)
	mov	eax, [writei.fclust] ; first cluster
	jmp	short mget_w_10

mget_w_6:
	cmp	eax, [writei.fs_index] ; current cluster index (>0)
	jne	short mget_w_7
	mov	eax, [writei.cluster] ; current cluster
	jmp	short mget_w_11

mget_w_7:
	mov	ecx, eax
	sub	ecx, [writei.fs_index]
	jnc	short mget_w_8
	; get cluster by index from the first cluster
	mov	eax, [writei.fclust]
	mov	ecx, [writei.c_index]
	jmp	short mget_w_9

mget_w_8:
	mov	eax, [writei.cluster] ; beginning cluster
	; ecx = cluster sequence number after the beginning cluster
	; sub	edx, edx ; 0

mget_w_9:
	; EAX = Beginning cluster
	; EDX = Sector index in disk/file section
	;	(Only for SINGLIX file system!)
	; ECX = Cluster sequence number after the beginning cluster
	; ESI = Logical DOS Drive Description Table address
	call	get_cluster_by_index
	jc	short mget_w_err ; error code in EAX

	; EAX = Cluster number
mget_w_10:
	mov	[writei.cluster], eax ; FDT number for Singlix File System

	cmp	byte [esi+LD_FATType], 0
	jna	short mget_w_13
	; 01/11/2016
	mov	edx, [writei.c_index]
	mov	[writei.fs_index], edx
mget_w_11:
	;sub	eax, 2
	; 23/07/2022
	dec	eax
	dec	eax
	movzx	edx, byte [writei.spc]
	mul	edx

	add	eax, [esi+LD_DATABegin]
	mov	dl, [writei.s_index]
	add	eax, edx
mget_w_12:
	mov	[writei.sector], eax
	;; buffer validation must be done in writei
	;;mov	byte [writei.valid], 1
	retn

mget_w_13:
	; EAX = FDT number (Current Section)
	; EDX = Sector index from the first section (0,1,2,3,4...)
	sub	edx, [writei.fs_index]
	; EDX = Sector index from current section
	mov	[writei.fs_index], edx
	inc	eax ; the first data sector in FS disk section
	add	eax, edx
	jmp	short mget_w_12

mget_w_14:
	mov	cl, [esi+LD_FS_BytesPerSec+1]
	shr	cl, 1 ;  ; 1 for 512 bytes, 4 for 2048 bytes
	mov	[writei.spc], cl  ; sectors per cluster
	; NOTE: writei bytes per sector value is always 512 !
	mov	word [writei.bpc], 512

	mov	ecx, ebp
	add	ecx, [u.count] ; next file position
	cmp	ecx, [i.size] ; <= file size ?
	;jna	mget_w_19 ; no
	ja	short mget_w_22
	; 23/07/2022
	jmp	mget_w_19

mget_w_22:
	sub	edx, edx ; 0
	mov 	[writei.sector], edx ; 0
	mov	[writei.offset], dx  ; byte offset in cluster 
	mov	[writei.s_index], dl ; sector index in cluster (0 to spc -1)

	shr	ecx, 9 ; 1 cluster = 512 bytes
	mov	[writei.c_index], ecx ; section/cluster index
	
	mov	eax, ebx ; FDT number (First FDT address)

	; is this the 1st mget_w or a next mget_w call ? (by 'writei')
	cmp	byte [writei.valid], dl ; 0
	jna	short mget_w_15

	mov 	byte [writei.valid], dl ; 0 ; reset ('writei' will set it)

	cmp	eax, [writei.fclust]
	jne	short mget_w_15

	mov	cl, [cdev]
	cmp	cl, [writei.drv]
	jne	short mget_w_15
 	; [writei.l_clust] & [writei.l_index] are valid, 
	;  we don't need to get last cluster & last cluster index
	mov	ecx, [writei.l_index]
	jmp	short mget_w_17
mget_w_15:
	mov	[writei.fclust], eax ; first section (FDT number)
	; edx = 0
	mov	[writei.cluster], edx ; 0 ; current section
	mov 	[writei.fs_index], edx ; 0 ; curret section index

	; eax = FDT number (section 0 header address)
	call	get_last_section
	;jc	short mget_w_err ; eax = error code
	; 08/08/2022
	jnc	short mget_w_26
	jmp	mget_w_err
mget_w_26:
	mov 	[writei.fs_index], edx ; sector index in last section

	mov	[writei.lclust], eax ; last section address

	mov	ecx, [glc_index] ; last section index
	mov	[writei.l_index], ecx

	mov	al, [writei.ofn]
	inc	al
	mov	[setfmod], al ; update lm date&time sign

mget_w_16:
	; edx = (existing) last section (sector) index
	mov	ecx, [writei.c_index] ; final section (sector) index
	sub	ecx, edx
	jna	short mget_w_19
	; ecx = sector count
mget_w_17:
	mov	eax, [writei.lclust]
	; ESI = Logical dos drv desc. table address
        ; EAX = Last section
        ; (ECX = 0 for directory)
        ; ECX = sector count (except FDT)
	call	add_new_fs_section
	jnc	short mget_w_18

	; If error number = 27h (insufficient disk space)
	; it is needed to check free consequent sectors
	; (1 data sector at least and +1 section header sector)

	cmp	eax, 27h
	;jne	mget_w_err ; eax = error code
	; 25/07/2022
	je	short mget_w_24
mget_w_23:
	jmp	mget_w_err
mget_w_24:
	; ecx = count of free consequent sectors
	; ecx must be > 1 (1 data + 1 header sector)
	dec	ecx
	;jz	short mget_w_err
	;jmp	short mget_w_17
	; 23/07/2022
	jnz	short mget_w_17
	;jmp	mget_w_err
	; 25/07/2022
	jmp	short mget_w_23

mget_w_18:
	mov	[writei.lclust], eax ; (new) last section
	; ecx = sector count (except section header)
	mov	edx, [writei.l_index]
	add	edx, ecx ; add sector count to index
	mov	[writei.l_index], edx
	jmp	short mget_w_16

mget_w_19:
	mov	ecx, ebp
	add	ecx, [u.count]
	mov	[i.size], ecx ; save new file size
	;sub	edx, edx ; 0

	mov	al, [cdev]
	mov	[writei.drv], al ; physical drive number
	; edx = 0
	mov	eax, ebp ; file offset
	mov	edx, eax
	; 1 cluster = 512 bytes (for Singlix FS)
	shr	eax, 9  ; / 512
	and	edx, 1FFh
	; edx = byte offset in cluster/sector (<= 511)
	; eax = section (sector/cluster) index
	mov	[writei.c_index], eax
	mov	[writei.offset], dx
	;mov	byte [writei.s_index], 0 ; sector index in cluster
	jmp	mget_w_3

update_file_lmdt: ; & update file size
	; 03/09/2024
	; 27/08/2024 - TRDOS 386 v2.0.9
	; 30/07/2022
	; 23/07/2022 - TRDOS 386 v2.0.5
	; 26/10/2016
	; 24/10/2016
	; 23/10/2016
	; 22/10/2016 - TRDOS 386 (TRDOS v2.0)
	;
	; Update last modification date&time of file
	; (call from syswrite -> writei)
	; ((also updates file size)) // 26/10/2016
	;
	; INPUT:
	;      byte [setfmod] = open file number
	; OUTPUT:
	;      cf = 0 -> success !
	;      cf = 1 -> lmdt update has been failed!
	;
	; Modified registers: eax, ebx, ecx, edx, esi, edi
	;

	;cmp	byte [setfmod], 0
	;jna	short uflmdt_2 ; nothing to do

	; 27/08/2024
	; if [ebx+OF_FCLUSTER] = 0 and [setfclust] > 0
	; [setfclust] will be copied to Directory entry's
	; first cluster fields (LO/HI).
	; (and [setfclust] will be cleared.)

	xor	eax, eax

	movzx	ebx, byte [setfmod]
	dec	bl ; open file index number (0 based)

	mov	ah, [ebx+OF_DRIVE]
	mov	esi, Logical_DOSDisks
	add	esi, eax
	shl	bl, 2 ; *4
	mov	ecx, [ebx+OF_FCLUSTER] ; first cluster
	mov	edx, [ebx+OF_DIRCLUSTER] ; dir cluster

	shr	bl, 1 ; /2
	movzx	edi, word [ebx+OF_DIRENTRY]

	; 03/09/2024
	add	ah, 'A'
	mov	al, [esi+LD_FATType]
	
	cmp	byte [DirBuff_ValidData], 1
	jb	short uflmdt_4

	;mov	al, [DirBuff_DRV]
	;sub	al, 'A'	
	;cmp	al, ah
	; 03/09/2024
	; 27/08/2024
	;add	ah, 'A'
	;mov	al, [esi+LD_FATType]
	cmp	ah, [DirBuff_DRV]
	jne	short uflmdt_4 ; different drive
	;mov	al, [esi+LD_FATType]
	cmp	al, [DirBuff_FATType]
	jne	short uflmdt_5 ; different FS type
	cmp	edx, [DirBuff_Cluster]
	jne	short uflmdt_5 ; different cluster

uflmdt_1:
	; Directory buffer is ready here!
	; OF_FCLUSTER must be compared/verified
	mov	esi, Directory_Buffer
	;shl	di, 5 ; dir entry index * 32
	; 23/07/2022
	shl	edi, 5
	add	esi, edi ; offset
	;
	; 03/09/2024
	;test	byte [esi+DirEntry_Attr], 18h ; Vol & Dir
	;jnz	short uflmdt_2 ; not a valid file !
	mov	ax, [esi+DirEntry_FstClusHI]
	shl	eax, 16
	mov	ax, [esi+DirEntry_FstClusLO]

	; 03/09/2024 (short jump)
	test	byte [esi+DirEntry_Attr], 18h ; Vol & Dir
	jnz	short uflmdt_2 ; not a valid file !

	cmp	eax, ecx ; same first cluster ?
	;je	short uflmdt_3 ; yes, it is OK !!!
	; 23/07/2022
	jne	short uflmdt_2

	;;;
	; 03/09/2024
	shl	bl, 1 ; *2
	; 27/08/2024 
	and	eax, eax
	jnz	short uflmdt_3 ; not empty file
	xchg	eax, [setfclust]
	or	eax, eax
	jz	short uflmdt_3
	; 03/09/2024
	mov	[ebx+OF_FCLUSTER], eax
	;
	mov	[esi+DirEntry_FstClusLO], ax
	shr	eax, 16
	mov	[esi+DirEntry_FstClusHI], ax
	;;;

uflmdt_3:
	; Update directory entry
	; 03/09/2024
	; 26/10/2016
	;shl	bl, 1 ; *2
	mov	eax, [ebx+OF_SIZE] ; file size
	mov	[esi+DirEntry_FileSize], eax
	;
	call	convert_current_date_time
	; OUTPUT -> DX = Date in dos dir entry format
        ; 	    AX = Time in dos dir entry format
	mov	[esi+DirEntry_WrtTime], ax
	mov	[esi+DirEntry_WrtDate], dx
	mov	[esi+DirEntry_LastAccDate], dx
	mov	byte [DirBuff_ValidData], 2
	;call	save_directory_buffer
	;retn
	; 23/07/2022
	jmp	save_directory_buffer

uflmdt_4:
	; Directory buffer sector read&write
	; 23/10/2016
	; 27/08/2024
	;mov	al, [esi+LD_FATType]
uflmdt_5:
	mov	ebx, rw_buffer ; Common r/w sector buffer addr

	and	al, al ; 0 = Singlix FS
	jz	short uflmdt_11

;uflmdt_12:
	and	edx, edx
	jnz	short uflmdt_9

	cmp	al, 2  ; 3 = FAT32
	ja	short uflmdt_8

	mov	eax, edi ; directory entry index number
	;shr	ax, 4 ; 16 entries per sector
	; 23/07/2022
	shr	eax, 4
	add	eax, [esi+LD_ROOTBegin]
	; eax = root directory sector
uflmdt_6:
	push	eax ; * ; disk sector address
	push	ecx ; first cluster
	mov	ecx, 1
	; ecx = sector count
	call	disk_read
	pop	ecx
	jnc	short uflmdt_10
	pop	eax ; *
uflmdt_7:
	retn

uflmdt_2:
	; save directory buffer if has modified/changed sign
	; (It is good to save dir buff even if the searched
	; directory entry is not found !?)
	call	save_directory_buffer
	stc	; update failed
	retn

uflmdt_11:
	; 24/10/2016
	; Update last modification date & time of a file
	; on a disk with Singlix File System.
	;
	; (Method: Read the FDT -File Description Table-
	; sector of the file and update the lmdt data fields,
	; then write FDT sector to the disk.
	; /// It is easy but there is compatibility buffer
	; method also for changing directory entry data and
	; also there are some programming issues for Singlix
	; file system (TRFS), which are not completed yet!)
	;
	; Not ready yet ! (24/10/2016)
	; /// Temporary code for error return ! ///
	xor	eax, eax
	stc
	retn

uflmdt_8:
	mov	edx, [esi+LD_BPB+FAT32_RootFClust]
uflmdt_9:
	cmp	edx, 2
	jb	short uflmdt_7 ; invalid, nothing to do

	;sub	edx, 2
	; 30/07/2022
	dec	edx
	dec	edx
	mov	eax, edx
	movzx	edx, byte [esi+LD_BPB+SecPerClust]
	mul	edx
	add	eax, [esi+LD_DATABegin]
	; eax = sub directory (data) sector
	jmp	short uflmdt_6

uflmdt_10:
	; Directory sector buffer is ready here!
	; OF_FCLUSTER must be compared/verified
	; edi = dir entry index number (<= 2047)
	and	di, 0Fh ; 16 entries per sector
	;shl	di, 5 ; dir entry index * 32
	; 23/07/2022
	shl	edi, 5
	add	edi, rw_buffer
	;
	test	byte [edi+DirEntry_Attr], 18h ; Vol & Dir
	jnz	short uflmdt_2 ; not a valid file !
	mov	dx, [edi+DirEntry_FstClusHI]
	shl	edx, 16
	mov	dx, [edi+DirEntry_FstClusLO]
	cmp	edx, ecx ; same first cluster ?
	jne	short uflmdt_2 ; no !?

	;;;
	; 27/08/2024 
	and	edx, edx
	jnz	short uflmdt_12 ; not empty file
	xchg	edx, [setfclust]
	or	edx, edx
	jz	short uflmdt_12
	mov	[edi+DirEntry_FstClusLO], dx
	shr	edx, 16
	mov	[edi+DirEntry_FstClusHI], dx
uflmdt_12:
	;;;

	; Update directory entry
	call	convert_current_date_time
	; OUTPUT -> DX = Date in dos dir entry format
        ; 	    AX = Time in dos dir entry format
	mov	[edi+DirEntry_WrtTime], ax
	mov	[edi+DirEntry_WrtDate], dx
	mov	[edi+DirEntry_LastAccDate], dx

	pop	eax ; *

	mov	ebx, rw_buffer ; Common r/w sector buffer addr
	mov	ecx, 1
	; esi = logical dos description table address
	; eax = disk sector number/address (LBA)
	; ecx = sector count
	; ebx = buffer address
	call	disk_write
	jc	short uflmdt_2
;uflmdt_12:
	; save directory buffer if has modified/changed sign
	;call	save_directory_buffer
	;retn
	; 23/07/2022
	jmp	save_directory_buffer

sysalloc:
	; 23/07/2022 - TRDOS 386 v2.0.5
	; 14/10/2017
	; 20/08/2017 - 01/09/2017
	; 20/02/2017 - 04/03/2017 - 15/05/2017
	; 19/02/2017 - TRDOS 386 (TRDOS v2.0)
	; (TRDOS 386 feature only!)
	;
	; Allocate Contiguous Memory Block/Pages (for user)
	; (System call for DMA Buffer allocation etc.)
	;
	; INPUT ->
	;	EBX = Virtual address (for user)
	;	     (Physical memory block/aperture
	;	     will be mapped to this virtual address)
	;	ECX = Byte Count
	;	     (will be rounded up to page border)
	;	If ECX = 0
	;	    System call will return with an error (cf=1)
	;	    but ECX will contain maximum size of
	;	    available memory aperture and physical
	;	    (beginning) address of that aperture
	;	    (which have maximum size) will be in EAX.
	;	EDX = Upper limit of the requested physical memory
	;	      block/pages.
	;	     (The last byte address of the memory aperture
	;	      must not be equal to or above this limit.)
	;	If EDX = 0
	; 	   there is NOLIMIT !
	;	If EDX = 0FFFFFFFFh (-1)
	;	   ESI = Lower Limit !
	;		(Beginning of the block must not be 'less'
	;		than this.) (Must be equal to or above...)
	;	   EDI = Upper Limit !
	;		(End of the block must be !less! than this)
	;		(The last byte addr of the memory aperture
	;		must not be equal to or above this limit.)
	;
	; OUTPUT ->
	;	If CF = 0
	;	EAX = Physical address of the allocated memory block
	;	ECX = Allocated bytes (as rounded up to page borders)
	;	EBX = Virtual address (as rounded up)
	;	IF CF = 1
	;	    Requested (size of) Memory block could not be
	;	    allocated to the user!
	;	IF CF = 1 & EAX = 0 (Insufficient memory error!)
	;	   ECX = Total number of free bytes
	;	         (not size of available contiguous bytes!)
	;	If CF = 1 & EAX > 0
	;	   there is not a memory aperture with requested size
	;	   but total free mem is not less than requested size.
	;	   EAX = Physical addr of available memory aperture
	;	   	 with max size 
	;	        (but it doesn't fit to the conditions!)
	;	   ECX = Size of available memory aperture in bytes.
	;	If CF = 1 -> EAX = 0FFFFFFFFh
	;	   Conditions/Parameters are wrong !
	;	   ECX is same with input value.
	;
	; Note:	Previously allocated pages will be deallocated if
	;       new allocation conditions are met.
	;
	; Note: u.break control may be included in future versions
	;

	xor	eax, eax ; 0
	; 14/10/2017
	dec	edx ; is there a limit ?
	js	short sysalloc_1 ;  0 -> 0FFFFFFFFh -> NO LIMIT
	inc	edx ; > 0
	; Check upper address limit
	;(round up to page borders)
	add	ecx, PAGE_SIZE-1 ; 4095
	and	cx, ~PAGE_OFF ; not 4095
	cmp	edx, ecx ; upper limit - block size
	jb	short sysalloc_err
sysalloc_1:
	; EAX = Beginning address (physical)
	; EAX = 0 -> Allocate mem block from the 1st proper aperture
	; ECX = Number of bytes to be allocated
	call	allocate_memory_block
	jc	short sysalloc_err
	; 01/09/2017
	sub	edx, eax ; upper limit address - beginning address
	jna	short sysalloc_3 ; begin addr not less than the limit
	cmp	edx, ecx
	jb	short sysalloc_3 ; end address overs the limit
sysalloc_2:	
	; EAX = Beginning (physical) addr of the allocated mem block
	; ECX = Num of allocated bytes (rounded up to page borders) 
	push	eax ; * ; 04/03/2017
	; Here, requested contiquous memory pages have been allocated
	; on Memory Allocation Table but user's page directory
	; and page tables have not been updated yet!
	push	ecx ; **
	; ebx = virtual address (will be rounded up to page border)
	; ecx = number of bytes to be deallocated
	;	will be adjusted to ebx+ecx round down - ebx round up
	call	deallocate_user_pages
	jnc	short sysalloc_4 ; EAX = Deallocated memory bytes
	pop	ecx ; **
	pop	eax ; *
sysalloc_3:
	; error !
	; restore Memory Allocation Table Content
	call	deallocate_memory_block
	xor	eax, eax ; 0
	dec	eax ; 0FFFFFFFFh ; 15/05/2017
	jmp	short sysalloc_wrong
sysalloc_err:
	mov	ebp, [u.usp]  ; ebp points to user's registers
	mov	[ebp+24], ecx ; return to user with ecx value
sysalloc_wrong:
	; eax = 0FFFFFFFFh
	mov	[u.r0], eax
	jmp	error
sysalloc_4:
	mov	ebp, [u.usp]  ; ebp points to user's registers
	mov	[ebp+24], eax ; return to user with ecx value
	mov	[ebp+16], ebx ; new value of ebx (rounded up)
	mov	ecx, eax ; byte count (from 'deallocate_user_pages')
	pop	edx ; ** ; discard (another) byte count
	pop	eax ; *
	mov	[u.r0], eax ; physical address

	push	ecx ; 20/08/2017
	;
	; Write newly allocated contiguous (physical) pages
	; on page dir and page tables of current user/process
	; as PRESENT, USER, WRITABLE
	; (then clear allocated pages)
	call	allocate_user_pages
	;jnc	sysret ; OK! return to process with success...

	; 20/08/2017 ('sysdma' modification)
	pop	ecx
	mov	eax, [u.r0]   ; physical address (of the block)

	jc	short sysalloc_6

	cmp	dword [dma_addr], 0FFFFFFFFh ; -1
	;jb	sysret
	; 23/07/2022
	jb	short sysalloc_5 ; jmp sysret

	mov	[dma_addr], eax ; save dma address for sysdma
	mov	[dma_size], ecx ; save dma buff size for sysdma
sysalloc_5:
	jmp	sysret

sysalloc_6:
	;
	; unexpected error ! insufficient memory !? conflict !?
	; (!!?there is not a free page for a new page table?!!)
	; We need to terminate process with error message !!!
	;
	mov	ebp, [u.usp]  ; ebp points to user's registers
	mov	ecx, [ebp+24] ; byte count

	; 20/08/2017
	;mov	eax, [u.r0]   ; physical address (of the block)

	;
	; restore Memory Allocation Table Content
	call	deallocate_memory_block
	;
	cmp	byte [CRT_MODE], 3 ; 80x25 text mode?
	je	short sysalloc_7 ; yes
	; Current mode is VGA (or CGA graphics) mode,
	; We need to return to text mode for displaying
	; error message just before 'sysexit'.
	mov	al, 3
	call	_set_mode
sysalloc_7:
	mov	esi, beep_Insufficient_Memory ; error message
	call	print_msg ; print/display the message
	mov	eax, 1 ; ax=1 is needed for 'sysexit' procedure
	jmp	sysexit ; and terminate the process !

sysdalloc:
	; 19/02/2017 - TRDOS 386 (TRDOS v2.0)
	; (TRDOS 386 feature only!)
	;
	; Deallocate Memory Block/Pages (for user)
	; (Complementary call for sysalloc.)
	;
	; INPUT ->
	;	EBX = Virtual address (for user)
	;	      (will be rounded up to page border)
	;	ECX = Byte Count
	;	     (will be adjusted to page borders)
	;	If ICX = 0
	;	   nothing to do
	;	If EBX + ECX > User's ESP
	;	   nothing to do
	;
	; Note: u.break control may be included in future versions
	;
	; OUTPUT ->
	;	If CF = 0
	;	   EAX = Deallocated memory bytes
	;	   EBX = Virtual address (as rounded up)
	;	IF CF = 1
	;	   EAX = 0
	;
	; Note:	Main purpose of this call is to deallocate/release
	;	previously allocated (physically) contiguous memory
	;	pages but beginning (virtual) address may not be
	;	followed by physically contiguous pages. So, this
	;	system call will deallocate user's virtually
	;	contiguous memory pages. Also, there is not any
	;	objections to use this system call without sysalloc
	;	system call; only possible objection is to lost data
	;	within user's memory space, if the beginning address
	;	and size is not proper.
	;
	; Note: Empty page tables will not be deallocated!!!
	;       (they will be deallocated at process termination)
	;
	; Note: When the program terminates itself or when it is 
	;	terminated by operating system kernel, all allocated
	;	memory pages will be deallocated during termination
	;	stage. So, 'sysdalloc' is not necessary except 
	;	forgiving memory block to other programs/processes.
	;
	mov	edx, [u.sp]
	mov	eax, [edx+12] ; user's stack pointer
	sub	eax, ecx ; esp - byte count
	and	al, 0FCh ; dword alignment
	cmp	eax, ebx
	jb	short sysdalloc_err ; deallocation overlaps with stack

	xor	eax, eax
	and	ecx, ecx
	jz	short sysdalloc_2
	
	call	deallocate_user_pages
	jc	short sysdalloc_err

sysdalloc_2:
	mov	[u.r0], eax
	mov	ebp, [u.usp]
	mov	[ebp+16], ebx ; new value of ebx
	jmp	sysret

sysdalloc_err:
	mov	[u.r0], eax ; 0
	jmp	error

syscalbac:
	; SYS CALLBACK
	; 23/07/2022 - TRDOS 386 v2.0.5
	; 03/08/2020
	; 16/04/2017
	; 14/04/2017
	; 13/04/2017
	; 28/02/2017
	; 26/02/2017
	; 24/02/2017
	; 21/02/2017 - TRDOS 386 (TRDOS v2.0)
	; (TRDOS 386 feature only!)
	;
	; Link or unlink IRQ callback service to/from user (ring 3)
	;
	; INPUT ->
	;	BL = IRQ number (Hardware interrupt request number)
	;	     (0 t0 15 but IRQ 0,1,2,6,8,14,15 are prohibited)
	;	     IRQ numbers 3,4,5,7,9,10,11,12,13 are valid
	;	     (numbers >15 are invalid)
	;
	;	BH = 0 = Unlink IRQ (in BL) from user (ring 3) service
	;	     1 = Link IRQ by using Signal Response Byte method
	;	     2 = Link IRQ by using Callback service method
	;	     3 = Link IRQ by using Auto Increment S.R.B. method
	;	    >3 = invalid 
	;
	;	CL = Signal Return/Response Byte value
	;
	;	If BH = 3, kernel will put a counter value  ; 03/08/2020
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
	;	NOTE: Timer callbacks are set by using 'systimer'
	;	      system call (IRQ 0, PIT and IRQ 8, RTC)
	;
	;	      Direct keyboard access is performed by using
	;	      Keyboard Interrupt (INT 32h)
	;
	;	      It is prohibited here because:
	;		1) Signal Response Byte method has not advantage
	;		   against INT 32h, function AH = 1. Also,
	;		   keyboard service interrupt will return with
	;		   ascii and scan codes (AL, AH) while
	;		   SRB method has only 1 byte space for ascii code
	;		   or scan code. One byte signal response is used
	;		   for ensuring very simple and very fast
	;		   virtual to physical memory address conversion
	;		   without any memory page crossover risk.
	;		   (Otherwise double page conversion or word
	;		   alignment would be needed.)
	;		2) Badly written user code (callback code)
	;		   can prevent keyboard and timesharing functions
	;		   of the operating system via continuous and long
	;		   keyboard event handling by callback service.
	;		   (It can cause to lose immediate keystroke
	;		   response from hardware to user.)
	;		3) If user will check any keyboard events, 'getkey'
	;		   (or 'getchar') must have more priority than other
	;		   (video etc.) events because only control ability
	;		   on a procedural infinite loop is a keyboard or
	;		   mouse event. So user can use keyboard function
	;		   at the end or at the beginning of a loop.
	;		   In this case, INT 32h is used for that purpose
	;		   and timer interrupt etc. callbacks can be used
	;		   for dynamic and synchronized data refresh/transfer
	;		   while cpu is in a static loop (without polling).
	;		   Keyboard Int callback is not more useful because
	;		   already a manual check (a key is pressed or not)
	;		   can be performed (via INT 32h, AH = 1) efficiently
	;		   in a loop to prevent a locked infinitive loop.
	;
	;	    Disk IRQs (6,14,15) have been phohibited from ring 3 
	;	    callback because, disk operations (file system services
	;	    etc.) are independent from user program, for fast disk r/w.
	;	    They are not more useful at ring 3 while they are in use
	;	    by standard diskio functions which are mandatory part of
	;	    (monolithic) OS kernel and mainprog command interpreter.
	;	    INT 33h diskio functions are enough for user level disk
	;	    r/w.
	;
	; TRDOS 386 - IRQ CALLBACK structures (parameters):
	;	
	;	   [u.irqlock] = 1 word, IRQ flags (0-15) that indicates
	;			which IRQs are locked by (that) user.
	;		        Lock and unlock (by user) will change
	;			these flags or 'terminate process' (sysexit)
	;			will clear these flags and unlock those IRQs.
	;			               
	;		   	Bit 0 is for IRQ 0 and Bit 15 is for IRQ 15
	;
	;	   IRQ(x).owner	 : 1 byte, user, [u.uno], 0 = free (unlocked)
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
	;			 the Signal Response byte address
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
	;
	;	NOTE: If user's process/program calls the kernel (INT 40h)
	;	      while it is already running in a (ring 3) callback
	;	      service, kernel will force (convert) system call to
	;	      'sysrele' (sys release). So, this feature provides
	;	      easy and simple usage of callback services without
	;	      falling into deepless <please 'callback me' then 
	;	      let me 'callback you'> cycles! (User must return
	;	      from callback service by using 'sysrele' system
	;	      call, without a significant delay. Otherwise user
	;	      process/program may be late to catch the next event
	;	      within same callback purpose.
	;

	xor	al, al ; the caller is 'syscalbac' sign/flag
 	call	set_irq_callback_service
	; 16/04/2017
	mov	[u.r0], eax
	;jnc	sysret
	; 23/07/2022
	jc	short syscalbac_err
	jmp	sysret
syscalbac_err:
	mov	dword [u.error], eax
	jmp	error

sysfpstat:
	; 28/02/2017 - TRDOS 386 (TRDOS v2.0)
	; (TRDOS 386 feature only!)
	;
	; Set or reset FPU registers save/restore option (for user)
	;	       (during software task switching, wswap-rswap)
	;
	; INPUT ->
	;	BL = 0 -> reset
	;	BL = 1 -> set (FPU register will be saved and restored)
	;	
	; OUTPUT ->
	;	cf = 0 -> no error, FPU is ready...
	;		  (EAX = 0)
	;	Cf = 1 -> error, 80387 FPU is not ready !
	;		  (EAX = 0FFFFFFFFh)

	xor	eax, eax
	cmp	byte [fpready], 0
	jna	short sysfpstat_err

	and	bl, 1 ; use BIT 0 only !
	mov	[u.fpsave], bl
	mov	[u.r0], eax ; 0
	jmp	sysret

sysfpstat_err:
	dec 	eax ; 0FFFFFFFFh
	mov 	[u.r0], eax ; -1
	jmp 	error

sysdelete: ; Delete (Remove, Unlink) File
	; 29/12/2017 (TRDOS 386 = TRDOS v2.0) 
	;	
        ; INPUT ->
        ;          EBX = File name (ASCIIZ string) address
	; OUTPUT ->
	;          cf = 0 -> eax = 0
	;          cf = 1 -> Error code in AL
	;
	; Modified Registers: EAX (at the return of system call)
	;

	mov	esi, ebx
	; file name is forced, change directory as temporary
	;mov	ax, 1
	;mov	[FFF_Valid], ah ; 0 ; reset
	;call	set_working_path 
	call	set_working_path_x
	jnc	short sysdelete_1

	and	eax, eax  ; 0 -> Bad Path!
	jnz	short sysdelete_err
	; eax = 0
sysdelete_path_err:
	mov	eax, ERR_INV_PATH_NAME ; 'bad path name !'
sysdelete_err:
	mov	[u.r0], eax
	mov	[u.error], eax
	call 	reset_working_path
	jmp	error
sysdelete_1:
	;mov	esi, FindFile_Name
	mov	ax, 1800h ; Only files
	call	find_first_file
	jc	short sysdelete_err
sysdelete_2:
	; check file attributes

	;test	bl, 17 ; system, hidden, readonly, directory
        test	bl, 7 ; system, hidden, readonly 
        jz	short sysdelete_3

        mov	eax, ERR_FILE_ACCESS ; 11 = 'permission denied !'
        jmp	short sysdelete_err
sysdelete_3:
	and	dx, dx ; Ambiguous filename chars used sign (DX>0)
	jz	short sysdelete_4
	mov	eax, ERR_INV_FILE_NAME ; 26 = 'invalid file name !'
        jmp	short sysdelete_err
sysdelete_4:
	;mov	bh, [LongName_EntryLength]
	mov	[DelFile_LNEL], bh ; Long name entry length (if > 0)
	; edi = Directory Entry Offset (DirBuff)
	; esi = Directory Entry (FFF Structure)
	call	remove_file
	jc	short sysdelete_err
sysrmdir_5:
	xor	eax, eax ; 0
	mov	[u.r0], eax
	;mov	[u.error], eax
	call 	reset_working_path
	jmp	sysret

sysrmdir: ; Remove (Unlink) Directory
	; 23/07/2022 - TRDOS 386 v2.0.5
	; 19/01/2021
	; 29/12/2017 (TRDOS 386 = TRDOS v2.0)
	;
        ; INPUT ->
        ;          EBX = Pointer to directory name
	; OUTPUT ->
	;          cf = 0 -> eax = 0
	;          cf = 1 -> Error code in AL
	;
	; Modified Registers: EAX (at the return of system call)
	;

	; 19/01/2021
	cmp	byte [u.uid], 0  ; root (super user) ?
	jna	short sysrmdir_0

	;mov	dword [u.r0], ERR_PERM_DENIED
	mov	eax, ERR_PERM_DENIED ; ERR_NOT_SUPERUSER
	mov	[u.r0], eax
	mov	[u.error], eax
	jmp	error

sysrmdir_0:
	mov	esi, ebx
	; file name is forced, change directory as temporary
	;mov	ax, 1
	;mov	[FFF_Valid], ah ; 0 ; reset
	;call	set_working_path 
	call	set_working_path_x
	jnc	short sysrmdir_1

	and	eax, eax  ; 0 -> Bad Path!
	jnz	short sysrmdir_err
	; eax = 0
sysrmdir_not_found:
	;mov	eax, ERR_DIR_NOT_FOUND ; Directory not found !
	; 23/07/2022
	mov	al, ERR_DIR_NOT_FOUND
	jmp	short sysrmdir_err
;sysrmdir_err:
;	mov	[u.r0], eax
;	mov	[u.error], eax
;	call 	reset_working_path
;	jmp	error
sysrmdir_1:
	;mov	esi, FindFile_Name
	mov	ax, 0810h ; Only directories
	call	find_first_file
	jnc	short sysrmdir_2

	; eax = 2 (File not found !)
	cmp	al, 2 ; ERR_NOT_FOUND
	je	short sysrmdir_not_found
	;jmp	short sysrmdir_err
	; 23/07/2022
sysrmdir_err:
	mov	[u.r0], eax
	mov	[u.error], eax
sysrmdir_8:	; 23/07/2022
	call 	reset_working_path
	jmp	error
sysrmdir_2:
	; check directory attributes

        test	bl, 7 ; system, hidden, readonly
        jz	short sysrmdir_3

        mov	eax, ERR_DIR_ACCESS ; 11 = 'permission denied !'
        jmp	short sysrmdir_err
sysrmdir_3:
	and	dx, dx ; Ambiguous filename chars used sign (DX>0)
	jz	short sysrmdir_4
	;mov	eax, ERR_NOT_DIR ; 'not a valid directory !'
	mov	eax, ERR_INV_PATH_NAME ; 'bad path name !'
        jmp	short sysrmdir_err
sysrmdir_4:
	;mov	bh, [LongName_EntryLength]
	mov	[DelFile_LNEL], bh ; Long name entry length (if > 0)
	; edi = Directory Entry Offset (DirBuff)
	; esi = Directory Entry (FFF Structure)
	call	delete_sub_directory
	;jnc	sysrmdir_5
	; 23/07/2022
	jc	short sysrmdir_6
	jmp	sysrmdir_5

;	jc	short sysrmdir_6
;
;	xor	eax, eax ; 0
;sysrmdir_5:
;	mov	[u.r0], eax
;	;mov	[u.error], eax
;	call 	reset_working_path
;	jmp	sysret

sysrmdir_6:
	mov	[u.r0], eax
	mov	[u.error], eax

	or	eax, eax ; EAX = 0 -> Directory not empty!
	jz	short sysrmdir_9

	; EAX > 0 -> Error code in AL (or AX or EAX)

	cmp	dword [FAT_ClusterCounter], 1
	jb	short sysrmdir_8
sysrmdir_7:
	; ESI = Logical DOS Drive Description Table address
	mov	bx, 0FF00h ; BH = FFh -> use ESI for Drive parameters
	           ; BL = 0 -> Recalculate free cluster count
	call	calculate_fat_freespace	
	; 23/07/2022
	jmp	short sysrmdir_8
;sysrmdir_8:
;	call 	reset_working_path
;	jmp	error

sysrmdir_9:
	mov	eax, [FAT_ClusterCounter]
	or	eax, eax ; 0 ?
	;jz	short sysrmdir_err
	; 23/07/2022
	jz	short sysrmdir_8

	; ESI = Logical DOS Drive Description Table address
	mov	bx, 0FF01h ; BH = FFh -> use ESI for Drive parameters
	           ; BL = 1 -> add free clusters
	call	calculate_fat_freespace
	or	ecx, ecx
        jz	short sysrmdir_8 ; ecx = 0 -> OK
	; ecx > 0 -> Error (Recalculation is needed)
	jmp	short sysrmdir_7

syschdir: ; Change Current (Working) Drive & Directory (for user)
	; 30/12/2017 (TRDOS 386 = TRDOS v2.0) 
	;
        ; INPUT ->
        ;          EBX = Directory name (ASCIIZ string) address
	; OUTPUT ->
	;          cf = 0 -> eax = 0
	;          cf = 1 -> Error code in AL
	;
	; Modified Registers: EAX (at the return of system call)
	;
	; NOTE: If drive name is not included, only the working
	; directory (for user, not for drive/OS) will be chanded.
	; If there is a drive name (as A:, B:, C:, D: etc.)
	; at the beginning of the ASCIIZ (directory) string,
	; working drive and working directory (for user) 
	; will be changed together.
	; (When the program is terminated, MainProg -internal
	; shell- will reset working directory to the previous
	; -current- logical drive's current directory again.)

	mov	esi, ebx
	; file name is not forced, change directory as temporary
	xor	eax, eax
	;mov	[FFF_Valid], ah ; 0 ; reset
	;call	set_working_path 
	call	set_working_path_xx
	jnc	short syschdir_ok
	and	eax, eax  ; 0 -> Bad Path!
	jnz	short syschdir_err
	; eax = 0
syschdir_not_found:
	mov	eax, ERR_DIR_NOT_FOUND ; Directory not found !
syschdir_err:
	mov	[u.r0], eax
	mov	[u.error], eax
	call 	reset_working_path
	jmp	error
syschdir_ok:
	xor	eax, eax ; 0
	mov	[u.r0], eax
	;mov	[u.error], eax
	jmp	sysret


syschmod: ; Get & Change File (or Directory) Attributes
	; 26/09/2024 - TRDOS 386 v2.0.9
	; 23/07/2022 - TRDOS 386 v2.0.5
	; 19/01/2021
	; 30/12/2017 (TRDOS 386 = TRDOS v2.0) 
	;
        ; INPUT ->
        ;          EBX = File/Directory (ASCIIZ) name address
	;	    CL = New attributes (if CL < 40h)
	;	    CL >= 40h -> Get File Attributes
	; OUTPUT ->
	;          cf = 0 -> EAX = File attributes (in AL)
	;          cf = 1 -> Error code in AL
	;
	; Modified Registers: EAX (at the return of system call)
	;
	; MSDOS File Attributes:    (bit value of attrib byte)
	;	ATTR_READ_ONLY	=	01h  (bit 0, 'R')
	;	ATTR_HIDDEN	=	02h  (bit 1, 'H')
	;	ATTR_SYSTEM	=	04h  (bit 2, 'S')
	;	ATTR_VOLUME_ID	=	08h  (bit 3)
	;	ATTR_DIRECTORY	=	10h  (bit 4)
	;	ATTR_ARCHIVE	=	20h  (bit 5, 'A')
	;	ATTR_LONG_NAME	=	ATTR_READONLY |
	;				ATTR_HIDDEN |
	;				ATTR_SYSTEM |
	;				ATTR_VOLUME_ID
	;	The upper two bits of attributes must be 0.

	; Note:	* If ATTR_DIRECTORY is set, only directory names
	;	  will be searched (and S,H,R,A attributeds of
	;	  the directory will be changed.)
	;	* If ATTR_VOLUME_ID is set, 'syschmod' system call
	;	  will return with 'permission denied' error.
	;	* If ATTR_DIRECTORY is not set, only file names
	;	  will be searched (and S,H,R,A attributes of the
	;	  file will be changed.)
	;
	; (Ony Super User can change S,H,R attributes.)

	cmp	cl, 40h
	jnb	short syschmod_0

	test	cl, 08h ; ATTR_VOLUME_ID
	jnz	short syschmod_perm_err

	; 19/01/2021
	cmp	byte [u.uid], 0  ; root (super user) ?
	jna	short syschmod_0

	; Not super user..
	test	cl, 07h	; S,H,R attributes
	jz	short syschmod_0

syschmod_perm_err:
	;mov	dword [u.r0], ERR_PERM_DENIED
	mov	eax, ERR_PERM_DENIED ; 'permission denied !'
	mov	[u.r0], eax
	mov	[u.error], eax
	jmp	error

syschmod_0:
	mov	[Attributes], cl
	mov	esi, ebx
	; file name is forced, change directory as temporary
	;mov	ax, 1
	;mov	[FFF_Valid], ah ; 0 ; reset
	;call	set_working_path 
	call	set_working_path_x
	jnc	short syschmod_1
	and	eax, eax  ; 0 -> Bad Path!
	jnz	short syschmod_err
	; eax = 0
syschmod_path_not_found:
	;mov	eax, ERR_INV_PATH_NAME ; 'Bad path name !'
	; 23/07/2022
	mov	al, ERR_INV_PATH_NAME
;syschmod_err:
	;mov	[u.r0], eax
	;mov	[u.error], eax
	;call 	reset_working_path
	;jmp	error
	; 23/07/2022
	jmp	short syschmod_err
syschmod_1:
	mov	al, 08h ; Except volume labels (& long names)
	mov	al, [Attributes]
	and	al, 10h ;
	;mov	esi, FindFile_Name
	;mov	ax, 1800h ; Only files
	;mov	ax, 0810h ; Only directories
	call	find_first_file
	;jnc	short syschmod_2
	jc	short syschmod_err

	;; eax = 2 (File not found !)
	;cmp	al, 2 ; ERR_NOT_FOUND
	;jne	short syschmod_err

	;and	byte [Attributes], 10h
	;jz	short syschmod_err

	;; Directory not found !
	;mov	al, 3 ; ERR_PATH_NOT_FOUND
	;jmp	short syschmod_err

syschmod_2:
	and	dx, dx ; Ambiguous filename chars used sign (DX>0)
	jz	short syschmod_3
	mov	eax, ERR_INV_FILE_NAME ; 'invalid file name !'
        ;jmp	short syschmod_err
syschmod_err:
	mov	[u.r0], eax
	mov	[u.error], eax
	call 	reset_working_path
	jmp	error
syschmod_3:
	; EDI = Directory buffer entry offset/address
	; BL = File (or Directory) Attributes
	; mov	bl, [EDI+0Bh]

	; check directory attributes
	mov	bh, [Attributes] ; new attributes
	cmp	bh, 40h  ;>=40 -> get file/directory attributes
	jnb	short syschmod_6

	; set file/directory attributes
	test	bl, 7 ; system, hidden, readonly
        jz	short syschmod_4

	; 19/01/2021
	cmp	byte [u.uid], 0  ; root (super user) ?
	ja	short syschmod_perm_err
syschmod_4:
	cmp	word [edi+DirEntry_NTRes], 01A1h ; Singlix FS
	je	short syschmod_7

	mov	[edi+0Bh], bh    ; Attributes (New!)

	mov	byte [DirBuff_ValidData], 2 ; modified sign
					    ; to force write
	call 	save_directory_buffer
	jc	short syschmod_err

syschmod_5:
	mov	bl, [Attributes]
syschmod_6:
	movzx	eax, bl
	mov	[u.r0], eax
	;mov	dword [u.error], 0
	jmp	sysret

syschmod_7:
	sub	eax, eax
        mov     ah, [DirBuff_DRV]
	; 26/09/2024 (BugFix)
	sub	ah, 'A'
	mov	esi, Logical_DOSDisks
        add     esi, eax
        cmp     byte [esi+LD_FSType], 0A1h
	jnc	short syschmod_8
	mov	al, ERR_INV_DATA ; 29 = Invalid Data
	jmp	short syschmod_err

syschmod_8:
	; BH = New MS-DOS File Attributes
	mov	al, bh ; File/Directory Attributes
	xor	ah, ah ; Attributes in MS-DOS format sign
	call	change_fs_file_attributes
	;jc	syschmod_err
	;jmp	short syschmod_5
	; 23/07/2022
	jnc	short syschmod_5
	jmp	syschmod_err ; 26/09/2024 


sysdrive: ; Get/Set Current (Working) Drive (for user)
	; 30/12/2017 (TRDOS 386 = TRDOS v2.0) 
	;	
        ; INPUT ->
        ;          BL = Logical DOS Drive number (0=A: ... 2=C:)
	;	   If BL = 0FFh -> Get Current Drive
	; OUTPUT ->
	;          cf = 0 -> 
	;		   AL = Current Drive number
	;		   AH = The Last Logical DOS Drive no.
	;          cf = 1 -> Error code in AL
	;
	; Modified Registers: EAX (at the return of system call)
	;
	; NOTE: If the requested logical dos drive is ready, 
	;	it's current current directory will be the user's
	;	(program's) current directory.
	;	(When the program is terminated, MainProg -internal
	;	shell- will reset the previous -current- logical drive
	;       as current drive again).

	cmp	bl, 0FFh
	je	short sysdrive_ok
	cmp	bl, [Last_DOS_DiskNo]
	ja	short sysdrive_err

	; Save current drive and reset mode
	; for 'reset_working_path' procedure (for MainProg)
	xor	al, al
	mov	[SWP_Mode], ax ; ah = 0
	mov	al, [Current_Drv]
	inc	ah ; mov ah, 1
	mov	[SWP_DRV], ax 

	mov	dl, bl
	call	change_current_drive
	jnc	short sysdrive_ok
sysdrive_err:
	mov	dword [u.r0], ERR_DRV_NOT_RDY ; 'drive not ready !'
	jmp	error
sysdrive_ok:
	mov	al, [Current_Drv]
	mov	ah, [Last_DOS_DiskNo]
	mov	[u.r0], eax
	jmp	sysret

sysdir: ; Get Current (Working) Drive & Directory (for user)
	; 16/05/2025 (TRDOS 386 v2.0.10)
	; 30/12/2017 (TRDOS 386 = TRDOS v2.0)
	;
        ; INPUT ->
        ;          EBX = Current directory name buffer address
	;		;; (Buffer length = 92 bytes)
	;	16/05/2025 - TRDOS 386 v2.0.10
	;		(Buffer length = 104 bytes)
	; OUTPUT ->
	;          AL = Current drive (0=A: .. 2=C:)
	;	   If CF = 1 -> AL = error code
	;
	; Modified Registers: EAX (at the return of system call)
	;
	; Note: Required directory name buffer length may be
	;	;<= 92 bytes for current TRDOS 386 version.
	;	;(7*12 name chars + 7 slash + 0)
	;	; 16/05/2025
	;	;<= 104 bytes for since TRDOS 386 v2.10.10.
	;	;(8*12 name chars + 7 slash + 0)

	mov	ebp, esp
	;sub	esp, 96
	; 16/05/2025
	sub	esp, 104
	push	ebx ; User's buffer address
	xor	dl, dl ; 0 = current drive
  	call	get_current_directory
	jc	short sysdrive_err ; 'drive not ready !' error
	mov	esi, esp ; System's buffer address
	pop	edi  ; User's buffer address
	; 16/05/2025
	;cmp	ecx, 104
	;ja	short sysdir_err
	; ecx = transfer (byte) count (<=104) ; 16/05/2025
	call	transfer_to_user_buffer
	mov	esp, ebp
	jnc	short sysdir_ok
sysdir_err:
	mov	dword [u.r0], ERR_BUFFER  ; 'buffer error !'
	jmp	error
sysdir_ok:
	mov	cl, [Current_Drv]
	mov	[u.r0], ecx
	jmp	sysret

sysldrvt: ; Get copy of Logical DOS Drive Description Table
	; 30/12/2017 (TRDOS 386 = TRDOS v2.0) 
	;
        ; INPUT ->
	;	    BL = Logical DOS drive number (zero based)
        ;          ECX = Logical DOS drv desc table buffer addr
	;		(Buffer length = 256 bytes)
	; OUTPUT ->
	;          cf = 0 -> 
	;		   AL = Current Drive number
	;		   AH = The Last Logical DOS Drive no.
	;          cf = 1 -> Error code in AL
	;		   AH = The Last Logical DOS Drive no.
	;
	; Modified Registers: EAX (at the return of system call)
	;
	; Note: Required description table buffer length is
	;	256 bytes for current TRDOS 386 version.

	mov	edi, ecx ; Destination address (user space)
	mov	ah, bl
	xor	al, al
	mov	esi, Logical_DOSDisks
	add	esi, eax ; Source address (system space)
	mov	ecx, 256 ; Byte count 
			 ; Logical Dos Drv Desc Table size
	call	transfer_to_user_buffer
	jc	short sysdir_err
	mov	ch, [Last_DOS_DiskNo]
	jmp	short sysdir_ok

systime: ; Get System Date&Time
	; 30/12/2017 (TRDOS 386 = TRDOS v2.0)
	;	
        ; INPUT -> BL =
	;	    0 = Get Date&Time in Unix/Epoch format
	;	    1 = Get Time in MSDOS format
	;	    2 = Get Date in MSDOS format
	;	    3 = Get Date&Time in MSDOS format
	;	    4 & other values =
	;		System timer ticks will be returned
	;		in EAX and Carry Flag will be set.
	;		(CF will not be set if BL = 4)
	; OUTPUT ->
	;	For BL input = 3
	;          EAX = Current Time (RTC)
	;		AL = Second (DL in MSDOS)
	;		AH = Minute (CL in MSDOS)
	;	   	HW of EAX = Hour (CH in MSDOS)
	;	   EDX = Current System Date (RTC)
	;		DL = Day (DL in MSDOS)
	;		DH = Month (DH in MSDOS)
	;		HW of EDX = Year (CX in MSDOS)
	;
	;	For BL input = 2
	;	   EAX = Current System Date (RTC)
	;		DL = Day (DL in MSDOS)
	;		DH = Month (DH in MSDOS)
	;		HW of EDX = Year (CX in MSDOS)
	;
	;	For BL input = 1
	;          EAX = Current Time (RTC)
	;		AL = Second (DL in MSDOS)
	;		AH = Minute (CL in MSDOS)
	;	   	HW of EAX = Hour (CH in MSDOS)
	;
	;	For BL input = 0
	;          EAX = Unix (Epoch) Time Ticks/Seconds
	;
	;	For BL input  = 4
	;	   EAX = System timer ticks
	;
	;	If CF = 1 (for other values of BL input)
	;	   EAX = System timer ticks (no error code!)
	;
	; Modified Registers: EAX, (EDX)
	;		 (at the return of system call)
	;

	and	bl, bl
	jnz	short systime_1
	call	epoch
systime_0:
	mov	[u.r0], eax
	jmp	sysret
systime_1:
	cmp	bl, 4
	jb	short systime_2
	mov	eax, [TIMER_LH] ; 18.2 Hz timer ticks
				; Note: [TIMER_LH] may be set
				; to wrong timer value due to
				; program functions.
				; (This value must not be
				; accepted as [TIMER_LH]/18.2
				; seconds since the midnight.)
	jna	short systime_0
	mov	[u.r0], eax	
	jmp	error ; cf = 1 & [u.r0] = eax = timer ticks

systime_2:
	;push	ebx
	call	get_rtc_date_time
	;pop	ebx
	test	bl, 1
	jz	short systime_4
	xor	ah, ah
	mov	al, [hour]
	mov	dl, al
	shl	eax, 16
	mov	al, [second]
	mov	ah, [minute]
	test	bl, 2
	jz	short systime_0
	; Check time & date match risk
	; (23:59:59 may cause to wrong
	; date -new day with previous date-...)
	cmp	dl, 23
	jb	short systime_3
	cmp	ax, (59*256)+59 ; if hour is 23:59:59
	jnb	short systime_2 ; wait for 1 second
systime_3:
	; eax = time
	mov	esi, eax
systime_4:	
	mov	ax, [year]
	shl	eax, 16
	mov	al, [day]
	mov	ah, [month]
	; eax = date
	and	bl, 1
	jz	short systime_0
	xchg	esi, eax
	; eax = time, esi = date
	mov	ebp, [u.usp]  ; EBP points to user's registers
	; (user) edx <-- (system) esi
	mov	[ebp+20], esi ; return to user with EDX value
	jmp	short systime_0

sysstime: ; Set System Date&Time
	; 31/12/2017
	; 30/12/2017 (TRDOS 386 = TRDOS v2.0)
	;
        ; INPUT -> BL =
	;	    0 = Set Date&Time in Unix/Epoch format
	;	    1 = Set Time in MSDOS format
	;	    2 = Set Date in MSDOS format
	;	    3 = Set Date&Time in MSDOS format
	;	    4 = Set System Timer (Ticks)
	;	    5 = Convert/Save current time to/as
	;		18.2 Hz system timer ticks
	;	    6 = Convert MSDOS Date&Time to UNIX format
	;		without setting system date&time ; (test)
	;	    7 = Convert UNIX Date&Time to MSDOS format
	;		without setting system date&time ; (test)
	;	   8-0FFh = invalid !
	;	  ECX = Time (or Timer) value in selected format
	;	  EDX = Date value in MSDOS format if BL=2,3,6
	;
	; OUTPUT ->
	;	If CF = 0 ->
	;          EAX = Set value
	;	If CF = 1 -> (invalid BL input)
	;	   EAX = Ticks count [TIMER_LH]
	;

	and	bl, bl ; 0
	jnz	short sysstime_0
	mov	eax, ecx
	call	convert_from_epoch
	call	set_rtc_date_time
	jmp	sysret
sysstime_0:
	cmp	bl, 8
	jb	short sysstime_1
	; invalid input (>7)
	mov	eax, [TIMER_LH] ; 18.2 Hz timer ticks
				; Note: [TIMER_LH] may be set
				; to wrong timer value due to
				; program functions.
				; (This value must not be
				; accepted as [TIMER_LH]/18.2
				; seconds since the midnight.)
	mov	[u.r0], eax	
	jmp	error ; cf = 1 & [u.r0] = eax = timer ticks

sysstime_8:
	; BL = 7
	mov	eax, ecx ; seconds since 1/1/1970 00:00:00
	call	convert_from_epoch
	xor	ah, ah
	mov	al, [hour]
	shl	eax, 16
	mov	al, [second]
	mov	ah, [minute]
	jmp	short systime_3

sysstime_1:
	cmp	bl, 4
	je	short sysstime_2 ; set system timer ticks
	cmp	bl, 5
	jne	short sysstime_4
	; convert current time to system timer ticks (18.2Hz)
	call	get_rtc_date_time
	movzx	ecx, byte [hour]
	mov	eax, 60*60 ; 1 hour = 3600 seconds
	mul	ecx
	mov	ebx, eax
	mov	cl, 60  ; 1 minute = 60 seconds
	movzx	eax, byte [minute]
	mul	ecx
	add	eax, ebx
	mov	cl, [second]
	add	eax, ecx
	mov	cl, 182
	mul	ecx
	add	eax, 9
	adc	edx, 0
	mov	cl, 10
	div	ecx
	; eax = ((182*seconds)+9)/10
	mov	ecx, eax
sysstime_2:
	mov	[TIMER_LH], ecx ; 18.2 * seconds
sysstime_3:
	mov	[u.r0], ecx
	jmp	sysret
sysstime_4:
	cmp	bl, 6
	ja	short sysstime_8

	mov	[u.r0], ecx

	mov	[second], cl
	mov	[minute], ch
	shr	ecx, 16
	mov	[hour], cl
	; BL = 1,2,3,6
	cmp	bl, 1
	jna	short sysstime_5
	; BL = 2,3,6
	mov	[day], dl
	mov	[month], dh
	shr	edx, 16
	mov	[year], dx
	and	bl, 3
	jz	short sysstime_7 ; 6
	; BL = 2,3
	test	bl, 1
	jz	short sysstime_6 ; 2
	; BL = 3
	call	set_rtc_date_time
	jmp	sysret
sysstime_5:
	; BL = 1
	call	set_time_bcd
	call	set_rtc_time
	jmp	sysret
sysstime_6:
	; BL = 2
	call	set_date_bcd
	call	set_rtc_date
	jmp	sysret
sysstime_7:
	; BL = 6
	; [year], [month], [day],
	; [hour], [minute], [second]
	call	convert_to_epoch
	mov	ecx, eax ; seconds since 1/1/1970 00:00:00
	jmp	sysstime_3

sysrename: ; Rename File (or Directory)
	; 08/08/2022
	; 23/07/2022 - TRDOS 386 v2.0.5
	; 19/01/2021
	; 31/12/2017 (TRDOS 386 = TRDOS v2.0)
	;
        ; INPUT ->
        ;          EBX = File/Directory (ASCIIZ) name address
	;	   ECX = New name (in same dir, no path name)
	; OUTPUT ->
	;          cf = 0 -> EAX = 0
	;          cf = 1 -> Error code in AL

	; 19/01/2021
	cmp	byte [u.uid], 0  ; root (super user) ?
	;jna	short sysrename_0
	; 23/07/2022
	ja	short sysrename_perm_err

;sysrename_perm_err:
;	;mov	dword [u.r0], ERR_PERM_DENIED
;	mov	eax, ERR_PERM_DENIED ; 'permission denied !'
;	mov	[u.r0], eax
;	mov	[u.error], eax
;	jmp	error

sysrename_0:
	push	ecx ; new file name address (in user space)
	mov	esi, ebx
	; file name is forced, change directory as temporary
	;mov	ax, 1
	;mov	[FFF_Valid], ah ; 0 ; reset
	;call	set_working_path
	call	set_working_path_x
	jnc	short sysrename_1
	and	eax, eax  ; 0 -> Bad Path!
	jnz	short sysrename_err
	; eax = 0
sysrename_path_not_found:
	mov	eax, ERR_INV_PATH_NAME ; 'Bad path name !'
	; 23/07/2022
	mov	al, ERR_INV_PATH_NAME
	jmp	short sysrename_err
;sysrename_err:
;	pop	ecx ; new file name address (in user space)
;sysrename_error:
;	mov	[u.r0], eax
;	mov	[u.error], eax
;	call 	reset_working_path
;	jmp	error

	; 23/07/2022
sysrename_perm_err:
	;mov	dword [u.r0], ERR_PERM_DENIED
	mov	eax, ERR_PERM_DENIED ; 'permission denied !'
	mov	[u.r0], eax
	mov	[u.error], eax
	jmp	error

sysrename_1:
	mov	al, 08h ; Except volume labels (& long names)
	mov	al, [Attributes]
	and	al, 10h ;
	;mov	esi, FindFile_Name
	;mov	ax, 1800h ; Only files
	;mov	ax, 0810h ; Only directories
	mov	ax, 0800h ; Find File or Directory
	call	find_first_file
	;jnc	short sysrename_2
	jc	short sysrename_err
sysrename_2:
	; ESI = Directory Entry (FindFile_DirEntry) Location
	; EDI = Directory Buffer Directory Entry Location
	; EAX = File Size
	;  BL = Attributes of The File/Directory
	;  BH = Long Name Yes/No Status (>0 is YES)
	;  DX > 0 : Ambiguous filename chars are used

	and	dx, dx ; Ambiguous filename chars used sign (DX>0)
	jz	short sysrename_3
	mov	eax, ERR_INV_FILE_NAME ; 'invalid file name !'
	;jmp	short sysrename_err
	; 23/07/2022
sysrename_err:
	pop	ecx ; new file name address (in user space)
sysrename_error:
	mov	[u.r0], eax
	mov	[u.error], eax
	call 	reset_working_path
	jmp	error
sysrename_3:
	; EDI = Directory buffer entry offset/address
	; BL = File (or Directory) Attributes 
	; mov	bl, [EDI+0Bh]

	pop	edx ; new file name address (in user space)

	; check file/directory attributes
	test	bl, 7 ; system, hidden, readonly
        jnz	short sysrename_perm_err
sysrename_4:
	cmp	word [edi+DirEntry_NTRes], 01A1h ; Singlix FS
	je	short sysrename_perm_err ; -temporary!-

	; save old file name & file info (FFF structure)
	mov	esi, FindFile_Drv
	mov	edi, SourceFile_Drv
	;mov	ecx, 128/4
	; 23/07/2022
	sub	ecx, ecx
	mov	cl, 128/4
	rep	movsd

	mov	esi, edx ; new file name address (in user space)
	mov	edi, DestinationFile_Drv
	call	parse_path_name
	jc	short sysrename_error ; eax = 1 (Bad file name)

	; same drive ?
	mov	al, [FindFile_Drv]
	cmp	al, [DestinationFile_Drv]
	;jne	short sysrename_perm_err ; Permission denied
	jne	short sysrename_5 ; Bad file name

	; no path name !? (rename file in same directory)
	cmp	byte [DestinationFile_Directory], 20h
	jna	short sysrename_6
sysrename_5:
	mov	eax, ERR_BAD_CMD_ARG ; 1 = Bad file name
				     ; (Bad argument)
	jmp	short sysrename_error
sysrename_6:
	cmp	byte [DestinationFile_Name], 20h
	jna	short sysrename_5

	mov	esi, DestinationFile_Name
	call	check_filename ; is it a valid msdos file name?
	jc	short sysrename_error ; 26 = ERR_INV_FILE_NAME

	;mov	esi, DestinationFile_Name
	mov	ax, 0800h ; Find File or Directory
	call	find_first_file
	jc	short sysrename_7

	mov	eax, ERR_FILE_EXISTS  ; file already exists !
jmp_sysrename_err:	; 08/08/2022
	jmp	sysrename_error
sysrename_7:
	; eax = 2 (File not found !)
	cmp	al, 2 ; ERR_NOT_FOUND
	;jne	short sysrename_error
	; 08/08/2022
	jne	short jmp_sysrename_err

	; 31/12/2017
	; Following code is also part of 'rename_file' in
	; 'trdosk3.s' (MainProg's 'rename' command) ; 13/11/2017
	mov	esi, DestinationFile_Name ; (Rename_NewName)
	mov	cx, [SourceFile_DirEntryNumber] 
	mov	ax, [SourceFile_DirEntry+20] ; First Cluster, HW 
	shl	eax, 16
	mov	ax, [SourceFile_DirEntry+26] ; First Cluster, LW 
  	movzx	ebx, byte [SourceFile_LongNameEntryLength]
   	call	rename_directory_entry
	;jc	short sysrename_error
	; 08/08/2022
	jc	short jmp_sysrename_err
	;xor	eax, eax
	mov	[u.r0], eax ; 0
	;mov	[u.error], eax
	call 	reset_working_path
	jmp	sysret

sysmem: ; Get Total&Free Memory amount 
	; 31/12/2017 (TRDOS 386 = TRDOS v2.0)
	;
        ; INPUT ->
	;	none
	; OUTPUT ->
	;	EAX = Total memory count (in bytes)
	;	EBX = Virtually available memory amount (in bytes)
	;	      = 4GB - CORE (4MB)
	;	ECX = Free memory count (in bytes)
	;	EDX = Calculated free memory count (in bytes)
	
	mov	eax, [memory_size] ; in pages
	shl	eax, 12		   ; in bytes
	mov	[u.r0], eax
	call	calc_free_mem
	; edx = calculated free pages
	; ecx = 0
	mov	ebp, [u.usp]  ; EBP points to user's registers
	mov	dword [ebp+16], ECORE ; EBX (for user)
				; 0FFC00000h ; 4GB - 4MB
	shl	edx, 12
	mov	[ebp+20], edx ; EDX (for user)
	mov 	ecx, [free_pages]
	shl	ecx, 12	; free bytes
	mov	[ebp+24], ecx ; ECX (for user)
	;mov	[free_pages], edx
	jmp	sysret

sysprompt:
	; Set TRDOS 386 Command Interpreter (MainProg) prompt
	; 30/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 31/12/2017 (TRDOS 386 = TRDOS v2.0) 
	;
        ; INPUT -> 
	;	EBX = 0 -> use default prompt
	;	EBX > 0 -> prompt string (ASCIIZ) address
	;		  (Max. 11 characters except ZERO tail)
	; OUTPUT ->
	;	(EAX = 0)
	;	CF = 0 -> Successful
	;	CF = 1 -> Failed

	and	ebx, ebx
	jnz	short sysprompt_0

	call	default_command_prompt ; '['+'TRDOS'+']'
	jmp	sysret

sysprompt_0:
	xor	eax, eax
	mov	[u.r0], eax
	mov	esi, ebx
	;mov	ecx, 12
	; 30/07/2022
	xor	ecx, ecx
	mov	cl, 12
	mov	ebp, esp
	sub	esp, ecx
	dec	ecx ; 11
	mov	edi, esp
	call	transfer_from_user_buffer
	jc	short sysprompt_err
	cmp	byte [esi], 20h
	jna	short sysprompt_err
	call	set_command_prompt
	mov	esp, ebp
	jmp	sysret
sysprompt_err:
syspath_err:
	mov	esp, ebp
	jmp	error

syspath:
	; Get/Set Run Path
	;
	; 30/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 31/12/2017 (TRDOS 386 = TRDOS v2.0)
	;
        ; INPUT ->
	;	EBX = 0 -> get path (to buffer address in ECX)
	;	EBX > 0 -> set path
	;	    EBX = Path string buffer address (ASCIIZ)
	;	    	  (Path description except 'PATH=')
	;	ECX = Buffer address (if EBX = 0)
	;	      (ECX will not be used if EBX > 0)
	;	DL = Buffer size (0 = 256 byte)	
	;
	; OUTPUT ->
	;	CF = 0 -> Successful (EAX = String length)
	;	CF = 1 -> Failed (EAX = 0)
	;
	; NOTE: 'PATH=' or 'PATH' must be excluded
	;  (It must not be at the beginning of the string.)

	mov	ebp, esp
	sub	esp, 256
	mov	edi, esp

	xor	eax, eax
	mov	[u.r0], eax

	and	ebx, ebx
	jnz	short syspath_0

	; EBX = 0 -> get run path
	mov	ebx, ecx ; buffer addr (in user's mem space)
	mov	esi, Cmd_Path  ; 'PATH' address
	movzx	ecx, dl
	;sub	cl, 1 ; 0 -> 255, 1 -> 0
	;adc	cx, 1 ; 255 -> 256, 0 -> 1
	; 30/07/2022
	dec	cl ; 0 -> 255, 1 -> 0
	inc	ecx ; 255 -> 256, 0 -> 1 
	; EDI = Output buffer
	; CX = Buffer length
	; AL = 0 -> use ASCIIZ word in [ESI]
	; ESI = 'PATH' address (with zero tail)
	call	get_environment_string
	jc	short syspath_err
	mov	edi, ebx ; User's buffer address
	mov	esi, esp
	; EDI = User's buffer address
	; ECX = transfer (byte) count
	call	transfer_to_user_buffer
	jc	short syspath_err
	mov	[u.r0], ecx 
	jmp	sysret

syspath_0:
	mov	esi, ebx
	movzx	ecx, dl
	;sub	cl, 1 ; 0 -> 255, 1 -> 0
	;adc	cx, 1 ; 255 -> 256, 0 -> 1
	; 30/07/2022
	dec	cl ; 0 -> 255, 1 -> 0
	inc	ecx ; 255 -> 256, 0 -> 1
	call	transfer_from_user_buffer
	jc	short syspath_err
	;(*) 'PATH=' will be added to
	;         the head of the string
	sub	esp, 8 ;(*)
	mov	esi, edi ;(*)
	call	set_path_x ;(*)
	jc	short syspath_err
	mov	[u.r0], edx ; run path string length
	jmp	sysret

sysenv:
	; Get/Set Environment Variables
	;
	; 30/07/2022
	; 23/07/2022 - TRDOS 386 v2.0.5
	; 31/12/2017 (TRDOS 386 = TRDOS v2.0)
	;
        ; INPUT ->
	;	EBX = 0 -> get (all) environment variables
	;	      (Required Buffer length = 512 bytes)
	;	EBX > 0 -> set (one) environment variable
	;	      (If there is not a '=' after
	;	      the environment variable name, it will
	;	      accepted as 'get environment variable'.)
	;	       EBX = Buffer address
	;	ECX = Buffer address (if EBX = 0)
	;	      (ECX will not be used if EBX > 0)
	;	      (Note: Buffer size is 512 bytes.)
	;	DL = Buffer size (0 = 256 byte)
	;	     (For one envrionment variable)
	;
	; OUTPUT ->
	;	(EAX = 0)
	;	CF = 0 -> Successful (EAX = String length)
	;	CF = 1 -> Failed (EAX = 0)
	;
	; Note: Environment variable name, for example,
	;	'PATH=' must be included at the beginning
	;	of the environment string. If the variable
	;	name is as 'PATH' but it is not as 'PATH='
	;	the variable string (row) will be returned.
	;	If variable name is as 'PATH=' but there is
	;	not a following text after the variable name,
	;	the environment variable will be reset/deleted.

	mov	ebp, esp
	sub	esp, 512
	mov	edi, esp

	xor	eax, eax
	mov	[u.r0], eax

	and	ebx, ebx
	jnz	short sysenv_0

	; EBX = 0 -> get (all) environment variables
	mov	esp, ebp
	mov	esi, Env_Page  ; Environment page
	mov	edi, ecx ; buffer addr (in user's mem space)
	;mov	ecx, 512
	; 30/07/2022
	sub	ecx, ecx
	mov	ch, 2
	; ecx = 512
	call	transfer_to_user_buffer
	;jc	error
	; 23/07/2022
	jc	short sysenv_error
	mov	[u.r0], ecx
	jmp	sysret

sysenv_0:
	mov	esi, ebx ; * ; user's buffer address
	movzx	ecx, dl
	;sub	cl, 1 ; 0 -> 255, 1 -> 0
	;adc	cx, 1 ; 255 -> 256, 0 -> 1
	; 30/07/2022
	dec	cl ; 0 -> 255, 1 -> 0
	inc	ecx ; 255 -> 256, 0 -> 1
	call	transfer_from_user_buffer
	jc	short sysenv_err
	mov	esi, edi
	mov	al, [esi]
	cmp	al, 20h
	jna	short sysenv_err
	cmp	al, '='
	je	short sysenv_err
	push	esi
sysenv_1:
	inc	esi
	cmp	byte [esi], '='
	je	short sysenv_3
	cmp	byte [esi], 20h
	jnb	short sysenv_1
	mov	byte [esi], 0
	pop	esi
	; EDI = Output buffer
	; CX = Buffer length
	xor	al, al
	; AL = 0 -> use ASCIIZ word in [ESI]
	; ESI = Environment variable name address
	call	get_environment_string
	jc	short sysenv_err
	mov	edi, ebx ; * ; user's buffer address
	mov	ecx, eax ; String length
	mov	esi, esp 
	; ESI = system buffer address
	; EDI = User's buffer address
	; ECX = transfer (byte) count
	call	transfer_to_user_buffer
	jc	short sysenv_err
	mov	[u.r0], ecx ; transfer (byte) count
sysenv_2:
	mov	esp, ebp
	jmp	sysret
sysenv_err:
	mov	esp, ebp
sysenv_error:	; 23/07/2022
	jmp	error
sysenv_3:
	inc	esi
	cmp	byte [esi], 20h
	jnb	short sysenv_3
	mov	byte [esi], 0
	pop	esi
	call	set_environment_string
	jc	short sysenv_err
	mov	[u.r0], edx
	jmp	short sysenv_2

isintr:
	; 21/08/2024 - TRDOS 386 v2.0.9
	; check terminate (CTRL+BREAK)
	;	interrupt is enabled or disabled
	;test	word [u.intr], 0FFFFh
	test	byte [u.intr], 0FFh
	; zf = 1 -> terminate interrupt disabled
	; zf = 0 -> terminate interrupt enabled
	retn

; 23/07/2022 - TRDOS 386 v2.0.5

; 22/01/2021
; temporary - 24/01/2016

iget:
	;retn
iopen:
	;retn
iclose:
	;retn
sndc:
	;retn
access:
	;retn
sleep:
	retn