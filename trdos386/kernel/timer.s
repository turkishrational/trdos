; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel) - v2.0.11 - timer.s
; ----------------------------------------------------------------------------
; Last Update: 02/05/2026  (Previous: 08/08/2022, v2.0.5)
; ----------------------------------------------------------------------------
; Beginning: 17/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
;
; Derived from 'Retro UNIX 386 Kernel - v0.2.1.0' source code by Erdogan Tan
;
; Derived from 'IBM PC-AT' BIOS source code (1985) 
; ****************************************************************************

; TRDOS 386  (TRDOS v2.0) Kernel - TIMER & REAL TIME CLOCK (BIOS) FUNCTIONS

; IBM PC-AT BIOS Source Code ('BIOS2.ASM')
; TITLE BIOS2 ---- 06/10/85 BIOS INTERRUPT ROUTINES

;-------------------------------------------------------------------------------
;
; ///////// TIMER (& REAL TIME CLOCK) FUNCTIONS ///////////////

int1Ah:
	; 29/01/2016
	; 17/01/2016 (TRDOS 386 = TRDOS v2.0)
	pushfd
	push 	cs
	call 	TIME_OF_DAY_1
	retn

;-------------------------------------------------------------------------------

;--- INT 1A H -- (TIME OF DAY) --------------------------------------------------
;       THIS BIOS ROUTINE ALLOWS THE CLOCKS TO BE SET OR READ			:
;										:
; PARAMETERS:									:
;     (AH) = 00H  READ THE CURRENT SETTING AND RETURN WITH,			:
;                      (CX) = HIGH PORTION OF COUNT				:
;                      (DX) = LOW PORTION OF COUNT				:
;                      (AL) = 0 TIMER HAS NOT PASSED 24 HOURS SINCE LAST READ	:
;                             1 IF ON ANOTHER DAY. (RESET TO ZERO AFTER READ)	:
;										:
;     (AH) = 01H  SET THE CURRENT CLOCK USING,					:
;		     (CX) = HIGH PORTION OF COUNT				:
;		     (DX) = LOW PORTION OF COUNT.				:
;										:
;             NOTE: COUNTS OCCUR AT THE RATE OF 1193180/65536 COUNTS/SECOND	:
;                   (OR ABOUT 18.2 PER SECOND -- SEE EQUATES)			:
;										:
;     (AH) = 02H  READ THE REAL TIME CLOCK AND RETURN WITH,			:
;                      (CH) = HOURS IN BCD (00-23)				:
;                      (CL) = MINUTES IN BCD (00-59)				:
;                      (DH) = SECONDS IN BCD (00-59)				:
;                      (DL) = DAYLIGHT SAVINGS ENABLE (00-01)			:
;										:
;     (AH) = 03H  SET THE REAL TIME CLOCK USING,				:
;                     (CH) = HOURS IN BCD (00-23)				:
;                     (CL) = MINUTES IN BCD (00-59)				:
;                     (DH) = SECONDS IN BCD (00-59)				:
;                     (DL) = 01 IF DAYLIGHT SAVINGS ENABLE OPTION, ELSE 00.	:
;										:
;             NOTE: (DL) = 00 IF DAYLIGHT SAVINGS TIME ENABLE IS NOT ENABLED.	:
;                   (DL) = 01 ENABLES TWO SPECIAL UPDATES THE LAST SUNDAY IN	:
;	            APRIL   (1:59:59 --> 3:00:00 AM) AND THE LAST SUNDAY IN	:
;                   OCTOBER (1:59:59 --> 1:00:00 AM) THE FIRST TIME.		:
;										:
;     (AH) = 04H  READ THE DATE FROM THE REAL TIME CLOCK AND RETURN WITH,	:
;                      (CH) = CENTURY IN BCD (19 OR 20)				:
;                      (CL) = YEAR IN BCD (00-99)				:
;                      (DH) = MONTH IN BCD (01-12)				:
;                      (DL) = DAY IN BCD (01-31).				:
;										:
;     (AH) = 05H  SET THE DATE INTO THE REAL TIME CLOCK USING,			:
;                     (CH) = CENTURY IN BCD (19 OR 20)				:
;                     (CL) = YEAR IN BCD (00-99)				:
;                     (DH) = MONTH IN BCD (01-12)				:
;                     (DL) = DAY IN BCD (01-31).				:
;										:
;     (AH) = 06H  SET THE ALARM TO INTERRUPT AT SPECIFIED TIME,			:
;                     (CH) = HOURS IN BCD (00-23 (OR FFH))			:
;                     (CL) = MINUTES IN BCD (00-59 (OR FFH))			:
;                     (DH) = SECONDS IN BCD (00-59 (OR FFH))			:
;										:
;     (AH) = 07H  RESET THE ALARM INTERRUPT FUNCTION.				:
;										:
; NOTES: FOR ALL RETURNS CY= 0 FOR SUCCESSFUL OPERATION.			:
;        FOR (AH)= 2, 4, 6 - CARRY FLAG SET IF REAL TIME CLOCK NOT OPERATING.	:
;        FOR (AH)= 6 - CARRY FLAG SET IF ALARM ALREADY ENABLED. 		:
;        FOR THE ALARM FUNCTION (AH = 6) THE USER MUST SUPPLY A ROUTINE AND	:
;         INTERCEPT THE CORRECT ADDRESS IN THE VECTOR TABLE FOR INTERRUPT 4AH.	:
;         USE 0FFH FOR ANY "DO NOT CARE" POSITION FOR INTERVAL INTERRUPTS.	:
;        INTERRUPTS ARE DISABLED DURING DATA MODIFICATION. 			:
;        AH & AL ARE RETURNED MODIFIED AND NOT DEFINED EXCEPT WHERE INDICATED.	:
;--------------------------------------------------------------------------------

; 29/07/2022
; 15/01/2017
; 14/01/2017
; 07/01/2017
; 02/01/2017
; 29/05/2016
; 29/01/2016
; 17/01/2016 (TRDOS 386 = TRDOS v2.0)

; 29/05/2016
; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
int35h:  ; Date/Time functions

TIME_OF_DAY_1:
	; 07/08/5022
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	;sti				; INTERRUPTS BACK ON
	; 29/05/2016
	and	byte [esp+8], 11111110b	; clear carry bit of eflags register
	;
	cmp	ah, (RTC_TBE-RTC_TB)/4	; CHECK IF COMMAND IN VALID RANGE (0-7)
	cmc				; COMPLEMENT CARRY FOR ERROR EXIT
	; (*) jc short TIME_9		; EXIT WITH CARRY = 1 IF NOT VALID
	jc	short _TIME_9 ; 29/05/2016

	push	ds
	push	esi
	mov	si, KDATA		; kernel data segment
	mov	ds, si

	;;15/01/2017
	; 14/01/2017
	; 02/01/2017
	;;mov	byte [intflg], 35h	; date & time interrupt 
	;sti
	;
	shl	ah, 2			; convert function to dword offset
	movzx	esi, ah			; PLACE INTO ADDRESSING REGISTER
	;cli				; NO INTERRUPTS DURING TIME FUNCTIONS
	call	[esi+RTC_TB]		; VECTOR TO FUNCTION REQUESTED WITH CY=0
					; RETURN WITH CARRY FLAG SET FOR RESULT
	;sti				; INTERRUPTS BACK ON
	mov	ah, 0			; CLEAR (AH) TO ZERO
	pop	esi			; RECOVER USERS REGISTER
	pop	ds			; RECOVER USERS SEGMENT SELECTOR

	;;15/01/2017
	; 02/01/2017
	;;mov	byte [ss:intflg], 0 ; 07/01/2017

;TIME_9:
					; RETURN WITH CY= 0 IF NO ERROR
	; (*) 29/05/2016
	; (*) retf 4 ; skip eflags on stack
	jnc	short _TIME_10
_TIME_9:
	; 29/05/2016 -set carry flag on stack-
	; [esp] = EIP
	; [esp+4] = CS
	; [esp+8] = E-FLAGS
	or	byte [esp+8], 1	 ; set carry bit of eflags register
	; [esp+12] = ESP (user)
	; [esp+16] = SS (User)
_TIME_10:
	iretd
	
	; (*) 29/05/2016 - 'ref 4' intruction causes to stack fault
	; (OUTER-PRIVILEGE-LEVEL)
	; INTEL 80386 PROGRAMMER'S REFERENCE MANUAL 1986
	; // RETF instruction:
	;
	; IF OperandMode=32 THEN
 	;    Load CS:EIP from stack;
 	;    Set CS RPL to CPL;
 	;    Increment eSP by 8 plus the immediate offset if it exists;
 	;    Load SS:eSP from stack;
 	; ELSE (* OperandMode=16 *)
 	;    Load CS:IP from stack;
 	;    Set CS RPL to CPL;
 	;    Increment eSP by 4 plus the immediate offset if it exists;
	;    Load SS:eSP from stack;
 	; FI;
	;
	; //					
					; ROUTINE VECTOR TABLE (AH)=
RTC_TB:
	dd	RTC_00			; 0 = READ CURRENT CLOCK COUNT
	dd	RTC_10			; 1 = SET CLOCK COUNT
	dd	RTC_20			; 2 = READ THE REAL TIME CLOCK TIME
	dd	RTC_30			; 3 = SET REAL TIME CLOCK TIME
	dd	RTC_40			; 4 = READ THE REAL TIME CLOCK DATE
	dd	RTC_50			; 5 = SET REAL TIME CLOCK DATE
	dd	RTC_60			; 6 = SET THE REAL TIME CLOCK ALARM
	dd	RTC_70			; 7 = RESET ALARM

RTC_TBE	equ	$

RTC_00:				; READ TIME COUNT
	mov	al, [TIMER_OFL]		; GET THE OVERFLOW FLAG
	mov	byte [TIMER_OFL], 0	; AND THEN RESET THE OVERFLOW FLAG
        mov     ecx, [TIMER_LH]         ; GET COUNT OF TIME
	retn

RTC_10:				; SET TIME COUNT
        mov     [TIMER_LH], ecx         ; SET TIME COUNT
	mov	byte [TIMER_OFL], 0	; RESET OVERFLOW FLAG
	retn				; RETURN WITH NO CARRY

RTC_20:				; GET RTC TIME
	call	UPD_IPR			; CHECK FOR UPDATE IN PROCESS
	jc	short RTC_29		; EXIT IF ERROR (CY= 1)

	mov	al, CMOS_SECONDS	; SET ADDRESS OF SECONDS
	call	CMOS_READ		; GET SECONDS
	mov	dh, al			; SAVE
	mov	al, CMOS_REG_B		; ADDRESS ALARM REGISTER
	call	CMOS_READ		; READ CURRENT VALUE OF DSE BIT
	and	al, 00000001b		; MASK FOR VALID DSE BIT
	mov	dl, al			; SET [DL] TO ZERO FOR NO DSE BIT
	mov	al, CMOS_MINUTES	; SET ADDRESS OF MINUTES
	call	CMOS_READ		; GET MINUTES
	mov	cl, al			; SAVE
        mov     al, CMOS_HOURS          ; SET ADDRESS OF HOURS
RTC_41:		; 29/07/2022
	call	CMOS_READ		; GET HOURS
	mov	ch, al			; SAVE
	; 29/07/2022
	;clc				; SET CY= 0
RTC_29:
	retn				; RETURN WITH RESULT IN CARRY FLAG

RTC_30:				; SET RTC TIME
	call	UPD_IPR			; CHECK FOR UPDATE IN PROCESS
	jnc	short RTC_35		; GO AROUND IF CLOCK OPERATING
	call	RTC_STA			; ELSE TRY INITIALIZING CLOCK
RTC_35:
	mov	ah, dh			; GET TIME BYTE - SECONDS
	mov	al, CMOS_SECONDS	; ADDRESS SECONDS
	call	CMOS_WRITE		; UPDATE SECONDS
	mov	ah, cl			; GET TIME BYTE - MINUTES
	mov	al, CMOS_MINUTES	; ADDRESS MINUTES
	call	CMOS_WRITE		; UPDATE MINUTES
	mov	ah, ch			; GET TIME BYTE - HOURS
	mov	al, CMOS_HOURS		; ADDRESS HOURS
	call	CMOS_WRITE		; UPDATE ADDRESS
	;mov	al, CMOS_REG_B		; ADDRESS ALARM REGISTER
	;mov	ah, al
	mov	ax, CMOS_REG_B * 257
	call	CMOS_READ		; READ CURRENT TIME
	and	al, 01100010b		; MASK FOR VALID BIT POSITIONS
	or	al, 00000010b		; TURN ON 24 HOUR MODE
	and	dl, 00000001b		; USE ONLY THE DSE BIT
	or	al, dl			; GET DAY LIGHT SAVINGS TIME BIT (OSE)
	xchg	ah, al			; PLACE IN WORK REGISTER AND GET ADDRESS
	;call	CMOS_WRITE		; SET NEW ALARM SITS
	;clc				; SET CY= 0
	;retn				; RETURN WITH CY= 0
	; 29/07/2022
	jmp	short CMOS_WRITE

RTC_40:				; GET RTC DATE
	call	UPD_IPR			; CHECK FOR UPDATE IN PROCESS
	;jc	short RTC_49		; EXIT IF ERROR (CY= 1)
	; 07/08/2022
	jc	short RTC_29

	mov	al, CMOS_DAY_MONTH	; ADDRESS DAY OF MONTH
	call	CMOS_READ		; READ DAY OF MONTH
	mov	dl, al			; SAVE
	mov	al, CMOS_MONTH		; ADDRESS MONTH
	call	CMOS_READ		; READ MONTH
	mov	dh, al			; SAVE
	mov	al, CMOS_YEAR		; ADDRESS YEAR
	call	CMOS_READ		; READ YEAR
	mov	cl, al			; SAVE
	mov	al, CMOS_CENTURY	; ADDRESS CENTURY LOCATION
; 29/07/2022
;	call	CMOS_READ		; GET CENTURY BYTE
;	mov	ch, al			; SAVE
;	; 29/07/2022
;	;clc				; SET CY=0
;RTC_49:
;	retn				; RETURN WITH RESULTS IN CARRY FLAG

	; 29/07/2022
	jmp	short RTC_41


RTC_50:				; SET RTC DATE
	call	UPD_IPR			; CHECK FOR UPDATE IN PROCESS
	jnc	short RTC_55		; GO AROUND IF NO ERROR
	call	RTC_STA			; ELSE INITIALIZE CLOCK
RTC_55:
	mov	ax, CMOS_DAY_WEEK	; ADDRESS OF DAY OF WEEK BYTE
	call	CMOS_WRITE		; LOAD ZEROS TO DAY OF WEEK
	mov	ah, dl			; GET DAY OF MONTH BYTE
	mov	al, CMOS_DAY_MONTH	; ADDRESS DAY OF MONTH BYTE
	call	CMOS_WRITE		; WRITE OF DAY OF MONTH REGISTER
	mov	ah, dh			; GET MONTH
	mov	al, CMOS_MONTH		; ADDRESS MONTH BYTE
	call	CMOS_WRITE		; WRITE MONTH REGISTER
	mov	ah, cl			; GET YEAR BYTE
	mov	al, CMOS_YEAR		; ADDRESS YEAR REGISTER
	call	CMOS_WRITE		; WRITE YEAR REGISTER
	mov	ah, ch			; GET CENTURY BYTE
	mov	al, CMOS_CENTURY	; ADDRESS CENTURY BYTE
	call	CMOS_WRITE		; WRITE CENTURY LOCATION
	;mov	al, CMOS_REG_B		; ADDRESS ALARM REGISTER
	;mov	ah, al
	mov	ax, CMOS_REG_B * 257
	call	CMOS_READ		; READ CURRENT SETTINGS
	and	al, 07Fh		; CLEAR 'SET BIT'
	xchg	ah, al			; MOVE TO WORK REGISTER
	;call	CMOS_WRITE		; AND START CLOCK UPDATING
	;clc				; SET CY= 0
	;retn				; RETURN CY=0
	; 29/07/2022
	;jmp	short CMOS_WRITE

;-------------------------------------------------------------------------------

; 08/08/2022
; 29/07/2022 (TRDOS 386 v2.0.5)
; 18/04/2021 (TRDOS 386 v2.0.4)
; 17/01/2016 (TRDOS 386 = TRDOS v2.0)

;--- CMOS_WRITE ----------------------------------------------------------------
;	WRITE BYTE TO CMOS SYSTEM CLOCK CONFIGURATION TABLE		       :
;									       :
; INPUT: (AL)=	CMOS TABLE ADDRESS TO BE WRITTEN TO			       :
;		BIT    7 = 0 FOR NMI ENABLED AND 1 FOR NMI DISABLED ON EXIT    :
;		BITS 6-0 = ADDRESS OF TABLE LOCATION TO WRITE		       :
;	 (AH)=	NEW VALUE TO BE PLACED IN THE ADDRESSED TABLE LOCATION	       :
;									       :
; OUTPUT:	VALUE IN (AH) PLACED IN LOCATION (AL) WITH NMI LEFT DISABLED   :
;		IF BIT 7 OF (AL) IS ON, DURING THE CMOS UPDATE BOTH NMI AND    :
;		NORMAL INTERRUPTS ARE DISABLED TO PROTECT CMOS DATA INTEGRITY. :
;		THE CMOS ADDRESS REGISTER IS POINTED TO A DEFAULT VALUE AND    :
;		THE INTERRUPT FLAG RESTORED TO THE ENTRY STATE ON RETURN.      :
;		ONLY THE CMOS LOCATION AND THE NMI STATE IS CHANGED.	       :
;-------------------------------------------------------------------------------

	; 08/08/2022
CMOS_WRITE:				; WRITE (AH) TO LOCATION (AL)
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	;pushf				; SAVE INTERRUPT ENABLE STATUS AND FLAGS
	;;push	ax			; SAVE WORK REGISTER VALUES
	; 18/04/2021
	;push	eax
	rol	al, 1			; MOVE NMI BIT TO LOW POSITION
	stc				; FORCE NMI BIT ON IN CARRY FLAG
	rcr	al, 1			; HIGH BIT ON TO DISABLE NMI - OLD IN CY
	cli				; DISABLE INTERRUPTS
	out	CMOS_PORT, al		; ADDRESS LOCATION AND DISABLE NMI
	mov	al, ah			; GET THE DATA BYTE TO WRITE
	out	CMOS_DATA, al		; PLACE IN REQUESTED CMOS LOCATION
	mov	al, CMOS_SHUT_DOWN*2	; GET ADDRESS OF DEFAULT LOCATION
	;mov	al, CMOS_REG_D*2 	; GET ADDRESS OF DEFAULT LOCATION
	rcr	al, 1			; PUT ORIGINAL NMI MASK BIT INTO ADDRESS
	out	CMOS_PORT, al		; SET DEFAULT TO READ ONLY REGISTER
	;nop				; I/O DELAY
	; 29/07/2022
	out	0EBh, al ; NEWIODELAY ; AWARD BIOS 1999, ATIME.ASM
	in	al, CMOS_DATA		; OPEN STANDBY LATCH
	;;pop	ax			; RESTORE WORK REGISTERS
	; 18/04/2021
	;pop	eax
	;popf
	; 29/07/2022
	;clc
	; 08/08/2022
	xor	al, al
	retn

;-------------------------------------------------------------------------------

RTC_60:				; SET RTC ALARM
	mov	al, CMOS_REG_B		; ADDRESS ALARM
	call	CMOS_READ		; READ ALARM REGISTER
	test	al, 20h			; CHECK FOR ALARM ALREADY ENABLED
	stc				; SET CARRY IN CASE OF ERROR
	jnz	short RTC_69		; ERROR EXIT IF ALARM SET
	call	UPD_IPR			; CHECK FOR UPDATE IN PROCESS
	jnc	short RTC_65		; SKIP INITIALIZATION IF NO ERROR
	call	RTC_STA			; ELSE INITIALIZE CLOCK
RTC_65:	
	mov	ah, dh			; GET SECONDS BYTE
	mov	al, CMOS_SEC_ALARM	; ADDRESS THE SECONDS ALARM REGISTER
	call	CMOS_WRITE		; INSERT SECONDS
	mov	ah, cl			; GET MINUTES PARAMETER
	mov	al, CMOS_MIN_ALARM	; ADDRESS MINUTES ALARM REGISTER
	call	CMOS_WRITE		; INSERT MINUTES
	mov	ah, ch			; GET HOURS PARAMETER
	mov	al, CMOS_HR_ALARM	; ADDRESS HOUR ALARM REGISTER
	call	CMOS_WRITE		; INSERT HOURS
	in	al, INTB01		; READ SECOND INTERRUPT MASK REGISTER
	and	al, 0FEh		; ENABLE ALARM TIMER BIT (CY= 0)
	out	INTB01, al		; WRITE UPDATED MASK
	;mov	al, CMOS_REG_B		; ADDRESS ALARM REGISTER
	;mov	ah, al
	mov	ax, CMOS_REG_B * 257
	call	CMOS_READ		; READ CURRENT ALARM REGISTER
	and	al, 07Fh		; ENSURE SET BIT TURNED OFF
	or	al, 20h			; TURN ON ALARM ENABLE
	xchg	ah, al			; MOVE MASK TO OUTPUT REGISTER
	call	CMOS_WRITE		; WRITE NEW ALARM MASK
	; 29/07/2022
	;clc				; SET CY= 0
RTC_69:
	mov	ax, 0			; CLEAR AX REGISTER
	retn				; RETURN WITH RESULTS IN CARRY FLAC

RTC_70:				; RESET ALARM
	;mov	al, CMOS_REG_B		; ADDRESS ALARM REGISTER
	;mov	ah, al
	mov	ax, CMOS_REG_B * 257	; ADDRESS ALARM REGISTER (TO BOTH AH,AL)
	call	CMOS_READ		; READ ALARM REGISTER
	and	al, 57h			; TURN OFF ALARM ENABLE
	xchg	ah, al			; SAVE DATA AND RECOVER ADDRESS
	;call	CMOS_WRITE		; RESTORE NEW VALUE
	;clc				; SET CY= 0
	;retn				; RETURN WITH NO CARRY
	; 29/07/2022
	jmp	short CMOS_WRITE

;-------------------------------------------------------------------------------

; 17/01/2016 (TRDOS 386 = TRDOS v2.0)

;--- HARDWARE INT 70 H -- ( IRQ LEVEL 8) ----------------------------------------
; ALARM INTERRUPT HANDLER (RTC)							:
;       THIS ROUTINE HANDLES THE PERIODIC AND ALARM INTERRUPTS FROM THE CMOS	:
;       TIMER. INPUT FREQUENCY IS 1.024 KHZ OR APPROXIMATELY 1024 INTERRUPTS	:
;       EVERY SECOND FOR THE PERIODIC INTERRUPT. FOR THE ALARM FUNCTION,	:
;       THE INTERRUPT WILL OCCUR AT THE DESIGNATED TIME.			:
;										:
;       INTERRUPTS ARE ENABLED WHEN THE EVENT OR ALARM FUNCTION IS ACTIVATED.	:
;       FOR THE EVENT INTERRUPT, THE HANDLER WILL DECREMENT THE WAIT COUNTER	:
;       AND WHEN IT EXPIRES WILL SET THE DESIGNATED LOCATION TO 80H. FOR	:
;       THE ALARM INTERRUPT. THE USER MUST PROVIDE A ROUTINE TO INTERCEPT	:
;       THE CORRECT ADDRESS FROM THE VECTOR TABLE INVOKED BY INTERRUPT 4AH	:
;       PRIOR TO SETTING THE REAL TIME CLOCK ALARM (INT 1AH, AH= 06H).		:
;--------------------------------------------------------------------------------

RTC_A_INT: ; 07/01/2017
;RTC_INT:				; ALARM INTERRUPT
	push	ds			; LEAVE INTERRUPTS DISABLED
	push	eax			; SAVE REGISTERS
	push	edi
RTC_I_1:				; CHECK FOR SECOND INTERRUPT
	mov	ax, 256*(CMOS_REG_B+NMI)+CMOS_REG_C+NMI ; ALARM AND STATUS
	out	CMOS_PORT, al		; WRITE ALARM FLAG MASK ADDRESS
	nop				; I/O DELAY
	jmp	short $+2
	in	al, CMOS_DATA		; READ AND RESET INTERRUPT REQUEST FLAGS
	test	al, 01100000b		; CHECK FOR EITHER INTERRUPT PENDING
	jz	short	RTC_I_9		; EXIT IF NOT A VALID RTC INTERRUPT

	xchg	ah, al			; SAVE FLAGS AND GET ENABLE ADDRESS
	out	CMOS_PORT, al		; WRITE ALARM ENABLE MASK ADDRESS
	nop				; I/O DELAY
	jmp	short $+2	
	in	al, CMOS_DATA		; READ CURRENT ALARM ENABLE MASK
	and	al, ah			; ALLOW ONLY SOURCES THAT ARE ENABLED
	test	al, 01000000b		; CHECK FOR PERIODIC INTERRUPT
	jz	short RTC_I_5		; SKIP IF NOT A PERIODIC INTERRUPT

;-----	DECREMENT WAIT COUNT BY INTERRUPT INTERVAL

	mov	di, KDATA		; kernel data segment
	mov	ds, di
	
	sub	dword [RTC_LH], 976	; DECREMENT COUNT BY 1/1024
	jnc	short RTC_I_5		; SKIP TILL 32 BIT WORD LESS THAN ZERO

;-----	TURN OFF PERIODIC INTERRUPT ENABLE

	;push	ax			; SAVE INTERRUPT FLAG MASK
	; 18/04/2021
	push	eax
	mov	ax, 257*(CMOS_REG_B+NMI) ; INTERRUPT ENABLE REGISTER
	out	CMOS_PORT, al		; WRITE ADDRESS TO CMOS CLOCK
	nop				; I/O DELAY
	jmp	short $+2
	in	al, CMOS_DATA		; READ CURRENT ENABLES
	and	al, 0BFh		; TURN OFF PIE
	xchg	al, ah			; GET CMOS ADDRESS AND SAVE VALUE
	out	CMOS_PORT, al		; ADDRESS REGISTER B
	xchg	al, ah			; GET NEW INTERRUPT ENABLE MASK
	out	CMOS_DATA, al		; SET MASK IN INTERRUPT ENABLE REGISTER
	mov	byte [RTC_WAIT_FLAG], 0	; SET FUNCTION ACTIVE FLAG OFF
	mov	edi, [USER_FLAG]	; SET UP (DS:DI) TO POINT TO USER FLAG
	mov	byte [edi], 80h		; TURN ON USERS FLAG
	;pop	ax			; GET INTERRUPT SOURCE BACK
	; 18/04/2021
	pop	eax
RTC_I_5:
	test	al, 00100000b		; TEST FOR ALARM INTERRUPT
	jz	short RTC_I_7		; SKIP USER INTERRUPT CALL IF NOT ALARM

	mov	al, CMOS_REG_D		; POINT TO DEFAULT READ ONLY REGISTER
	out	CMOS_PORT, al		; ENABLE NMI AND CMOS ADDRESS TO DEFAULT
	sti				; INTERRUPTS BACK ON NOW
	push	edx
	call	INT4Ah			; TRANSFER TO USER ROUTINE
	pop	edx
	cli				; BLOCK INTERRUPT FOR RETRY
RTC_I_7:				; RESTART ROUTINE TO HANDLE DELAYED
	jmp	short RTC_I_1		;  ENTRY AND SECOND EVENT BEFORE DONE

RTC_I_9:				; EXIT - NO PENDING INTERRUPTS
	mov	al, CMOS_REG_D		; POINT TO DEFAULT READ ONLY REGISTER
	out	CMOS_PORT, al		; ENABLE NMI AND CMOS ADDRESS TO DEFAULT
	mov	al, EOI			; END OF INTERRUPT MASK TO 8259 - 2
	out	INTB00, al		; TO 8259 - 2
	out	INTA00,	al		; TO 8259 - 1
	pop	edi			; RESTORE REGISTERS
	pop	eax
	pop	ds
	iretd				; END OF INTERRUPT

;-------------------------------------------------------------------------------

	; 29/05/2016 - TRDOS 386 (TRDOS v2.0)
	; 22/08/2014 (Retro UNIX 386 v1)
	; IBM PC/AT BIOS source code ----- 10/06/85 (bios2.asm)
UPD_IPR:				; WAIT TILL UPDATE NOT IN PROGRESS
	push	ecx

	; 29/05/2016
	mov	ecx, ((1984+244)*4)/2	; AWARD BIOS 1999, ATIME.ASM		
					; 'WAITCPU_CK_UD_STAT'
					; (244Us + 1984Us)
					; (assume each read takes
					;  2 microseconds).
	;mov	ecx, 65535		
		;mov cx, 800		; SET TIMEOUT LOOP COUNT (= 800)	
UPD_10:
	mov	al, CMOS_REG_A		; ADDRESS STATUS REGISTER A
	cli				; NO TIMER INTERRUPTS DURING UPDATES
	call	CMOS_READ		; READ UPDATE IN PROCESS FLAG
	test	al, 80h			; IF UIP BIT IS ON ( CANNOT READ TIME )
	jz	short UPD_90		; EXIT WITH CY= 0 IF CAN READ CLOCK NOW
	sti				; ALLOW INTERRUPTS WHILE WAITING
	loop	UPD_10			; LOOP TILL READY OR TIMEOUT
	xor	eax, eax ; xor ax, ax	; CLEAR RESULTS IF ERROR
	stc				; SET CARRY FOR ERROR
UPD_90:
	pop	ecx			; RESTORE CALLERS REGISTER
	cli				; INTERRUPTS OFF DURING SET
	retn				; RETURN WITH CY FLAG SET

	; 18/04/2021
RTC_STA:			; INITIALIZE REAL TIME CLOCK
	;mov	al, CMOS_REG_A		; ADDRESS REGISTER A AND LOAD DATA MASK		
	;mov	ah, 26h
	mov	ax, (26h*100h)+CMOS_REG_A
	call	CMOS_WRITE		; INITIALIZE STATUS REGISTER A
	;mov	al, CMOS_REG_B		; SET "SET BIT" FOR CLOCK INITIALIZATION	
	;mov	ah, 82h
	mov	ax, (82h*100h)+CMOS_REG_B
	call	CMOS_WRITE		; AND 24 HOUR MODE TO REGISTER B
	mov	al, CMOS_REG_C		; ADDRESS REGISTER C
	call	CMOS_READ		; READ REGISTER C TO INITIALIZE
	mov	al, CMOS_REG_D		; ADDRESS REGISTER D
	; 18/04/2021
	;call	CMOS_READ		; READ REGISTER D TO INITIALIZE
	;retn
	;jmp	short CMOS_READ ; 18/04/2021

;-------------------------------------------------------------------------------

	; 29/07/2022 - TRDOS 386 v2.0.5
	; 18/04/2021 - TRDOS 386 v2.0.4
	; 29/05/2016 - TRDOS 386 (TRDOS v2.0) 
	; 22/08/2014 (Retro UNIX 386 v1)
	; IBM PC/AT BIOS source code ----- 10/06/85 (test4.asm)

;--- CMOS_READ -----------------------------------------------------------------
;		READ BYTE FROM CMOS_SYSTEM CLOCK CONFIGURATION TABLE	       :
;									       :
; INPUT: (AL)=	CMOS_TABLE ADDRESS TO BE READ				       :
;		BIT    7 = 0 FOR NMI ENABLED AND 1 FOR NMI DISABLED ON EXIT    :
;		BITS 6-0 = ADDRESS OF TABLE LOCATION TO READ		       :
;									       :
; OUTPUT: (AL)	VALUE AT LOCATION (AL) MOVED INTO (AL). IF BIT 7 OF (AL) WAS   :
;		ON THEN NMI LEFT DISABLED, DURING THE CMOS READ BOTH NMI AND   :
;		NORMAL INTERRUPTS ARE DISABLED TO PROTECT CMOS DATA INTEGRITY. :
;		THE CMOS ADDRESS REGISTER IS POINTED TO A DEFAULT VALUE AND    :
;		THE INTERRUPT FLAG RESTORED TO THE ENTRY STATE ON RETURN.      :
;		ONLY THE (AL) REGISTER AND THE NMI STATE IS CHANGED.	       :
;-------------------------------------------------------------------------------

CMOS_READ:
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	;pushf				; SAVE INTERRUPT ENABLE STATUS AND FLAGS
	rol	al, 1			; MOVE NMI BIT TO LOW POSITION
	stc				; FORCE NMI BIT ON IN CARRY FLAG
	rcr	al, 1			; HIGH BIT ON TO DISABLE NMI - OLD IN CY
	cli				; DISABLE INTERRUPTS
	out	CMOS_PORT, al		; ADDRESS LOCATION AND DISABLE NMI
	; 29/05/2016
	;nop				; I/O DELAY
	out	0EBh, al ; NEWIODELAY ; AWARD BIOS 1999, ATIME.ASM
	;
	in	al, CMOS_DATA		; READ THE REQUESTED CMOS LOCATION
	;push	ax	; SAVE (AH) REGISTER VALUE AND CMOS BYTE
	; 18/04/2021
	push	eax
	; 15/03/2015 ; IBM PC/XT Model 286 BIOS source code 
		     ; ----- 10/06/85 (test4.asm)
	mov	al, CMOS_SHUT_DOWN*2 	; GET ADDRESS OF DEFAULT LOCATION
	;mov	al, CMOS_REG_D*2 	; GET ADDRESS OF DEFAULT LOCATION
	rcr	al, 1			; PUT ORIGINAL NMI MASK BIT INTO ADDRESS
	out	CMOS_PORT, al		; SET DEFAULT TO READ ONLY REGISTER
	;pop	ax			; RESTORE (AH) AND (AL), CMOS BYTE
	; 18/04/2021
	pop	eax
	; 29/07/2022
	;popf
	clc	; 29/07/2022	
	retn				; RETURN WITH FLAGS RESTORED

;-------------------------------------------------------------------------------

	; 03/05/2026
%if 0

	; 02/05/2026 - TRDOS 386 v2.0.11
	
	; ref: IBM PC XT286 BIOS - "test4.asm"

;-------------------------------------------------------------------------------
;	THIS ROUTINE INITIALIZES THE TIMER DATA AREA IN THE ROM BIOS	       :
;	DATA AREA.  IT IS CALLED BY THE POWER ON ROUTINES.  IT CONVERTS        :
;	HR:MIN:SEC  FROM CMOS TO TIMER TICS. IF CMOS IS INVALID, TIMER	       :
;	IS SET TO ZERO. 						       :
;									       :
; INPUT    NONE PASSED TO ROUTINE BY CALLER				       :
;	   CMOS LOCATIONS USED FOR TIME 				       :
;									       :
; OUTPUT   @TIMER_LOW							       :
;	   @TIMER_HIGH							       :
;	   @TIMER_OFL							       :
;	   ALL REGISTERS UNCHANGED					       :
;-------------------------------------------------------------------------------

COUNTS_SEC	EQU	18		; TIMER DATA CONVERSION EQUATES
COUNTS_MIN	EQU	1092
COUNTS_HOUR	EQU	7		; 65543 - 65536
UPDATE_TIMER	EQU	10000000b	; RTC UPDATE IN PROCESS BIT MASK
CMOS_DIAG	EQU	0Eh		; POST DIAGNOSTIC STATUS RESULTS BYTE
NMI		EQU	10000000b	; DISABLE NMI INTERRUPTS MASK -
					;  HIGH BIT OF CMOS LOCATION ADDRESS
BAD_BAT 	EQU	10000000b	; DEAD BATTERY - CMOS LOST POWER
BAD_CKSUM	EQU	01000000b	; CHECKSUM ERROR
CMOS_CLK_FAIL	EQU	00000100b	; CMOS CLOCK NOT UPDATING OR NOT VALID

CMOS_REG_A	EQU	0Ah		; STATUS REGISTER A
CMOS_SECONDS	EQU	00h		; SECONDS
CMOS_MINUTES	EQU	02h		; MINUTES
CMOS_HOURS	EQU	04h		; HOURS

	; 02/05/2026
	; set time of day (re-initialize timer tick count)
SET_TOD:
	sub	eax, eax
	mov	[TIMER_OFL], al		; RESET TIMER ROLL OVER INDICATOR
	;mov	word [TIMER_LOW], ax	; AND TIMER COUNT
	;mov	word [TIMER_HIGH], ax
	mov	[TIMER_LH], eax
	mov	al, CMOS_DIAG+NMI	; CHECK CMOS VALIDITY
	call	CMOS_READ		; READ DIAGNOSTIC LOCATION IN CMOS
	and	al, BAD_BAT+BAD_CKSUM+CMOS_CLK_FAIL

	jnz	short TOD_DONE		; CMOS NOT VALID -- TIMER SET TO ZERO
	mov	ecx, 65535		; BAD BATTERY, CHKSUM ERROR, CLOCK ERROR
UIP:
	mov	al, CMOS_REG_A+NMI	; ACCESS REGISTER A
	call	CMOS_READ		; READ CMOS CLOCK REGISTER A
	test	al, UPDATE_TIMER
	loopz	UIP			; WAIT TILL UPDATE BIT IS ON

	jecxz	POD_DONE		; CMOS CLOCK STUCK IF TIMEOUT
	mov	cx, 65535		;
UIPOFF:
	mov	al, CMOS_REG_A+NMI	; ACCESS REGISTER A
	call	CMOS_READ		; READ CMOS CLOCK REGISTER A
	test	al, UPDATE_TIMER
	loopnz	UIPOFF			; NEXT WAIT TILL END OF UPDATE

	jecxz	POD_DONE		; CMOS CLOCK STUCK IF TIMEOUT

	mov	al, CMOS_SECONDS+NMI	; TIME JUST UPDATED
	CALL	CMOS_READ		; ACCESS SECONDS VALUE IN CMOS
	cmp	al, 59h			; ARE THE SECONDS WITHIN LIMITS?
	ja	short TOD_ERROR		; GO IF NOT

	call	CVT_BINARY		; CONVERT IT TO BINARY
	mov	ecx, eax		; MOVE COUNT TO ACCUMULATION REGISTER
	shr	ecx, 2			; ADJUST FOR SYSTEMATIC SECONDS ERROR
	mov	bl, COUNTS_SEC
	mul	bl			; COUNT FOR SECONDS
	add	ecx, eax
	mov	al, CMOS_MINUTES+NMI
	call	CMOS_READ		; ACCESS MINUTES VALUE IN CMOS
	cmp	al, 59h			; ARE THE MINUTES WITHIN LIMITS?
	ja	short TOD_ERROR		; GO IF NOT
	call	CVT_BINARY		; CONVERT IT TO BINARY
	push	eax			; SAVE MINUTES COUNT
	shr	eax, 1			; ADJUST FOR SYSTEMATIC MINUTES ERROR
	add	ecx, eax		; ADD ADJUSTMENT TO COUNT
	pop	eax			; RECOVER BCD MINUTES VALUE
	mov	ebx, COUNTS_MIN
	mul	ebx			; COUNT FOR MINUTES
	add	ecx, eax		; ADD TO ACCUMULATED VALUE
	mov	al, CMOS_HOURS+NMI
	call	CMOS_READ		; ACCESS HOURS VALUE IN CMOS
	cmp	al, 23h			; ARE THE HOURS WITHIN LIMITS?
	ja	short TOD_ERROR		; GO IF NOT

	call	CVT_BINARY		; CONVERT IT TO BINARY
	;shl	eax, 16			; hour in hw of eax
	;; eax = 65536 * hour
	;mov	bl, COUNTS_HOUR
	;mul	bl			; COUNT FOR HOURS
	;; + (7 * hour)
	;; eax = 65543 * hour
	mov	ebx, 65543
	mul	ebx
	add	eax, ecx
	cli
	mov	[TIMER_LH], eax
	sti
TOD_DONE:
	retn

TOD_ERROR:
	mov	esi, E163		; DISPLAY CLOCK ERROR
	call	print_msg
	mov	eax, 257*(CMOS_DIAG+NMI) ; SET CLOCK ERROR IN STATUS
	CALL	CMOS_READ		; READ DIAGNOSTIC CMOS LOCATION
	or	al, CMOS_CLK_FAIL	; SET NEW STATUS WITH CMOS CLOCK ERROR
	xchg	al, ah			; MOVE NEW STATUS TO WORK REGISTER
	call	CMOS_WRITE		; UPDATE STATUS LOCATION
	retn

CVT_BINARY:
	;mov	ah, al			; UNPACK 2 BCD DIGITS IN AL
	;shr	ah, 4
	;and	al, 0Fh			; RESULT IS IN AX
	;aad				; CONVERT UNPACKED BCD TO BINARY
	; 02/05/2026
	db 	0D4h, 10h		; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
	aad				; AL = AH * 10 + AL, AH = 0
	retn

E163:	
	db 0Dh, 0Ah, 7
	db "Clock not updating !", 0Dh, 0Ah, 0

%endif

; /// End Of TIMER FUNCTIONS ///
