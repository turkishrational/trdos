; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel) - v2.0.5 - keyboard.s
; ----------------------------------------------------------------------------
; Last Update: 07/08/2022 (Previous: 12/04/2021)
; ----------------------------------------------------------------------------
; Beginning: 17/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
;
; Derived from 'Retro UNIX 386 Kernel - v0.2.1.0' source code by Erdogan Tan
; keyboard.inc (17/10/2015)
;
; Derived from 'IBM PC-XT-286' BIOS source code (1986) 
; ****************************************************************************

; Ref: Retro UNIX 386 v1.2 - keyboard.s - 11/06/2022

; Retro UNIX 386 v1 Kernel - KEYBOARD.INC
; Last Modification: 17/10/2015
;		    (Keyboard Data is in 'KYBDATA.INC')	
;
; ///////// KEYBOARD FUNCTIONS (PROCEDURES) ///////////////

; 17/01/2016 (TRDOS 386 = TRDOS v2.0)

; 03/12/2014
; 26/08/2014
; KEYBOARD I/O
; (INT_16h - Retro UNIX 8086 v1 - U9.ASM, 30/06/2014)

;NOTE: 'k0' to 'k7' are name of OPMASK registers.
;	(The reason of using '_k' labels!!!) (27/08/2014)    
;NOTE: 'NOT' keyword is '~' unary operator in NASM.
;	('NOT LC_HC' --> '~LC_HC') (bit reversing operator)

int16h:	; 30/06/2015
;getc:
	pushfd	; 28/08/2014
	push 	cs
	call 	KEYBOARD_IO_1 ; getc_int
	retn	

; 24/07/2022 - TRDOS 386 v2.0.5

	;-----	SHIFT STATUS
_K3E:                                   ; GET THE EXTENDED SHIFT STATUS FLAGS
	mov	ah, [KB_FLAG_1]		; GET SYSTEM SHIFT KEY STATUS
	and	ah, SYS_SHIFT		; MASK ALL BUT SYS KEY BIT
	;mov	cl, 5			; SHIFT THEW SYSTEMKEY BIT OVER TO
	;shl	ah, cl			; BIT 7 POSITION
        shl	ah, 5
	mov	al, [KB_FLAG_1]		; GET SYSTEM SHIFT STATES BACK
	and	al, 01110011b		; ELIMINATE SYS SHIFT, HOLD_STATE AND INS_SHIFT
	or	ah, al                  ; MERGE REMAINING BITS INTO AH
	mov	al, [KB_FLAG_3]		; GET RIGHT CTL AND ALT
	and	al, 00001100b		; ELIMINATE LC_E0 AND LC_E1
	or	ah, al			; OR THE SHIFT FLAGS TOGETHER
_K3:
	mov	al, [KB_FLAG]		; GET THE SHIFT STATUS FLAGS
	; 24/07/2022
	jmp	short _KIO_EXIT		; RETURN TO CALLER

getc_int:
	; 28/02/2015
	; 03/12/2014 (derivation from pc-xt-286 bios source code -1986-, 
	;	      instead of pc-at bios - 1985-)
	; 28/08/2014 (_k1d)
	; 30/06/2014
	; 03/03/2014
	; 28/02/2014
	; Derived from "KEYBOARD_IO_1" procedure of IBM "pc-xt-286" 
	; rombios source code (21/04/1986)
	;	 'keybd.asm', INT 16H, KEYBOARD_IO
	;
	; KYBD --- 03/06/86  KEYBOARD BIOS
	;
	;--- INT 16 H -----------------------------------------------------------------
	; KEYBOARD I/O								      :
	;	THESE ROUTINES PROVIDE READ KEYBOARD SUPPORT			      :
	; INPUT									      :
	;	(AH)= 00H  READ THE NEXT ASCII CHARACTER ENTERED FROM THE KEYBOARD,   :
	;		   RETURN THE RESULT IN (AL), SCAN CODE IN (AH).              :
	;		   THIS IS THE COMPATIBLE READ INTERFACE, EQUIVALENT TO THE   :
	;                  STANDARD PC OR PCAT KEYBOARD				      :	
	;-----------------------------------------------------------------------------:
	;	(AH)= 01H  SET THE ZERO FLAG TO INDICATE IF AN ASCII CHARACTER IS     :
	;		   AVAILABLE TO BE READ FROM THE KEYBOARD BUFFER.	      :
	;		   (ZF)= 1 -- NO CODE AVAILABLE			              :
	;		   (ZF)= 0 -- CODE IS AVAILABLE  (AX)= CHARACTER              :
	;		   IF (ZF)= 0, THE NEXT CHARACTER IN THE BUFFER TO BE READ IS :
	;		   IN (AX), AND THE ENTRY REMAINS IN THE BUFFER.              :
	;		   THIS WILL RETURN ONLY PC/PCAT KEYBOARD COMPATIBLE CODES    :
	;-----------------------------------------------------------------------------:	
	;	(AH)= 02H  RETURN THE CURRENT SHIFT STATUS IN AL REGISTER             :
	;		   THE BIT SETTINGS FOR THIS CODE ARE INDICATED IN THE        :
	;		   EQUATES FOR @KB_FLAG		                              :
	;-----------------------------------------------------------------------------:	
	;	(AH)= 03H  SET TYPAMATIC RATE AND DELAY                               :
	;	      (AL) = 05H                                                      :
	;	      (BL) = TYPAMATIC RATE (BITS 5 - 7 MUST BE RESET TO 0)           :
	;		       							      :
	;                     REGISTER     RATE      REGISTER     RATE                :
	;                      VALUE     SELECTED     VALUE     SELECTED              :
	;                     --------------------------------------------            :
	;			00H        30.0        10H        7.5                 :
	;			01H        26.7        11H        6.7                 :
	;			02H        24.0        12H        6.0                 :
	;			03H        21.8        13H        5.5                 :
	;			04H        20.0        14H        5.0                 :
	;			05H        18.5        15H        4.6                 :
	;			06H        17.1        16H        4.3                 :
	;			07H        16.0        17H        4.0                 :
	;			08H        15.0        18H        3.7                 :
	;			09H        13.3        19H        3.3                 :
	;			0AH        12.0        1AH        3.0                 :
	;			0BH        10.9        1BH        2.7                 :
        ;			0CH        10.0        1CH        2.5                 :
	;			0DH         9.2        1DH        2.3                 :
	;			0EH         8.6        1EH        2.1                 :
	;			0FH         8.0        1FH        2.0                 :
	;									      :
	;	      (BH) = TYPAMATIC DELAY  (BITS 2 - 7 MUST BE RESET TO 0)         :
	;		       							      :
	;                     REGISTER     DELAY                                      :
	;                      VALUE       VALUE                                      :
	;                     ------------------                                      :
	;			00H        250 ms                                     :
	;			01H        500 ms                                     :
	;			02H        750 ms                                     :
	;			03H       1000 ms                                     :
	;-----------------------------------------------------------------------------:
	;	(AH)= 05H  PLACE ASCII CHARACTER/SCAN CODE COMBINATION IN KEYBOARD    :
	;		   BUFFER AS IF STRUCK FROM KEYBOARD                          :
	;		   ENTRY:  (CL) = ASCII CHARACTER		              :
	;		           (CH) = SCAN CODE                                   :
	;		   EXIT:   (AH) = 00H = SUCCESSFUL OPERATION                  :
	;		           (AL) = 01H = UNSUCCESSFUL - BUFFER FULL            :
	;		   FLAGS:  CARRY IF ERROR                                     :
	;-----------------------------------------------------------------------------:		
	;	(AH)= 10H  EXTENDED READ INTERFACE FOR THE ENHANCED KEYBOARD,         :
	;		   OTHERWISE SAME AS FUNCTION AH=0                            :
	;-----------------------------------------------------------------------------:
	;	(AH)= 11H  EXTENDED ASCII STATUS FOR THE ENHANCED KEYBOARD,           :
	;		   OTHERWISE SAME AS FUNCTION AH=1                            :
	;-----------------------------------------------------------------------------:	
	;	(AH)= 12H  RETURN THE EXTENDED SHIFT STATUS IN AX REGISTER            :
	;		   AL = BITS FROM KB_FLAG, AH = BITS FOR LEFT AND RIGHT       :
	;		   CTL AND ALT KEYS FROM KB_FLAG_1 AND KB_FLAG_3              :
	; OUTPUT					                              :
	;	AS NOTED ABOVE, ONLY (AX) AND FLAGS CHANGED	                      :
	;	ALL REGISTERS RETAINED		                                      :
	;------------------------------------------------------------------------------

; 07/08/2022
; 24/07/2022 - TRDOS 386 v2.0.5
; 12/04/2021 - TRDOS 386 v2.0.3 (32 bit push/pop)
; 15/01/2017
; 14/01/2017
; 02/01/2017
; 29/05/2016
; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
int32h:  ; Keyboard BIOS

KEYBOARD_IO_1:	
	;sti				; INTERRUPTS BACK ON
	; 29/05/2016
        and     byte [esp+8], 10111110b ; clear zero flag and cary flag
	;
	push	ds			; SAVE CURRENT DS
	push	ebx			; SAVE BX TEMPORARILY
	;push	ecx			; SAVE CX TEMPORARILY
        mov     bx, KDATA
	mov	ds, bx			; PUT SEGMENT VALUE OF DATA AREA INTO DS
	; 14/01/2017
	mov	ebx, [esp]
	;; 15/01/2017
	; 02/01/2017
	;;mov	byte [intflg], 32h	; keyboard interrupt 
	sti
	;
	or	ah, ah			; CHECK FOR (AH)= 00H
	jz	short _K1		; ASCII_READ
	dec	ah                      ; CHECK FOR (AH)= 01H
        jz      short _K2               ; ASCII_STATUS
	dec	ah			; CHECK FOR (AH)= 02H
        jz	short _K3		; SHIFT STATUS
	dec	ah			; CHECK FOR (AH)= 03H	
        jz      short _K300		; SET TYPAMATIC RATE/DELAY
	sub	ah, 2			; CHECK FOR (AH)= 05H	
        ;jz	short _K500		; KEYBOARD WRITE
	; 07/08/2022
	jnz	short _KIO1
	jmp	_K500         
_KIO1:	
	sub	ah, 11			; AH =  10H
	jz	short _K1E		; EXTENDED ASCII READ
	dec	ah			; CHECK FOR (AH)= 11H
	jz	short _K2E		; EXTENDED_ASCII_STATUS
	dec	ah			; CHECK FOR (AH)= 12H
	jz	short _K3E		; EXTENDED_SHIFT_STATUS
_KIO_EXIT:
	; 02/01/2017
	cli
	;;mov	byte [intflg], 0 ;; 15/01/2017
	;
	;pop	ecx			; RECOVER REGISTER
	pop	ebx			; RECOVER REGISTER
	pop	ds			; RECOVER SEGMENT
	iretd				; INVALID COMMAND, EXIT

; 24/07/2022
;
;	;-----	SHIFT STATUS
;_K3E:					; GET THE EXTENDED SHIFT STATUS FLAGS
;	mov	ah, [KB_FLAG_1]		; GET SYSTEM SHIFT KEY STATUS
;	and	ah, SYS_SHIFT		; MASK ALL BUT SYS KEY BIT
;	;mov	cl, 5			; SHIFT THEW SYSTEMKEY BIT OVER TO
;	;shl	ah, cl			; BIT 7 POSITION
;       shl	ah, 5
;	mov	al, [KB_FLAG_1]		; GET SYSTEM SHIFT STATES BACK
;	and	al, 01110011b		; ELIMINATE SYS SHIFT, HOLD_STATE AND INS_SHIFT
;	or	ah, al                  ; MERGE REMAINING BITS INTO AH
;	mov	al, [KB_FLAG_3]		; GET RIGHT CTL AND ALT
;	and	al, 00001100b		; ELIMINATE LC_E0 AND LC_E1
;	or	ah, al			; OR THE SHIFT FLAGS TOGETHER
;_K3:
;	mov	al, [KB_FLAG]		; GET THE SHIFT STATUS FLAGS
;	; 24/07/2022
;	jmp	short _KIO_EXIT		; RETURN TO CALLER

	;-----	ASCII CHARACTER
_K1E:	
	call	_K1S			; GET A CHARACTER FROM THE BUFFER (EXTENDED)
	call	_KIO_E_XLAT		; ROUTINE TO XLATE FOR EXTENDED CALLS
	jmp	short _KIO_EXIT         ; GIVE IT TO THE CALLER
_K1:	
	call	_K1S			; GET A CHARACTER FROM THE BUFFER
	call	_KIO_S_XLAT		; ROUTINE TO XLATE FOR STANDARD CALLS
	jc	short _K1		; CARRY SET MEANS TROW CODE AWAY
_K1A:
	jmp	short _KIO_EXIT         ; RETURN TO CALLER

	;-----	ASCII STATUS
_K2E:	
	call	_K2S			; TEST FOR CHARACTER IN BUFFER (EXTENDED)
	jz	short _K2B		; RETURN IF BUFFER EMPTY
	pushf				; SAVE ZF FROM TEST
	call	_KIO_E_XLAT		; ROUTINE TO XLATE FOR EXTENDED CALLS
	jmp	short _K2A	        ; GIVE IT TO THE CALLER
_K2:	
	call	_K2S			; TEST FOR CHARACTER IN BUFFER
	jz	short _K2B		; RETURN IF BUFFER EMPTY
	pushf				; SAVE ZF FROM TEST
	call	_KIO_S_XLAT		; ROUTINE TO XLATE FOR STANDARD CALLS
	jnc	short _K2A	        ; CARRY CLEAR MEANS PASS VALID CODE
	popf				; INVALID CODE FOR THIS TYPE OF CALL
	call	_K1S			; THROW THE CHARACTER AWAY
	jmp	short _K2		; GO LOOK FOR NEXT CHAR, IF ANY
_K2A:
	popf				; RESTORE ZF FROM TEST
_K2B:
	; 02/01/2017
	cli
	;; mov	byte [intflg], 0 ;; 15/01/2017
	;
	;pop	ecx			; RECOVER REGISTER
	pop	ebx			; RECOVER REGISTER
	pop	ds			; RECOVER SEGMENT
	; (*) 29/05/2016
	; (*) retf 4			; THROW AWAY (e)FLAGS
	jc	short _k2d
	jnz	short _k2c
	or	byte [esp+8], 01000000b	; set zero flag bit of eflags register
_k2c:
	iretd
_k2d:
	; 29/05/2016 -set carry flag on stack-
	; [esp] = EIP
	; [esp+4] = CS
	; [esp+8] = E-FLAGS
	or	byte [esp+8], 1  ; set carry bit of eflags register
	; [esp+12] = ESP (user)
	; [esp+16] = SS (User)
	iretd
	
	; (*) 29/05/2016 - 'retf 4' intruction causes to stack fault
	; (OUTER-PRIVILEGE-LEVEL)
	; INTEL 80386 PROGRAMMER'S REFERENCE MANUAL 1986
	; // RETF instruction:
	;
	; IF OperandMode=32 THEN
 	;    Load CS:EIP from stack;
 	;    Set CS RPL to CPL;
 	;    Increment ESP by 8 plus the immediate offset if it exists;
 	;    Load SS:ESP from stack;
 	; ELSE (* OperandMode=16 *)
 	;    Load CS:IP from stack;
 	;    Set CS RPL to CPL;
 	;    Increment SP by 4 plus the immediate offset if it exists;
	;    Load SS:SP from stack;
 	; FI;
	;
	; //

	; 24/07/2022
	;-----	SET TYPAMATIC RATE AND DELAY
_K300:
	cmp	al, 5			; CORRECT FUNCTION CALL?
	jne	short _KIO_EXIT		; NO, RETURN
	test	bl, 0E0h		; TEST FOR OUT-OF-RANGE RATE
	jnz	short _KIO_EXIT		; RETURN IF SO
	test	bh, 0FCh		; TEST FOR OUT-OF-RANGE DELAY
	jnz	short _KIO_EXIT		; RETURN IF SO
	mov	al, KB_TYPA_RD		; COMMAND FOR TYPAMATIC RATE/DELAY		
	call	SND_DATA		; SEND TO KEYBOARD	
	;mov	cx, 5			; SHIFT COUNT
	;shl	bh, cl			; SHIFT DELAY OVER
	shl	bh, 5
	mov	al, bl			; PUT IN RATE
	or	al, bh			; AND DELAY
	call	SND_DATA		; SEND TO KEYBOARD	
        jmp     _KIO_EXIT               ; RETURN TO CALLER

	;-----	WRITE TO KEYBOARD BUFFER
_K500:
	push	esi			; SAVE SI (esi)
	cli				; 
     	mov	ebx, [BUFFER_TAIL]	; GET THE 'IN TO' POINTER TO THE BUFFER
	mov	esi, ebx		; SAVE A COPY IN CASE BUFFER NOT FULL
	call	_K4			; BUMP THE POINTER TO SEE IF BUFFER IS FULL
	cmp	ebx, [BUFFER_HEAD]	; WILL THE BUFFER OVERRUN IF WE STORE THIS?
	je	short _K502		; YES - INFORM CALLER OF ERROR		
	mov	[esi], cx		; NO - PUT ASCII/SCAN CODE INTO BUFFER	
	mov	[BUFFER_TAIL], ebx	; ADJUST 'IN TO' POINTER TO REFLECT CHANGE
	sub	al, al			; TELL CALLER THAT OPERATION WAS SUCCESSFUL
	jmp	short _K504		; SUB INSTRUCTION ALSO RESETS CARRY FLAG
_K502:
	mov	al, 01h			; BUFFER FULL INDICATION
_K504:
	sti				
	pop	esi			; RECOVER SI (esi)
        jmp     _KIO_EXIT               ; RETURN TO CALLER WITH STATUS IN AL

	;-----	READ THE KEY TO FIGURE OUT WHAT TO DO -----
_K1S:
	cli	; 03/12/2014
        mov     ebx, [BUFFER_HEAD] 	; GET POINTER TO HEAD OF BUFFER
        cmp     ebx, [BUFFER_TAIL] 	; TEST END OF BUFFER
	;jne	short _K1U		; IF ANYTHING IN BUFFER SKIP INTERRUPT
	jne	short _k1x ; 03/12/2014
	;
	; 03/12/2014
	; 28/08/2014
	; PERFORM OTHER FUNCTION ?? here !
	;; MOV	AX, 9002h		; MOVE IN WAIT CODE & TYPE
	;; INT 	15H			; PERFORM OTHER FUNCTION
_K1T:                                   ; ASCII READ
	sti				; INTERRUPTS BACK ON DURING LOOP
	nop				; ALLOW AN INTERRUPT TO OCCUR
_K1U:	
	cli				; INTERRUPTS BACK OFF
        mov    	ebx, [BUFFER_HEAD] 	; GET POINTER TO HEAD OF BUFFER
        cmp     ebx, [BUFFER_TAIL] 	; TEST END OF BUFFER
_k1x:
	push	ebx			; SAVE ADDRESS		
	pushf				; SAVE FLAGS
	call	MAKE_LED		; GO GET MODE INDICATOR DATA BYTE
	mov	bl, [KB_FLAG_2] 	; GET PREVIOUS BITS
	xor	bl, al			; SEE IF ANY DIFFERENT
	and	bl, 07h	; KB_LEDS	; ISOLATE INDICATOR BITS
	jz	short _K1V		; IF NO CHANGE BYPASS UPDATE
	call	SND_LED1
	cli				; DISABLE INTERRUPTS
_K1V:
	popf				; RESTORE FLAGS
	pop	ebx			; RESTORE ADDRESS
        je      short _K1T              ; LOOP UNTIL SOMETHING IN BUFFER
	;
	mov	ax, [ebx] 		; GET SCAN CODE AND ASCII CODE
        call    _K4                     ; MOVE POINTER TO NEXT POSITION
        mov     [BUFFER_HEAD], ebx      ; STORE VALUE IN VARIABLE
	retn				; RETURN

	;-----	READ THE KEY TO SEE IF ONE IS PRESENT -----
_K2S:
	cli				; INTERRUPTS OFF
        mov     ebx, [BUFFER_HEAD]      ; GET HEAD POINTER
        cmp     ebx, [BUFFER_TAIL]      ; IF EQUAL (Z=1) THEN NOTHING THERE
	mov	ax, [ebx]
	pushf				; SAVE FLAGS
	;push	ax			; SAVE CODE
	; 12/04/2021
	push	eax
	call	MAKE_LED		; GO GET MODE INDICATOR DATA BYTE
	mov	bl, [KB_FLAG_2] 	; GET PREVIOUS BITS
	xor	bl, al			; SEE IF ANY DIFFERENT
	and	bl, 07h ; KB_LEDS	; ISOLATE INDICATOR BITS
	jz	short _K2T		; IF NO CHANGE BYPASS UPDATE
	call	SND_LED			; GO TURN ON MODE INDICATORS
_K2T:
	;pop	ax			; RESTORE CODE
	; 12/04/2021
	pop	eax
	popf				; RESTORE FLAGS
	sti				; INTERRUPTS BACK ON
	retn				; RETURN

	;-----	ROUTINE TO TRANSLATE SCAN CODE PAIRS FOR EXTENDED CALLS -----
_KIO_E_XLAT:
	cmp	al, 0F0h		; IS IT ONE OF THE FILL-INs?
	jne	short _KIO_E_RET	; NO, PASS IT ON
        or 	ah, ah			; AH = 0 IS SPECIAL CASE
        jz	short _KIO_E_RET        ; PASS THIS ON UNCHANGED
	xor	al, al			; OTHERWISE SET AL = 0
_KIO_E_RET:				
	retn				; GO BACK

	;-----	ROUTINE TO TRANSLATE SCAN CODE PAIRS FOR STANDARD CALLS -----
_KIO_S_XLAT:
	cmp	ah, 0E0h		; IS IT KEYPAD ENTER OR / ?
	jne	short _KIO_S2		; NO, CONTINUE
	cmp	al, 0Dh			; KEYPAD ENTER CODE?
        je	short _KIO_S1		; YES, MASSAGE A BIT
	cmp	al, 0Ah			; CTRL KEYPAD ENTER CODE?
        je	short _KIO_S1		; YES, MASSAGE THE SAME
	mov	ah, 35h			; NO, MUST BE KEYPAD /
_kio_ret: ; 03/12/2014
	clc
	retn
	;jmp	short _KIO_USE		; GIVE TO CALLER
_KIO_S1:				
	mov	ah, 1Ch			; CONVERT TO COMPATIBLE OUTPUT
	;jmp	short _KIO_USE		; GIVE TO CALLER
	retn
_KIO_S2:		
	cmp	ah, 84h			; IS IT ONE OF EXTENDED ONES?
	ja	short _KIO_DIS		; YES, THROW AWAY AND GET ANOTHER CHAR
	cmp	al, 0F0h		; IS IT ONE OF THE FILL-INs?
        jne	short _KIO_S3		; NO, TRY LAST TEST
	or	ah, ah			; AH = 0 IS SPECIAL CASE
        jz	short _KIO_USE		; PASS THIS ON UNCHANGED
	jmp	short _KIO_DIS		; THROW AWAY THE REST
_KIO_S3:
	cmp	al, 0E0h		; IS IT AN EXTENSION OF A PREVIOUS ONE?
	;jne	short _KIO_USE		; NO, MUST BE A STANDARD CODE
	jne	short _kio_ret
	or	ah, ah			; AH = 0 IS SPECIAL CASE
        jz	short _KIO_USE		; JUMP IF AH = 0
	xor	al, al			; CONVERT TO COMPATIBLE OUTPUT
	;jmp	short _KIO_USE		; PASS IT ON TO CALLER
_KIO_USE:
	;clc				; CLEAR CARRY TO INDICATE GOOD CODE
	retn				; RETURN	
_KIO_DIS:
	stc				; SET CARRY TO INDICATE DISCARD CODE
	retn				; RETURN

	;-----	INCREMENT BUFFER POINTER ROUTINE -----
_K4:    
	inc     ebx
	inc	ebx			; MOVE TO NEXT WORD IN LIST
        cmp     ebx, [BUFFER_END] 	; AT END OF BUFFER?
        ;jne    short _K5               ; NO, CONTINUE
	jb	short _K5
        mov     ebx, [BUFFER_START]     ; YES, RESET TO BUFFER BEGINNING
_K5:
	retn

; 20/02/2015
; 05/12/2014
; 26/08/2014
; KEYBOARD (HARDWARE) INTERRUPT -  IRQ LEVEL 1
; (INT_09h - Retro UNIX 8086 v1 - U9.ASM, 07/03/2014)
;
; Derived from "KB_INT_1" procedure of IBM "pc-at" 
; rombios source code (06/10/1985)
; 'keybd.asm', HARDWARE INT 09h - (IRQ Level 1)

; EQUATES (IBM PC-XT-286 BIOS, 1986, 'POSQEQU.INC')

;--------- 8042 COMMANDS -------------------------------------------------------
ENA_KBD		equ	0AEh		; ENABLE KEYBOARD COMMAND
DIS_KBD		equ	0ADh		; DISABLE KEYBOARD COMMAND
SHUT_CMD	equ	0FEh		; CAUSE A SHUTDOWN COMMAND
;--------- 8042 KEYBOARD INTERFACE AND DIAGNOSTIC CONTROL REGISTERS ------------
STATUS_PORT	equ	064h		; 8042 STATUS PORT
INPT_BUF_FULL	equ	00000010b 	; 1 = +INPUT BUFFER FULL
PORT_A		equ	060h		; 8042 KEYBOARD SCAN CODE/CONTROL PORT
;---------- 8042 KEYBOARD RESPONSE ---------------------------------------------
KB_ACK		equ	0FAh		; ACKNOWLEDGE PROM TRANSMISSION
KB_RESEND	equ	0FEh		; RESEND REQUEST
KB_OVER_RUN	equ	0FFh		; OVER RUN SCAN CODE
;---------- KEYBOARD/LED COMMANDS ----------------------------------------------
KB_ENABLE	equ	0F4h		; KEYBOARD ENABLE
LED_CMD		equ	0EDh		; LED WRITE COMMAND
KB_TYPA_RD	equ	0F3h		; TYPAMATIC RATE/DELAY COMMAND
;---------- KEYBOARD SCAN CODES ------------------------------------------------
NUM_KEY		equ	69		; SCAN CODE FOR	 NUMBER LOCK KEY
SCROLL_KEY	equ	70		; SCAN CODE FOR	 SCROLL LOCK KEY
ALT_KEY		equ	56		; SCAN CODE FOR	 ALTERNATE SHIFT KEY
CTL_KEY		equ	29		; SCAN CODE FOR	 CONTROL KEY
CAPS_KEY	equ	58		; SCAN CODE FOR	 SHIFT LOCK KEY
DEL_KEY		equ	83		; SCAN CODE FOR	 DELETE KEY
INS_KEY		equ	82		; SCAN CODE FOR	 INSERT KEY
LEFT_KEY	equ	42		; SCAN CODE FOR	 LEFT SHIFT
RIGHT_KEY	equ	54		; SCAN CODE FOR	 RIGHT SHIFT
SYS_KEY		equ	84		; SCAN CODE FOR	 SYSTEM KEY
;---------- ENHANCED KEYBOARD SCAN CODES ---------------------------------------
ID_1		equ	0ABh		; 1ST ID CHARACTER FOR KBX
ID_2		equ	041h		; 2ND ID CHARACTER FOR KBX
ID_2A		equ	054h		; ALTERNATE 2ND ID CHARACTER FOR KBX
F11_M		equ	87		; F11 KEY MAKE
F12_M		equ	88		; F12 KEY MAKE
MC_E0		equ	224		; GENERAL MARKER CODE
MC_E1		equ	225		; PAUSE KEY MARKER CODE
;---------- FLAG EQUATES WITHIN @KB_FLAG----------------------------------------
RIGHT_SHIFT	equ	00000001b	; RIGHT SHIFT KEY DEPRESSED
LEFT_SHIFT	equ	00000010b	; LEFT SHIFT KEY DEPRESSED
CTL_SHIFT	equ	00000100b	; CONTROL SHIFT KEY DEPRESSED
ALT_SHIFT	equ	00001000b	; ALTERNATE SHIFT KEY DEPRESSED
SCROLL_STATE	equ	00010000b	; SCROLL LOCK STATE IS ACTIVE
NUM_STATE	equ	00100000b	; NUM LOCK STATE IS ACTIVE
CAPS_STATE	equ	01000000b	; CAPS LOCK STATE IS ACTIVE
INS_STATE	equ	10000000b	; INSERT STATE IS ACTIVE
;---------- FLAG EQUATES WITHIN	@KB_FLAG_1 -------------------------------------
L_CTL_SHIFT	equ	00000001b	; LEFT CTL KEY DOWN
L_ALT_SHIFT	equ	00000010b	; LEFT ALT KEY DOWN
SYS_SHIFT	equ	00000100b	; SYSTEM KEY DEPRESSED AND HELD
HOLD_STATE	equ	00001000b	; SUSPEND KEY HAS BEEN TOGGLED
SCROLL_SHIFT	equ	00010000b	; SCROLL LOCK KEY IS DEPRESSED
NUM_SHIFT	equ	00100000b	; NUM LOCK KEY IS DEPRESSED
CAPS_SHIFT	equ	01000000b	; CAPS LOCK KEY IS DEPRE55ED
INS_SHIFT	equ	10000000b	; INSERT KEY IS DEPRESSED
;---------- FLAGS EQUATES WITHIN @KB_FLAG_2 -----------------------------------
KB_LEDS		equ	00000111b	; KEYBOARD LED STATE BITS
;		equ	00000001b	; SCROLL LOCK INDICATOR
;		equ	00000010b	; NUM LOCK INDICATOR
;		equ	00000100b	; CAPS LOCK INDICATOR
;		equ	00001000b	; RESERVED (MUST BE ZERO)
KB_FA		equ	00010000b	; ACKNOWLEDGMENT RECEIVED
KB_FE		equ	00100000b	; RESEND RECEIVED FLAG
KB_PR_LED	equ	01000000b	; MODE INDICATOR UPDATE
KB_ERR		equ	10000000b	; KEYBOARD TRANSMIT ERROR FLAG
;----------- FLAGS EQUATES WITHIN @KB_FLAG_3 -----------------------------------
LC_E1		equ	00000001b	; LAST CODE WAS THE E1 HIDDEN CODE
LC_E0		equ	00000010b	; LAST CODE WAS THE E0 HIDDEN CODE
R_CTL_SHIFT	equ	00000100b	; RIGHT CTL KEY DOWN
R_ALT_SHIFT	equ	00001000b	; RIGHT ALT KEY DOWN
GRAPH_ON	equ	00001000b	; ALT GRAPHICS KEY DOWN (WT ONLY)	
KBX		equ	00010000b	; ENHANCED KEYBOARD INSTALLED
SET_NUM_LK	equ	00100000b	; FORCE NUM LOCK IF READ ID AND KBX
LC_AB		equ	01000000b	; LAST CHARACTER WAS FIRST ID CHARACTER
RD_ID		equ	10000000b	; DOING A READ ID (MUST BE BIT0)
;
;----------- INTERRUPT EQUATES -------------------------------------------------
EOI		equ	020h		; END OF INTERRUPT COMMAND TO 8259
INTA00		equ	020h		; 8259 PORT


kb_int:

; 24/07/2022 - TRDOS 386 v2.0.5
; 12/04/2021 - TRDOS 386 v2.0.3 (32 bit push/pop)
; 17/10/2015 ('ctrlbrk') 
; 05/12/2014
; 04/12/2014 (derived from pc-xt-286 bios source code -1986-)
; 26/08/2014
;
; 03/06/86  KEYBOARD BIOS
;
;--- HARDWARE INT 09H -- (IRQ LEVEL 1) ------------------------------------------
;										;
;	KEYBOARD INTERRUPT ROUTINE						;
;										;
;--------------------------------------------------------------------------------

KB_INT_1:
	sti				; ENABLE INTERRUPTS
	;push	ebp
	push	eax
	push	ebx
	push	ecx
	push	edx
	push	esi
	push	edi
	push	ds
	push	es
	cld				; FORWARD DIRECTION
	mov	ax, KDATA
	mov	ds, ax
	mov	es, ax
	;
	;-----	WAIT FOR KEYBOARD DISABLE COMMAND TO BE ACCEPTED
	mov	al, DIS_KBD		; DISABLE THE KEYBOARD COMMAND
	call	SHIP_IT			; EXECUTE DISABLE
	cli				; DISABLE INTERRUPTS
	mov	ecx, 10000h		; SET MAXIMUM TIMEOUT
KB_INT_01:
	in	al, STATUS_PORT		; READ ADAPTER STATUS
	test	al, INPT_BUF_FULL	; CHECK INPUT BUFFER FULL STATUS BIT
	loopnz	KB_INT_01		; WAIT FOR COMMAND TO BE ACCEPTED
	;
	;-----	READ CHARACTER FROM KEYBOARD INTERFACE
	in	al, PORT_A		; READ IN THE CHARACTER
	;
	;-----	SYSTEM HOOK INT 15H - FUNCTION 4FH (ON HARDWARE INT LEVEL 9H) 	
	;mov	ah, 04Fh		; SYSTEM INTERCEPT - KEY CODE FUNCTION
	;stc				; SET CY=1 (IN CASE OF IRET)
	;int	15h			; CASETTE CALL (AL)=KEY SCAN CODE
	;				; RETURNS CY=1 FOR INVALID FUNCTION
	;jc	KB_INT_02		; CONTINUE IF CARRY FLAG SET ((AL)=CODE)
	;jmp	K26			; EXIT IF SYSTEM HANDLES SCAN CODE
	;				; EXÝT HANDLES HARDWARE EOI AND ENABLE		
	;
	;-----	CHECK FOR A RESEND COMMAND TO KEYBOARD
KB_INT_02:				; 	  (AL)= SCAN CODE
	sti				; ENABLE INTERRUPTS AGAIN
	cmp	al, KB_RESEND		; IS THE INPUT A RESEND
        je      short KB_INT_4          ; GO IF RESEND
	;
	;-----	CHECK FOR RESPONSE TO A COMMAND TO KEYBOARD
	cmp	al, KB_ACK		; IS THE INPUT AN ACKNOWLEDGE
        jne     short KB_INT_2          ; GO IF NOT
	;
	;-----	A COMMAND TO THE KEYBOARD WAS ISSUED
	cli				; DISABLE INTERRUPTS
	or	byte [KB_FLAG_2], KB_FA ; INDICATE ACK RECEIVED
        ;jmp	K26                     ; RETURN IF NOT ACK RETURNED FOR DATA)
	; 12/04/2021
	jmp	short ID_EX  ; K26
	;
	;-----	RESEND THE LAST BYTE
KB_INT_4:
	cli				; DISABLE INTERRUPTS
	or	byte [KB_FLAG_2], KB_FE ; INDICATE RESEND RECEIVED
        ;jmp	K26                     ; RETURN IF NOT ACK RETURNED FOR DATA)
	; 12/04/2021
	jmp	short ID_EX  ; K26
	;
;-----	UPDATE MODE INDICATORS IF CHANGE IN STATE
KB_INT_2:
	;push 	ax			; SAVE DATA IN
	; 12/04/2021
	push	eax
	call	MAKE_LED		; GO GET MODE INDICATOR DATA BYTE
	mov	bl, [KB_FLAG_2] 	; GET PREVIOUS BITS
	xor	bl, al			; SEE IF ANY DIFFERENT
	and	bl, KB_LEDS		; ISOLATE INDICATOR BITS
	jz	short UP0		; IF NO CHANGE BYPASS UPDATE
	call	SND_LED			; GO TURN ON MODE INDICATORS
UP0:
	;pop	ax			; RESTORE DATA IN
	; 12/04/2021
	pop	eax
;------------------------------------------------------------------------
;	START OF KEY PROCESSING						;
;------------------------------------------------------------------------
	mov	ah, al			; SAVE SCAN CODE IN AH ALSO
	;
	;-----	TEST FOR OVERRUN SCAN CODE FROM KEYBOARD
	cmp	al, KB_OVER_RUN		; IS THIS AN OVERRUN CHAR
        ;je	K62			; BUFFER_FULL_BEEP
	; 12/04/2021
	jne	short K16
	jmp	K62
K16:	
	mov	bh, [KB_FLAG_3]		; LOAD FLAGS FOR TESTING
	;
	;-----	TEST TO SEE IF A READ_ID IS IN PROGRESS
	test 	bh, RD_ID+LC_AB 	; ARE WE DOING A READ ID?
	jz	short NOT_ID		; CONTINUE IF NOT
	jns	short TST_ID_2		; IS THE RD_ID FLAG ON?
	cmp	al, ID_1		; IS THIS THE 1ST ID CHARACTER?
	jne	short RST_RD_ID
	or	byte [KB_FLAG_3], LC_AB ; INDICATE 1ST ID WAS OK
RST_RD_ID:
	and	byte [KB_FLAG_3], ~RD_ID ; RESET THE READ ID FLAG
        jmp	short ID_EX		; AND EXIT
	; 12/04/2021
	;jmp	K26
	;
TST_ID_2:
	and	byte [KB_FLAG_3], ~LC_AB ; RESET FLAG
	cmp	al, ID_2A		; IS THIS THE 2ND ID CHARACTER?
        je	short KX_BIT		; JUMP IF SO
	cmp	al, ID_2		; IS THIS THE 2ND ID CHARACTER?
        jne	short ID_EX		; LEAVE IF NOT
	; 12/04/2021
	;jne	K26
	;
	;-----	A READ ID SAID THAT IT WAS ENHANCED KEYBOARD
	test	bh, SET_NUM_LK 		; SHOULD WE SET NUM LOCK?
        jz      short KX_BIT		; EXIT IF NOT
	or	byte [KB_FLAG], NUM_STATE ; FORCE NUM LOCK ON
	call	SND_LED			; GO SET THE NUM LOCK INDICATOR
KX_BIT:
	or	byte [KB_FLAG_3], KBX	; INDICATE ENHANCED KEYBOARD WAS FOUND
ID_EX:	jmp     K26			; EXIT
	;
NOT_ID:
	cmp	al, MC_E0		; IS THIS THE GENERAL MARKER CODE?
	jne	short TEST_E1
	or	byte [KB_FLAG_3], LC_E0+KBX ; SET FLAG BIT, SET KBX, AND
	jmp	short EXIT		; THROW AWAY THIS CODE
	; 12/04/2021
	;jmp	K26A	
TEST_E1:	
	cmp	al, MC_E1		; IS THIS THE PAUSE KEY?
	jne	short NOT_HC
	or	byte [KB_FLAG_3], LC_E1+KBX ; SET FLAG BIT, SET KBX, AND
EXIT:	jmp	K26A			; THROW AWAY THIS CODE
	;
NOT_HC:
	and	al, 07Fh		; TURN OFF THE BREAK BIT
	test	bh, LC_E0		; LAST CODE THE E0 MARKER CODE
	jz	short NOT_LC_E0		; JUMP IF NOT
	;
	mov	edi, _K6+6		; IS THIS A SHIFT KEY?
	scasb
	;je	K26 ; K16B              ; YES, THROW AWAY & RESET FLAG
	; 12/04/2021
	je	short K16B ; K26
	scasb
	jne	short K16A		; NO, CONTINUE KEY PROCESSING
	;jmp	short K16B		; YES, THROW AWAY & RESET FLAG
	jmp	K26
	;
NOT_LC_E0:
	test	bh, LC_E1		; LAST CODE THE E1 MARKER CODE?
	jz	short T_SYS_KEY		; JUMP IF NOT
	mov	ecx, 4			; LENGHT OF SEARCH
	mov	edi, _K6+4		; IS THIS AN ALT, CTL, OR SHIFT?
	repne	scasb			; CHECK IT
	je	short EXIT		; THROW AWAY IF SO
	; 12/04/2021
	;je	K26A			
	;
	cmp	al, NUM_KEY		; IS IT THE PAUSE KEY?
	jne	short K16B		; NO, THROW AWAY & RESET FLAG
	; 12/04/2021
	;jne	K26
	test	ah, 80h			; YES, IS IT THE BREAK OF THE KEY?
	jnz	short K16B		; YES, THROW THIS AWAY, TOO	
	; 24/07/2022
	;jnz	K26
        ; 20/02/2015 
	test	byte [KB_FLAG_1],HOLD_STATE ; NO, ARE WE PAUSED ALREADY?
	jnz	short K16B		; YES, THROW AWAY
	; 12/04/2021
	;jnz	K26
	jmp     K39P                    ; NO, THIS IS THE REAL PAUSE STATE
	;
	;-----	TEST FOR SYSTEM KEY
T_SYS_KEY:
	cmp	al, SYS_KEY		; IS IT THE SYSTEM KEY?
	jnz	short K16A		; CONTINUE IF NOT
	;
	test	ah, 80h			; CHECK IF THIS A BREAK CODE
	jnz	short K16C		; DO NOT TOUCH SYSTEM INDICATOR IF TRUE
	;
	test	byte [KB_FLAG_1], SYS_SHIFT ; SEE IF IN SYSTEM KEY HELD DOWN 
	jnz	short K16B		; IF YES, DO NOT PROCESS SYSTEM INDICATOR	
	; 12/04/2021
	;jnz	K26		
	;
	or	byte [KB_FLAG_1], SYS_SHIFT ; INDICATE SYSTEM KEY DEPRESSED
	mov	al, EOI			; END OF INTERRUPT COMMAND
	out	20h, al ;out INTA00, al	; SEND COMMAND TO INTERRUPT CONTROL PORT
					; INTERRUPT-RETURN-NO-EOI
	mov	al, ENA_KBD		; INSURE KEYBOARD IS ENABLED
	call	SHIP_IT			; EXECUTE ENABLE
	; !!! SYSREQ !!! function/system call (INTERRUPT) must be here !!!
	;MOV	AL, 8500H		; FUNCTION VALUE FOR MAKE OF SYSTEM KEY
	;STI				; MAKE SURE INTERRUPTS ENABLED
	;INT	15H			; USER INTERRUPT	
        jmp     K27A                    ; END PROCESSING
	;
K16B:	jmp	K26			; IGNORE SYSTEM KEY
	;
K16C:
	and	byte [KB_FLAG_1], ~SYS_SHIFT ; TURN OFF SHIFT KEY HELD DOWN
	mov	al, EOI			; END OF INTERRUPT COMMAND
	out	20h, al ;out INTA00, al ; SEND COMMAND TO INTERRUPT CONTROL PORT
					; INTERRUPT-RETURN-NO-EOI
	;MOV	AL, ENA_KBD		; INSURE KEYBOARD IS ENABLED
	;CALL	SHIP_IT			; EXECUTE ENABLE
	;
	;MOV	AX, 8501H		; FUNCTION VALUE FOR BREAK OF SYSTEM KEY
	;STI				; MAKE SURE INTERRUPTS ENABLED
	;INT	15H			; USER INTERRUPT
	;JMP	K27A			; INGONRE SYSTEM KEY				
	;
	jmp     K27			; IGNORE SYSTEM KEY
	;
	;-----	TEST FOR SHIFT KEYS
K16A:
	mov	bl, [KB_FLAG]		; PUT STATE FLAGS IN BL
	mov	edi, _K6		; SHIFT KEY TABLE offset
	mov	ecx, _K6L		; LENGTH
	repne	scasb			; LOOK THROUGH THE TABLE FOR A MATCH
	mov	al, ah			; RECOVER SCAN CODE
        ;jne	K25                     ; IF NO MATCH, THEN SHIFT NOT FOUND
	; 12/04/2021
	je	short K17
	jmp	K25
	;
	;------	SHIFT KEY FOUND
K17:
        sub     edi, _K6+1              ; ADJUST PTR TO SCAN CODE MATCH
       	mov     ah, [edi+_K7]       	; GET MASK INTO AH
	mov	cl, 2			; SETUP COUNT FOR FLAG SHIFTS
	test	al, 80h			; TEST FOR BREAK KEY
	;jnz	short K23		; JUMP OF BREAK
	; 12/04/2021
	jz	short K17C
	jmp	K23
	;
	;-----	SHIFT MAKE FOUND, DETERMINE SET OR TOGGLE
K17C:
	cmp	ah, SCROLL_SHIFT
	jae	short K18		; IF SCROLL SHIFT OR ABOVE, TOGGLE KEY
	;
	;-----	PLAIN SHIFT KEY, SET SHIFT ON
	or	[KB_FLAG], ah		; TURN ON SHIFT BIT
        test	al, CTL_SHIFT+ALT_SHIFT ; IS IT ALT OR CTRL?
	;;jnz	short K17D		; YES, MORE FLAGS TO SET
	;jz	K26			; NO, INTERRUPT RETURN
	; 12/04/2021
	jz	short k17f
K17D:
	test	bh, LC_E0		; IS THIS ONE OF NEW KEYS?
	jz 	short K17E		; NO, JUMP
	or	[KB_FLAG_3], ah		; SET BITS FOR RIGHT CTRL, ALT
	;jmp	K26			; INTERRUPT RETURN
	; 12/04/2021
	jmp	short k17f
K17E:
	shr	ah, cl			; MOVE FLAG BITS TWO POSITIONS
	or	[KB_FLAG_1], ah		; SET BITS FOR LEFT CTRL, ALT
k17f:	; 12/04/2021
	jmp	K26
	;
	;-----	TOGGLED SHIFT KEY, TEST FOR 1ST MAKE OR NOT
K18:					; SHIFT-TOGGLE
	test	bl, CTL_SHIFT 		; CHECK CTL SHIFT STATE
	jz	short K18A              ; JUMP IF NOT CTL STATE
        ;jnz	K25                     ; JUMP IF CTL STATE
	; 12/04/2021
	jmp	short k20a ; K25
K18A:
	cmp	al, INS_KEY		; CHECK FOR INSERT KEY
	jne	short K22		; JUMP IF NOT INSERT KEY
	test	bl, ALT_SHIFT 		; CHECK FOR ALTERNATE SHIFT
      	jz	short K18B		; JUMP IF NOT ALTERNATE SHIFT	
        ;jnz	K25			; JUMP IF ALTERNATE SHIFT
	; 12/04/2021
	jmp	short k20a ; K25
K18B:
	test	bh, LC_E0 ;20/02/2015	; IS THIS NEW INSERT KEY?
	jnz	short K22		; YES, THIS ONE'S NEVER A '0'
K19:	
	test	bl, NUM_STATE 		; CHECK FOR BASE STATE
	jnz	short K21		; JUMP IF NUM LOCK IS ON
	test	bl, LEFT_SHIFT+RIGHT_SHIFT ; TEST FOR SHIFT STATE
	jz	short K22		; JUMP IF BASE STATE
K20:					; NUMERIC ZERO, NOT INSERT KEY
	mov	ah, al			; PUT SCAN CODE BACK IN AH
k20a:	; 12/04/2021
        jmp     K25                     ; NUMERAL '0', STNDRD. PROCESSING
K21:					; MIGHT BE NUMERIC
	test	bl, LEFT_SHIFT+RIGHT_SHIFT
	jz	short K20		; IS NUMERIC, STD. PROC.
	;
K22:					; SHIFT TOGGLE KEY HIT; PROCESS IT
	test	ah, [KB_FLAG_1] 	; IS KEY ALREADY DEPRESSED
        ;jnz	short K26		; JUMP IF KEY ALREADY DEPRESSED
	; 12/04/2021
	jnz	short k17f ; K26
K22A:
        or      [KB_FLAG_1], ah 	; INDICATE THAT THE KEY IS DEPRESSED
	xor	[KB_FLAG], ah		; TOGGLE THE SHIFT STATE
	;
	;-----	TOGGLE LED IF CAPS, NUM  OR SCROLL KEY DEPRESSED
	test	ah, CAPS_SHIFT+NUM_SHIFT+SCROLL_SHIFT ; SHIFT TOGGLE?
	jz	short K22B		; GO IF NOT
	;
	; 12/04/2021 (32 bit push/pop)
	push	eax ; push ax		; SAVE SCAN CODE AND SHIFT MASK
	call	SND_LED			; GO TURN MODE INDICATORS ON
	pop	eax ; pop ax		; RESTORE SCAN CODE
K22B:
	cmp	al, INS_KEY		; TEST FOR 1ST MAKE OF INSERT KEY
        ;jne	short K26		; JUMP IF NOT INSERT KEY
	; 12/04/2021
	jne	short k17f ; K26
	mov	ah, al		        ; SCAN CODE IN BOTH HALVES OF AX
        jmp	K28                     ; FLAGS UPDATED, PROC. FOR BUFFER
	;
	;-----	BREAK SHIFT FOUND
K23:					; BREAK-SHIFT-FOUND
	cmp	ah, SCROLL_SHIFT	; IS THIS A TOGGLE KEY
	not	ah			; INVERT MASK
	jae	short K24		; YES, HANDLE BREAK TOGGLE
	and	[KB_FLAG], ah		; TURN OFF SHIFT BIT
	cmp	ah, ~CTL_SHIFT		; IS THIS ALT OR CTL?
	ja	short K23D		; NO, ALL DONE
	;
	test	bh, LC_E0		; 2ND ALT OR CTL?
	jz	short K23A		; NO, HANSLE NORMALLY
	and 	[KB_FLAG_3], ah		; RESET BIT FOR RIGHT ALT OR CTL
	jmp	short K23B		; CONTINUE
K23A:
	sar	ah, cl			; MOVE THE MASK BIT TWO POSITIONS
	and	[KB_FLAG_1], ah		; RESET BIT FOR LEFT ALT AND CTL
K23B:
	mov	ah, al			; SAVE SCAN CODE
	mov	al, [KB_FLAG_3]		; GET RIGHT ALT & CTRL FLAGS
	shr	al, cl			; MOVE TO BITS 1 & 0
	or	al, [KB_FLAG_1]		; PUT IN LEFT ALÞT & CTL FLAGS
	shl	al, cl			; MOVE BACK TO BITS 3 & 2
	and	al, ALT_SHIFT+CTL_SHIFT ; FILTER OUT OTHER GARBAGE
	or	[KB_FLAG], al		; PUT RESULT IN THE REAL FLAGS	
	mov	al, ah
K23D:
	cmp	al, ALT_KEY+80h		; IS THIS ALTERNATE SHIFT RELEASE
	jne	short K26		; INTERRUPT RETURN
	;	
	;-----	ALTERNATE SHIFT KEY RELEASED, GET THE VALUE INTO BUFFER
	mov	al, [ALT_INPUT]
	mov	ah, 0			; SCAN CODE OF 0
	mov	[ALT_INPUT], ah 	; ZERO OUT THE FIELD
	cmp	al, 0			; WAS THE INPUT = 0?
	je	short K26		; INTERRUPT_RETURN
        ; 29/01/2016
	;jmp	K61			; IT WASN'T, SO PUT IN BUFFER
	jmp	_K60
	;
K24:					; BREAK-TOGGLE
	and	[KB_FLAG_1], ah 	; INDICATE NO LONGER DEPRESSED
	jmp	short K26		; INTERRUPT_RETURN
	;
	;-----	TEST FOR HOLD STATE
					; AL, AH = SCAN CODE
K25:					; NO-SHIFT-FOUND
	cmp	al, 80h			; TEST FOR BREAK KEY
	jae	short K26		; NOTHING FOR BREAK CHARS FROM HERE ON
	test	byte [KB_FLAG_1], HOLD_STATE ; ARE WE IN HOLD STATE
	jz	short K28		; BRANCH AROUND TEST IF NOT
	cmp	al, NUM_KEY
	je	short K26		; CAN'T END HOLD ON NUM_LOCK
	and	byte [KB_FLAG_1], ~HOLD_STATE ; TURN OFF THE HOLD STATE BIT
K26:
	and	byte [KB_FLAG_3], ~(LC_E0+LC_E1) ; RESET LAST CHAR H.C. FLAG
K26A:					; INTERRUPT-RETURN
	cli				; TURN OFF INTERRUPTS
	mov	al, EOI			; END OF INTERRUPT COMMAND
	out	20h, al	;out INTA00, al	; SEND COMMAND TO INTERRUPT CONTROL PORT
K27:					; INTERRUPT-RETURN-NO-EOI
	mov	al, ENA_KBD		; INSURE KEYBOARD IS ENABLED
	call	SHIP_IT			; EXECUTE ENABLE
K27A:
	cli				; DISABLE INTERRUPTS
	;;mov	byte [intflg], 0 ; 07/01/2017 ;; 15/01/2017
	pop	es			; RESTORE REGISTERS
	pop	ds
	pop	edi
	pop	esi
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax
	;pop	ebp
	iretd				; RETURN

	;-----	NOT IN	HOLD STATE
K28:					; NO-HOLD-STATE
	cmp	al, 88			; TEST FOR OUT-OF-RANGE SCAN CODES
	ja	short K26		; IGNORE IF OUT-OF-RANGE	
	;
	test	bl, ALT_SHIFT 		; ARE WE IN ALTERNATE SHIFT
        jz	short K28A		; IF NOT ALTERNATE
        ; 12/04/2021
	;jz	K38
	;
	test	bh, KBX			; IS THIS THE ENCHANCED KEYBOARD?
	jz	short K29		; NO, ALT STATE IS REAL
	 ;28/02/2015
	test	byte [KB_FLAG_1], SYS_SHIFT ; YES, IS SYSREQ KEY DOWN?
	jz	short K29		;  NO, ALT STATE IS REAL
	; 12/04/2021
	;jnz	K38			; YES, THIS IS PHONY ALT STATE 
        ;				; DUE TO PRESSING SYSREQ	
K28A:	jmp	K38
	;
	;-----	TEST FOR RESET KEY SEQUENCE (CTL ALT DEL)
K29:					; TEST-RESET
	test	bl, CTL_SHIFT 		; ARE WE IN CONTROL SHIFT ALSO?
	jz	short K31		; NO_RESET
	cmp	al, DEL_KEY		; CTL-ALT STATE, TEST FOR DELETE KEY
	jne	short K31		; NO_RESET, IGNORE
	;
	;-----	CTL-ALT-DEL HAS BEEN FOUND
 	; 26/08/2014
cpu_reset:
	; IBM PC/AT ROM BIOS source code - 10/06/85 (TEST4.ASM - PROC_SHUTDOWN)
	; Send FEh (system reset command) to the keyboard controller.
	mov	al, SHUT_CMD		; SHUTDOWN COMMAND
	out	STATUS_PORT, al		; SEND TO KEYBOARD CONTROL PORT
khere:
	hlt				; WAIT FOR 80286 RESET
	jmp 	short khere		; INSURE HALT
	;
	;-----	IN ALTERNATE SHIFT, RESET NOT FOUND
K31:					; NO-RESET
	cmp	al, 57			; TEST FOR SPACE KEY
	jne	short K311		; NOT THERE
	mov	al, ' '			; SET SPACE CHAR
k31a:	; 12/04/2021
        jmp     K57                     ; BUFFER_FILL
K311:
	cmp	al, 15			; TEST FOR TAB KEY
	jne	short K312		; NOT THERE
	mov	ax, 0A500h		; SET SPECIAL CODE FOR ALT-TAB
        ;jmp	K57			; BUFFER_FILL
	; 12/04/2021
	jmp	short k31a
K312:
	cmp	al, 74			; TEST FOR KEY PAD -
        ;je	short K37B		; GO PROCESS
	; 12/04/2021
	je	short k312a
	cmp	al, 78			; TEST FOR KEY PAD +
        ;je	short K37B		; GO PROCESS
	; 12/04/2021
	jne	short K32
k312a:
	jmp	K37B	
	;
	;-----	LOOK FOR KEY PAD ENTRY
K32:					; ALT-KEY-PAD
	mov	edi, K30		; ALT-INPUT-TABLE offset
	mov	ecx, 10			; LOOK FOR ENTRY USING KEYPAD
	repne	scasb			; LOOK FOR MATCH
	jne	short K33		; NO_ALT_KEYPAD
	test	bh, LC_E0		; IS THIS ONE OF THE NEW KEYS?
        jnz	short K37C		; YES, JUMP, NOT NUMPAD KEY
	sub	edi, K30+1		; DI NOW HAS ENTRY VALUE
	mov	al, [ALT_INPUT] 	; GET THE CURRENT BYTE
	mov	ah, 10			; MULTIPLY BY 10
	mul	ah
	add	ax, di			; ADD IN THE LATEST ENTRY
	mov	[ALT_INPUT], al 	; STORE IT AWAY
K32A:
        jmp     K26                     ; THROW AWAY THAT KEYSTROKE
	;
	;-----	LOOK FOR SUPERSHIFT ENTRY
K33:					; NO-ALT-KEYPAD
        mov     byte [ALT_INPUT], 0     ; ZERO ANY PREVIOUS ENTRY INTO INPUT
	mov	ecx, 26			; (DI),(ES) ALREADY POINTING
	repne	scasb			; LOOK FOR MATCH IN ALPHABET
	je	short K37A		; MATCH FOUND, GO FILLL THE BUFFER
	;
	;-----	LOOK FOR TOP ROW OF ALTERNATE SHIFT
K34:					; ALT-TOP-ROW
	cmp	al, 2			; KEY WITH '1' ON IT
	jb	short K37B		; MUST BE ESCAPE
	cmp	al, 13			; IS IT IN THE REGION
	ja	short K35		; NO, ALT SOMETHING ELSE
	add	ah, 118			; CONVERT PSEUDO SCAN CODE TO RANGE
	jmp	short K37A		; GO FILL THE BUFFER
	;
	;-----	TRANSLATE ALTERNATE SHIFT PSEUDO SCAN CODES
K35:					; ALT-FUNCTION
	cmp	al, F11_M		; IS IT F11?	
	jb	short K35A ; 20/02/2015	; NO, BRANCH
	cmp	al, F12_M		; IS IT F12?
	ja	short K35A ; 20/02/2015	; NO, BRANCH
	add	ah, 52			; CONVERT TO PSEUDO SCAN CODE
	jmp	short K37A		; GO FILL THE BUFFER
K35A:
	test	bh, LC_E0		; DO WE HAVE ONE OF THE NEW KEYS?
	jz	short K37		; NO, JUMP
	cmp	al, 28			; TEST FOR KEYPAD ENTER
        jne     short K35B              ; NOT THERE
	mov	ax, 0A600h		; SPECIAL CODE
	;jmp	K57			; BUFFER FILL
	; 12/04/2021
	jmp	short k35c
K35B:
	cmp	al, 83			; TEST FOR DELETE KEY
	je	short K37C		; HANDLE WITH OTHER EDIT KEYS
	cmp	al, 53			; TEST FOR KEYPAD /
	jne	short K32A		; NOT THERE, NO OTHER E0 SPECIALS
        ; 12/04/2021
	;jne	K26
	mov	ax, 0A400h		; SPECIAL CODE1
k35c:	; 12/04/2021
	jmp	K57			; BUFFER FILL
K37:
	cmp	al, 59			; TEST FOR FUNCTION KEYS (F1)
        jb      short K37B		; NO FN, HANDLE W/OTHER EXTENDED
	cmp	al, 68			; IN KEYPAD REGION?
	ja	short K32A		; IF SO, IGNORE
	; 12/04/2021
	;ja	K26
	add	ah, 45			; CONVERT TO PSEUDO SCAN CODE
K37A:
	mov	al, 0			; ASCII CODE OF ZERO
	;jmp	K57			; PUT IT IN THE BUFFER
	; 12/04/2021
	jmp	short k35c
K37B:
	mov	al, 0F0h		; USE SPECIAL ASCII CODE
	;jmp	K57			; PUT IT IN THE BUFFER
	; 12/04/2021
	jmp	short k35c
K37C:
	add	al, 80			; CONVERT SCAN CODE (EDIT KEYS)
	mov	ah, al			; (SCAN CODE NOT IN AH FOR INSERT)
	jmp     short K37A              ; PUT IT IN THE BUFFER
	;
	;-----	NOT IN ALTERNATE SHIFT
K38:					; NOT-ALT-SHIFT
					; BL STILL HAS SHIFT FLAGS
	test	bl, CTL_SHIFT 		; ARE WE IN CONTROL SHIFT?
	;;jnz	short K38A		; YES, START PROCESSING	
        ;jz	K44			; NOT-CTL-SHIFT
	; 12/04/2021
	jnz	short K38A		; YES, START PROCESSING	
	jmp	K44			; NOT-CTL-SHIFT
	;
	;-----	CONTROL SHIFT, TEST SPECIAL CHARACTERS
	;-----	TEST FOR BREAK
K38A:
	cmp	al, SCROLL_KEY		; TEST FOR BREAK
	jne	short K39		; JUMP, NO-BREAK
	test	bh, KBX			; IS THIS THE ENHANCED KEYBOARD?
	jz	short K38B		; NO, BREAK IS VALID	
	test	bh, LC_E0		; YES, WAS LAST CODE AN E0?
	jz	short K39		; NO-BREAK, TEST FOR PAUSE	
K38B:
	mov	ebx, [BUFFER_HEAD] 	; RESET BUFFER TO EMPTY
	mov	[BUFFER_TAIL], ebx
	mov	byte [BIOS_BREAK], 80h  ; TURN ON BIOS_BREAK BIT
	;
	;-----	ENABLE KEYBOARD
	mov	al, ENA_KBD		; ENABLE KEYBOARD
	call	SHIP_IT			; EXECUTE ENABLE
	;
	; CTRL+BREAK code here !!!
	;INT	1BH			; BREAK INTERRUPT VECTOR
	; 17/10/2015	
	call	ctrlbrk ; control+break subroutine
	;
	;sub	ax, ax			; PUT OUT DUMMY CHARACTER
        ; 12/04/2021
	sub	eax, eax
        jmp     K57                     ; BUFFER_FILL
	;
	;-----	TEST FOR PAUSE
K39:					; NO_BREAK
	test	bh, KBX			; IS THIS THE ENHANCED KEYBOARD?
	jnz	short K41		; YES, THEN THIS CAN'T BE PAUSE	
	cmp	al, NUM_KEY		; LOOK FOR PAUSE KEY
	jne	short K41		; NO-PAUSE
K39P:
	or	byte [KB_FLAG_1], HOLD_STATE ; TURN ON THE HOLD FLAG
	;
	;-----	ENABLE KEYBOARD
	mov	al, ENA_KBD		; ENABLE KEYBOARD
	call	SHIP_IT			; EXECUTE ENABLE
K39A:
	mov	al, EOI			; END OF INTERRUPT TO CONTROL PORT
	out	20h, al ;out INTA00, al	; ALLOW FURTHER KEYSTROKE INTERRUPTS
	;
	;-----	DURING PAUSE INTERVAL, TURN COLOR CRT BACK ON
        cmp     byte [CRT_MODE], 7      ; IS THIS BLACK AND WHITE CARD
        je      short K40              	; YES, NOTHING TO DO
	mov	dx, 03D8h		; PORT FOR COLOR CARD
        mov     al, [CRT_MODE_SET] 	; GET THE VALUE OF THE CURRENT MODE
	out	dx, al			; SET THE CRT MODE, SO THAT CRT IS ON
	;
K40:					; PAUSE-LOOP
        test    byte [KB_FLAG_1], HOLD_STATE ; CHECK HOLD STATE FLAG
	jnz	short K40		; LOOP UNTIL FLAG TURNED OFF
	;
        jmp     K27                     ; INTERRUPT_RETURN_NO_EOI
        ;
	;-----	TEST SPECIAL CASE KEY 55
K41:					; NO-PAUSE
	cmp	al, 55			; TEST FOR */PRTSC KEY
	jne	short K42		; NOT-KEY-55
	test	bh, KBX			; IS THIS THE ENHANCED KEYBOARD?
	jz	short K41A		; NO, CTL-PRTSC IS VALID	
	test	bh, LC_E0		; YES, WAS LAST CODE AN E0?
	jz	short K42B		; NO, TRANSLATE TO A FUNCTION
K41A:	
	mov	ax, 114*256		; START/STOP PRINTING SWITCH
        jmp     K57                     ; BUFFER_FILL
	;
	;-----	SET UP TO TRANSLATE CONTROL SHIFT
K42:					; NOT-KEY-55
	cmp	al, 15			; IS IT THE TAB KEY?
	je	short K42B		; YES, XLATE TO FUNCTION CODE
	cmp	al, 53			; IS IT THE / KEY?
	jne	short K42A		; NO, NO MORE SPECIAL CASES	
	test	bh, LC_E0		; YES, IS IT FROM THE KEY PAD?
	jz	short K42A		; NO, JUST TRANSLATE
	mov	ax, 9500h		; YES, SPECIAL CODE FOR THIS ONE
	jmp	K57			; BUFFER FILL	
K42A: 
	;;mov	ebx, _K8		; SET UP TO TRANSLATE CTL
	cmp	al, 59			; IS IT IN CHARACTER TABLE?
        ;jb	short K45F              ; YES, GO TRANSLATE CHAR
	;;jb	K56 ; 20/02/2015
	;;jmp	K64 ; 20/02/2015
K42B:
	mov	ebx, _K8		; SET UP TO TRANSLATE CTL
	;jb	K56 ;; 20/02/2015
	; 12/04/2021
	jb	short K45F	
	jmp	K64	
        ;
	;-----	NOT IN CONTROL SHIFT
K44:					; NOT-CTL-SHIFT
	cmp	al, 55			; PRINT SCREEN KEY?
	jne	short K45		; NOT PRINT SCREEN
	test	bh, KBX			; IS THIS ENHANCED KEYBOARD?
	jz	short K44A		; NO, TEST FOR SHIFT STATE	
	test	bh, LC_E0		; YES, LAST CODE A MARKER?
	jnz	short K44B		; YES, IS PRINT SCREEN
	jmp	short K45C		; NO, TRANSLATE TO '*' CHARACTER
K44A:
	test	bl, LEFT_SHIFT+RIGHT_SHIFT ; NOT 101 KBD, SHIFT KEY DOWN?
	jz	short K45C		; NO, TRANSLATE TO '*' CHARACTER
	;
	;-----	ISSUE INTERRUPT TO INDICATE PRINT SCREEN FUNCTION
K44B:
	mov	al, ENA_KBD		; INSURE KEYBOARD IS ENABLED
	call	SHIP_IT			; EXECUTE ENABLE
	mov	al, EOI			; END OF CURRENT INTERRUPT
	out	20h, al ;out INTA00, al	; SO FURTHER THINGS CAN HAPPEN
	; Print Screen !!!		; ISSUE PRINT SCREEN INTERRUPT (INT 05h)
	;PUSH 	BP			; SAVE POINTER
	;INT 	5H			; ISSUE PRINT SCREEN INTERRUPT
	;POP	BP			; RESTORE POINTER
        and     byte [KB_FLAG_3], ~(LC_E0+LC_E1) ; ZERO OUT THESE FLAGS
        jmp     K27                     ; GO BACK WITHOUT EOI OCCURRING
	;
	;-----	HANDLE IN-CORE KEYS
K45:					; NOT-PRINT-SCREEN
	cmp	al, 58			; TEST FOR IN-CORE AREA
	ja	short K46		; JUMP IF NOT
	cmp	al, 53			; IS THIS THE '/' KEY?
	jne	short K45A		; NO, JUMP
	test	bh, LC_E0		; WAS THE LAST CODE THE MARKER?
	jnz	short K45C		; YES, TRANSLATE TO CHARACTER
K45A:
	mov	ecx, 26			; LENGHT OF SEARCH
	mov	edi, K30+10		; POINT TO TABLE OF A-Z CHARS
	repne	scasb			; IS THIS A LETTER KEY?
		; 20/02/2015
	jne	short K45B              ; NO, SYMBOL KEY
	;
	test	bl, CAPS_STATE		; ARE WE IN CAPS_LOCK?
	jnz	short K45D		; TEST FOR SURE
K45B:
	test	bl, LEFT_SHIFT+RIGHT_SHIFT ; ARE WE IN SHIFT STATE?
	jnz	short K45E		; YES, UPPERCASE
					; NO, LOWERCASE
K45C:
	mov	ebx, K10		; TRANSLATE TO LOWERCASE LETTERS
	jmp	short K56	
K45D:					; ALMOST-CAPS-STATE
	test	bl, LEFT_SHIFT+RIGHT_SHIFT ; CL ON. IS SHIFT ON, TOO?
	jnz	short K45C		; SHIFTED TEMP OUT OF CAPS STATE
K45E:
	mov	ebx, K11		; TRANSLATE TO UPPER CASE LETTERS
K45F:	jmp	short K56
	;
	;-----	TEST FOR KEYS F1 - F10
K46:					; NOT IN-CORE AREA
	cmp	al, 68			; TEST FOR F1 - F10
	;ja	short K47		; JUMP IF NOT
	;jmp	short K53		; YES, GO DO FN KEY PROCESS			
	jna	short K53		
	;
	;-----	HANDLE THE NUMERIC PAD KEYS
K47:					; NOT F1 - F10
	cmp	al, 83			; TEST NUMPAD KEYS
	ja	short K52		; JUMP IF NOT
	;
	;-----	KEYPAD KEYS, MUST TEST NUM LOCK FOR DETERMINATION
K48:
	cmp	al, 74			; SPECIAL CASE FOR MINUS
	je	short K45E		; GO TRANSLATE
	cmp	al, 78			; SPECIAL CASE FOR PLUS
	je	short K45E		; GO TRANSLATE
	test	bh, LC_E0		; IS THIS ONE OFTHE NEW KEYS?
	jnz	short K49		; YES, TRANSLATE TO BASE STATE
	;		
	test 	bl, NUM_STATE		; ARE WE IN NUM LOCK
	jnz	short K50		; TEST FOR SURE
	test	bl, LEFT_SHIFT+RIGHT_SHIFT ; ARE WE IN SHIFT STATE?
	;jnz	short K51		; IF SHIFTED, REALLY NUM STATE
	jnz	short K45E
	;
	;-----	BASE CASE FOR KEYPAD
K49:					
	cmp	al, 76			; SPECIAL CASE FOR BASE STATE 5
	jne	short K49A		; CONTINUE IF NOT KEYPAD 5
	mov	al, 0F0h		; SPECIAL ASCII CODE	
	jmp	short K57		; BUFFER FILL
K49A:
	mov	ebx, K10		; BASE CASE TABLE	
	jmp	short K64		; CONVERT TO PSEUDO SCAN
	;
	;-----	MIGHT BE NUM LOCK, TEST SHIFT STATUS
K50:					; ALMOST-NUM-STATE
        test    bl, LEFT_SHIFT+RIGHT_SHIFT
	jnz 	short K49		; SHIFTED TEMP OUT OF NUM STATE
K51:	jmp	short K45E		; REALLY NUM STATE
	;
	;-----	TEST FOR THE NEW KEYS ON WT KEYBOARDS 
K52:					; NOT A NUMPAD KEY
	cmp	al, 86			; IS IT THE NEW WT KEY?
	;jne	short K53		; JUMP IF NOT
	;jmp	short K45B		; HANDLE WITH REST OF LETTER KEYS
	je	short K45B		
	;
	;-----	MUST BE F11 OR F12 
K53:					; F1 - F10 COME HERE, TOO
	test	bl, LEFT_SHIFT+RIGHT_SHIFT ; TEST SHIFT STATE
	jz	short K49		; JUMP, LOWER CASE PSEUDO SC'S
		; 20/02/2015 
	mov	ebx, K11		; UPPER CASE PSEUDO SCAN CODES
	jmp	short K64		; TRANSLATE SCAN
	;
	;-----	TRANSLATE THE CHARACTER
K56:					; TRANSLATE-CHAR
	dec	al			; CONVERT ORIGIN
	xlat    			; CONVERT THE SCAN CODE TO ASCII
	test	byte [KB_FLAG_3], LC_E0	; IS THIS A NEW KEY?
	jz	short K57		; NO, GO FILL BUFFER
	mov	ah, MC_E0		; YES, PUT SPECIAL MARKER IN AH
	jmp	short K57		; PUT IT INTO THE BUFFER	
	;
	;-----	TRANSLATE SCAN FOR PSEUDO SCAN CODES
K64:					; TRANSLATE-SCAN-ORGD
	dec	al			; CONVERT ORIGIN
       	xlat    	                ; CTL TABLE SCAN
	mov	ah, al			; PUT VALUE INTO AH
	mov	al, 0			; ZERO ASCII CODE
	test	byte [KB_FLAG_3], LC_E0	; IS THIS A NEW KEY?
	jz	short K57		; NO, GO FILL BUFFER
	mov	al, MC_E0		; YES, PUT SPECIAL MARKER IN AL
	;
	;-----	PUT CHARACTER INTO BUFFER
K57:					; BUFFER_FILL
	cmp	al, -1			; IS THIS AN IGNORE CHAR
	je	short K59		; YES, DO NOTHING WITH IT
	;je	K26			; YES, DO NOTHING WITH IT
	cmp	ah, -1			; LOOK FOR -1 PSEUDO SCAN
        ;;jne	short K61		; NEAR_INTERRUPT_RETURN
	;je	K26			; INTERRUPT_RETURN
	; 12/04/2021
        jne	short _K60		; NEAR_INTERRUPT_RETURN
K59:					; NEAR_INTERRUPT_RETURN
	jmp	K26			; INTERRUPT_RETURN

_K60: ; 29/01/2016
	cmp	ah, 68h	; ALT + F1 key
	jb	short K61
	cmp	ah, 6Fh ; ALT + F8 key	
	ja	short K61
	;
	mov	bl, [ACTIVE_PAGE]
	add	bl, 68h
	cmp	bl, ah
	je	short K61
	; 24/07/2022
	;push	ax
	push	eax
	mov	al, ah
	sub	al, 68h
	call	set_active_page
	pop	eax
	;pop	ax
K61:					; NOT-CAPS-STATE
	mov	ebx, [BUFFER_TAIL] 	; GET THE END POINTER TO THE BUFFER
	mov	esi, ebx		; SAVE THE VALUE
	call	_K4			; ADVANCE THE TAIL
	cmp	ebx, [BUFFER_HEAD] 	; HAS THE BUFFER WRAPPED AROUND
	je	short K62		; BUFFER_FULL_BEEP
	mov	[esi], ax		; STORE THE VALUE
	mov	[BUFFER_TAIL], ebx 	; MOVE THE POINTER UP
	jmp	K26
	;;cli				; TURN OFF INTERRUPTS
	;;mov	al, EOI			; END OF INTERRUPT COMMAND
	;;out	INTA00, al		; SEND COMMAND TO INTERRUPT CONTROL PORT
	;mov	al, ENA_KBD		; INSURE KEYBOARD IS ENABLED
	;call	SHIP_IT			; EXECUTE ENABLE
	;mov	ax, 9102h		; MOVE IN POST CODE & TYPE
	;int	15h			; PERFORM OTHER FUNCTION
	;;and	byte [KB_FLAG_3],~(LC_E0+LC_E1) ; RESET LAST CHAR H.C. FLAG
	;jmp	K27A			; INTERRUPT_RETURN
	;;jmp   K27                    
	;
	;-----	BUFFER IS FULL SOUND THE BEEPER
K62:
	mov	al, EOI			; ENABLE INTERRUPT CONTROLLER CHIP
	out	INTA00, al
	mov	cx, 678			; DIVISOR FOR 1760 HZ
	mov	bl, 4			; SHORT BEEP COUNT (1/16 + 1/64 DELAY)
	call	beep			; GO TO COMMON BEEP HANDLER
	jmp     K27			; EXIT   

SHIP_IT:
	;---------------------------------------------------------------------------------
	; SHIP_IT
	;	THIS ROUTINES HANDLES TRANSMISSION OF COMMAND AND DATA BYTES
	;	TO THE KEYBOARD CONTROLLER.
	;---------------------------------------------------------------------------------
	;
	
	;push	ax			; SAVE DATA TO SEND
	; 12/04/2021
	push	eax

	;-----	WAIT FOR COMMAND TO ACCEPTED
	cli				; DISABLE INTERRUPTS TILL DATA SENT
	; xor	ecx, ecx		; CLEAR TIMEOUT COUNTER
	mov	ecx, 10000h			
S10:
	in	al, STATUS_PORT		; READ KEYBOARD CONTROLLER STATUS
	test	al, INPT_BUF_FULL	; CHECK FOR ITS INPUT BUFFER BUSY
	loopnz	S10			; WAIT FOR COMMAND TO BE ACCEPTED

	;pop	ax			; GET DATA TO SEND
	; 12/04/2021
	pop	eax

	out	STATUS_PORT, al		; SEND TO KEYBOARD CONTROLLER
	sti				; ENABLE INTERRUPTS AGAIN
	retn				; RETURN TO CALLER

	; 12/04/2021 (32 bit push/pop)
SND_DATA:
	; ---------------------------------------------------------------------------------
	; SND_DATA
	;	THIS ROUTINES HANDLES TRANSMISSION OF COMMAND AND DATA BYTES
	;	TO THE KEYBOARD AND RECEIPT OF ACKNOWLEDGEMENTS. IT ALSO
	;	HANDLES ANY RETRIES IF REQUIRED
	; ---------------------------------------------------------------------------------
	;
	push	eax ; push ax		; SAVE REGISTERS
	push	ebx ; push bx
	push	ecx
	mov	bh, al			; SAVE TRANSMITTED BYTE FOR RETRIES
	mov	bl, 3			; LOAD RETRY COUNT
SD0:
	cli				; DISABLE INTERRUPTS
	and	byte [KB_FLAG_2], ~(KB_FE+KB_FA) ; CLEAR ACK AND RESEND FLAGS
	;
	;-----	WAIT FOR COMMAND TO BE ACCEPTED
	mov	ecx, 10000h		; MAXIMUM WAIT COUNT
SD5:
	in	al, STATUS_PORT		; READ KEYBOARD PROCESSOR STATUS PORT
	test	al, INPT_BUF_FULL	; CHECK FOR ANY PENDING COMMAND
	loopnz	SD5			; WAIT FOR COMMAND TO BE ACCEPTED
	;
	mov	al, bh			; REESTABLISH BYTE TO TRANSMIT
	out	PORT_A, al		; SEND BYTE
	sti				; ENABLE INTERRUPTS
	;mov	cx, 01A00h		; LOAD COUNT FOR 10 ms+
	mov	ecx, 0FFFFh
SD1:
	test	byte [KB_FLAG_2], KB_FE+KB_FA ; SEE IF EITHER BIT SET
	jnz	short SD3		; IF SET, SOMETHING RECEIVED GO PROCESS
	loop	SD1			; OTHERWISE WAIT
SD2:
	dec	bl			; DECREMENT RETRY COUNT
	jnz	short SD0		; RETRY TRANSMISSION
	or	byte [KB_FLAG_2], KB_ERR ; TURN ON TRANSMIT ERROR FLAG
	jmp	short SD4		; RETRIES EXHAUSTED FORGET TRANSMISSION
SD3:
	test	byte [KB_FLAG_2], KB_FA ; SEE IF THIS IS AN ACKNOWLEDGE
	jz	short SD2		; IF NOT, GO RESEND
SD4:	
	pop	ecx			; RESTORE REGISTERS
	pop	ebx ; pop bx
	pop	eax ; pop ax
	retn				; RETURN, GOOD TRANSMISSION

SND_LED:
	; ---------------------------------------------------------------------------------
	; SND_LED
	;	THIS ROUTINES TURNS ON THE MODE INDICATORS.
	;
	;----------------------------------------------------------------------------------
	;
	cli				; TURN OFF INTERRUPTS
	test	byte [KB_FLAG_2], KB_PR_LED ; CHECK FOR MODE INDICATOR UPDATE
	jnz 	short SL1		; DON'T UPDATE AGAIN IF UPDATE UNDERWAY
	;
	or	byte [KB_FLAG_2], KB_PR_LED ; TURN ON UPDATE IN PROCESS
	mov	al, EOI			; END OF INTERRUPT COMMAND
	out	20h, al ;out INTA00, al	; SEND COMMAND TO INTERRUPT CONTROL PORT
	jmp	short SL0		; GO SEND MODE INDICATOR COMMAND
SND_LED1:
	cli				; TURN OFF INTERRUPTS
	test	byte [KB_FLAG_2], KB_PR_LED ; CHECK FOR MODE INDICATOR UPDATE
	jnz	short SL1		; DON'T UPDATE AGAIN IF UPDATE UNDERWAY
	;
	or	byte [KB_FLAG_2], KB_PR_LED ; TURN ON UPDATE IN PROCESS
SL0:
	mov	al, LED_CMD		; LED CMD BYTE
	call	SND_DATA		; SEND DATA TO KEYBOARD
	cli
	call	MAKE_LED		; GO FORM INDICATOR DATA BYTE
	and	byte [KB_FLAG_2], 0F8h	; ~KB_LEDS ; CLEAR MODE INDICATOR BITS
	or	[KB_FLAG_2], al 	; SAVE PRESENT INDICATORS FOR NEXT TIME
	test	byte [KB_FLAG_2], KB_ERR ; TRANSMIT ERROR DETECTED
	jnz	short SL2		; IF SO, BYPASS SECOND BYTE TRANSMISSION
	;
	call	SND_DATA		; SEND DATA TO KEYBOARD
	cli				; TURN OFF INTERRUPTS
	test	byte [KB_FLAG_2], KB_ERR ; TRANSMIT ERROR DETECTED
	jz	short SL3		; IF NOT, DON'T SEND AN ENABLE COMMAND
SL2:
	mov	al, KB_ENABLE		; GET KEYBOARD CSA ENABLE COMMAND
	call	SND_DATA		; SEND DATA TO KEYBOARD
	cli				; TURN OFF INTERRUPTS
SL3:
	and	byte [KB_FLAG_2], ~(KB_PR_LED+KB_ERR) ; TURN OFF MODE INDICATOR
SL1:					; UPDATE AND TRANSMIT ERROR FLAG
	sti				; ENABLE INTERRUPTS
	retn				; RETURN TO CALLER

MAKE_LED:
	;---------------------------------------------------------------------------------
	; MAKE_LED
	;	THIS ROUTINES FORMS THE DATA BYTE NECESSARY TO TURN ON/OFF
	;	THE MODE INDICATORS.
	;---------------------------------------------------------------------------------
	;
	;push 	cx			; SAVE CX
	mov	al, [KB_FLAG]		; GET CAPS & NUM LOCK INDICATORS
	and	al, CAPS_STATE+NUM_STATE+SCROLL_STATE ; ISOLATE INDICATORS
	;mov	cl, 4			; SHIFT COUNT
	;rol	al, cl			; SHIFT BITS OVER TO TURN ON INDICATORS
	rol	al, 4 ; 20/02/2015
	and	al, 07h			; MAKE SURE ONLY MODE BITS ON
	;pop	cx
	retn				; RETURN TO CALLER

; % include 'kybdata.s'   ; KEYBOARD DATA


; /// End Of KEYBOARD FUNCTIONS ///
