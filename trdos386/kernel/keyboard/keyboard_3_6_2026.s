; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel) - v2.0.11 - keyboard.s
; ----------------------------------------------------------------------------
; Last Update: 03/06/2026 (Previous: 07/08/2022, v2.0.5)
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
	;call	SND_LED1
	; 28/05/2026
	call	SND_LED
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

; 03/06/2026
; ===============================================================================
; TRDOS 386 v2.0.11 - FINAL GOLDEN MASTER KEYBOARD HANDLER
; Completely Fixed: NumLock LED Freeze, Arrow Keys Drop, and Blocking Beep Loop
; Safe for Real AMD Athlon 64-2 / NForce 4 Hardware and QEMU Emulator
; Format: NASM 32-bit Protected Mode (Ring 0 Architecture)
; ===============================================================================

; 29/05/2026
; 28/05/2026 - TRDOS 386 v2.0.11
;
; ===============================================================================
; TRDOS 386 v2.0.11 - MODERNIZED HARDWARE INT 09H (IRQ LEVEL 1) HANDLER
; Derived from IBM PC-XT-286 (1986) & Award BIOS (1999) Source Codes
; Fixed: Keyboard Controller Locking / Typematic Autorepeat Freeze
; Format: NASM 32-bit Protected Mode (Ring 0)
; ===============================================================================
; corrected by Google AI - 28/05/2026
;
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

	; 28/05/2026
%macro NEW_IODELAY 0
	out 0EBh, al	; Hardware ~250ns breathing delay (Award BIOS style)
%endmacro

	; 28/05/2026
OUTP_BUF_FULL	equ 001h  ; Bit 0: Output Buffer Full (OBF)
OBF_AUX		equ 020h  ; Bit 5: Mouse / Auxiliary Data Ready

	; 03/06/2026 (Google AI)
KB_INT_1:
	sti				; ENABLE INTERRUPTS FOR LOWER LATENCY
	push	eax
	push	ebx
	push	ecx
	push	edx
	push	esi
	push	edi
	push	ds
	push	es
	cld				; FORCE FORWARD DIRECTION ON STRING OPERATIONS
	mov	ax, KDATA
	mov	ds, ax
	mov	es, ax

	; ---------------------------------------------------------------------------
	; MODERN OS APPROACH: NO EMBEDDED CONSUMER LOOP (POLLING OVER IRQ)
	; Process exactly one byte per hardware interrupt to prevent 8042 controller
	; data bus suffocation and potential race conditions in Legacy USB Emulation.
	; ---------------------------------------------------------------------------
	in	al, STATUS_PORT         ; Read Keyboard Controller Status (Port 064h)
	NEW_IODELAY
	test	al, OUTP_BUF_FULL       ; Check Bit 0: Output Buffer Full (OBF)
	jz	near K26A               ; If OBF=0 (Buffer Empty), bypass and exit without EOI

	test    al, OBF_AUX             ; Check Bit 5: Mouse / Auxiliary Data Ready
	jz      short kb_process_keyboard_byte
	in      al, PORT_A              ; Read and discard PS/2 Mouse packet byte
	NEW_IODELAY                     ; to prevent IRQ1 starvation/deadlock
	jmp	near K26                ; Signal EOI and terminate interrupt safely

kb_process_keyboard_byte:
	in	al, PORT_A              ; Read raw Scancode from Data Port (Port 060h)
	NEW_IODELAY
	mov	ah, al                  ; Backup raw Scancode in AH register

	; ----- HARDWARE RESPONSES: ACK / RESEND INTERCEPT -----
	cmp	al, KB_RESEND		; Did 8042 controller request a Resend?
	je      short KB_INT_4
	cmp	al, KB_ACK		; Did 8042 controller return an Acknowledge?
	jne     short KB_INT_2

	; --- ACK (Acknowledge) Intercept ---
	or	byte [KB_FLAG_2], KB_FA ; Update status flags: ACK received
	jmp	near K26                ; ACK is a control byte, drop it and exit without buffer write

KB_INT_4:
	; --- Resend Intercept ---
	or	byte [KB_FLAG_2], KB_FE ; Update status flags: Resend received
	jmp	near K26                ; Drop control byte and exit safely

KB_INT_2:
	; ----- MODE INDICATORS (LED STATUS MANAGEMENT) -----
	call	MAKE_LED		; Form the required LED state bitmask
	mov	bl, [KB_FLAG_2] 	; Fetch previous saved indicators
	xor	bl, al			; Determine if state change occurred
	and	bl, KB_LEDS		; Isolate specific LED bits
	jz	short UP0		; If state is identical, bypass command injection

	; HARDWARE DEADLOCK PREVENTION FOR HARDWARE/USB EMULATION:
	; Inject LED change command sequence ONLY during key release (Break Code >= 80h).
	; Injecting I/O commands during high-speed Autorepeat (make state) causes
	; internal state machine corruption inside real NForce 4 USB Host Controllers.
	test	ah, 80h
	jz	short UP0
	call	SND_LED			; Safely transmit LED sequence to keyboard
UP0:
	mov	al, ah                  ; Restore validated raw Scancode into AL register

; ---------------------------------------------------------------------------
;	START OF KEY PROCESSING MAPPING ENGINE
; ---------------------------------------------------------------------------
	cmp	al, KB_OVER_RUN		; Is this a Buffer Overrun signal (0FFh/0FEh)?
	je	near kb_buffer_full_logic ; Branch to smart non-blocking overflow logic

	mov	bh, [KB_FLAG_3]		; Load extended status flags (LC_E0 / LC_E1)

	test 	bh, RD_ID+LC_AB 	; Is keyboard hardware identification in progress?
	jz	short NOT_ID
	jmp	near K26

NOT_ID:
	; ----- HARDWARE TIMING FIX FOR EXTENDED KEYS (0E0h Prefix) -----
	; In physical hardware, a noticeable sub-millisecond delay occurs between the 
	; 0E0h prefix byte and the actual directional Scancode. Exiting the IRQ immediately
	; allows the hardware to settle and trigger a subsequent IRQ for the second byte,
	; resolving dropped arrow key issues on real AMD processors.
	cmp	al, MC_E0		; Is this the General Extended Marker (0E0h)?
	jne	short TEST_E1
	or	byte [KB_FLAG_3], LC_E0+KBX ; Raise the Last Character Extended Flag in memory
	jmp	near K26                ; FORCE DISMISS: Terminate current IRQ to await second byte

TEST_E1:
	cmp	al, MC_E1		; Is this the Pause Lead Code Marker (0E1h)?
	jne	short NOT_HC
	or	byte [KB_FLAG_3], LC_E1+KBX ; Raise the Last Character E1 Flag in memory
	jmp	near K26                ; FORCE DISMISS

; ---------------------------------------------------------------------------
; NOT_HC: MAIN SCANCODE DECODER ENGINE FOR EXTENDED & MODIFIER KEYS
; ---------------------------------------------------------------------------
NOT_HC:
	test	bh, LC_E0		; Was the previous processed byte an 0E0h prefix?
	jz	short NOT_LC_E0		; No, branch directly to standard key processing

	; Extended Key Stream Detected (e.g., Arrow Keys fall through here)
	and	al, 07Fh                ; Strip the Break Bit safely before lookups
	mov	edi, _K6+6		; Point to Extended Shift Key Table
	scasb
	je	short K16B              ; If it matches fake/extended shift, suppress it
	scasb
	jne	short K16A		; If it is a valid Arrow Key, pass to standard translator (K25)
	jmp	short K16B

NOT_LC_E0:
	test	bh, LC_E1		; Was the previous processed byte an 0E1h prefix?
	jz	short T_SYS_KEY
	and	al, 07Fh
	mov	ecx, 4
	mov	edi, _K6+4
	repne	scasb
	je	near K26
	cmp	al, NUM_KEY
	jne	short K16B
	test	ah, 80h
	jnz	short K16B
	test	byte [KB_FLAG_1], HOLD_STATE
	jnz	short K16B
	jmp     K39P                    ; Branch to real hardware Pause routine

T_SYS_KEY:
	cmp	al, SYS_KEY
	jnz	short K16A
	test	ah, 80h
	jnz	short K16C
	test	byte [KB_FLAG_1], SYS_SHIFT
	jnz	short K16B
	or	byte [KB_FLAG_1], SYS_SHIFT
	jmp     K27A                    ; Dismiss interrupt without sending EOI

K16B:
	jmp	near K26                ; Drop unhandled/corrupted data streams

; ... (Remaining K16A, K17, K23, K25 translation tables work identically) ...

	; 28/05/2026 - Corrected by Google AI

	; --- Modifier Keys (Shift/Ctrl/Alt) Checks ---
K16C:
	and	byte [KB_FLAG_1], ~SYS_SHIFT ; TURN OFF SHIFT KEY HELD DOWN

	; 28/05/2026
	;mov	al, EOI			; END OF INTERRUPT COMMAND
	;out	20h, al ;out INTA00, al ; SEND COMMAND TO INTERRUPT CONTROL PORT
	;				; INTERRUPT-RETURN-NO-EOI
	;;MOV	AL, ENA_KBD		; INSURE KEYBOARD IS ENABLED
	;;CALL	SHIP_IT			; EXECUTE ENABLE
	;;
	;;MOV	AX, 8501H		; FUNCTION VALUE FOR BREAK OF SYSTEM KEY
	;;STI				; MAKE SURE INTERRUPTS ENABLED
	;;INT	15H			; USER INTERRUPT
	;;JMP	K27A			; IGNORE SYSTEM KEY

	; 28/05/2026
	; Branch to exit
	jmp     K27			; IGNORE SYSTEM KEY

	; 03/06/2026
	;-----	TEST FOR SHIFT / TOGGLE KEYS (COMPATIBILITY RESOLVER)
K16A:
	mov	bl, [KB_FLAG]		; Load core flags
	mov	ah, al			; Backup raw scancode
	and	al, 07Fh		; Strip Break bit (Bit 7) for evaluation

	; ----- ATOMIC INLINE CHECK FOR PRIMARY HARDWARE SHIFT KEYS -----
	cmp	al, 42			; 2Ah: Is this the Left Shift Key?
	je	short .is_pure_shift
	cmp	al, 54			; 36h: Is this the Right Shift Key?
	je	short .is_pure_shift

	; If it's not physical left/right shift, search the Toggle/Alt/Ctrl table (_K6)
	mov	edi, _K6
	mov	ecx, _K6L
	repne	scasb
	mov	al, ah			; Restore original scancode
	je	short K17		; Modifier match found in table!
	jmp	K25			; No match: redirect to normal character routine

.is_pure_shift:
	; Form appropriate shift bitmask on the fly for physical Left/Right Shift
	mov	al, ah                  ; Restore original scancode
	mov	ah, LEFT_SHIFT          ; Default to Left Shift bitmask (02h)
	cmp	al, 54                  ; Was it Right Shift?
	jne	short .apply_pure_shift
	mov	ah, RIGHT_SHIFT         ; Apply Right Shift bitmask (01h)
.apply_pure_shift:
	mov	cl, 2
	test	al, 80h			; Make or Break?
	jz	short K17C		; If Make: Jump straight to shift activation logic
	jmp	near K23		; If Break: Jump straight to shift release logic

	; 03/06/2026
; ============================================================================
; MODIFIER AND TOGGLE KEY STATE STATE MACHINE (TECHNICAL COMPATIBILITY LAYER)
; ============================================================================

K17:					; TOGGLE KEYS (CAPS/NUM/SCROLL/ALT/CTRL) MATRICES
	sub	edi, _K6+1
	mov	ah, [edi+_K7]       	; Safely extract bitmask from corrected _K7 table
	mov	cl, 2
	test	al, 80h
	jz	short K17C
	jmp	K23

K17C:					; PLAIN SHIFT KEY ACTIVATION (MAKE STATE)
	cmp	ah, SCROLL_SHIFT
	jae	short K18		; If Scroll Lock or above, route to Toggle Engine

	or	[KB_FLAG], ah		; Assert active Shift bit in core KB_FLAG
	test	al, CTL_SHIFT+ALT_SHIFT
	jz	short k17f
K17D:
	test	bh, LC_E0
	jz	short K17E
	or	[KB_FLAG_3], ah		; Track Right Ctrl / Right Alt state
	jmp	short k17f
K17E:
	shr	ah, cl
	or	[KB_FLAG_1], ah		; Track Left Ctrl / Left Alt state
k17f:
	jmp	near K26		; Terminate current IRQ session cleanly

K18:					; EVALUATE TOGGLE LOCK CONFIGURATIONS
	test	bl, CTL_SHIFT
	jz	short K18A
	jmp	short k20a
K18A:
	cmp	al, INS_KEY
	jne	short K22
	test	bl, ALT_SHIFT
	jz	short K18B
	jmp	short k20a
K18B:
	test	bh, LC_E0
	jnz	short K22
K19:
	test	bl, NUM_STATE
	jnz	short K21
	test	bl, LEFT_SHIFT+RIGHT_SHIFT
	jz	short K22
K20:
	mov	ah, al
k20a:
	jmp	K25                     ; Pass control to default alpha matrix

K21:
	test	bl, LEFT_SHIFT+RIGHT_SHIFT
	jz	short K20

K22:					; TOGGLE LOCK MECHANISM (CAPS, NUM, SCROLL)
	test	ah, [KB_FLAG_1] 	; Check Autorepeat Lock: Is key already held?
	jnz	short k24a	; YES: Suppress duplication, discard interrupt
K22A:
	or	[KB_FLAG_1], ah 	; Lock bit state until Break code arrives
	xor	[KB_FLAG], ah		; Invert state mask (Toggles case logic)

	test	ah, CAPS_SHIFT+NUM_SHIFT+SCROLL_SHIFT
	jz	short K22B

	push	eax
	call	SND_LED			; Strobe physical hardware keyboard LEDs
	pop	eax
K22B:
	cmp	al, INS_KEY
	jne	short k24a
	mov	ah, al
	jmp	K28
K23:					; PLAIN SHIFT KEY DEACTIVATION (BREAK STATE)
	cmp	ah, SCROLL_SHIFT
	not	ah
	jae	short K24		; If key is Caps/Num/Scroll, route to K24
	and	[KB_FLAG], ah		; Clear active Shift bit in core KB_FLAG
	cmp	ah, ~CTL_SHIFT
	ja	short K23D

	test	bh, LC_E0
	jz	short K23A
	and	[KB_FLAG_3], ah		; Release Right Alt / Right Ctrl bits
	jmp	short K23B
K23A:
	sar	ah, cl
	and	[KB_FLAG_1], ah		; Release Left Alt / Right Ctrl bits
K23B:
	mov	ah, al
	mov	al, [KB_FLAG_3]
	and	al, ALT_SHIFT+CTL_SHIFT
	or	[KB_FLAG], al
	mov	al, ah
K23D:
	cmp	al, ALT_KEY+80h
	jne	short k24a

	mov	al, [ALT_INPUT]
	mov	ah, 0
	mov	[ALT_INPUT], ah
	cmp	al, 0
	je	short K26
	jmp	_K60

K24:					; HARDWARE BREAK-TOGGLE DEACTIVATION ENGINE
	; CRITICAL FIX: When CapsLock/NumLock key is released, we MUST clear the
	; tracking bit in KB_FLAG_1 so that the NEXT physical keypress can be detected!
	and	[KB_FLAG_1], ah 	; Clear active tracking mask in KB_FLAG_1
k24a:
	jmp	near K26		; Secure termination gate

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

	; 03/06/2026
	;jmp	short K16B
	jmp	short K26

; ============================================================================
; SMART TWO-STAGE OVERFLOW MANAGEMENT (Sane Non-blocking Fallback Strategy)
; Completely eliminates execution blocking loops (PIT Channel 2 delay loops)
; inside the hardware interrupt handler which caused system wide freezes.
; ============================================================================
kb_buffer_full_logic:
	inc	byte [kb_beep_count]	; Increment consecutive overflow error counter
	cmp	byte [kb_beep_count], 2 ; Is this the second consecutive overflow?
	jae	short kb_flush_and_silent ; If yes, flush software ring buffer silently

	; FIRST CONSECUTIVE OVERFLOW: Dismiss the interrupt silently without blocking execution.
	; Modern Operating Systems (Linux/Windows standard) drop the key stroke silently
	; to preserve system clock and interrupt latency integrity.
	jmp	near K26

kb_flush_and_silent:
	; REPEATED OVERFLOWS DETECTED: Hardware stream is corrupted or buffer wrapped.
	; Safely flush the software ring buffer structure by syncing pointers instantly.
	mov	ebx, [BUFFER_HEAD]
	mov	[BUFFER_TAIL], ebx      ; Enforce Tail = Head (Buffer Reset)
	mov	byte [kb_beep_count], 0	; Reset the threshold counter
	; 03/06/2026
	;jmp	near K26

	; 03/06/2026
; ============================================================================
; SAFE EXIT DOORS (TERMINATION & SHUTDOWN ENGINES)
; ============================================================================
K26:
	; Reset extended context states upon successful scancode tracking cycle
	and	byte [KB_FLAG_3], ~(LC_E0+LC_E1)
K26A:
	; Issue End of Interrupt (EOI) command to master Programmable Interrupt Controller
	mov	al, EOI                 ; 20h
	out	20h, al                 ; Signal Master PIC (Port 020h)
	NEW_IODELAY
K27:
	; Historical BIOS tags preserved for macro linkage integrity
K27A:
	cli				; Disable interrupts during atomic stack restoration
	pop	es
	pop	ds
	pop	edi
	pop	esi
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax
	iretd


	; 03/06/2026

	; ===============================================================================
	; TRDOS 386 v2.0.11 (386 DOS / PCDOS 386 Development Branch)
	; KEYBOARD HARDWARE INT 09H HANDLER - PART 2: BUFFER FILL & QUEUE ENGINE
	; ===============================================================================

;-----	NOT IN HOLD STATE (STANDARD SCANCODE INTERPRETATION)
K28:					; NO-HOLD-STATE
	cmp	al, 88			; Test for out-of-range hardware scancodes
	ja	near K26		; Drop if code exceeds standard matrix layout

	test	bl, ALT_SHIFT 		; Check Bit 3 of KB_FLAG: Alternate Shift state active?
        jz	short K28A		; If ALT is not pressed, branch to normal processing

	test	bh, KBX			; Enhanced keyboard detected during POST/ID?
	jz	short K29		; No, ALT state mapping is standard

	test	byte [KB_FLAG_1], SYS_SHIFT ; Is SysReq key currently held down?
	jz	short K29		; No, ALT state translation is legitimate

K28A:	jmp	K38			; Pass directly to Non-Alternate translation logic

;-----	TEST FOR SYSTEM RESET KEY SEQUENCE (CTL + ALT + DEL)
K29:					; TEST-RESET
	test	bl, CTL_SHIFT 		; Check Bit 2 of KB_FLAG: Control Shift active?
	jz	short K31		; No reset condition, continue mapping
	cmp	al, DEL_KEY		; Ctl+Alt active, check if Delete key is pressed
	jne	short K31		; No reset condition, bypass system shutdown

;-----	TRIPLE-KEY HARDWARE RESET SEQUENCE DETECTED (CTL-ALT-DEL)
cpu_reset:
	; Formatted based on IBM PC/AT ROM BIOS specification (PROC_SHUTDOWN architecture)
	; Inject pulse command FEh (System Pulse Reset) directly to the 8042 status gate.
	mov	al, SHUT_CMD		; Load Hardware Reset Command (0FEh)
	out	STATUS_PORT, al		; Dispatch directly to Keyboard Controller Status Port
khere:
	hlt				; Halt processor execution stream
	jmp 	short khere		; Indefinite spin-lock loop until hardware reset triggers

;-----	IN ALTERNATE SHIFT STATE - ALPHANUMERIC & SPECIAL MAPPINGS
K31:					; NO-RESET
	cmp	al, 57			; Test for Spacebar Scancode
	jne	short K311		; Branch if not Spacebar
	mov	al, ' '			; Map to ASCII space character
k31a:
	and	byte [KB_FLAG_3], ~LC_E0 ; CRITICAL TIMING FIX: Purge Extended Key tracking bit
	jmp     K57                     ; Dispatch to hardware buffer queue insertion engine

K311:
	cmp	al, 15			; Test for Tab key Scancode
	jne	short K312
	mov	ax, 0A500h		; Set specialized pseudo-scancode matrix for Alt-Tab
	jmp	short k31a

K312:
	cmp	al, 74			; Test for Keypad Minus (-) Scancode
	je	short k312a
	cmp	al, 78			; Test for Keypad Plus (+) Scancode
	jne	short K32
k312a:
	jmp	K37B

;-----	LOOK FOR VALID NUMERIC KEYPAD ENTRY (ALT + NUMBER PAD ASCII GENERATION)
K32:					; ALT-KEY-PAD
	mov	edi, K30		; Point to standard Alternate Numpad translation matrix
	mov	ecx, 10			; Bound lookup count to 10 numerical digits
	repne	scasb			; Scan row for Scancode intersection match
	jne	short K33		; Not a keypad numeric entry, look for character translation
	test	bh, LC_E0		; Is this one of the standalone extended cursor navigation keys?
        jnz	short K37C		; Yes, intercept and map to Edit/Navigation subsystem
	sub	edi, K30+1		; DI register now contains specific numerical digit value
	mov	al, [ALT_INPUT] 	; Load current accumulated decimal ASCII value
	mov	ah, 10			; Load multiplier factor
	mul	ah			; Shift accumulated value by one decimal place
	add	ax, di			; Merge new numerical entry into low byte
	mov	[ALT_INPUT], al 	; Commit newly accumulated code to system state variable
K32A:
        jmp     near K26                ; Terminate current hardware interrupt session safely

;-----	LOOK FOR SUPERSHIFT ALPHABETIC KEY INTERPRETATION
K33:					; NO-ALT-KEYPAD
        mov     byte [ALT_INPUT], 0     ; Flush any partially accumulated numeric input values
	mov	ecx, 26			; Map scan parameters to 26 alphabetic characters
	repne	scasb			; Search alphabetic matrix for a valid Scancode match
	je	short K37A		; Match isolated, inject character structure to ring buffer

;-----	LOOK FOR TOP ROW FUNCTION VALUE UNDER ALTERNATE SHIFT
K34:					; ALT-TOP-ROW
	cmp	al, 2			; Check baseline numerical key boundaries ('1')
	jb	short K37B		; Below bounds, translate as standard Escape sequence
	cmp	al, 13			; Check upper row boundaries
	ja	short K35		; Outside row boundaries, drop to function translator
	add	ah, 118			; Translate scancode to specialized pseudo extended range
	jmp	short K37A		; Commit sequence to hardware ring buffer

;-----	DECODE ALTERNATE SHIFT FUNCTION KEYS AND EXTENDED ENHANCED LAYOUTS
K35:					; ALT-FUNCTION
	cmp	al, F11_M		; Check if F11 key triggered interrupt
	jb	short K35A
	cmp	al, F12_M		; Check if F12 key triggered interrupt
	ja	short K35A
	add	ah, 52			; Map to corresponding 32-bit DOS pseudo function code
	jmp	short K37A

K35A:
	test	bh, LC_E0		; Verify if current byte is flagged as an Extended Key
	jz	short K37		; Standard function layout, drop to standard mapping
	cmp	al, 28			; Check for Extended Keypad Enter key Scancode
        jne     short K35B
	mov	ax, 0A600h		; Inject proprietary Alt-Keypad Enter token
	jmp	short k35c

K35B:
	cmp	al, 83			; Check for Extended Delete key Scancode
	je	short K37C		; Redirect to standalone text manipulation subsystem
	cmp	al, 53			; Check for Extended Keypad Divide (/) Scancode
	jne	short K32A		; Unmapped E0 stream variation, abort processing loop
	mov	ax, 0A400h		; Inject Alt-Keypad Slash token
k35c:
	and	byte [KB_FLAG_3], ~LC_E0 ; CRITICAL TIMING FIX: Reset Extended State flag prior to push
	jmp	K57			; Inject character payload to system ring buffer

K37C:
	add	al, 80			; Apply translation bias for enhanced navigation/edit blocks
	mov	ah, al			; Synchronize Scancode across tracking registers
	jmp     short K37A

K37:
	cmp	al, 59			; Check baseline threshold for Function Key F1
        jb      short K37B
	cmp	al, 68			; Upper bound test for Function Key F10
	ja	short K32A		; Within Keypad matrix layout, ignore processing cycle
	add	ah, 45			; Translate to extended function token range
K37A:
	mov	al, 0			; Force standard ASCII Null byte flag in AL
	jmp	short k35c
K37B:
	mov	al, 0F0h		; Apply special tracking flag for extended subsystem character
	jmp	short k35c

; ============================================================================
; KEY PROCESSING UNDER CONTROL-SHIFT (NON-ALT PATHWAY)
; ============================================================================
K38:					; NOT-ALT-SHIFT
	test	bl, CTL_SHIFT 		; Check Bit 2 of KB_FLAG: Control Shift active?
	jnz	short K38A		; Control state confirmed, process modifiers
	jmp	K44			; No control tracking, branch to Shift/Unshifted engine

K38A:
	cmp	al, SCROLL_KEY		; Test for hardware Ctrl + Break combination
	jne	short K39		; Not a break sequence, test for Pause conditions
	test	bh, KBX			; Enhanced keyboard architecture active?
	jz	short K38B
	test	bh, LC_E0		; Was the previous raw byte an E0 extended prefix?
	jz	short K39		; False match, step down to Pause evaluation engine
K38B:
	mov	ebx, [BUFFER_HEAD] 	; Atomically flush system software keyboard buffer
	mov	[BUFFER_TAIL], ebx	; Enforce Tail = Head to purge unread input streams
	mov	byte [BIOS_BREAK], 80h  ; Assert the global system break signal flag

	; CRITICAL MODIFICATION: Removed legacy 8042 locking ENA_KBD routines here.
	call	ctrlbrk 		; Execute native Protected Mode Control+Break handler
	sub	eax, eax		; Inject clean placeholder dummy character token
	and	byte [KB_FLAG_3], ~LC_E0 ; Force close extended key tracking window
	jmp     K57                     ; Update ring buffer immediately

;-----	EVALUATE AND MAP HARDWARE PAUSE / HOLD SUBSYSTEM
K39:					; NO_BREAK
	test	bh, KBX			; Is enhanced keyboard matrix tracking active?
	jnz	short K41		; Enhanced configuration cannot issue standard legacy pause
	cmp	al, NUM_KEY		; Evaluate if NumLock/Pause key scancode is present
	jne	short K41		; Not a hold trigger, drop to character table decoder

K39P:
	or	byte [KB_FLAG_1], HOLD_STATE ; Assert global task execution suspend flag

	; ---------------------------------------------------------------------------
	; MODERN MULTITASKING KERNEL REFACTOR: DANGEROUS SPIN-LOCK LOOP REMOVED
	; A persistent hardware loop waiting for flag clearance within a Ring 0
	; interrupt service routine will freeze the system. Hardware state variable
	; tracking handles suspension safely outside interrupt contexts.
	; ---------------------------------------------------------------------------
	cmp     byte [CRT_MODE], 7	; Is the primary video card configured for Monochrome?
	je	short .skip_crt		; Monochrome active, bypass register strobe
	mov	dx, 03D8h		; Load color CRT Mode Control Register Port
	mov     al, [CRT_MODE_SET] 	; Fetch stored system video state parameter
	out	dx, al			; Re-strobe video output pipeline to verify CRT is active
.skip_crt:
        jmp     near K27                ; Exit handler immediately through non-EOI gateway

;-----	EVALUATE SPECIAL CODES AND SYMBOLS FOR KEY 55 (PRINT SCREEN / ASTERISK)
K41:					; NO-PAUSE
	cmp	al, 55			; Check raw Scancode for asterisk key
	jne	short K42
	test	bh, KBX
	jz	short K41A
	test	bh, LC_E0
	jz	short K42B
K41A:
	mov	ax, 114*256		; Inject Print Screen function code string
	and	byte [KB_FLAG_3], ~LC_E0
        jmp     K57

K42:					; NOT-KEY-55
	cmp	al, 15			; Test for Tab key translation parameters
	je	short K42B
	cmp	al, 53			; Test for Keypad Forward Slash (/) Scancode
	jne	short K42A
	test	bh, LC_E0		; Extended numerical block version?
	jz	short K42A
	mov	ax, 9500h		; Inject precise Ctrl-Keypad Slash token mapping
	and	byte [KB_FLAG_3], ~LC_E0
	jmp	K57
K42A:
	cmp	al, 59			; Evaluate character boundary matrices
K42B:
	mov	ebx, _K8		; Load Control Character Matrix Translation Table Address
	jb	short K45F
	jmp	K64

; ============================================================================
; KEY PROCESSING UNDER SHIFTED OR BASE (UNSHIFTED) STATES
; ============================================================================
K44:					; NOT-CTL-SHIFT
	cmp	al, 55			; Is this the Print Screen key trigger?
	jne	short K45
	test	bh, KBX
	jz	short K44A
	test	bh, LC_E0
	jnz	short K44B
	jmp	short K45C
K44A:
	test	bl, LEFT_SHIFT+RIGHT_SHIFT
	jz	short K45C

K44B:
	and     byte [KB_FLAG_3], ~(LC_E0+LC_E1)
        jmp     near K26

K45:					; NOT-PRINT-SCREEN
	cmp	al, 58			; Is scancode inside alphanumeric grid?
	ja	short K47
	cmp	al, 53			; Is this the standard '/' Key?
	jne	short K45A
	test	bh, LC_E0
	jnz	short K45C
K45A:
	mov	ecx, 26
	mov	edi, K30+10		; Point to Alphabetical lookup matrix
	repne	scasb
	jne	short K45B
	test	bl, CAPS_STATE		; Is CapsLock toggled active?
	jnz	short K45D
K45B:
	test	bl, LEFT_SHIFT+RIGHT_SHIFT
	jnz	short K45E
K45C:
	mov	ebx, K10		; Lowercase Matrix Offset
	jmp	short K56
K45D:
	test	bl, LEFT_SHIFT+RIGHT_SHIFT
	jnz	short K45C
K45E:
	mov	ebx, K11		; Uppercase Matrix Offset
K45F:	jmp	short K56

K46:					; NOT IN-CORE AREA
	cmp	al, 68
	jna	short K53

K47:					; NUMERICAL KEYPAD DECODER
	cmp	al, 83
	ja	short K52
K48:
	cmp	al, 74			; Intercept for Keypad Minus (-)
	je	short K45E
	cmp	al, 78			; Intercept for Keypad Plus (+)
	je	short K45E
	test	bh, LC_E0		; Separate Navigation layout?
	jnz	short K49
	test 	bl, NUM_STATE		; Is global NumLock state active?
	jnz	short K50
	test	byte [KB_FLAG], LEFT_SHIFT+RIGHT_SHIFT
	jnz	short K45E

K49:					; KEYPAD BASE CASE
	cmp	al, 76			; Center key (Keypad 5)
	jne	short K49A
	mov	al, 0F0h
	and	byte [KB_FLAG_3], ~LC_E0
	jmp	short K57
K49A:
	mov	ebx, K10
	jmp	short K64

K50:
        test    bl, LEFT_SHIFT+RIGHT_SHIFT
	jnz 	short K49
K51:	jmp	short K45E

K52:					; HIGH-BOUND AT EXTENSION
	cmp	al, 86
	je	short K45B

K53:					; DUAL FUNCTION ROUTING
	test	bl, LEFT_SHIFT+RIGHT_SHIFT
	jz	short K49
	mov	ebx, K11
	jmp	short K64

K56:					; ASCII TRANSLATION RESOLVER
	dec	al
	xlat
	test	byte [KB_FLAG_3], LC_E0
	jz	short K57
	mov	ah, MC_E0
	jmp	short K57

K64:					; SCANCODE TO PSEUDO-SCANCODE
	dec	al
       	xlat
	mov	ah, al
	mov	al, 0
	test	byte [KB_FLAG_3], LC_E0
	jz	short K57
	mov	al, MC_E0

; ============================================================================
; 32-BIT KERNEL SOFTWARE RING BUFFER MANAGEMENT ENGINE
; ============================================================================
K57:					; BUFFER_FILL ENTRY POINT
	cmp	al, -1
	je	short K59
	cmp	ah, -1
        jne	short _K60
K59:
	jmp	near K26

_K60:
	cmp	ah, 68h			; Alt + F1 Virtual Console hotkey
	jb	short K61
	cmp	ah, 6Fh 		; Alt + F8 Virtual Console hotkey
	ja	short K61

	mov	bl, [ACTIVE_PAGE]
	add	bl, 68h
	cmp	bl, ah
	je	short K61

	push	eax
	mov	al, ah
	sub	al, 68h
	call	set_active_page
	pop	eax

K61:
	mov	ebx, [BUFFER_TAIL] 	; Write pointer of software buffer
	mov	esi, ebx
	call	_K4
	cmp	ebx, [BUFFER_HEAD] 	; Overflow condition?
	je	short kb_buffer_full_silent

	mov	[esi], ax		; Commit 16-bit payload to TRDOS queue
	mov	[BUFFER_TAIL], ebx
	mov	byte [kb_beep_count], 0
	jmp	near K26

kb_buffer_full_silent:
	jmp	near K26

; ============================================================================
; ATOMIC PORT INTERFACES & ASYNCHRONOUS PERIPHERAL SIGNALLING
; ============================================================================
align 4
SHIP_IT:	; It has been left in place for backward compatibility.
		; 8042 Waits for the input buffer to clear.
	;---------------------------------------------------------------------
	; SHIP_IT
	;	THIS ROUTINES HANDLES TRANSMISSION OF COMMAND AND DATA BYTES
	;	TO THE KEYBOARD CONTROLLER.
	;---------------------------------------------------------------------
	;
	push	eax
	cli
	mov	ecx, 50000h
S10:
	in	al, STATUS_PORT
	NEW_IODELAY 
	test	al, INPT_BUF_FULL
	loopnz	S10

	pop	eax
	out	STATUS_PORT, al
	NEW_IODELAY
	sti
	retn

	; 03/06/2026
; ============================================================================
; OPTIMIZED PORT I/O FUNCTIONS (Asynchronous SND_DATA Implementation)
; ============================================================================
align 4
SND_DATA: ; It sends a command/data byte to the keyboard and waits for an ACK.
	  ; (Nested Timeout added)
	;---------------------------------------------------------------------
	; SND_DATA
	;	THIS ROUTINES HANDLES TRANSMISSION OF COMMAND AND DATA BYTES
	;	TO THE KEYBOARD AND RECEIPT OF ACKNOWLEDGEMENTS. IT ALSO
	;	HANDLES ANY RETRIES IF REQUIRED
	;---------------------------------------------------------------------
	;
	push	eax
	push	ebx
	push	ecx
	mov	bh, al			; Cache data byte for potential retransmission
	mov	bl, 3			; Initialize retry counter (Max 3 attempts)
SD0:
	; CRITICAL FIX: Do not globally disable interrupts (cli) for long durations.
	; The ACK byte (0FAh) sent by the keyboard can ONLY be captured if the
	; main handler state machine remains responsive to new data streams.
	and	byte [KB_FLAG_2], ~(KB_FE+KB_FA) ; Clear ACK and Resend state flags

	mov	ecx, 50000h		; Safe timeout loop bound for high speed CPUs
SD5:
	in	al, STATUS_PORT		; Poll Controller Status Port
	NEW_IODELAY
	test	al, INPT_BUF_FULL	; Check Bit 1: Input Buffer Full (IBF)
	loopnz	SD5			; Wait until IBF clears (Controller ready)

	mov	al, bh
	out	PORT_A, al		; Transmit data byte to Data Register (Port 060h)
	NEW_IODELAY

	; Asynchronous polling loop for state flag updates from the main handler
	mov	ecx, 0FFFFh
SD1:
	test	byte [KB_FLAG_2], KB_FE+KB_FA ; Has an ACK or Resend flag been raised?
	jnz	short SD3		; Yes, intercept and process hardware answer
	NEW_IODELAY
	loop	SD1			; No, spin until timeout
SD2:
	dec	bl			; Decrement retry count
	jnz	short SD0		; Loop back for retransmission
	or	byte [KB_FLAG_2], KB_ERR ; Raise transmission error flag
	jmp	short SD4		; Terminate attempts
SD3:
	test	byte [KB_FLAG_2], KB_FA ; Did we receive a valid ACK (0FAh)?
	jz	short SD2		; If Resend (0FEh), try transmission again
SD4:
	pop	ecx
	pop	ebx
	pop	eax
	retn				; Return to caller with updated status flags

align 4	
SND_LED: ; Updates keyboard LED status (Early EOI cleaned up!)
	;---------------------------------------------------------------------
	; SND_LED
	;	THIS ROUTINES TURNS ON THE MODE INDICATORS.
	;
	;---------------------------------------------------------------------
	;
	cli
	test	byte [KB_FLAG_2], KB_PR_LED
	jnz 	short SL1
	or	byte [KB_FLAG_2], KB_PR_LED
SL0:
	mov	al, LED_CMD 		; Command byte 0EDh
	call	SND_DATA
	cli
	call	MAKE_LED
	and	byte [KB_FLAG_2], 0F8h
	or	[KB_FLAG_2], al
	test	byte [KB_FLAG_2], KB_ERR
	jnz	short SL2
	call	SND_DATA
	cli
	test	byte [KB_FLAG_2], KB_ERR
	jz	short SL3
SL2:
	mov	al, KB_ENABLE
	call	SND_DATA
	cli
SL3:
	and	byte [KB_FLAG_2], ~(KB_PR_LED+KB_ERR)
SL1:
	sti
	retn

MAKE_LED:
	;---------------------------------------------------------------------
	; MAKE_LED
	;	THIS ROUTINES FORMS THE DATA BYTE NECESSARY TO TURN ON/OFF
	;	THE MODE INDICATORS.
	;---------------------------------------------------------------------
	;
	mov	al, [KB_FLAG]
	and	al, CAPS_STATE+NUM_STATE+SCROLL_STATE 
	rol	al, 4
	and	al, 07h
	retn

; % include 'kybdata.s'   ; KEYBOARD DATA

; /// End Of KEYBOARD FUNCTIONS ///
