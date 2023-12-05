; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel) - v2.0.7 - diskio.s
; ----------------------------------------------------------------------------
; Last Update: 02/12/2023 (Previous: 11/08/2022 - Kernel v2.0.5)
; ----------------------------------------------------------------------------
; Beginning: 24/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
;
; Derived from 'Retro UNIX 386 Kernel - v0.2.1.0' source code by Erdogan Tan
; diskio.inc (22/08/2015)
;
; Derived from 'IBM PC-XT-286' BIOS source code (1986) 
; ****************************************************************************
; Ref: Retro UNIX 386 v1.1 (Kernel v0.2.1.5) 'diskio' modification: 12/07/2022

; Retro UNIX 386 v1 Kernel - DISKIO.INC
; Last Modification: 22/08/2015
; 	(Initialized Disk Parameters Data is in 'DISKDATA.INC') 
; 	(Uninitialized Disk Parameters Data is in 'DISKBSS.INC') 

; DISK I/O SYSTEM - Erdogan Tan (Retro UNIX 386 v1 project)

; ///////// DISK I/O SYSTEM ///////////////

; 11/04/2021
;; 06/02/2015
;diskette_io:
	;clc ; 20/07/2020
	;pushfd
	;push 	cs
	;;call 	DISKETTE_IO_1
	;;retn
	
;;;;;; DISKETTE I/O ;;;;;;;;;;;;;;;;;;;; 06/02/2015 ;;;
;//////////////////////////////////////////////////////

; DISKETTE I/O - Erdogan Tan (Retro UNIX 386 v1 project)
; 20/02/2015
; 06/02/2015 (unix386.s)
; 16/12/2014 - 02/01/2015 (dsectrm2.s)
;
; Code (DELAY) modifications - AWARD BIOS 1999 (ADISK.EQU, COMMON.MAC)
;
; ADISK.EQU

;----- Wait control constants 

;amount of time to wait while RESET is active.

WAITCPU_RESET_ON	EQU	21		;Reset on must last at least 14us
						;at 250 KBS xfer rate.
						;see INTEL MCS, 1985, pg. 5-456

WAITCPU_FOR_STATUS	EQU	100		;allow 30 microseconds for
						;status register to become valid
						;before re-reading.

;After sending a byte to NEC, status register may remain
;incorrectly set for 24 us.

WAITCPU_RQM_LOW		EQU	24		;number of loops to check for
						;RQM low.

; COMMON.MAC
;
;	Timing macros
;

%macro 		SIODELAY 0 			; SHORT IODELAY
		jmp short $+2
%endmacro		

%macro		IODELAY  0			; NORMAL IODELAY
		jmp short $+2
		jmp short $+2
%endmacro

%macro		NEWIODELAY 0
		out 0EBh, al
%endmacro 

; (According to) AWARD BIOS 1999 - ATORGS.ASM (dw -> equ, db -> equ)
;;; WAIT_FOR_MEM
;WAIT_FDU_INT_LO	equ	017798		; 2.5 secs in 30 micro units.
;WAIT_FDU_INT_HI	equ	1
WAIT_FDU_INT_LH		equ	83334		; 27/02/2015 (2.5 seconds waiting)
;;; WAIT_FOR_PORT
;WAIT_FDU_SEND_LO	equ	16667		; .5 secons in 30 us units.
;WAIT_FDU_SEND_HI	equ	0
WAIT_FDU_SEND_LH	equ 	16667		; 27/02/2015	
;Time to wait while waiting for each byte of NEC results = .5
;seconds.  .5 seconds = 500,000 micros.  500,000/30 = 16,667.
;WAIT_FDU_RESULTS_LO	equ	16667		; .5 seconds in 30 micro units.
;WAIT_FDU_RESULTS_HI	equ	0
WAIT_FDU_RESULTS_LH	equ	16667  ; 27/02/2015
;;; WAIT_REFRESH
;amount of time to wait for head settle, per unit in parameter
;table = 1 ms.
WAIT_FDU_HEAD_SETTLE	equ	33		; 1 ms in 30 micro units.


; //////////////// DISKETTE I/O ////////////////

; 11/12/2014 (copy from IBM PC-XT Model 286 BIOS - POSTEQU.INC)

;----------------------------------------
;	EQUATES USED BY POST AND BIOS	:
;----------------------------------------

;--------- 8042 KEYBOARD INTERFACE AND DIAGNOSTIC CONTROL REGISTERS ------------
;PORT_A		EQU	060H		; 8042 KEYBOARD SCAN CODE/CONTROL PORT
;PORT_B		EQU	061H		; PORT B READ/WRITE DIAGNOSTIC REGISTER
;REFRESH_BIT	EQU	00010000B	; REFRESH TEST BIT

;----------------------------------------
;	CMOS EQUATES FOR THIS SYSTEM	:
;-------------------------------------------------------------------------------
;CMOS_PORT	EQU	070H		; I/O ADDRESS OF CMOS ADDRESS PORT
;CMOS_DATA	EQU	071H		; I/O ADDRESS OF CMOS DATA PORT
;NMI		EQU	10000000B	; DISABLE NMI INTERRUPTS MASK -
					;  HIGH BIT OF CMOS LOCATION ADDRESS

;---------- CMOS TABLE LOCATION ADDRESS'S ## -----------------------------------
CMOS_DISKETTE	EQU	010H		; DISKETTE DRIVE TYPE BYTE	      ;
;		EQU	011H		; - RESERVED			      ;C
CMOS_DISK	EQU	012H		; FIXED DISK TYPE BYTE		      ;H
;		EQU	013H		; - RESERVED			      ;E
CMOS_EQUIP	EQU	014H		; EQUIPMENT WORD LOW BYTE	      ;C

;---------- DISKETTE EQUATES ---------------------------------------------------
INT_FLAG	EQU	10000000B	; INTERRUPT OCCURRENCE FLAG
DSK_CHG 	EQU	10000000B	; DISKETTE CHANGE FLAG MASK BIT
DETERMINED	EQU	00010000B	; SET STATE DETERMINED IN STATE BITS
HOME		EQU	00010000B	; TRACK 0 MASK
SENSE_DRV_ST	EQU	00000100B	; SENSE DRIVE STATUS COMMAND
TRK_SLAP	EQU	030H		; CRASH STOP (48 TPI DRIVES)
QUIET_SEEK	EQU	00AH		; SEEK TO TRACK 10
;MAX_DRV 	EQU	2		; MAX NUMBER OF DRIVES
HD12_SETTLE	EQU	15		; 1.2 M HEAD SETTLE TIME
HD320_SETTLE	EQU	20		; 320 K HEAD SETTLE TIME
MOTOR_WAIT	EQU	37		; 2 SECONDS OF COUNTS FOR MOTOR TURN OFF

;---------- DISKETTE ERRORS ----------------------------------------------------
;TIME_OUT	EQU	080H		; ATTACHMENT FAILED TO RESPOND
;BAD_SEEK	EQU	040H		; SEEK OPERATION FAILED
BAD_NEC 	EQU	020H		; DISKETTE CONTROLLER HAS FAILED
BAD_CRC 	EQU	010H		; BAD CRC ON DISKETTE READ
MED_NOT_FND	EQU	00CH		; MEDIA TYPE NOT FOUND
DMA_BOUNDARY	EQU	009H		; ATTEMPT TO DMA ACROSS 64K BOUNDARY
BAD_DMA 	EQU	008H		; DMA OVERRUN ON OPERATION
MEDIA_CHANGE	EQU	006H		; MEDIA REMOVED ON DUAL ATTACH CARD
RECORD_NOT_FND	EQU	004H		; REQUESTED SECTOR NOT FOUND
WRITE_PROTECT	EQU	003H		; WRITE ATTEMPTED ON WRITE PROTECT DISK
BAD_ADDR_MARK	EQU	002H		; ADDRESS MARK NOT FOUND
BAD_CMD 	EQU	001H		; BAD COMMAND PASSED TO DISKETTE I/O

;---------- DISK CHANGE LINE EQUATES -------------------------------------------
NOCHGLN 	EQU	001H		; NO DISK CHANGE LINE AVAILABLE
CHGLN		EQU	002H		; DISK CHANGE LINE AVAILABLE

;---------- MEDIA/DRIVE STATE INDICATORS ---------------------------------------
TRK_CAPA	EQU	00000001B	; 80 TRACK CAPABILITY
FMT_CAPA	EQU	00000010B	; MULTIPLE FORMAT CAPABILITY (1.2M)
DRV_DET 	EQU	00000100B	; DRIVE DETERMINED
MED_DET 	EQU	00010000B	; MEDIA DETERMINED BIT
DBL_STEP	EQU	00100000B	; DOUBLE STEP BIT
RATE_MSK	EQU	11000000B	; MASK FOR CLEARING ALL BUT RATE
RATE_500	EQU	00000000B	; 500 KBS DATA RATE
RATE_300	EQU	01000000B	; 300 KBS DATA RATE
RATE_250	EQU	10000000B	; 250 KBS DATA RATE
STRT_MSK	EQU	00001100B	; OPERATION START RATE MASK
SEND_MSK	EQU	11000000B	; MASK FOR SEND RATE BITS

;---------- MEDIA/DRIVE STATE INDICATORS COMPATIBILITY -------------------------
M3D3U		EQU	00000000B	; 360 MEDIA/DRIVE NOT ESTABLISHED
M3D1U		EQU	00000001B	; 360 MEDIA,1.2DRIVE NOT ESTABLISHED
M1D1U		EQU	00000010B	; 1.2 MEDIA/DRIVE NOT ESTABLISHED
MED_UNK 	EQU	00000111B	; NONE OF THE ABOVE

;---------- INTERRUPT EQUATES --------------------------------------------------
;EOI		EQU	020H		; END OF INTERRUPT COMMAND TO 8259
;INTA00		EQU	020H		; 8259 PORT
INTA01		EQU	021H		; 8259 PORT
INTB00		EQU	0A0H		; 2ND 8259
INTB01		EQU	0A1H		;

;-------------------------------------------------------------------------------
DMA08		EQU	008H		; DMA STATUS REGISTER PORT ADDRESS
DMA		EQU	000H		; DMA CH.0 ADDRESS REGISTER PORT ADDRESS
DMA18		EQU	0D0H		; 2ND DMA STATUS PORT ADDRESS
DMA1		EQU	0C0H		; 2ND DMA CH.0 ADDRESS REGISTER ADDRESS
;-------------------------------------------------------------------------------
;TIMER		EQU	040H		; 8254 TIMER - BASE ADDRESS

;-------------------------------------------------------------------------------
DMA_PAGE	EQU	081H		; START OF DMA PAGE REGISTERS

; 06/02/2015 (unix386.s, protected mode modifications)
; (unix386.s <-- dsectrm2.s)
; 11/12/2014 (copy from IBM PC-XT Model 286 BIOS - DSEG.INC)

; 27/05/2016 - TRDOS 386 (TRDOS v2.0)
; 10/12/2014
;
;int40h:
;	pushf
;	push 	cs
;	;cli
;	call 	DISKETTE_IO_1
;	retn

; DSKETTE ----- 04/21/86 DISKETTE BIOS
; (IBM PC XT Model 286 System BIOS Source Code, 04-21-86)
;

;-- INT13H ---------------------------------------------------------------------
; DISKETTE I/O
;	THIS INTERFACE PROVIDES ACCESS TO THE 5 1/4 INCH 360 KB,
;	1.2 MB, 720 KB AND 1.44 MB DISKETTE DRIVES.
; INPUT
;	(AH) =  00H RESET DISKETTE SYSTEM
;		HARD RESET TO NEC, PREPARE COMMAND, RECALIBRATE REQUIRED
;		ON ALL DRIVES
;------------------------------------------------------------------------------- 
;	(AH)= 01H  READ THE STATUS OF THE SYSTEM INTO (AH)
;		@DISKETTE_STATUS FROM LAST OPERATION IS USED
;-------------------------------------------------------------------------------
;	REGISTERS FOR READ/WRITE/VERIFY/FORMAT
;	(DL) - DRIVE NUMBER (0-1 ALLOWED, VALUE CHECKED)
;	(DH) - HEAD NUMBER (0-1 ALLOWED, NOT VALUE CHECKED)
;	(CH) - TRACK NUMBER (NOT VALUE CHECKED)
;		MEDIA	DRIVE	TRACK NUMBER
;		320/360	320/360	    0-39
;		320/360	1.2M	    0-39
;		1.2M	1.2M	    0-79
;		720K	720K	    0-79
;		1.44M	1.44M	    0-79	
;	(CL) - 	SECTOR NUMBER (NOT VALUE CHECKED, NOT USED FOR FORMAT)
;		MEDIA	DRIVE	SECTOR NUMBER
;		320/360	320/360	     1-8/9
;		320/360	1.2M	     1-8/9
;		1.2M	1.2M	     1-15
;		720K	720K	     1-9
;		1.44M	1.44M	     1-18		
;	(AL)	NUMBER OF SECTORS (NOT VALUE CHECKED)
;		MEDIA	DRIVE	MAX NUMBER OF SECTORS
;		320/360	320/360	     8/9
;		320/360	1.2M	     8/9
;		1.2M	1.2M	     15
;		720K	720K	      9
;		1.44M	1.44M	     18
;
;	(ES:BX) - ADDRESS OF BUFFER (NOT REQUIRED FOR VERIFY)
;
;-------------------------------------------------------------------------------
;	(AH)= 02H  READ THE DESIRED SECTORS INTO MEMORY
;-------------------------------------------------------------------------------
;	(AH)= 03H  WRITE THE DESIRED SECTORS FROM MEMORY
;-------------------------------------------------------------------------------
;	(AH)= 04H  VERIFY THE DESIRED SECTORS
;-------------------------------------------------------------------------------
;	(AH)= 05H  FORMAT THE DESIRED TRACK
;		(ES,BX) MUST POINT TO THE COLLECTION OF DESIRED ADDRESS FIELDS
;		FOR THE	TRACK. EACH FIELD IS COMPOSED OF 4 BYTES, (C,H,R,N),
;		WHERE C = TRACK NUMBER, H=HEAD NUMBER, R = SECTOR NUMBER, 
;		N= NUMBER OF BYTES PER SECTOR (00=128,01=256,02=512,03=1024),
;		THERE MUST BE ONE ENTRY FOR EVERY SECTOR ON THE TRACK.
;		THIS INFORMATION IS USED TO FIND THE REQUESTED SECTOR DURING 
;		READ/WRITE ACCESS.
;		PRIOR TO FORMATTING A DISKETTE, IF THERE EXISTS MORE THAN
;		ONE SUPPORTED MEDIA FORMAT TYPE WITHIN THE DRIVE IN QUESTION,
;		THEN "SET DASD TYPE" (INT 13H, AH = 17H) OR 'SET MEDIA TYPE'
;		(INT 13H, AH =  18H) MUST BE CALLED TO SET THE DISKETTE TYPE
;		THAT IS TO BE FORMATTED. IF "SET DASD TYPE" OR "SET MEDIA TYPE"
;		IS NOT CALLED, THE FORMAT ROUTINE WILL ASSUME THE 
;		MEDIA FORMAT TO BE THE MAXIMUM CAPACITY OF THE DRIVE.
;
;		THESE PARAMETERS OF DISK BASE MUST BE CHANGED IN ORDER TO
;		FORMAT THE FOLLOWING MEDIAS:
;		---------------------------------------------
;		: MEDIA  :     DRIVE      : PARM 1 : PARM 2 :
;		---------------------------------------------
;		: 320K	 : 320K/360K/1.2M :  50H   :   8    :
;		: 360K	 : 320K/360K/1.2M :  50H   :   9    :
;		: 1.2M	 : 1.2M           :  54H   :  15    :
;		: 720K	 : 720K/1.44M     :  50H   :   9    :
;		: 1.44M	 : 1.44M          :  6CH   :  18    :		  	
;		---------------------------------------------
;		NOTES: - PARM 1 = GAP LENGTH FOR FORMAT
;		       - PARM 2 = EOT (LAST SECTOR ON TRACK)
;		       - DISK BASE IS POINTED BY DISK POINTER LOCATED
;			 AT ABSOLUTE ADDRESS 0:78.
;		       - WHEN FORMAT OPERATIONS ARE COMPLETE, THE PARAMETERS
;			 SHOULD BE RESTORED TO THEIR RESPECTIVE INITIAL VALUES.			
;-------------------------------------------------------------------------------
;	(AH) = 08H READ DRIVE PARAMETERS
;	REGISTERS
;	  INPUT
;	    (DL) - DRIVE NUMBER (0-1 ALLOWED, VALUE CHECKED)
;	     ** 27/05/2016 - TRDOS 386 (TRDOS v2.0) **	
;            ** EBX = Buffer address for floppy disk parameters table **
;	  OUTPUT
;	    (ES:DI) POINTS TO DRIVE PARAMETER TABLE
; 	    *** TRDOS 386 note: floppy disk parameter table (16 bytes)
;	    will be returned to user in EBX, buffer address *** 27/05/2016 ***		
;					
;	    (CH) - LOW ORDER 8 OF 10 BITS MAXIMUM NUMBER OF TRACKS
;	    (CL) - BITS 7 & 6 - HIGH ORDER TWO BITS OF MAXIMUM TRACKS
;	           BITS 5 THRU 0 - MAXIMUM SECTORS PER TRACK
;	    (DH) - MAXIMUM HEAD NUMBER
;	    (DL) - NUMBER OF DISKETTE DRIVES INSTALLED
;	    (BH) - 0
;	    (BL) - BITS 7 THRU 4 - 0
;	           BITS 3 THRU 0 - VALID DRIVE TYPE VALUE IN CMOS
;	    (AX) - 0
;	 UNDER THE FOLLOWING CIRCUMSTANCES:
;	    (1) THE DRIVE NUMBER IS INVALID,
;	    (2) THE DRIVE TYPE IS UNKNOWN AND CMOS IS NOT PRESENT, 
;	    (3) THE DRIVE TYPE IS UNKNOWN AND CMOS IS BAD,
;	    (4) OR THE DRIVE TYPE IS UNKNOWN AND THE CMOS DRIVE TYPE IS INVALID
;	    THEN ES,AX,BX,CX,DH,DI=0 ; DL=NUMBER OF DRIVES. 
;	    IF NO DRIVES ARE PRESENT THEN: ES,AX,BX,CX,DX,DI=0.
;	    @DISKETTE_STATUS = 0 AND CY IS RESET.
;-------------------------------------------------------------------------------
;	(AH)= 15H  READ DASD TYPE
;	OUTPUT REGISTERS
;	(AH) - ON RETURN IF CARRY FLAG NOT SET, OTHERWISE ERROR	
;		00 - DRIVE NOT PRESENT	
;		01 - DISKETTE, NO CHANGE LINE AVAILABLE
;		02 - DISKETTE, CHANGE LINE AVAILABLE	
;		03 - RESERVED (FIXED DISK)
;	(DL) - DRIVE NUMBER (0-1 ALLOWED, VALUE CHECKED)
;-------------------------------------------------------------------------------
;	(AH)= 16H  DISK CHANGE LINE STATUS
;	OUTPUT REGISTERS
;	(AH) - 00 - DISK CHANGE LINE NOT ACTIVE	
;	       06 - DISK CHANGE LINE ACTIVE & CARRY BIT ON
;	(DL) - DRIVE NUMBER (0-1 ALLOWED, VALUE CHECKED)
;-------------------------------------------------------------------------------
;	(AH)= 17H  SET DASD TYPE FOR FORMAT
;	INPUT REGISTERS
;	(AL) -	00 - NOT USED	
;		01 - DISKETTE 320/360K IN 360K DRIVE	
;		02 - DISKETTE 360K IN 1.2M DRIVE
;		03 - DISKETTE 1.2M IN 1.2M DRIVE
;		04 - DISKETTE 720K IN 720K DRIVE
;	(DL) - DRIVE NUMBER (0-1 ALLOWED, VALUE CHECKED:
;	       (DO NOT USE WHEN DISKETTE ATTACH CARD USED)
;-------------------------------------------------------------------------------
;	(AH)= 18H  SET MEDIA TYPE FOR FORMAT
;	INPUT REGISTERS
;	(CH) - LOW ORDER 8 OF 10 BITS MAXIMUM TRACKS
;	(CL) - BITS 7 & 6 - HIGH ORDER TWO BITS OF MAXIMUM TRACKS
;	       BITS 5 THRU 0 - MAXIMUM SECTORS PER TRACK
;	(DL) - DRIVE NUMBER (0-1 ALLOWED, VALUE CHACKED)
;	OUTPUT REGISTERS:
;	(ES:DI) - POINTER TO DRIVE PARAMETERS TABLE FOR THIS MEDIA TYPE,
;		  UNCHANGED IF (AH) IS NON-ZERO
;	(AH) - 00H, CY = 0, TRACK AND SECTORS/TRACK COMBINATION IS SUPPORTED
;	     - 01H, CY = 1, FUNCTION IS NOT AVAILABLE
;	     - 0CH, CY = 1, TRACK AND SECTORS/TRACK COMBINATION IS NOT SUPPORTED
;	     - 80H, CY = 1, TIME OUT (DISKETTE NOT PRESENT)		
;-------------------------------------------------------------------------------
;	DISK CHANGE STATUS IS ONLY CHECKED WHEN A MEDIA SPECIFIED IS OTHER
;	THAN 360 KB DRIVE. IF THE DISK CHANGE LINE IS FOUND TO BE
;	ACTIVE THE FOLLOWING ACTIONS TAKE PLACE:
;		ATTEMPT TO RESET DISK CHANGE LINE TO INACTIVE STATE. 
;		IF ATTEMPT SUCCEEDS SET DASD TYPE FOR FORMAT AND RETURN DISK 
;		CHANGE ERROR CODE
;		IF ATTEMPT FAILS RETURN TIMEOUT ERROR CODE AND SET DASD TYPE 
;		TO A PREDETERMINED STATE INDICATING MEDIA TYPE UNKNOWN.
;	IF THE DISK CHANGE LINE IN INACTIVE PERFORM SET DASD TYPE FOR FORMAT.
;
; DATA VARIABLE -- @DISK_POINTER
;	DOUBLE WORD POINTER TO THE CURRENT SET OF DISKETTE PARAMETERS
;-------------------------------------------------------------------------------
; OUTPUT FOR ALL FUNCTIONS
;	AH = STATUS OF OPERATION
;		STATUS BITS ARE DEFINED IN THE EQUATES FOR @DISKETTE_STATUS
;		VARIABLE IN THE DATA SEGMENT OF THIS MODULE
;	CY = 0	SUCCESSFUL OPERATION (AH=0 ON RETURN, EXCEPT FOR READ DASD
;		TYPE AH=(15)).
;	CY = 1	FAILED OPERATION (AH HAS ERROR REASON)
;	FOR READ/WRITE/VERIFY
;		DS,BX,DX,CX PRESERVED
;	NOTE: IF AN ERROR IS REPORTED BY THE DISKETTE CODE, THE APPROPRIATE 
;		ACTION IS TO RESET THE DISKETTE, THEN RETRY THE OPERATION.
;		ON READ ACCESSES, NO MOTOR START DELAY IS TAKEN, SO THAT 
;		THREE RETRIES ARE REQUIRED ON READS TO ENSURE THAT THE 
;		PROBLEM IS NOT DUE TO MOTOR START-UP.
;-------------------------------------------------------------------------------
;
; DISKETTE STATE MACHINE - ABSOLUTE ADDRESS 40:90 (DRIVE A) & 91 (DRIVE B)
;
;   -----------------------------------------------------------------
;   |       |       |       |       |       |       |       |       |
;   |   7   |   6   |   5   |   4   |   3   |   2   |   1   |   0   |
;   |       |       |       |       |       |       |       |       |
;   -----------------------------------------------------------------
;	|	|	|	|	|	|	|	|
;	|	|	|	|	|	-----------------
;	|	|	|	|	|		|
;	|	|	|	|    RESERVED		|
;	|	|	|	|		  PRESENT STATE
;	|	|	|	|	000: 360K IN 360K DRIVE UNESTABLISHED
;	|	|	|	|	001: 360K IN 1.2M DRIVE UNESTABLISHED
;	|	|	|	|	010: 1.2M IN 1.2M DRIVE UNESTABLISHED
;	|	|	|	|	011: 360K IN 360K DRIVE ESTABLISHED
;	|	|	|	|	100: 360K IN 1.2M DRIVE ESTABLISHED
;	|	|	|	|	101: 1.2M IN 1.2M DRIVE ESTABLISHED
;	|	|	|	|	110: RESERVED
;	|	|	|	|	111: NONE OF THE ABOVE
;	|	|	|	|
;	|	|	|	------>	MEDIA/DRIVE ESTABLISHED
;	|	|	|
;	|	|	-------------->	DOUBLE STEPPING REQUIRED (360K IN 1.2M
;	|	|			DRIVE)
;	|	|
;	------------------------------>	DATA TRANSFER RATE FOR THIS DRIVE:
;
;						00: 500 KBS
;						01: 300 KBS
;						10: 250 KBS
;						11: RESERVED
;
;
;-------------------------------------------------------------------------------
; STATE OPERATION STARTED - ABSOLUTE ADDRESS 40:92 (DRIVE A) & 93 (DRIVE B)
;-------------------------------------------------------------------------------
; PRESENT CYLINDER NUMBER - ABSOLUTE ADDRESS 40:94 (DRIVE A) & 95 (DRIVE B)
;-------------------------------------------------------------------------------

struc MD
	.SPEC1		resb	1	; SRT=D, HD UNLOAD=0F - 1ST SPECIFY BYTE
	.SPEC2		resb	1	; HD LOAD=1, MODE=DMA - 2ND SPECIFY BYTE
	.OFF_TIM	resb	1	; WAIT TIME AFTER OPERATION TILL MOTOR OFF
	.BYT_SEC	resb	1	; 512 BYTES/SECTOR
	.SEC_TRK	resb	1	; EOT (LAST SECTOR ON TRACK)
	.GAP		resb	1	; GAP LENGTH
	.DTL		resb	1	; DTL
	.GAP3		resb	1	; GAP LENGTH FOR FORMAT
	.FIL_BYT	resb	1	; FILL BYTE FOR FORMAT
	.HD_TIM		resb	1	; HEAD SETTLE TIME (MILLISECONDS)
	.STR_TIM	resb	1	; MOTOR START TIME (1/8 SECONDS)
	.MAX_TRK	resb	1	; MAX. TRACK NUMBER
	.RATE		resb	1	; DATA TRANSFER RATE
endstruc

BIT7OFF	EQU	7FH
BIT7ON	EQU	80H

; 30/08/2020 - TRDOS 386 v2

;;int13h: ; 16/02/2015
;; 16/02/2015 - 21/02/2015
; 17/07/2022 - TRDOS 386 v2.0.5
;int40h:
;; 11/04/2021
;diskette_io:
;	clc ; 20/07/2020
;	pushfd
;	push 	cs
;	call 	DISKETTE_IO_1
;	retn

	; 09/08/2022
	; 06/08/2022
	; 17/07/2022 - TRDOS 386 v2.0.5
	; (jump from DISK_IO)
DISKETTE_IO_1:
	;sti ; 17/07/2022		; INTERRUPTS BACK ON
	
	;push	ebp			; USER REGISTER
	;push	edi			; USER REGISTER
	;push	edx			; HEAD #, DRIVE # OR USER REGISTER
	;push	ebx			; BUFFER OFFSET PARAMETER OR REGISTER
	;push	ecx			; TRACK #-SECTOR # OR USER REGISTER
	;mov	ebp, esp		; BP     => PARAMETER LIST DEP. ON AH
					; [BP]   = SECTOR #
					; [BP+1] = TRACK #
					; [BP+2] = BUFFER OFFSET
					; FOR RETURN OF DRIVE PARAMETERS:
					; CL/[BP] = BITS 7&6 HI BITS OF MAX CYL
					; 	    BITS 0-5 MAX SECTORS/TRACK
					; CH/[BP+1] = LOW 8 BITS OF MAX CYL.
					; BL/[BP+2] = BITS 7-4 = 0
					;	      BITS 3-0 = VALID CMOS TYPE
					; BH/[BP+3] = 0
					; DL/[BP+4] = # DRIVES INSTALLED
					; DH/[BP+5] = MAX HEAD #
					; DI/[BP+6] = OFFSET TO DISK BASE
	;push	es ; 06/02/2015	
	;push	ds			; BUFFER SEGMENT PARM OR USER REGISTER
	;push	esi			; USER REGISTERS
	;;call	DDS			; SEGMENT OF BIOS DATA AREA TO DS
	;;mov	cx, cs
	;;mov	ds, cx
	;mov	cx, KDATA
	;mov	ds, cx
	;mov	es, cx

	; 17/07/2022
	; Registers are also on stack 
	; (with same contents) 
	; in following order:
	;
	;    ebx = esp+20
	;    ecx = esp+16
	;    edx = esp+12
	;    esi = esp+8
	;    edi = esp+4
	;
	; [esp] = caller's return address (from 'DISK_IO')	
	;
	; cs = KCODE == KDATA
	; ds = es = ss = KDATA

	; 17/07/2022
	push	ebp
	mov	ebp, ebx

	;cmp	ah, (FNC_TAE-FNC_TAB)/2	; CHECK FOR > LARGEST FUNCTION
	cmp	ah, (FNC_TAE-FNC_TAB)/4	; 18/02/2015
	;jb	short OK_FUNC		; FUNCTION OK
	;mov	ah, 14h			; REPLACE WITH KNOWN INVALID FUNCTION
	; 09/08/2022
	jnb	short INV_FUNC
OK_FUNC:
	cmp	ah, 1			; RESET OR STATUS ?
	jbe	short OK_DRV		; IF RESET OR STATUS DRIVE ALWAYS OK
	cmp	ah, 8			; READ DRIVE PARMS ?
	je	short OK_DRV		; IF SO DRIVE CHECKED LATER
	cmp	dl, 1			; DRIVES 0 AND 1 OK
	jbe	short OK_DRV		; IF 0 OR 1 THEN JUMP
INV_FUNC:
	mov	ah, 14h			; REPLACE WITH KNOWN INVALID FUNCTION
OK_DRV:
	; 17/07/2022
	;xor	ecx, ecx
	;;mov	esi, ecx ; 08/02/2015
	;mov	edi, ecx ; 08/02/2015
	;mov	cl, ah			; CL = FUNCTION
	;;xor	ch, ch			; CX = FUNCTION
	;;shl	cl, 1			; FUNCTION TIMES 2
	;shl	cl, 2 ; 20/02/2015	; FUNCTION TIMES 4 (for 32 bit offset)
	;mov	ebx, FNC_TAB		; LOAD START OF FUNCTION TABLE
	;add	ebx, ecx		; ADD OFFSET INTO TABLE => ROUTINE

	; 17/07/2022	
	sub	ebx, ebx
	mov	bl, ah			; BL = FUNCTION	
	shl	bl, 2 ; * 4
	add	ebx, FNC_TAB		; [EBX] = FUNCTION ADDRESS
	
	mov	ah, dh			; AX = HEAD #,# OF SECTORS OR DASD TYPE
	;xor	dh, dh			; DX = DRIVE #
	;mov	si, ax			; SI = HEAD #,# OF SECTORS OR DASD TYPE
	;mov	di, dx			; DI = DRIVE #
	
	movzx	esi, ax			; ESI = HEAD #,# OF SECTORS OR DASD TYPE
	movzx	edi, dl			; EDI = DRIVE # 

	; CH = cylinder number (low 8 bit)
	; CL = sector number (and high 2 bits of cylinder number)

	; 06/08/2022
	; 11/12/2014
        ;mov	[cfd], dl               ; current floppy drive (for 'GET_PARM')        
	; 06/08/2022
	; EDI = (current) DRIVE #
	;
	mov	ah, [DSKETTE_STATUS]	; LOAD STATUS TO AH FOR STATUS FUNCTION
	mov	byte [DSKETTE_STATUS], 0 ; INITIALIZE FOR ALL OTHERS

;	THROUGHOUT THE DISKETTE BIOS, THE FOLLOWING INFORMATION IS CONTAINED IN
;	THE FOLLOWING MEMORY LOCATIONS AND REGISTERS. NOT ALL DISKETTE BIOS
;	FUNCTIONS REQUIRE ALL OF THESE PARAMETERS.
;
;		DI	: DRIVE #
;		SI-HI	: HEAD #
;		SI-LOW	: # OF SECTORS OR DASD TYPE FOR FORMAT
;		ES	: BUFFER SEGMENT
;		[BP]	: SECTOR #
;		[BP+1]	: TRACK #
;		[BP+2]	: BUFFER OFFSET
;
;	ACROSS CALLS TO SUBROUTINES THE CARRY FLAG (CY=1), WHERE INDICATED IN 
;	SUBROUTINE PROLOGUES, REPRESENTS AN EXCEPTION RETURN (NORMALLY AN ERROR 
;	CONDITION). IN MOST CASES, WHEN CY = 1, @DSKETTE_STATUS CONTAINS THE 
;	SPECIFIC ERROR CODE.

	; 17/07/2022
	; EBX = pointer to function address
	; EBP = buffer address
	; EDI = drive number (0 or 1)
	; ESI = head number (byte 1) and sector count or disk type (byte 0)
	; CH = cylinder number (low 8 bit)
	; CL = sector number (and high 2 bits of cylinder number)

					; (AH) = @DSKETTE_STATUS
	call	dword [ebx]		; CALL THE REQUESTED FUNCTION
	
	;pop	esi			; RESTORE ALL REGISTERS
	;pop	ds
	;pop	es	; 06/02/2015
	;pop	ecx
	;pop	ebx
	;pop	edx
	;pop	edi
	;mov	ebp, esp
	;push	eax
	;pushfd
	;pop	eax
	;;mov	[bp+6], ax
	;mov	[ebp+12], eax  ; 18/02/2015, flags
	;pop	eax
	;pop	ebp
	;iretd

	; 17/07/2022
	pop	ebp

	; 17/07/2022
	; Stack order:
	;    ebx = esp+20
	;    ecx = esp+16
	;    edx = esp+12
	;    esi = esp+8
	;    edi = esp+4
	;
	; [esp] = caller's return address (from 'DISK_IO')

	; CF = disk i/o status (1 = error)
	; AH = error code (if > 0 and cf = 1)

	; 17/07/2022
	retn	; return to the caller of 'DISK_IO' 

;-------------------------------------------------------------------------------
; DW --> dd (06/02/2015)
FNC_TAB	dd	DSK_RESET		; AH = 00H; RESET
	dd	DSK_STATUS		; AH = 01H; STATUS
	dd	DSK_READ		; AH = 02H; READ
	dd	DSK_WRITE		; AH = 03H; WRITE
	dd	DSK_VERF		; AH = 04H; VERIFY
	dd	DSK_FORMAT		; AH = 05H; FORMAT
	dd	FNC_ERR			; AH = 06H; INVALID
	dd	FNC_ERR			; AH = 07H; INVALID
	dd	DSK_PARMS		; AH = 08H; READ DRIVE PARAMETERS
	dd	FNC_ERR			; AH = 09H; INVALID
	dd	FNC_ERR			; AH = 0AH; INVALID
	dd	FNC_ERR			; AH = 0BH; INVALID
	dd	FNC_ERR			; AH = 0CH; INVALID
	dd	FNC_ERR			; AH = 0DH; INVALID
	dd	FNC_ERR			; AH = 0EH; INVALID
	dd	FNC_ERR			; AH = 0FH; INVALID
	dd	FNC_ERR			; AH = 10H; INVALID
	dd	FNC_ERR			; AH = 11H; INVALID
	dd	FNC_ERR			; AH = 12H; INVALID
	dd	FNC_ERR			; AH = 13H; INVALID
	dd	FNC_ERR			; AH = 14H; INVALID
	dd	DSK_TYPE		; AH = 15H; READ DASD TYPE
	dd	DSK_CHANGE		; AH = 16H; CHANGE STATUS
	dd	FORMAT_SET		; AH = 17H; SET DASD TYPE
	dd	SET_MEDIA		; AH = 18H; SET MEDIA TYPE	
FNC_TAE EQU     $                       ; END

	; 17/07/2022 - TRDOS 386 v2.0.5
;-------------------------------------------------------------------------------
; DISK_RESET	(AH = 00H)	
;		RESET THE DISKETTE SYSTEM.
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------
DSK_RESET:
	; 17/07/2022
	mov	dx, 03F2h		; ADAPTER CONTROL PORT
	cli				; NO INTERRUPTS
	mov	al, [MOTOR_STATUS]	; GET DIGITAL OUTPUT REGISTER REFLECTION
	and	al, 00111111b		; KEEP SELECTED AND MOTOR ON BITS
	rol	al, 4			; MOTOR VALUE TO HIGH NIBBLE
					; DRIVE SELECT TO LOW NIBBLE
	or	al, 00001000b		; TURN ON INTERRUPT ENABLE
	out	dx, al			; RESET THE ADAPTER
	mov	byte [SEEK_STATUS], 0	; SET RECALIBRATE REQUIRED ON ALL DRIVES
	;jmp	$+2			; WAIT FOR I/O
	;jmp	$+2			; WAIT FOR I/O (TO INSURE MINIMUM
					;      PULSE WIDTH)
	; 19/12/2014
	NEWIODELAY

	; 17/12/2014 
	; AWARD BIOS 1999 - RESETDRIVES (ADISK.ASM)
	mov	ecx, WAITCPU_RESET_ON	; cx = 21 -- Min. 14 micro seconds !?
wdw1:
	NEWIODELAY   ; 27/02/2015
	loop	wdw1
	;
	or	al, 00000100b		; TURN OFF RESET BIT
	out	dx, al			; RESET THE ADAPTER
	; 16/12/2014
	IODELAY
	;
	;sti				; ENABLE THE INTERRUPTS
	call	WAIT_INT		; WAIT FOR THE INTERRUPT
	jc	short DR_ERR		; IF ERROR, RETURN IT
	;mov	cx, 11000000b		; CL = EXPECTED @NEC_STATUS
	; 17/07/2022
	;xor	ch, ch
	mov	cl, 11000000b
NXT_DRV:
	;push	cx			; SAVE FOR CALL
	; 11/04/2021
	push	ecx
	mov	eax, DR_POP_ERR 	; LOAD NEC_OUTPUT ERROR ADDRESS
	push	eax			; "
	mov	ah, 08h			; SENSE INTERRUPT STATUS COMMAND
	call	NEC_OUTPUT
	pop	eax			; THROW AWAY ERROR RETURN
	call	RESULTS			; READ IN THE RESULTS
	;pop	cx			; RESTORE AFTER CALL
	; 11/04/2021
	pop	ecx
	jc	short DR_ERR		; ERROR RETURN
	cmp	cl, [NEC_STATUS]	; TEST FOR DRIVE READY TRANSITION
	jnz	short DR_ERR		; EVERYTHING OK
	inc	cl			; NEXT EXPECTED @NEC_STATUS
	cmp	cl, 11000011b		; ALL POSSIBLE DRIVES CLEARED
	jbe	short NXT_DRV		; FALL THRU IF 11000100B OR >
	;
	call	SEND_SPEC		; SEND SPECIFY COMMAND TO NEC
RESBAC:
	; 06/08/2022
	jmp	short SETUP_END_X
	;call	SETUP_END		; VARIOUS CLEANUPS
	;;mov	bx, si			; GET SAVED AL TO BL
	;; 17/07/2022
	;mov	ebx, esi		
	;mov	al, bl			; PUT BACK FOR RETURN
	;retn		

DR_POP_ERR:
	;pop	cx			; CLEAR STACK
	; 11/04/2021
	pop	ecx
DR_ERR:
	or	byte [DSKETTE_STATUS], BAD_NEC ; SET ERROR CODE
	;jmp	short RESBAC		; RETURN FROM RESET
	; 06/08/2022
	jmp	short SETUP_END_X

;-------------------------------------------------------------------------------
; DISK_STATUS	(AH = 01H)
;	DISKETTE STATUS.
;
; ON ENTRY:	AH : STATUS OF PREVIOUS OPERATION
;
; ON EXIT:	AH, @DSKETTE_STATUS, CY REFLECT STATUS OF PREVIOUS OPERATION.
;-------------------------------------------------------------------------------
DSK_STATUS:
	mov	[DSKETTE_STATUS], ah	; PUT BACK FOR SETUP END
SETUP_END_X:	; 06/08/2022
	call	SETUP_END		; VARIOUS CLEANUPS
	;mov	bx, si			; GET SAVED AL TO BL
	;mov	al, bl			; PUT BACK FOR RETURN
	; 06/08/2022
	mov	ebx, esi
	mov	al, bl
	retn		

;-------------------------------------------------------------------------------
; DISK_READ	(AH = 02H)	
;	DISKETTE READ.
;
; ON ENTRY:	DI	: DRIVE #
;		SI-HI	: HEAD #
;		SI-LOW	: # OF SECTORS
;		ES	: BUFFER SEGMENT
;		[BP]	: SECTOR #
;		[BP+1]	: TRACK #
;		[BP+2]	: BUFFER OFFSET
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------

; 06/02/2015, ES:BX -> EBX (unix386.s)

DSK_READ:
	and	byte [MOTOR_STATUS], 01111111b ; INDICATE A READ OPERATION
	mov	ax, 0E646h		; AX = NEC COMMAND, DMA COMMAND
	;call	RD_WR_VF		; COMMON READ/WRITE/VERIFY
	;retn
	; 06/08/2022
	jmp	short RD_WR_VF

;-------------------------------------------------------------------------------
; DISK_WRITE	(AH = 03H)
;	DISKETTE WRITE.
;
; ON ENTRY:	DI	: DRIVE #
;		SI-HI	: HEAD #
;		SI-LOW	: # OF SECTORS
;		ES	: BUFFER SEGMENT
;		[BP]	: SECTOR #
;		[BP+1]	: TRACK #
;		[BP+2]	: BUFFER OFFSET
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------

; 06/02/2015, ES:BX -> EBX (unix386.s)

DSK_WRITE:
	mov	ax, 0C54Ah		; AX = NEC COMMAND, DMA COMMAND
	or	byte [MOTOR_STATUS], 10000000b ; INDICATE WRITE OPERATION
	;call	RD_WR_VF		; COMMON READ/WRITE/VERIFY
	;retn
	; 06/08/2022
	;jmp	short RD_WR_VF

	; 09/08/2022
	; 06/08/2022 - TRDOS 386 Kernel v2.0.5
;-------------------------------------------------------------------------------
; RD_WR_VF
;	COMMON READ, WRITE AND VERIFY: 
;	MAIN LOOP FOR STATE RETRIES.
;
; ON ENTRY:	AH = READ/WRITE/VERIFY NEC PARAMETER
;		AL = READ/WRITE/VERIFY DMA PARAMETER
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------
RD_WR_VF:
	; 02/12/2023
	; 11/08/2022
	; 09/08/2022
	; 07/08/2022
	; 06/08/2022
	; 11/04/2021 (32 bit push/pop, AX -> EAX)
	
	push	eax			; SAVE DMA, NEC PARAMETERS
	
	; 02/12/2023
	; (diskette change check for 'dir', 28 seconds)
	mov	al, [TIMER_LOW+1]
	shr	al, 1
	mov	[P_TIMER], al

	call	XLAT_NEW		; TRANSLATE STATE TO PRESENT ARCH.
	call	SETUP_STATE		; INITIALIZE START AND END RATE
	pop	eax			; RESTORE READ/WRITE/VERIFY
DO_AGAIN:
	push	eax			; SAVE READ/WRITE/VERIFY PARAMETER
	call	MED_CHANGE		; MEDIA CHANGE AND RESET IF CHANGED
	pop	eax			; RESTORE READ/WRITE/VERIFY
	;jc	short RWV_END		; MEDIA CHANGE ERROR OR TIME-OUT
	; 07/08/2022
	jnc	short RWV
	jmp	RWV_END
RWV:
	push	eax			; SAVE READ/WRITE/VERIFY PARAMETER
	mov	dh, [DSK_STATE+edi]	; GET RATE STATE OF THIS DRIVE
	and	dh, RATE_MSK		; KEEP ONLY RATE
	call	CMOS_TYPE		; RETURN DRIVE TYPE IN AL (AL)
	;;20/02/2015
	;;jc	short RWV_ASSUME	; ERROR IN CMOS
	jz	short RWV_ASSUME ; 20/02/2015
	cmp	al, 1			; 40 TRACK DRIVE?
	jne	short RWV_1		; NO, BYPASS CMOS VALIDITY CHECK
	test	byte [DSK_STATE+edi], TRK_CAPA ; CHECK FOR 40 TRACK DRIVE
	jz	short RWV_2		; YES, CMOS IS CORRECT
	;mov	al, 2			; CHANGE TO 1.2M
	; 06/08/2022
	inc	al ; al = 2
	jmp	short RWV_2
RWV_1:
	; 09/08/2022
	;jb	short RWV_2		; NO DRIVE SPECIFIED, CONTINUE
	test    byte [DSK_STATE+edi], TRK_CAPA ; IS IT REALLY 40 TRACK?
	jnz	short RWV_2		; NO, 80 TRACK
	mov	al, 1			; IT IS 40 TRACK, FIX CMOS VALUE
	jmp	short rwv_3
RWV_2:
	; 09/08/2022
	;or	al, al			; TEST FOR NO DRIVE
	;jz	short RWV_ASSUME	; ASSUME TYPE, USE MAX TRACK
rwv_3:
	call	DR_TYPE_CHECK		; RTN CS:BX = MEDIA/DRIVE PARAM TBL.
	jc	short RWV_ASSUME	; TYPE NOT IN TABLE (BAD CMOS)

;-----	SEARCH FOR MEDIA/DRIVE PARAMETER TABLE

	push	edi			; SAVE DRIVE #
	; 09/08/2022
	;xor	ebx, ebx		; EBX = INDEX TO DR_TYPE TABLE
	mov	ebx, DR_TYPE
	;mov	ecx, DR_CNT		; ECX = LOOP COUNT
	mov	cl, DR_CNT
RWV_DR_SEARCH:
	;mov	ah, [DR_TYPE+ebx]	; GET DRIVE TYPE
	mov	ah, [ebx]
	and	ah, BIT7OFF		; MASK OUT MSB
	cmp	al, ah			; DRIVE TYPE MATCH?
	; 09/08/2022
	;cmp	dl, ah
	jne	short RWV_NXT_MD	; NO, CHECK NEXT DRIVE TYPE
RWV_DR_FND:
	;mov	edi, [DR_TYPE+ebx+1] 	; EDI = MEDIA/DRIVE PARAMETER TABLE
	inc	ebx
	mov	edi, [ebx]
	dec	ebx
RWV_MD_SEARH:
        cmp	dh, [edi+MD.RATE]       ; MATCH?
	je	short RWV_MD_FND	; YES, GO GET 1ST SPECIFY BYTE
RWV_NXT_MD:
	add	ebx, 5			; CHECK NEXT DRIVE TYPE
	;loop	RWV_DR_SEARCH
	dec	cl
	jnz	short RWV_DR_SEARCH 
	pop	edi			; RESTORE DRIVE #

;-----	ASSUME PRIMARY DRIVE IS INSTALLED AS SHIPPED

RWV_ASSUME:
	mov	ebx, MD_TBL1		; POINT TO 40 TRACK 250 KBS
	test 	byte [DSK_STATE+edi], TRK_CAPA ; TEST FOR 80 TRACK
	jz	short RWV_MD_FND1	; MUST BE 40 TRACK
	mov	ebx, MD_TBL3		; POINT TO 80 TRACK 500 KBS
	jmp	short RWV_MD_FND1	; GO SPECIFY PARAMTERS

;-----	CS:BX POINTS TO MEDIA/DRIVE PARAMETER TABLE
	 			
RWV_MD_FND:
	mov	ebx, edi		; BX = MEDIA/DRIVE PARAMETER TABLE
	pop	edi			; RESTORE DRIVE #
	
;-----	SEND THE SPECIFY COMMAND TO THE CONTROLLER

RWV_MD_FND1:
	call	SEND_SPEC_MD
	call	CHK_LASTRATE		; ZF=1 ATTEMP RATE IS SAME AS LAST RATE
	jz	short RWV_DBL		; YES,SKIP SEND RATE COMMAND
	call	SEND_RATE		; SEND DATA RATE TO NEC
RWV_DBL:
	push	ebx			; SAVE MEDIA/DRIVE PARAM TBL ADDRESS
	call	SETUP_DBL		; CHECK FOR DOUBLE STEP
	pop	ebx			; RESTORE ADDRESS
	jc	short CHK_RET		; ERROR FROM READ ID, POSSIBLE RETRY
	;pop	eax			; RESTORE NEC COMMAND
	;push	eax			; SAVE NEC COMMAND
	; 09/08/2022
	mov	eax, [esp]
	;push	ebx			; SAVE MEDIA/DRIVE PARAM TBL ADDRESS
	call	DMA_SETUP		; SET UP THE DMA
	;pop	ebx
	pop	eax			; RESTORE NEC COMMAND
	jc	short RWV_BAC		; CHECK FOR DMA BOUNDARY ERROR
	push	eax			; SAVE NEC COMMAND
	push	ebx			; SAVE MEDIA/DRIVE PARAM TBL ADDRESS
	; 11/08/2022
	mov	ebx, [esp+32] ; ECX
	call	NEC_INIT		; INITIALIZE NEC
	pop	ebx			; RESTORE ADDRESS
	jc	short CHK_RET		; ERROR - EXIT
	call	RWV_COM			; OP CODE COMMON TO READ/WRITE
	jc	short CHK_RET		; ERROR - EXIT
	call	NEC_TERM		; TERMINATE, GET STATUS, ETC.
CHK_RET:
	call	RETRY			; CHECK FOR, SETUP RETRY
	pop	eax			; RESTORE READ/WRITE PARAMETER
	jnc	short RWV_END		; CY = 0 NO RETRY
        jmp	DO_AGAIN                ; CY = 1 MEANS RETRY
RWV_END:
	call	DSTATE			; ESTABLISH STATE IF SUCCESSFUL
	call	NUM_TRANS		; AL = NUMBER TRANSFERRED
RWV_BAC:				; BAD DMA ERROR ENTRY
	; 06/08/2022
	;push	eax			; SAVE NUMBER TRANSFERRED
	;call	XLAT_OLD		; TRANSLATE STATE TO COMPATIBLE MODE
	;pop	eax			; RESTORE NUMBER TRANSFERRED
	;call	SETUP_END		; VARIOUS CLEANUPS
	;retn

;-------------------------------------------------------------------------------
; SETUP_END
;	RESTORES @MOTOR_COUNT TO PARAMETER PROVIDED IN TABLE 
;	AND LOADS @DSKETTE_STATUS TO AH, AND SETS CY.
;
; ON EXIT:
;	AH, @DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------
SETUP_END:
	; 06/08/2022 - TRDOS 386 v2.0.5
	;mov	dl, 2			; GET THE MOTOR WAIT PARAMETER
	;push	ax			; SAVE NUMBER TRANSFERRED
	; 11/04/2021
	push	eax
	; 06/08/2022
	mov	al, 2			; GET THE MOTOR WAIT PARAMETER		
	call	GET_PARM
	mov	[MOTOR_COUNT], ah	; STORE UPON RETURN
	;pop	ax			; RESTORE NUMBER TRANSFERRED
	; 11/04/2021
	pop	eax
	mov	ah, [DSKETTE_STATUS]	; GET STATUS OF OPERATION
	or	ah, ah			; CHECK FOR ERROR
	jz	short NUN_ERR		; NO ERROR
	xor	al, al			; CLEAR NUMBER RETURNED
	; 06/08/2022
	stc
NUN_ERR: 
	;cmp	ah, 1			; SET THE CARRY FLAG TO INDICATE
	;cmc				; SUCCESS OR FAILURE
	retn

;-------------------------------------------------------------------------------
; DISK_VERF	(AH = 04H)
;	DISKETTE VERIFY.
;
; ON ENTRY:	DI	: DRIVE #
;		SI-HI	: HEAD #
;		SI-LOW	: # OF SECTORS
;		ES	: BUFFER SEGMENT
;		[BP]	: SECTOR #
;		[BP+1]	: TRACK #
;		[BP+2]	: BUFFER OFFSET
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------
DSK_VERF:
	and	byte [MOTOR_STATUS], 01111111b ; INDICATE A READ OPERATION
	mov	ax, 0E642h		; AX = NEC COMMAND, DMA COMMAND
	;call	RD_WR_VF		; COMMON READ/WRITE/VERIFY
	;retn
	; 06/08/2022
	jmp	RD_WR_VF

;-------------------------------------------------------------------------------
; DISK_FORMAT	(AH = 05H)
;	DISKETTE FORMAT.
;
; ON ENTRY:	DI	: DRIVE #
;		SI-HI	: HEAD #
;		SI-LOW	: # OF SECTORS
;		ES	: BUFFER SEGMENT
;		[BP]	: SECTOR #
;		[BP+1]	: TRACK #
;		[BP+2]	: BUFFER OFFSET
;		@DISK_POINTER POINTS TO THE PARAMETER TABLE OF THIS DRIVE
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------
DSK_FORMAT:
	; 11/08/2022
	; 06/08/2022 - TRDOS 386 v2.0.5
	call	XLAT_NEW		; TRANSLATE STATE TO PRESENT ARCH.
	call	FMT_INIT		; ESTABLISH STATE IF UNESTABLISHED
	or	byte [MOTOR_STATUS], 10000000b ; INDICATE WRITE OPERATION
	call	MED_CHANGE		; CHECK MEDIA CHANGE AND RESET IF SO
        jc      short FM_DON            ; MEDIA CHANGED, SKIP
	call	SEND_SPEC		; SEND SPECIFY COMMAND TO NEC
	call	CHK_LASTRATE		; ZF=1 ATTEMPT RATE IS SAME AS LAST RATE
        jz      short FM_WR             ; YES, SKIP SPECIFY COMMAND
	call	SEND_RATE		; SEND DATA RATE TO CONTROLLER
FM_WR:
	call	FMTDMA_SET		; SET UP THE DMA FOR FORMAT
        jc      short FM_DON            ; RETURN WITH ERROR
	mov	ah, 04Dh		; ESTABLISH THE FORMAT COMMAND
	; 11/08/2022
	mov	ebx, [esp+24] ; ECX
	call	NEC_INIT		; INITIALIZE THE NEC
	jc	short FM_DON            ; ERROR - EXIT
        mov     eax, FM_DON             ; LOAD ERROR ADDRESS
	push	eax			; PUSH NEC_OUT ERROR RETURN
	;mov	dl, 3			; BYTES/SECTOR VALUE TO NEC
	; 06/08/2022
	mov	al, 3
	call	GET_PARM
	call	NEC_OUTPUT
	;mov	dl, 4			; SECTORS/TRACK VALUE TO NEC
	; 06/08/2022
	mov	al, 4
	call	GET_PARM
	call	NEC_OUTPUT
	;mov	dl, 7			; GAP LENGTH VALUE TO NEC
	; 06/08/2022
	mov	al, 7
	call	GET_PARM
	call	NEC_OUTPUT
	;mov	dl, 8			; FILLER BYTE TO NEC
	; 06/08/2022
	mov	al, 8
	call	GET_PARM
	call	NEC_OUTPUT
	pop	eax			; THROW AWAY ERROR
	call	NEC_TERM		; TERMINATE, RECEIVE STATUS, ETC,
FM_DON:
	; 06/08/2022
	;call	XLAT_OLD		; TRANSLATE STATE TO COMPATIBLE MODE
	; 06/08/2022
	jmp	SETUP_END_X
	;call	SETUP_END		; VARIOUS CLEANUPS
	;; 06/08/2022
	;mov	ebx, esi		; GET SAVED AL TO BL
	;mov	al, bl			; PUT BACK FOR RETURN
	;retn

;-------------------------------------------------------------------------------
; FNC_ERR
;	INVALID FUNCTION REQUESTED OR INVALID DRIVE: 
;	SET BAD COMMAND IN STATUS.
;
; ON EXIT: 	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------
FNC_ERR:				; INVALID FUNCTION REQUEST
	;mov	ax, si
	; 06/08/2022
	mov	eax, esi		; RESTORE AL
	mov	ah, BAD_CMD		; SET BAD COMMAND ERROR
	mov	[DSKETTE_STATUS], ah	; STORE IN DATA AREA
	stc				; SET CARRY INDICATING ERROR
	retn

; 06/08/2022 - TRDOS 386 Kernel v2.0.5
; 30/08/2020
; 29/08/2020
; 01/06/2016
; 28/05/2016
; 27/05/2016 - TRDOS 386 (TRDOS v.2.0)
;-------------------------------------------------------------------------------
; DISK_PARMS	(AH = 08H)	
;	READ DRIVE PARAMETERS.
;
; ON ENTRY:	DI : DRIVE #
;		; 27/05/2016
;		EBX = Buffer Address for floppy disk parameters table (16 bytes)
;
; ON EXIT:	CL/[BP]   = BITS 7 & 6 HI 2 BITS OF MAX CYLINDER
;		            BITS 0-5 MAX SECTORS/TRACK
;		CH/[BP+1] = LOW 8 BITS OF MAX CYLINDER
;		BL/[BP+2] = BITS 7-4 = 0
;		            BITS 3-0 = VALID CMOS DRIVE TYPE
;		BH/[BP+3] = 0
;		DL/[BP+4] = # DRIVES INSTALLED (VALUE CHECKED)
;		DH/[BP+5] = MAX HEAD #
;	     ** 27/05/2016 - TRDOS 386 (TRDOS v2.0) **	
;            ** EBX = Buffer address for floppy disk parameters table **
;		;DI/[BP+6] = OFFSET TO DISK_BASE
;		;ES        = SEGMENT OF DISK_BASE
;
;		AX        = 0
;
;		NOTE : THE ABOVE INFORMATION IS STORED IN THE USERS STACK AT
;		       THE LOCATIONS WHERE THE MAIN ROUTINE WILL POP THEM
;		       INTO THE APPROPRIATE REGISTERS BEFORE RETURNING TO THE
;		       CALLER.
;-------------------------------------------------------------------------------
DSK_PARMS:
	; 09/08/2022
	; 06/08/2022 - TRDOS 386 v2.0.5
	;
	; Registers on stack:
	;    ebx = esp+28
	;    ecx = esp+24
	;    edx = esp+20
	;    esi = esp+16
	;    edi = esp+12
	;    return address from DSKETTE_IO_1 = esp+8
	;    ebp = esp+4
	;    return address from DSK_PARMS = esp
	;
	; INPUT:
	;   ebp = buffer address
	;   edi = drive number (0 or 1)
	
	; OUTPUT:
	;   ebx = [esp+28] ((BL = cmos type, BH = 0))
	;   ecx = [esp+24] ((CL = sectors per track, CH = tracks - 1))
	;   edx = [esp+20] ((DL = floppy drive count, DH = heads - 1))
	;   user's buffer = FDPT table
			 
	call	XLAT_NEW		; TRANSLATE STATE TO PRESENT ARCH,
     	
	;;mov	word [bp+2], 0		; DRIVE TYPE = 0
     	;;mov	ax, [EQUIP_FLAG]	; LOAD EQUIPMENT FLAG FOR # DISKETTES
     	;;and	al, 11000001b		; KEEP DISKETTE DRIVE BITS
     	;;mov	dl, 2			; DISKETTE DRIVES = 2
     	;;cmp	al, 01000001b		; 2 DRIVES INSTALLED ?
     	;;jz	short STO_DL		; IF YES JUMP
     	;;dec	dl			; DISKETTE DRIVES = 1
     	;;cmp	al, 00000001b		; 1 DRIVE INSTALLED ?
     	;;jnz	short NON_DRV		; IF NO JUMP
	
	sub	edx, edx
	mov     ax, [fd0_type]
	and     ax, ax
        jz      short NON_DRV
	inc     dl
	and     ah, ah
	jz      short STO_DL
	inc     dl
STO_DL:
	; 30/08/2020
	;cmp	dx, di
	; 06/08/2022
	cmp	edx, edi
	jna	short NON_DRV
	;
	;;mov	[bp+4], dl		; STORE NUMBER OF DRIVES
	;mov	[ebp+8], edx ; 20/02/2015	 	

	; 06/08/2022
	inc	dh ; number of heads - 1 
	; dh = 1
	; dl = number of floppy/diskette drives (DL)
	mov	[esp+20], edx

	; 11/04/2021
	;cmp	di, 1			; CHECK FOR VALID DRIVE
	;;ja	short NON_DRV1		; DRIVE INVALID
	;ja	NON_DRV1 ; 29/08/2020
	;	
	;;mov	byte [bp+5], 1		; MAXIMUM HEAD NUMBER =	1
	; 06/08/2022
	;mov	byte [ebp+9], 1 ; 20/02/2015	
	
	call	CMOS_TYPE		; RETURN DRIVE TYPE IN AL
	;;20/02/2015
	;;jc	short CHK_EST		; IF CMOS BAD CHECKSUM ESTABLISHED
	;;or	al, al			; TEST FOR NO DRIVE TYPE
	jz	short CHK_EST		; JUMP IF SO
	call	DR_TYPE_CHECK		; RTN CS:BX = MEDIA/DRIVE PARAM TBL
	jc	short CHK_EST		; TYPE NOT IN TABLE (POSSIBLE BAD CMOS)
	;mov	[bp+2], al		; STORE VALID CMOS DRIVE TYPE
        ;mov	[ebp+4], al ; 06/02/2015
	mov	cl, [ebx+MD.SEC_TRK]	; GET SECTOR/TRACK
        mov	ch, [ebx+MD.MAX_TRK]	; GET MAX. TRACK NUMBER
	jmp	short STO_CX		; CMOS GOOD, USE CMOS
CHK_EST:
	mov	ah, [DSK_STATE+edi]	; LOAD STATE FOR THIS DRIVE
	test	ah, MED_DET		; CHECK FOR ESTABLISHED STATE
	jz	short NON_DRV1		; CMOS BAD/INVALID OR UNESTABLISHED
USE_EST:
	and	ah, RATE_MSK		; ISOLATE STATE
	cmp	ah, RATE_250		; RATE 250 ?
	jne	short USE_EST2		; NO, GO CHECK OTHER RATE

;-----	DATA RATE IS 250 KBS, TRY 360 KB TABLE FIRST

	mov	al, 1			; DRIVE TYPE 1 (360KB)
	call	DR_TYPE_CHECK		; RTN CS:BX = MEDIA/DRIVE PARAM TBL
        mov	cl, [ebx+MD.SEC_TRK]	; GET SECTOR/TRACK
        mov	ch, [ebx+MD.MAX_TRK]	; GET MAX. TRACK NUMBER
	test	byte [DSK_STATE+edi], TRK_CAPA ; 80 TRACK ?
	jz	short STO_CX		; MUST BE 360KB DRIVE 

;-----	IT IS 1.44 MB DRIVE

PARM144:
	mov	al, 4			; DRIVE TYPE 4 (1.44MB)
	call	DR_TYPE_CHECK		; RTN CS:BX = MEDIA/DRIVE PARAM TBL
	mov	cl, [ebx+MD.SEC_TRK]	; GET SECTOR/TRACK
	mov	ch, [ebx+MD.MAX_TRK]	; GET MAX. TRACK NUMBER
STO_CX:
	;mov	[ebp], ecx		; SAVE POINTER IN STACK FOR RETURN
	; 06/08/2022
	mov	[esp+24], ecx ; spt (cl), tracks - 1 (ch)
ES_DI:
	;mov	[bp+6], bx		; ADDRESS OF MEDIA/DRIVE PARM TABLE 
	;mov	[ebp+12], ebx ; 06/02/2015
	;mov	ax, cs			; SEGMENT MEDIA/DRIVE PARAMETER TABLE
	;mov	es, ax			; ES IS SEGMENT OF TABLE
	;
	; 28/05/2016
	; 27/05/2016
	; return floppy disk parameters table to user
	; in user's buffer, which is pointed by EBX
	
	; 09/08/2022
	;movzx	eax, al
	sub	ecx, ecx
	mov	cl, al
	;;mov	[ebp+4], eax  ; ebx	; drive type (for floppy drives)
	; 06/08/2022
	;mov	[esp+28], eax ; drive type	
	; 09/08/2022
	mov	[esp+28], ecx ; drive type

	; 06/08/2022
	;push	edi
	
	;mov	edi, [ebp+4]  		; ebx (input), user's buffer address
	; 06/08/2022
	;mov	edi, ebp ; [esp+28] ; user's buffer address
	;
	;; 29/08/2020
	;or	edi, edi
	;jz	short no_copy_fdpt
	; 06/08/2022
	or	ebp, ebp ; [esp+28] = ebx ; user's buffer address if > 0
	jz	short DP_OUT
	; 06/08/2022
	;push	edi
	mov	edi, ebp
	;
	; 06/08/2022
	; 01/06/2016 (Int 33h, disk type return for floppy disks, in BL)
	;mov	[user_buffer], eax	; 01/06/2016 (overwrite ebx return value)
	
	;(Int 33h, Function 08h will replace user's buffer addr with disk type!)
	;
	mov	esi, ebx 		; floppy disk parameter table (16 bytes)
	;mov	ecx, 16 ; 16 bytes
        ; 09/08/2022
	; ecx < 256
	; 06/08/2022
	;sub	ecx, ecx
	mov	cl, 16
	call    transfer_to_user_buffer ; trdosk6.s (16/05/2016)
no_copy_fdpt:
	; 06/08/2022
	;pop	edi	
DP_OUT:
	; 06/08/2022
	;call	XLAT_OLD		; TRANSLATE STATE TO COMPATIBLE MODE
	;xor	ax, ax			; CLEAR
	; 06/08/2022
	xor	eax, eax
	;clc
	retn

;-----	NO DRIVE PRESENT HANDLER

NON_DRV:
	;;mov	byte [bp+4], 0		; CLEAR NUMBER OF DRIVES
	;mov	[ebp+8], edx ; 0 ; 20/02/2015
	; 06/08/2022
	mov	[esp+20], edx ; 0 ; number of floppy drives and heads are 0
NON_DRV1:
	cmp	di, 80h			; CHECK FOR FIXED MEDIA TYPE REQUEST
	jb	short NON_DRV2		; CONTINUE IF NOT REQUEST FALL THROUGH

;-----	FIXED DISK REQUEST FALL THROUGH ERROR
	
	; 06/08/2022
	;call	XLAT_OLD		; ELSE TRANSLATE TO COMPATIBLE MODE
	;mov	ax, si			; RESTORE AL
	; 06/08/2022
	mov	eax, esi
	mov	ah, BAD_CMD		; SET BAD COMMAND ERROR
	stc
	retn

NON_DRV2:
	;xor	ax, ax			; CLEAR PARMS IF NO DRIVES OR CMOS BAD
	xor	eax, eax	
	;mov	[ebp], ax		; TRACKS, SECTORS/TRACK = 0
	; 06/08/2022
	mov	[esp+24], eax ; spt and max. track number is 0

	;;mov	[bp+5], ah		; HEAD = 0
	; 06/08/2022
	;mov	[ebp+9], ah ; 06/02/2015
	; 06/08/2022
	;mov	[esp+21], ah ; 0	

	;;mov	[bp+6], ax		; OFFSET TO DISK_BASE = 0
	; 06/08/2022
	;mov	[ebp+12], eax
	
	;;mov	es, ax			; ES IS SEGMENT OF TABLE
	;jmp	short DP_OUT

	; 06/08/2022
	; 30/08/2020
	;call	XLAT_OLD
	;;mov	ah, NOT_RDY ; drive not ready
	mov	ah, INIT_FAIL ; DRIVE PARAMETER ACTIVITY FAILED 
	stc	; cf -> 1, ah = 'drive not ready' error code
	retn		

;-----	DATA RATE IS EITHER 300 KBS OR 500 KBS, TRY 1.2 MB TABLE FIRST

USE_EST2:
	mov	al, 2			; DRIVE TYPE 2 (1.2MB)
	call	DR_TYPE_CHECK		; RTN CS:BX = MEDIA/DRIVE PARAM TBL
        mov     cl, [ebx+MD.SEC_TRK]    ; GET SECTOR/TRACK
        mov     ch, [ebx+MD.MAX_TRK]    ; GET MAX. TRACK NUMBER
	cmp	ah, RATE_300		; RATE 300 ?
	jz	short STO_CX		; MUST BE 1.2MB DRIVE
	jmp	short PARM144		; ELSE, IT IS 1.44MB DRIVE 

; 30/08/2020

;-------------------------------------------------------------------------------
; DISK_TYPE (AH = 15H)	
;	THIS ROUTINE RETURNS THE TYPE OF MEDIA INSTALLED.
;
;  ON ENTRY:	DI = DRIVE #
;
;  ON EXIT:	AH = DRIVE TYPE, CY=0
;-------------------------------------------------------------------------------
DSK_TYPE:
	; 06/08/2022 - TRDOS 386 v2.0.5
	call	XLAT_NEW		; TRANSLATE STATE TO PRESENT ARCH.
	mov	al, [DSK_STATE+edi]	; GET PRESENT STATE INFORMATION
	or	al, al			; CHECK FOR NO DRIVE
	jz	short NO_DRV
	mov	ah, NOCHGLN		; NO CHANGE LINE FOR 40 TRACK DRIVE
	test	al, TRK_CAPA		; IS THIS DRIVE AN 80 TRACK DRIVE?
	jz	short DT_BACK		; IF NO JUMP
	mov	ah, CHGLN		; CHANGE LINE FOR 80 TRACK DRIVE
DT_BACK:
	; 06/08/2022
	;;push	ax			; SAVE RETURN VALUE
	; 11/04/2021
	;push	eax
	;call	XLAT_OLD		; TRANSLATE STATE TO COMPATIBLE MODE
	;; 11/04/2021
	;pop	eax
	;; pop	ax
	;clc				; NO ERROR
	;mov	bx, si			; GET SAVED AL TO BL
	; 06/08/2022
	mov	ebx, esi
	mov	al, bl			; PUT BACK FOR RETURN
	retn
NO_DRV:	
	;xor	ah, ah			; NO DRIVE PRESENT OR UNKNOWN
	;jmp	short DT_BACK
	
	; 06/08/2022
	; 30/08/2020
	;call	XLAT_OLD
	sub	eax, eax
	stc	; cf = 1 -> drive not ready, ah = 0 (disk type = 0)
	retn

;-------------------------------------------------------------------------------
; DISK_CHANGE	(AH = 16H)
;	THIS ROUTINE RETURNS THE STATE OF THE DISK CHANGE LINE.
;
; ON ENTRY:	DI = DRIVE #
;
; ON EXIT:	AH = @DSKETTE_STATUS
;		     00 - DISK CHANGE LINE INACTIVE, CY = 0
;		     06 - DISK CHANGE LINE ACTIVE, CY = 1
;-------------------------------------------------------------------------------
DSK_CHANGE:
	; 06/08/2022 - TRDOS 386 v2.0.5
	call	XLAT_NEW		; TRANSLATE STATE TO PRESENT ARCH.
	mov	al, [DSK_STATE+edi]	; GET MEDIA STATE INFORMATION
	or	al, al			; DRIVE PRESENT ?
	jz	short DC_NON		; JUMP IF NO DRIVE
	test	al, TRK_CAPA		; 80 TRACK DRIVE ?
	jz	short SETIT		; IF SO , CHECK CHANGE LINE
DC0:
        call    READ_DSKCHNG            ; GO CHECK STATE OF DISK CHANGE LINE
	jz	short FINIS		; CHANGE LINE NOT ACTIVE
SETIT:	
	mov	byte [DSKETTE_STATUS], MEDIA_CHANGE ; INDICATE MEDIA REMOVED

FINIS:	; 06/08/2022
	;call	XLAT_OLD		; TRANSLATE STATE TO COMPATIBLE MODE
	; 06/08/2022
	jmp	SETUP_END_X
	;call	SETUP_END		; VARIOUS CLEANUPS
	;; 06/08/2022
	;mov	ebx, esi		; GET SAVED AL TO BL
	;mov	al, bl			; PUT BACK FOR RETURN
	;retn
DC_NON:
	or	byte [DSKETTE_STATUS], TIME_OUT ; SET TIMEOUT, NO DRIVE
	jmp	short FINIS

;-------------------------------------------------------------------------------
; FORMAT_SET	(AH = 17H)
;	THIS ROUTINE IS USED TO ESTABLISH THE TYPE OF MEDIA TO BE USED
;	FOR THE FOLLOWING FORMAT OPERATION.
;
; ON ENTRY:	SI LOW = DASD TYPE FOR FORMAT
;		DI     = DRIVE #
;
; ON EXIT:	@DSKETTE_STATUS REFLECTS STATUS
;		AH = @DSKETTE_STATUS
;		CY = 1 IF ERROR
;-------------------------------------------------------------------------------
FORMAT_SET:
	call	XLAT_NEW		; TRANSLATE STATE TO PRESENT ARCH.
	;push	si			; SAVE DASD TYPE
	; 11/04/2021
	push	esi
	;mov	ax, si			; AH = ? , AL = DASD TYPE
	;xor	ah, ah			; AH = 0 , AL = DASD TYPE
	;mov	si, ax			; SI = DASD TYPE
	; 11/04/2021
	mov	eax, esi
	movzx	esi, al	
	and	byte [DSK_STATE+edi], ~(MED_DET+DBL_STEP+RATE_MSK) ; CLEAR STATE
	;dec	si			; CHECK FOR 320/360K MEDIA & DRIVE
	; 11/04/2021
	dec	esi
	jnz	short NOT_320		; BYPASS IF NOT
	or	byte [DSK_STATE+edi], MED_DET+RATE_250 ; SET TO 320/360
	jmp	short S0

NOT_320:
	call	MED_CHANGE		; CHECK FOR TIME_OUT
	cmp	byte [DSKETTE_STATUS], TIME_OUT
	jz	short S0		; IF TIME OUT TELL CALLER
S3:
	;dec	si			; CHECK FOR 320/360K IN 1.2M DRIVE
	; 11/04/2021
	dec	esi
	jnz	short NOT_320_12	; BYPASS IF NOT
	OR	byte [DSK_STATE+edi], MED_DET+DBL_STEP+RATE_300 ; SET STATE
	jmp	short S0

NOT_320_12:
	;dec	si			; CHECK FOR 1.2M MEDIA IN 1.2M DRIVE
	; 11/04/2021
	dec	esi
	jnz	short NOT_12		; BYPASS IF NOT
	or	byte [DSK_STATE+edi], MED_DET+RATE_500 ; SET STATE VARIABLE
	jmp	short S0		; RETURN TO CALLER

NOT_12:	
	;dec	si			; CHECK FOR SET DASD TYPE 04
	; 11/04/2021
	dec	esi
	jnz	short FS_ERR		; BAD COMMAND EXIT IF NOT VALID TYPE

	test	byte [DSK_STATE+edi], DRV_DET ; DRIVE DETERMINED ?
	jz	short ASSUME		; IF STILL NOT DETERMINED ASSUME
	mov	al, MED_DET+RATE_300
        test    byte [DSK_STATE+edi], FMT_CAPA ; MULTIPLE FORMAT CAPABILITY ?
	jnz	short OR_IT_IN		; IF 1.2 M THEN DATA RATE 300

ASSUME:
	mov	al, MED_DET+RATE_250	; SET UP

OR_IT_IN:
	or	[DSK_STATE+edi], al	; OR IN THE CORRECT STATE
S0:
	; 06/08/2022
	;call	XLAT_OLD		; TRANSLATE STATE TO COMPATIBLE MODE
	call	SETUP_END		; VARIOUS CLEANUPS
	;pop	BX			; GET SAVED AL TO BL
	; 11/04/2021
	pop	ebx
	mov	al, bl			; PUT BACK FOR RETURN
	retn

FS_ERR:
	mov	byte [DSKETTE_STATUS], BAD_CMD ; UNKNOWN STATE,BAD COMMAND
	jmp	short S0

;-------------------------------------------------------------------------------
; SET_MEDIA	(AH = 18H)
;	THIS ROUTINE SETS THE TYPE OF MEDIA AND DATA RATE 
;	TO BE USED FOR THE FOLLOWING FORMAT OPERATION.
;
; ON ENTRY:
;	[BP]	= SECTOR PER TRACK
;	[BP+1]	= TRACK #
;	DI	= DRIVE #
;
; ON EXIT:
;	@DSKETTE_STATUS REFLECTS STATUS
;	IF NO ERROR:
;		AH = 0
;		CY = 0
;		ES = SEGMENT OF MEDIA/DRIVE PARAMETER TABLE
;		DI/[BP+6] = OFFSET OF MEDIA/DRIVE PARAMETER TABLE
;	IF ERROR:	
;		AH = @DSKETTE_STATUS
;		CY = 1
;-------------------------------------------------------------------------------
SET_MEDIA:
	; 06/08/2022 - TRDOS 386 v2.0.5
	call	XLAT_NEW		; TRANSLATE STATE TO PRESENT ARCH.
        test    byte [DSK_STATE+edi], TRK_CAPA ; CHECK FOR CHANGE LINE AVAILABLE
	jz	short SM_CMOS		; JUMP IF 40 TRACK DRIVE
	call	MED_CHANGE		; RESET CHANGE LINE
	cmp	byte [DSKETTE_STATUS], TIME_OUT ; IF TIME OUT TELL CALLER
	je	short SM_RTN
	mov	byte [DSKETTE_STATUS], 0 ; CLEAR STATUS
SM_CMOS:
	call	CMOS_TYPE		; RETURN DRIVE TYPE IN (AL)
	;;20/02/2015
	;;jc	short MD_NOT_FND	; ERROR IN CMOS
	;;or	al, al			; TEST FOR NO DRIVE
	jz	short SM_RTN		; RETURN IF SO
	call	DR_TYPE_CHECK		; RTN CS:BX = MEDIA/DRIVE PARAM TBL
	jc	short MD_NOT_FND	; TYPE NOT IN TABLE (BAD CMOS)
	push	edi			; SAVE REG.
	xor	ebx, ebx		; BX = INDEX TO DR. TYPE TABLE
	mov	ecx, DR_CNT		; CX = LOOP COUNT
DR_SEARCH:
	mov	ah, [DR_TYPE+ebx]	; GET DRIVE TYPE
	and	ah, BIT7OFF		; MASK OUT MSB
	cmp	al, ah			; DRIVE TYPE MATCH ?
	jne	short NXT_MD		; NO, CHECK NEXT DRIVE TYPE
DR_FND:
	mov	edi, [DR_TYPE+ebx+1] 	; DI = MEDIA/DRIVE PARAM TABLE
MD_SEARCH:
        mov	ah, [edi+MD.SEC_TRK]    ; GET SECTOR/TRACK
	;cmp	[ebp], ah		; MATCH?
	; 06/08/2022
	cmp	[esp+24], ah  ; CL ; spt	
	jne	short NXT_MD		; NO, CHECK NEXT MEDIA
        mov     ah, [edi+MD.MAX_TRK]    ; GET MAX. TRACK #
	;cmp 	[ebp+1], ah		; MATCH?
	; 06/08/2022
	cmp	[esp+25], ah  ; CH ; heads - 1
	je	short MD_FND		; YES, GO GET RATE
NXT_MD:
	;add	bx, 3			; CHECK NEXT DRIVE TYPE
        add	ebx, 5 ; 18/02/2015
	loop	DR_SEARCH
	pop	edi			; RESTORE REG.
MD_NOT_FND:
	mov	byte [DSKETTE_STATUS], MED_NOT_FND ; ERROR, MEDIA TYPE NOT FOUND
	jmp	short SM_RTN		; RETURN
MD_FND:
        mov	al, [edi+MD.RATE]       ; GET RATE
	cmp	al, RATE_300		; DOUBLE STEP REQUIRED FOR RATE 300
	jne	short MD_SET
	or	al, DBL_STEP
MD_SET:
	;;mov	[bp+6], di		; SAVE TABLE POINTER IN STACK
	; 06/08/2022
	;mov	[ebp+12], edi ; 18/02/2015
	
	or	al, MED_DET		; SET MEDIA ESTABLISHED
	pop	edi
	and	byte [DSK_STATE+edi], ~(MED_DET+DBL_STEP+RATE_MSK) ; CLEAR STATE
	or	[DSK_STATE+edi], al
	;mov	ax, cs			; SEGMENT OF MEDIA/DRIVE PARAMETER TABLE
	;mov	es, ax			; ES IS SEGMENT OF TABLE
SM_RTN:
	; 06/08/2022
	;call	XLAT_OLD		; TRANSLATE STATE TO COMPATIBLE MODE
	;call	SETUP_END		; VARIOUS CLEANUPS
	;retn
	; 06/08/2022
	jmp	SETUP_END

;----------------------------------------------------------------
; DR_TYPE_CHECK							:
;	CHECK IF THE GIVEN DRIVE TYPE IN REGISTER (AL)		:
;	IS SUPPORTED IN BIOS DRIVE TYPE TABLE			:
; ON ENTRY:							:
;	AL = DRIVE TYPE						:
; ON EXIT:							:
;	CY = 0 	DRIVE TYPE SUPPORTED				:
;	     EBX = OFFSET TO MEDIA/DRIVE PARAMETER TABLE	:
;	CY = 1	DRIVE TYPE NOT SUPPORTED 			:
; REGISTERS ALTERED: EBX, AH ; 11/07/2022 			:
;----------------------------------------------------------------
DR_TYPE_CHECK:
	; 09/08/2022 - TRDOS 386 Kernel v2.0.5 
	; 12/07/2022
	; 11/07/2022
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	; 24/12/2021
	;push	eax ; 11/07/2022
	;push	ecx ; 08/07/2022
	;xor	ebx,ebx			; EBX = INDEX TO DR_TYPE TABLE
	mov	ebx, DR_TYPE
	;;mov	ecx, DR_CNT		; ECX = LOOP COUNT
	;mov	cl, DR_CNT
	mov	ah, DR_CNT ; 11/07/2022
TYPE_CHK:	
	;;mov	ah, [DR_TYPE+ebx]	; GET DRIVE TYPE
	;mov	ah, [ebx]
	;cmp	al, ah			; DRIVE TYPE MATCH?
	cmp	al, [ebx] ; 11/07/2022
	je	short DR_TYPE_VALID	; YES, RETURN WITH CARRY RESET
	; 16/02/2015 (32 bit address modification)
	add	ebx, 5			; CHECK NEXT DRIVE TYPE
	;loop	TYPE_CHK
	;dec	cl
	dec	ah ; 11/07/2022
	jnz	short TYPE_CHK
	;
	mov	ebx, MD_TBL6		; 1.44MB fd parameter table
					; Default for GET_PARM (11/12/2014)
	;
	stc				; DRIVE TYPE NOT FOUND IN TABLE
	;jmp	short TYPE_RTN
	; 12/07/2022
	retn
DR_TYPE_VALID:
	;mov	ebx, [DR_TYPE+ebx+1] 	; EBX = MEDIA TABLE
	inc	ebx
	mov	ebx, [ebx]
TYPE_RTN:
	;pop	ecx ; 08/07/2022
	; 24/12/2021
	;pop	eax ; 11/07/2022
	retn	
		
;----------------------------------------------------------------
; SEND_SPEC							:
;	SEND THE SPECIFY COMMAND TO CONTROLLER USING DATA FROM	:
;	THE DRIVE PARAMETER TABLE POINTED BY @DISK_POINTER	:
; ON ENTRY:	@DISK_POINTER = DRIVE PARAMETER TABLE		:
; ON EXIT:	NONE						:	
; REGISTERS ALTERED: CX, DX					:
;----------------------------------------------------------------		
SEND_SPEC:
	; 06/08/2022
	push	eax			; SAVE AX
	mov	eax, SPECBAC		; LOAD ERROR ADDRESS
	push	eax			; PUSH NEC_OUT ERROR RETURN
	mov	ah, 03h			; SPECIFY COMMAND
	call	NEC_OUTPUT		; OUTPUT THE COMMAND
	;sub	dl, dl			; FIRST SPECIFY BYTE
	; 06/08/2022
	sub	al, al ; 0 
	call	GET_PARM		; GET PARAMETER TO AH
	call	NEC_OUTPUT		; OUTPUT THE COMMAND
	;mov	dl, 1			; SECOND SPECIFY BYTE
	; 06/08/2022
	mov	al, 1
	call	GET_PARM		; GET PARAMETER TO AH
	call	NEC_OUTPUT		; OUTPUT THE COMMAND
	pop	eax			; POP ERROR RETURN
SPECBAC:
	pop	eax			; RESTORE ORIGINAL AX VALUE
	retn

;----------------------------------------------------------------
; SEND_SPEC_MD							:
;	SEND THE SPECIFY COMMAND TO CONTROLLER USING DATA FROM	:
;	THE MEDIA/DRIVE PARAMETER TABLE POINTED BY (CS:BX)	:
; ON ENTRY:	CS:BX = MEDIA/DRIVE PARAMETER TABLE		:
; ON EXIT:	NONE						:	
; REGISTERS ALTERED: AX						:
;----------------------------------------------------------------		
SEND_SPEC_MD:
	push	eax			; SAVE RATE DATA
	mov	eax, SPEC_ESBAC		; LOAD ERROR ADDRESS
	push	eax			; PUSH NEC_OUT ERROR RETURN
	mov	ah, 03h			; SPECIFY COMMAND
	call	NEC_OUTPUT		; OUTPUT THE COMMAND
        mov     ah, [ebx+MD.SPEC1]      ; GET 1ST SPECIFY BYTE
	call	NEC_OUTPUT		; OUTPUT THE COMMAND
        mov     ah, [ebx+MD.SPEC2]      ; GET SECOND SPECIFY BYTE
	call	NEC_OUTPUT		; OUTPUT THE COMMAND
	pop	eax			; POP ERROR RETURN
SPEC_ESBAC:
	pop	eax			; RESTORE ORIGINAL AX VALUE
	retn

;-------------------------------------------------------------------------------
; XLAT_NEW  
;	TRANSLATES DISKETTE STATE LOCATIONS FROM COMPATIBLE
;	MODE TO NEW ARCHITECTURE.
;
; ON ENTRY:	DI = DRIVE #
;-------------------------------------------------------------------------------
XLAT_NEW:
	; 06/08/2022 - TRDOS 386 Kernel v2.0.5
	; 11/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	cmp	edi, 1			; VALID DRIVE
	ja	short XN_OUT		; IF INVALID BACK
	cmp	byte [DSK_STATE+edi], 0	; NO DRIVE ?
	jz	short DO_DET		; IF NO DRIVE ATTEMPT DETERMINE
	
	;;mov	cx, di			; CX = DRIVE NUMBER
	;mov	ecx, edi
	;or	cl, cl
	;jz	short XN_0
	;shl	cl, 2			; CL = SHIFT COUNT, A=0, B=4
	;mov	al, [HF_CNTRL]		; DRIVE INFORMATION
	;ror	al, cl			; TO LOW NIBBLE
;XN_0:	
	;and	al, DRV_DET+FMT_CAPA+TRK_CAPA ; KEEP DRIVE BITS
        ;and	byte [DSK_STATE+edi], ~(DRV_DET+FMT_CAPA+TRK_CAPA)
	;or	[DSK_STATE+edi], al	; UPDATE DRIVE STATE
XN_OUT:
	retn

DO_DET:
	;call	DRIVE_DET		; TRY TO DETERMINE
	;retn
	;jmp	DRIVE_DET

;-------------------------------------------------------------------------------
; DRIVE_DET
;	DETERMINES WHETHER DRIVE IS 80 OR 40 TRACKS AND
;	UPDATES STATE INFORMATION ACCORDINGLY.
; ON ENTRY:	DI = DRIVE #
;-------------------------------------------------------------------------------
DRIVE_DET:
	; 06/08/2022 - TRDOS 386 Kernel v2.0.5
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	call	MOTOR_ON		; TURN ON MOTOR IF NOT ALREADY ON
	call	RECAL			; RECALIBRATE DRIVE
	jc	short DD_BAC		; ASSUME NO DRIVE PRESENT
	mov	ch, TRK_SLAP		; SEEK TO TRACK 48
	call	SEEK
	jc	short DD_BAC		; ERROR NO DRIVE
	mov	ch, QUIET_SEEK+1	; SEEK TO TRACK 10
SK_GIN:
	dec	ch			; DECREMENT TO NEXT TRACK
	;push	cx			; SAVE TRACK
	; 11/04/2021
	push	ecx
	call	SEEK
	jc	short POP_BAC		; POP AND RETURN
	mov	eax, POP_BAC		; LOAD NEC OUTPUT ERROR ADDRESS
	push	eax
	mov	ah, SENSE_DRV_ST	; SENSE DRIVE STATUS COMMAND BYTE
	call	NEC_OUTPUT		; OUTPUT TO NEC
	;mov	ax, di			; AL = DRIVE
	; 06/08/2022
	mov	eax, edi
	mov	ah, al			; AH = DRIVE
	call	NEC_OUTPUT		; OUTPUT TO NEC
	call	RESULTS			; GO GET STATUS
	pop	eax			; THROW AWAY ERROR ADDRESS
	;pop	cx			; RESTORE TRACK
	; 11/04/2021
	pop	ecx
	test	byte [NEC_STATUS], HOME	; TRACK 0 ?
	jz	short SK_GIN		; GO TILL TRACK 0
	or	ch, ch			; IS HOME AT TRACK 0
	jz	short IS_80		; MUST BE 80 TRACK DRIVE

;	DRIVE IS A 360; SET DRIVE TO DETERMINED;
;	SET MEDIA TO DETERMINED AT RATE 250.

	or	byte [DSK_STATE+edi], DRV_DET+MED_DET+RATE_250
	retn				; ALL INFORMATION SET
IS_80:
	or	byte [DSK_STATE+edi], TRK_CAPA ; SETUP 80 TRACK CAPABILITY
DD_BAC:
	retn
POP_BAC:
	;pop	cx			; THROW AWAY
	; 11/04/2021
	pop	ecx
	retn


	; 06/08/2022 - TRDOS 386 Kernel v2.0.5

%if 0

;-------------------------------------------------------------------------------
; XLAT_OLD 
;	TRANSLATES DISKETTE STATE LOCATIONS FROM NEW
;	ARCHITECTURE TO COMPATIBLE MODE.
;
; ON ENTRY:	DI = DRIVE
;-------------------------------------------------------------------------------
XLAT_OLD:
	cmp	edi, 1			; VALID DRIVE ?
	ja	short XO_OUT            ; IF INVALID BACK
        cmp	byte [DSK_STATE+edi], 0	; NO DRIVE ?
	jz	short XO_OUT		; IF NO DRIVE TRANSLATE DONE

;-----	TEST FOR SAVED DRIVE INFORMATION ALREADY SET

	;mov	cx, di			; CX = DRIVE NUMBER
	; 06/08/2022
	mov	ecx, edi
	shl	cl, 2			; CL = SHIFT COUNT, A=0, B=4
	mov	ah, FMT_CAPA		; LOAD MULTIPLE DATA RATE BIT MASK
	ror	ah, cl			; ROTATE BY MASK
	test	[HF_CNTRL], ah		; MULTIPLE-DATA RATE DETERMINED ?
	jnz	short SAVE_SET		; IF SO, NO NEED TO RE-SAVE

;-----	ERASE DRIVE BITS IN @HF_CNTRL FOR THIS DRIVE

	mov	ah, DRV_DET+FMT_CAPA+TRK_CAPA ; MASK TO KEEP
	ror	ah, cl			; FIX MASK TO KEEP
	not	ah			; TRANSLATE MASK
	and	[HF_CNTRL], ah		; KEEP BITS FROM OTHER DRIVE INTACT

;-----	ACCESS CURRENT DRIVE BITS AND STORE IN @HF_CNTRL

	mov	al, [DSK_STATE+edi]	; ACCESS STATE
	and	al, DRV_DET+FMT_CAPA+TRK_CAPA ; KEEP DRIVE BITS
	ror	al, cl			; FIX FOR THIS DRIVE
	or	[HF_CNTRL], al		; UPDATE SAVED DRIVE STATE

;-----	TRANSLATE TO COMPATIBILITY MODE

SAVE_SET:
	mov	ah, [DSK_STATE+edi]	; ACCESS STATE
	mov	bh, ah			; TO BH FOR LATER
	and	ah, RATE_MSK		; KEEP ONLY RATE
	cmp	ah, RATE_500		; RATE 500 ?
	jz	short CHK_144		; YES 1.2/1.2 OR 1.44/1.44
	mov	al, M3D1U		; AL = 360 IN 1.2 UNESTABLISHED
	cmp	ah, RATE_300		; RATE 300 ?
	jnz	short CHK_250		; NO, 360/360, 720/720 OR 720/1.44
	test	bh, DBL_STEP		; CHECK FOR DOUBLE STEP
	jnz	short TST_DET		; MUST BE 360 IN 1.2
UNKNO:
	mov	al, MED_UNK		; NONE OF THE ABOVE
	jmp	short AL_SET		; PROCESS COMPLETE
CHK_144:
	call	CMOS_TYPE		; RETURN DRIVE TYPE IN (AL)
	;;20/02/2015
	;;jc	short UNKNO		; ERROR, SET 'NONE OF ABOVE'
	jz	short UNKNO ;; 20/02/2015
	cmp	al, 2			; 1.2MB DRIVE ?
	jne	short UNKNO		; NO, GO SET 'NONE OF ABOVE'
	mov	al, M1D1U		; AL = 1.2 IN 1.2 UNESTABLISHED
	jmp	short TST_DET
CHK_250:
	mov	al, M3D3U		; AL = 360 IN 360 UNESTABLISHED
	cmp	ah, RATE_250		; RATE 250 ?
	jnz	short UNKNO		; IF SO FALL IHRU
	test	bh, TRK_CAPA		; 80 TRACK CAPABILITY ?
	jnz	short UNKNO		; IF SO JUMP, FALL THRU TEST DET
TST_DET:
	test	bh, MED_DET		; DETERMINED ?
	jz	short AL_SET		; IF NOT THEN SET
	add	al, 3			; MAKE DETERMINED/ESTABLISHED
AL_SET:
	and	byte [DSK_STATE+edi], ~(DRV_DET+FMT_CAPA+TRK_CAPA) ; CLEAR DRIVE
	or	[DSK_STATE+edi], al	; REPLACE WITH COMPATIBLE MODE
XO_OUT:
	retn

%endif

;-------------------------------------------------------------------------------
; SETUP_STATE:	INITIALIZES START AND END RATES.
;-------------------------------------------------------------------------------
SETUP_STATE:
	test	byte [DSK_STATE+edi], MED_DET ; MEDIA DETERMINED ?
	jnz	short J1C		; NO STATES IF DETERMINED
        mov     ax, (RATE_500*256)+RATE_300 ; AH = START RATE, AL = END RATE
	test	byte [DSK_STATE+edi], DRV_DET ; DRIVE ?
	jz	short AX_SET		; DO NOT KNOW DRIVE
	test	byte [DSK_STATE+edi], FMT_CAPA ; MULTI-RATE?
	jnz	short AX_SET		; JUMP IF YES
        mov     ax, RATE_250*257	; START A END RATE 250 FOR 360 DRIVE
AX_SET:	
	and	byte [DSK_STATE+edi], ~(RATE_MSK+DBL_STEP) ; TURN OFF THE RATE
	or	[DSK_STATE+edi], ah	; RATE FIRST TO TRY
	and	byte [LASTRATE], ~STRT_MSK ; ERASE LAST TO TRY RATE BITS
	ror	al, 4			; TO OPERATION LAST RATE LOCATION
	or	[LASTRATE], al		; LAST RATE
J1C:	
	retn

;-------------------------------------------------------------------------------
;  FMT_INIT: ESTABLISH STATE IF UNESTABLISHED AT FORMAT TIME.
;-------------------------------------------------------------------------------
FMT_INIT:
	test	byte [DSK_STATE+edi], MED_DET ; IS MEDIA ESTABLISHED
	jnz	short F1_OUT		; IF SO RETURN
	call	CMOS_TYPE		; RETURN DRIVE TYPE IN AL
	;; 20/02/2015
	;;jc	short CL_DRV		; ERROR IN CMOS ASSUME NO DRIVE
	jz	short CL_DRV ;; 20/02/2015
	dec	al			; MAKE ZERO ORIGIN
	;;JS	short CL_DRV		; NO DRIVE IF AL 0
	mov	ah, [DSK_STATE+edi]	; AH = CURRENT STATE
	and	ah, ~(MED_DET+DBL_STEP+RATE_MSK) ; CLEAR
	or	al, al			; CHECK FOR 360
	jnz	short N_360		; IF 360 WILL BE 0
	or	ah, MED_DET+RATE_250	; ESTABLISH MEDIA
	jmp	short SKP_STATE		; SKIP OTHER STATE PROCESSING
N_360:	
	dec	al			; 1.2 M DRIVE
	jnz	short N_12		; JUMP IF NOT
F1_RATE:
	or	ah, MED_DET+RATE_500	; SET FORMAT RATE
	jmp	short SKP_STATE		; SKIP OTHER STATE PROCESSING
N_12:	
	dec	al			; CHECK FOR TYPE 3
	jnz	short N_720		; JUMP IF NOT
	test	ah, DRV_DET		; IS DRIVE DETERMINED
	jz	short ISNT_12		; TREAT AS NON 1.2 DRIVE
	test	ah, FMT_CAPA		; IS 1.2M
	jz	short ISNT_12		; JUMP IF NOT
	or	ah, MED_DET+RATE_300	; RATE 300
	jmp	short SKP_STATE		; CONTINUE
N_720:
	dec	al			; CHECK FOR TYPE 4
	jnz	short CL_DRV		; NO DRIVE, CMOS BAD
	jmp	SHORT F1_RATE
ISNT_12: 
	or	ah, MED_DET+RATE_250	; MUST BE RATE 250
SKP_STATE:
	mov	[DSK_STATE+edi], ah	; STORE AWAY
F1_OUT:
	retn
CL_DRV:	
	xor	ah, ah			; CLEAR STATE
	jmp	short SKP_STATE		; SAVE IT

;-------------------------------------------------------------------------------
; MED_CHANGE	
;	CHECKS FOR MEDIA CHANGE, RESETS MEDIA CHANGE, 
;	CHECKS MEDIA CHANGE AGAIN.
;
; ON EXIT:	CY = 1 MEANS MEDIA CHANGE OR TIMEOUT
;		@DSKETTE_STATUS = ERROR CODE
;-------------------------------------------------------------------------------
MED_CHANGE:
	; 06/08/2022 - TRDOS 386 v2.0.5
	call	READ_DSKCHNG		; READ DISK CHANCE LINE STATE
	jz	short MC_OUT		; BYPASS HANDLING DISK CHANGE LINE
	and	byte [DSK_STATE+edi], ~MED_DET ; CLEAR STATE FOR THIS DRIVE

;	THIS SEQUENCE ENSURES WHENEVER A DISKETTE IS CHANGED THAT
;	ON THE NEXT OPERATION THE REQUIRED MOTOR START UP TIME WILL
;	BE WAITED. (DRIVE MOTOR MAY GO OFF UPON DOOR OPENING).

	;mov	cx, di			; CL = DRIVE 0
	; 06/08/2022
	mov	ecx, edi
	mov	al, 1			; MOTOR ON BIT MASK
	shl	al, cl			; TO APPROPRIATE POSITION
	not	al			; KEEP ALL BUT MOTOR ON
	cli				; NO INTERRUPTS
	and	[MOTOR_STATUS], al	; TURN MOTOR OFF INDICATOR
	sti				; INTERRUPTS ENABLED
	call	MOTOR_ON		; TURN MOTOR ON

;-----	THIS SEQUENCE OF SEEKS IS USED TO RESET DISKETTE CHANGE SIGNAL

	call	DSK_RESET		; RESET NEC
	mov	ch, 1			; MOVE TO CYLINDER 1
	call	SEEK			; ISSUE SEEK
	xor	ch, ch			; MOVE TO CYLINDER 0
	call	SEEK			; ISSUE SEEK
	mov	byte [DSKETTE_STATUS], MEDIA_CHANGE ; STORE IN STATUS
OK1:
	call	READ_DSKCHNG		; CHECK MEDIA CHANGED AGAIN
	jz	short OK2		; IF ACTIVE, NO DISKETTE, TIMEOUT
OK4:
	mov	byte [DSKETTE_STATUS], TIME_OUT ; TIMEOUT IF DRIVE EMPTY
OK2:		
	stc				; MEDIA CHANGED, SET CY
MC_OUT:	; 06/08/2022 (cf = 0)
	retn
;MC_OUT:
	;clc				; NO MEDIA CHANGED, CLEAR CY
	;retn

;-------------------------------------------------------------------------------
; SEND_RATE
;	SENDS DATA RATE COMMAND TO NEC
; ON ENTRY:	DI = DRIVE #
; ON EXIT:	NONE
; REGISTERS ALTERED: DX
;-------------------------------------------------------------------------------
SEND_RATE:
	;push	ax			; SAVE REG.
	; 11/04/2021
	push	eax
	and	byte [LASTRATE], ~SEND_MSK ; ELSE CLEAR LAST RATE ATTEMPTED
	mov	al, [DSK_STATE+edi]	; GET RATE STATE OF THIS DRIVE
	and	al, SEND_MSK		; KEEP ONLY RATE BITS
	or	[LASTRATE], al		; SAVE NEW RATE FOR NEXT CHECK
	rol	al, 2			; MOVE TO BIT OUTPUT POSITIONS
	mov	dx, 03F7h		; OUTPUT NEW DATA RATE
	out	dx, al
	;pop	ax			; RESTORE REG.
	; 11/04/2021
	pop	eax
	retn

;-------------------------------------------------------------------------------
; CHK_LASTRATE
;	CHECK PREVIOUS DATE RATE SNT TO THE CONTROLLER.
; ON ENTRY:
;	DI = DRIVE #
; ON EXIT:
;	ZF =  1 DATA RATE IS THE SAME AS THE LAST RATE SENT TO NEC
;	ZF =  0 DATA RATE IS DIFFERENT FROM LAST RATE
; REGISTERS ALTERED: DX
;-------------------------------------------------------------------------------
CHK_LASTRATE:
	; 13/07/2022 - TRDOS 386 v2.0.5
	;push	ax			; SAVE REG.
	; 11/04/2021
	push	eax
	; 13/07/2022 (BugFix)
	mov	ah, [LASTRATE] 		; GET LAST DATA RATE SELECTED
	mov	al, [DSK_STATE+edi]	; GET RATE STATE OF THIS DRIVE
        and	ax, SEND_MSK*257        ; KEEP ONLY RATE BITS OF BOTH
	cmp	al, ah			; COMPARE TO PREVIOUSLY TRIED
					; ZF = 1 RATE IS THE SAME
	;pop	ax			; RESTORE REG.
	; 11/04/2021
	pop	eax
	retn

;-------------------------------------------------------------------------------
; DMA_SETUP
;	THIS ROUTINE SETS UP THE DMA FOR READ/WRITE/VERIFY OPERATIONS.
;
; ON ENTRY:	AL = DMA COMMAND
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------

; SI = Head #, # of Sectors or DASD Type

; 22/08/2015
; 08/02/2015 - Protected Mode Modification
; 06/02/2015 - 07/02/2015
; NOTE: Buffer address must be in 1st 16MB of Physical Memory (24 bit limit).
; (DMA Addres = Physical Address)
; (Retro UNIX 386 v1 Kernel/System Mode Virtual Address = Physical Address)
;
; 09/08/2022
; 06/08/2022
; 04/02/2016 (clc)
; 20/02/2015 modification (source: AWARD BIOS 1999, DMA_SETUP)
; 16/12/2014 (IODELAY)

DMA_SETUP:
	; 09/08/2022
	; 06/08/2022 - TRDOS 386 Kernel v2.0.5
	;; 20/02/2015
	;;mov	edx, [ebp+4] 		; Buffer address
	; 06/08/2022
	;mov	edx, ebp ; buffer address
	;test	edx, 0FF000000h		; 16 MB limit (22/08/2015, bugfix)
	test	ebp, 0FF000000h
	jnz	short dma_bnd_err_stc
	; 09/08/2022
	mov	edx, ebp
	;
	;;push	ax			; DMA command
	; 11/04/2021
	push	eax
	; 06/08/2022
	;push	edx			; *
	;mov	dl, 3			; GET BYTES/SECTOR PARAMETER
	; 06/08/2022
	mov	al, 3			; GET BYTES/SECTOR PARAMETER
	call	GET_PARM		; 
	mov	cl, ah 			; SHIFT COUNT (0=128, 1=256, 2=512 ETC)
	;mov	ax, si			; Sector count
	; 06/08/2022
	mov	eax, esi
	mov	ah, al			; AH = # OF SECTORS
	sub	al, al			; AL = 0, AX = # SECTORS * 256
	;shr	ax, 1			; AX = # SECTORS * 128
	; 06/08/2022
	shr	eax, 1
	;shl	ax, cl			; SHIFT BY PARAMETER VALUE
	;dec	ax			; -1 FOR DMA VALUE
	;mov	cx, ax
	shl	eax, cl
	dec	eax
	mov	ecx, eax
	; 06/08/2022
	;pop	edx			; *
	;;pop	ax
	; 11/04/2021
	pop	eax
	cmp	al, 42h
        jne     short NOT_VERF
	; 09/08/2022
	mov	edx, 0FF0000h
	; 06/08/2022
	;mov	ebp, 0FF0000h
	jmp	short J33
NOT_VERF:
	add	dx, cx			; check for overflow
	jc	short dma_bnd_err
	;
	sub	dx, cx			; Restore start address
J33:
	cli				; DISABLE INTERRUPTS DURING DMA SET-UP
	out	DMA+12, al		; SET THE FIRST/LA5T F/F
	IODELAY				; WAIT FOR I/O
	out	DMA+11, al		; OUTPUT THE MODE BYTE
	;mov	eax, edx		; Buffer address
	; 06/08/2022
	;mov	eax, ebp
	; 09/08/2022
	mov	eax, edx
	out	DMA+4, al		; OUTPUT LOW ADDRESS
	IODELAY				; WAIT FOR I/O
	mov	al, ah
	out	DMA+4, al		; OUTPUT HIGH ADDRESS
	shr	eax, 16
	IODELAY				; I/O WAIT STATE
	out	081h, al		; OUTPUT HIGHEST BITS TO PAGE REGISTER
	IODELAY
	;mov	ax, cx			; Byte count - 1
	; 06/08/2022
	mov	eax, ecx
	out	DMA+5, al		; LOW BYTE OF COUNT
	IODELAY				; WAIT FOR I/O
	mov	al, ah
	out	DMA+5, al		; HIGH BYTE OF COUNT
	IODELAY
	sti				; RE-ENABLE INTERRUPTS
	mov	al, 2			; MODE FOR 8237
	out	DMA+10, al		; INITIALIZE THE DISKETTE CHANNEL

	clc	; 04/02/2016
	retn

dma_bnd_err_stc:
	stc
dma_bnd_err:
	mov	byte [DSKETTE_STATUS], DMA_BOUNDARY ; SET ERROR
	retn				; CY SET BY ABOVE IF ERROR

;; 16/12/2014
;;	CLI				; DISABLE INTERRUPTS DURING DMA SET-UP
;;	OUT	DMA+12,AL		; SET THE FIRST/LA5T F/F
;;	;jmp	$+2			; WAIT FOR I/O
;;	IODELAY
;; 	OUT	DMA+11,AL		; OUTPUT THE MODE BYTE
;;	;SIODELAY
;;      ;cmp	AL,42H			; DMA VERIFY COMMAND
;;      ;JNE	short NOT_VERF		; NO
;;      ;xor	AX,AX			; START ADDRESS
;;      ;jmp	SHORT J33
;;;NOT_VERF:	
;;	;mov	AX,ES			; GET THE ES VALUE
;;	;ROL	AX,4			; ROTATE LEFT
;;	;mov	CH,AL			; GET HIGHEST NIBBLE OF ES TO CH
;;	;and	AL,11110000B		; ZERO THE LOW NIBBLE FROM SEGMENT
;;	;add	AX,[BP+2]		; TEST FOR CARRY FROM ADDITION
;;	mov	eax,[ebp+4] ; 06/02/2015	
;;	;jnc	short J33
;;	;inc	CH			; CARRY MEANS HIGH 4 BITS MUST BE INC
;;;J33:
;;	push	eax			; SAVE START ADDRESS
;;	OUT	DMA+4,AL		; OUTPUT LOW ADDRESS
;;	;jmp	$+2			; WAIT FOR I/O
;;	IODELAY
;;	mov	AL,AH
;;	OUT	DMA+4,AL		; OUTPUT HIGH ADDRESS
;;	shr	eax, 16	     ; 07/02/2015
;;	;mov	AL,CH			; GET HIGH 4 BITS
;;	;jmp	$+2			; I/O WAIT STATE
;;	IODELAY
;;	;and	AL,00001111B
;;	OUT	081H,AL			; OUTPUT HIGH 4 BITS TO PAGE REGISTER
;;	;SIODELAY
;;
;;;----- DETERMINE COUNT
;;	sub	eax, eax ; 08/02/2015
;;	mov	AX,SI			; AL =  # OF SECTORS
;;	xchg	AL,AH			; AH =  # OF SECTORS
;;	sub	AL,AL			; AL = 0, AX = # SECTORS * 256
;;	SHR	AX,1			; AX = # SECTORS * 128
;;	push	AX			; SAVE # OF SECTORS * 128
;;	mov	DL,3			; GET BYTES/SECTOR PARAMETER
;;	call	GET_PARM		; "
;;	mov	CL,AH			; SHIFT COUNT (0=128, 1=256, 2=512 ETC)
;;	pop	AX			; AX = # SECTORS * 128
;;	SHL	AX,CL			; SHIFT BY PARAMETER VALUE
;;	dec	AX			; -1 FOR DMA VALUE
;;	push	eax  ; 08/02/2015	; SAVE COUNT VALUE
;;	OUT	DMA+5,AL		; LOW BYTE OF COUNT
;;	;jmp	$+2			; WAIT FOR I/O
;;	IODELAY
;;	mov	AL,AH
;;	OUT	DMA+5,AL		; HIGH BYTE OF COUNT
;;	;IODELAY
;;	STI				; RE-ENABLE INTERRUPTS
;;	pop	ecx  ; 08/02/2015 	; RECOVER COUNT VALUE
;;	pop	eax  ; 08/02/2015	; RECOVER ADDRESS VALUE
;;	;add	AX, CX			; ADD, TEST FOR 64K OVERFLOW
;;	add	ecx,eax ; 08/02/2015
;;	mov	AL, 2			; MODE FOR 8237
;;	;jmp	$+2			; WAIT FOR I/O
;;	SIODELAY
;;	OUT	DMA+10,AL		; INITIALIZE THE DISKETTE CHANNEL
;;	;jnc	short NO_BAD		; CHECK FOR ERROR
;;	jc	short dma_bnd_err ; 08/02/2015
;;	and	ecx,0FFF00000h	; 16 MB limit
;;	jz	short NO_BAD
;;dma_bnd_err:
;;	mov	byte [DSKETTE_STATUS],DMA_BOUNDARY ; SET ERROR
;;NO_BAD:
;;	retn				; CY SET BY ABOVE IF ERROR

;-------------------------------------------------------------------------------
; FMTDMA_SET
;	THIS ROUTINE SETS UP THE DMA CONTROLLER FOR A FORMAT OPERATION.
;
; ON ENTRY:	NOTHING REQUIRED
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------

FMTDMA_SET:
	; 06/08/2022 - TRDOS 386 v2.0.5
	;; 20/02/2015 modification	
	;;mov	edx, [ebp+4] 		; Buffer address
	; 06/08/2022
	;mov	edx, ebp ; buffer address
	; 06/08/2022
	;test	edx, 0FF000000h		; 16 MB limit
	test	ebp, 0FF000000h
	jnz	short dma_bnd_err_stc
	;
	;;push	dx			; *
	;; 11/04/2021
	; 06/08/2022
	;push	edx
	;mov	dl, 4			; SECTORS/TRACK VALUE IN PARM TABLE
	; 06/08/2022
	mov	al, 4			; SECTORS/TRACK VALUE IN PARM TABLE				
	call	GET_PARM		; "
	mov	al, ah			; AL = SECTORS/TRACK VALUE
	sub	ah, ah			; AX = SECTORS/TRACK VALUE
	;shl	ax, 2			; AX = SEC/TRK * 4 (OFFSET C,H,R,N)
	; 06/08/2022
	shl	eax, 2
	;dec	ax			; -1 FOR DMA VALUE
	dec	eax
	;mov	cx, ax
	mov	ecx, eax
	;;pop	dx			; *
	;; 11/04/2021
	; 06/08/2022
	;pop	edx
	; 06/08/2022
	mov	al, 04Ah
	;
	jmp	short NOT_VERF ; 06/08/2022	

;; 06/08/2022	
;	add	dx, cx			; check for overflow
;	jc	short dma_bnd_err
;	;
;	sub	dx, cx			; Restore start address
;	;
;	;mov	al, 04Ah		; WILL WRITE TO THE DISKETTE
;	cli				; DISABLE INTERRUPTS DURING DMA SET-UP
;	out	DMA+12, al		; SET THE FIRST/LA5T F/F
;	IODELAY				; WAIT FOR I/O
;	out	DMA+11, al		; OUTPUT THE MODE BYTE
;	mov	eax, edx		; Buffer address
;	out	DMA+4, al		; OUTPUT LOW ADDRESS
;	IODELAY				; WAIT FOR I/O
;	mov	al, ah
;	out	DMA+4, al		; OUTPUT HIGH ADDRESS
;	shr	eax, 16
;	IODELAY				; I/O WAIT STATE
;	out	081h, al		; OUTPUT HIGHEST BITS TO PAGE REGISTER
;	IODELAY
;	;mov	ax, cx			; Byte count - 1
;	; 06/08/2022
;	mov	eax, ecx
;	out	DMA+5, al		; LOW BYTE OF COUNT
;	IODELAY				; WAIT FOR I/O
;	mov	al, ah
;	out	DMA+5, al		; HIGH BYTE OF COUNT
;	IODELAY
;	sti				; RE-ENABLE INTERRUPTS
;	mov	al, 2			; MODE FOR 8237
;	out	DMA+10, al		; INITIALIZE THE DISKETTE CHANNEL
;
;	; 06/08/2022
;	clc
;	retn

;; 08/02/2015 - Protected Mode Modification
;;	mov	AL,04AH			; WILL WRITE TO THE DISKETTE
;;	CLI				; DISABLE INTERRUPTS DURING DMA SET-UP
;;	OUT	DMA+12,AL		; SET THE FIRST/LA5T F/F
;;	;jmp	$+2			; WAIT FOR I/O
;;	IODELAY
;;	OUT	DMA+11,AL		; OUTPUT THE MODE BYTE
;;	;mov	AX,ES			; GET THE ES VALUE
;;	;ROL	AX,4			; ROTATE LEFT
;;	;mov	CH,AL			; GET HIGHEST NIBBLE OF ES TO CH
;;	;and	AL,11110000B		; ZERO THE LOW NIBBLE FROM SEGMENT
;;	;add	AX,[BP+2]		; TEST FOR CARRY FROM ADDITION
;;	;jnc	short J33A
;;	;inc	CH			; CARRY MEANS HIGH 4 BITS MUST BE INC
;;	mov	eax,[ebp+4] ; 08/02/2015
;;;J33A:
;;	push	eax ; 08/02/2015	; SAVE START ADDRESS
;;	OUT	DMA+4,AL		; OUTPUT LOW ADDRESS
;;	;jmp	$+2			; WAIT FOR I/O
;;	IODELAY
;;	mov	AL,AH
;;	OUT	DMA+4,AL		; OUTPUT HIGH ADDRESS
;;	shr 	eax,16 ; 08/02/2015
;;	;mov	AL,CH			; GET HIGH 4 BITS
;;	;jmp	$+2			; I/O WAIT STATE
;;	IODELAY
;;	;and	AL,00001111B
;;	OUT	081H,AL			; OUTPUT HIGH 4 BITS TO PAGE REGISTER
;;
;;;----- DETERMINE COUNT
;;	sub	eax,eax ; 08/02/2015
;;	mov	DL,4			; SECTORS/TRACK VALUE IN PARM TABLE
;;	call	GET_PARM		; "
;;	xchg	AL,AH			; AL = SECTORS/TRACK VALUE
;;	sub	AH,AH			; AX = SECTORS/TRACK VALUE
;;	SHL	AX,2			; AX = SEC/TRK * 4 (OFFSET C,H,R,N)
;;	dec	AX			; -1 FOR DMA VALUE
;;	push	eax 	; 08/02/2015	; SAVE # OF BYTES TO BE TRANSFERED
;;	OUT	DMA+5,AL		; LOW BYTE OF COUNT
;;	;jmp	$+2			; WAIT FOR I/O
;;	IODELAY
;;	mov	AL,AH
;;	OUT	DMA+5,AL		; HIGH BYTE OF COUNT
;;	STI				; RE-ENABLE INTERRUPTS
;;	pop	ecx	; 08/02/2015	; RECOVER COUNT VALUE
;;	pop	eax	; 08/02/2015	; RECOVER ADDRESS VALUE
;;	;add	AX,CX			; ADD, TEST FOR 64K OVERFLOW
;;	add	ecx,eax ; 08/02/2015
;;	mov	AL,2			; MODE FOR 8237
;;	;jmp	$+2			; WAIT FOR I/O
;;	SIODELAY
;;	OUT	DMA+10,AL		; INITIALIZE THE DISKETTE CHANNEL
;;	;jnc	short FMTDMA_OK		; CHECK FOR ERROR
;;	jc	short fmtdma_bnd_err ; 08/02/2015
;;	and	ecx,0FFF00000h	; 16 MB limit
;;	jz	short FMTDMA_OK
;;	stc	; 20/02/2015
;;fmtdma_bnd_err:
;;	mov	byte [DSKETTE_STATUS],DMA_BOUNDARY ; SET ERROR
;;FMTDMA_OK:
;;	retn				; CY SET BY ABOVE IF ERROR

;-------------------------------------------------------------------------------
; NEC_INIT	
;	THIS ROUTINE SEEKS TO THE REQUESTED TRACK AND INITIALIZES
;	THE NEC FOR THE READ/WRITE/VERIFY/FORMAT OPERATION.
;
; ON ENTRY:	AH = NEC COMMAND TO BE PERFORMED
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------
NEC_INIT:
	; 11/08/2022
	; EBX = user's ECX register content (on stack)
	; 10/08/2022
	; 06/08/2022 - TRDOS 386 v2.0.5
	;push	ax			; SAVE NEC COMMAND
	; 11/04/2021
	push	eax
	call	MOTOR_ON		; TURN MOTOR ON FOR SPECIFIC DRIVE

;-----	DO THE SEEK OPERATION

	;mov	ch, [ebp+1]		; CH = TRACK #
	; 10/08/2022
	mov	ch, bh ; CH = TRACK #
	call	SEEK			; MOVE TO CORRECT TRACK
	;pop	ax			; RECOVER COMMAND
	; 11/04/2021
	pop	eax
	jc	short ER_1		; ERROR ON SEEK
	mov	ebx, ER_1		; LOAD ERROR ADDRESS
	push	ebx			; PUSH NEC_OUT ERROR RETURN

;-----	SEND OUT THE PARAMETERS TO THE CONTROLLER

	call	NEC_OUTPUT		; OUTPUT THE OPERATION COMMAND
	;mov	ax, si			; AH = HEAD #
	; 06/08/2022
	mov	eax, esi
	mov	ebx, edi		; BL = DRIVE #
	sal	ah, 2			; MOVE IT TO BIT 2
	and	ah, 00000100b		; ISOLATE THAT BIT
	or	ah, bl			; OR IN THE DRIVE NUMBER
	call	NEC_OUTPUT		; FALL THRU CY SET IF ERROR
	pop	ebx			; THROW AWAY ERROR RETURN
ER_1:
	retn

;-------------------------------------------------------------------------------
; RWV_COM
;	THIS ROUTINE SENDS PARAMETERS TO THE NEC SPECIFIC TO THE 
;	READ/WRITE/VERIFY OPERATIONS.
;
; ON ENTRY:	CS:BX = ADDRESS OF MEDIA/DRIVE PARAMETER TABLE
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------
RWV_COM:
	; 11/08/2022
	; 10/08/2022
	; 06/08/2022 - TRDOS 386 v2.0.5
	mov	eax, ER_2		; LOAD ERROR ADDRESS
	push	eax			; PUSH NEC_OUT ERROR RETURN
	;mov	ah, [ebp+1]		; OUTPUT TRACK #
	; 11/08/2022
	mov	ah, [esp+37] ; CH = OUTPUT TRACK #
	call	NEC_OUTPUT
	;mov	ax, si			; OUTPUT HEAD #
	; 06/08/2022
	mov	eax, esi
	call	NEC_OUTPUT
	;mov	ah, [ebp]		; OUTPUT SECTOR #
	; 11/08/2022
	mov	ah, [esp+36] ; CL = OUTPUT SECTOR #
	call	NEC_OUTPUT
	;mov	dl, 3			; BYTES/SECTOR PARAMETER FROM BLOCK
	; 06/08/2022
	mov	al, 3
	call	GET_PARM 		; ... TO THE NEC
	call	NEC_OUTPUT		; OUTPUT TO CONTROLLER
	;mov	dl, 4			; EOT PARAMETER FROM BLOCK
	; 06/08/2022
	mov	al, 4
	call	GET_PARM 		; ... TO THE NEC
	call	NEC_OUTPUT		; OUTPUT TO CONTROLLER
	mov	ah, [ebx+MD.GAP]        ; GET GAP LENGTH
_R15:
	call	NEC_OUTPUT
	;mov	dl, 6			; DTL PARAMETER PROM BLOCK
	; 06/08/2022
	mov	al, 6
	call	GET_PARM		; ... TO THE NEC
	call	NEC_OUTPUT		; OUTPUT TO CONTROLLER
	pop	eax			; THROW AWAY ERROR EXIT
ER_2:
	retn

;-------------------------------------------------------------------------------
; NEC_TERM
;	THIS ROUTINE WAITS FOR THE OPERATION THEN ACCEPTS THE STATUS 
;	FROM THE NEC FOR THE READ/WRITE/VERIFY/FORWAT OPERATION.
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------
NEC_TERM:

;-----	LET THE OPERATION HAPPEN

	push	esi			; SAVE HEAD #, # OF SECTORS
	call	WAIT_INT		; WAIT FOR THE INTERRUPT
	pushf
	call	RESULTS			; GET THE NEC STATUS
	jc	short SET_END_POP
	popf
	jc	short SET_END		; LOOK FOR ERROR

;-----	CHECK THE RESULTS RETURNED BY THE CONTROLLER

	cld				; SET THE CORRECT DIRECTION
	mov	esi, NEC_STATUS		; POINT TO STATUS FIELD
	lodsb				; GET ST0
	and	AL, 11000000B		; TEST FOR NORMAL TERMINATION
	jz	short SET_END
	cmp	AL,01000000B		; TEST FOR ABNORMAL TERMINATION
	jnz	short J18		; NOT ABNORMAL, BAD NEC

;-----	ABNORMAL TERMINATION, FIND OUT WHY

	lodsb				; GET ST1
	sal	al, 1			; TEST FOR EDT FOUND
	mov	ah, RECORD_NOT_FND
	jc	short J19
	sal	al, 2
	mov	ah, BAD_CRC
	jc	short J19
	sal	al, 1			; TEST FOR DMA OVERRUN
	mov	ah, BAD_DMA
	jc	short J19
	sal	al, 2			; TEST FOR RECORD NOT FOUND
	mov	ah, RECORD_NOT_FND
	jc	short J19
	sal	al, 1
	mov	ah, WRITE_PROTECT	; TEST FOR WRITE_PROTECT
	jc	short J19
	sal	al, 1			; TEST MISSING ADDRESS MARK
	mov	ah, BAD_ADDR_MARK
	jc	short J19

;----- 	NEC MUST HAVE FAILED
J18:
	mov	ah, BAD_NEC
J19:
	or	[DSKETTE_STATUS], ah
SET_END:
	cmp	byte [DSKETTE_STATUS], 1 ; SET ERROR CONDITION
	cmc
	pop	esi
	retn				; RESTORE HEAD #, # OF SECTORS

SET_END_POP:
	popf
	jmp	short SET_END

;-------------------------------------------------------------------------------
; DSTATE:	ESTABLISH STATE UPON SUCCESSFUL OPERATION.
;-------------------------------------------------------------------------------
DSTATE:
	cmp	byte [DSKETTE_STATUS], 0 ; CHECK FOR ERROR
	jnz	short SETBAC		; IF ERROR JUMP
	or	byte [DSK_STATE+edi], MED_DET
					; NO ERROR, MARK MEDIA AS DETERMINED
	test	byte [DSK_STATE+edi], DRV_DET ; DRIVE DETERMINED ?
	jnz	short SETBAC		; IF DETERMINED NO TRY TO DETERMINE
	mov	al, [DSK_STATE+edi]	; LOAD STATE
	and	al, RATE_MSK		; KEEP ONLY RATE
	cmp	al, RATE_250		; RATE 250 ?
	jne	short M_12		; NO, MUST BE 1.2M OR 1.44M DRIVE

;----- 	CHECK IF IT IS 1.44M

	call	CMOS_TYPE		; RETURN DRIVE TYPE IN (AL)
	;;20/02/2015
	;;jc	short M_12		; CMOS BAD
	jz	short M_12 ;; 20/02/2015
	cmp	al, 4			; 1.44MB DRIVE ?
	je	short M_12		; YES
M_720:
	and	byte [DSK_STATE+edi], ~FMT_CAPA ; TURN OFF FORMAT CAPABILITY
	or	byte [DSK_STATE+edi], DRV_DET ; MARK DRIVE DETERMINED
	jmp	short SETBAC		; BACK
M_12:	
	or	byte [DSK_STATE+edi], DRV_DET+FMT_CAPA 
					; TURN ON DETERMINED & FMT CAPA
SETBAC:
	retn

;-------------------------------------------------------------------------------
; RETRY	
;	DETERMINES WHETHER A RETRY IS NECESSARY. 
;	IF RETRY IS REQUIRED THEN STATE INFORMATION IS UPDATED FOR RETRY.
;
; ON EXIT:	CY = 1 FOR RETRY, CY = 0 FOR NO RETRY
;-------------------------------------------------------------------------------
RETRY:
	; 06/08/2022 - TRDOS 386 v2.0.5
	cmp	byte [DSKETTE_STATUS], 0 ; GET STATUS OF OPERATION
	jz	short NO_RETRY		; SUCCESSFUL OPERATION
	cmp	byte [DSKETTE_STATUS], TIME_OUT ; IF TIME OUT NO RETRY
	jz	short NO_RETRY
	mov	ah, [DSK_STATE+edi]	; GET MEDIA STATE OF DRIVE
	test	ah, MED_DET		; ESTABLISHED/DETERMINED ?
	jnz	short NO_RETRY		; IF ESTABLISHED STATE THEN TRUE ERROR
	and	ah, RATE_MSK		; ISOLATE RATE
	mov	ch, [LASTRATE]		; GET START OPERATION STATE
	rol	ch, 4			; TO CORRESPONDING BITS
	and	ch, RATE_MSK		; ISOLATE RATE BITS
	cmp	ch, ah			; ALL RATES TRIED
	je	short NO_RETRY		; IF YES, THEN TRUE ERROR

;	SETUP STATE INDICATOR FOR RETRY ATTEMPT TO NEXT RATE
;	 00000000B (500) -> 10000000B	(250)
;	 10000000B (250) -> 01000000B	(300)
;	 01000000B (300) -> 00000000B	(500)

	cmp	ah, RATE_500+1		; SET CY FOR RATE 500
	rcr	ah, 1			; TO NEXT STATE
	and	ah, RATE_MSK		; KEEP ONLY RATE BITS
	and	byte [DSK_STATE+edi], ~(RATE_MSK+DBL_STEP)
					; RATE, DBL STEP OFF
	or	[DSK_STATE+edi], ah	; TURN ON NEW RATE
	mov	byte [DSKETTE_STATUS], 0  ; RESET STATUS FOR RETRY
	stc				; SET CARRY FOR RETRY
	retn				; RETRY RETURN

NO_RETRY:
	; 06/08/2022
	;clc				; CLEAR CARRY NO RETRY
	retn				; NO RETRY RETURN

;-------------------------------------------------------------------------------
; NUM_TRANS
;	THIS ROUTINE CALCULATES THE NUMBER OF SECTORS THAT WERE
;	ACTUALLY TRANSFERRED TO/FROM THE DISKETTE.
;
; ON ENTRY:	[BP+1] = TRACK
;		SI-HI  = HEAD
;		[BP]   = START SECTOR
;
; ON EXIT:	AL = NUMBER ACTUALLY TRANSFERRED
;-------------------------------------------------------------------------------
NUM_TRANS:
	; 10/08/2022
	; 06/08/2022 - TRDOS 386 v2.0.5
	xor	al, al			; CLEAR FOR ERROR
	;cmp	byte [DSKETTE_STATUS], 0
	cmp	[DSKETTE_STATUS], al ; 0 ; CHECK FOR ERROR
	jnz	short NT_OUT		; IF ERROR 0 TRANSFERRED
	;mov	dl, 4			; SECTORS/TRACK OFFSET TO DL
	; 06/08/2022
	mov	al, 4
	call	GET_PARM		; AH = SECTORS/TRACK
	mov	bl, [NEC_STATUS+5]	; GET ENDING SECTOR
	;mov	cx, si			; CH = HEAD # STARTED
	; 06/08/2022
	mov	ecx, esi
	cmp	ch, [NEC_STATUS+4]	; GET HEAD ENDED UP ON
	jnz	short DIF_HD		; IF ON SAME HEAD, THEN NO ADJUST
	mov	ch, [NEC_STATUS+3]	; GET TRACK ENDED UP ON
	;cmp	ch, [ebp+1]		; IS IT ASKED FOR TRACK
	; 10/08/2022
	cmp	ch, [esp+29] ; CH = TRACK #
	jz	short SAME_TRK		; IF SAME TRACK NO INCREASE
	add	bl, ah			; ADD SECTORS/TRACK
DIF_HD:
	add	bl, ah			; ADD SECTORS/TRACK
SAME_TRK:
	;sub	bl, [ebp]		; SUBTRACT START FROM END
	; 10/08/2022
	sub	bl, [esp+28] ; CL = SECTOR #	
	mov	al, bl			; TO AL
NT_OUT:
	retn

;-------------------------------------------------------------------------------
; SETUP_DBL
;	CHECK DOUBLE STEP.
;
; ON ENTRY :	DI = DRIVE
;
; ON EXIT :	CY = 1 MEANS ERROR
;-------------------------------------------------------------------------------
SETUP_DBL:
	; 06/08/2022 - TRDOS 386 v2.0.5
	mov	ah, [DSK_STATE+edi]	; ACCESS STATE
	test	ah, MED_DET		; ESTABLISHED STATE ?
	jnz	short NO_DBL		; IF ESTABLISHED THEN DOUBLE DONE

;-----	CHECK FOR TRACK 0 TO SPEED UP ACKNOWLEDGE OF UNFORMATTED DISKETTE

	mov	byte [SEEK_STATUS], 0	; SET RECALIBRATE REQUIRED ON ALL DRIVES
	call	MOTOR_ON		; ENSURE MOTOR STAY ON
	mov	ch, 0			; LOAD TRACK 0
	call	SEEK			; SEEK TO TRACK 0
	call	READ_ID			; READ ID FUNCTION
	jc	short SD_ERR		; IF ERROR NO TRACK 0

;-----	INITIALIZE START AND MAX TRACKS (TIMES 2 FOR BOTH HEADS)

	mov	cx, 0450h 		; START, MAX TRACKS
	test	byte [DSK_STATE+edi], TRK_CAPA ; TEST FOR 80 TRACK CAPABILITY
	jz	short CNT_OK		; IF NOT COUNT IS SETUP
	mov	cl, 0A0h		; MAXIMUM TRACK 1.2 MB

;	ATTEMPT READ ID OF ALL TRACKS, ALL HEADS UNTIL SUCCESS; UPON SUCCESS,
;	MUST SEE IF ASKED FOR TRACK IN SINGLE STEP MODE = TRACK ID READ; IF NOT
;	THEN SET DOUBLE STEP ON.

CNT_OK:
	; 11/04/2021 (32 bit push/pop)
        mov     byte [MOTOR_COUNT], 0FFh ; ENSURE MOTOR STAYS ON FOR OPERATION
	push	ecx			; SAVE TRACK, COUNT
	mov	byte [DSKETTE_STATUS], 0 ; CLEAR STATUS, EXPECT ERRORS
	;xor	ax, ax			; CLEAR AX
	; 06/08/2022
	xor	eax, eax
	shr	ch, 1			; HALVE TRACK, CY = HEAD
	rcl	al, 3			; AX = HEAD IN CORRECT BIT
	push	eax			; SAVE HEAD
	call	SEEK			; SEEK TO TRACK
	pop	eax			; RESTORE HEAD
	;or	di, ax			; DI = HEAD OR'ED DRIVE
	; 06/08/2022
	or	edi, eax
	call	READ_ID			; READ ID HEAD 0
	pushf				; SAVE RETURN FROM READ_ID
	and	di, 11111011b		; TURN OFF HEAD 1 BIT
	popf				; RESTORE ERROR RETURN
	pop	ecx			; RESTORE COUNT
	jnc	short DO_CHK		; IF OK, ASKED = RETURNED TRACK ?
	inc	ch			; INC FOR NEXT TRACK
	cmp	ch, cl			; REACHED MAXIMUM YET
	jnz	short CNT_OK		; CONTINUE TILL ALL TRIED

;-----	FALL THRU, READ ID FAILED FOR ALL TRACKS

SD_ERR:	
	stc				; SET CARRY FOR ERROR
	retn				; SETUP_DBL ERROR EXIT

DO_CHK:
	mov	cl, [NEC_STATUS+3]	; LOAD RETURNED TRACK
	mov	[DSK_TRK+edi], cl	; STORE TRACK NUMBER
	shr	ch, 1			; HALVE TRACK
	cmp	ch, cl			; IS IT THE SAME AS ASKED FOR TRACK
	jz	short NO_DBL		; IF SAME THEN NO DOUBLE STEP
	or	byte [DSK_STATE+edi], DBL_STEP ; TURN ON DOUBLE STEP REQUIRED
NO_DBL:
	; 06/08/2022
	;clc				; CLEAR ERROR FLAG
	retn

;-------------------------------------------------------------------------------
; READ_ID
;	READ ID FUNCTION.
;
; ON ENTRY:	DI : BIT 2 = HEAD; BITS 1,0 = DRIVE
;
; ON EXIT: 	DI : BIT 2 IS RESET, BITS 1,0 = DRIVE
;		@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------
READ_ID:
	; 06/08/2022 - TRDOS 386 v2.0.5
	mov	eax, ER_3		; MOVE NEC OUTPUT ERROR ADDRESS
	push	eax
	mov	ah, 4Ah			; READ ID COMMAND
	call	NEC_OUTPUT		; TO CONTROLLER
	;mov	ax, di			; DRIVE # TO AH, HEAD 0
	; 06/08/2022
	mov	eax, edi
	mov	ah, al
	call	NEC_OUTPUT		; TO CONTROLLER
	call	NEC_TERM		; WAIT FOR OPERATION, GET STATUS
	pop	eax			; THROW AWAY ERROR ADDRESS
ER_3:
	retn

;-------------------------------------------------------------------------------
; CMOS_TYPE
;	RETURNS DISKETTE TYPE FROM CMOS
;
; ON ENTRY:	DI = DRIVE #
;
; ON EXIT:	AL = TYPE; CY REFLECTS STATUS
;-------------------------------------------------------------------------------

CMOS_TYPE: ; 11/12/2014
	mov	al, [edi+fd0_type]
	and 	al, al ; 18/12/2014
	retn

;CMOS_TYPE:
;	mov	al, CMOS_DIAG		; CMOS DIAGNOSTIC STATUS BYTE ADDRESS
;	call	CMOS_READ		; GET CMOS STATUS
;	test	al, BAD_BAT+BAD_CKSUM	; BATTERY GOOD AND CHECKSUM VALID
;	stc				; SET CY = 1 INDICATING ERROR FOR RETURN
;	jnz	short BAD_CM		; ERROR IF EITHER BIT ON
;	mov	al, CMOS_DISKETTE	; ADDRESS OF DISKETTE BYTE IN CMOS
;	call	CMOS_READ		; GET DISKETTE BYTE
;	or	di, di			; SEE WHICH DRIVE IN QUESTION
;	jnz	short TB		; IF DRIVE 1, DATA IN LOW NIBBLE
;	ror	al, 4			; EXCHANGE NIBBLES IF SECOND DRIVE
;TB:
;	and	al, 0Fh			; KEEP ONLY DRIVE DATA, RESET CY, 0
;BAD_CM:
;	retn				; CY, STATUS OF READ

;-------------------------------------------------------------------------------
; GET_PARM
;	THIS ROUTINE FETCHES THE INDEXED POINTER FROM THE DISK_BASE
;	BLOCK POINTED TO BY THE DATA VARIABLE @DISK_POINTER. A BYTE FROM
;	THAT TABLE IS THEN MOVED INTO AH, THE INDEX OF THAT BYTE BEING
;	THE PARAMETER IN DL.
;
; ON ENTRY:	DL = INDEX OF BYTE TO BE FETCHED
;
; ON EXIT:	AH = THAT BYTE FROM BLOCK
;		AL, DH DESTROYED
;-------------------------------------------------------------------------------
GET_PARM:
	; 09/08/2022
	; 06/08/2022 - TRDOS 386 v2.0.5
	; AL = INDEX
	; EDI = (current) DRIVE #
	;;push	ds
	;push	esi
    	;;sub	ax, ax			; DS = 0, BIOS DATA AREA
    	;;mov	ds, ax
	;;mov	ax, cs
	;;mov	ds, ax
	; 08/02/2015 (protected mode modifications, bx -> ebx)
	;xchg	edx, ebx		; BL = INDEX
	; 06/08/2022
	push	ebx			; SAVE EBX	
	;movzx	ebx, dl			; EBX = INDEX
	movzx	ebx, al ; 06/08/2022
	;;sub	bh, bh			; BX = INDEX
	;and	ebx, 0FFh
    	;;lds	si, [DISK_POINTER]	; POINT TO BLOCK
	;
	; 17/12/2014
	;mov	ax, [cfd]		; current (AL) and previous fd (AH)
	; 06/08/2022
	mov	eax, edi		; EDI = DRIVE #
	;cmp	al, ah
	cmp	al, [pfd]	
	je	short gpndc
	mov	[pfd], al		; current drive -> previous drive
	push	ebx ; 08/02/2015
	;mov	bl, al 
	;; 11/12/2014
	;mov	al, [ebx+fd0_type]	; Drive type (0,1,2,3,4)
	; 09/08/2022
	mov	al, [edi+fd0_type]	; Drive type (0,1,2,3,4)
	; 18/12/2014
	and	al, al
	jnz	short gpdtc
	mov	ebx, MD_TBL6		; 1.44 MB param. tbl. (default)
        jmp     short gpdpu
gpdtc:	
	call	DR_TYPE_CHECK
	; cf = 1 -> ebx points to 1.44MB fd parameter table (default)
gpdpu:
	mov	[DISK_POINTER], ebx
	pop	ebx
gpndc:
	;mov	esi, [DISK_POINTER] ; 08/02/2015, si -> esi
	; 06/08/2022
	add	ebx, [DISK_POINTER]
	;mov	ah, [esi+ebx]		; GET THE WORD
	mov	ah, [ebx]
	;xchg	edx, ebx		; RESTORE BX
	; 06/08/2022
	pop	ebx			; RESTORE EBX
	;pop	esi
	;;pop	ds
	retn

;-------------------------------------------------------------------------------
; MOTOR_ON
;	TURN MOTOR ON AND WAIT FOR MOTOR START UP TIME. THE @MOTOR_COUNT
;	IS REPLACED WITH A SUFFICIENTLY HIGH NUMBER (0FFH) TO ENSURE
;	THAT THE MOTOR DOES NOT GO OFF DURING THE OPERATION. IF THE
;	MOTOR NEEDED TO BE TURNED ON, THE MULTI-TASKING HOOK FUNCTION
;	(AX=90FDH, INT 15) IS CALLED TELLING THE OPERATING SYSTEM
;	THAT THE BIOS IS ABOUT TO WAIT FOR MOTOR START UP. IF THIS
;	FUNCTION RETURNS WITH CY = 1, IT MEANS THAT THE MINIMUM WAIT
;	HAS BEEN COMPLETED. AT THIS POINT A CHECK IS MADE TO ENSURE
;	THAT THE MOTOR WASN'T TURNED OFF BY THE TIMER. IF THE HOOK DID
;	NOT WAIT, THE WAIT FUNCTION (AH=086H) IS CALLED TO WAIT THE
;	PRESCRIBED AMOUNT OF TIME. IF THE CARRY FLAG IS SET ON RETURN,
;	IT MEANS THAT THE FUNCTION IS IN USE AND DID NOT PERFORM THE
;	WAIT. A TIMER 1 WAIT LOOP WILL THEN DO THE WAIT.
;
; ON ENTRY:	DI = DRIVE #
; ON EXIT:	AX,CX,DX DESTROYED
;-------------------------------------------------------------------------------
MOTOR_ON:
	; 06/08/2022 - TRDOS 386 Kernel v2.0.5
	push	ebx			; SAVE REG.
	call	TURN_ON			; TURN ON MOTOR
	jc	short MOT_IS_ON		; IF CY=1 NO WAIT
	; 06/08/2022
	;call	XLAT_OLD		; TRANSLATE STATE TO COMPATIBLE MODE
	call	XLAT_NEW		; TRANSLATE STATE TO PRESENT ARCH,
	;call	TURN_ON 		; CHECK AGAIN IF MOTOR ON
	;jc	short MOT_IS_ON		; IF NO WAIT MEANS IT IS ON
M_WAIT:
	;mov	dl, 10			; GET THE MOTOR WAIT PARAMETER
	; 06/08/2022
	mov	al, 10
	call	GET_PARM
	;mov	al, ah			; AL = MOTOR WAIT PARAMETER
	;xor	ah, ah			; AX = MOTOR WAIT PARAMETER
	;cmp	al, 8			; SEE IF AT LEAST A SECOND IS SPECIFIED
	cmp	ah, 8
	;jae	short GP2		; IF YES, CONTINUE
	ja	short J13
	;mov	al, 8			; ONE SECOND WAIT FOR MOTOR START UP
	mov	ah, 8

;-----	AS CONTAINS NUMBER OF 1/8 SECONDS (125000 MICROSECONDS) TO WAIT
GP2:	
;----- 	FOLLOWING LOOPS REQUIRED WHEN RTC WAIT FUNCTION IS ALREADY IN USE
J13:					; WAIT FOR 1/8 SECOND PER (AL)
	;mov	ecx, 8286		; COUNT FOR 1/8 SECOND AT 15.085737 US
	; 11/04/2021
	mov	ecx, 4167 ; count of 30 micro seconds * (1/8) 
	call	WAITF			; GO TO FIXED WAIT ROUTINE
	;dec	al			; DECREMENT TIME VALUE
	dec	ah
	jnz	short J13		; ARE WE DONE YET
MOT_IS_ON:
	pop	ebx			; RESTORE REG.
	retn

;-------------------------------------------------------------------------------
; TURN_ON
;	TURN MOTOR ON AND RETURN WAIT STATE.
;
; ON ENTRY:	DI = DRIVE #
;
; ON EXIT:	CY = 0 MEANS WAIT REQUIRED
;		CY = 1 MEANS NO WAIT REQUIRED
;		AX,BX,CX,DX DESTROYED
;-------------------------------------------------------------------------------
TURN_ON:
	mov	ebx, edi		; BX = DRIVE #
	mov	cl, bl			; CL = DRIVE #
	rol	bl, 4			; BL = DRIVE SELECT
	cli				; NO INTERRUPTS WHILE DETERMINING STATUS
	mov	byte [MOTOR_COUNT], 0FFh ; ENSURE MOTOR STAYS ON FOR OPERATION
	mov	al, [MOTOR_STATUS]	; GET DIGITAL OUTPUT REGISTER REFLECTION
	and	al, 00110000b		; KEEP ONLY DRIVE SELECT BITS
	mov	ah, 1			; MASK FOR DETERMINING MOTOR BIT
	shl	ah, cl			; AH = MOTOR ON, A=00000001, B=00000010

;  AL = DRIVE SELECT FROM @MOTOR_STATUS
;  BL = DRIVE SELECT DESIRED
;  AH = MOTOR ON MASK DESIRED

	cmp	al, bl			; REQUESTED DRIVE ALREADY SELECTED ?
	jnz	short TURN_IT_ON	; IF NOT SELECTED JUMP
	test	ah, [MOTOR_STATUS]	; TEST MOTOR ON BIT
	jnz	short NO_MOT_WAIT	; JUMP IF MOTOR ON AND SELECTED

TURN_IT_ON:
	or	ah, bl			; AH = DRIVE SELECT AND MOTOR ON
	mov	bh, [MOTOR_STATUS]	; SAVE COPY OF @MOTOR_STATUS BEFORE
	and	bh, 00001111b		; KEEP ONLY MOTOR BITS
	and	byte [MOTOR_STATUS], 11001111b ; CLEAR OUT DRIVE SELECT
	or	[MOTOR_STATUS], ah	; OR IN DRIVE SELECTED AND MOTOR ON
	mov	al, [MOTOR_STATUS]	; GET DIGITAL OUTPUT REGISTER REFLECTION
	mov	bl, al			; BL=@MOTOR_STATUS AFTER, BH=BEFORE
	and	bl, 00001111b		; KEEP ONLY MOTOR BITS
	sti				; ENABLE INTERRUPTS AGAIN
	and	al, 00111111b		; STRIP AWAY UNWANTED BITS
	rol	al, 4			; PUT BITS IN DESIRED POSITIONS
	or	al, 00001100b		; NO RESET, ENABLE DMA/INTERRUPT
	mov	dx, 03F2h		; SELECT DRIVE AND TURN ON MOTOR
	out	dx, al
	cmp	bl, bh			; NEW MOTOR TURNED ON ?
	;jz	short NO_MOT_WAIT	; NO WAIT REQUIRED IF JUST SELECT
	je	short no_mot_w1 ; 27/02/2015 
	clc				; (re)SET CARRY MEANING WAIT
	retn

NO_MOT_WAIT:
	sti
no_mot_w1: ; 27/02/2015
	stc				; SET NO WAIT REQUIRED
	;sti				; INTERRUPTS BACK ON
	retn

;-------------------------------------------------------------------------------
; HD_WAIT
;	WAIT FOR HEAD SETTLE TIME.
;
; ON ENTRY:	DI = DRIVE #
;
; ON EXIT:	AX,BX,CX,DX DESTROYED
;-------------------------------------------------------------------------------
HD_WAIT:
	; 06/08/2022 - TRDOS 386 v2.0.5
	;mov	dl,9			; GET HEAD SETTLE PARAMETER
	; 06/08/2022
	mov	al, 9
	call	GET_PARM
	or	ah, ah	; 17/12/2014
	jnz	short DO_WAT
        test    byte [MOTOR_STATUS], 10000000b ; SEE IF A WRITE OPERATION
	;jz	short ISNT_WRITE	; IF NOT, DO NOT ENFORCE ANY VALUES
	;or	ah, ah			; CHECK FOR ANY WAIT?
	;jnz	short DO_WAT		; IF THERE DO NOT ENFORCE
	jz	short HW_DONE
	mov	ah, HD12_SETTLE		; LOAD 1.2M HEAD SETTLE MINIMUM
	mov	al, [DSK_STATE+edi]	; LOAD STATE
	and	al, RATE_MSK		; KEEP ONLY RATE
	cmp	al, RATE_250		; 1.2 M DRIVE ?
	jnz	short DO_WAT		; DEFAULT HEAD SETTLE LOADED
;GP3:
	mov	ah, HD320_SETTLE	; USE 320/360 HEAD SETTLE
;	jmp	short DO_WAT

;ISNT_WRITE:
;	or	ah, ah			; CHECK FOR NO WAIT
;	jz	short HW_DONE		; IF NOT WRITE AND 0 ITS OK

;-----	AH CONTAINS NUMBER OF MILLISECONDS TO WAIT
DO_WAT:
;	mov	al, ah			; AL = # MILLISECONDS
;	;xor	ah, ah			; AX = # MILLISECONDS
J29:					; 	1 MILLISECOND LOOP
	;;mov	cx, WAIT_FDU_HEAD_SETTLE ; 33 ; 1 ms in 30 micro units.
	;mov	ecx, 66			; COUNT AT 15.085737 US PER COUNT
	; 11/04/2021
	;mov	ecx, WAIT_FDU_HEAD_SETTLE ; 33
	; 06/08/2022
	sub	ecx, ecx
	mov	cl, WAIT_FDU_HEAD_SETTLE ; 33
	call	WAITF			; DELAY FOR 1 MILLISECOND
	;dec	al			; DECREMENT THE COUNT
	dec	ah
	jnz	short J29		; DO AL MILLISECOND # OF TIMES
HW_DONE:
	retn

;-------------------------------------------------------------------------------
; NEC_OUTPUT
;	THIS ROUTINE SENDS A BYTE TO THE NEC CONTROLLER AFTER TESTING
;	FOR CORRECT DIRECTION AND CONTROLLER READY THIS ROUTINE WILL
;	TIME OUT IF THE BYTE IS NOT ACCEPTED WITHIN A REASONABLE AMOUNT
;	OF TIME, SETTING THE DISKETTE STATUS ON COMPLETION.
; 
; ON ENTRY: 	AH = BYTE TO BE OUTPUT
;
; ON EXIT:	CY = 0  SUCCESS
;		CY = 1  FAILURE -- DISKETTE STATUS UPDATED
;		        IF A FAILURE HAS OCCURRED, THE RETURN IS MADE ONE LEVEL
;		        HIGHER THAN THE CALLER OF NEC OUTPUT. THIS REMOVES THE
;		        REQUIREMENT OF TESTING AFTER EVERY CALL OF NEC_OUTPUT.
;		AX,CX,DX DESTROYED
;-------------------------------------------------------------------------------

; 09/12/2014 [Erdogan Tan] 
;	(from 'PS2 Hardware Interface Tech. Ref. May 88', Page 09-05.)
; Diskette Drive Controller Status Register (3F4h)
;	This read only register facilitates the transfer of data between
;	the system microprocessor and the controller.
; Bit 7 - When set to 1, the Data register is ready to transfer data 
;	  with the system micrprocessor.
; Bit 6 - The direction of data transfer. If this bit is set to 0,
;	  the transfer is to the controller.
; Bit 5 - When this bit is set to 1, the controller is in the non-DMA mode.
; Bit 4 - When this bit is set to 1, a Read or Write command is being executed.
; Bit 3 - Reserved.
; Bit 2 - Reserved.
; Bit 1 - When this bit is set to 1, dskette drive 1 is in the seek mode.
; Bit 0 - When this bit is set to 1, dskette drive 1 is in the seek mode.

; Data Register (3F5h)
; This read/write register passes data, commands and parameters, and provides
; diskette status information.
  		
NEC_OUTPUT:
	; 09/08/2022
	; 06/08/2022 - TRDOS 386 Kernel v2.0.5
	;
	;push	bx			; SAVE REG.
	mov	dx, 03F4h		; STATUS PORT
	;mov	bl,2			; HIGH ORDER COUNTER
	;xor	cx, cx			; COUNT FOR TIME OUT
	; 16/12/2014
	; waiting for (max.) 0.5 seconds
        ;;mov	byte [wait_count], 0 ;; 27/02/2015
	;
	; 17/12/2014
	; Modified from AWARD BIOS 1999 - ADISK.ASM - SEND_COMMAND
	;
	;WAIT_FOR_PORT:	Waits for a bit at a port pointed to by DX to
	;		go on.
	;INPUT:
	;	AH=Mask for isolation bits.
	;	AL=pattern to look for.
	;	DX=Port to test for
	;	BH:CX=Number of memory refresh periods to delay.
	;	     (normally 30 microseconds per period.)
	;
	;WFP_SHORT:  
	;	Wait for port if refresh cycle is short (15-80 Us range).
	;

;	mov	bl, WAIT_FDU_SEND_HI+1	; 0+1
;	mov	cx, WAIT_FDU_SEND_LO	; 16667
	mov	ecx, WAIT_FDU_SEND_LH   ; 16667 (27/02/2015)
;
;WFPS_OUTER_LP:
;	;
;WFPS_CHECK_PORT:
J23:
	in	al, dx			; GET STATUS
	and	al, 11000000b		; KEEP STATUS AND DIRECTION
	cmp	al, 10000000b		; STATUS 1 AND DIRECTION 0 ?
	jz	short J27		; STATUS AND DIRECTION OK
WFPS_HI:
	in	al, PORT_B  ; 061h	; SYS1	; wait for hi to lo
	test	al, 010h		; transition on memory
	jnz	short WFPS_HI		; refresh.
WFPS_LO:
	in	al, PORT_B		; SYS1
	test	al, 010h
	jz	short WFPS_LO
	;loop	short WFPS_CHECK_PORT
	loop	J23	; 27/02/2015
;	;
;	dec	bl
;	jnz	short WFPS_OUTER_LP
;	jmp	short WFPS_TIMEOUT	; fail
;J23:
;	in	al, dx			; GET STATUS
;	and	al, 11000000b		; KEEP STATUS AND DIRECTION
;	cmp	al, 10000000b		; STATUS 1 AND DIRECTION 0 ?
;	jz	short J27		; STATUS AND DIRECTION OK
	;loop	J23			; CONTINUE TILL CX EXHAUSTED
	;dec	bl			; DECREMENT COUNTER
	;jnz	short J23		; REPEAT TILL DELAY FINISHED, CX = 0
   
	;;27/02/2015
	;16/12/2014
        ;;cmp	byte [wait_count], 10   ; (10/18.2 seconds)
	;;jb	short J23

;WFPS_TIMEOUT:

;-----	FALL THRU TO ERROR RETURN

	or	byte [DSKETTE_STATUS], TIME_OUT
	;pop	bx			; RESTORE REG.
	pop	eax ; 08/02/2015	; DISCARD THE RETURN ADDRESS
	stc				; INDICATE ERROR TO CALLER
	retn

;-----	DIRECTION AND STATUS OK; OUTPUT BYTE

J27:	
	mov	al, ah			; GET BYTE TO OUTPUT
	;inc	dx			; DATA PORT = STATUS PORT + 1
	; 06/08/2022
	inc	dl
	out	dx, al			; OUTPUT THE BYTE
	;;NEWIODELAY  ;; 27/02/2015
	; 27/02/2015
	;pushf				; SAVE FLAGS
	; 09/08/2022
	; cf = 0, zf = 1
	;mov	ecx, 3			; 30 TO 45 MICROSECONDS WAIT FOR
	; 11/04/2021
	;mov	ecx, 2 
	; 06/08/2022
	sub	ecx, ecx
	mov	cl, 2
	call 	WAITF			; NEC FLAGS UPDATE CYCLE
	; 09/08/2022
	; cf = 0, zf = 1
	;popf				; RESTORE FLAGS FOR EXIT
	;pop	bx			; RESTORE REG
	retn				; CY = 0 FROM TEST INSTRUCTION

;-------------------------------------------------------------------------------
; SEEK
;	THIS ROUTINE WILL MOVE THE HEAD ON THE NAMED DRIVE TO THE NAMED
;	TRACK. IF THE DRIVE HAS NOT BEEN ACCESSED SINCE THE DRIVE
;	RESET COMMAND WAS ISSUED, THE DRIVE WILL BE RECALIBRATED.
;
; ON ENTRY:	DI = DRIVE #
;		CH = TRACK #
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION.
;		AX,BX,CX DX DESTROYED
;-------------------------------------------------------------------------------
SEEK:
	mov	ebx, edi		; BX = DRIVE #
	mov	al, 1			; ESTABLISH MASK FOR RECALIBRATE TEST
	; 06/08/2022
	;xchg	cl, bl			; SET DRIVE VALUE INTO CL
	;rol	al, cl			; SHIFT MASK BY THE DRIVE VALUE
	;xchg	cl, bl			; RECOVER TRACK VALUE
	; 06/08/2022
	test	bl, al ; test bl, 1
	jz	short seek_0
	inc	al ; shl al, 1
seek_0:
	test	al, [SEEK_STATUS]	; TEST FOR RECALIBRATE REQUIRED
	jnz	short J28A		; JUMP IF RECALIBRATE NOT REQUIRED

	or	[SEEK_STATUS], al	; TURN ON THE NO RECALIBRATE BIT IN FLAG
	call	RECAL			; RECALIBRATE DRIVE
	jnc	short AFT_RECAL		; RECALIBRATE DONE

;-----	ISSUE RECALIBRATE FOR 80 TRACK DISKETTES

	mov	byte [DSKETTE_STATUS], 0 ; CLEAR OUT INVALID STATUS
	call	RECAL			; RECALIBRATE DRIVE
	jc	short RB		; IF RECALIBRATE FAILS TWICE THEN ERROR

AFT_RECAL:
        mov     byte [DSK_TRK+edi], 0	; SAVE NEW CYLINDER AS PRESENT POSITION
	or	ch, ch			; CHECK FOR SEEK TO TRACK 0
	jz	short DO_WAIT		; HEAD SETTLE, CY = 0 IF JUMP

;-----	DRIVE IS IN SYNCHRONIZATION WITH CONTROLLER, SEEK TO TRACK

J28A:	test	byte [DSK_STATE+edi], DBL_STEP ; CHECK FOR DOUBLE STEP REQUIRED
	jz	short _R7		; SINGLE STEP REQUIRED BYPASS DOUBLE
	shl	ch, 1			; DOUBLE NUMBER OF STEP TO TAKE

_R7:	cmp	ch, [DSK_TRK+edi]	; SEE IF ALREADY AT THE DESIRED TRACK
	je	short RB		; IF YES, DO NOT NEED TO SEEK

	mov	edx, NEC_ERR		; LOAD RETURN ADDRESS
	push	edx ; (*)		; ON STACK FOR NEC OUTPUT ERROR
	mov	[DSK_TRK+edi], ch	; SAVE NEW CYLINDER AS PRESENT POSITION
	mov	ah, 0Fh			; SEEK COMMAND TO NEC
	call	NEC_OUTPUT
	mov	ebx, edi		; BX = DRIVE #
	mov	ah, bl			; OUTPUT DRIVE NUMBER
	call	NEC_OUTPUT
	mov	ah, [DSK_TRK+edi]	; GET CYLINDER NUMBER
	call	NEC_OUTPUT
	call	CHK_STAT_2		; ENDING INTERRUPT AND SENSE STATUS

;-----	WAIT FOR HEAD SETTLE

DO_WAIT:
	pushf				; SAVE STATUS
	call	HD_WAIT			; WAIT FOR HEAD SETTLE TIME
	popf				; RESTORE STATUS
RB:
NEC_ERR:
	; 08/02/2015 (code trick here from original IBM PC/AT DISKETTE.ASM)
	; (*) nec_err -> retn (push edx -> pop edx) -> nec_err -> retn
	retn				; RETURN TO CALLER

;-------------------------------------------------------------------------------
; RECAL
;	RECALIBRATE DRIVE
;
; ON ENTRY:	DI = DRIVE #
;
; ON EXIT:	CY REFLECTS STATUS OF OPERATION.
;-------------------------------------------------------------------------------
RECAL:
	;push	cx
	; 11/04/2021
	push	ecx
	mov	eax, RC_BACK		; LOAD NEC_OUTPUT ERROR
	push	eax
	mov	ah, 07h			; RECALIBRATE COMMAND
	call	NEC_OUTPUT
	mov	ebx, edi		; BX = DRIVE #
	mov	ah, bl
	call	NEC_OUTPUT		; OUTPUT THE DRIVE NUMBER
	call	CHK_STAT_2		; GET THE INTERRUPT AND SENSE INT STATUS
	pop	eax			; THROW AWAY ERROR
RC_BACK:
	;pop	cx
	; 11/04/2021
	pop	ecx
	retn

;-------------------------------------------------------------------------------
; CHK_STAT_2
;	THIS ROUTINE HANDLES THE INTERRUPT RECEIVED AFTER RECALIBRATE,
;	OR SEEK TO THE ADAPTER. THE INTERRUPT IS WAITED FOR, THE
;	INTERRUPT STATUS SENSED, AND THE RESULT RETURNED TO THE CALLER.
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION.
;-------------------------------------------------------------------------------
CHK_STAT_2:
        mov     eax, CS_BACK            ; LOAD NEC_OUTPUT ERROR ADDRESS
	push	eax
	call	WAIT_INT		; WAIT FOR THE INTERRUPT
	jc	short J34		; IF ERROR, RETURN IT
	mov	ah, 08h			; SENSE INTERRUPT STATUS COMMAND
	call	NEC_OUTPUT
	call	RESULTS			; READ IN THE RESULTS
	jc	short J34
	mov	al, [NEC_STATUS]	; GET THE FIRST STATUS BYTE
	and	al, 01100000B		; ISOLATE THE BITS
	cmp	al, 01100000B		; TEST FOR CORRECT VALUE
	jz	short J35		; IF ERROR, GO MARK IT
	clc				; GOOD RETURN
J34:
	pop	eax			; THROW AWAY ERROR RETURN
CS_BACK:
	retn
J35:
	or	byte [DSKETTE_STATUS], BAD_SEEK
	stc				; ERROR RETURN CODE
	jmp	short J34

;-------------------------------------------------------------------------------
; WAIT_INT
;	THIS ROUTINE WAITS FOR AN INTERRUPT TO OCCUR A TIME OUT ROUTINE
;	TAKES PLACE DURING THE WAIT, SO THAT AN ERROR MAY BE RETURNED
;	IF THE DRIVE IS NOT READY.
;
; ON EXIT: 	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION.
;-------------------------------------------------------------------------------

; 17/12/2014
; 2.5 seconds waiting !
;(AWARD BIOS - 1999, WAIT_FDU_INT_LOW, WAIT_FDU_INT_HI)
; amount of time to wait for completion interrupt from NEC.

	; 06/08/2022 - TRDOS 386 v2.0.5
WAIT_INT:
	sti				; TURN ON INTERRUPTS, JUST IN CASE
	; 06/08/2022
	;clc				; CLEAR TIMEOUT INDICATOR
	;
	;mov	bl, 10			; CLEAR THE COUNTERS
	;xor	cx, cx			; FOR 2 SECOND WAIT

	; Modification from AWARD BIOS - 1999 (ATORGS.ASM, WAIT
	;
	;WAIT_FOR_MEM:	
	;	Waits for a bit at a specified memory location pointed
	;	to by ES:[DI] to become set.
	;INPUT:
	;	AH= Mask to test with.
	;	ES:[DI] = memory location to watch.
	;	BH:CX= Number of memory refresh periods to delay.
	;	     (normally 30 microseconds per period.)

	; waiting for (max.) 2.5 secs in 30 micro units.
;	mov 	cx, WAIT_FDU_INT_LO		; 017798
;;	mov 	bl, WAIT_FDU_INT_HI
;	mov 	bl, WAIT_FDU_INT_HI + 1
	; 27/02/2015
	mov 	ecx, WAIT_FDU_INT_LH	; 83334 (2.5 seconds)		
WFMS_CHECK_MEM:
	test	byte [SEEK_STATUS], INT_FLAG
					; TEST FOR INTERRUPT OCCURRING
        jnz     short J37
WFMS_HI:
	in	al, PORT_B  ; 061h	; SYS1, wait for lo to hi
	test	al, 010h		; transition on memory
	jnz	short WFMS_HI		; refresh.
WFMS_LO:
	in	al, PORT_B		; SYS1
	test	al, 010h
	jz	short WFMS_LO
        loop	WFMS_CHECK_MEM

;WFMS_OUTER_LP:
;;	or	bl, bl			; check outer counter
;;	jz	short J36A		; WFMS_TIMEOUT
;	dec	bl
;	jz	short J36A	
;	jmp	short WFMS_CHECK_MEM

	; 17/12/2014
	; 16/12/2014
;	mov	byte [wait_count], 0	; Reset (INT 08H) counter
;J36:
;	test	byte [SEEK_STATUS], INT_FLAG
;					; TEST FOR INTERRUPT OCCURRING
;	jnz	short J37

	; 16/12/2014
	;loop	J36			; COUNT DOWN WHILE WAITING
	;dec	bl			; SECOND LEVEL COUNTER
	;jnz	short J36
;       cmp     byte [wait_count], 46   ; (46/18.2 seconds)
;	jb	short J36

;WFMS_TIMEOUT:
;J36A:
	or	byte [DSKETTE_STATUS], TIME_OUT ; NOTHING HAPPENED
	stc				; ERROR RETURN
J37:
	pushf				; SAVE CURRENT CARRY
	and	byte [SEEK_STATUS], ~INT_FLAG ; TURN OFF INTERRUPT FLAG
	popf				; RECOVER CARRY
	retn				; GOOD RETURN CODE

;-------------------------------------------------------------------------------
; RESULTS
;	THIS ROUTINE WILL READ ANYTHING THAT THE NEC CONTROLLER RETURNS 
;	FOLLOWING AN INTERRUPT.
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION.
;		AX,BX,CX,DX DESTROYED
;-------------------------------------------------------------------------------
RESULTS:
	; 06/08/2022 - TRDOS 386 v2.0.5
	push	edi
	mov	edi, NEC_STATUS		; POINTER TO DATA AREA
	mov	bl, 7			; MAX STATUS BYTES
	mov	dx, 03F4h		; STATUS PORT

;-----	WAIT FOR REQUEST FOR MASTER

_R10: 
	; 16/12/2014
	; wait for (max) 0.5 seconds
	;mov	bh, 2			; HIGH ORDER COUNTER
	;xor	cx, cx			; COUNTER

	;Time to wait while waiting for each byte of NEC results = .5
	;seconds.  .5 seconds = 500,000 micros.  500,000/30 = 16,667.
	; 27/02/2015
	mov 	ecx, WAIT_FDU_RESULTS_LH ; 16667  
	;mov	cx, WAIT_FDU_RESULTS_LO  ; 16667
	;mov	bh, WAIT_FDU_RESULTS_HI+1 ; 0+1

WFPSR_OUTER_LP:
	;
WFPSR_CHECK_PORT:
J39:					; WAIT FOR MASTER
	in	al, dx			; GET STATUS
	and	al, 11000000b		; KEEP ONLY STATUS AND DIRECTION
	cmp	al, 11000000b		; STATUS 1 AND DIRECTION 1 ?
	jz	short J42		; STATUS AND DIRECTION OK
WFPSR_HI:
	in	al, PORT_B	; 061h	; SYS1	; wait for hi to lo
	test	al, 010h		; transition on memory
	jnz	short WFPSR_HI		; refresh.
WFPSR_LO:
	in	al, PORT_B		; SYS1
	test	al, 010h
	jz	SHORT WFPSR_LO
	loop	WFPSR_CHECK_PORT

	;; 27/02/2015
	;;dec	bh
	;;jnz	short WFPSR_OUTER_LP
	;jmp	short WFPSR_TIMEOUT	; fail

	;;mov	byte [wait_count], 0
;J39:					; WAIT FOR MASTER
;	in	al, dx			; GET STATUS
;	and	al, 11000000b		; KEEP ONLY STATUS AND DIRECTION
;	cmp	al, 11000000b		; STATUS 1 AND DIRECTION 1 ?
;	jz	short J42		; STATUS AND DIRECTION OK
	;loop	J39			; LOOP TILL TIMEOUT
	;dec	bh			; DECREMENT HIGH ORDER COUNTER
	;jnz	short J39		; REPEAT TILL DELAY DONE
	;
	;;cmp	byte [wait_count], 10  ; (10/18.2 seconds)
	;;jb	short J39	

;WFPSR_TIMEOUT:
	or	byte [DSKETTE_STATUS], TIME_OUT
	stc				; SET ERROR RETURN
	jmp	short POPRES		; POP REGISTERS AND RETURN

;-----	READ IN THE STATUS

J42:
	jmp	$+2			; I/O DELAY
	;inc	dx			; POINT AT DATA PORT
	; 06/08/2022
	inc	dl
	in	al, dx			; GET THE DATA
	; 16/12/2014
	NEWIODELAY
        mov     [edi], al		; STORE THE BYTE
	inc	edi			; INCREMENT THE POINTER

	; 16/12/2014
;	push	cx
;	mov	cx, 30
;wdw2:
;	NEWIODELAY
;	loop	wdw2
;	pop	cx

	;;mov	ecx, 3			; MINIMUM 24 MICROSECONDS FOR NEC
	; 11/04/2021
	;mov	ecx, 2
	; 06/08/2022
	sub	ecx, ecx
	mov	cl, 2
	call	WAITF			; WAIT 30 TO 45 MICROSECONDS
	;dec	dx			; POINT AT STATUS PORT
	; 06/08/2022
	dec	dl
	in	al, dx			; GET STATUS
	; 16/12/2014
	NEWIODELAY
	;
	test	al, 00010000b		; TEST FOR NEC STILL BUSY
	jz	short POPRES		; RESULTS DONE ?

	dec	bl			; DECREMENT THE STATUS COUNTER
        jnz     short _R10              ; GO BACK FOR MORE
	or	byte [DSKETTE_STATUS], BAD_NEC ; TOO MANY STATUS BYTES
	stc				; SET ERROR FLAG

;-----	RESULT OPERATION IS DONE
POPRES:
	pop	edi
	retn				; RETURN WITH CARRY SET

;-------------------------------------------------------------------------------
; READ_DSKCHNG
;	READS THE STATE OF THE DISK CHANGE LINE.
;
; ON ENTRY:	DI = DRIVE #
;
; ON EXIT:	DI = DRIVE #
;		ZF = 0 : DISK CHANGE LINE INACTIVE
;		ZF = 1 : DISK CHANGE LINE ACTIVE
;		AX,CX,DX DESTROYED
;-------------------------------------------------------------------------------
READ_DSKCHNG:
	call	MOTOR_ON		; TURN ON THE MOTOR IF OFF
	mov	dx, 03F7h		; ADDRESS DIGITAL INPUT REGISTER
	in	al, dx			; INPUT DIGITAL INPUT REGISTER
	test	al, DSK_CHG		; CHECK FOR DISK CHANGE LINE ACTIVE
	retn				; RETURN TO CALLER WITH ZERO FLAG SET

fdc_int:  
	  ; 30/07/2015	
	  ; 16/02/2015
;int_0Eh: ; 11/12/2014

;--- HARDWARE INT 0EH -- ( IRQ LEVEL 6 ) ---------------------------------------
; DISK_INT
;	THIS ROUTINE HANDLES THE DISKETTE INTERRUPT.
;
; ON EXIT:	THE INTERRUPT FLAG IS SET IN @SEEK_STATUS.
;-------------------------------------------------------------------------------
DISK_INT_1:
	;push	AX			; SAVE WORK REGISTER
	; 11/04/2021
	push	eax
	push	ds
	mov	ax, KDATA
	mov 	ds, ax
        or	byte [SEEK_STATUS], INT_FLAG ; TURN ON INTERRUPT OCCURRED
	mov     al, EOI			; END OF INTERRUPT MARKER
	out	INTA00, al		; INTERRUPT CONTROL PORT
	pop	ds
	;pop	ax			; RECOVER REGISTER
	; 11/04/2021
	pop	eax
	iretd				; RETURN FROM INTERRUPT

;-------------------------------------------------------------------------------
; DSKETTE_SETUP
;	THIS ROUTINE DOES A PRELIMINARY CHECK TO SEE WHAT TYPE OF
;	DISKETTE DRIVES ARE ATTACH TO THE SYSTEM.
;-------------------------------------------------------------------------------

; 09/08/2022 - TRDOS 386 Kernel v2.0.5
; 29/05/2016 - TRDOS 386 (TRDOS v2.0)

DSKETTE_SETUP:
	;push	ax			; SAVE REGISTERS
	;push	bx
	;push	cx
	push	edx
	;push	di
	;;push	ds
	; 14/12/2014
	;mov	word [DISK_POINTER], MD_TBL6
	;mov	[DISK_POINTER+2], cs
	;
	;or	byte [RTC_WAIT_FLAG], 1	; NO RTC WAIT, FORCE USE OF LOOP
	xor	edi, edi		; INITIALIZE DRIVE POINTER
	; 09/08/2022
	;mov	esi, eax
	xor	eax, eax ; eax = 0
	mov	[DSK_STATE], ax		; INITIALIZE STATES
	and	byte [LASTRATE], ~(STRT_MSK+SEND_MSK) ; CLEAR START & SEND
	or	byte [LASTRATE], SEND_MSK ; INITIALIZE SENT TO IMPOSSIBLE
	mov	[SEEK_STATUS], al	; INDICATE RECALIBRATE NEEDED
	mov	[MOTOR_COUNT], al	; INITIALIZE MOTOR COUNT
	mov	[MOTOR_STATUS], al	; INITIALIZE DRIVES TO OFF STATE
	mov	[DSKETTE_STATUS], al	; NO ERRORS
	;
	; 28/02/2015
	;mov	word [cfd], 100h 
	call	DSK_RESET
	pop	edx
	clc	; 29/05/2016
	retn

;SUP0:
;	call	DRIVE_DET		; DETERMINE DRIVE
;	call	XLAT_OLD		; TRANSLATE STATE TO COMPATIBLE MODE
;	; 02/01/2015
;	;inc	di			; POINT TO NEXT DRIVE
;	;cmp	di, MAX_DRV		; SEE IF DONE
;	;jnz	short SUP0		; REPEAT FOR EACH ORIVE
;       cmp     byte [fd1_type], 0	
;	jna	short sup1
;	or	di, di
;	jnz	short sup1
;	inc	di
;       jmp     short SUP0
;sup1:
;	mov	byte [SEEK_STATUS], 0	; FORCE RECALIBRATE
;	;and	byte [RTC_WAIT_FLAG], 0FEh ; ALLOW FOR RTC WAIT
;	call	SETUP_END		; VARIOUS CLEANUPS
;	;;pop	ds			; RESTORE CALLERS REGISTERS
;	;pop	di
;	pop	edx
;	;pop	cx
;	;pop	bx
;	;pop	ax
;	retn

;//////////////////////////////////////////////////////
;; END OF DISKETTE I/O ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 17/04/2021 (TRDOS 386 v2.0.4) 

; 11/04/2021
;int13h: ; 21/02/2015
	;pushfd
	;push 	cs
	;;call 	DISK_IO
	;;retn

;;;;;; DISK I/O ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 21/02/2015 ;;;
;/////////////////////////////////////////////////////////////////////

; DISK I/O - Erdogan Tan (Retro UNIX 386 v1 project)
; 18/02/2016
; 17/02/2016
; 23/02/2015
; 21/02/2015 (unix386.s)
; 22/12/2014 - 14/02/2015 (dsectrm2.s)
;
; Original Source Code:
; DISK ----- 09/25/85 FIXED DISK BIOS
; (IBM PC XT Model 286 System BIOS Source Code, 04-21-86)
;
; Modifications: by reference of AWARD BIOS 1999 (D1A0622) 
;		 Source Code - ATORGS.ASM, AHDSK.ASM
;

;The wait for controller to be not busy is 10 seconds.
;10,000,000 / 30 = 333,333. 333,333 decimal = 051615h
;;WAIT_HDU_CTLR_BUSY_LO	equ 1615h		
;;WAIT_HDU_CTLR_BUSY_HI	equ 05h
WAIT_HDU_CTRL_BUSY_LH	equ 51615h	; 21/02/2015		

;The wait for controller to issue completion interrupt is 10 seconds.
;10,000,000 / 30 = 333,333.  333,333 decimal = 051615h
;;WAIT_HDU_INT_LO	equ 1615h
;;WAIT_HDU_INT_HI	equ 05h
WAIT_HDU_INT_LH		equ 51615h	; 21/02/2015

;The wait for Data request on read and write longs is
;2000 us. (?)
;;WAIT_HDU_DRQ_LO	equ 1000	; 03E8h
;;WAIT_HDU_DRQ_HI	equ 0
WAIT_HDU_DRQ_LH		equ 1000	; 21/02/2015

; Port 61h (PORT_B)
SYS1	equ 61h		; PORT_B  (diskette.inc)

; 23/12/2014
%define CMD_BLOCK       ebp-8  ; 21/02/2015

;--- INT 13H -------------------------------------------------------------------
;									       :
; FIXED DISK I/O INTERFACE						       :
;									       :
;	THIS INTERFACE PROVIDES ACCESS TO 5 1/4" FIXED DISKS THROUGH           :
;	THE IBM FIXED DISK CONTROLLER.					       :
;									       :
;	THE  BIOS  ROUTINES  ARE  MEANT  TO  BE  ACCESSED  THROUGH	       :
;	SOFTWARE  INTERRUPTS  ONLY.    ANY  ADDRESSES  PRESENT	IN	       :
;	THESE  LISTINGS  ARE  INCLUDED	 ONLY	FOR  COMPLETENESS,	       :
;	NOT  FOR  REFERENCE.  APPLICATIONS   WHICH  REFERENCE  ANY	       :
;	ABSOLUTE  ADDRESSES  WITHIN  THE  CODE	SEGMENTS  OF  BIOS	       :
;	VIOLATE  THE  STRUCTURE  AND  DESIGN  OF  BIOS. 		       :
;									       :
;------------------------------------------------------------------------------:
;									       :
; INPUT  (AH)= HEX COMMAND VALUE					       :
;									       :
;	(AH)= 00H  RESET DISK (DL = 80H,81H) / DISKETTE 		       :
;	(AH)= 01H  READ THE STATUS OF THE LAST DISK OPERATION INTO (AL)        :
;		    NOTE: DL < 80H - DISKETTE				       :
;			  DL > 80H - DISK				       :
;	(AH)= 02H  READ THE DESIRED SECTORS INTO MEMORY 		       :
;	(AH)= 03H  WRITE THE DESIRED SECTORS FROM MEMORY		       :
;	(AH)= 04H  VERIFY THE DESIRED SECTORS				       :
;	(AH)= 05H  FORMAT THE DESIRED TRACK				       :
;	(AH)= 06H  UNUSED						       :
;	(AH)= 07H  UNUSED						       :
;	(AH)= 08H  RETURN THE CURRENT DRIVE PARAMETERS			       :
;	(AH)= 09H  INITIALIZE DRIVE PAIR CHARACTERISTICS		       :
;		    INTERRUPT 41 POINTS TO DATA BLOCK FOR DRIVE 0	       :
;		    INTERRUPT 46 POINTS TO DATA BLOCK FOR DRIVE 1	       :
;	(AH)= 0AH  READ LONG						       :
;	(AH)= 0BH  WRITE LONG  (READ & WRITE LONG ENCOMPASS 512 + 4 BYTES ECC) :
;	(AH)= 0CH  SEEK 						       :
;	(AH)= 0DH  ALTERNATE DISK RESET (SEE DL)			       :
;	(AH)= 0EH  UNUSED						       :
;	(AH)= 0FH  UNUSED						       :
;	(AH)= 10H  TEST DRIVE READY					       :
;	(AH)= 11H  RECALIBRATE						       :
;	(AH)= 12H  UNUSED						       :
;	(AH)= 13H  UNUSED						       :
;	(AH)= 14H  CONTROLLER INTERNAL DIAGNOSTIC			       :
;	(AH)= 15H  READ DASD TYPE					       :
;									       :
;-------------------------------------------------------------------------------
;									       :
;	REGISTERS USED FOR FIXED DISK OPERATIONS			       :
;									       :
;		(DL)	-  DRIVE NUMBER     (80H-81H FOR DISK. VALUE CHECKED)  :
;		(DH)	-  HEAD NUMBER	    (0-15 ALLOWED, NOT VALUE CHECKED)  :
;		(CH)	-  CYLINDER NUMBER  (0-1023, NOT VALUE CHECKED)(SEE CL):
;		(CL)	-  SECTOR NUMBER    (1-17, NOT VALUE CHECKED)	       :
;									       :
;			   NOTE: HIGH 2 BITS OF CYLINDER NUMBER ARE PLACED     :
;				 IN THE HIGH 2 BITS OF THE CL REGISTER	       :
;				 (10 BITS TOTAL)			       :
;									       :
;		(AL)	-  NUMBER OF SECTORS (MAXIMUM POSSIBLE RANGE 1-80H,    :
;					      FOR READ/WRITE LONG 1-79H)       :
;									       :
;		(ES:BX) -  ADDRESS OF BUFFER FOR READS AND WRITES,	       :
;			   (NOT REQUIRED FOR VERIFY)			       :
;									       :
;		FORMAT (AH=5) ES:BX POINTS TO A 512 BYTE BUFFER. THE FIRST     :
;			   2*(SECTORS/TRACK) BYTES CONTAIN F,N FOR EACH SECTOR.:
;			   F = 00H FOR A GOOD SECTOR			       :
;			       80H FOR A BAD SECTOR			       :
;			   N = SECTOR NUMBER				       :
;			   FOR AN INTERLEAVE OF 2 AND 17 SECTORS/TRACK	       :
;			   THE TABLE SHOULD BE: 			       :
;									       :
;		   DB	   00H,01H,00H,0AH,00H,02H,00H,0BH,00H,03H,00H,0CH     :
;		   DB	   00H,04H,00H,0DH,00H,05H,00H,0EH,00H,06H,00H,0FH     :
;		   DB	   00H,07H,00H,10H,00H,08H,00H,11H,00H,09H	       :
;									       :
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; OUTPUT								       :
;	AH = STATUS OF CURRENT OPERATION				       :
;	     STATUS BITS ARE DEFINED IN THE EQUATES BELOW		       :
;	CY = 0	SUCCESSFUL OPERATION (AH=0 ON RETURN)			       :
;	CY = 1	FAILED OPERATION (AH HAS ERROR REASON)			       :
;									       :
;	NOTE:	ERROR 11H  INDICATES THAT THE DATA READ HAD A RECOVERABLE      :
;		ERROR WHICH WAS CORRECTED BY THE ECC ALGORITHM.  THE DATA      :
;		IS PROBABLY GOOD,   HOWEVER THE BIOS ROUTINE INDICATES AN      :
;		ERROR TO ALLOW THE CONTROLLING PROGRAM A CHANCE TO DECIDE      :
;		FOR ITSELF.  THE  ERROR  MAY  NOT  RECUR  IF  THE DATA IS      :
;		REWRITTEN.						       :
;									       :
;	IF DRIVE PARAMETERS WERE REQUESTED (DL >= 80H), 		       :
;	   INPUT:							       :
;	     (DL) = DRIVE NUMBER					       :	
;	     ; 27/05/2016 - TRDOS 386 (TRDOS v2.0)						       :	 	
;	     EBX = Buffer address for fixed disk parameters table (32 bytes)   :
;	   OUTPUT:							       :
;	     (DL) = NUMBER OF CONSECUTIVE ACKNOWLEDGING DRIVES ATTACHED (1-2)  :
;		    (CONTROLLER CARD ZERO TALLY ONLY)			       :
;	     (DH) = MAXIMUM USEABLE VALUE FOR HEAD NUMBER		       :
;	     (CH) = MAXIMUM USEABLE VALUE FOR CYLINDER NUMBER		       :
;	     (CL) = MAXIMUM USEABLE VALUE FOR SECTOR NUMBER		       :
;		    AND CYLINDER NUMBER HIGH BITS			       :
;									       :
;	IF READ DASD TYPE WAS REQUESTED,				       :
;									       :
;	AH = 0 - NOT PRESENT						       :
;	     1 - DISKETTE - NO CHANGE LINE AVAILABLE			       :
;	     2 - DISKETTE - CHANGE LINE AVAILABLE			       :
;	     3 - FIXED DISK						       :
;									       :
;	CX,DX = NUMBER OF 512 BYTE BLOCKS WHEN AH = 3			       :
;									       :
;	REGISTERS WILL BE PRESERVED EXCEPT WHEN THEY ARE USED TO RETURN        :
;	INFORMATION.							       :
;									       :
;	NOTE: IF AN ERROR IS REPORTED BY THE DISK CODE, THE APPROPRIATE        :
;		ACTION IS TO RESET THE DISK, THEN RETRY THE OPERATION.	       :
;									       :
;-------------------------------------------------------------------------------

SENSE_FAIL	EQU	0FFH		; NOT IMPLEMENTED
NO_ERR		EQU	0E0H		; STATUS ERROR/ERROR REGISTER=0
WRITE_FAULT	EQU	0CCH		; WRITE FAULT ON SELECTED DRIVE
UNDEF_ERR	EQU	0BBH		; UNDEFINED ERROR OCCURRED
NOT_RDY 	EQU	0AAH		; DRIVE NOT READY
TIME_OUT	EQU	80H		; ATTACHMENT FAILED TO RESPOND
BAD_SEEK	EQU	40H		; SEEK OPERATION FAILED
BAD_CNTLR	EQU	20H		; CONTROLLER HAS FAILED
DATA_CORRECTED	EQU	11H		; ECC CORRECTED DATA ERROR
BAD_ECC 	EQU	10H		; BAD ECC ON DISK READ
BAD_TRACK	EQU	0BH		; NOT IMPLEMENTED
BAD_SECTOR	EQU	0AH		; BAD SECTOR FLAG DETECTED
;DMA_BOUNDARY	EQU	09H		; DATA EXTENDS TOO FAR
INIT_FAIL	EQU	07H		; DRIVE PARAMETER ACTIVITY FAILED
BAD_RESET	EQU	05H		; RESET FAILED
;RECORD_NOT_FND	EQU	04H		; REQUESTED SECTOR NOT FOUND
;BAD_ADDR_MARK	EQU	02H		; ADDRESS MARK NOT FOUND
;BAD_CMD 	EQU	01H		; BAD COMMAND PASSED TO DISK I/O

;--------------------------------------------------------
;							:
; FIXED DISK PARAMETER TABLE				:
;  -  THE TABLE IS COMPOSED OF A BLOCK DEFINED AS:	:
;							:
;  +0	(1 WORD) - MAXIMUM NUMBER OF CYLINDERS		:
;  +2	(1 BYTE) - MAXIMUM NUMBER OF HEADS		:
;  +3	(1 WORD) - NOT USED/SEE PC-XT			:
;  +5	(1 WORD) - STARTING WRITE PRECOMPENSATION CYL	:
;  +7	(1 BYTE) - MAXIMUM ECC DATA BURST LENGTH	:
;  +8	(1 BYTE) - CONTROL BYTE 			:
;		   BIT	  7 DISABLE RETRIES -OR-	:
;		   BIT	  6 DISABLE RETRIES		:
;		   BIT	  3 MORE THAN 8 HEADS		:
;  +9	(3 BYTES)- NOT USED/SEE PC-XT			:
; +12	(1 WORD) - LANDING ZONE 			:
; +14	(1 BYTE) - NUMBER OF SECTORS/TRACK		:
; +15	(1 BYTE) - RESERVED FOR FUTURE USE		:
;							:
;	 - TO DYNAMICALLY DEFINE A SET OF PARAMETERS	:
;	   BUILD A TABLE FOR UP TO 15 TYPES AND PLACE	:
;	   THE CORRESPONDING VECTOR INTO INTERRUPT 41	:
;	   FOR DRIVE 0 AND INTERRUPT 46 FOR DRIVE 1.	:
;							:
;--------------------------------------------------------

;--------------------------------------------------------
;							:
; HARDWARE SPECIFIC VALUES				:
;							:
;  -  CONTROLLER I/O PORT				:
;							:
;     > WHEN READ FROM: 				:
;	HF_PORT+0 - READ DATA (FROM CONTROLLER TO CPU)	:
;	HF_PORT+1 - GET ERROR REGISTER			:
;	HF_PORT+2 - GET SECTOR COUNT			:
;	HF_PORT+3 - GET SECTOR NUMBER			:
;	HF_PORT+4 - GET CYLINDER LOW			:
;	HF_PORT+5 - GET CYLINDER HIGH (2 BITS)		:
;	HF_PORT+6 - GET SIZE/DRIVE/HEAD 		:
;	HF_PORT+7 - GET STATUS REGISTER 		:
;							:
;     > WHEN WRITTEN TO:				:
;	HF_PORT+0 - WRITE DATA (FROM CPU TO CONTROLLER) :
;	HF_PORT+1 - SET PRECOMPENSATION CYLINDER	:
;	HF_PORT+2 - SET SECTOR COUNT			:
;	HF_PORT+3 - SET SECTOR NUMBER			:
;	HF_PORT+4 - SET CYLINDER LOW			:
;	HF_PORT+5 - SET CYLINDER HIGH (2 BITS)		:
;	HF_PORT+6 - SET SIZE/DRIVE/HEAD 		:
;	HF_PORT+7 - SET COMMAND REGISTER		:
;							:
;--------------------------------------------------------

;HF_PORT 	EQU	01F0H	; DISK PORT
;HF1_PORT	equ	0170h	
;HF_REG_PORT	EQU	03F6H
;HF1_REG_PORT	equ	0376h

HDC1_BASEPORT	equ	1F0h
HDC2_BASEPORT	equ	170h		

align 2

;-----		STATUS REGISTER

ST_ERROR	EQU	00000001B	;
ST_INDEX	EQU	00000010B	;
ST_CORRCTD	EQU	00000100B	; ECC CORRECTION SUCCESSFUL
ST_DRQ		EQU	00001000B	;
ST_SEEK_COMPL	EQU	00010000B	; SEEK COMPLETE
ST_WRT_FLT	EQU	00100000B	; WRITE FAULT
ST_READY	EQU	01000000B	;
ST_BUSY 	EQU	10000000B	;

;-----		ERROR REGISTER

ERR_DAM 	EQU	00000001B	; DATA ADDRESS MARK NOT FOUND
ERR_TRK_0	EQU	00000010B	; TRACK 0 NOT FOUND ON RECAL
ERR_ABORT	EQU	00000100B	; ABORTED COMMAND
;		EQU	00001000B	; NOT USED
ERR_ID		EQU	00010000B	; ID NOT FOUND
;		EQU	00100000B	; NOT USED
ERR_DATA_ECC	EQU	01000000B
ERR_BAD_BLOCK	EQU	10000000B


RECAL_CMD	EQU	00010000B	; DRIVE RECAL	(10H)
READ_CMD	EQU	00100000B	;	READ	(20H)
WRITE_CMD	EQU	00110000B	;	WRITE	(30H)
VERIFY_CMD	EQU	01000000B	;	VERIFY	(40H)
FMTTRK_CMD	EQU	01010000B	; FORMAT TRACK	(50H)
INIT_CMD	EQU	01100000B	;   INITIALIZE	(60H)
SEEK_CMD	EQU	01110000B	;	SEEK	(70H)
DIAG_CMD	EQU	10010000B	; DIAGNOSTIC	(90H)
SET_PARM_CMD	EQU	10010001B	; DRIVE PARMS	(91H)
NO_RETRIES	EQU	00000001B	; CHD MODIFIER	(01H)
ECC_MODE	EQU	00000010B	; CMD MODIFIER	(02H)
BUFFER_MODE	EQU	00001000B	; CMD MODIFIER	(08H)

;MAX_FILE	EQU	2
;S_MAX_FILE	EQU	2
MAX_FILE	equ	4		; 22/12/2014
S_MAX_FILE	equ	4		; 22/12/2014

DELAY_1 	EQU	25H		; DELAY FOR OPERATION COMPLETE
DELAY_2 	EQU	0600H		; DELAY FOR READY
DELAY_3 	EQU	0100H		; DELAY FOR DATA REQUEST

HF_FAIL 	EQU	08H		; CMOS FLAG IN BYTE 0EH

;-----		COMMAND BLOCK REFERENCE

;CMD_BLOCK      EQU     BP-8            ; @CMD_BLOCK REFERENCES BLOCK HEAD IN SS
					;  (BP) POINTS TO COMMAND BLOCK TAIL
					;	AS DEFINED BY THE "ENTER" PARMS
; 19/12/2014
ORG_VECTOR	equ	4*13h		; INT 13h vector
DISK_VECTOR	equ	4*40h		; INT 40h vector (for floppy disks)
;HDISK_INT	equ	4*76h		; Primary HDC - Hardware interrupt (IRQ14)
;HDISK_INT1	equ	4*76h		; Primary HDC - Hardware interrupt (IRQ14)
;HDISK_INT2	equ	4*77h		; Secondary HDC - Hardware interrupt (IRQ15)
;HF_TBL_VEC	equ	4*41h		; Pointer to 1st fixed disk parameter table
;HF1_TBL_VEC	equ	4*46h		; Pointer to 2nd fixed disk parameter table

align 2

;----------------------------------------------------------------
; FIXED DISK I/O SETUP						:
;								:
;  -  ESTABLISH TRANSFER VECTORS FOR THE FIXED DISK		:
;  -  PERFORM POWER ON DIAGNOSTICS				:
;     SHOULD AN ERROR OCCUR A "1701" MESSAGE IS DISPLAYED       :
;								:
;----------------------------------------------------------------

; 09/08/2022
; 06/08/2022 - TRDOS 386 Kernel v2.0.5
; 29/05/2016 - TRDOS 386 (TRDOS v2.0)

DISK_SETUP:
	;CLI
	;;mov	ax, ABS0 		; GET ABSOLUTE SEGMENT
	;xor	ax, ax
	;mov	ds, ax			; SET SEGMENT REGISTER
	;mov	ax, [ORG_VECTOR] 	; GET DISKETTE VECTOR
	;mov	[DISK_VECTOR], ax	;  INTO INT 40H
	;mov	ax, [ORG_VECTOR+2]
	;mov	[DISK_VECTOR+2], ax
	;mov	word [ORG_VECTOR], DISK_IO ; FIXED DISK HANDLER
	;mov	[ORG_VECTOR+2], cs
	; 1st controller (primary master, slave)   - IRQ 14
	;;mov	word [HDISK_INT], HD_INT   ; FIXED DISK INTERRUPT
	;mov	word [HDISK_INT1], HD_INT  ;
	;;mov	[HDISK_INT+2], cs
	;mov	[HDISK_INT1+2], cs
	; 2nd controller (secondary master, slave) - IRQ 15
	;mov	word [HDISK_INT2], HD1_INT ;
	;mov	[HDISK_INT2+2], cs
	;
	;;mov	word [HF_TBL_VEC], HD0_DPT	; PARM TABLE DRIVE 80
	;;mov	word [HF_TBL_VEC+2], DPT_SEGM
	;;mov	word [HF1_TBL_VEC], HD1_DPT	; PARM TABLE DRIVE 81
	;;mov	word [HF1_TBL_VEC+2], DPT_SEGM
	;push	cs
	;pop	ds
	;mov	word [HDPM_TBL_VEC], HD0_DPT	; PARM TABLE DRIVE 80h
	;mov	word [HDPM_TBL_VEC+2], DPT_SEGM
	mov 	dword [HDPM_TBL_VEC], (DPT_SEGM*16)+HD0_DPT
	;mov	word [HDPS_TBL_VEC], HD1_DPT	; PARM TABLE DRIVE 81h
	;mov	word [HDPS_TBL_VEC+2], DPT_SEGM
	mov 	dword [HDPS_TBL_VEC], (DPT_SEGM*16)+HD1_DPT
	;mov	word [HDSM_TBL_VEC], HD2_DPT	; PARM TABLE DRIVE 82h
	;mov	word [HDSM_TBL_VEC+2], DPT_SEGM
	mov 	dword [HDSM_TBL_VEC], (DPT_SEGM*16)+HD2_DPT
	;mov	word [HDSS_TBL_VEC], HD3_DPT	; PARM TABLE DRIVE 83h
	;mov	word [HDSS_TBL_VEC+2], DPT_SEGM
	mov 	dword [HDSS_TBL_VEC], (DPT_SEGM*16)+HD3_DPT
	;
	;;in	al, INTB01		; TURN ON SECOND INTERRUPT CHIP
	;;;and	al, 0BFh
	;;and	al, 3Fh			; enable IRQ 14 and IRQ 15
	;;;jmp	$+2
	;;IODELAY
	;;out	INTB01, al
	;;IODELAY
	;;in	al, INTA01		; LET INTERRUPTS PASS THRU TO
	;;and	al, 0FBh 		;  SECOND CHIP
	;;;jmp	$+2
	;;IODELAY
	;;out	INTA01, al
	;
	;sti
	;;push	ds			; MOVE ABS0 POINTER TO
	;;pop	es			; EXTRA SEGMENT POINTER
	;;;call	DDS			; ESTABLISH DATA SEGMENT
	;;mov	byte [DISK_STATUS1], 0 	; RESET THE STATUS INDICATOR
	;;mov	byte [HF_NUM], 0	; ZERO NUMBER OF FIXED DISKS
	;;mov	byte [CONTROL_BYTE], 0
	;;mov	byte [PORT_OFF], 0	; ZERO CARD OFFSET
	; 20/12/2014 - private code by Erdogan Tan
		      ; (out of original PC-AT, PC-XT BIOS code)
	;mov	si, hd0_type
	mov	esi, hd0_type
	;;mov	cx, 4
	;mov	ecx, 4
	; 06/08/2022
	sub	ecx, ecx
	mov	cl, 4
hde_l:
	lodsb
	cmp	al, 80h			; 8?h = existing
	jb	short _L4
	inc	byte [HF_NUM]		; + 1 hard (fixed) disk drives
_L4: ; 26/02/2015
	loop	hde_l	
;_L4:					; 0 <= [HF_NUM] =< 4
;L4:
	;; 31/12/2014 - cancel controller diagnostics here
	;;;mov 	cx, 3  ; 26/12/2014 (Award BIOS 1999)
	;;mov 	cl, 3
	;;
	;;mov	DL, 80H			; CHECK THE CONTROLLER
;;hdc_dl:
	;;mov	AH, 14H			; USE CONTROLLER DIAGNOSTIC COMMAND
	;;INT	13H			; CALL BIOS WITH DIAGNOSTIC COMMAND
	;;;jc	short CTL_ERRX		; DISPLAY ERROR MESSAGE IF BAD RETURN
	;;;jc	short POD_DONE ;22/12/2014
	;;jnc	short hdc_reset0
	;;loop	hdc_dl
	;;; 27/12/2014
	;;stc
	;;retn
	;
;;hdc_reset0:
	; 18/01/2015
	mov	cl, [HF_NUM]
	and	cl, cl
	jz	short POD_DONE
	;
	mov	dl, 7Fh
hdc_reset1:
	inc	dl
	;; 31/12/2015
	;;push	dx
	;;push	cx
	;;push	ds
	;;sub	ax, ax
	;;mov	ds, ax
	;;mov	ax, [TIMER_LOW]		; GET START TIMER COUNTS
	;;pop	ds
	;;mov	bx, ax
	;;add	ax, 6*182		; 60 SECONDS* 18.2
	;;mov	cx, ax
	;;mov	word [wait_count], 0	; 22/12/2014 (reset wait counter)
	;;
	;; 31/12/2014 - cancel HD_RESET_1
	;;call	HD_RESET_1		; SET UP DRIVE 0, (1,2,3)
	;;pop	cx
	;;pop	dx
	;;
	; 18/01/2015
	mov	ah, 0Dh ; ALTERNATE RESET
	;int	13h
	call	int13h
	loop	hdc_reset1
	;clc 	; 29/05/2016
POD_DONE:
	retn

;;-----	POD_ERROR

;;CTL_ERRX:
;	;mov	SI,OFFSET F1782 	; CONTROLLER ERROR
;	;call	SET_FAIL		; DO NOT IPL FROM DISK
;	;call	E_MSG			; DISPLAY ERROR AND SET (BP) ERROR FLAG
;	;jmp	short POD_DONE

;;HD_RESET_1:
;;	;push	BX			; SAVE TIMER LIMITS
;;	;push	CX
;;RES_1: mov	AH,09H			; SET DRIVE PARAMETERS
;;	INT	13H
;;	jc	short RES_2
;;	mov	AH,11h			; RECALIBRATE DRIVE
;;	INT	13H
;;	jnc	short RES_CK		; DRIVE OK
;;RES_2: ;call	POD_TCHK		; CHECK TIME OUT
;;	cmp	word [wait_count], 6*182 ; waiting time (in timer ticks)
;;					; (30 seconds)		
;;	;cmc
;;	;jnc	short RES_1
;;	jb	short RES_1
;;;RES_FL: ;mov	SI,OFFSET F1781 	; INDICATE DISK 1 FAILURE;
;;	;test	DL,1
;;	;jnz	short RES_E1
;;	;mov	SI,OFFSET F1780 	; INDICATE DISK 0 FAILURE
;;	;call	SET_FAIL		; DO NOT TRY TO IPL DISK 0
;;	;jmp	SHORT RES_E1
;;RES_ER: ; 22/12/2014
;;RES_OK:
;;	;pop	CX			; RESTORE TIMER LIMITS
;;	;pop	BX
;;	retn
;;
;;RES_RS: mov	AH,00H			; RESET THE DRIVE
;;	INT	13H
;;RES_CK: mov	AH,08H			; GET MAX CYLINDER,HEAD,SECTOR
;;	mov	BL,DL			; SAVE DRIVE CODE
;;	INT	13H
;;	jc	short RES_ER
;;	mov	[NEC_STATUS],CX 	; SAVE MAX CYLINDER, SECTOR
;;	mov	DL,BL			; RESTORE DRIVE CODE
;;RES_3: mov	AX,0401H		; VERIFY THE LAST SECTOR
;;	INT	13H
;;	jnc	short RES_OK		; VERIFY OK
;;	cmp	AH,BAD_SECTOR		; OK ALSO IF JUST ID READ
;;	JE	short RES_OK
;;	cmp	AH,DATA_CORRECTED
;;	JE	short RES_OK
;;	cmp	AH,BAD_ECC
;;	JE	short RES_OK
;;	;call	POD_TCHK		; CHECK FOR TIME OUT
;;	cmp	word [wait_count], 6*182 ; waiting time (in timer ticks)
;;					; (60 seconds)		
;;	cmc
;;	jc	short RES_ER		; FAILED
;;	mov	CX,[NEC_STATUS] 	; GET SECTOR ADDRESS, AND CYLINDER
;;	mov	AL,CL			; SEPARATE OUT SECTOR NUMBER
;;	and	AL,3FH
;;	dec	AL			; TRY PREVIOUS ONE
;;	jz	short RES_RS		; WE'VE TRIED ALL SECTORS ON TRACK
;;	and	CL,0C0H 		; KEEP CYLINDER BITS
;;	OR	CL,AL			; MERGE SECTOR WITH CYLINDER BITS
;;	mov	[NEC_STATUS],CX 	; SAVE CYLINDER, NEW SECTOR NUMBER
;;	jmp	short RES_3		; TRY AGAIN
;;;RES_ER: mov	SI,OFFSET F1791 	; INDICATE DISK 1 ERROR
;;	;test	DL,1
;;	;jnz	short RES_E1
;;	;mov	SI,OFFSET F1790 	; INDICATE DISK 0 ERROR
;;;RES_E1:
;;	;call	E_MSG			; DISPLAY ERROR AND SET (BP) ERROR FLAG
;;;RES_OK:
;;	;pop	CX			; RESTORE TIMER LIMITS
;;	;pop	BX
;;	;retn
;
;;SET_FAIL:
;	;mov	AX,X*(CMOS_DIAG+NMI)	; GET CMOS ERROR BYTE
;	;call	CMOS_READ
;	;OR	AL,HF_FAIL		; SET DO NOT IPL FROM DISK FLAG
;	;xchg	AH,AL			; SAVE IT
;	;call	CMOS_WRITE		; PUT IT OUT
;	;retn
;
;;POD_TCHK:				; CHECK FOR 30 SECOND TIME OUT
;	;pop	AX			; SAVE RETURN
;	;pop	CX			; GET TIME OUT LIMITS
;	;pop	BX
;	;push	BX			; AND SAVE THEM AGAIN
;	;push	CX
;	;push	AX
;	;push	ds
;	;xor	ax, ax
;	;mov	ds, ax			; RESTORE RETURN
;	;mov	AX, [TIMER_LOW]		; AX = CURRENT TIME
;	;				; BX = START TIME
;	;				; CX = END TIME
;	;pop	ds
;	;cmp	BX,CX
;	;JB	short TCHK1		; START < END
;	;cmp	BX,AX
;	;JB	short TCHKG		; END < START < CURRENT
;	;jmp	SHORT TCHK2		; END, CURRENT < START
;;TCHK1: cmp	AX,BX
;;	JB	short TCHKNG		; CURRENT < START < END
;;TCHK2: cmp	AX,CX
;;	JB	short TCHKG		; START < CURRENT < END
;;					; OR CURRENT < END < START
;;TCHKNG: STC				; CARRY SET INDICATES TIME OUT
;;	retn
;;TCHKG: CLC				; INDICATE STILL TIME
;;	retn
;;
;;int_13h:

;----------------------------------------
;	FIXED DISK BIOS ENTRY POINT	:
;----------------------------------------

; 17/07/2022
; 16/07/2022
; 13/07/2022 - TRDOS 386 v2.0.5
; 15/01/2017
; 14/01/2017
; 07/01/2017
; 02/01/2017
; 01/06/2016
; 16/05/2016, 27/05/2016, 28/05/2016, 29/05/2016
; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
int33h: ; DISK I/O
	; 29/05/2016
	and	byte [esp+8], 11111110b  ; clear carry bit of eflags register

	; 13/07/2022
	push	es
	; 16/05/2016
	push	ds
	push	ebx ; user's buffer address (virtual)
	; 13/07/2022
	push	ecx
	push	edx
	push	esi
	push	edi

	;mov	bx, KDATA ; System (Kernel's) data segment
	;mov	ds, bx
	; 13/07/2022
	mov	di, KDATA
	mov	ds, di
	mov	es, di

	;;15/01/2017
	; 14/01/2017
	; 02/01/2017
	;;mov	byte [intflg], 33h  ; disk io interrupt 
	;pop	ebx

	; 13/07/2022
	;pop	dword [user_buffer] ; 01/06/2016
	mov	[user_buffer], ebx		

	mov	byte [scount], 0 ; sector count for transfer
	cmp	ah, 03h ; chs write
	ja	short int33h_2
	je	short int33h_0
	cmp	ah, 02h ; chs read
	jb	short int33h_5
	jmp	short int33h_4
int33h_0:
	;; 17/07/2022 - 64K r/w buffer limit check ?
	;cmp	al, 80h ; 128	
	;ja	short int33h_8 ; error
	;; 17/07/2022 - zero r/w count check ?
	;or	al, al
	;jz	short int33h_8 ; error
	
	; 17/07/2022 (buffer limit and zero count check)
	dec	al
	js	short int33h_8 ; error
	inc	al	

	; transfer user's buffer content to sector buffer
	push	ecx
	movzx	ecx, al
int33h_1:
	; 13/07/2022
	;push	esi
	;mov	esi, [user_buffer]
	mov	esi, ebx
	; esi = user's buffer address (virtual, ebx)
	;push	edi
	;push	es
	push	eax
	;mov	ax, KDATA
	;mov	es, ax
	mov	edi, Cluster_Buffer
	shl	ecx, 9 ; * 512
	call	transfer_from_user_buffer
		; (ecx and eax will be modified)
	pop	eax
	; 13/07/2022
	;pop	es
	;pop	edi
	;pop	esi
	pop	ecx
	jnc	short int33h_5

	;mov	ebx, [user_buffer] ; 01/06/2016
	;pop	ds

int33h_8:
	; 13/07/2022
	mov	eax, 0FFh ; Unknown error !?
int33h_9:
	pop	edi
	pop	esi
	pop	edx
	pop	ecx
	pop	ebx
	pop	ds
	pop	es

	; 13/07/2022
	jnc	short int33h_7

	;;15/01/2017
	; 02/01/2017
	;cli
	;;mov	byte [ss:intflg], 0 ; 07/01/2017
	;
	; (*) 29/05/2016
	; (*) retf 4 ; skip eflags on stack

	; 29/05/2016 -set carry flag on stack-
	; [esp] = EIP
	; [esp+4] = CS
	; [esp+8] = E-FLAGS
	or	byte [esp+8], 1  ; set carry bit of eflags register
	; [esp+12] = ESP (user)
	; [esp+16] = SS (User)
	;
	; 13/07/2022
int33h_7:	
	cli
	;;15/01/2017
	;;mov	byte [ss:intflg], 0 ; 07/01/2017
	; cf = 0  ; use eflags which is in stack
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
 	;    Increment ESP by 4 plus the immediate offset if it exists;
	;    Load SS:ESP from stack;
 	; FI;
	;
	; //

int33h_2:
	cmp	ah, 05h ; format track
	ja	short int33h_3
	jb	short int33h_5
	push	ecx
	;mov	ecx, 1
	; 17/07/2022
	xor	ecx, ecx
	inc	cl
	; ecx = 1
	jmp	short int33h_1
int33h_3:
	cmp	ah, 1Ch ; LBA write
	ja	short int33h_5
	je	short int33h_0
	cmp	ah, 1Bh ; LBA read
	je	short int33h_4
	cmp	ah, 08h ; get disk parameters
	jne	short int33h_5
	; 01/06/2016
	mov	ebx, [user_buffer] ; user's buffer address
	jmp	short int33h_6
int33h_4:
	;; 17/07/2022 - 64K r/w buffer limit check ?
	;cmp	al, 80h ; 128	
	;ja	short int33h_8 ; error
	;; 17/07/2022 - zero r/w count check ?
	;or	al, al
	;jz	short int33h_8 ; error

	; 17/07/2022 (buffer limit and zero count check)
	dec	al
	js	short int33h_8 ; error
	inc	al

	mov	byte [scount], al ; <= 128 sectors
int33h_5:
	mov	ebx, Cluster_Buffer ; max. 65536 bytes
				    ; buf. addr: 70000h	
	;mov	byte [ClusterBuffer_Valid], 0
int33h_6:
	; 13/07/2022
	;pop	ds
	;pushfd
	;push 	cs
	
	call 	DISK_IO
	
	;;mov	ebx, [cs:user_buffer] ; 01/06/2016
	;mov	ebx, [user_buffer] ; 13/07/2022 
	jc	short int33h_9
	;
	;cmp	byte [cs:scount], 0
	;jna	short int33h_7	
	cmp	byte [scount], 0 ; 13/07/2022
	jna	short int33h_9

	; 13/07/2022

	; transfer sector buffer content to user's buffer
	;push	es
	;push	ds
	;push	eax
	;mov	ax, KDATA
	;mov	ds, ax
	;mov	es, ax
	;push	ecx
	;push	esi
	;push	edi
	
	; 13/07/2022
	push	eax
	movzx	ecx, byte [scount]
	shl	ecx, 9 ; * 512 bytes
	;mov	edi, ebx ; user's buffer address
	mov	edi, [user_buffer] ; 13/07/2022
	mov	esi, Cluster_Buffer
	call	transfer_to_user_buffer 
		; (ecx and eax will be modified)
	pop	eax

	; 13/07/2022
	jc	short int33h_8 ; eax = 0FFh
	jmp	short int33h_9 ; cf = 0

	;pop	edi
	;pop	esi
	;pop	ecx
	;pop	eax
	;pop	ds
	;pop	es
	;jc	short int33h_8
;int33h_7:
	;cli
	;;;15/01/2017
	;;;mov	byte [ss:intflg], 0 ; 07/01/2017
	;; cf = 0  ; use eflags which is in stack
	;iretd	
;int33h_8:
	;mov	eax, 0FFh ; Unknown error !?
	; 13/07/2022
	;xor	eax, eax
	;dec	al  ; eax = 0FFh	
	;jmp	short int33h_9
	 
;int33h_9:
	;; cf = 1
	;
	;; (*) 29/05/2016	
	;; (*) retf 4 ; skip eflags on stack
	;; Note: This 'retf 4' was wrong, -it was causing
	;;       to stack errors in ring 3-
	;;	POP sequence of 'retf 4' is as
	;;       "eip, cs, eflags, esp, ss, +4 bytes" 
        ;;       it is not as "eip, cs, +4 bytes, esp, ss" ! 
	;
	;; 29/05/2016 -set carry flag on stack-
	;or	byte [esp+8], 1  ; set carry bit of eflags register
	;;iretd
	;jmp	short int33h_7 ; 07/01/2017

;; 11/04/2021
;int13h: ; 21/02/2015
;	clc ; 11/04/2021
;	pushfd
;	push 	cs
;	call 	DISK_IO
;	retn

int13h:
	; 13/07/2022 - TRDOS 386 v2.0.5
	; Note: DISK_IO sets registers on stack
	; 	as return parameters. So,
	;	stack order (at the entry of 'DISK_IO')
	;	must be same with 'int33h:' as above.
		
	;push	es ; not necessary
	;push	ds ; not necessary
	;
	; following pushes are necessary
	; for setting registers -return values- in DISK_IO
	push	ebx
	push	ecx
	push	edx
	push	esi
	push	edi
	;push	ebp
	;mov	ebp, esp
	; edi = ebp+4
	; esi = ebp+8
	; edx = ebp+12
	; ecx = ebp+16
	; ebx = ebp+20
	;
	call	DISK_IO
	;
	pop	edi
	pop	esi
	pop	edx
	pop	ecx
	pop	ebx	
	;
	;pop	ds
	;pop	es
	;
	retn

; 10/08/2022
; 07/08/2022
; 17/07/2022
; 13/07/2022 - TRDOS 386 v2.0.5
; 18/04/2021 - TRDOS 386 v2.0.4
; 11/04/2021 - TRDOS 386 v2.0.3
; 30/08/2020
; 09/12/2017
; 29/05/2016
; 27/05/2016 - TRDOS 386 (TRDOS v2.0)

DISK_IO:
	; 10/08/2022
	; 17/07/2022
	; 13/07/2022
	; Registers are also on stack 
	; (with same contents) 
	; in following order:
	;
	;    ebx = esp+20
	;    ecx = esp+16
	;    edx = esp+12
	;    esi = esp+8
	;    edi = esp+4

	; cs = KCODE (== KDATA base address)
	; ss = KDATA
	; ds = KDATA
	; es = KDATA

	; 17/07/2022
	sti				; ENABLE INTERRUPTS

	cmp	dl, 80h			; TEST FOR FIXED DISK DRIVE
	;jae	short A1		; YES, HANDLE HERE
	;;;int	40h			; DISKETTE HANDLER
	;;call	int40h
	;jb	DISKETTE_IO_1
;RET_2:
	;;retf	2			; BACK TO CALLER
	;retf	4
	; 11/04/2021
	jnb	short A1
	jmp	DISKETTE_IO_1
A1:
	;sti	 ; 17/07/2022		; ENABLE INTERRUPTS
	
	;; 04/01/2015
	;;or	ah, ah
	;;jnz	short A2
	;;int	40h			; RESET NEC WHEN AH=0
	;;sub	ah, ah
	
	cmp	dl, (80h + S_MAX_FILE - 1)
	;ja	short RET_2
	jna	short _A0
	
	; 13/07/2022
	; (here, DS is KDATA segment already) 
	;
	; 29/05/2016
	;push	ds
	; 11/04/2021
	;push	eax
	;mov	ax, KDATA
	;mov	ds, ax
	; 11/04/2021
	;pop	eax
	
	mov     ah, 0AAh        ; Hard disk drive not ready !
				; (Programmer's guide to AMIBIOS, 1992)
	mov     byte [DISK_STATUS1], ah
	; 13/07/2022
	;pop	ds
	;jmp	short RET_2
	stc
	retn
_A0:
	; 18/01/2015
	or	ah, ah
	jz	short A4
	cmp	ah, 0Dh	; Alternate reset
	jne	short A2
	sub	ah, ah	; Reset
	jmp	short A4
A2:
	; 13/07/2022
	cmp	ah, 08h			; GET PARAMETERS IS A SPECIAL CASE
	jne	short A3
        jmp	GET_PARM_N
A3:	
	; 13/07/2022
	cmp	ah, 15h			; READ DASD TYPE IS ALSO
	jne	short A4
        jmp	READ_DASD_TYPE	; Return Drive Type
				; (Programmer's guide to AMIBIOS, 1992)
	; 13/07/2022
int33h_bad_cmd:
	; 16/05/2016
	; 30/01/2015
	; 29/05/2016
	;push	ds
	;push	eax
	;mov	ax, KDATA
	;mov	ds, ax
	;pop	eax

	mov	ah, BAD_CMD
	mov     [DISK_STATUS1], ah ; BAD_CMD  ; COMMAND ERROR
        ;jmp	short RET_2
	; 13/07/2022
;RET_2:
	; (*) 29/05/2016
	; (*) retf 4
	;or	byte [esp+8], 1 ; set carry bit of eflags register
	;iretd
	
	; 13/07/2022
	stc
	; cf = 1, ah = BAD_CMD
	retn
_A4:
	; 13/07/2022
	; 02/02/2015
	cmp	ah, 1Dh			; (Temporary for Retro UNIX 386 v1)
	; 12/01/2015
	;cmc
	;jnc	short A4
	; 13/07/2022
	jnb	short int33h_bad_cmd	
A4:					; SAVE REGISTERS DURING OPERATION
	enter	8, 0			; SAVE (BP) AND MAKE ROOM FOR @CMD_BLOCK
	
	; 13/07/2022
	; ENTER 8, 0
	;;push	ebp
	;;mov	ebp, esp
	;;sub	esp, 8
	;
	;push	ebx			;  IN THE STACK, THE COMMAND BLOCK IS:
	;push	ecx			;   @CMD_BLOCK == BYTE PTR [BP]-8
	;push	edx
	;push	esi
	;push	edi

	; 13/07/2022
	; edi = ebp+8
	; esi = ebp+12
	; edx = ebp+16
	; ecx = ebp+20
	; ebx = ebp+24

	;;04/01/2015
	;;or	ah, ah			; CHECK FOR RESET
	;;jnz	short A5
	;;mov	dl, 80h			; FORCE DRIVE 80 FOR RESET
;;A5:	
	; 13/07/2022
	call	DISK_IO_CONT		; PERFORM THE OPERATION
	;;call	DDS			; ESTABLISH SEGMENT
	mov	ah, [DISK_STATUS1]	; GET STATUS FROM OPERATION
	;(*) cmp ah, 1			; SET THE CARRY FLAG TO INDICATE
					; SUCCESS OR FAILURE
	;pop	edi			; RESTORE REGISTERS
	;pop	esi
	;pop	edx
	;pop	ecx
	;pop	ebx
	
	leave				; ADJUST (SP) AND RESTORE (BP)
	
	;retf	2			; THROW AWAY SAVED FLAGS
	; (*) 29/05/2016
	; (*) retf 4
	
	; 13/07/2022
	cmp	ah, 1
	;jc	short _A5 
	;or	byte [esp+8], 1 ; set carry bit of eflags register
;_A5:
	;iretd
	; 10/08/2022
	cmc
	; 13/07/2022
	retn

; 21/02/2015
;       dw --> dd
	; 13/07/2022
D1:					; FUNCTION TRANSFER TABLE
	dd	DISK_RESET		; 00h
	dd	RETURN_STATUS		; 01h
	dd	DISK_READ		; 02h
	dd	DISK_WRITE		; 03h
	dd	DISK_VERF		; 04h
	dd	FMT_TRK 		; 05h
	dd	BAD_COMMAND		; 06h	FORMAT BAD SECTORS
	dd	BAD_COMMAND		; 07h	FORMAT DRIVE
	dd	BAD_COMMAND		; 08h	RETURN PARAMETERS
	dd	INIT_DRV		; 09h
	dd	RD_LONG 		; 0Ah
	dd	WR_LONG 		; 0Bh
	dd	DISK_SEEK		; 0Ch
	dd	DISK_RESET		; 0Dh
	dd	BAD_COMMAND		; 0Eh	READ BUFFER
	dd	BAD_COMMAND		; 0Fh	WRITE BUFFER
	dd	TST_RDY 		; 10h
	dd	HDISK_RECAL		; 11h
	dd	BAD_COMMAND		; 12h	MEMORY DIAGNOSTIC
	dd	BAD_COMMAND		; 13h	DRIVE DIAGNOSTIC
	dd	CTLR_DIAGNOSTIC 	; 14h	CONTROLLER DIAGNOSTIC
	;; 02/02/2015 (Temporary - Retro UNIX 386 v1 - DISK I/O test)
	dd	BAD_COMMAND		; 15h
	dd	BAD_COMMAND		; 16h
	dd	BAD_COMMAND		; 17h
	dd	BAD_COMMAND		; 18h
	dd	BAD_COMMAND		; 19h
	dd	BAD_COMMAND		; 1Ah
	dd	DISK_READ		; 1Bh ; LBA read
	dd	DISK_WRITE		; 1Ch ; LBA write
D1L     EQU    $ - D1

	; 02/12/2023
	; 01/12/2023 - TRDOS 386 v2.0.7 
	; 07/08/2022
	; 17/07/2022 - TRDOS 386 v2.0.5
DISK_IO_CONT:
	;;call	DDS			; ESTABLISH SEGMENT
	; 11/04/2021
	cmp	ah, 01h			; RETURN STATUS
	jne	short SU0
	jmp	RETURN_STATUS
SU0:
	mov	byte [DISK_STATUS1], 0 	; RESET THE STATUS INDICATOR
	; 13/07/2022
	mov	esi, ebx ; 21/02/2015	; SAVE DATA ADDRESS
	mov	bl, [HF_NUM]		; GET NUMBER OF DRIVES
	and	dl, 7Fh			; GET DRIVE AS 0 OR 1
					; (get drive number as 0 to 3)
	; 14/02/2015 
	cmp	bl, dl
	;jbe	short BAD_COMMAND	; INVALID DRIVE
	; 07/08/2022
	ja	short SU0X
	jmp	BAD_COMMAND
SU0X:
	;;03/01/2015
	sub	ebx, ebx
	mov	bl, dl
	mov	[LBAMode], bh 	; 0
	
	;test	byte [ebx+hd0_type], 1 	; LBA ready ?
	;jz	short su1		; no
	;inc	byte [LBAMode]
;su1:
	; 11/04/2021 (32 bit push/pop)
	; 21/02/2015 (32 bit modification)
	; 04/01/2015
	push	eax ; ***
	;push	es  ; **
	push	edx ; *
	push	eax ; ****
	call	GET_VEC 		; GET DISK PARAMETERS
	; 02/02/2015
	;mov	ax, [ES:BX+16] ; I/O port base address (1F0h, 170h)
	mov	ax, [ebx+16]
	mov	[HF_PORT], ax
	;mov	dx, [ES:BX+18] ; control port address (3F6h, 376h)
	mov	dx, [ebx+18]
	mov	[HF_REG_PORT], dx
	;mov	al, [ES:BX+20] ; head register upper nibble (A0h,B0h,E0h,F0h)
	mov	al, [ebx+20]
	; 23/02/2015
	test	al, 40h	 ; LBA bit (bit 6)
	jz 	short su1
	inc	byte [LBAMode] ; 1 
su1: 	 
	shr 	al, 4
	and	al, 1			
	mov	[hf_m_s], al 
	;
	; 03/01/2015
	;mov	al, [ES:BX+8]		; GET CONTROL BYTE MODIFIER
	mov	al, [ebx+8]
	;mov	dx, [HF_REG_PORT]	; Device Control register
	out	dx, al			; SET EXTRA HEAD OPTION
					; -here-
					; Control Byte: (= 08h)
					;  bit 0 - 0
					;  bit 1 - nIEN (1 = disable irq)
					;  bit 2 - SRST (software RESET)
					;  bit 3 - use extra heads (8 to 15)
					;          -always set to 1-	
					;  (bits 3 to 7 are reserved
					;          for ATA devices)
	mov	ah, [CONTROL_BYTE]	; SET EXTRA HEAD OPTION IN
	and	ah, 0C0h 		; CONTROL BYTE
	or	ah, al
	mov	[CONTROL_BYTE], ah
	
	; 11/04/2021 (32 bit push/pop)
	; 04/01/2015
	pop	eax ; ****
	pop	edx ; * ; 14/02/2015
	and	ah, ah	; Reset function ?
	jnz	short su2
	;pop	es ; **
	pop	eax ; ***
        jmp     DISK_RESET
su2:
	cmp	byte [LBAMode], 0
	jna	short su3
	;
	; 02/02/2015 (LBA read/write function calls)
	cmp	ah, 1Bh
	jb	short lbarw1
	cmp	ah, 1Ch
	ja 	short invldfnc
	;;pop	edx ; * ; 14/02/2015
	;mov	ax, cx ; Lower word of LBA address (bits 0-15)

	; 01/12/2023 (48 bit LBA rw) 
	jmp	lba_read_write

lbarw1:
	; convert CHS to LBA
	;
	; LBA calculation - AWARD BIOS - 1999 - AHDSK.ASM
	; LBA = "# of Heads" * Sectors/Track * Cylinder + Head * Sectors/Track
	;	+ Sector - 1
	; 11/04/2021 (32 bit push/pop)
	push	edx ; * ;; 14/02/2015
	;xor	dh, dh
	xor	edx, edx
	;mov	dl, [ES:BX+14]	; sectors per track (logical)
	mov	dl, [ebx+14]
	;xor	ah, ah
	xor	eax, eax
	;mov	al, [ES:BX+2]	; heads (logical) 	
	mov	al, [ebx+2]
	dec	al
	inc	ax		; 0 = 256
	mul 	dx
		; AX = # of Heads * Sectors/Track
	mov	dx, cx
	;and	cx, 3Fh	 ; sector  (1 to 63)
	and	ecx, 3Fh
	xchg	dl, dh
	shr	dh, 6
		; DX = cylinder (0 to 1023)
	;mul 	dx
		; DX:AX = # of Heads" * Sectors/Track * Cylinder
	mul	edx
	dec	cl ; sector - 1
	;add	ax, cx
	;adc	dx, 0
		; DX:AX = # of Heads" * Sectors/Track * Cylinder + Sector - 1
	add	eax, ecx
	; 11/04/2021 (32 bit push/pop)
	pop	ecx ; * ; ch = head, cl = drive number (zero based)
	;push	dx
	;push	ax
	push	eax
	; 13/07/2022
	sub	eax, eax
	;mov	al, [ES:BX+14]	; sectors per track (logical)	
	mov	al, [ebx+14]
	mul	ch
		; AX = Head * Sectors/Track
        ; 13/07/2022
	;movzx	eax, ax ; 09/12/2017
	;pop	dx
	pop	edx
	;add	ax, dx
	;pop	dx
	;adc	dx, 0 ; add carry bit
	add	eax, edx
lbarw2:
	sub	edx, edx ; 21/02/2015
	mov	dl, cl ; 21/02/2015
        mov     byte [CMD_BLOCK], 0 ; Features Register
				; NOTE: Features register (1F1h, 171h)
				; is not used for ATA device R/W functions. 
				; It is old/obsolete 'write precompensation'
				; register and error register
				; for old ATA/IDE devices.
	; 18/01/2014
	;mov	ch, [hf_m_s]	; Drive 0 (master) or 1 (slave)
	mov	cl, [hf_m_s]
	;shl	ch, 4		; bit 4 (drive bit)
	;or	ch, 0E0h	; bit 5 = 1
				; bit 6 = 1 = LBA mode
				; bit 7 = 1
	or	cl, 0Eh ; 1110b
	;and	dh, 0Fh		; LBA byte 4 (bits 24 to 27)
	and	eax, 0FFFFFFFh
	shl	ecx, 28 ; 21/02/2015
	;or	dh, ch
	or	eax, ecx	
	;;mov	[CMD_BLOCK+2], al ; LBA byte 1 (bits 0 to 7)
				  ; (Sector Number Register)
	;;mov	[CMD_BLOCK+3], ah ; LBA byte 2 (bits 8 to 15)
				  ; (Cylinder Low Register)
	;mov	[CMD_BLOCK+2], ax ; LBA byte 1, 2
	;mov	[CMD_BLOCK+4], dl ; LBA byte 3 (bits 16 to 23)
				  ; (Cylinder High Register)
	;;mov	[CMD_BLOCK+5], dh ; LBA byte 4 (bits 24 to 27)
				  ; (Drive/Head Register)
	
	;mov	[CMD_BLOCK+4], dx ; LBA byte 4, LBA & DEV select bits
	mov	[CMD_BLOCK+2], eax ; 21/02/2015
	; 14/02/2015
	;mov	dl, cl ; Drive number (INIT_DRV)		
	jmp	short su4
su3:
	; 07/08/2022
	; 13/07/2022
	; 02/02/2015 
	; (1Bh & 1Ch functions are not valid for CHS mode) 
	cmp 	ah, 14h
	jna 	short chsfnc
invldfnc:
        ; 14/02/2015  
	;pop	es ; **
	; 11/04/2021
	pop	eax ; *** 
        jmp     short BAD_COMMAND
chsfnc:	
	;mov	ax, [ES:BX+5]		; GET WRITE PRE-COMPENSATION CYLINDER
	mov	ax, [ebx+5]
	;shr	ax, 2
	; 07/08/2022
	shr	eax, 2
	mov	[CMD_BLOCK], al
	
	;;mov	al, [ES:BX+8]		; GET CONTROL BYTE MODIFIER
	;;push	edx ; *
	;;mov	dx, [HF_REG_PORT]
	;;out	dx, al			; SET EXTRA HEAD OPTION
	;;pop	edx ; * 
	;;pop	es  ; **
	;;mov	ah, [CONTROL_BYTE]	; SET EXTRA HEAD OPTION IN
	;;and	ah, 0C0h 		; CONTROL BYTE	
	;;or	ah, al
	;;mov	[CONTROL_BYTE], ah
	
	mov	al, cl			; GET SECTOR NUMBER
	and	al, 3Fh
	mov	[CMD_BLOCK+2], al
	mov	[CMD_BLOCK+3], ch 	; GET CYLINDER NUMBER
	mov	al, cl
	shr	al, 6
	mov	[CMD_BLOCK+4], al 	; CYLINDER HIGH ORDER 2 BITS
	
	;;05/01/2015
	;;mov	al, dl			; DRIVE NUMBER
	mov	al, [hf_m_s]
	shl	al, 4
	and	dh, 0Fh			; HEAD NUMBER
	or	al, dh
	;or	al, 80h or 20h
	or	al, 80h+20h		; ECC AND 512 BYTE SECTORS
	mov	[CMD_BLOCK+5], al 	; ECC/SIZE/DRIVE/HEAD
su4:
	;pop	es ; **
        ;; 14/02/2015
        ;;pop	ax
        ;;mov	[CMD_BLOCK+1], al	; SECTOR COUNT
        ;;push	ax
        ;;mov	al, ah			; GET INTO LOW BYTE
        ;;xor	ah, ah			; ZERO HIGH BYTE
        ;;sal	ax, 1			; *2 FOR TABLE LOOKUP
        ; 11/04/2021
	pop	eax ; *** 
        mov     [CMD_BLOCK+1], al
        sub	ebx, ebx
	;xor	bh, bh
	mov     bl, ah
        ;sal	bx, 1
        ; 17/07/2022
	sal	ebx, 2	; 32 bit offset (21/02/2015)
	;mov	si, ax			; PUT INTO SI FOR BRANCH
        ; 13/07/2022
	;cmp	bx, D1L			; TEST WITHIN RANGE
	;jnb	short BAD_COMMAND_POP
	cmp	ebx, D1L		; TEST WITHIN RANGE
	jnb	short BAD_COMMAND
        ;xchg	bx, si
        xchg	ebx, esi
	;;;pop	ax			; RESTORE AX
	;;;pop	bx			; AND DATA ADDRESS
	
	;;push	cx
	;;push	ax			; ADJUST ES:BX
	;mov	cx, bx			; GET 3 HIGH ORDER NIBBLES OF BX
	;shr	cx, 4
	;mov	ax, es
	;add	ax, cx
	;mov	es, ax
	;and	bx, 000Fh		; ES:BX CHANGED TO ES:000X
	;;pop	ax
	;;pop	cx
	;;jmp	word [CS:SI+D1]
	;jmp	word [SI+D1]
	
	jmp	dword [esi+D1]

	; 07/08/2022
	; 13/07/2022
BAD_COMMAND:
        mov	byte [DISK_STATUS1], BAD_CMD ; COMMAND ERROR
	mov	al, 0
	retn

; -----------------------------------------------------
; 48 bit LBA read/write
; -----------------------------------------------------

su10:	; 01/12/2023 ; 28 bit LBA r/w
	mov	eax, ecx ; LBA address (21/02/2015)
	; 13/07/2022
	mov	cl, dl ; 14/02/2015
	jmp	short lbarw2

lba_read_write:
	; 02/12/2023
	; 01/12/2023 - TRDOS 386 v2.0.7 (48 bit LBA rw)
	cmp	ecx, 0FFFFFFFh
	jnb	short su6
	;movzx	eax, byte [esp] ; ***
	xor	eax, eax
	mov	al, [esp] ; *** ; sector count
	add	eax, ecx
	jnc	short su5
	mov	byte [DISK_STATUS1], ERR_INV_PARAMETER
	jmp	short su7
su5:	
	cmp	eax, 0FFFFFFFh ; 28 bit limit
	jna	short su10
su6:
	; 48 bit LBA r/w
	mov	al, [hf_m_s]	; (+!+) ; 02/12/2023
	shl	al, 4
	; 02/12/2023
	;add	al, 0E0h
	add	al, 40h
	;;add	al, 0Eh
	;add	al, 04h
	;shl	al, 4
	mov	dx, [HF_PORT]
	add	dl, 6	; hd base port + 6
	out	dx, al
	; 02/12/2023
	mov	[CMD_BLOCK+6], al
	inc	edx	; hd base port + 7
	in	al, dx
	NEWIODELAY
	;test	al, 128		; READY ?
	and	al, 128
	jz	short su8
	in	al, dx
	;test	al, 128
	and	al, 128
	jz	short su8
	mov	byte [DISK_STATUS1], TIME_OUT
su7:
	pop	eax ; ***
	mov	al, 0
	retn
su8:
	;mov	dx, [HF_PORT]
	;inc	edx
	;inc	edx	; hd base port + 2
	sub	dl, 5	; hd base port + 2
	;xor	al, al
	out	dx, al	; sector count hb (bits 8 to 15) = 0
	inc	edx	; hd base port + 3
	mov	eax, ecx ; LBA disk sector address
	rol	eax, 8 
	out	dx, al	; LBA byte 4 (bits 24 to 31)
	inc	edx	; hd base port + 4
	xor	al, al
	out	dx, al	; LBA byte 5 (bits 32 to 39) = 0
	inc	edx	; hd base port + 5
	;sub	al, al
	out	dx, al	; LBA byte 6 (bits 40 to 47) = 0

	mov	al, [esp] ; ***
	; 02/12/2023
	mov	[CMD_BLOCK+1], al

	sub	dl, 3	; hd base port + 2
	out	dx, al	; sector count lb (bits 0 to 7) = 0
	inc	edx	; hd base port + 3
	mov	eax, ecx ; LBA disk sector address
	out	dx, al	; LBA byte 1 (bits 0 to 7)
	inc	edx	; hd base port + 4
	shr	eax, 8
	out	dx, al	; LBA byte 2 (bits 8 to 15) = 0
	inc	edx	; hd base port + 5
	shr	eax, 8
	out	dx, al	; LBA byte 3 (bits 16 to 23) = 0

 	inc     edx	; hd base port + 6

	; 02/12/2023 (not necessary) (+!+)
	;mov	al, [hf_m_s]
	;shl	al, 4
	;add	al, 40h
	;out	dx, al

	pop	eax	; ***

	inc	edx	; dx = hd base port + 7 
			;      command/status port
	;xchg	esi, ebx
	;mov	edi, ebx
	mov	edi, esi ; sector buffer 
	cmp	ah, 1Ch
	jne	short su_9
	mov	al, 34h ; WRITE SECTOR(S) EXT
	out	dx, al
	jmp	CMD_WX
su_9:
	mov	al, 24h ; READ SECTOR(S) EXT
	out	dx, al
	jmp	CMD_RX

; -----------------------------------------------------

; 09/08/2022
; 07/08/2022
; 17/07/2022
; 16/07/2022 - TRDOS 386 v2.0.5
; 10/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

;----------------------------------------
;	RESET THE DISK SYSTEM  (AH=00H) :
;----------------------------------------

; 18-1-2015 : one controller reset (not other one)

DISK_RESET:
	cli
	in	al, INTB01		; GET THE MASK REGISTER
	;jmp	$+2
	IODELAY
	;and	al, 0BFh 		; ENABLE FIXED DISK INTERRUPT
	and	al, 3Fh			; 22/12/2014 (IRQ 14 & IRQ 15)
	out	INTB01, al
	sti				; START INTERRUPTS
	; 14/02/2015
	;mov	di, dx
	; 24/12/2021
	mov	edi, edx	
	; 04/01/2015
	;xor	di,di
drst0:
	mov	al, 04h  ; bit 2 - SRST 
	;mov	dx, HF_REG_PORT
	mov	dx, [HF_REG_PORT]
	out	dx, al			; RESET
;	mov	cx, 10			; DELAY COUNT
;DRD:	dec	cx
;	jnz	short DRD		; WAIT 4.8 MICRO-SEC
	;mov	cx, 2			; wait for 30 micro seconds	
        ;mov	ecx, 2 ; 21/02/2015
	; 10/07/2022
	sub	ecx, ecx
	mov	cl, 2
	call    WAITF                   ; (Award Bios 1999 - WAIT_REFRESH,
                                        ; 40 micro seconds)
	mov	al, [CONTROL_BYTE]
	and	al, 0Fh			; SET HEAD OPTION
	out	dx, al			; TURN RESET OFF
	call	NOT_BUSY
	jnz	short DRERR		; TIME OUT ON RESET
	mov	dx, [HF_PORT]
	inc	dl  ; HF_PORT+1
	; 02/01/2015 - Award BIOS 1999 - AHDSK.ASM
        ;mov	cl, 10
        ;mov	ecx, 10 ; 21/02/2015
	; 17/07/2022
	;xor	ecx, ecx
	mov	cl, 10
drst1:
	in	al, dx			; GET RESET STATUS
	cmp	al, 1
	; 04/01/2015
	jz	short drst2
	;jnz	short DRERR		; BAD RESET STATUS
        	; Drive/Head Register - bit 4
	;loop	drst1
	; 17/07/2022
	dec	cl
	jnz	short drst1
DRERR:	
	mov	byte [DISK_STATUS1], BAD_RESET ; CARD FAILED
	retn
drst2:
	; 14/02/2015
	;mov	dx, di
	; 07/08/2022
	mov	edx, edi
;drst3:
;	; 05/01/2015
;	shl 	di, 1
;	; 04/01/2015
;	mov	ax, [di+hd_cports]
;	cmp	ax, [HF_REG_PORT]
;	je	short drst4
;	mov	[HF_REG_PORT], ax
;	; 03/01/2015
;	mov	ax, [di+hd_ports]
;       mov     [HF_PORT], ax
;	; 05/01/2014
;	shr	di, 1
;	; 04/01/2015
;	jmp	short drst0	; reset other controller
;drst4:
;	; 05/01/2015
;	shr	di, 1
;	mov	al, [di+hd_dregs]
;	and	al, 10h ; bit 4 only
;	shr	al, 4 ; bit 4  -> bit 0
;	mov	[hf_m_s], al ; (0 = master, 1 = slave)
	;
; 09/08/2022
; (('INIT_DRV' prodedure sets [CMD_BLOCKS+5] value))
;
;	mov	al, [hf_m_s] ; 18/01/2015
;	test	al, 1
;	;jnz	short drst6
;       jnz     short drst4
;	and	byte [CMD_BLOCK+5], 0EFh ; SET TO DRIVE 0
;drst5:
drst3:
	call	INIT_DRV		; SET MAX HEADS
	;mov	dx, di
	call	HDISK_RECAL		; RECAL TO RESET SEEK SPEED
	; 04/01/2014
;	inc	di
;	mov	dx, di
;	cmp	dl, [HF_NUM]
;	jb	short drst3
;DRE:
	mov	byte [DISK_STATUS1], 0 	; IGNORE ANY SET UP ERRORS
	retn
;drst6:
drst4:		; Drive/Head Register - bit 4
;	or	byte [CMD_BLOCK+5], 010h ; SET TO DRIVE 1
;       ;jmp    short drst5
;       jmp     short drst3

;----------------------------------------
;	DISK STATUS ROUTINE  (AH = 01H) :
;----------------------------------------

RETURN_STATUS:
	mov	al, [DISK_STATUS1]	; OBTAIN PREVIOUS STATUS
	mov	byte [DISK_STATUS1], 0	; RESET STATUS
	retn

	; 16/07/2022 - TRDOS 386 v2.0.5

;----------------------------------------
;	READ LONG	     (AH = 0AH) :
;----------------------------------------

RD_LONG:
	;mov	@CMD_BLOCK+6, READ_CMD OR ECC_MODE
	mov     byte [CMD_BLOCK+6], READ_CMD + ECC_MODE 
	jmp	short COMMANDI

	; 16/07/2022 - TRDOS 386 v2.0.5

;----------------------------------------
;	DISK READ ROUTINE    (AH = 02H) :
;----------------------------------------

DISK_READ:
	mov	byte [CMD_BLOCK+6], READ_CMD
        ;jmp	COMMANDI

	; 16/07/2022 - TRDOS 386 v2.0.5

;----------------------------------------
; COMMANDI				:
;	REPEATEDLY INPUTS DATA TILL	:
;	NSECTOR RETURNS ZERO		:
;----------------------------------------
COMMANDI:
	; 16/07/2022 
	;	(check 64K boundary is not needed)
	;call	CHECK_DMA		; CHECK 64K BOUNDARY ERROR
	;jc	short CMD_ABORT

	;mov	di, bx
	mov	edi, ebx ; 21/02/2015
	call	COMMAND 		; OUTPUT COMMAND
	jnz	short CMD_ABORT
CMD_RX:	; 01/12/2023 (48 bit LBA read)
CMD_I1:
	call	_WAIT			; WAIT FOR DATA REQUEST INTERRUPT
	jnz	short TM_OUT		; TIME OUT
cmd_i1x:
	; 18/02/2016
	;;mov	cx, 256			; SECTOR SIZE IN WORDS
	;mov	ecx, 256 ; 21/02/2015	
	; 16/07/2022
	sub	ecx, ecx
	inc	ch  ; ecx = 256
	;mov	dh, HF_PORT
	mov	dx, [HF_PORT]
	cli
	cld
	rep	insw			; GET THE SECTOR
	sti

	test	byte [CMD_BLOCK+6], ECC_MODE ; CHECK FOR NORMAL INPUT
	jz	short CMD_I3
	call	WAIT_DRQ		; WAIT FOR DATA REQUEST
	jc	short TM_OUT
	;mov	dx, HF_PORT
	mov	dx, [HF_PORT]
	;;mov	cx, 4			; GET ECC BYTES
	;mov 	ecx, 4 ; mov cx, 4
	; 16/07/2022
 	xor	ecx, ecx
	mov	cl, 4
CMD_I2:
	in	al, dx
	mov 	[edi], al ; 21/02/2015	; GO SLOW FOR BOARD
	inc	edi
	loop	CMD_I2
CMD_I3: 
	; wait for 400 ns
	add 	dl, 7
	in	al, dx
	in	al, dx
	in	al, dx
	;
	call	CHECK_STATUS
	jnz	short CMD_ABORT		; ERROR RETURNED
	dec	byte [CMD_BLOCK+1]	; CHECK FOR MORE
	;jnz	short CMD_I1
	jnz	short cmd_i1x ; 18/02/2016
CMD_ABORT:
TM_OUT: 
	retn

	; 16/07/2022 - TRDOS 386 v2.0.5

;----------------------------------------
;	WRITE LONG	     (AH = 0BH) :
;----------------------------------------

WR_LONG:
	;mov	@CMD_BLOCK+6, WRITE_CMD OR ECC_MODE
	mov	byte [CMD_BLOCK+6], WRITE_CMD + ECC_MODE
	jmp	short COMMANDO

	; 16/07/2022 - TRDOS 386 v2.0.5
	
;----------------------------------------
;	DISK WRITE ROUTINE   (AH = 03H) :
;----------------------------------------

DISK_WRITE:
	mov	byte [CMD_BLOCK+6], WRITE_CMD
	;jmp	COMMANDO

	; 16/07/2022 - TRDOS 386 v2.0.5

;----------------------------------------
; COMMANDO				:
;	REPEATEDLY OUTPUTS DATA TILL	:
;	NSECTOR RETURNS ZERO		:
;----------------------------------------
COMMANDO:
	; 16/07/2022 
	;	(check 64K boundary is not needed)
	;call	CHECK_DMA		; CHECK 64K BOUNDARY ERROR
	;jc	short CMD_ABORT
CMD_OF: 
	mov	esi, ebx ; 21/02/2015
	call	COMMAND 		; OUTPUT COMMAND
	jnz	short CMD_ABORT
CMD_WX:	; 01/12/2023 (48 bit LBA write)
	call	WAIT_DRQ		; WAIT FOR DATA REQUEST
	jc	short TM_OUT		; TOO LONG
CMD_O1: 
	; 16/07/2022
	mov	dx, [HF_PORT]

	;mov	ecx, 256 ; 21/02/2015
	xor	ecx, ecx
	inc	ch
	; ecx = 256
	cli
	cld
	;rep	outsw
	; 01/12/2023 - TRDOS 386 v2.0.7
CMD_01_L:	
	outsw
	jmp	$+2
	loop	CMD_01_L

	sti

	test	byte [CMD_BLOCK+6], ECC_MODE ; CHECK FOR NORMAL OUTPUT
	jz	short CMD_O3
	call	WAIT_DRQ		; WAIT FOR DATA REQUEST
	jc	short TM_OUT
	;mov	dx, HF_PORT
	mov	dx, [HF_PORT]
					; OUTPUT THE ECC BYTES
	;mov	ecx, 4  ; mov cx, 4
	; 16/07/2022
	sub	ecx, ecx
	mov	cl, 4
CMD_O2:
	mov	al, [esi]
	out	dx, al
	inc	esi
	loop	CMD_O2
CMD_O3:
	call	_WAIT			; WAIT FOR SECTOR COMPLETE INTERRUPT
	jnz	short TM_OUT		; ERROR RETURNED
	call	CHECK_STATUS
	jnz	short CMD_ABORT
	test	byte [HF_STATUS], ST_DRQ ; CHECK FOR MORE
	jnz	short CMD_O1
	;mov	dx, HF_PORT+2		; CHECK RESIDUAL SECTOR COUNT
	mov	dx, [HF_PORT]
	add	dl, 2
	;inc	dl
	;inc	dl
	in	al, dx			;
	test	al, 0FFh 		;
	jz	short CMD_O4		; COUNT = 0 OK
	mov	byte [DISK_STATUS1], UNDEF_ERR 
					; OPERATION ABORTED - PARTIAL TRANSFER
CMD_O4:
	retn

	; 16/07/2022 - TRDOS 386 v2.0.5

;----------------------------------------
;	FORMATTING	     (AH = 05H) :
;----------------------------------------

FMT_TRK:				; FORMAT TRACK (AH = 005H)
	mov	byte [CMD_BLOCK+6], FMTTRK_CMD
	;push	es
	;push	bx
	push	ebx
	call	GET_VEC 		; GET DISK PARAMETERS ADDRESS
	;mov	al, [ES:BX+14]		; GET SECTORS/TRACK
	mov	al, [ebx+14]
	mov	[CMD_BLOCK+1], al	; SET SECTOR COUNT IN COMMAND
	pop	ebx
	;pop	bx
	;pop	es
	;jmp	short CMD_OF		; GO EXECUTE THE COMMAND
	; 01/12/2023
	jmp	CMD_OF

;----------------------------------------
;	DISK VERIFY	     (AH = 04H) :
;----------------------------------------

DISK_VERF:
	mov	byte [CMD_BLOCK+6], VERIFY_CMD
	call	COMMAND
	jnz	short VERF_EXIT		; CONTROLLER STILL BUSY
	call	_WAIT			; (Original: CALL WAIT)	
	jnz	short VERF_EXIT		; TIME OUT
	call	CHECK_STATUS
VERF_EXIT:
	retn

;----------------------------------------
;	READ DASD TYPE	     (AH = 15H) :
;----------------------------------------

RETURN_DRIVE_TYPE:
	; 10/08/2022
	; 13/07/2022
	; (Ref: Programmer's Guide to the AMIBIOS -Page 214-, 1992)
	;
	; INPUT:
	;	DL = Disk number (>= 80h)
	;   TRDOS 386 v2.0.5 Feature:
	;	If AL = 0FFh, return disk size in ECX
	;		otherwise in CX:DX
	; OUTPUT:
	;	AH = 00h - No drive present
	;	   = 03h - Hard disk drive
	;	CF = 0 - No error
	;	   = 1 - Error  	 
	;   TRDOS 386 v2.0.5 Feature:
	;	AL = 00h - LBA not ready !	
	;	   = 01h - LBA ready 
	;		(28 bit or 48 bit LBA r/w depending
	;		on disk size in CX:DX)
	;	CX:DX = Number of 512 byte sectors
	;
	; (Note: High words of ECX and EDX will be zero at return)
	; ((If AL input is 0FFh, disk size will be in ECX only))

READ_DASD_TYPE:
READ_D_T:				; GET DRIVE PARAMETERS
	;push	ds			; SAVE REGISTERS
	
	;;push	es
	; 18/04/2021
	;push	ebx
	;;call	DDS			; ESTABLISH ADDRESSING
	;;push	cs
	;;pop	ds
        
	; 18/04/2021
	;mov	bx, KDATA
	;mov	ds, bx
	;;mov	es, bx
	;mov	byte [DISK_STATUS1], 0
	;mov	bl, [HF_NUM]		; GET NUMBER OF DRIVES
	;and	dl, 7Fh			; GET DRIVE NUMBER
	;cmp	bl, dl
	;jbe	short RDT_NOT_PRESENT 	; RETURN DRIVE NOT PRESENT
	
	;mov	ax, KDATA
	;mov	ds, ax
	
	mov	byte [DISK_STATUS1], 0
	mov	cl, [HF_NUM]
	and	dl, 7Fh
	cmp	cl, dl
	jbe	short RDT_NOT_PRESENT

	; 18/04/2021 - TRDOS 386 v2.0.4
	
	;call	GET_VEC 		; GET DISK PARAMETER ADDRESS
	;
	;;mov	al, [ES:BX+2]		; HEADS
	;mov	al, [ebx+2]  ; heads (logical)
	;;;mov	cl, [ES:BX+14]
	;;mov	cl, [ebx+14]
	;; 17/04/2021
	;mov	ah, [ebx+14] ; sectors per track (logical)
	;;imul	cl			; * NUMBER OF SECTORS
	;;mov	cx, [ES:BX]		; MAX NUMBER OF CYLINDERS
	;mov	cx, [ebx]    ; cylinders (logical)
	;; 02/01/2015 
	;; ** leave the last cylinder as reserved for diagnostics **
	;; (Also in Award BIOS - 1999, AHDSK.ASM, FUN15 -> sub ax, 1)
	;dec	cx			; LEAVE ONE FOR DIAGNOSTICS
	;;imul	cx			; NUMBER OF SECTORS	 	
	;; 17/04/2021
	;mul	ah
	;; ax = spt*heads
	;mul	cx	 
	;; dx:ax = number of sectors
	;
	;mov	cx, dx			; HIGH ORDER HALF
	;mov	dx, ax			; LOW ORDER HALF

	; 18/04/2021
	mov	cl, 2
	add	dl, cl ; hd0 = 2

	; 13/07/2022
	movzx	edx, dl
	mov	bl, [edx+drv.status]
	and	bl, 1 ; LBA ready bit (bit 0) 

	shl	dl, cl ; * 4
	
	;mov	eax, [edx+drv.size]
	;mov	dx, ax
	;shr	eax, 16
	;mov	cx, ax

	mov	ecx, [edx+drv.size]

	; 13/07/2022
	;cmp	al, 0FFh   ; return disk size in ecx ?
	;je	short RDT1 ; yes
	inc	al  ; 0FFh -> 0
	jz	short RDT1

	mov	dx, cx
	shr	ecx, 16

	; 13/07/2022
	; ebx = esp+20
	; ecx = esp+16
	; edx = esp+12
	; esi = esp+8
	; edi = esp+4

	; return disk size in user's registers
	mov	[esp+12], edx
	; cx:dx = disk size
RDT1:
	;mov	[esp+16], ecx

	;;sub	al, al
	sub	eax, eax
	mov	ah, 03h			; INDICATE FIXED DISK
	mov	al, bl
	; al = 1 -> LBA r/w ready/applicable
	;    = 0 -> LBA r/w not ready/applicable	 
	; cf = 0
RDT2:
	mov	[esp+16], ecx	
;RDT2:
	; 13/07/2022
	; 18/04/2021
	;pop	ebx			; RESTORE REGISTERS
	;;pop	es
	;pop	ds
	; (*) clc			; CLEAR CARRY
	;retf	2
	; (*) 29/05/2016
	; (*) retf 4
	;and	byte [esp+8], 0FEh ; clear carry bit of eflags register
	;iretd

	; 13/07/2022
	; [DISK_STATUS1] = 0
	; ah = 3
	; al = 0 or 1 (LBA ready)
	; cf = 0	
	
	retn

RDT_NOT_PRESENT:
	;;sub	ax, ax			; DRIVE NOT PRESENT RETURN
	;; 18/04/2021
	;sub	eax, eax
	;;mov	cx, ax			; ZERO BLOCK COUNT
	;;mov	dx, ax
	;mov	ecx, eax
	;mov	edx, eax
	;jmp	short RDT2
	; 13/07/2022
	sub	ecx, ecx
	mov	cl, al ; if AL = 0FFh, disk size will be in ECX
		       ; if not, disk size will be in CX:DX 
	sub	eax, eax
	inc	cl ; 0FFh -> 0
	cmp	cl, 1
	jb	short RDT2 ; ecx = 0
	; 10/08/2022
	sub	cl, cl
	; ecx = 0
	mov	[esp+12], eax ; edx = 0
	stc
	jmp	short RDT2 ; cf = 1, eax = 0	

; 10/08/2022
; 07/08/2022
; 13/07/2022 - TRDOS 386 v2.0.5
; 28/05/2016
; 27/05/2016 - TRDOS 386 (TRDOS v2.0)

;----------------------------------------
;	GET PARAMETERS	     (AH = 08H) :
;----------------------------------------

GET_PARM_N:
	; ebx = user's buffer address for parameters table
	; 10/08/2022
	; 13/07/2022
	; (if ebx = 0, HDPT will not be returned to user)
	;	
;GET_PARM:				; GET DRIVE PARAMETERS
	;push	ds			; SAVE REGISTERS
	;push	es
	
	;push	ebx
	mov	edi, ebx ; 13/07/2022 	

	; 13/07/2022
	; ((IBM PC XT-286 ROM BIOS source code remainders))
	;;mov	ax, ABS0 		; ESTABLISH ADDRESSING
	;;mov	ds, ax
	
	;;test	dl, 1			; CHECK FOR DRIVE 1
	;;jz	short G0
	;;les	bx, @HF1_TBL_VEC
	;;jmp	short G1
;;G0:	
	;les	bx, @HF_TBL_VEC
;;G1:
	;;call	DDS			; ESTABLISH SEGMENT
	
	; 13/07/2022
	; 22/12/2014
	;;push	cs
	;;pop	ds
	;mov	bx, KDATA
	;mov	ds, bx
	;mov	es, bx	; 27/05/2016
	;
	; 18/04/2021
	sub	ecx, ecx
	;
	sub	dl, 80h
	cmp	dl, MAX_FILE		; TEST WITHIN RANGE
	jae	short G2 ; 13/07/2022
	;
 	; 21/02/2015
	xor	ebx, ebx
	; 18/04/2021
	;sub	ecx, ecx
	; 22/12/2014
	mov	bl, dl
	;xor	bh, bh  
	shl	bl, 2			; convert index to offset
	;add	bx, HF_TBL_VEC
	add	ebx, HF_TBL_VEC
	;mov	ax, [bx+2]
	;mov	es, ax			; dpt segment
	;mov	bx, [bx]		; dpt offset
	mov	ebx, [ebx] ; 32 bit offset	
	; 18/04/2021
	sub	edx, edx
 	mov	[DISK_STATUS1], dl ; 0

	;mov	byte [DISK_STATUS1], 0
        ;mov	ax, [ES:BX]		; MAX NUMBER OF CYLINDERS
	mov	ax, [ebx]
	;;sub	ax, 2			; ADJUST FOR 0-N
	dec	ax			; max. cylinder number
	mov	ch, al
	and	ax, 0300h		; HIGH TWO BITS OF CYLINDER
	;shr	ax, 1
	;shr	ax, 1
	; 13/07/2022
	;shr	ax, 2
	; 07/08/2022
	shr	eax, 2
	;or	al, [ES:BX+14]		; SECTORS
	or	al, [ebx+14]
	mov	cl, al
	;mov	dh, [ES:BX+2]		; HEADS
	mov	dh, [ebx+2]
	dec	dh			; 0-N RANGE
	mov	dl, [HF_NUM]		; DRIVE COUNT
	;;sub	ax, ax
	; 18/04/2021
	;sub	eax, eax

	; 27/12/2014 
	;mov	di, bx			; HDPT offset

	; 13/07/2022
	; ebx = esp+20
	; ecx = esp+16
	; edx = esp+12
	; esi = esp+8
	; edi = esp+4

	; 13/07/2022
	; set return register contents/values
	mov	[esp+16], ecx
	mov	[esp+12], edx

	; is hard disk parameters table requested ?
	or	edi, edi ; (edi = [ebp+24] = ebx)
	jnz	short G3 ; yes

	sub	eax, eax	

	; [DISK_STATUS1] = 0
	; eax = 0
	; cf = 0

	retn

G2:
	;mov	ah, INIT_FAIL
	;mov	byte [DISK_STATUS1], ah ; (INIT_FAIL)
	;				; OPERATION FAILED
	;sub	al, al
	;sub	dx, dx
	;sub	cx, cx
	; 18/04/2021
	sub	eax, eax
	mov	ah, INIT_FAIL
	mov     [DISK_STATUS1], ah	; OPERATION FAILED

	; 13/07/2022
	;sub	edx, edx
	;sub	ecx, ecx
	; ecx = 0
	mov	[esp+12], ecx ; 0 ; edx (heads-1, drive count)
	mov	[esp+16], ecx ; 0 ; ecx (cylinders-1, sectors)
	mov	[esp+20], ecx ; 0 ; ebx (HDPT address)

	stc
	retn

	; 13/07/2022
	; 29/05/2016 (*)
	;;stc				; SET ERROR FLAG
	;;jmp	short G5
	;jmp	short _G6

G3:	
	; 27/05/2016
	; return fixed disk parameters table to user
	; in user's buffer, which is pointed by EBX
	
	;xchg	edi, [esp]		; ebx (input)-> edi, edi -> [esp]
	; 13/07/2022
	;pop	edi
	; edi = user's buffer address
	;push	esi
	mov	esi, ebx		; hard disk parameter table (32 bytes)	
	;mov	ebx, edi		; ebx = user's buffer address
	push	ecx
	;push	eax
	;mov	ecx, 32 ; 32 bytes
	xor	ch, ch
	mov	cl, 32
	; ecx = 32
	call	transfer_to_user_buffer ; trdosk6.s (16/05/2016)
	;pop	eax
	pop	ecx
	; 10/08/2022
	;pop	esi
	;pop	edi
	jnc	short G4
	; 29/05/2016 (*)
	mov	eax, 0FFh ; unknown error !
	; [DISK_STATUS1] = 0
	; ah = 0, al = 0FFh
	; cf = 1
	
	retn
;_G6:
	;or	byte [esp+16], 1 ; set carry bit of eflags register
;G5:
	; 27/05/2016
	;pop	ebx			; RESTORE REGISTERS
	;pop	es
	;pop	ds
	;;retf	2
	; (*) 29/05/2016
	; (*) retf 4
	; (*) or byte [esp+8], 1 ; set carry bit of eflags register
	;iretd

G4:
	; 13/07/2022
	xor	eax, eax

	; [user_buffer] = [ebp+24] = HDPT
	; [DISK_STATUS1] = 0
	; eax = 0
	; cf = 0

	retn

	; 16/07/2022 - TRDOS 386 v2.0.5
	
;----------------------------------------
;	INITIALIZE DRIVE     (AH = 09H) :
;----------------------------------------
	; 03/01/2015
	; According to ATA-ATAPI specification v2.0 to v5.0
	; logical sector per logical track
	; and logical heads - 1 would be set but
	; it is seen as it will be good
	; if physical parameters will be set here
	; because, number of heads <= 16.
	; (logical heads usually more than 16)
	; NOTE: ATA logical parameters (software C, H, S)
	;	== INT 13h physical parameters

;INIT_DRV:
;	mov	byte [CMD_BLOCK+6], SET_PARM_CMD
;	call	GET_VEC 		; ES:BX -> PARAMETER BLOCK
;	mov	al, [es:bx+2]		; GET NUMBER OF HEADS
;	dec	al			; CONVERT TO 0-INDEX
;	mov	ah, [CMD_BLOCK+5] 	; GET SDH REGISTER
;	and	ah, 0F0h 		; CHANGE HEAD NUMBER
;	or	ah, al			; TO MAX HEAD
;	mov	[CMD_BLOCK+5], ah
;	mov	al, [es:bx+14]		; MAX SECTOR NUMBER
;	mov	[CMD_BLOCK+1], al
;	sub	ax, ax
;	mov	[CMD_BLOCK+3], al 	; ZERO FLAGS
;	call	COMMAND 		; TELL CONTROLLER
;	jnz	short INIT_EXIT		; CONTROLLER BUSY ERROR
;	call	NOT_BUSY		; WAIT FOR IT TO BE DONE
;	jnz	short INIT_EXIT		; TIME OUT
;	call	CHECK_STATUS
;INIT_EXIT:
;	retn

; 16/07/2022 - TRDOS 386 v2.0.5

; 04/01/2015
; 02/01/2015 - Derived from from AWARD BIOS 1999
;				 AHDSK.ASM - INIT_DRIVE
INIT_DRV:
	;xor	ah,ah
	xor	eax, eax ; 21/02/2015
	mov	al, 11 ; Physical heads from translated HDPT
        cmp     [LBAMode], ah   ; 0
	ja	short idrv0
	mov	al, 2  ; Physical heads from standard HDPT
idrv0:
	; DL = drive number (0 based)
	call	GET_VEC
	;push	bx
	push	ebx ; 21/02/2015
	;add	bx, ax
	add	ebx, eax
	;; 05/01/2015
	mov	ah, [hf_m_s] ; drive number (0= master, 1= slave)
	;;and 	ah, 1 
	shl	ah, 4
	or	ah, 0A0h  ; Drive/Head register - 10100000b (A0h)	
	;mov	al, [es:bx]
	mov	al, [ebx] ; 21/02/2015
	dec	al	 ; last head number 
	;and	al, 0Fh
	or	al, ah	 ; lower 4 bits for head number
	;
	mov	byte [CMD_BLOCK+6], SET_PARM_CMD
	mov	[CMD_BLOCK+5], al
	;pop	bx
	pop	ebx
	sub	eax, eax ; 21/02/2015
	mov	al, 4	; Physical sec per track from translated HDPT
	cmp	byte [LBAMode], 0
	ja	short idrv1
	mov	al, 14	; Physical sec per track from standard HDPT
idrv1:
	;xor	ah, ah
	;add	bx, ax
	add	ebx, eax ; 21/02/2015
	;mov	al, [es:bx]
			; sector number
	mov	al, [ebx]
	mov	[CMD_BLOCK+1], al
	sub	al, al
	mov	[CMD_BLOCK+3], al ; ZERO FLAGS
	call	COMMAND 	  ; TELL CONTROLLER
	jnz	short INIT_EXIT	  ; CONTROLLER BUSY ERROR
	call	NOT_BUSY	  ; WAIT FOR IT TO BE DONE
	jnz	short INIT_EXIT	  ; TIME OUT
	; 16/07/2022
	;call	CHECK_STATUS
	;jmp	short CHECK_STATUS
;INIT_EXIT:
	;retn

	; 16/07/2022 - TRDOS 386 v2.0.5
	; 10/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

;----------------------------------------
;	CHECK FIXED DISK STATUS 	:
;----------------------------------------
CHECK_STATUS:
	call	CHECK_ST		; CHECK THE STATUS BYTE
	;jnz	short CHECK_S1		; AN ERROR WAS FOUND
	; 10/07/2022
	jnz	short CHECK_S2
	test	al, ST_ERROR		; WERE THERE ANY OTHER ERRORS
	jz	short CHECK_S1		; NO ERROR REPORTED
	call	CHECK_ER		; ERROR REPORTED
CHECK_S1:
	cmp	byte [DISK_STATUS1], 0 	; SET STATUS FOR CALLER
CHECK_S2:
INIT_EXIT:	; 10/07/2022
	retn

	; 16/07/2022 - TRDOS 386 v2.0.5

;----------------------------------------
;	SEEK		     (AH = 0CH) :
;----------------------------------------

DISK_SEEK:
        mov	byte [CMD_BLOCK+6], SEEK_CMD
	call	COMMAND
	jnz	short DS_EXIT		; CONTROLLER BUSY ERROR
	call	_WAIT
        jnz	short DS_EXIT		; TIME OUT ON SEEK
	call	CHECK_STATUS
        cmp	byte [DISK_STATUS1], BAD_SEEK
	jne	short DS_EXIT
        mov	byte [DISK_STATUS1], 0
DS_EXIT:
	retn

;----------------------------------------
;	TEST DISK READY      (AH = 10H) :
;----------------------------------------

TST_RDY:				; WAIT FOR CONTROLLER
	call	NOT_BUSY
	jnz	short TR_EX
	mov	al, [CMD_BLOCK+5] 	; SELECT DRIVE
	mov	dx, [HF_PORT]
	add	dl, 6
	out	dx, al
	call	CHECK_ST		; CHECK STATUS ONLY
	jnz	short TR_EX
	mov	byte [DISK_STATUS1], 0 	; WIPE OUT DATA CORRECTED ERROR
TR_EX:	
	retn

;----------------------------------------
;	RECALIBRATE	     (AH = 11H) :
;----------------------------------------

HDISK_RECAL:
        mov	byte [CMD_BLOCK+6], RECAL_CMD ; 10h, 16
	call	COMMAND 		; START THE OPERATION
	jnz	short RECAL_EXIT	; ERROR
	call	_WAIT			; WAIT FOR COMPLETION
	jz	short RECAL_X 		; TIME OUT ONE OK ?
	call	_WAIT			; WAIT FOR COMPLETION LONGER
	jnz	short RECAL_EXIT	; TIME OUT TWO TIMES IS ERROR
RECAL_X:
	call	CHECK_STATUS
	cmp	byte [DISK_STATUS1], BAD_SEEK ; SEEK NOT COMPLETE
	jne	short RECAL_EXIT	; IS OK
	mov	byte [DISK_STATUS1], 0
RECAL_EXIT:
        cmp	byte [DISK_STATUS1], 0
	retn

;----------------------------------------
;      CONTROLLER DIAGNOSTIC (AH = 14H) :
;----------------------------------------

CTLR_DIAGNOSTIC:
	; 07/08/2022 - TRDOS 386 v2.0.5
	cli				; DISABLE INTERRUPTS WHILE CHANGING MASK
	in	al, INTB01		; TURN ON SECOND INTERRUPT CHIP
	;and	al, 0BFH
	and	al, 3Fh			; enable IRQ 14 & IRQ 15
	;jmp	$+2
	IODELAY
	out	INTB01, al
	IODELAY
	in	al, INTA01		; LET INTERRUPTS PASS THRU TO
	and	al, 0FBh 		;  SECOND CHIP
	;jmp	$+2
	IODELAY
	out	INTA01, al
	sti
	call	NOT_BUSY		; WAIT FOR CARD
	jnz	short CD_ERR		; BAD CARD
	;mov	dx, PORT+7
	mov	dx, [HF_PORT]
	add	dl, 7
	mov	al, DIAG_CMD		; START DIAGNOSE
	out	dx, al
	call	NOT_BUSY		; WAIT FOR IT TO COMPLETE
	mov	ah, TIME_OUT
	jnz	short CD_EXIT 		; TIME OUT ON DIAGNOSTIC
	;mov	dx, HF_PORT+1		; GET ERROR REGISTER
	mov	dx, [HF_PORT]
	inc	dl
	in	al, dx
	; 07/08/2022
	;mov	[HF_ERROR], al		; SAVE IT
	mov	ah, 0
	cmp	al, 1			; CHECK FOR ALL OK
	je	short CD_EXIT
CD_ERR:
	mov	ah, BAD_CNTLR
CD_EXIT:
	mov	[DISK_STATUS1], ah
	retn

	; 16/07/2022 - TRDOS 386 v2.0.5
	; 10/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

;--------------------------------------------------------
; COMMAND						:
;	THIS ROUTINE OUTPUTS THE COMMAND BLOCK		:
; OUTPUT						:
;	BL = STATUS					:
;	BH = ERROR REGISTER				:
;--------------------------------------------------------

COMMAND:
	;push	ebx ; 10/07/2022	; WAIT FOR SEEK COMPLETE AND READY
	;;mov	ecx, DELAY_2		; SET INITIAL DELAY BEFORE TEST
COMMAND1:
	;;push	ecx			; SAVE LOOP COUNT
	call	TST_RDY 		; CHECK DRIVE READY
	;;pop	ecx
	;pop	ebx ; 10/07/2022
	jz	short COMMAND2		; DRIVE IS READY
        cmp	byte [DISK_STATUS1], TIME_OUT ; TST_RDY TIMED OUT--GIVE UP
	;jz	short CMD_TIMEOUT
	;;loop	COMMAND1		; KEEP TRYING FOR A WHILE
	;jmp	short COMMAND4		; ITS NOT GOING TO GET READY
	jne	short COMMAND4
CMD_TIMEOUT:
	mov	byte [DISK_STATUS1], BAD_CNTLR
COMMAND4:
	;;pop	ebx ; 10/07/2022
        cmp	byte [DISK_STATUS1], 0	; SET CONDITION CODE FOR CALLER
	retn
COMMAND2:
	;;pop	ebx ; 10/07/2022
	;push	edi ; 10/07/2022
	mov	byte [HF_INT_FLAG], 0	; RESET INTERRUPT FLAG
	cli				; INHIBIT INTERRUPTS WHILE CHANGING MASK
	in	al, INTB01		; TURN ON SECOND INTERRUPT CHIP
	;and	al, 0BFh
	and	al, 3Fh			; Enable IRQ 14 & 15
	;jmp	$+2
	IODELAY
	out	INTB01, al
	in	al, INTA01		; LET INTERRUPTS PASS THRU TO
	and	al, 0FBh 		; SECOND CHIP
	;jmp	$+2
	IODELAY
	out	INTA01, al
	sti
	;xor	edi, edi		; INDEX THE COMMAND TABLE
	; 10/07/2022
	xor	ecx, ecx
	;mov	dx, HF_PORT+1		; DISK ADDRESS
	mov	dx, [HF_PORT]
	inc	dl
	test	byte [CONTROL_BYTE], 0C0h ; CHECK FOR RETRY SUPPRESSION
	jz	short COMMAND3
	mov	al, [CMD_BLOCK+6] 	; YES-GET OPERATION CODE
	and	al, 0F0h 		; GET RID OF MODIFIERS
	cmp	al, 20h			; 20H-40H IS READ, WRITE, VERIFY
	jb	short COMMAND3
	cmp	al, 40h
	ja	short COMMAND3
	or	byte [CMD_BLOCK+6], NO_RETRIES 
					; VALID OPERATION FOR RETRY SUPPRESS
COMMAND3:
	;mov	al, [CMD_BLOCK+edi]	; GET THE COMMAND STRING BYTE
	; 10/07/2022
	mov	al, [CMD_BLOCK+ecx]
	out	dx, al			; GIVE IT TO CONTROLLER
	IODELAY
	;inc	edi			; NEXT BYTE IN COMMAND BLOCK
	; 10/07/2022
	inc	ecx
	;inc	dx			; NEXT DISK ADAPTER REGISTER
	inc	edx   ; 10/07/2022	
	;cmp	di, 7 ; 01/01/2015	; ALL DONE?
	;jne	short COMMAND3		; NO--GO DO NEXT ONE
	cmp	cl, 7 ; 10/07/2022
	jb	short COMMAND3
	;pop	edi ; 10/07/2022
	retn				; ZERO FLAG IS SET

;CMD_TIMEOUT:
;	mov	byte [DISK_STATUS1], BAD_CNTLR
;COMMAND4:
;	pop	ebx
;	cmp	byte [DISK_STATUS1], 0 	; SET CONDITION CODE FOR CALLER
;	retn

	; 16/07/2022 - TRDOS 386 v2.0.5
	; 10/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

;----------------------------------------
;	WAIT FOR INTERRUPT		:
;----------------------------------------
;WAIT:
_WAIT:
	sti				; MAKE SURE INTERRUPTS ARE ON
	;sub	cx, cx			; SET INITIAL DELAY BEFORE TEST
	;clc
	;mov	ax, 9000h		; DEVICE WAIT INTERRUPT
	;int	15h
	;jc	short WT2		; DEVICE TIMED OUT
	;mov	bl, DELAY_1		; SET DELAY COUNT

	;mov	bl, WAIT_HDU_INT_HI
	;; 21/02/2015
	;;mov	bl, WAIT_HDU_INT_HI + 1
	;;mov	cx, WAIT_HDU_INT_LO
	mov	ecx, WAIT_HDU_INT_LH
					; (AWARD BIOS -> WAIT_FOR_MEM)
;-----	WAIT LOOP

WT1:	
	;test	byte [HF_INT_FLAG], 80h	; TEST FOR INTERRUPT
	test 	byte [HF_INT_FLAG], 0C0h
	;loopz	WT1
	jnz	short WT3		; INTERRUPT--LETS GO
	;dec	bl
	;jnz	short WT1		; KEEP TRYING FOR A WHILE

WT1_hi:
	in	al, SYS1 ; 61h (PORT_B)	; wait for lo to hi
	test	al, 10h			; transition on memory
	jnz	short WT1_hi		; refresh.
WT1_lo:
	in	al, SYS1 		; 061h (PORT_B)	
	test	al, 10h			
	jz	short WT1_lo
	loop	WT1
	;;or	bl, bl
	;;jz	short WT2	
	;;dec	bl
	;;jmp	short WT1
	;dec	bl
	;jnz	short WT1	
WT2:	
	; 10/07/2022
	;mov	byte [DISK_STATUS1], TIME_OUT ; REPORT TIME OUT ERROR
	mov	al, TIME_OUT
	jmp	short WT4
WT3:
	;mov	byte [DISK_STATUS1], 0
	;mov	byte [HF_INT_FLAG], 0
	sub	al, al ; 0
	mov	byte [HF_INT_FLAG], al
WT4:
NB2:	
	mov	byte [DISK_STATUS1], al

	;cmp	byte [DISK_STATUS1], 0 	; SET CONDITION CODE FOR CALLER
	and	al, al
	; zf = 0 -> time out, zf = 1 -> ok
	retn

	; 16/07/2022 - TRDOS 386 v2.0.5
	; 10/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

;----------------------------------------
;	WAIT FOR CONTROLLER NOT BUSY	:
;----------------------------------------
NOT_BUSY:
	sti				; MAKE SURE INTERRUPTS ARE ON
	;push	ebx
	;sub	cx, cx			; SET INITIAL DELAY BEFORE TEST
	mov	dx, [HF_PORT]
	add	dl, 7			; Status port (HF_PORT+7)
	;mov	bl, DELAY_1
					; wait for 10 seconds
	;mov 	cx, WAIT_HDU_INT_LO	; 1615h
	;;mov 	bl, WAIT_HDU_INT_HI	;   05h
	;mov	bl, WAIT_HDU_INT_HI + 1
	mov	ecx, WAIT_HDU_INT_LH  ; 21/02/2015
	;
	;;mov	byte [wait_count], 0    ; Reset wait counter
NB1:	
	in	al, dx			; CHECK STATUS
	;test	al, ST_BUSY
	and	al, ST_BUSY
	;loopnz NB1
	jz	short NB2 ; al = 0	; NOT BUSY--LETS GO
	;dec	bl			
	;jnz	short NB1		; KEEP TRYING FOR A WHILE

NB1_hi: 
	in	al, SYS1		; wait for hi to lo
	test	al, 010h		; transition on memory
	jnz	short NB1_hi		; refresh.
NB1_lo: 
	in	al, SYS1
	test	al, 010h
	jz	short NB1_lo
	loop	NB1
	;dec	bl
	;jnz	short NB1
	;
	;;cmp	byte [wait_count], 182  ; 10 seconds (182 timer ticks)
	;;jb	short NB1
	;
	;mov	byte [DISK_STATUS1], TIME_OUT ; REPORT TIME OUT ERROR
	;jmp	short NB3
	mov	al, TIME_OUT
;NB2:	
	jmp	short NB2 ; 10/07/2022

;	;mov	byte [DISK_STATUS1], 0
;;NB3:	
;	;pop	ebx
;	mov	[DISK_STATUS1], al	;;; will be set after return
;	;cmp	byte [DISK_STATUS1], 0 	; SET CONDITION CODE FOR CALLER
;	or	al, al			; (zf = 0 --> timeout)
;	retn

;----------------------------------------
;	WAIT FOR DATA REQUEST		:
;----------------------------------------
WAIT_DRQ:
	;mov	cx, DELAY_3
	;mov	dx, HF_PORT+7
	mov	dx, [HF_PORT]
	add	dl, 7
	;;mov	bl, WAIT_HDU_DRQ_HI	; 0
	;mov	cx, WAIT_HDU_DRQ_LO	; 1000 (30 milli seconds)
					; (but it is written as 2000
					; micro seconds in ATORGS.ASM file
					; of Award Bios - 1999, D1A0622)
	mov 	ecx, WAIT_HDU_DRQ_LH ; 21/02/2015 
WQ_1:
	in	al, dx			; GET STATUS
	test	al, ST_DRQ		; WAIT FOR DRQ
	jnz	short WQ_OK
	;loop	WQ_1			; KEEP TRYING FOR A SHORT WHILE
WQ_hi:	
	in	al, SYS1		; wait for hi to lo
	test	al, 010h		; transition on memory
	jnz	short WQ_hi		; refresh.
WQ_lo:  
	in	al, SYS1
	test	al, 010h
	jz	short WQ_lo
	loop	WQ_1

	mov	byte [DISK_STATUS1], TIME_OUT ; ERROR
	stc
WQ_OK:
	retn
;WQ_OK:
	;clc
	;ret

	; 16/07/2022 - TRDOS 386 v2.0.5

;----------------------------------------
;	CHECK FIXED DISK STATUS BYTE	:
;----------------------------------------
CHECK_ST:
	;mov	dx, HF_PORT+7		; GET THE STATUS
	mov	dx, [HF_PORT]
	add	dl, 7
	
	; 17/02/2016
	;(http://wiki.osdev.org/ATA_PIO_Mode)
	;"delay 400ns to allow drive to set new values of BSY and DRQ"
	in	al, dx
	;in	al, dx ; 100ns
	;in	al, dx ; 100ns
 	;in	al, dx ; 100ns
	NEWIODELAY ; 18/02/2016 (AWARD BIOS - 1999, 'CKST' in AHSDK.ASM)
	;

	; 16/07/2022
	mov	[HF_STATUS], al
	;mov	ah, 0
	sub	ah, ah ; 0
	test	al, ST_BUSY		; IF STILL BUSY
	jnz	short CKST_EXIT		; REPORT OK
	mov	ah, WRITE_FAULT
	test 	al, ST_WRT_FLT		; CHECK FOR WRITE FAULT
	jnz	short CKST_EXIT
	mov	ah, NOT_RDY
	test	al, ST_READY		; CHECK FOR NOT READY
	jz	short CKST_EXIT
	mov	ah, BAD_SEEK
	test	al, ST_SEEK_COMPL	; CHECK FOR SEEK NOT COMPLETE
	jz	short CKST_EXIT
	mov	ah, DATA_CORRECTED
	test	al, ST_CORRCTD		; CHECK FOR CORRECTED ECC
	jnz	short CKST_EXIT
	;mov	ah, 0
	xor	ah, ah ; 0
CKST_EXIT:
	mov	[DISK_STATUS1], ah	; SET ERROR FLAG
	cmp	ah, DATA_CORRECTED	; KEEP GOING WITH DATA CORRECTED
	je	short CKST_EX1
	;cmp	ah, 0
	and	ah, ah
CKST_EX1:
	retn

	; 16/07/2022 - TRDOS 386 v2.0.5

;----------------------------------------
;	CHECK FIXED DISK ERROR REGISTER :
;----------------------------------------
CHECK_ER:
	;mov	dx, HF_PORT+1		; GET THE ERROR REGISTER
	mov	dx, [HF_PORT]		;
	inc	dl
	in	al, dx
	; 16/07/2022
	;mov	[HF_ERROR], al
	;push	ebx  ; 21/02/2015
	sub	ecx, ecx
	;mov	ecx, 8			; TEST ALL 8 BITS
	mov	cl, 8
CK1:	
	shl	al, 1			; MOVE NEXT ERROR BIT TO CARRY
	jc	short CK2		; FOUND THE ERROR
	loop	CK1			; KEEP TRYING
CK2:
	;mov	ebx, ERR_TBL		; COMPUTE ADDRESS OF
	;add	ebx, ecx		; ERROR CODE
	add	ecx, ERR_TBL ; 16/07/2022	

	;;;mov	ah, byte [cs:bx]	; GET ERROR CODE
	;;mov	ah, [bx]
	;mov	ah, [ebx] ; 21/02/2015
	mov	ah, [ecx]	
CKEX:
	mov	[DISK_STATUS1], ah	; SAVE ERROR CODE
	; 16/07/2022
	;pop	ebx
	;;cmp	ah, 0
	;and	ah, ah
	retn

;--------------------------------------------------------
; CHECK_DMA						:
;  -CHECK ES:BX AND # SECTORS TO MAKE SURE THAT IT WILL :
;   FIT WITHOUT SEGMENT OVERFLOW.			:
;  -ES:BX HAS BEEN REVISED TO THE FORMAT SSSS:000X	:
;  -OK IF # SECTORS < 80H (7FH IF LONG READ OR WRITE)	:
;  -OK IF # SECTORS = 80H (7FH) AND BX <= 00H (04H)	:
;  -ERROR OTHERWISE					:
;--------------------------------------------------------

	; 16/07/2022 - TRDOS 386 v2.0.5
	; (not needed for hard disks and 32 bit OS)

;CHECK_DMA:
;	; 11/04/2021 (32 bit push/pop)
;	push	eax			; SAVE REGISTERS
;	mov	ax, 8000h		; AH = MAX # SECTORS
;					; AL = MAX OFFSET
;	test	byte [CMD_BLOCK+6], ECC_MODE
;	jz	short CKD1
;	mov	ah, 7F04h		; ECC IS 4 MORE BYTES
;CKD1:	
;	cmp	ah, [CMD_BLOCK+1] 	; NUMBER OF SECTORS
;	ja	short CKDOK		; IT WILL FIT
;	jb	short CKDERR		; TOO MANY
;	cmp	al, bl			; CHECK OFFSET ON MAX SECTORS
;	;jb	short CKDERR		; ERROR
;	jnb	short CKDOK ; 16/07/2022
;;CKDOK:	
;	;clc				; CLEAR CARRY
;	;pop	eax
;	;retn				; NORMAL RETURN
;CKDERR:
;	;stc				; INDICATE ERROR
;	mov	byte [DISK_STATUS1], DMA_BOUNDARY
;CKDOK:
;	pop	eax
;	retn

;----------------------------------------
;	SET UP EBX-> DISK PARMS		:
;----------------------------------------
					
; INPUT -> DL = 0 based drive number
; OUTPUT -> EBX = disk parameter table address

GET_VEC:
	;sub	ax, ax			; GET DISK PARAMETER ADDRESS
	;mov	es, ax
	;test	dl, 1
	;jz	short GV_0
;	les	bx, [HF1_TBL_VEC] 	; ES:BX -> DRIVE PARAMETERS
;	jmp	short GV_EXIT
;GV_0:
;	les 	bx,[HF_TBL_VEC]		; ES:BX -> DRIVE PARAMETERS
;
	xor	ebx, ebx
	mov	bl, dl
	;02/01/2015
	;xor	bh, bh
	;shl	bl, 1			; port address offset
	;mov	ax, [bx+hd_ports]	; Base port address (1F0h, 170h)
	;shl	bl, 1			; dpt pointer offset
	shl	bl, 2
	;add	bx, HF_TBL_VEC		; Disk parameter table pointer
	add	ebx, HF_TBL_VEC ; 21/02/2015
	;push	word [bx+2]		; dpt segment
	;pop	es
	;mov	bx, [bx]		; dpt offset
	mov	ebx, [ebx]		
;GV_EXIT:
	retn

hdc1_int: ; 21/02/2015
;--- HARDWARE INT 76H -- ( IRQ LEVEL 14 ) -----------------------
;								:
;	FIXED DISK INTERRUPT ROUTINE				:
;								:
;----------------------------------------------------------------

; 22/12/2014
; IBM PC-XT Model 286 System BIOS Source Code - DISK.ASM (HD_INT)
;	 '11/15/85'
; AWARD BIOS 1999 (D1A0622) 
;	Source Code - ATORGS.ASM (INT_HDISK, INT_HDISK1)

;int_76h:
HD_INT:
	; 11/04/2021 (32 bit push/pop)
	push	eax
	push	ds
	;call	DDS
	; 21/02/2015 (32 bit, 386 pm modification)
	mov	ax, KDATA
	mov 	ds, ax
	;
	;;mov	@HF_INT_FLAG,0FFH	; ALL DONE
        ;mov	byte [CS:HF_INT_FLAG], 0FFh
	mov	byte [HF_INT_FLAG], 0FFh
	;
	push	edx
	mov	dx, HDC1_BASEPORT+7	; Status Register (1F7h)
					; Clear Controller
Clear_IRQ1415:				; (Award BIOS - 1999)
	in	al, dx			;
	pop	edx
	NEWIODELAY
	;
	mov	al, EOI			; NON-SPECIFIC END OF INTERRUPT
	out	INTB00, al		; FOR CONTROLLER #2
	;jmp	$+2			; WAIT
	NEWIODELAY
	out	INTA00, al		; FOR CONTROLLER #1
	pop	ds
	;sti				; RE-ENABLE INTERRUPTS
	;mov	ax, 9100h		; DEVICE POST
	;int	15h			;  INTERRUPT
irq15_iret: ; 25/02/2015
	pop	eax
	iretd				; RETURN FROM INTERRUPT

hdc2_int: ; 21/02/2015
;--- HARDWARE INT 77H -- ( IRQ LEVEL 15 ) -----------------------
;								:
;	FIXED DISK INTERRUPT ROUTINE				:
;								:
;----------------------------------------------------------------

;int_77h:
HD1_INT:
	; 11/04/2021 (32 bit push/pop)
	push	eax
	; Check if that is a spurious IRQ (from slave PIC)
	; 25/02/2015 (source: http://wiki.osdev.org/8259_PIC)
	mov	al, 0Bh  ; In-Service Register
	out	0A0h, al
        jmp short $+2
	jmp short $+2
	in	al, 0A0h
	and 	al, 80h ; bit 7 (is it real IRQ 15 or fake?)
	jz	short irq15_iret ; Fake (spurious) IRQ, do not send EOI)
	;
	push	ds
	;call	DDS
	; 21/02/2015 (32 bit, 386 pm modification)
	mov	ax, KDATA
	mov 	ds, ax
	;
	;;mov	@HF_INT_FLAG,0FFH	; ALL DONE
        ;or	byte [CS:HF_INT_FLAG],0C0h 
	or	byte [HF_INT_FLAG], 0C0h
	;
	push	edx
	mov	dx, HDC2_BASEPORT+7	; Status Register (177h)
					; Clear Controller (Award BIOS 1999)
	jmp	short Clear_IRQ1415

;%include 'diskdata.inc' ; 11/03/2015
;%include 'diskbss.inc' ; 11/03/2015

;////////////////////////////////////////////////////////////////////
;; END OF DISK I/O SYTEM ///