; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel) - v2.0.5 - diskbss.s
; ----------------------------------------------------------------------------
; Last Update: 06/08/2022 (Previous: 24/01/2016)
; ----------------------------------------------------------------------------
; Beginning: 24/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
;
; Derived from 'Retro UNIX 386 Kernel - v0.2.1.0' source code by Erdogan Tan
; diskbss.inc (10/07/2015)
;
; Derived from 'IBM PC-XT-286' BIOS source code (1986) 
; ****************************************************************************

; Retro UNIX 386 v1 Kernel - DISKBSS.INC
; Last Modification: 10/07/2015
;	(Unitialized Disk Parameters Data section for 'DISKIO.INC') 

alignb 2

;----------------------------------------
;	TIMER DATA AREA 		:
;----------------------------------------

TIMER_LH:	; 16/02/2015
TIMER_LOW:      resw	1               ; LOW WORD OF TIMER COUNT
TIMER_HIGH:     resw	1               ; HIGH WORD OF TIMER COUNT
TIMER_OFL:      resb 	1               ; TIMER HAS ROLLED OVER SINCE LAST READ

;----------------------------------------
;	DISKETTE DATA AREAS		:
;----------------------------------------

SEEK_STATUS:	resb	1
MOTOR_STATUS:	resb	1
MOTOR_COUNT:	resb	1
DSKETTE_STATUS:	resb	1
NEC_STATUS:	resb	7

;----------------------------------------
;	ADDITIONAL MEDIA DATA		:
;----------------------------------------

LASTRATE:	resb 	1
HF_STATUS:	resb 	1
; 06/08/2022
;HF_ERROR:	resb 	1
HF_INT_FLAG:	resb 	1
; 06/08/2022
;HF_CNTRL:	resb 	1
;DSK_STATE:	resb 	4
; 06/08/2022
DSK_STATE:	resb	2 	
DSK_TRK:	resb 	2

;----------------------------------------
;	FIXED DISK DATA AREAS		:
;----------------------------------------

DISK_STATUS1:	resb 	1		; FIXED DISK STATUS
HF_NUM:		resb 	1		; COUNT OF FIXED DISK DRIVES
CONTROL_BYTE:	resb 	1		; HEAD CONTROL BYTE
;@PORT_OFF	resb	1		; RESERVED (PORT OFFSET)
;port1_off	resb	1		; Hard disk controller 1 - port offset
;port2_off	resb	1		; Hard disk controller 2 - port offset

alignb 4

;HF_TBL_VEC:	resd	1 		; Primary master disk param. tbl. pointer
;HF1_TBL_VEC:	resd	1		; Primary slave disk param. tbl. pointer
HF_TBL_VEC: ; 22/12/2014	
HDPM_TBL_VEC:	resd	1 		; Primary master disk param. tbl. pointer
HDPS_TBL_VEC:	resd	1		; Primary slave disk param. tbl. pointer
HDSM_TBL_VEC:	resd	1 		; Secondary master disk param. tbl. pointer
HDSS_TBL_VEC:	resd	1		; Secondary slave disk param. tbl. pointer

; 03/01/2015
LBAMode:     	resb	1

; *****************************************************************************
