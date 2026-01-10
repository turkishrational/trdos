; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel) - v2.0.5 - kybdata.s
; ----------------------------------------------------------------------------
; Last Update: 24/07/2022 (Previous: 17/01/2016)
; ----------------------------------------------------------------------------
; Beginning: 17/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
;
; Derived from 'Retro UNIX 386 Kernel - v0.2.1.0' source code by Erdogan Tan
; kybdata.inc (11/03/2015)
;
; Derived from 'IBM PC-XT-286' BIOS source code (1986)
; ****************************************************************************

; Retro UNIX 386 v1 Kernel - KYBDATA.INC
; Last Modification: 11/03/2015
;		 (Data Section for 'KEYBOARD.INC')
;
; ///////// KEYBOARD DATA ///////////////

; 24/07/2022 - TRDOS 386 Kernel v2.0.5
; 05/12/2014
; 04/12/2014 (derived from pc-xt-286 bios source code -1986-)
; 03/06/86  KEYBOARD BIOS

;-----------------------------------------------------------------------------
;	KEY IDENTIFICATION SCAN TABLES
;-----------------------------------------------------------------------------

;-----	TABLES FOR ALT CASE ------------
;-----	ALT-INPUT-TABLE 
K30:	db	82,79,80,81,75
	db	76,77,71,72,73		; 10 NUMBER ON KEYPAD
;-----	SUPER-SHIFT-TABLE 
	db	16,17,18,19,20,21	; A-Z TYPEWRITER CHARS
	db	22,23,24,25,30,31
	db	32,33,34,35,36,37
	db	38,44,45,46,47,48
	db	49,50

;-----	TABLE OF SHIFT KEYS AND MASK VALUES
;-----	KEY_TABLE 
_K6:    db      INS_KEY                 ; INSERT KEY
	db	CAPS_KEY,NUM_KEY,SCROLL_KEY,ALT_KEY,CTL_KEY
        db      LEFT_KEY,RIGHT_KEY
_K6L    equ     $-_K6

;-----	MASK_TABLE
_K7:    db      INS_SHIFT               ; INSERT MODE SHIFT
	db	CAPS_SHIFT,NUM_SHIFT,SCROLL_SHIFT,ALT_SHIFT,CTL_SHIFT
	db	LEFT_SHIFT,RIGHT_SHIFT

;-----	TABLES FOR CTRL CASE		;---- CHARACTERS ------
_K8:	db	27,-1,0,-1,-1,-1	; Esc, 1, 2, 3, 4, 5
	db 	30,-1,-1,-1,-1,31	; 6, 7, 8, 9, 0, -
	;db	-1,127,-1,17,23,5	; =, Bksp, Tab, Q, W, E
	db	-1,127,148,17,23,5 ; 24/07/2022
	db	18,20,25,21,9,15	; R, T, Y, U, I, O
	db	16,27,29,10,-1,1	; P, [, ], Enter, Ctrl, A
	db	19,4,6,7,8,10		; S, D, F, G, H, J
	db	11,12,-1,-1,-1,-1	; K, L, :, ', `, LShift
	db	28,26,24,3,22,2		; Bkslash, Z, X, C, V, B
	db	14,13,-1,-1,-1,-1	; N, M, ,, ., /, RShift
	db	150,-1,' ',-1		; *, ALT, Spc, CL
	;				;----- FUNCTIONS ------
	db 	94,95,96,97,98,99	; F1 - F6
	db	100,101,102,103,-1,-1	; F7 - F10, NL, SL
	db	119,141,132,142,115,143	; Home, Up, PgUp, -, Left, Pad5
	db 	116,144,117,145,118,146 ; Right, +, End, Down, PgDn, Ins
	db	147,-1,-1,-1,137,138	; Del, SysReq, Undef, WT, F11, F12

;-----	TABLES FOR LOWER CASE ----------
K10:	db 	27,'1234567890-=',8,9
	db 	'qwertyuiop[]',13,-1,'asdfghjkl;',39
	db	96,-1,92,'zxcvbnm,./',-1,'*',-1,' ',-1
;-----	LC TABLE SCAN
	db	59,60,61,62,63		; BASE STATE OF F1 - F10
	db	64,65,66,67,68
	db	-1,-1			; NL, SL

;-----	KEYPAD TABLE
K15:	db	71,72,73,-1,75,-1	; BASE STATE OF KEYPAD KEYS
	db	77,-1,79,80,81,82,83
	db	-1,-1,92,133,134	; SysRq, Undef, WT, F11, F12

;-----	TABLES FOR UPPER CASE ----------
K11:	db 	27,'!@#$%',94,'&*()_+',8,0
	db 	'QWERTYUIOP{}',13,-1,'ASDFGHJKL:"'
	db	126,-1,'|ZXCVBNM<>?',-1,'*',-1,' ',-1
;-----	UC TABLE SCAN
K12:	db	84,85,86,87,88		; SHIFTED STATE OF F1 - F10
	db	89,90,91,92,93
	db	-1,-1			; NL, SL

;-----	NUM STATE TABLE
K14:	db 	'789-456+1230.'		; NUMLOCK STATE OF KEYPAD KEYS
	;
	db	-1,-1,124,135,136	; SysRq, Undef, WT, F11, F12

; 26/08/2014
; Retro UNIX 8086 v1 - UNIX.ASM (03/03/2014)
; Derived from IBM "pc-at" 
; rombios source code (06/10/1985)
; 'dseg.inc'

;---------------------------------------;
;	SYSTEM DATA AREA		;
;----------------------------------------
BIOS_BREAK	db	0		; BIT 7=1 IF BREAK KEY HAS BEEN PRESSED

;----------------------------------------
;	KEYBOARD DATA AREAS		;
;----------------------------------------

KB_FLAG		db	0		; KEYBOARD SHIFT STATE AND STATUS FLAGS
KB_FLAG_1	db	0		; SECOND BYTE OF KEYBOARD STATUS
KB_FLAG_2	db	0		; KEYBOARD LED FLAGS
KB_FLAG_3	db	0		; KEYBOARD MODE STATE AND TYPE FLAGS
ALT_INPUT	db	0		; STORAGE FOR ALTERNATE KEY PAD ENTRY
BUFFER_START	dd	KB_BUFFER 	; OFFSET OF KEYBOARD BUFFER START
BUFFER_END	dd	KB_BUFFER + 32	; OFFSET OF END OF BUFFER
BUFFER_HEAD	dd	KB_BUFFER 	; POINTER TO HEAD OF KEYBOARD BUFFER
BUFFER_TAIL	dd	KB_BUFFER 	; POINTER TO TAIL OF KEYBOARD BUFFER
; ------	HEAD = TAIL INDICATES THAT THE BUFFER IS EMPTY
KB_BUFFER	times	16 dw 0		; ROOM FOR 16 SCAN CODE ENTRIES

; /// End Of KEYBOARD DATA ///