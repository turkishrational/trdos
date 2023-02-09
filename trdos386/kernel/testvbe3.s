; ****************************************************************************
; testvbe3 - TRDOS 386 (TRDOS v2.0) Test Program - 16 bit PM vbios test 
; ----------------------------------------------------------------------------
;
; 01/12/2020
;
; ****************************************************************************

struc PMInfo  ;  VESA VBE3 PMInfoBlock ('PMID' block)

 .Signature:	resb 4  ; db 'PMID' ; PM Info Block Signature
 .EntryPoint:	resw 1	; Offset of PM entry point within BIOS
 .PMInitialize: resw 1	; Offset of PM initialization entry point
 .BIOSDataSel:	resw 1 	; Selector to BIOS data area emulation block
 .A0000Sel:	resw 1	; Selector to access A0000h physical mem
 .B0000Sel:	resw 1  ; Selector to access B0000h physical mem
 .B8000Sel:	resw 1	; Selector to access B8000h physical mem
 .CodeSegSel:	resw 1	; Selector to access code segment as data
 .InProtectMode: resb 1 ; Set to 1 when in protected mode
 .Checksum:	resb 1	; Checksum byte for structure
 .size:

endstruc
;========================================================================
; TEST VESA VBE 3 16 BIT PROTECTED MODE INTERFACE FAR CALL FOR TRDOS 386 
;========================================================================

[BITS 16]	; 16-bit intructions

[ORG 0] 
START_CODE:
		jmp	short pmentry
pminit:
		mov	ds, [cs:CodeSegSel]
		mov	[_5345], ax
		mov	[_5349], ax			
pmentry:
		call	show_message
		retf		
show_message:
		mov	es, [B8000Sel]
		mov	ds, [cs:CodeSegSel]
		mov	ah, 4Eh
		mov	si, message
		xor	di, di
smsg_1:						
		lodsb
		or	al, al
		jz	short smsg_2
		stosw
		jmp	short smsg_1
smsg_2:
		retn
_5345:
		dw	0
PMInfoBlock:	db	'PMID'
PM_EntryPoint:  dw 	pm_entry
PM_Initialize:  dw	pm_init
BIOSDataSel:	dw	0   
A0000Sel        dw	0A000h
B0000Sel        dw	0B000h
B8000Sel        dw	0B800h
CodeSegSel      dw	0C000h
InProtectMode   db	0
CheckSum        db	88h
_5349:		
		dw	0
pm_entry:
		jmp	pmentry
pm_init:
		jmp	pminit
		
		db	0

;-----------------------------------------------------------------
;  MESSAGE
;-----------------------------------------------------------------

message:
	db	"VESA VBE3 16 bit protected mode test for TRDOS 386"
	db	0 