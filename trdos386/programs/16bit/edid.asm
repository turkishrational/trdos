; ****************************************************************************
; edid.asm - TRDOS 386 (TRDOS v2.0.3) Test Program - MSDOS version
; ----------------------------------------------------------------------------
;
; 19/01/2021 (Erdogan Tan)
;
; ****************************************************************************

;========================================================================
; READ VESA EDID (MONITOR INFO) 
;========================================================================

; Modified from TRDOS 386 version (edid.s)

[BITS 16]

[ORG 100h] 
START_CODE:
		mov	si, TrDOS_Welcome
		call	print_msg

		; Read EDID
		mov	ax, 4F15h
		mov	bl, 01h
		xor	dx, dx
		xor	cx, cx
		;push	cs
		;pop	es
		mov	di, edid_buffer	
		int	10h

		cmp	ax, 4Fh
		jne	short edid_read_error

		mov	si, edid_header
		call	print_msg

		mov	si, edid_buffer
		call	convert_to_hex_tbl
	
terminate:
		int	20h
here:
		jmp	short here

edid_read_error:
		mov	si, edid_rerr_msg
		call	print_msg
		jmp	short terminate

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Subroutine - print text/message on display
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_msg:
		mov	bx, 07h
		mov	ah, 0Eh
ploop:
		lodsb
		or	al, al
		jz	short pexit
		int	10h
		jmp	short ploop
pexit:			
		retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Subroutine - Convert to hexadecimal character table
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

convert_to_hex_tbl:
	; si = binary table address

	mov	cx, 8
_h_1:
	push	cx
	mov	cl, 16
	mov	di, edid_row_chars - 2
_h_2:
	;add	di, 2
	inc	di
	inc	di
	lodsb
	xor	bh, bh
	mov	bl, al
	and	bl, 0Fh ; low 4 bits

	mov	ah, [_hexchars+bx]
	shr	al, 4  ; high 4 bits
	mov	bl, al
	mov	al, [_hexchars+bx]	
	stosw	
	loop	_h_2		

	push	si
	mov	si, edid_row
	call	print_msg
	pop	si

	pop	cx
	loop	_h_1

	retn

_hexchars: 
	db	"0123456789ABCDEF"

;=============================================================================
;        	initialized data
;=============================================================================		

;-----------------------------------------------------------------------------
;  MESSAGES
;-----------------------------------------------------------------------------

TrDOS_Welcome:
	db	0Dh, 0Ah
	db	"VESA EDID reading program for TRDOS 386 v2 (MSDOS version)"
	db	0Dh, 0Ah
	db	"by Erdogan TAN (19/01/2021)"
	db	0Dh, 0Ah, 0Dh, 0Ah, 0

edid_header:
	db	"EDID :", 0Dh, 0Ah, 0Dh, 0Ah,0	
edid_row:
	db	20h
edid_row_chars:	
	db	"00h 00h 00h 00h 00h 00h 00h 00h "
	db	"00h 00h 00h 00h 00h 00h 00h 00h "
	db	0Dh, 0Ah, 0

edid_rerr_msg:
	db	"EDID read error !"
	db	0Dh, 0Ah
	db	"(EDID is not ready or Video Bios function error!)"
	db	0Dh, 0Ah, 0

;=============================================================================
;        	uninitialized data
;=============================================================================

bss_start:

ABSOLUTE bss_start

alignb 2

buffer:

edid_buffer:

	resb	128
		
bss_clear_end:

bss_end:	 	