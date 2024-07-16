; ****************************************************************************
; COPY VIDEO BIOS CODE - Erdogan Tan - 25/11/2018
; ****************************************************************************
; 28/11/2018

[BITS 16]
[ORG 100h]

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Copy video bios to data segment
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	bp, cs
	add	bp, 1000h
	push	es
	push	ds
	mov	es, bp
	xor	di, di
	mov	si, 0C000h
	mov	ds, si
	xor	si, si
	mov	cx, 32768
	rep	movsw
	pop	ds
	pop	es

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Create video bios image file
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		
	mov	dx, img_file_name
	;mov	cx, 0 ; File Attributes
	mov	ah, 3Ch ; MS-DOS Function = Create File
	int	21h
	jnc	short cvb_2
cvb_0:
	mov	si, Msg_Error
cvb_1:
	call	print_msg

	mov	si, CRLF
	call	print_msg
	mov	ax, 4C00h		; terminate
	int	21h

cvb_2:
	mov	si, CRLF
	call	print_msg

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Open image file for writing
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	al, 2 ; open for reading and writing
	;mov	dx, img_file_name
	mov	ah, 3Dh ; open file
	int	21h
	jc	short cvb_0

	mov	[img_file_handle], ax

	mov	si, Msg_Writing_Bios_Image
	call	print_msg

	push	ds
	mov	bx, [img_file_handle]
	mov	ds, bp ; Video bios copy segment
	xor	dx, dx	
	mov	cx, 32768
	mov	ah, 40h ; write to file	
	int	21h
	jnc	short cvb_4
cvb_3:
	pop	ds
	jmp	short cvb_0
cvb_4:	
	;mov	bx, [cs:img_file_handle]
	;mov	cx, 32768
	add	dx, cx ; + 32768	
	mov	ah, 40h ; write to file	
	int	21h
	;pop	ds
	;jc	short cvb_0
	jc	short cvb_3

	pop	ds

	mov	si, Msg_OK
	jmp	short cvb_1

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Print messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_msg:

print_msg_LOOP:
	lodsb                           ; Load byte at DS:SI to AL
	and     al, al            
	jz      short print_msg_OK       
	mov	ah, 0Eh			
	mov     bx, 07h             
	int	10h			; BIOS Service func ( ah ) = 0Eh
					; Write char as TTY
					; AL-char BH-page BL-color
	jmp     short print_msg_LOOP           

print_msg_OK:
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Data
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

img_file_handle:
	dw	0
img_file_name:
	db	'VBIOS64K.BIN'
	db 	0

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Msg_Error:
	db	0Dh, 0Ah
	db	'Error ! '
	db	0

Msg_Writing_Bios_Image:
	db	"Writing Video BIOS image...", 0
Msg_OK:
	db	' OK.'
	db	0
CRLF:
	db	0Dh, 0Ah
	db	0
Signature:
	db	'Erdogan Tan - 28/11/2018'
	db	0