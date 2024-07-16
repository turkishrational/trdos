; 18/10/2023
; 17/10/2023 - Erdogan Tan
; MSDOS Test Program (for TRDOS 386 development)
; Show Video Hardwware State for Video MODE 13h

[BITS 16]

start:
	[Org 100h] ; COM file

	mov	si, start_msg
	call	print_msg

	mov	si, press_a_key_msg
	call	print_msg

	; wait a key press just after start message
	xor	ah, ah
	int	16h

	cmp	al, 1Bh  ; ESC key ?
	je	short terminate

	; check Function 1Ch (Save/restore video state)
	; support at first

	; check video state function support
	mov	ax, 1C00h ; return state buffer size
	mov	cx, 1 ; video hardware state (only)
	int	10h

	cmp	al, 1Ch
	je	short continue ; function supported

error:
	; print error message and terminate
	mov	si, vstate_f_err_msg
	call	print_msg
	jmp	short terminate

continue:
	; Note: BOCHS, QEMU etc emulators return buffer
	; size in bytes not as 64 byte block counts
	cmp	bx, 70
	jnb	short chk_up_limit
	; A real video bios (for example ATI bios)
	; returns buf size as
	; 64 byte blocks (bx must be 2 here)
	mov	cl, 6
	shl	bx, cl ; * 64
chk_up_limit:
	; buffer size here bx is 70 for QEMU
	;		    and 128 for real computer
	;cmp	bx, 128
	cmp	bx, 256	; NVIDIA vbios buf size = 192 (3*64)
			; ATI vbios buf size = 128 (2*64)
	ja	short error ; unknown structure
	mov	bp, bx
setmode_13h:	
	;mov	ah, 0
	;mov	al, 13h	; 300x200x256 graphics mode
	mov	ax, 13h
	int	10h

	; save video state for mode 13h
	mov	bx, vstate_buf
	mov	cx, 1 ; video hardware state (only)
	;mov	ah, 1Ch
	;mov	al, 1	; save video state
	mov	ax, 1C01h
	int	10h

	; Return to 80x25 text mode
	;mov	ah, 0
	;mov	al, 3
	mov	ax, 3
	int	10h

	; save video state for mode 03h
	mov	bx, vstate_buf+128
	mov	cx, 1 ; video hardware state (only)
	;mov	ah, 1Ch
	;mov	al, 1  ; save video state
	mov	ax, 1C01h
	int	10h

	mov	si, mode_13h_state_msg
	call	print_msg

	mov	si, vstate_buf
	call	print_video_state ; as hex sting

	mov	si, press_a_key_msg
	call	print_msg

	; wait a key press
	xor	ah, ah
	int	16h

	cmp	al, 1Bh  ; ESC key ?
	je	short terminate
	
	mov	byte [mode_num_str], '0'

	mov	si, mode_03h_state_msg
	call	print_msg
	
	mov	si, vstate_buf+128
	call	print_video_state ; as hex sting

terminate:
	call	CRLF

	int	20h
halt:
	hlt
	jmp	short halt

print_video_state:
	;mov	cx, 70 ; byte count (buffer size)
	mov	cx, bp ; buffer size
	;mov	si, vstate_buf
	xor	dx, dx ; 0
pvs_1:
	lodsb
	call	bintohex
	push	ax
	mov	ah, 0Eh
	mov	bx, 07h
	int	10h
	pop	ax
	mov	al, ah
	mov	ah, 0Eh
	int	10h		
	mov	al, 'h'
	int	10h
	inc	dx
	cmp	dx, 16
	jb	short pvs_3
	sub	dx, dx ; 0
	mov	al, ' '
	int	10h
	call	CRLF
pvs_2:
	loop	pvs_1
CRLF:
	mov	ah, 0Eh
	mov	al, 0Dh ; CR
	mov	bx, 07h
	int	10h
	mov	al, 0Ah ; LF
	int 	10h
	retn
pvs_3:
	cmp	cx, 1
	jna	short pvs_4
	mov	al, ','
	int	10h
pvs_4:
	mov	al, ' '
	int	10h
	jmp	short pvs_2

bintohex:
	xor	ah, ah
	mov	bx, ax
	mov	ah, al
	shr	bl, 1
	shr	bl, 1
	shr	bl, 1
	shr	bl, 1	; 0?h
	mov	al, [bx+hex_chars]
	and	ah, 0Fh	; ?0h
	mov	bl, ah
	mov	ah, [bx+hex_chars]
	; ax = hex chars ; ??h
pmsg_ret:
	retn

print_msg:
	mov	ah, 0Eh
	mov	bx, 7
pmsg_nch:
	lodsb
	and	al, al
	jz	short pmsg_ret
	int	10h
	jmp	short pmsg_nch
	
start_msg:
	db 0Dh, 0Ah
	db 'Video State Test Program (for TRDOS 386 development)'
	db 0Dh, 0Ah
	db 'Erdogan Tan - 17/10/2023'
	db 0Dh, 0Ah, 0

press_a_key_msg:
	db 0Dh, 0Ah
	db 'Press ESC to CANCEL or press another key to continue.'
	db 0Dh, 0Ah, 0	

vstate_f_err_msg:
	db 0Dh, 0Ah
	db 'Error !'
	db 0Dh, 0Ah
	db 'INT 10h Function 1Ch is not supported !'
	db 0Dh, 0Ah, 0

mode_03h_state_msg:
mode_13h_state_msg:
	db 0Dh, 0Ah
	db 'Video MODE '
mode_num_str:
	db '13h'
	db 0Dh, 0Ah
	db 'VGA video hardware state : '
	db 0Dh, 0Ah, 0

hex_chars:
	db '0123456789ABCDEF',0

vstate_buf:
	db 0FFh
