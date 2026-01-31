; ****************************************************************************
; SETMODE.ASM - Set Video Mode sample for MSDOS (For TRDOS 386 v2, Erdogan) 
; ----------------------------------------------------------------------------
; Special fd boot sector for int 10h video hacking (reverse vbios engineering)
; ((for TRDOS 386 v2 project, for video bios functions in protected mode))
; ----------------------------------------------------------------------------
; Beginning & Last Update: 16/11/2020
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15
; ----------------------------------------------------------------------------
; nasm setmode.s -l setmode.lst -o SETMODE.COM -Z error.txt
; ****************************************************************************

		[BITS 16]
		[ORG 100h]

		mov	esi, message1
		call	print_msg

		xor	ah, ah
		int	16h

		mov	ax, 1 ; mode 1 
		int	10h

		mov	esi, message2
		call	print_msg
key_loop:
		xor	ah, ah
		int	16h

		cmp	al, 13 ; ENTER key
		je	short crlf
			
		cmp	al, 27 ; ESCape key
		je	short terminate

		cmp	al, 3 ; CTRL+C
		je	short terminate

		cmp	ax, 0 ; CTRL+BREAK
		je	short terminate
print_char:
		mov	ah, 0Eh
		mov	bx, 07h
		int	10h
		jmp	short key_loop 
crlf:
		mov	ah, 0Eh  ; Carriage return
		mov	bx, 07h
		int	10h
		mov	al, 0Ah  ; Line feed 
		jmp	short print_char
terminate:
		mov	ax, 3 ; mode 3
		int	10h		

		int	20h
_retn:
		retn	
print_msg:
		mov	ah, 0Eh
		mov	bx, 07h
next_char:
		lodsb
		and	al, al
		jz	short _retn
		int	10h		

		jmp	short next_char

;-----------------------------------------------------------------
;  MESSAGES
;-----------------------------------------------------------------

message1:
	db	0Dh, 0Ah
	db	"Press a key to set video mode 1 (40x25 text)"
	db	0Dh, 0Ah, 0
message2:
	db	0Dh, 0Ah, 0Dh, 0Ah
	db	"Press ESC to exit"
	db	0Dh, 0Ah, 0