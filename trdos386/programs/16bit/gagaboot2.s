; ****************************************************************************
; GAGABOOT2.ASM - GAGABOOT v2 for video bios hacking by Erdogan Tan
; ----------------------------------------------------------------------------
; Special fd boot sector for int 10h video hacking (reverse vbios engineering)
; ((for TRDOS 386 v2 project, for video bios functions in protected mode))
; ----------------------------------------------------------------------------
; Beginning & Last Update: 09/11/2020
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15
; ----------------------------------------------------------------------------
; nasm gagaboot2.s -l gagaboot2.lst -o GAGABOO2.BIN -Z error.txt
; ----------------------------------------------------------------------------
; Assembled/Compiled boot sector must be copied to fd0 sector 0 (bootable fd)
; ****************************************************************************

; Modified from gagaboot.s (02/11/2020) - GAGABOOT v1 -


		[BITS 16]
		[ORG 7C00h]

		jmp	short BS_01
		nop
BS_01:
		mov	ax, cs
		mov	ds, ax
		mov	es, ax
		
		cli
		mov	ss, ax
		mov     sp, 0FFFEh
		sti

		; Display program name and version !
		mov	si, program_info
		call	print_msg
BS_02:
		xor	ah, ah
		int	16h
		cmp	al, 13 ; ENTER
		je	short BS_04
		cmp	al, 27 ; ESC
		je	short BS_03
BS_03:
		call	beep
		jmp	short BS_02
BS_04:	
		mov	bx, 1000h
		mov	es, bx ; buffer segment

		mov	si, writing_vectors
		call	print_msg

		; copy interrupt vectors (0 to 127)
		; to the buffer
		;push	ds
		xor	si, si
		;mov	ds, si
		sub	di, di
		cli
		mov	cx, 256
		rep	movsw
		sti
		;pop	ds

		mov	ax, 1
		xor	bx, bx
		; es = buffer segment, 1000h
		call	fd_write
		jnc	short BS_07
BS_05:
		; error
		mov     si, error_msg
                call    print_msg
BS_06:
		mov     si, reboot_msg
                call    print_msg

                xor	ax, ax
		int	16h			; BIOS Service func ( ah ) = 0
						; Read next kbd char
						; AH-scan code AL-char code
		int	19h			; Reboot
BS_07:
		mov	si, msg_ok
		call	print_msg

		; copy video bios (64KB)
		; to the buffer
		push	ds
		mov	si, 0C000h
		mov	ds, si
		xor	si, si
		xor	di, di
		mov	cx, 32768
		rep	movsw 
		pop	ds

		mov	si, writing_vbios
		call	print_msg

		xor	bx, bx
		mov	ax, 2  ; sector 2 
		mov	cl, 128 ; 128 sectors 
BS_08:
		call	fd_write
		jc	short BS_05
		dec	cl
		jz	short BS_09
		add	bx, 512
		inc	ax
		jmp	short BS_08
BS_09:
		;mov	si, msg_ok
		;call	print_msg

		; 09/11/2020
		; writing video parameters
		xor	bx, bx
		mov	ds, bx
		mov	bx, 04A8h  ; video parameters table
		lds	si, [bx]
		lds	si, [si] 	
		;mov	bx, 1000h
		;mov	es, bx
		sub	di, di
		mov	cx, 2048/2
		rep	movsw
		push	cs
		pop	ds

		mov	ax, 130	; sector 130 
		mov	cl, 4	; 4 sectors 
		xor	bx, bx
BS_13:
		call	fd_write
		jc	short BS_14
		dec	cl
		jz	short BS_14
		inc	bh
		inc	bh
		inc	ax
		jmp	short BS_13	
BS_14:
		mov	si, msg_ok
		call	print_msg

		call	beep

		jmp	short BS_06

beep:
		mov	al, 07h
		mov	ah, 0Eh
		mov	bh, 0
		int	10h
		retn

print_msg:
		lodsb				; Load byte at DS:SI to AL
                and     al, al            
                jz	short BS_11		; If AL = 0 then stop

                mov     ah, 0Eh
                mov     bx, 07h             
		int	10h			; BIOS Service func ( ah ) = 0Eh
						; Write char as TTY
						; AL-char BH-page BL-color
                jmp     short print_msg

BS_10:         
                dec	byte [RetryCount]
		jz	short BS_11 ; cf = 1

		; Reset disk system
		push	ax
		xor	ah, ah
                ;mov	dl, 0
		int     13h
                pop	ax
		jnc	short BS_12
BS_11:
		retn
 
fd_write:
                ; Only for FAT12 Floppy Disks
                
                mov     byte [RetryCount], 7
BS_12:
                push    cx
                push    ax                      ; PHYSICAL ADDRESS CALCULATION
		mov	cl, 18                  ; Sectors per track
		div     cl                      
                mov     cl, ah                  ; Sector (zero based)
                inc     cl                      ; To make it 1 based
                                                ; AL=cyl, AH=head, CL=sector
		sub	dx, dx
		shr	al, 1
		adc	dh, 0
                mov     ch, al            

                mov     ax, 0301h
		int	13h			; BIOS Service func ( ah ) = 3
						; Write disk sectors
						; AL-sec num CH-track CL-sec
						; DH-head DL-drive ES:BX-buffer
						; CF-flag AH-stat AL-sec read
		pop	ax			
                pop     cx
                jc      short BS_10
   		retn
RetryCount:
		db	07h  ; Filler
error_msg:
                db      0Dh, 0Ah
                db      "Disk Writing Error!"
		db	0Dh, 0Ah, 0

writing_vectors:
		db      0Dh, 0Ah
		db      "Writing interrupt vectors ..."
                db      0Dh, 0Ah, 0

writing_vbios:
		db      0Dh, 0Ah
		db      "Writing video bios ..."
                db      0Dh, 0Ah, 0

program_info:
		db	0Dh, 0Ah
		db	"GAGABOOT v2 for video bios hacking by Erdogan Tan [09/11/2020]"
		db	0Dh, 0Ah
		db	0Dh, 0Ah
		db 	"Press ENTER to continue or press ESC to exit."
		db	0Dh, 0Ah, 0

reboot_msg:	db      0Dh, 0Ah
		db	"Press any key to reboot."
                db	0Dh, 0Ah, 0

msg_ok:		db	0Dh, 0Ah
		db	"OK."
		db	0Dh, 0Ah, 0

		times	510 - ($ - $$) db 0

bootsignature:  db      55h, 0AAh