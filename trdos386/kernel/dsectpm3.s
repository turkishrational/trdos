; ****************************************************************************
; dsectpm3.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'dsectpm3.prg')
; ---------------------------------------------------------------------------
; DSECTPM3.PRG ! TEST program !
; 'Display Disk Sectors' by using TRDOS 386 disk and timer interrupts.
;
; 27/05/2016 - 07/07/202016 (dsectpm2)
; 28/08/2020 (dsectpm3)
;
; Derived from 'dsectrm2.s' source code for Retro UNIX 386 v1 'boot'
;
; [ Last Modification: 29/08/2020 ]
;
; ****************************************************************************
; dsectrm2.s (21/02/2015, Retro UNIX 386 v1, standalone program, real mode)
; dsectpm.s (28/02/2015, Retro UNIX 386 v1, standalone prog, protected mode)
; dsectpm2.s (07/07/2016, TRDOS 386 v1 application, protecded mode program)
; 
; Assembler: NASM 2.11

; display disk sector data [Retro Unix 386 v1 - test ]
; by Erdogan Tan [ Real Mode adaption (Standalone program), 21/02/2015 ]

ESCKey	 equ 1Bh    ;27		
ENTERKey equ 0Dh    ;13
SPACEKey equ 20h    ;32
BACKSPC	 equ 08h    ; 8
DELKey	 equ 53E0h
F1Key	 equ 3B00h
F2Key	 equ 3C00h
F3Key	 equ 3D00h
F4Key	 equ 3E00h ; 28/08/2020
HOMEKey  equ 47E0h
ENDKey	 equ 4FE0h
PgUpKey	 equ 49E0h
PgDnKey  equ 51E0h 

; 02/07/2016

[BITS 32]
	
	; clear bss area

	; ecx = 0
	mov	ecx, bss_end - bss_start
	shr	cx, 2 ; dword count
	mov	edi, bss_start
	; eax = 0
	;xor	eax, eax
	rep	stosd
	
        mov     esi, prg_msg
	call	print_msg

	; Filling disk parameters tables
_fd0:
	;xor	dl, dl ; fd0
	;mov	[drv], dl
	mov	ebx, fd0_dpt
	mov	ah, 08h	; return disk parameters
  	int	33h	; TRDOS 386 disk io interrupt
	jc	short _hd0
	mov	byte [drv_status], 80h
	add	[fd0_type], bl
	mov	[fdc], dl
	call	set_disk_parms
	dec	byte [fdc]
	jz	short _hd0
_fd1:
	mov	dl, 1 ; fd1
	mov	[drv], dl
	mov	ebx, fd1_dpt
	mov	ah, 08h	; return disk parameters
  	int	33h	; TRDOS 386 disk io interrupt
	jc	short _hd0
	mov	byte [drv_status+1], 80h
	add	[fd1_type], bl
	call	set_disk_parms
        dec     byte [fdc] ; = 0 
_hd0:
	mov	dl, 80h ; hd0
	mov	[drv], dl
	mov	ebx, hd0_dpt
	mov	ah, 08h	; return disk parameters
  	int	33h	; TRDOS 386 disk io interrupt
	jc	short _hd1
	mov	al, [ebx+16+4] ; device register, bit 6 = LBA bit
	shr	al, 6 ; bit 6 = bit 0
	add	al, 80h
	mov	[drv_status+2], al
	mov	[hdc], dl
	call	set_disk_parms
	dec	byte [hdc]  ; number of fixed disk drives - 1
	jz	sccps
_hd1:
	mov	dl, 81h ; hd1
	mov	[drv], dl
	mov	ebx, hd1_dpt
	mov	ah, 08h	; return disk parameters
  	int	33h	; TRDOS 386 disk io interrupt
	jc	short _hd2
	mov	al, [ebx+16+4] ; device register, bit 6 = LBA bit
	shr	al, 6 ; bit 6 = bit 0
	add	al, 80h
	mov	[drv_status+3], al
	call	set_disk_parms
	dec	byte [hdc]
	jz	short sccps
_hd2:
	mov	dl, 82h ; hd2
	mov	[drv], dl
	mov	ebx, hd2_dpt
	mov	ah, 08h	; return disk parameters
  	int	33h	; TRDOS 386 disk io interrupt
	jc	short _hd3
	mov	al, [ebx+16+4] ; device register, bit 6 = LBA bit
	shr	al, 6 ; bit 6 = bit 0
	add	al, 80h
	mov	[drv_status+4], al
	call	set_disk_parms
	dec	byte [hdc]
	jz	short sccps
_hd3:
	mov	dl, 83h ; hd3
	mov	[drv], dl
	mov	ebx, hd3_dpt
	mov	ah, 08h	; return disk parameters
  	int	33h	; TRDOS 386 disk io interrupt
	jc	short sccps
	mov	al, [ebx+16+4] ; device register, bit 6 = LBA bit
	shr	al, 6 ; bit 6 = bit 0
	add	al, 80h
	mov	[drv_status+5], al
	call	set_disk_parms
	dec	byte [hdc] ; = 0

sccps:
	; get cursor position
	xor	ebx, ebx ; bh = video page 0
	mov	ah, 03h	; get cursor position and shape
	int	31h 	; TRDOS 386 video interrupt
			; (IBM PC/AT ROMBIOS, INT 10h) 	
	mov	[cursor_posn], dx ; position
	mov	[cursor_shp], cx ; shape

	; Save video page (before displaying sector)

	; copy video page 0 to video page 6
	;sub	ebx, ebx ; bl = 0 -> system to system
			 ; bh = 0 -> 80*25 text mode 
	sub	cl, cl   ; source = video page 0
	mov	dl, 6    ; destination = video page 6
	mov	eax, 31  ; 'sysvideo'
	int	40h	 ; TRDOS 386 system call  

display_sectors:
	call	hide_cursor
	; Save cursor position
	mov	ax, [cursor_posn] ; cursor pos. 
				  ; for video page 0
	mov	[cursor_posb], ax
	call	clear_frame

	; start (Real Time Clock) timer function
        mov     bl, 0FFh ; signal return (response) byte
        mov     bh, 3    ; 1 second (rtc interrupt) 
	;mov	ecx, 1
	mov	cx, 1
	mov	edx, timer_event ; signal return (response) address
	mov	eax, 33	; 'systimer'
	int	40h	; TRDOS 386 system call
        jc      short dscl_0

	mov	[timer_event_number], al 

	jmp	short dscl_0

dscl_esc:
	call	restore_video_page
dscl_getc:
	call	getch
	;
	cmp	al, ESCKey
        je      dscl_exit
	mov	byte [dscmd], 0 ; reset
	; 29/08/2020
	cmp	ax, F1Key
	jb	dscl_6	
	je	short dscl_0 ; [dscmd] = 0
	; 28/08/2020
	cmp	ax, F4Key
	;ja	dscl_6 ; not one of F1 to F4 functions
	ja	dscl_29 ; 29/08/2020
	jb	short dscl_f3 ; F3key or F2Key
dscl_f4:
	mov	byte [dscmd], 3 ; Display disk size (and CHS)
	jmp	short dscl_0
dscl_f3:
	; 29/08/2020
	;cmp	ax, F1Key
        ;je	short dscl_0 ; [dscmd] = 0
	;
	inc	byte [dscmd] ; 1
	; 28/08/2020
	;cmp	ax, F3Key
	;jb	short dscl_5 ; F2Key
	; 29/08/2020
	cmp	ax, F2Key
	je	short dscl_5
	; 28/08/2020
	inc	byte [dscmd] ; 2
	; Display disk parameters (HDPT table)
dscl_0:
	call	save_video_page
	mov	esi, F1_ib ; F1 (Change drive)
			   ; Inputbox address
dscl_ib:
	call	inputbox
		; cursor position in DX
	call	show_cursor
		; cursor blinks at current position
	mov	ecx, [prev_sec]
dscl_3:
	call	getch
	cmp	al, ESCKey
	jne	short dscl_27
        call    hide_cursor
        jmp     dscl_esc
dscl_5:
	; 28/08/2020
	;cmp	ax, F2Key
	;jne	dscl_6
	call	save_video_page
	mov	esi, F2_ib ; F2 (Change sector)
		           ; Inputbox address
	;mov	byte [dscmd], 1
        jmp     short dscl_ib
dscl_27:
	cmp	al, SPACEKey
	je	short dscl_4	
     	cmp	al, ENTERKey
	je	short dscl_4
	;
	xor	ebx, ebx
    	cmp     byte [dscmd], 1
	je	dscl_12
	;
	cmp	al, '0'
	jb	short dscl_3
	cmp	al, '5'
	ja	short dscl_3
	mov	edi, [current_txtpos]
	stosb
	;
	;xor	bh, bh  ; video page 0
	mov	cx, 1   ; character count
	mov	ah, 0Ah ; write chr at current cursor pos.
	int	31h     ; TRDOS 386 video interrupt
	;
	sub	al, '0'
	mov	dl, al
	xor	dh, dh
	mov	bl, al
	shl	bl, 2  ; *4
	add	ebx, ds_sec ; current_sector
	mov	ecx, [ebx]
        mov     esi, sector_buffer
	jmp	short dscl_3 
dscl_4:
	cmp	byte [inds],  0 ; display other half or not ?
        ja      dscl_oh         ; other half
	push	dx
	; save regs (ESI, ECX, DX)
	call	hide_cursor
	; restore regs (ESI, ECX, DX)
	pop	dx
	mov	eax, ecx
	;
	cmp     byte [dscmd], 1 ; Requested function ?
        je      dscl_17         ; Change sector (F2)
        jb      dscl_ns         ; Change drive (F1)

	; Display disk parameters (dscmd = 2)
	cmp	dl, 2
	jb	short dscl_28
	add	dl, 7Eh
dscl_28:
	; 28/08/2020
	cmp	byte [dscmd], 3
	jne	short dscl_dskprm

	call	dskvprm ; disk size and virtual chs parms
	jmp	dscl_esc

dscl_dskprm:
	call	dskprm
        jmp     dscl_esc
dscl_12:
	cmp	ax, DELKey	; DEL key
	je	short dscl_bs
	cmp	al, BACKSPC	; Backspace key
	jne	short dscl_13
dscl_bs:
	cmp	byte [txtposoff], 0
        jna     dscl_3
	dec	byte [txtposoff]
	dec	byte [cursor_posn]
	call	set_cpos
	movzx	ebx, byte [txtposoff]
	dec	byte [txtposoff]
	dec	byte [cursor_posn]
	mov	al, 20h
        jmp     short dscl_14
dscl_13:
	mov	bl, [txtposoff]
	cmp	bl, 8
        jnb     dscl_3
	;
	cmp	al, '0'
        jb      dscl_3
	cmp	al, '9'
	ja	short dscl_15
dscl_14:
	shl	bl, 1
	mov	esi, [current_txtpos]
	add	ebx, esi
	mov	[ebx], al
	;
	xor	bh, bh  ; video page 0
	mov	cx, 1	; character count
	mov	ah, 0Ah ; write chr at current cursor pos.
	int	31h     ; TRDOS 386 video interrupt
	;
	cmp	byte [txtposoff], 8
	jge	dscl_3 ; JGE !
	inc	byte [txtposoff]
	inc	byte [cursor_posn]
	call	set_cpos
        jmp     dscl_3 
dscl_15:
	cmp	al, 'A'
        jb      dscl_3
	cmp	al, 'F'
        jna     short dscl_14
dscl_16:
	cmp	al, 'a'
        jb      dscl_3
	cmp	al, 'f'
        ja      dscl_3
	sub	al, 'a' - 'A'
	jmp	short dscl_14
	;
dscl_17:
	mov	esi, [current_txtpos]
	xor	eax, eax
	mov	byte [txtposoff], al ; 0
	push	eax  ; sector value (reset)	
dscl_18:
	lodsw
	cmp	al, '0'
	jb	short dscl_22
dscl_19: 
	sub	ecx, ecx
	mov	ebx, hexchrs
dscl_20:
	cmp	al, [ebx]
	je	short dscl_21
	;cmp	cl, 15
	;jnb	short dscl_22
	inc	cl
	inc	ebx
	jmp	short dscl_20
dscl_21: 
	pop	eax
	shl	eax, 4	; * 16
	add	eax, ecx
	push	eax
	jmp	short dscl_18
dscl_22:
	mov	dl, [ds_drv]
	xor	dh, dh
	pop	eax
        mov     esi, sector_buffer
        jmp     short dscl_ns
dscl_oh:
	mov	dl, [ds_drv]
	movzx	ebx, dl
	shl	bl, 2
	add	ebx, ds_sec
	mov	eax, [ebx]
        mov     esi, sector_buffer
	;
	mov	dh, [ds_drv+1]
	or	dh, dh
	jz	short dscl_nh ; second half of sector (0->1)
	xor	dh, dh	      ; reset (0)	
	jmp	short dscl_nx
dscl_nh:
	add	esi, 256
	inc	dh
dscl_nx:
	mov	[ds_drv+1], dh
        jmp     dscl_25
dscl_ns:
	mov	[ds_drv+1], dh
	movzx	ebx, dl
	shl	bl, 2
	add	ebx, ds_sec
	cmp	dl, [ds_drv]
	jne	short dscl_23
	cmp	eax, [ebx]
	je	dscl_25
dscl_23:
	mov	cl, [ds_drv]
	mov	[prev_drv], cl
	mov	[ds_drv], dl
dscl_26:
	mov	ecx, [ebx]
	mov	[prev_sec], ecx
	mov	[ebx], eax
	call	read_disk_sector
	jnc	short dscl_24
dscl_rd_err:
	;
	;mov	al, ah	; error code
	;mov	edi, err_code_str
	;call	write_hex
	;
	mov	esi, dskr_err ; drive not ready or read error
	call	inputbox
	call	getch
	call	restore_video_page
	movzx	ebx, byte [prev_drv]
	mov	[ds_drv], bl
	shl	bl, 2
	add	ebx, ds_sec
	mov	eax, [prev_sec]
	mov	[ebx], eax
        jmp     dscl_getc
dscl_24:
	mov	dx, [ds_drv]
	movzx	ebx, dl
	shl	bl, 2
	add	ebx, ds_sec
	mov	eax, [ebx]
        mov     esi, sector_buffer
dscl_25:
	call	display_sector
	call	save_video_page
        jmp     dscl_getc
dscl_11:
        mov     esi, sector_buffer
	mov	dl, [ds_drv]
	sub	dh, dh	 ; 0 = first half of sector
        jmp     dscl_ns
dscl_6:	
	cmp	al, SPACEKey
        je      dscl_oh
     	cmp	al, ENTERKey
        je      dscl_oh
dscl_29:
	cmp	ax, HOMEKey
	jne	short dscl_7
	xor	eax, eax
	jmp	short dscl_11
dscl_7:
	cmp	ax, ENDKey
	jne	short dscl_8
	movzx	ebx, byte [ds_drv]
	shl	bl, 2
        add     ebx, drv_size
	mov	eax, [ebx]
	dec	eax
	jmp	short dscl_11
dscl_8:
	cmp	ax, PgDnKey
	jne	short dscl_10
	call	dscl_9
	inc	eax
	cmp	eax, ecx ; last sector
	jna	dscl_ns
	xor	eax, eax
	jmp	dscl_26 
dscl_9:	
	movzx	edx, byte [ds_drv]
	mov	ebx, edx
	shl	bl, 2  ; *4
        add     ebx, drv_size
	mov	ecx, [ebx]
	dec	ecx
        sub     ebx, drv_size
	add	ebx, ds_sec ; current sector
	mov	eax, [ebx]	
        mov     esi, sector_buffer
        retn
dscl_10:
	cmp	ax, PgUpKey
        jne     dscl_getc
	call	dscl_9
	dec	eax
	cmp	eax, ecx ; last sector
	jna	dscl_ns
	mov	eax, ecx
	jmp	dscl_26

dscl_exit:
	;
	; Stop timer event
	movzx	ebx, byte [timer_event_number]
		; bh = 0 -> stop timer event

	and	bl, bl
	jz	short dscl_rvp
	mov	eax, 33	; 'systimer'
	int	40h	; TRDOS 386 system call

	sub	bl, bl
dscl_rvp:
	; Restore video page (before displaying sector)

	; copy video page 6 to video page 0
	;sub	ebx, ebx ; bl = 0 -> system to system
			 ; bh = 0 -> 80*25 text mode 
	mov	cl, 6    ; source = video page 6

	call	restore_v_pg_x

	; Restore cursor position
	mov	dx, [cursor_posb] 
	;
	; Set cursor position
	;xor	bh, bh  ; Video page 0
	mov	ah, 2	; set cursor position
	int	31h 	; TRDOS 386 video interrupt
	
	; Show standard blinking text cursor 
        mov 	cx, [cursor_shp]
	mov	ah, 1	; set cursor type
	int	31h 	; TRDOS 386 video interrupt

terminate:
	mov	eax, 1	; 'sysexit'
	int	40h	; TRDOS 386 system call
haltsys:
	hlt
	jmp	short haltsys

getch:
dscl_rtc_p:
	cmp	byte [timer_event], 0
	jna	short dscl_getch

	; timer function
	mov	byte [timer_event], 0
		
	mov	edi, video_buffer + 0A0h + 50h ; Row 1, Column 40
	cmp     byte [edi+1], 3Fh ; cyan (3) Background
			; white (F) forecolor 
			; (display disk sector frame)
	jne	short dscl_getchar

	push	ecx
	push	edx

	call	rtc_p

	; print real time clock content (as formatted)
	; to video page line 1, column 40
	mov	esi, video_buffer + 0A0h + 50h
	mov	ecx, 10028h ; row 1, column 40 (top left)
        mov     edx, 10028h + rtc_msg_end - rtc_msg ; (bottom right)
			    ; row 1, column 40 + rtc_msg lenth 
	sub	edi, edi ; no swap
	mov	ebx, 5  ; user to system window transfer (active page)
	mov	eax, 31 ; 'sysvideo'
	int	40h	; TRDOS 386 system call	

	pop	edx
	pop	ecx
	
dscl_getch:
	; Check keyboard buffer
	mov	ah, 11h
	int 	32h ; TRDOS 386 keyboard interrupt
		    ; (IBM PC/AT ROMBIOS, INT 16h)			
	jz	short dscl_rtc_p ; keyboard buffer empty

dscl_getchar:
	; Getchar by using keyboard interrupt
	mov	ah, 10h
	int 	32h ; TRDOS 386 keyboard interrupt
		    ; (IBM PC/AT ROMBIOS, INT 16h)			
	retn
		
save_video_page:
	; Save video page

	; copy video page 0 to video page 7
	sub	ebx, ebx ; bl = 0 -> system to system
			 ; bh = 0 -> 80*25 text mode 
	sub	cl, cl   ; source = video page 0
	mov	dl, 7    ; destination = video page 7
	mov	eax, 31  ; 'sysvideo'
	int	40h	 ; TRDOS 386 system call

	retn  

restore_video_page:
	; copy video page 7 to video page 0
	sub	ebx, ebx ; bl = 0 -> system to system
			 ; bh = 0 -> 80*25 text mode 
	mov	cl, 7    ; source = video page 7

restore_v_pg_x:
	sub	dl, dl   ; destination = video page 0
	mov	eax, 31  ; 'sysvideo'
	int	40h	 ; TRDOS 386 system call  

	mov	bl, 2	 ; system to user
	;xor	dl, dl   ; video page 0
	mov	ecx, video_buffer ; user buffer
	mov	eax, 31	 ; 'sysvideo'
 	int	40h	 ; TRDOS 386 system call  
	
	retn
	
display_sector:
	; display disk sector data (on video page 0)
	;
	; INPUT ->
	;	ESI = sector buffer offset
	; 	      (sector size: 512 bytes)
	;	EAX = sector number
	;	DL = drive number (0,1,2,3,4,5,6)
	;	DH = portion control byte 
	;		 (0= first half of the sector, 
	;		 >0= second half of the sector) 
	; OUTPUT ->
	;	Video page 0 (0B8000h) will be filled
	;	with sector data
	;	(ESI points to byte 256 of the buffer
	;	or end of the buffer)	
	;
	; Modified registers: eax, edx, ecx, ebx, esi, edi
	;
	;
	;xor	ecx, ecx ; reset for cx loop counts
	mov	byte [inds], 1 ; for ENTER key handling 
	;
	push	eax
	push	edx
	call	clear_frame
	pop	edx
	pop	eax
dsfh:
	xor	ebx, ebx
	or	dh, dh
	jz	short dsfh1
	mov	bl, 10h
dsfh1:
	mov	[paragr], bl	; Paragraph (16 bytes)
	;
	mov	bl, dl
	shl	bl, 2	; *4
	add	ebx, drv_names
	mov	edx, [ebx]
	mov	[drv_name], edx
	call	dwordtohex
	mov	[sector_num], edx
	mov	[sector_num+4], eax
	mov	al, 1
	mov	ah, 3Fh ; cyan background, white forecolor
	mov	ebx, dpheader
	call	print_line
	mov	al, 21
	;mov	ah, 3Fh ; cyan background, white forecolor
	mov	ebx, dpfooter1
	call	print_line
	mov	al, 22
	;mov	ah, 3Fh ; cyan background, white forecolor
	mov	ebx, dpfooter2
	call	print_line
ds1:
	mov	ecx, 16
ds2:
	mov	al, [paragr]
	call	bytetohex
	mov	[sdline_1], ax
	;
	push	ecx
	mov	cl, 16
	mov	edi, sdline_2
ds3:
	lodsb	
	call	bytetohex
	stosw
	inc	edi
	loop	ds3
	sub	esi, 16
	inc	edi
	mov	cl, 16
	rep	movsb
	pop	ecx
	mov	al, 19	; line (row) 3 to 24
	sub	al, cl
	mov	ah, 07h ; Black background, light gray forecolor
	mov	ebx, sdline
	call	print_line_80 ; 04/12/2014
	loop	ds4
	
	;call	video_page_update
	;retn

	jmp	video_page_update
ds4:
	inc	byte [paragr]
	jmp	short ds2

; Convert binary number to hexadecimal string

bytetohex:
	; INPUT ->
	; 	AL = byte (binary number)
	; OUTPUT ->
	;	AX = hexadecimal string
	;
	push	ebx
	movzx	ebx, al
	shr	bl, 4
	mov	bl, [ebx+hexchrs] 	 	
	xchg	bl, al
	and	bl, 0Fh
	mov	ah, [ebx+hexchrs] 
	pop	ebx	
	retn

wordtohex:
	; INPUT ->
	; 	AX = word (binary number)
	; OUTPUT ->
	;	EAX = hexadecimal string
	;
	push	ebx
	xchg	ah, al
	push	ax
	movzx	ebx, ah
	shr	bl, 4
	mov	al, [ebx+hexchrs] 	 	
	mov	bl, ah
	and	bl, 0Fh
	mov	ah, [ebx+hexchrs]
	shl	eax, 16
	pop	ax
	pop	ebx
	jmp	short bytetohex
	;mov	bl, al
	;shr	bl, 4
	;mov	bl, [ebx+hexchrs] 	 	
	;xchg	bl, al	 	
	;and	bl, 0Fh
	;mov	ah, [ebx+hexchrs] 
	;pop	ebx	
	;retn

dwordtohex:
	; INPUT ->
	; 	EAX = dword (binary number)
	; OUTPUT ->
	;	EDX:EAX = hexadecimal string
	;
	push	eax
	shr	eax, 16
	call	wordtohex
	mov	edx, eax
	pop	eax
	call	wordtohex
	retn

print_line_80:
	; 04/12/2014
	; al = line (0 to 24)
	; ah = color attributes
	; ebx = 80 chars string address	
	call 	get_lpos
	push	ecx
	mov	ecx, 80
pl80:
	mov	al, [ebx]
	inc	ebx
	stosw
	loop	pl80
	pop	ecx
	retn

print_line:
	; al = line (0 to 24)
	; ah = color attributes	
	; ebx = ASCIIZ string address
	call	get_lpos
	push	esi
	mov	esi, ebx
prl1:
	lodsb
	and	al, al
	jz	short prl2
	stosw
	jmp	short prl1
prl2:
	pop	esi
	retn

clear_frame:
	xor	al, al ; Line 0
	call	clear_line
	mov	al, 1
	mov	ah, 3Fh ; cyan background, white forecolor
	call	fill_color
	mov	al, 1
dscf0:	
	inc	al
	push	ax
	call	clear_line
	pop	ax
	cmp	al, 19
	jb	short dscf0
	;inc	al ; line 20
	mov	ah, 3Fh
dscf1:
	inc	al
	push	ax
	call	fill_color	  
	pop	ax
	cmp	al, 23
	jb	short dscf1
	inc	al
	call	clear_line

	;call	video_page_update
	;retn

video_page_update:
	; copy video buffer content to video page 0
	mov	ebx, 1	; BL = 1 = user to system
	mov	dl, 0	; video page 0
	mov	ecx, video_buffer
	mov	eax, 31 ; 'sysvideo'
	int	40h	; TRDOS 386 system call	
	retn

clear_line:
	xor	ah, ah ; blank
fill_color:
	; al = line (0 to 24)
	; ah = color attributes
	call	get_lpos
	mov	ecx, 80
	mov	al, 20h ; space/blank
	rep	stosw
	retn

get_lpos:  ; Get line position in video buffer
	push	ax
	mov	ah, 80*2
	mul	ah
	movzx	edi, ax
	add	edi, video_buffer
	pop	ax
	retn

rtc_p:	
	; Print Real Time Clock content
	;
	mov	ah, 4	; read the date
	int	35h	; TRDOS 386 date&time interrupt
			; (IBM PC/AT ROMBIOS, INT 1Ah)
	;mov	[date_day], dl
	;mov	[date_month], dh
	mov	[date_day], dx
	;mov	[date_year], cl
	;mov	[date_century], ch
	mov	[date_year], cx
	;
	mov	ah, 2	; read the time
	int	35h	; TRDOS 386 date&time interrupt
        mov     [time_second], dh
        ;mov    [time_minute], cl
        ;mov    [time_hour], ch
        mov     [time_minute], cx
	;
	mov	al, [date_century]
	call	bcd_to_ascii
	mov	word [datestr+6], ax
	mov	al, byte [date_year]
	call	bcd_to_ascii
	mov	word [datestr+8], ax
	mov	al, byte [date_month]
	call	bcd_to_ascii
	mov	word [datestr+3], ax
	mov	al, byte [date_day]
	call	bcd_to_ascii
	mov	word [datestr], ax
	;
        mov     al, byte [time_hour]
	call	bcd_to_ascii
	mov	word [timestr], ax
        mov     al, byte [time_minute]
	call	bcd_to_ascii
	mov	word [timestr+3], ax
        mov     al, byte [time_second]
	call	bcd_to_ascii
	mov	word [timestr+6], ax
	;		
	mov	esi, rtc_msg ; message offset
	;
	;mov	edi, video_buffer + 0A0h + 050h ; Row 1, Column 40
	;mov	ah, [edi+1]
	;cmp	ah, 3Fh ; cyan (3) Background
			; white (F) forecolor 
			; (display disk sector frame)
	;jne	short prtcmsg_ok	
prtcmsg:
	lodsb
	or	al, al
	jz	short prtcmsg_ok
	stosb
	inc 	edi
	jmp	short prtcmsg
prtcmsg_ok:
	retn

bcd_to_ascii:
	; INPUT ->
	;	AL = Packed BCD number
	; OUTPUT ->
	;	AX = ASCII word/number
	;
	db	0D4h, 10h	; Undocumented inst. AAM
				; AH = AL / 10h
				; AL = AL MOD 10h
	or	ax, '00'	; Make it ASCII based

        xchg	ah, al 
	
	retn	

inputbox:
	; Show an input box for user/keyboard input
	; INPUT ->
	;	ESI = input structure address 
	; OUTPUT ->
	;	DX  = cursor position for input
	;	input box will be displayed (on tty0)
	;
	; Modified registers: eax, ebx, ecx, edx, esi, edi

	mov	byte [inds], 0 ; for ENTER key handling
	xor	ecx, ecx
	mov	ebx, video_buffer
	mov	eax, 5018h ; 80, 24
	mov	dx, [esi] ; box width (dl)
			  ; box height (dh)
	sub	al, dh
	shr	al, 1
	mov	[ibcp+1], al ; row
	mul	ah
	shl	ax, 1  ; char + attribute
	add	ebx, eax
	mov	al, 80
	sub	al, dl
	shr	al, 1 
	mov	[ibcp], al ; column
	shl	al, 1  ; char + attribute
	sub	ah, ah
	add	ebx, eax
	mov	ah, [esi+5] ; color attributes
	mov	al, 20h	; space/blank
	mov	cl, dh ; height
ib0:
	push	ecx
	mov	cl, dl
	mov	edi, ebx	
	rep	stosw
	pop	ecx
	add	ebx, 80*2 ; number of columns * 2
	loop	ib0
	;
	mov	edi, video_buffer
	mov	al, [ibcp+1] ; row position
	add	al, [esi+2] ; label offset (row)
	mov	[ibcp+1], al
	mov	ah, 80*2
	mul	ah
	add	edi, eax
	mov	al, [ibcp] ; column position
	add	al, [esi+3] ; label offset (column)
	mov	[ibcp], al
	xor	ah, ah
	shl	al, 1
	add	edi, eax
	mov	ebx, esi
	add	esi, 6 ; Label offset
ib2:
	lodsb
	or	al, al
	jz	short ib3
	stosb
	inc 	edi	
	inc	cl
	jmp	short ib2
ib3:
	add	[ibcp], cl ; column position
	mov	[current_txtpos], edi
	;
	mov	cl, [ebx+4] ; input char count
	or	cl, cl
	jz	short ib5 ; message box (no input)	
	mov	al, 20h
	mov	ah, 07h ; black background
			; light gray fore color	
ib4:
	rep	stosw
ib5:	
	call	video_page_update
	mov	dx, [ibcp] ; cursor position
	retn

hide_cursor:
      	;CH = cursor start line (bits 0-4) 
	;     and options (bits 5-7).
	;CL = bottom cursor line (bits 0-4).
	; when bit 5 of CH is set to 0, the cursor is visible. 
	; when bit 5 is 1, the cursor is not visible. 
	; hide blinking text cursor: 
        push	ecx
	mov 	ch, 32
	xor	bh, bh ; video page 0
	jmp	short hc_sc

show_cursor:
  	; dh = row
	; dl = column
	push	ecx
	mov	[cursor_posn], dx
	call	set_cposx
	;
	;show box-shaped blinking text cursor
	mov	ch, 13
hc_sc:
        mov 	cl, 15
        mov 	ah, 1
        int 	31h
	pop	ecx
	retn

set_disk_parms:
	; 26/08/2020
	mov	dl, [drv]
	cmp	dl, 80h
	jb	short set_disk_parms_fd ; floppy
	and	al, 1  ; LBA ready ?
	jz	short set_disk_parms_chs
set_disk_parms_lba:
	; 28/08/2020
	;; Translated FDPT
	;mov	ax, [ebx+9]  ; physical cylinders
	;mov	dh, [ebx+11] ; physical heads
	;mov	cl, [ebx+4]  ; physical sectors per track 
	;jmp	short sdp0

	; 28/08/2020 
	; TRDOS 386 kernel, INT 33h, Function 15h modification
	; (Read DASD type) get disk size function return:
	; ah = 03h (eax = 300h)
	; cx:dx = disk size (LBA disk dize)
	
	mov	ah, 15h
	;;sub	al, al
	;mov	dl, [drv]
	int	33h
	;jnc	short set_disk_parms_lba_exact

	;; Translated FDPT
	;mov	ax, [ebx+9]  ; physical cylinders
	;mov	dh, [ebx+11] ; physical heads
	;mov	cl, [ebx+4]  ; physical sectors per track 
	;mov	dl, [drv]
	;jmp	short sdp0

;set_disk_parms_lba_exact
	; 28/08/2020
	mov	ax, cx ; hw of disk size
	shl	eax, 16
	mov	ax, dx ; lw of disk size
	movzx	ebx, byte [drv] ; physical disk drive number
	sub	bl, 07Eh ; hd0 = 2
	shl	bl, 2 ; * 4
	mov	[ebx+drv_size], eax ; 32 bit LBA disk size
	shr	bl, 1 ; / 2
	mov	word [ebx+drv_heads], 255 ; virtual heads
	mov	word [ebx+drv_spt], 63 ; virtual sectors per track
	mov	ecx, 16065 ; 255*63	
	xor	edx, edx
	div	ecx
	; eax = ax = cylinders
	mov	[ebx+drv_cylinders], ax
	retn

set_disk_parms_fd:
	;mov	al, ch ; last cylinder (bits 0-7)
	;mov	ah, cl ; 
	;shr	ah, 6  ; last cylinder (bits 8-9)
	inc	ax  ; convert max. cyl number to cyl count
	mov	al, ch
	inc	al
	sub	ah, ah
	inc	dh  ; convert last head to heads
	;and	cl, 63 ; sectors per track
	jmp	short sdp0  

set_disk_parms_chs:
	; Standard FDPT
	mov	ax, [ebx]  ; physical cylinders
	mov	dh, [ebx+2] ; physical heads
	mov	cl, [ebx+14] ; physical sectors per track 
sdp0:
	movzx   ebx, dl
	cmp	bl, 80h
	jb	short sdp1
	sub	bl, 7Eh
sdp1:	
	shl	bl, 1
	add	ebx, drv_cylinders
	mov	[ebx], ax
	push	ax ; ** cylinders
	sub	ebx, drv_cylinders
	add	ebx, drv_heads
	mov	al, dh ; heads
	xor	ah, ah
	mov	[ebx], ax
	sub     ebx, drv_heads
	add     ebx, drv_spt
	and	cx, 3Fh  ; sectors (bits 0-6)
	mov	[ebx], cx ; sectors per track
	sub     ebx, drv_spt
	shl	bx, 1
	add	ebx, drv_size ; disk size (in sectors)
	; LBA size = cylinders * heads * secpertrack
	mul	cx 
	mov	dx, ax ; heads*spt					
	pop	ax ; ** cylinders
	cmp	byte [drv], 80h
	jb	short sdp2
	dec	ax ; 1 cylinder reserved (!?)
sdp2:
	mul	dx ; cylinders * (heads*spt)		
	mov	[ebx], ax
	mov	[ebx+2], dx
	;
	retn	

;set_disk_parms
;	movzx   ebx, byte [drv]
;	cmp	bl, 80h
;	jb	short sdp0
;	sub	bl, 7Eh
;sdp0:	
;	;add	ebx, drv_status
;	;mov     byte [ebx], 80h ; 'Present' flag
;	;
;	mov	al, ch ; last cylinder (bits 0-7)
;	mov	ah, cl ; 
;	shr	ah, 6  ; last cylinder (bits 8-9)
;	;sub	ebx, drv_status
;	shl	bl, 1
;	add	ebx, drv_cylinders
;	inc	ax  ; convert max. cyl number to cyl count		
;	mov	[ebx], ax
;	push	ax ; ** cylinders
;	sub	ebx, drv_cylinders
;	add	ebx, drv_heads
;	mov	al, dh ; last head number
;	xor	ah, ah
;	inc	ax     ; heads 	
;	mov	[ebx], ax
;	sub     ebx, drv_heads
;	add     ebx, drv_spt
;	and	cx, 3Fh  ; sectors (bits 0-6)
;	mov	[ebx], cx
;	sub     ebx, drv_spt
;	shl	bx, 1
;	add	ebx, drv_size ; disk size (in sectors)
;	; LBA size = cylinders * heads * secpertrack
;	mul	cx 
;	mov	dx, ax ; heads*spt					
;	pop	ax ; ** cylinders
;	cmp	byte [drv], 80h
;	jb	short sdp1
;	dec	ax ; 1 cylinder reserved (!?)
;sdp1:
;	mul	dx ; cylinders * (heads*spt)		
;	mov	[ebx], ax
;	mov	[ebx+2], dx
;	;
;	retn

read_disk_sector:
	; EAX = sector number (LBA)
	;
	movzx	ebx, byte [ds_drv]
	mov	dl, bl	
	cmp	dl, 2
	jb	short rd0
	add	dl, 7Eh  ; 80h, 81h, 82h, 83h
rd0:
	mov	esi, ebx
	mov	[drv], dl
	add	ebx, drv_status
	mov	dh, [ebx]
rd1:
	cmp	dh, 0F0h
	cmc
        jc      short rd_lba_fails
	;
	mov	ebx, esi
	shl	bl, 2
	add	ebx, ds_sec
	mov	eax, [ebx]
	sub	ebx, ds_sec
        add     ebx, drv_size 
	cmp	eax, [ebx] ; Last sector + 1 (number of secs.)
	cmc
        jc      short rd_lba_fails
	;
	test	dh, 1 ; LBA ready ?
        jz      short rd_chs
rd_lba:
	; LBA read (private function)
	;((Retro UNIX 386 v1 - DISK I/O Test))
	add	esi, drv_status
	and	byte [esi], 8Fh ; clear error bits
	;
	mov	ecx, eax ; Logical Block/Sector Address
	shr	ebx, 16
        mov     ebx, sector_buffer
	mov	dl, [drv]
	mov	byte [retry_count], 4
rd_lba_retry:
	mov	ah, 1Bh ; LBA read (private function)		
	mov	al, 1
	int	33h	; TRDOS 386 disk io interrupt
	jnc	short rd_lba_ok

	cmp	ah, 80h ; time out ?
	je	short rd_lba_rfails
	dec	byte [retry_count]
	jz	short rd_lba_rfails
	
	mov	ah, 0Dh ; Alternate reset
	int	33h	; TRDOS 386 disk io interrupt
        jnc     short rd_lba_retry
	or	byte [esi], 0F0h ; drive not ready !
rd_lba_rfails:
	stc
rd_lba_fails:
rd_lba_ok:
	retn
	;
	; CHS read (convert LBA address to CHS values)	;	
rd_chs:
	shl	esi, 1
	mov	ebx, esi
	xor	edx, edx ; 0
	sub	ecx, ecx 
        add     ebx, drv_spt
	mov	cx, [ebx] ; sector per track
                ; EAX = sector address (LBA)
	div	ecx
	mov	cl, dl	; sector number - 1
	inc	cl	; sector number (1 based)
	push	cx
	mov	ebx, esi
        add     ebx, drv_heads
	mov	cx, [ebx] ; heads
	xor	edx, edx
		; EAX = cylinders * heads + head
	div	ecx
	pop	cx     ; sector number
	mov	dh, dl ; head number
	mov	dl, [drv]
	mov	ch, al ; cylinder (bits 0-7)
	shl	ah, 6
	or	cl, ah ; cylinder (bits 8-9)
		       ; sector (bits 0-7)
        mov     ebx, sector_buffer
		; CL = sector (bits 0-6)
		;      cylinder (bits 7-8 -> bits 8-9)
		; CH = cylinder (bits 0-7)
		; DH = head
		; DL = drive

	shr	esi, 1 ; drive index (byte alignment)
	add	esi, drv_status
	and	byte [esi], 8Fh ; clear error bits
	;
	mov	byte [retry_count], 4
rd_retry:	
	mov	ah, 02h ; read sectors
	mov	al, 1 ; sector count	
	int	33h	; TRDOS 386 disk io interrupt
	jnc	short rd_ok
	cmp	ah, 80h ; time out ?
	je	short rd_rfails
	dec	byte [retry_count]
	jnz	short rd_reset
rd_rfails:
	stc
rd_fails:
	retn
rd_reset:
	sub	ah, ah
	cmp	dl, 80h
	jb	short rd_fd_reset
	mov	ah, 0Dh ; Alternate reset
rd_fd_reset:
	int	33h	; TRDOS 386 disk io interrupt
        jnc     short rd_retry
	or	byte [esi], 0F0h ; drive not ready !
	stc
rd_ok:
	retn

clear_screen:
	mov	edi, video_buffer
	mov	ecx, 80*25
        mov     ax, 0720h ; light gray char space (blank)
	rep	stosw

	call	video_page_update

        xor     dx, dx    ; column 0, row 0
	jmp	set_cposx ; set cursor position

rfdp_err:
	call	clear_screen
	mov	esi, drv_not_ready
	jmp	print_msg	

dskprm:
	; DISPLAY DISK PARAMETERS TABLE
	;
	; INPUT -> DL = Disk/Drive #
	; 
	mov	byte [drv], dl  ; 0,1,80h,81h,82h,83h 
	;
	test	dl, 80h
	jz	short dskprm0
	sub	dl, 7Eh ; hd0 = 2
dskprm0:
	movzx	ebx, dl
	add	ebx, drv_status

	cmp	byte [ebx], 80h  ; existing ?
	jb	short rfdp_err
	;
	call	clear_screen	 ; clear video page 0	
	;
        mov	bl, [drv]
	test	bl, 80h
        jnz     print_hdpt

	movzx	esi, bl
	add	bl, 30h	; '0'
	mov	byte [flpdnum], bl
	add	esi, fd0_type
	mov	al, [esi]
        mov     byte [flpdtype], al 
                                   ; floppy disk drive type
				   ; (1=360K, 2=1.2M, 3=720K, 4=1.44M)
print_flpdpt:
	; Writing the Diskette Parameter Table on screen
	shl	bl, 4 ; * 16
	movzx	esi, bl
	add	esi, fd0_dpt
	lodsb 	; bits 0-3: SRT step rate time
		; bits 4-7: head unload time
	mov	edi, rSrtHdUnld
	call	write_hex
	lodsb 	; bit 0: 1=use DMA
		; bits 2-7: head load time
	mov	edi, rDmaHdLd
	call	write_hex
	lodsb 	; 55-ms increments 
		; before turning disk motor off
	mov	edi, bMotorOff
	call	write_hex
	lodsb 	; sector size
		; (0=128, 1=256, 2=512, 3=1024)
	mov	edi, bSectSize
	call	write_hex
	lodsb 	; EOT (last sector on a track)
	mov	edi, bLastTrack
	call	write_hex
	lodsb 	; gap length 
		; for read/write operations
	mov	edi, bGapLen
	call	write_hex
	lodsb 	; DTL (Data Transfer Length)
		; max transfer when length not set	
	mov	edi, bDTL
	call	write_hex
	lodsb 	; gap length for format operation
	mov	edi, bGapFmt
	call	write_hex
	lodsb 	; fill character for format 
		; (normally F6H)
	mov	edi, bFillChar
	call	write_hex
	lodsb 	; head-settle time
		; (in milliseconds)
	mov	edi, bHdSettle
	call	write_hex
	lodsb 	; motor-startup time
		; (in 1/8th-second intervals)
	mov	edi, bMotorOn
	call	write_hex
	;
	; (extension, not in original bios function)
	lodsb	; Max. track number
	mov	edi, bMaxTrack
	call	write_hex
	lodsb	; Data transfer rate
	mov	edi, bDataRate
	call	write_hex
	;
	mov	al, [drv]
	add	al, 30h ; '0'
        mov     byte [flpdnum], al
        mov     esi, FLPDPT
	call	print_msg
	retn

write_dhex:
	mov	bl, ah
        shr     bl, 4
        call    dhgd
	mov	bl, ah
	call	dhgd

write_hex:
	mov	bl, al
        shr     bl, 4
	call	dhgd
	mov	bl, al
	;call	dhgd
	;retn
dhgd:
	push	eax
	and	ebx, 0Fh
        add     ebx, hex_digits
        mov     al, [ebx]
	stosb
	pop	eax
	retn

print_hdpt:
	;mov	bl, [drv]
	and	bl, 3
	mov	al, bl
	add	al, 2
	mov	[drv], al
	;
	shl	bl, 5 ; * 32
	movzx	esi, bl
	add	esi, hd0_dpt  
	;
	cmp	byte [esi+3], 0A0h ; Translated table
        je      print_thdpt       ; indicator
	;
	; Writing Fixed Disk Parameter Table on screen
	lodsw 	; Number of Cylinders
	mov	edi, cylnum
	call	write_dhex
	lodsb	; Number of Heads
	mov	edi, headnum
	call	write_hex
	lodsb	; Reserved
	mov	edi, rsvd3
	call	write_hex
	lodsb	; Reserved
	mov	edi, rsvd4
	call	write_hex
	lodsw	; Precompensation (Obsolete)
	mov	edi, pcompnum
	call	write_dhex
	lodsb	; Reserved
	mov	edi, rsvd7
	call	write_hex
	lodsb	; Drive Control Byte
	mov	edi, dcbnum
	call	write_hex
	lodsw	; Reserved
	mov	edi, rsvd9
	call	write_dhex
	lodsb	; Reserved
	mov	edi, rsvd11
	call	write_hex
	lodsw	; Landing Zone (Obsolete)
	mov	edi, lzonenum
	call	write_dhex
	lodsb	; Sectors per Track
	mov	edi, psptnum
	call	write_hex
	lodsb	; Reserved
	mov	edi, rsvd15
	call	write_hex
	;
	; (extension, not in original bios function)
	lodsw	; I/O Port Base Address
	mov	edi, bPortAddr
	call	write_dhex
	; 06/01/2015
	lodsw	; Control Port Address
	mov	edi, cPortAddr
	call	write_dhex
	lodsb	; Head Register Upper Nibble 
	mov	edi, hregupnib
	call	write_hex
	;
	mov     al, [drv]
	mov	bl, al
	add	al, '0'
        mov     [dsknum], al
	;	      
	shl	bl, 2
	movzx	esi, bl
        add     esi, drv_size
	mov	ax, [esi+2]
        mov     edi, disksize
	call	write_dhex
	mov	ax, [esi]
	mov	edi, disksize+4
	call	write_dhex	
	;
        mov     esi, HDPT
	call	print_msg
	retn

print_thdpt:
	; Writing the Translated FDPT on screen
	; (PHOENIX - EDD specification v1.1)
	lodsw 	; Logical Numbers of Cylinders, Limit 1024
	mov	edi, lcylnum
	call	write_dhex
	lodsb	; Logical Numbers of Heads, Limit 256
	mov	edi, lheadnum
	call	write_hex
	lodsb	; A0h signature, indicates translated table
	mov	edi, tsignum
	call	write_hex
	lodsb	; Physical Sectors per Track
	mov	edi, tpsptnum
	call	write_hex
	lodsw	; Precompensation (Obsolete)
	mov	edi, tpcompnum
	call	write_dhex
	lodsb	; Reserved
	mov	edi, trsvd7
	call	write_hex
	lodsb	; Drive Control Byte
	mov	edi, tdcbnum
	call	write_hex
	lodsw	; Physical Cylinders, limit 65536
	mov	edi, tpcylnum
	call	write_dhex
	lodsb	; Physical Heads, limit 16
	mov	edi, tpheadnum
	call	write_hex
	lodsw	; Landing Zone (Obsolete)
	mov	edi, tlzonenum
	call	write_dhex
	lodsb	; Logical Sectors per Track, Limit 63
	mov	edi, lsptnum
	call	write_hex
	lodsb	; Checksum for translated FDPT 
	mov	edi, checksum
	call	write_hex
	;
	; (extension, not in original bios function)
	lodsw	; I/O Port Base Address
	mov	edi, tbPortAddr
	call	write_dhex
	; 06/01/2015
	lodsw	; Control Port Address
	mov	edi, tcPortAddr
	call	write_dhex
	lodsb	; Head Register Upper Nibble 
	mov	edi, thregupnib
	call	write_hex
	;
	mov     al, [drv]
	mov	bl, al
	add	al, '0'
        mov     [tdsknum], al
	;  
	shl	bl, 2
	movzx	esi, bl
        add     esi, drv_size
	mov	ax, [esi+2]
	mov	edi, tdisksize
	call	write_dhex
	mov	ax, [esi]
	mov	edi, tdisksize+4
	call	write_dhex	
	;
	mov     esi, THDPT
	;call	print_msg
	;retn

print_msg:
	mov	bx, 7
        mov     ah, 0Eh
pmsg_loop:
	lodsb
	and	al, al
	jz	short pmsg_ok
	int	31h	; TRDOS 386 video interrupt
	jmp	short pmsg_loop	
pmsg_ok:
	mov	ah, 10h ; Getchar
	int	32h	; TRDOS 386 keyboard interrupt
	retn

	; 28/08/2020
dskvprm:
	; DISPLAY (LBA) DISK SIZE AND VIRTUAL CHS PARAMETERS
	;
	; INPUT -> DL = Disk/Drive #
	; 
	mov	byte [drv], dl  ; 0,1,80h,81h,82h,83h 
	;
	test	dl, 80h
	jz	short dskvprm0
	sub	dl, 7Eh ; hd0 = 2
dskvprm0:
	movzx	esi, dl

	cmp	byte [esi+drv_status], 80h  ; existing ?
	jb	rfdp_err
	;
	call	clear_screen	 ; clear video page 0	
	;

	shl	si, 2 ; * 4
	mov	ax, [esi+drv_size+2]
	mov	edi, lbadisksize
	call	write_dhex
	mov	ax, [esi+drv_size]
	mov	edi, lbadisksize+4
	call	write_dhex
	shr	si, 1
	mov	ax, [esi+drv_cylinders]
	mov	edi, vcylinders
	call	write_dhex	
	mov	al, [esi+drv_heads]
	mov	edi, vheads
	call	write_hex
	mov	al, [esi+drv_spt]
	mov	edi, vspt
	call	write_hex

	mov	ax, si
	shr	al, 1
	add	al, '0'
	mov	[vprm_drv], al

        mov     esi, VPRMS
	call	print_msg
	retn

;
FLPDPT:
	db 07h
	db 0Dh, 0Ah	
	db 'Disk '
flpdnum:
	db 'X - '
	db 'DISKETTE PARAMETER TABLE'
	db 0Dh, 0Ah, 0Dh, 0Ah
	db 'Type                 : '
flpdtype:
	db 'X   '
	db '[ 1 = 360K, 2 = 1.2M, 3 = 720K, 4 = 1.44M ]'
	db 0Dh, 0Ah, 0DH, 0Ah 
	db 'SRT - Head Unld Time : '
rSrtHdUnld:
	db 'XXh (bits 0-3: SRT, bits 4-7: head unload time)'
	db 0Dh, 0Ah
	db 'DMA - Head Load Time : '
rDmaHdLd:
	db 'XXh (bit 0: 1 = DMA, bits 2-7: head load time)'
	db 0Dh, 0Ah
	db 'Motor Off Count      : '
bMotorOff:
	db 'XXh (with 55ms icrements before turning off)'
	db 0Dh, 0Ah
	db 'Sector Size          : '
bSectSize:
	db 'XXh (2 = 512 bytes)'
	db 0Dh, 0Ah	
	db 'Last Sect on a Track : '
bLastTrack:
	db 'XXh'
	db 0Dh, 0Ah
	db 'Gap Length  (R/W)    : '
bGapLen:
	db 'XXh'
	db 0Dh, 0Ah
	db 'Data Transfer Length : '
bDTL:
	db 'XXh'
	db 0Dh, 0Ah		
	db 'Gap Length (Format)  : '
bGapFmt:
	db 'XXh'
	db 0Dh, 0Ah
	db 'Fill Char for format : '
bFillChar:
	db 'XXh (normally F6h)'
	db 0Dh, 0Ah
	db 'Head Settle Time     : '
bHdSettle:
	db 'XXh milliseconds'
	db 0Dh, 0Ah
	db 'Motor Startup Time   : '
bMotorOn:
	db 'XXh (in 1/8th second intervals)'
	db 0Dh, 0Ah
	; 19/12/2014
	db 0Dh, 0Ah
	db 'Maximum Track Number : '
bMaxTrack:
	db 'XXh'
	db 0Dh, 0Ah
	db 'Data Transfer Rate   : '
bDataRate:
	db 'XXh (00h = 500KBS, 40h = 300KBS, 80H = 250KBS)'
	db 0Dh, 0Ah
	db 0Dh, 0Ah, 0

HDPT:
	db 07h
	db 0Dh, 0Ah
	db 'Disk '
dsknum:
	db 'X - '	
	db 'FIXED DISK PARAMETER TABLE'
	db 0Dh, 0Ah, 0Dh, 0Ah 
	db 'Number of Cylinders : '
cylnum:
	db 'XXXXh'
	db 0Dh, 0Ah
	db 'Number of Heads     : '
headnum:
	db 'XXh'
	db 0Dh, 0Ah
	db 'Reserved            : '
rsvd3:
	db 'XXh'
	db 0Dh, 0Ah
	db 'Reserved            : '
rsvd4:
	db 'XXh'
	db 0Dh, 0Ah	
	db 'Precompensation     : '
pcompnum:
	db 'XXXXh'
	db 0Dh, 0Ah
	db 'Reserved            : '
rsvd7:
	db 'XXh'
	db 0Dh, 0Ah
	db 'Drive Control Byte  : '
dcbnum:
	db 'XXh'
	db 0Dh, 0Ah		
	db 'Reserved            : '
rsvd9:
	db 'XXXXh'
	db 0Dh, 0Ah
	db 'Reserved            : '
rsvd11:
	db 'XXh'
	db 0Dh, 0Ah
	db 'Landing Zone        : '
lzonenum:
	db 'XXXXh'
	db 0Dh, 0Ah
	db 'Sectors per Track   : '
psptnum:
	db 'XXh'
	db 0Dh, 0Ah
	db 'Reserved            : '
rsvd15:
	db 'XXh'
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db 'I/O Port Base Addr  : '
bPortAddr:
	db 'XXXXh'
	db 0Dh, 0Ah
	db 'Control Port Addr   : '
cPortAddr:
	db 'XXXXh'
	db 0Dh, 0Ah
	db 'Head Reg Upp Nibb   : '
hregupnib:
	db 'XXh'
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db 'Size (in sectors)   : '
disksize:
	db 'XXXXXXXXh'
	db 0Dh, 0Ah
	db 0Dh, 0Ah, 0

THDPT:
	db 07h
	db 0Dh, 0Ah
	db 'Disk '
tdsknum:
	db 'X - '	
	db 'TRANSLATED FIXED DISK PARAMETER TABLE'
	db 0Dh, 0Ah, 0Dh, 0Ah 
	db 'Logical Cylinders   : '
lcylnum:
	db 'XXXXh'
	db 0Dh, 0Ah
	db 'Logical Heads       : '
lheadnum:
	db 'XXh'
	db 0Dh, 0Ah
	db 'Signature           : '
tsignum:
	db 'XXh'
	db 0Dh, 0Ah
	db 'Phy Sec per Track   : '
tpsptnum:
	db 'XXh'
	db 0Dh, 0Ah	
	db 'Precompensation     : '
tpcompnum:
	db 'XXXXh  (Obsolete)'
	db 0Dh, 0Ah
	db 'Reserved            : '
trsvd7:
	db 'XXh'
	db 0Dh, 0Ah
	db 'Drive Control Byte  : '
tdcbnum:
	db 'XXh'
	db 0Dh, 0Ah		
	db 'Physical Cylinders  : '
tpcylnum:
	db 'XXXXh'
	db 0Dh, 0Ah
	db 'Physical Heads      : '
tpheadnum:
	db 'XXh'
	db 0Dh, 0Ah
	db 'Landing Zone        : '
tlzonenum:
	db 'XXXXh  (Obsolete)'
	db 0Dh, 0Ah
	db 'Logic Sec per Trk   : '
lsptnum:
	db 'XXh'
	db 0Dh, 0Ah
	db 'Checksum            : '
checksum:
	db 'XXh'
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db 'I/O Port Base Addr  : '
tbPortAddr:
	db 'XXXXh'
	db 0Dh, 0Ah
	db 'Control Port Addr   : '
tcPortAddr:
	db 'XXXXh'
	db 0Dh, 0Ah
	db 'Head Reg Upp Nibb   : '
thregupnib:
	db 'XXh'
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db 'Size (in sectors)   : '
tdisksize:
	db 'XXXXXXXXh'
	db 0Dh, 0Ah
	db 0Dh, 0Ah, 0

hex_digits:
hexchrs:
	db '0123456789ABCDEF'

ds_drv:
	db 0FFh ; Current drive (on display)
 	db 0    ; Current half (0 or >0)

drv_names:
	db 'fd0 fd1 hd0 hd1 hd2 hd3 '

dpheader:
	db ' Drive : '
drv_name:
	db '000  '
	db  'Sector : '
sector_num:
	db  'FFFFFFFFh'
        db 0

sdline:
	db ' Byte '
sdline_1:
	db '000h'
	db ' -  '
sdline_2:
	db '00 00 00 00 00 00 00 00 '
	db '00 00 00 00 00 00 00 00 '
	db ' '
sdline_3:
	db '................'
	db 20h

dpfooter1:
	db ' F1 = Change Drive  '
	db 'Home = First Sector '
	db 'PgUp = Previous Sector '
	db 'ESC = EXIT'
	db 0
dpfooter2:
	db ' F2 = Change Sector '
	db 'End = Last Sector   '
	db 'PgDown = Next Sector   ' 
	db 'ENTER = Prv/Nxt'
	db 0

F1_ib:
	db 16	; box width (columns)
	db 3	; box height (rows)
	db 1	; label offset (vertical)
	db 1	; label offset (horizontal)
	db 1	; text (input) size
	db 4Eh	; box color
	db 'Drive: '  ; Label
	db 0

F2_ib:
	db 20	; box width (columns)
	db 3	; box height (rows)
	db 1	; label offset (vertical)
	db 1	; label offset (horizontal)
	db 8	; text (input) size
	db 4Eh	; box color
	db 'Sector : '  ; Label
	db 0

dskr_err:
	db 33	; box width (columns)
	;db 17	
	db 3	; box height (rows)
	db 1	; label offset (vertical)
	db 1	; label offset (horizontal)
	db 0	; text (input) size
	db 4Eh	; box color
	db 'Drive not ready or read error !'  ; Label
	;db ' Error : '
;err_code_str:
;	db '00h ! '	
	db 0

	; 28/08/2020
VPRMS:
	db 07h
	db 0Dh, 0Ah
	db 'Disk '
vprm_drv:
	db 'X - '	
	db 'DISK SIZE AND (VIRTUAL) CHS VALUES'
	db 0Dh, 0Ah, 0Dh, 0Ah
	db 'Disk Size : '
lbadisksize:
	db 'XXXXXXXXh sectors'		
 	db 0Dh, 0Ah, 0Dh, 0Ah

	db 'Cylinders : '
vcylinders:
	db 'XXXXh'
	db 0Dh, 0Ah
	db 'Heads     : '
vheads:
	db 'XXh'
	db 0Dh, 0Ah
	db 'Sectors   : '
vspt:
	db 'XXh'
	db 0Dh, 0Ah
	db 0Dh, 0Ah, 0	

; Additional functions, variables/pointers for 
; Real Mode adaption (out of unix386.s) variables/pointers

set_cpos:
	mov	dx, [cursor_posn] ; dh = row, dl = column
set_cposx:
	; DX = cursor position
	mov	ah, 2		; Set cursor position
	xor	bh, bh		; for video page 0
	int	31h		; TRDOS 386 video interrupt
	retn

align 2

prg_msg:
	db 0Dh, 0Ah, 07h
	db 'Disk Read Utility - TRDOS 386 v2 Disk I/O and timer test.'
	db 0Dh, 0Ah	
	;db 'by Erdogan Tan  [07/07/2016]'
	db 'by Erdogan Tan  [29/08/2020]'   ; LBA disk (>8GB) bugfix
	db 0Dh, 0Ah, 0Dh, 0Ah
        db '(Press any key to continue...)'
	db 0Dh, 0Ah, 0

drv_not_ready:
	db 07h, 0Dh, 0Ah 
	db 'Drive not ready !'
	db 0Dh, 0Ah, 0

fd0_type: db '0'
fd1_type: db '0'

rtc_msg:
	db "Real Time Clock - "
datestr:
	db "00/00/0000"
	db "  "
timestr:	
        db "00:00:00"
rtc_msg_end:
	db 0

timer_event:
	db 0 

align 4 ; dword alignment

current_txtpos: dd video_buffer

bss_start:

ABSOLUTE bss_start

cursor_posn: resw 1
cursor_shp:  resw 1
cursor_posb: resw 1 ; (cursor position backup, for video page 0)

txtposoff:   resb 1 ; txtpos offset for sector number input	
dscmd:	     resb 1 ; 0 = change drive
	            ; 1 = change sector
	            ; 2 = display disk parameters

inds:	     resb 1 	 
paragr:	     resb 1	 

ibcp:	     resb 1 ; input box - row position
	     resb 1 ; input box - column position

retry_count: resb 1
drv:	     resb 1  ; physical drive number (0, 1, 80h, 81h, 82h, 83h)

drv_status:  resb 2  ; fd0, fd1 (FFh = failure, 80h = existing)		
	     resb 4  ; hd0, hd1 hd2, hd3 (FFh = failure)
                    ;                   (80h - 87h = existing)
                    ;                   (bit 0 = 1 : LBA ready)

drv_cylinders :	resw 6
drv_heads     :	resw 6
drv_spt       :	resw 6
alignb 4
drv_size :	resd 6

fd0_dpt: resb 16
fd1_dpt: resb 16
hd0_dpt: resb 32
hd1_dpt: resb 32
hd2_dpt: resb 32
hd3_dpt: resb 32

ds_sec:
	resd 1 ; Current sector (on display), drv 0		
	resd 1 ; Current sector (on display), drv 1
	resd 1 ; Current sector (on display), drv 2
	resd 1 ; Current sector (on display), drv 3
	resd 1 ; Current sector (on display), drv 4
	resd 1 ; Current sector (on display), drv 5

prev_sec: resd 1  ; previous sector (before reading)	

sector_buffer:
	resb 512

date_day:
	resb 1
date_month:
	resb 1
date_year:
	resb 1
date_century:
	resb 1

time_second:
	resb 1
time_minute:
	resb 1
time_hour:
	resb 1
	
	resb 1

video_buffer:
	resb	4000 ; 80*25*2

timer_event_number:
	resb 1

hdc:	resb 1
fdc:	resb 1

prev_drv:
	resb 1

alignb 4

bss_end:
	
_end:
