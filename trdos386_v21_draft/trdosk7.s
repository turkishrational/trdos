; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel - v2.0.6) - DISK READ&WRITE : trdosk7.s
; ----------------------------------------------------------------------------
; Last Update: 29/08/2023  (Previous: 25/07/2022 - Kernel v2.0.5)
; ----------------------------------------------------------------------------
; Beginning: 24/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11 (trdos386.s)
; ----------------------------------------------------------------------------
; Derived from TRDOS Operating System v1.0 (8086) source code by Erdogan Tan
; DISK_IO.ASM (20/07/2011)
; ****************************************************************************
; DISK_IO.ASM (c) 2009-2011 Erdogan TAN [ 04/07/2009 ] Last Update: 20/07/2011

disk_write:
	; 25/02/2016
	; 24/02/2016
	; 23/02/2016
	cmp	byte [esi+LD_LBAYes], 0
        ja      short lba_write

chs_write:
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 25/02/2016
	; 23/02/2016
	mov	byte [disk_rw_op], 3 ; CHS write
	jmp	short chs_rw

disk_read:
	; 25/02/2016
	; 24/02/2016
	; 23/02/2016
	; 17/02/2016
	; 14/02/2016
	; 31/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 17/10/2010
	; 18/04/2010
	;
	; INPUT -> EAX = Logical Block Address
	;	   ESI = Logical Dos Disk Table Offset (DRV)
	;	   ECX = Sector Count
	; 	   EBX = Destination Buffer
	; OUTPUT ->
	;	   cf = 0 or cf = 1
	; (Modified registers: EAX, EBX, ECX, EDX)

	cmp	byte [esi+LD_LBAYes], 0
        ja      short lba_read

chs_read:
	; 29/08/2023 (TRDOS 386 Kernel v2.0.6)
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 25/02/2016
	; 24/02/2016
	; 23/02/2016
	; 31/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 20/07/2011
	; 04/07/2009
	;
	; INPUT -> EAX = Logical Block Address
	;	   ECX = Number of sectors to read
	; 	   ESI = Logical Dos Disk Table Offset (DRV)
	; 	   EBX = Destination Buffer
	; OUTPUT ->
	;	   cf = 0 or cf = 1
	; (Modified registers: EAX, EBX, ECX, EDX)

	; 23/02/2016
	mov	byte [disk_rw_op], 2 ; CHS read

chs_rw:
	;;movzx	edx, word [esi+LD_BPB+SecPerTrack]
	;movzx	edx, byte [esi+LD_BPB+SecPerTrack] ; <= 63
	;mov	[disk_rw_spt], dl

chs_read_next_sector:
	mov	byte [retry_count], 4

chs_read_retry:
	;mov	[sector_count], ecx ; 23/02/2016

	push	eax			; Linear sector #
	push	ecx			; # of FAT/FILE/DIR sectors

	movzx	ecx, word [esi+LD_BPB+SecPerTrack]
	;movzx	ecx, byte [disk_rw_spt] ; 23/02/2016
	sub	edx, edx
	div	ecx
	; eax = track, dx (dl ) = sector (on track)
	;sub	cl, dl ; 24/02/2016 (spt - sec)
	;push	ecx ; *
	; 25/07/2022
	;mov	cx, dx			; Sector (zero based)
	;inc	cx			; To make it 1 based
	;push	cx
	; 29/08/2023
	;mov	ecx, edx
	;inc	ecx
	;push	ecx
	inc	edx
	push	edx ; dl = sector number (on track)
	;
	mov	cx, [esi+LD_BPB+Heads]
	;sub	dx, dx
	; 25/07/2022
	sub	edx, edx
	div	ecx			; Convert track to head & cyl
	; eax (ax) = cylinder, dx (dl) = head (max. FFh)
	mov	dh, dl
	;pop	cx			; AX=Cyl, DH=Head, CX=Sector
	; 25/07/2022
	pop	ecx ; sector number (on track)
	mov	dl, [esi+LD_PhyDrvNo]

	mov	ch, al			; NOTE: max. 1023 cylinders !
	ror	ah, 2			; Rotate 2 bits right
	or	cl, ah

	; 24/02/2016
	;pop	eax ; * (spt - sec) (example: 63 - 0 = 63)
	;cmp	eax, [sector_count]
	;jb	short chs_write_sectors
	;je	short chs_read_sectors
	;; (# of sectors to read is more than remaining sectors on the track)
	;mov	al, [sector_count]
;chs_read_sectors: ; read or write !
	mov	al, 1 ; 25/02/2016
	mov	ah, [disk_rw_op]  ; 02h = chs read, 03h = chs write 
	;
	call	int13h			; BIOS Service func ( ah ) = 2
                                        ; Read disk sectors
                                        ; AL-sec num CH-track CL-sec
                                        ; DH-head DL-drive ES:BX-buffer
                                        ; CF-flag AH-stat AL-sec read
	                                ; If CF = 1 then (If AH > 0)
	mov	[disk_rw_err], ah

	pop	ecx
	pop	eax
	jnc	short chs_read_ok

	cmp	byte [disk_rw_err], 09h ; DMA crossed 64K segment boundary
	je	short chs_read_error_retn

	dec	byte [retry_count]
	jnz	short chs_read_retry

chs_read_error_retn:
	stc
	;retn
	jmp	short update_drv_error_byte

;chs_write_sectors: ; read or write
	;; (# of sectors to read is less than remaining sectors on the track)
	;mov	[sector_count], al
	;jmp	short chs_read_sectors

chs_read_ok:
	;; 23/02/2016
	;movzx	edx, byte [sector_count] ; sector count (<= spt)
        ;sub    ecx, edx  ; remaining sector count
	;jna	short update_drv_error_byte
	;add	eax, edx ; next disk sector
	;shl	edx, 9 ; 512 * sector count
	;add	ebx, edx ; next buffer byte address
        ;jmp	chs_read_next_sector
	; 25/02/2016
	inc	eax ; next sector
	add	ebx, 512
	loop	chs_read_next_sector
	jmp	short update_drv_error_byte

lba_write:
	; 23/02/2016
	mov	byte [disk_rw_op], 1Ch ; LBA write
	jmp	short lba_rw

lba_read:
	; 14/07/2022
	; 23/02/2016
	; 17/02/2016
	; 14/02/2016
	; 13/02/2016
	; 31/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 10/07/2015 (Retro UNIX 386 v1)
	;
	; INPUT -> EAX = Logical Block Address
	;	   ESI = Logical Dos Disk Table Offset (DRV)
	;	   ECX = Sector Count	
	; 	   EBX = Destination Buffer
	; OUTPUT ->
	;	   cf = 0 or cf = 1
	; (Modified registers: EAX, EBX, ECX, EDX)

	; LBA read/write (with private LBA function)
	;((Retro UNIX 386 v1 - DISK I/O code by Erdogan Tan))

	; 23/02/2016
	mov	byte [disk_rw_op], 1Bh ; LBA read

lba_rw:
	; 17/02/2016
	push	edi

	mov	[sector_count], ecx ; total sector (read) count

	mov	dl, [esi+LD_PhyDrvNo]
	; dl = physical drive number (0, 1, 80h, 81h, 82h, 83h)

lba_read_next:
	; 14/07/2022
	mov	edi, 256
	cmp	ecx, edi
	;cmp	ecx, 256
	jna	short lba_read_rsc
	;mov	ecx, 256 ; 17/02/2016
	mov	ecx, edi
lba_read_rsc:
	sub	[sector_count], ecx ; remain sectors

	mov	edi, ecx 
	mov	ecx, eax ; sector number/address

	mov	byte [retry_count], 4
lba_read_retry:
	mov	eax, edi
	;
	; ecx = sector number
	; al = sector count (0 - 255) /// (0 = 256)
	; dl = drive number
	; ebx = buffer offset
	;
	; Function 1Bh = LBA read, 1Ch = LBA write
	; 23/02/2016
	mov	ah, [disk_rw_op] ; 1Bh = LBA read, 1Ch = LBA write
	call	int13h
	; al = ? (changed)
	; ah = error code
	mov	[disk_rw_err], ah
	jnc	short lba_read_ok
	cmp	ah, 80h ; time out?
        je      short lba_read_stc_retn
	dec	byte [retry_count]
	jg	short lba_read_retry
	jz	short lba_read_reset
	; sf = 1

lba_read_stc_retn:
	stc
lba_read_retn:
	pop	edi

update_drv_error_byte:
	pushf
	push	ebx
	;push	cx
	push	ecx ; 14/07/2022
	;or	ecx, ecx
	;jz	short udrv_errb0
	mov	cl, [disk_rw_err]
udrv_errb0:
	movzx	ebx, byte [esi+LD_PhyDrvNo]
	cmp	bl, 2
        jb      short udrv_errb1
	sub	bl, 7Eh
	;cmp	bl, 5
	;ja	short udrv_errb2
udrv_errb1:
        add     ebx, drv.error ; 13/02/2016
	mov	[ebx], cl ; error code
udrv_errb2:
	;pop	cx
	pop	ecx  ; 14/07/2022
	pop	ebx
	popf
	retn

lba_read_ok:
	mov	eax, ecx ; sector number
	add	eax, edi ; sector number (next)
	shl	edi, 9 ; sector count * 512
	add	ebx, edi ; next buffer offset

	mov	ecx, [sector_count] ; remaining sectors
	or	ecx, ecx
	jnz	short lba_read_next
	jmp	short lba_read_retn

lba_read_reset:
	mov	ah, 0Dh ; Alternate reset
        call	int13h
	; al = ? (changed)
	; ah = error code
	jnc	short lba_read_retry
	jmp	short lba_read_retn
