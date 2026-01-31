; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel) - v2.0.0 - trfs1fdb.s - TRFS1 BOOT SECTOR
; ----------------------------------------------------------------------------
; Turkish Rational SINGLIX File System 1 1.44MB Floppy Disk Boot Sector Code
; ----------------------------------------------------------------------------
; Last Update: 02/02/2018
; ----------------------------------------------------------------------------
; Beginning: 06/01/2018 
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11  
; ----------------------------------------------------------------------------
;	    ((nasm trfs1fdb.s -l trfs1fdb.lst -o TRFS1FDB.BIN)) 	
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
;
; Derived from 'trfs1_chs.s' TRDOS 386 TRFS1 boot sector source code
; by Erdogan Tan (04/01/2018).
;
; ****************************************************************************
; incbin "TRFS1FDB.BIN"

rts_segment     equ	1000h

; FDT equalities
; 03-01-2018 [ TRFS Issue 2, Revision 14 ]

FDT_Sign	equ 0	; 'FDT'
FDT_SectorSize	equ 4	; 512
FDT_Number	equ 6	; FDT sequence number
FDT_FileNumber	equ 8	; 1st FDT address (offset)	
FDT_NextFDTNum	equ 12  ; Next FDT address
FDT_SectorCount	equ 16	; Sector count in section
FDT_ParentDir	equ 20	; Parent directory address
FDT_PDirSerial	equ 24	; Parent directory serial number
FDT_FileSize	equ 28	; File Size lower 4 bytes
FDT_FileSizeHW	equ 32	; File Size higher 2 bytes

[BITS 16]
[ORG 7C00h]
bsjmpBoot:
                ;jmp     short loc_41  ; jmp short start
		jmp	short loc_30
bsjmpBoot_nop:
                nop

; TR-SINGLIX FS1 BootSector Identification (Data) Block
; 03-01-2018 FS1 Boot Sector
; 02-01-2018 [ TRFS Issue 1, Revision 14 ]

bsFSystemID:    db 'FS'			; bp+3
                db 0   
bsBytesPerSec:  dw 512			; bp+6
bsMediaAttrib:  db 3			; bp+8
bsPartitionID:  db 0A1h			; bp+9
bsFSVersionMaj: db 01h			; bp+10
bsFSVersionMin: db 0			; bp+11
bsBeginSector:	dd 0			; bp+12 
bsVolumeSize:   dd 2880			; bp+16
bsStartupFDT:	dd 0			; bp+20
bsMATLocation:  dd 1			; bp+24
bsRootDirDT:    dd 3			; bp+28
bsSysConfFDT:	dd 0			; bp+32
bsSwapFD:       dd 0			; bp+36
bsUndelDirDT:	dd 0			; bp+40
bsDriveNumber:  db 0			; bp+44
bs_LBA_Ready:	db 0			; bp+45
bsMagicWord:	
bs_Disk_SecPerTrack:
		db 18			; bp+46
bs_Disk_Heads: 
                db 2			; bp+47
;bsOperationSys: 
;;		db 'TR-SINGLIX v1.0b'	; bp+48
;		db 'TR-DOS 386 TRFS1'
;terminator:	db 0

; NOTE: This boot sector is valid for < 32MB Hard Disks and Floppy Disks
; [bs_Disk_SecPerTrack] and [bs_Disk_Heads] and
; [bsVolumeSize] and [bsRootDirDT] and [bsBeginSector] and
; [bsMediaAttrib] must be set as correct!
; High word of all double word -dd- parameters must be 0!!!
; Max. volume size = 65535 sectors. 

; !!! ((Default configuration: 1.44MB (3.5") Floppy Disk)) !!!

start:
;loc_41:
loc_30:		mov	bp, 7C00h

		xor	ax, ax
		mov	ds, ax
		mov	es, ax
		cli
		mov	ss, ax
		mov	sp, bp
		sti
		mov	dl, [bp+44] ; [bsDriveNumber]

		mov	[bsReserved1], ax 

		;; Check File System ID value
		;cmp	word [bp+3], 'FS' ; [bsFSystemID]
		;jne	short invalid_system_disk

		; Check Bytes/Sector value
		; It must be 512 !? (at least, for TRDOS386) 
		;cmp	word [bp+6], 512 ; [bsBytesPerSec]
		;jne	short invalid_system_disk

		; Check LBA signature
		;cmp	byte [bp+45], 0 ; [bs_LBA_Ready]
		;ja	short invalid_system_disk

		; Check Volume Size (>= 32MB)
		;cmp	word [bp+18], 0
		;ja	short invalid_system_disk

check_startup_file_address: 
		mov	ax, [bp+20] ; [bsStartupFDT]
		and	ax, ax
		jz	short invalid_system_disk

		cmp	ax, [bp+16] ; [bsVolumeSize]
		jnb	short invalid_system_disk

		sub	sp, 4 ; 13/01/2018
		; SP = 7BFCh

		add	ax, [bp+12] ; [bsBeginSector]
		mov	[bp-2], ax

		; overwrite hd drive number !
                ;mov	[bsDriveNumber], dl ; drive number from INT 19h
		;mov	[bp+44], dl

		mov	bx, 7E00h    ; FDT Buffer address
		;mov	si, bx

		call	read_sector
		jc	short disk_io_error

check_startup_file_fdt: 
		; Check FDT signature
		;lodsw
		;cmp	ax, 'FD' 
		;jne	short invalid_system_disk ; not a valid FDT!
		;lodsb
		;cmp	al, 'T'
		;jne	short invalid_system_disk ; not a valid FDT!
		;add	si, 5
		mov	si, 7E00h+FDT_FileNumber
		lodsw	; FDT_FileNumber
		mov	dx, [bp+12] ; [bsBeginSector]
		mov	di, [bp-2]  ; Current FDT address (LBA) 
		add	ax, dx
		cmp	di, ax
		jne	short invalid_system_disk ; not a valid FDT!
		lodsw ; +2
		lodsw	; FDT_NextFDTNum
		add	ax, dx  ; DX = Volume Beginning Sector
		mov	[bp-2], ax  ; Next FDT address (LBA)
		lodsw ; +2
		; DI = Current FDT address
		lodsw	; FDT_SectorCount
		or	ax, ax
		jz	short invalid_system_disk ; not a valid FDT!
		mov	cx, ax
		add	si, 2+8
		lodsw	; FDT_FileSize
		mov	dx, ax
		lodsw
		xchg	dx, ax

		or	ax, ax
		jnz	short bs_02
		or	dx, dx
		jnz	short bs_02 

invalid_system_disk:
		mov	si, Inv_disk_Msg
		call	print_msg
getchar_reboot:
		; Wait for a keystroke just before reboot
		xor	ah, ah
		int	16h
		
		int	19h	; disk boot	
				; causes reboot of disk system
disk_io_error:
		mov	si, Diskio_err_Msg
		call	print_msg
;replace_disk:		
;		mov	si, Replace_Msg	
replace_disk:	
		mov	si, Disk_err_replace_Msg
		call	print_msg
		jmp	short getchar_reboot

bs_02:	
		mov	bx, 511
		add	ax, bx
		adc	dx, 0
		inc	bx
		div	bx
		mov	bx, 1151
		cmp	ax, bx		; Maximum 1151 sectors
					; (Segment 1000h to 9FE0h)
					; ((512 bytes must be reserved for
					; stack just before segment A000h))	
		jna	short bs_03
		mov	ax, bx
bs_03:
		; CX = sector count (in section)
load_startup_file:
		cmp	ax, cx
		jnb	short bs_04
		mov	cx, ax		; sector count (in section)
					; must not be greater
					; remain sectors (for file) to read
bs_04:
		sub	ax, cx
		mov	[bp-4], ax	; Remain sector count (for next read)	
		;mov	ax, di ; FDT address
		xchg	ax, di
		inc	ax	 ; +1 (section data)
		; CX = sector count (<= 1151) in section
		mov	bx, [next_segment]
		push	es
		mov	es, bx ; segment = 1000h +
		xor	bx, bx ; offset = 0 
		; CX = num of sectors to read (= sectors/cluster)
		call	disk_read
		pop	es
		jc	short disk_io_error
		shr	bx, 4 ; from byte count to paragraph count
		add	[next_segment], bx

		;mov	di, [bp-4]	; Remain sector count
		or	di, di
		jz	bs_07		; none
		
		mov	ax, [bp-2]	; Next FDT address
		mov	bx, 7E00h	; FDT Buffer address
		mov	si, bx

		call	read_sector
		jc	short disk_io_error
		
		lodsw
		cmp	ax, 'FD' 
		jne	short invalid_system_disk ; not a valid FDT!
		lodsb
		cmp	al, 'T'
		jne	short invalid_system_disk ; not a valid FDT!
		add	si, 9
		lodsw	; FDT_NextFDTNum
		add	ax, [bp+12] ; [bsBeginSector]
		xchg	[bp-2], ax  ; Next FDT address (LBA)
		mov	dx, ax ; Current FDT address
		;xchg	dx, ax
		lodsw	; +2
		lodsw	; FDT_SectorCount
		or	ax, ax
		jz	invalid_system_disk ; not a valid FDT!
		mov	cx, ax ; sector count (in section)
		;xchg	cx, ax
		mov	ax, di ; [bp-4] ; remain sectors to read
		;xchg	ax, di
		mov	di, dx
		;xchg	di, dx
		; DI = Current FDT address
		jmp	short load_startup_file

read_sector:	; 25/12/2017 (Read 1 sector)
		mov	cx, 1
disk_read:
		;mov	byte [bp+retry_count-7C00h], 4
		mov	dl, 4 ; retry count
disk_read_0:
		pusha
chs_read:	
		; Convert LBA to CHS
		xor	dx, dx
		xor	ch, ch
		;mov	cl, byte [bs_Disk_SecPerTrack] ; [bp+46]
				; sectors per track (18 or 17 or 63)
		mov	cl, [bp+46]
		div	cx
		inc	dl	; sector number (1 based)
		push	dx
		; ax = (heads * cylinder) + head number
		sub	dx, dx
		;mov	cl, [bs_Disk_Heads] ; [bp+47]
		mov	cl, [bp+47] ; number of heads (2 to 255)	
		div	cx 	
		; AX = cylinder (0 to 1023)
		; DX = head number (in DL)
		mov	dh, dl	 ; head number in DH
		;mov	dl, [bsDriveNumber] ; [bp+2Ch] ; Drive number (80h)
		mov	dl, [bp+44]
		pop	cx
		mov	ch, al ; Low 8 bits of cylinder number (0 to 7)
		shl	ah, 6  ; High 2 bits of cylinder is in bit 7&8	
		or	cl, ah ; High two bits of CL is cylinder bits 8&9 
		mov	ax, 201h ; Read 1 sector
		int	13h
disk_read_1:
		popa
		jnc	short disk_read_2
		; cf = 1
		;dec	byte [retry_count]
		;dec	byte [bp+retry_count-7C00h]
		dec	dl ; Retry count
		jnz	short disk_read_0 ; Retry
		; cf = 1
		retn
disk_read_2:
		;add	bx, [bp+6] ; [bsBytesPerSec] ; 512
		add	bx, 512
		;add	bh, 2 ; **
		jnc	short disk_read_3
		;mov	bx, [next_segment]
		mov	bx, es
		add	bh, 10h	 
		mov	[next_segment], bx
		mov	es, bx
		xor	bx, bx
disk_read_3:
		inc	ax
		dec	cx
		jnz	short disk_read
		;clc 	; ** (128 sectors/cluster!?)
bs_05:
		retn

print_msg:
		; DS:SI = Error message address (ASCIIZ string)	
		mov	ah, 0Eh
		mov	bx, 7
bs_06:
		lodsb
		test	al, al
		jz	short bs_05
		int	10h
		jmp	short bs_06

next_segment:
		dw	rts_segment

		; Filler (Magic Number)
		db	4
		db	1
		db	7

		; Filler (File System)
		db	'TRFS1'
		db	0

		; Filler (Boot Code Date)
		db	'02/02/2018'
		db	0

bs_07:
		; Set TRDOS 386 kernel specific parameters (& signs)
		; and
		; Launch TRDOS 386 Kernel (Startup/RTS file)

		mov	ax, [next_segment] ; 16 paragraphs after the
					  ; start of the last segment
					  ; of the kernel file loading
					  ; space.
					  ; So, (top of) stack will have
					  ; 256 bytes or more distance
					  ; from the last byte
					  ; of the kernel file.	 							
					  ; (This will be enough for
					  ; TRDOS 386 kernel before 
					  ; entering protected mode.)
		cli
		mov	ss, ax
		mov	sp, 0FFFEh			
		sti

		mov     bx, rts_segment ; 1000h

		mov	ds, bx
		mov	es, bx
		;mov	fs, bx
		;mov	gs, bx

		;xor	bx, bx
		;xor	cx, cx
		;xor	dx, dx
		;xor	si, si
		;xor	di, di
		;xor	bp, bp

		; bp = 7C00h

		;mov	dl, [bsDriveNumber]
                mov	dx, [bp+44] ; DL = Drive number, DH = 0 (CHS)

		mov	ax, 417 ; TRDOS boot sector sign for TRDOS386.SYS

		jmp	rts_segment:0

Diskio_err_Msg:
		db	0Dh, 0Ah
		db	'Disk error'
		;db	'!'
		db	0
Inv_disk_Msg:   
		db	0Dh, 0Ah
		db	'Invalid system disk'
Disk_err_replace_Msg:
		db	'!'
Replace_Msg:    
		db	0Dh, 0Ah
		db	'Replace the disk and press any key to reboot.'
		db	0Dh, 0Ah, 0

		times	508 - ($ - $$) db 0
bsReserved1:
		db	'TR'  ; 'Turkish Rational DOS' feature identifier.
bootsignature1:
		db	55h, 0AAh