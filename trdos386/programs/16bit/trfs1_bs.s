; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel) - v2.0.0 - trfs1_bs.s - TRFS1 BOOT SECTOR
; ----------------------------------------------------------------------------
; Turkish Rational SINGLIX File System 1 (Hard Disk) Boot Sector Source Code
; ----------------------------------------------------------------------------
; Last Update: 04/01/2018
; ----------------------------------------------------------------------------
; Beginning: 03/01/2018 
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11  
; ----------------------------------------------------------------------------
;	    ((nasm trfs1_bs.s -l trfs1_bs.lst -o TRFS1_BS.BIN)) 	
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
;
; Derived from 'fat32_bs.s' TRDOS 386 (v2) FAT32 boot sector source code
; by Erdogan Tan (25/12/2017).
;
; Derived from 'FS1_HDBS.ASM' TRDOS 8086 (v1) Singlix FS1 boot sector
; source code by Erdogan Tan (21/02/2010).
; ****************************************************************************
; incbin "TRFS1_BS.BIN" (in 'hdimage.s' & 'hdformat.s')

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
                jmp     short loc_42  ; jmp short start
bsjmpBoot_nop:
                nop


; TR-SINGLIX FS1 BootSector Identification (Data) Block
; 03-01-2018 FS1 HD Partition Boot Sector
; 02-01-2018 [ TRFS Issue 1, Revision 14 ]

bsFSystemID:    db 'FS'			; bp+3
                db 0   
bsBytesPerSec:  dw 512			; bp+6
bsMediaAttrib:  db 1			; bp+8
bsPartitionID:  db 0A1h			; bp+9
bsFSVersionMaj: db 01h			; bp+10
bsFSVersionMin: db 0			; bp+11
bsBeginSector:	dd 0			; bp+12 
bsVolumeSize:   dd 0			; bp+16
bsStartupFDT:	dd 0			; bp+20
bsMATLocation:  dd 2			; bp+24
bsRootDirDT:    dd 0			; bp+28
bsSysConfFDT:	dd 0			; bp+32
bsSwapFD:       dd 0			; bp+36
bsUndelDirDT:	dd 0			; bp+40
bsDriveNumber:  db 0			; bp+44
bs_LBA_Ready:	db 01h			; bp+45
bsMagicWord:	
bs_Disk_SecPerTrack:
		db 0A1h			; bp+46
bs_Disk_Heads: 
                db 01h			; bp+47 
bsOperationSys: 
;		db 'TR-SINGLIX v1.0b'	; bp+48
		db 'TR-DOS 386 TRFS1'
terminator:     db 0

start:
		nop
loc_42:
		mov	bp, 7C00h

		cmp	ax, 417  ; If AX=417, the masterboot sector
				 ; has a SINGLIX FS (& TRDOS 386)
				 ; masterboot code; and...  
				 ; DX=ES=SS=0, BP=7C00h
				 ; SP=7C00h ... masterboot sector has
				 ; been loaded at 0:600h, it has
				 ; CHS parameters at offset 600h+420.
				 ; (There is a 01A1h in offset 600h+417)
	
		je	short bs_01 ; no need to following assignments !

		xor	ax, ax
		mov	ds, ax
		mov	es, ax
		cli
		mov	ss, ax
		mov	sp, bp
		sti
		mov	dl, [bp+44] ; [bsDriveNumber]
bs_01:
		mov	[bsReserved1], si ; Partition entry address..
		; 24/12/2017		  ; (from Singlix FS MBR)
					  ; (7BEh,7CEh,7DEh,7EEh) 

		; Check File System ID value
		cmp	word [bp+3], 'FS' ; [bsFSystemID]
		jne	invalid_system_disk

		; Check Bytes/Sector value
		; It must be 512 !? (at least, for TRDOS386) 
		cmp	word [bp+6], 512 ; [bsBytesPerSec]
		jne	invalid_system_disk

		; overwrite hd drive number !
                ;mov	[bsDriveNumber], dl ; drive number from INT 19h
		mov	[bp+44], dl
		
		; reset SINGLIX FS1 reading pointers and set SP to 7BF4h
		xor	ecx, ecx ; *
		;sub	ecx, ecx ; *
		push	ecx 	; [bp-4] = 0   ; CHS limit (8.4GB)
		push	ecx	; [bp-8] = 0   ; Startup File (FDT) Address
		push	ecx	; [bp-12] = 0  ; Remain sectors to read

		; SP = 7BF4h

		; check for ROMBIOS INT 13h extensions
		mov	ah, 41h
		;mov	ebx, 55AAh  
		mov	bx, 55AAh
		;mov	dl, [bsDriveNumber]
		;mov	dl, [bp+44]
		int	13h
		jc	short bs_02
		cmp	bx, 0AA55h
		jne	short bs_02
		test	cl, 1
		jz	short bs_02
		mov	byte [bp+2], 42h ; LBA ready
		add	byte [bp+45], 41h
bs_02:
		; ..CHS limit setup..

		; Get drive parameters (CHS parameters)
		;mov	dl, [bsDriveNumber]
		;mov	dl, [bp+44]
		mov	ah, 08h
		int	13h
		jc	disk_io_error

		; CX = maximum value for cylinder and sector
		; DH = maximum value for head
		; DL = number of harddisks on first controller 			
		; ES:DI = address of hard disk parameters table
		; (Bits 6&7 of CL is high 2 bits of 10 bit clinder value
		; which is low 8 bits are in CH.)

		push	ds
		pop	es

		; convert CHS values to CHS limit (as LBA)
		movzx	eax, dh
		inc	ax
		mov	[bp+47], al	; [bs_Disk_Heads]
		;movzx	edx, cl
		mov	dl, cl
		;and	dl, 3Fh
		and	dx, 3Fh
		mov	[bp+46], dl	; [bs_Disk_SecPerTrack]
		mul	dx
		shr	cl, 6
		xchg	cl, ch
		inc	cx
		;movzx	ecx, cx ; *
		mul	ecx
		mov	[bp-4], eax ; dword [7BFCh] ; CHS limit

		; Load the second half (remain bytes) of this boot code
		; at 7E00h.

		mov	eax, [bp+12] ; [bsBeginSector]
		add	eax, 2 ; Second half of boot code is in BS 2
		;mov	ebx, 7E00h
		mov	bx, 7E00h
		;mov	cx, 1
		;call	disk_read
		call	read_sector ; (Read 1 sector)
		jc	short disk_io_error

		; Boot sector 2 validation check 
		cmp	word [7FA1h], 417 ; The magic word !
		je	check_startup_file_address

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
		jmp	getchar_reboot

print_msg:
		; DS:SI = Error message address (ASCIIZ string)	
		mov	ah, 0Eh
		mov	bx, 7
bs_03:
		lodsb
		test	al, al
		jz	short bs_04
		int	10h
		jmp	short bs_03
bs_04:
		retn


read_sector:	; 25/12/2017 (Read 1 sector)
		mov	cx, 1
disk_read:
		;mov	byte [bp+retry_count-7C00h], 4
		mov	dl, 4 ; retry count
disk_read_0:
		pushad
		mov	dl, 42h
		cmp	[bp+45], dl ; 42h ; TRFS1 LBA partition & LBA ready
		je	short lba_read
		; Jump to lba_read if sector addr overs CHS limit
		cmp	eax, [bp-4] ; CHS limit ([7BFCh])
		jb	short chs_read
		; Disk I/O error if Int 13h LBA read func is not usable
		; byte [bsjmpBoot+2] = 'LBA read function is ready' sign 
		;cmp	byte [bsjmpBoot+2], 42h ;  LBA availability
		;cmp	byte [bp+2], 42h
		cmp	[bp+2], dl ; 42h ; is LBA mode ready ? 
		je	short lba_read ; LBA mode is usable/available
		stc ; cf = 1
		retn
lba_read:
		;pushad

		;mov	di, sp
				
		push	dword 0
		push	eax
		push	es
		push	bx
		push 	byte 1
		push	byte 16 ; 10h
		mov	ah, 42h
		;mov	dl, [bsDriveNumber]
		mov	dl, [bp+44]
		mov	si, sp
		int	13h

		;pop	eax
		;pop	eax
		;pop	eax
		;pop	eax
		;mov	sp, di
		
		popa
		popa
		jmp	short disk_read_1
chs_read:	
		;pushad

		; Convert LBA to CHS
		xor	edx, edx
		;movzx	ecx, byte [bs_Disk_SecPerTrack] ; [bp+46]
				; sectors per track (17 or 63)
		movzx	ecx, byte [bp+46]
		div	ecx
		inc	dl	; sector number (1 based)
		push	dx
		mov	edx, eax ; (heads * cylinder) + head number
		shr	edx, 16	 ; high word in DX, low word in AX
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
		popad
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
		;add	bx, 512
		add	bh, 2 ; **
		inc	eax
		dec	cx
		jnz	short disk_read
		;clc 	; ** (128 sectors/cluster!?)
		retn

		db	07h
Diskio_err_Msg:
		db	0Dh, 0Ah
		db	'Disk I/O error'
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

		; Boot sector code writing date (by Erdogan Tan)
		db	4
		db	1
		db	'2018'
bsReserved1:
		dw	0

		; TRDOS 386 TRFS1 boot sector code version
		db	'Turkish Rational Singlix FS v1'
		dw	0
		db	'BOOT SECTOR'
                db      0
		times	510 - ($ - $$) db 0
bootsignature1:
		db	55h, 0AAh

bsReserved2:
		db	'RT'  ; 'Turkish Rational DOS' feature identifier


check_startup_file_address: 
		mov	eax, [bp+20] ; [bsStartupFDT]
		and	eax, eax
		jz	invalid_system_disk


		cmp	eax, [bp+16] ; [bsVolumeSize]
		jnb	invalid_system_disk

		add	eax, [bp+12] ; [bsBeginSector]
		mov	[bp-8], eax
		
		mov	bx, 8000h    ; FDT Buffer address
		mov	si, bx

		call	read_sector
		jc	disk_io_error

		; BX = 8200h

check_startup_file_fdt: 
		; Check FDT signature
		lodsw
		cmp	ax, 'FD' 
		jne	invalid_system_disk ; not a valid FDT!
		lodsb
		cmp	al, 'T'
		jne	invalid_system_disk ; not a valid FDT!
		add	si, 5
		lodsd	; FDT_FileNumber
		mov	edi, [bp+12] ; [bsBeginSector]
		mov	edx, [bp-8] ; Current FDT address (LBA) 
		add	edi, eax
		cmp	edi, edx
		jne	invalid_system_disk
		lodsd	; FDT_NextFDTNum
		add	eax, edx
		mov	[bp-8], eax  ; Next FDT address (LBA)
		; EDI = Current FDT address
		lodsd	; FDT_SectorCount
		or	eax, eax
		jz	invalid_system_disk ; not a valid FDT!
		push	eax ; *
		add	si, 8
		lodsd	; FDT_FileSize
		mov	edx, eax
		lodsw	; FDT_FileSizeHW
		movzx	eax, ax		
		mov	ecx, edx
		or	ecx, eax
		jz	invalid_system_disk ; not a valid FDT!	
		xchg	eax, edx
		mov	ecx, 511
		add	eax, ecx
		adc	edx, 0
		inc	ecx
		div	ecx
		mov	cx, 1151
		cmp	eax, ecx	; Maximum 1151 sectors
					; (Segment 1000h to 9FE0h)
					; ((512 bytes must be reserved for
					; stack just before segment A000h))	
		jna	short bs_05
		mov	eax, ecx
bs_05:
		; Load  RTS (Kernel) file
		push	eax
		; "Loading ..." message
                mov     si, Loading_Msg
		call    print_msg
		; "TRDOS386.SYS ..."
		mov	si, 8000h+40h ; FS1 File Name (<= 64 bytes)
		mov	byte [si+64], 0 ; Force max. 64 bytes file name.
		call    print_msg
		mov	si, tridot ; " ..."
		call    print_msg		
		pop	eax
		pop	ecx ; * ; sector count (in section)
load_startup_file:
		cmp	eax, ecx
		jnb	short bs_06
		mov	ecx, eax	; sector count (in section)
					; must not be greater
					; remain sectors (for file) to read
bs_06:
		sub	eax, ecx
		mov	[bp-12], eax	; Remain sector count (for next read)	
		mov	eax, edi ; FDT address
		inc	eax	 ; +1 (section data)
		; CX = Sector count (<= 1151) in section
		mov	bx, [next_segment]
		push	es
		mov	es, bx ; segment = 1000h +
		xor	bx, bx ; offset = 0 
		; CX = num of sectors to read (= sectors/cluster)
		call	disk_read
		pop	es
		;jc	diskio_error
		jc	short trdos_loading_error
		shr	bx, 4 ; from byte count to paragraph count
		add	[next_segment], bx

		mov	edi, [bp-12]
		or	edi, edi
		jz	bs_07
		
		mov	eax, [bp-8]	; Next FDT address
		mov	bx, 8000h	; FDT Buffer address
		mov	si, bx

		call	read_sector
		;jc	diskio_error
		jc	short trdos_loading_error
		
		lodsw
		cmp	ax, 'FD' 
		jne	invalid_system_disk ; not a valid FDT!
		lodsb
		cmp	al, 'T'
		jne	invalid_system_disk ; not a valid FDT!
		add	si, 9
		lodsd	; FDT_NextFDTNum
		add	eax, [bp+12] ; [bsBeginSector]
		mov	[bp-8], eax  ; Next FDT address (LBA)
		mov	edx, eax
		lodsd	; FDT_SectorCount
		or	eax, eax
		jz	invalid_system_disk ; not a valid FDT!
		;mov	ecx, eax ; sector count (in section)
		xchg	ecx, eax
		;mov	eax, edi ; [bp-12] ; remain sectors to read
		xchg	eax, edi
		mov	edi, edx
		; EDI = Current FDT address
		jmp	short load_startup_file

trdos_loading_error:
		mov	si, Load_err_Msg
		call	print_msg
		jmp	replace_disk

Loading_Msg:    db	0Dh, 0Ah
		db	'Loading ...'
                db 	0Dh, 0Ah, 0

Load_err_Msg:
		db	0Dh, 0Ah
		db	'Startup File Loading Error'
		;db	'!'
		db	0

		db	07h ; Filler

		; TRDOS 386 TRFS1 boot sector code version
		db	'Turkish Rational Singlix File System 1'
tridot:
		db	' ...', 0

bs_07:
		; Set TRDOS 386 kernel specific parameters (& signs)
		; and
		; Launch TRDOS 386 Kernel (Startup/RTS file)

		;mov	dl, [bsDriveNumber]
                mov	dx, [bp+44] ; DL = Drive number, DH = 41h (CHS)
						       ; DH = 42h (LBA)
   		
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
                mov     ds, bx
                mov     es, bx
		;mov	fs, bx
		;mov	gs, bx 

		;xor	ebx, ebx
		;xor	ecx, ecx
		;xor	edx, edx
		;xor	esi, esi
		;xor	edi, edi
		;xor	ebp, ebp

		; bp = 7C00h

		; NOTE: Offset 417 in boot sector 2 (the 2nd half of VBR)
		; is also TRFS1 boot record validation check address and 
		; boot sector 2 must have 417 here for boot sector 1 code
		; (the 1st half of volume boot record).
		; ((So, 'mov eax, 417' has double meaning here.))
loc_39F:                
                mov     eax, 417 ; TRDOS boot sector sign for TRDOS386.SYS

		jmp	rts_segment:0

next_segment:
		dw	rts_segment

		db	'TRFS1'

		times	1020 - ($ - $$) db 0
bsReserved3:
		db	'TR'  ; 'Turkish Rational DOS' feature identifier.
bootsignature2:
		db	55h, 0AAh