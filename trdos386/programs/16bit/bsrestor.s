; ****************************************************************************
; bsrestor.s (BSRESTOR.COM) - TRDOS 386 Harddisk Boot Sector Restoring Utility
; 						      (for MSDOS/WINDOWS)
; ****************************************************************************
; Last Update: 10/10/2020
; ----------------------------------------------------------------------------
; Beginning: 05/10/2020
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
; ----------------------------------------------------------------------------
; Purpose:
;	BSRESTOR.COM is used to restore previous boot sector of a primary
;	dos partition. (BSRESTOR.COM is used after BSSAVE.COM. It uses boot
;	sector backup file which will be created by BSSAVE.COM utility.)
;	((For example: BSSAVE is used before modifying windows boot sector
;	by using TRHDBOOT.COM, for TRDOS386 hard disk boot test. Then, 
;	BSRESTOR is used to restore windows boot sector after the test.))
; Backup File format: 
;	Binary file which contains MBR and Bootsector
;	of primary dos partition. First 512 bytes is the MBR of a harddisk
;	and remain 512 bytes (FAT fs) or 1024 bytes (FAT32 fs)
;	is the boot sector of same hard disk.					
; ****************************************************************************
; nasm bsrestor.s -l bsrestor.lst -o BSRESTOR.COM
; ----------------------------------------------------------------------------
; Derived from: bssave.s (BSSAVE.COM) by Erdogan Tan, 03/10/2020

; DTA (PSP+80h= Offset 128)
DTA_Attrib equ 149 ; PDP+21
DTA_Time equ 150 ; PSP+22
DTA_Date equ 152 ; PSP 24
DTA_FileSize equ 154 ; PSP + 26
DTA_FileName equ 158 ; PSP + 30

; Masterboot / Partition Table at Beginning+1BEh
ptBootable      equ 0
ptBeginHead     equ 1
ptBeginSector   equ 2
ptBeginCylinder equ 3
ptFileSystemID	equ 4
ptEndHead       equ 5
ptEndSector     equ 6
ptEndCylinder   equ 7
ptStartSector   equ 8
ptSectors       equ 12

; BIOS INT 13h Extensions (LBA extensions)
; Just After DP Data (DPDiskNumber+)
DAP_PacketSize equ 10h  ; If extensions present, this byte will be >=10h
DAP_Reserved1 equ 11h   ; Reserved Byte 
DAP_NumOfBlocks equ 12h ; Value of this byte must be 0 to 127
DAP_Reserved2 equ 13h   ; Reserved Byte
DAP_Destination equ 14h ; Address of Transfer Buffer as SEGMENT:OFFSET
DAP_LBA_Address equ 18h ; LBA=(C1*H0+H1)*S0+S1-1
                        ; C1= Selected Cylinder Number
                        ; H0= Number Of Heads (Maximum Head Number + 1)
                        ; H1= Selected Head Number
                        ; S0= Maximum Sector Number
                        ; S1= Selected Sector Number
                        ; QUAD WORD
; DAP_Flat_Destination equ 20h ; 64 bit address, if value in 4h is FFFF:FFFFh
                             ; QUAD WORD (Also, value in 0h must be 18h) 
                             ; TR-DOS will not use 64 bit Flat Address

pTableOffset equ 1BEh ; 446

[BITS 16]
[ORG 100h]

	;;cli
	;;cld
	;;push	cs
	;;pop	ss
	;;mov	sp, 0FFFEh
	;;sti
	;
	;;mov	bx, SizeOfFile+100
	;
	;mov	bx, bss_end
	;
        ;add	bx, 15
        ;shr	bx, 1
        ;shr	bx, 1
	;shr	bx, 1
	;shr	bx, 1
        ;mov	ah, 4Ah ; modify memory allocation
        ;;push	cs
        ;;pop	es
        ;int	21h

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; clear BSS
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	;mov	cx, bss_clear_end
	;
	;mov	di, bss_start
	;sub	cx, di
	;;inc	cx
	;shr	cx, 1
	;xor	ax, ax
	;rep	stosw 

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get command arguments (command tail)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, 80h			; PSP command tail
 	lodsb
	or	al, al 			; command tail length                            
	jz	short _03		; jump if zero
_01:
	lodsb
	cmp	al, ' '			; is it SPACE ?
	je	short _01 		
	jb	short _03

	; check disk name

	cmp	al, 'h'
	jne	short _03	
	cmp	byte [si], 'd'
	jne	short _03
	inc	si
	lodsb
	cmp	al, '0'
	je	short _02
	jb	short _03
	cmp	al, '3'
	ja	short _03
_02:
	cmp	byte [si], ' '
	je	short _04
_03:
	mov	si, TrDOS_Welcome
	jmp	_43
_04:
	inc	si
	add	al, 80h - '0'
	mov	[DrvNum], al	; 80h .. 83h
_05:
	lodsb
	cmp	al, ' '
	je	short _05
	jb	short _03

	; check backup file name
_06:
       	mov	di, bs_file_name
	stosb
_07:
	lodsb
	;cmp	al, 0Dh ; ENTER (CR) key
	cmp	al, 20h ; ' '
	jna	short _08
	stosb
	cmp	di, bs_file_name + 12
	jb	short _07
	cmp	byte [si], 20h 
	ja	short _14
_08:
	sub	al, al
	stosb

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; File name capitalization
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, bs_file_name
	mov	di, si
	mov	bx, si
_09:
	lodsb
	cmp	al, 'a'
	jnb	short _11
	and	al, al
	jz	short _12
	cmp	al, '.'
	jne	short _10
	mov	bx, di ; dot position	
_10:
	stosb
	jmp	short _09 		
_11:
	cmp	al, 'z'
	ja	short _10
	and	al, 0DFh ; NOT 32
	stosb
	jmp	short _09	
_12:
	mov	[di], al
	dec	di
	cmp	bx, di
	jnb	short _14
	sub	di, bx
	sub	bx, bs_file_name
	cmp	di, 3
	jna	short _13
	and	bx, bx
	jnz	short _14
	jmp	short _15		
_13:
	cmp	bx, 8
	jna	short _15
_14:
	mov	si, msg_inv_file_name
	jmp	_43

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Find boot sector backup file
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	
_15:
	mov	dx, bs_file_name
	mov	cx, 3Fh ; File Attributes
	mov	ah, 4Eh ; MS-DOS Function = Find First File
	int	21h
	jnc	short _16

	; bs backup file not found in working/current directory
	mov	si, msg_file_notfound
	jmp	_43

_16:

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Check bs backup file features
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, DTA_Attrib
	mov	al, [si]
	;and	al, 1Fh ; directory, volume label, system, hidden, read only
	and	al, 1Eh ; directory, volume label, system, hidden
	jnz	short _18     
	mov	si, DTA_FileSize
	lodsw
	mov	dx, [si]
	or	ax, ax 
	jz	short _18 ; zero file size (invalid backup file)

	and	dx, dx	
	jnz	short _18 ; wrong file size (>1536 bytes)

	cmp	ax, 1024
	je	short _19 ; correct file size (for FAT16 & FAT12 boot sector)

	mov	byte [fat32], 1

	cmp	ax, 1536
	je	short _19 ; correct file size (only for FAT32 boot sectors)	

	; invalid backup file !
_18:
	mov	si, msg_inv_backup_file
	jmp	_43

_19:

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Next line
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	;mov	si, CRLF
	;call	print_msg

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Read	masterboot sector (MBR)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	di, 5

	;mov	ax, 0201h		; read disk
	mov	bx, MasterBootBuff	; location of masterboot code

	mov	cx, 1			; cylinder = 0
					; sector = 1
	mov	dh, 0			; head = 0
	mov	dl, [DrvNum]		; drive number, 80h .. 83h
_20:
	mov	ax, 0201h
	int	13h
	jnc	short _21		; read masterboot sector, OK
	
 	; reset hard disk(s)
	xor	ah, ah
	;mov	dl, [drv]
	int	13h

	;dec	byte [RetryCount]
	dec	di
	jnz	short _20

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; write disk error message and terminate
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, msg_disk_not_ready_error
	jmp	_43
_21:

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Check MBR then read MBR & BS
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	cmp 	word [MBIDCode], 0AA55h
	je	short _24 ; Valid MBR	

	; invalid MBR !

	mov	si, msg_inv_mbr
	jmp	_43
_22:
	; Check fat32 boot sectors flag (for backup file)
	cmp	byte [fat32], 1 ; 2 boot sectors for FAT32 fs
	jb	short _18  ; Invalid bs backup file !
	jmp	short _29
_23:
	; Check fat32 boot sector(s) flag (for backup file)
	cmp	byte [fat32], 1 ; 1 bs for FAT16 & FAT12 fs
	jnb	short _18  ; Invalid bs backup file !
	jmp	short _29
_24:
	; check if MBR contains primary DOS partition or not
	mov	si, MasterBootBuff+pTableOffset
_25:
	mov	al, [si+ptFileSystemID]
	;xor	ah, ah	; LBA = 0
	cmp	al, 0Bh ; FAT32 CHS
	jb	short _27
	je	short _22
	;inc	ah	; LBA = 1
	cmp	al, 0Ch ; FAT32 LBA
	je	short _22
	cmp	al, 0Eh ; FAT16 LBA
	je	short _23
_26:
	cmp	si, MasterBootBuff+pTableOffset+64
	jnb	short _28
	add	si, 16
	jmp	short _25
_27:
	cmp	al, 06h ; FAT16 CHS big
	ja	short _26
	je	short _23
	cmp	al, 04h	; FAT16 CHS
	je	short _23
	cmp	al, 01h	; FAT12	
	jne	short _26
	jmp	short _23	
_28:
	; MBR does not contain primary DOS partition
	mov	si, msg_dosp_notfound
	jmp	_43
_29:
	;mov	[fsID], al ; file system type

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; copy partition table row which is for primary dos partition
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	di, PTrow
	;mov	cx, 8 
	mov	cl, 8
	rep	movsw

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; read MBR in the backup file and compare it with disk's MBR
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	ax, 3D00h ; open file for read
	mov	dx, bs_file_name
	int	21h
	jc	short _31
_30:
	;mov	[bs_file_handle], ax
	mov	bx, ax
	mov	cx, [DTA_FileSize] ; read all bytes of the file
	;mov	ax, 3F00h ; read from file
	mov	ah, 3Fh
	mov	dx, MasterBootBuff 
	int	21h

	pushf
	mov	ah, 3Eh ; close file
	;mov	bx, [bs_file_handle]
	int	21h
	popf
	jnc	short _32
_31:
	mov	si, msg_file_read_error
	jmp	_43
_32:
	; Check file's MBR
	
	cmp 	word [MBIDCode], 0AA55h
	je	short _34 ; Valid MBR	
_33:
	; invalid MBR !
	mov	si, msg_inv_file_mbr
	jmp	_43
_34:
	;mov	al, [fsID]

	mov	si, PTrow
	mov	di, MasterBootBuff+pTableOffset
	
	mov	al, [si+ptFileSystemID]
	cmp	al, [di+ptFileSystemID]
	jne	short _33

	; Check boot sector signature

	mov	bx, BootSectorBuff

	cmp	word [bx+510], 0AA55h
	je	short _35

  	; Invalid boot sector in file
	mov	si, msg_inv_file_bs
	jmp	short _43

_35:
	mov	ax, [di+ptStartSector]
	mov	dx, [di+ptStartSector+2]

	cmp	ax, [si+ptStartSector]
	jne	short _33

	cmp	dx, [si+ptStartSector+2]
	jne	short _33

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Disk write (bs overwrite) question and get the answer (Y/N/ESC)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, msg_overwrite_question
	call	print_msg
_36:
	xor	ax, ax
	int	16h			; wait for keyboard command
	cmp	al, 'y'
	je	short _38		; retry
	cmp	al, 'Y'
	je	short _38
	cmp	al, 'n'
	je	short _37 		; exit
	cmp	al, 'N'
	je	short _37
	cmp	al, 'C'-40h
	je	short _41              
	cmp	al, 27
	jne	short _36
_37:
	mov	si, _NO
	jmp	short _43
_38:
	mov	si, _YES
	call	print_msg

	mov	si, msg_writing_bs
	call	print_msg

	mov	ax, [di+ptStartSector]
	;mov	dx, [di+ptStartSector+2]
	mov	bx, BootSectorBuff
_39:

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Write boot sector(s) to disk
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	call	lba_write
	jc	short _40

	cmp	byte [fat32], 1
	jne	short _42
	add	ax, 2	; Second part of 1024 bytes boot sector
	adc	dx, 0	; just after FSINFO sector  
	mov	bx, BootSectorBuff+512
	mov	byte [fat32], 2
	jmp	short _39
_40:
	; disk write error
	mov	si, CRLF
	call	print_msg
	mov	si, msg_disk_write_error
	jmp	short _43
_41:
	; nothing to do !
	mov	si, CRLF 
	jmp	short _43
_42:
	; disk (bs) writing is OK.
	mov	si, msg_OK
_43:
	call	print_msg

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Exit
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	ax, 4C00h		; terminate
	int	21h
_44:
	jmp	short _44

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; LBA read (read one sector)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;lba_read:
;	mov	[rw], 42h
;	jmp	short lba_rw

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; LBA write (write one sector)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

lba_write:
	;mov	byte [rw], 43h
	;jmp	short lba_rw

;lba_read:
lba_rw:
	mov	di, 5
lba_rw_1:
	;pusha				; db 60h
	db	60h
	;push 	0                       ; db 6Ah, 00h
	db	6Ah, 0
	;push	0                       ; db 6Ah, 00h
	db	6Ah, 0
	push    dx
	push    ax
	push    es
	push    bx
	;push	1			; db 6Ah, 01h
	db	6Ah, 01h                     
	;push	10h                     ; db 6Ah, 10h
	db	6Ah, 10h

	mov     si, sp
	mov     dl, [DrvNum]
	xor	al, al	; verify off (for LBA write)
lba_rw_2:
	;mov	ah, [rw] ; LBA read/write
	;xor	al, al	; verify off
	mov	ah, 43h
	int     13h

	;mov	[error], ah
	jnc     short lba_rw_3

	dec	di                 
	jz	short lba_rw_3 
        
	xor	ah, ah                   
	;mov	dl, [DrvNum]
	int	13h	; BIOS Service func (ah) = 0
			; Reset disk system

	;mov	word [si+2], 1 ; set r/w count to 1 again
	mov	byte [si+2], 1

	jmp	short lba_rw_2

lba_rw_3:
	;popa
	db	61h
	;popa
	db	61h

_retn:
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; print message
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_msg:
	lodsb				; Load byte at DS:SI to AL
	and	al, al            
	jz	short _retn      
	mov	ah, 0Eh			
	mov	bx, 07h             
	int	10h			; BIOS Service func ( ah ) = 0Eh
					; Write char as TTY
					; AL-char BH-page BL-color
	jmp     short print_msg          

;=============================================================================
;        	initialized data
;=============================================================================

;rw:	db	42h	; LBA read (default)

DrvNum:
	db	0
fat32:
	db	0

;align 2

;bs_file_handle:
;	dw	0

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  Messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TrDOS_Welcome:
	db	0Dh, 0Ah
	db	"Primary DOS Partition Boot Sector Restore Utility for TR-DOS 386"
	db	0Dh, 0Ah
	db	"v1.0.101020 (c) Erdogan TAN 2020"
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"Usage: bsrestor <disk drive name> <backup file name>"
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"Disk drive names: "
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	" hd0 ..for primary dos partition on 1st disk "
	db	0Dh, 0Ah
	db	" hd1 ..for primary dos partition on 2nd disk "
	db	0Dh, 0Ah
	db	" hd2 ..for primary dos partition on 3rd disk "
	db	0Dh, 0Ah
	db	" hd3 ..for primary dos partition on 4th disk "
	db	0Dh, 0Ah, 0Dh, 0Ah
	db	"Example: bsrestor hd0 bsbackup.bin "
	db	0Dh, 0Ah, 0

msg_inv_file_name: 
	db	0Dh, 0Ah
	db	"Invalid file name !", 0Dh, 0Ah
	db	"(File name must fit to 8.3 DOS format) !"
	db	0Dh, 0Ah, 0

msg_dosp_notfound:
	db	0Dh, 0Ah
	db	"MBR does not contain a primary DOS partition !"	
	db	0Dh, 0Ah
	db	0

msg_inv_mbr:
	db	0Dh, 0Ah
	db	"Invalid MBR !"	
	db	0Dh, 0Ah, 0

msg_overwrite_question:
	db	0Dh, 0Ah
	db	"Do you want to overwrite boot sector ? "
	db	0

_YES:	db	"YES"
	db	0Dh, 0Ah, 0

_NO:	db	"NO"
	db	0Dh, 0Ah, 0

msg_writing_bs:
	db	0Dh, 0Ah
	db	"Writing primary dos partition's boot sector ... "
	db	0
msg_OK:
	db	' OK.'
CRLF:
	db	0Dh, 0Ah, 0

msg_file_notfound:
	db	0Dh, 0Ah
	db	"Backup file not found !"
	db	0Dh, 0Ah, 0

msg_file_read_error:
	db	0Dh, 0Ah
	db	"File reading error !"
	db	0Dh, 0Ah, 0

msg_disk_not_ready_error:
	db	0Dh, 0Ah
	db	"Disk read error or drive not ready !"
	db	0Dh, 0Ah, 0

msg_disk_write_error:
	db	0Dh, 0Ah
	db	"Disk write error !"
	db	0Dh, 0Ah, 0

msg_inv_backup_file:
	db	0Dh, 0Ah
	db	"Invalid backup file !", 0Dh, 0Ah
	db	"(Improper file attributes or wrong file size) !"
	db	0Dh, 0Ah, 0

msg_inv_file_mbr:
	db	0Dh, 0Ah
	db	"Invalid backup file !", 0Dh, 0Ah
	db	"(Improper/Different/Invalid MBR) !"	
	db	0Dh, 0Ah, 0

msg_inv_file_bs:
	db	0Dh, 0Ah
	db	"Invalid backup file !", 0Dh, 0Ah
	db	"(Invalid BS) !"	
	db	0Dh, 0Ah, 0

SizeOfFile equ $-100

;=============================================================================
;        	uninitialized data
;=============================================================================

bss_start:

ABSOLUTE bss_start

alignb 2

bs_file_name:  
	resb	13
	resb	1 ; word alignment

bss_clear_end:

PTrow:
	resb	16

;alignb 2

; Masterboot sector (MBR)

MasterBootBuff:
MasterBootCode: 
	resb	446 
PartitionTable:
	resb	64
MBIDCode:
	resw	1

BootSectorBuff:
	resb	512
BootSectorBuff2:
	resb	512	; FAT32 fs boot sector buffer, 2nd part

bss_end:	 	