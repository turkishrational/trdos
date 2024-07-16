; ****************************************************************************
; mbsave.s (MBSAVE.COM) - TRDOS 386 Harddisk MBR Saving/Backup Utility
; 						   (for MSDOS/WINDOWS)
; ****************************************************************************
; Last Update: 11/10/2020
; ----------------------------------------------------------------------------
; Beginning: 03/10/2020
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
; ----------------------------------------------------------------------------
; Purpose: To save masterboot sector (MBR) to a file before modifying it.
; ****************************************************************************
; nasm mbsave.s -l mbsave.lst -o MBSAVE.COM
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


; Known partition types

;FileSys_Names: ; 2003-2017
;; (Valid FileSystems for TRDOS 386, SINGLIX, RETRO UNIX OS projects in 2017)
FS_FAT12	equ 1		; 01h = FAT12
FS_XENIX	equ 2		; 02h , XENIX System V root
FS_XENIX_USR	equ 3		; 03h , XENIX System V user
FS_FAT16	equ 4		; 04h = FAT16 < 32MB
FS_EXT_CHS	equ 5		; 05h = Extended DOS Partition
FS_FAT16_BIG	equ 6		; 06h = FAT16 > 32MB, CHS mode
FS_NTFS		equ 7		; 07h , WINDOWS NTFS Partition
FS_FAT32_CHS	equ 11		; 0Bh = FAT32, CHS mode
FS_FAT32_LBA	equ 12		; 0Ch = FAT32, LBA mode
FS_FAT16_LBA	equ 14		; 0Eh = FAT16, LBA mode
FS_EXT_LBA	equ 15		; 0Fh = Extented Partition, LBA mode
FS_UNIX_SYSV	equ 99		; 63h , SCO UNIX, UNIXWARE, OPENSERVER
FS_RETROUNIX	equ 113		; 71h , Retro UNIX 386 v2 Partition
FS_UNIX_V7	equ 114		; 72h , UNIX v7 x86 Partition  
FS_LINUXSWAP	equ 139		; 82h , LINUX SWAP Partition
FS_LINUX	equ 131		; 83h , LINUX NATIVE (ext2) Partition
FS_LINUXEXT	equ 133		; 85h , LINUX EXTENDED Partition
FS_TRDD		equ 160		; A0h , (Random Data Disk) LBA
FS_TRFS		equ 161		; A1h , (32 bit, 512 bytes per sector)
 
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
	call	print_msg

	jmp	_29
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
       	mov	di, mbr_file_name
	stosb
_07:
	lodsb
	;cmp	al, 0Dh ; ENTER (CR) key
	cmp	al, 20h ; ' '
	jna	short _08
	stosb
	cmp	di, mbr_file_name + 12
	jb	short _07
	cmp	byte [si], 20h 
	ja	short _14
_08:
	sub	al, al
	stosb

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; File name capitalization
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, mbr_file_name
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
	sub	bx, mbr_file_name
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
	call	print_msg
	jmp	_29

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Find masterboot record/sector (MBR) backup file
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	
_15:
	mov	dx, mbr_file_name
	mov	cx, 3Fh ; File Attributes
	mov	ah, 4Eh ; MS-DOS Function = Find First File
	int	21h
	jc	short _20

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Check mbr backup file features
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, DTA_Attrib
	mov	al, [si]
	and	al, 1Fh ; directory, volume label, system, hidden, read only
	jnz	short _17     
	mov	si, DTA_FileSize
	lodsw
	mov	dx, [si]
	or	dx, ax 
	jz	short _20 ; zero file size (do not display owr question)	

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Display file overwrite question and get the answer (Y/N/ESC)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, msg_overwrite_question1
	call	print_msg

	mov	si, mbr_file_name
	call	print_msg

	mov	si, msg_overwrite_question2
	call	print_msg

_16:
	xor	ax, ax
	int	16h			; wait for keyboard command
	cmp	al, 'y'
	je	short _19		; retry
	cmp	al, 'Y'
	je	short _19
	cmp	al, 'n'
	je	short _18 		; exit
	cmp	al, 'N'
	je	short _18
	cmp	al, 'C'-40h
	je	_29              
	cmp	al, 27
	jne	short _16
	jmp	_29

	; invalid backup file !
_17:
	mov	si, msg_inv_backup_file
	jmp	_32
_18:
	mov	si, _NO
	call	print_msg
	jmp	_29
_19:
	mov	si, _YES
	call	print_msg

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Next line
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, CRLF
	call	print_msg

_20:

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Read masterboot sector (MBR)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	di, 5

	;mov	ax, 0201h		; read disk
	mov	bx, MasterBootBuff	; location of masterboot code

	mov	cx, 1			; cylinder = 0
					; sector = 1
	mov	dh, 0			; head = 0
	mov	dl, [DrvNum]		; drive number, 80h .. 83h
_21:
	mov	ax, 0201h
	int	13h
	jnc	short _22		; read masterboot sector, OK
	
 	; reset hard disk(s)
	xor	ah, ah
	;mov	dl, [drv]
	int	13h

	;dec	byte [RetryCount]
	dec	di
	jnz	short _21

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; write disk error message and terminate
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, msg_disk_not_ready_error
	jmp	_32

_22:

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Check MBR if it is valid and it contains a known partition
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	cmp 	word [MBIDCode], 0AA55h
	je	short _23 ; Valid MBR	

	; invalid MBR !

	mov	si, msg_inv_mbr
	jmp	short _32

_23:
	; check if MBR contains a known type of partition or not

	mov	si, MasterBootBuff+pTableOffset
_24:
	mov	bx, known_partitions
	mov	cx, kpc-known_partitions
_25:
	mov	al, [si+ptFileSystemID]
	or	al, al
	jz	short _27
_26:	
	cmp	al, [bx]
	je	short _30
	dec	cx
	jz	short _27
	inc	bx
	jmp	short _26
_27:
	cmp	si, MasterBootBuff+pTableOffset+64
	jnb	short _28
	add	si, 16
	jmp	short _24
_28:
	; MBR does not contain a known partition type
	mov	si, msg_kp_notfound
	jmp	short _32
_29:
	mov	si, CRLF
	jmp	short _32

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Create new file or overwrite/truncate existing file
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

_30:
	xor	cx, cx  ; 0 ; Regular file with write permission
	mov	dx, mbr_file_name
	mov	ax, 3C00h ; create a file
	int	21h
	jc	short _34

	;mov	[bs_file_handle], ax
	mov	cx, ax

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Writing MBR to the backup file
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, msg_writing_file
	call	print_msg

	;mov	bx, [bs_file_handle]
	mov	bx, cx
	mov	cx, 512 ; MBR size in bytes
	mov	dx, MasterBootBuff
	mov	ah, 40h	; write to file	
	int	21h

	pushf
	mov	ah, 3Eh ; close file
	;mov	bx, [bs_file_handle]
	int	21h
	popf
	jnc	short _31

	; Masterboot sector backup file writing error !
	mov	si, msg_file_write_error
	jmp	short _32
_31:
	mov	si, msg_OK
_32:
	call	print_msg

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Exit
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	ax, 4C00h		; terminate
	int	21h
_33:
	jmp	short _33
_34:
	mov	si, msg_file_create_error
	jmp	short _32

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; print message
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

_35:
	retn

print_msg:
	lodsb				; Load byte at DS:SI to AL
	and	al, al            
	jz	short _35     
	mov	ah, 0Eh			
	mov	bx, 07h             
	int	10h			; BIOS Service func ( ah ) = 0Eh
					; Write char as TTY
					; AL-char BH-page BL-color
	jmp     short print_msg          

;=============================================================================
;        	initialized data
;=============================================================================

known_partitions:
	      db FS_FAT12, FS_XENIX, FS_XENIX_USR, FS_FAT16, FS_EXT_CHS
	      db FS_FAT16_BIG, FS_NTFS, FS_FAT32_CHS, FS_FAT32_LBA	
	      db FS_FAT16_LBA, FS_EXT_LBA, FS_UNIX_SYSV, FS_RETROUNIX 	
	      db FS_UNIX_V7, FS_LINUXSWAP, FS_LINUX, FS_LINUXEXT
	      db FS_TRDD, FS_TRFS

kpc:	db	0			
 
DrvNum:
	db	0

;align 2

;bs_file_handle:
;	dw	0

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  Messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TrDOS_Welcome:
	db	0Dh, 0Ah
	db	"MasterBoot Record/Sector Backup Utility for TR-DOS 386"
	db	0Dh, 0Ah
	db	"v1.0.111020 (c) Erdogan TAN 2020"
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"Usage: mbsave <disk drive name> <backup file name>"
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"Disk drive names: "
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	" hd0 ..for MBR of 1st hard disk "
	db	0Dh, 0Ah
	db	" hd1 ..for MBR of 2nd hard disk "
	db	0Dh, 0Ah
	db	" hd2 ..for MBR of 3rd hard disk "
	db	0Dh, 0Ah
	db	" hd3 ..for MBR of 4th hard disk "
	db	0Dh, 0Ah, 0Dh, 0Ah
	db	"Example: mbsave hd0 mbsector.bin "
	db	0Dh, 0Ah, 0

msg_inv_file_name: 
	db	0Dh, 0Ah
	db	"Invalid file name !", 0Dh, 0Ah
	db	"(File name must fit to 8.3 DOS format) !"
	db	0Dh, 0Ah, 0

msg_kp_notfound:
	db	0Dh, 0Ah
	db	'MBR does not contain a known partition type ! '	
	db	0Dh, 0Ah
	db	0

msg_inv_mbr:
	db	0Dh, 0Ah
	db	'Invalid MBR ! '	
	db	0Dh, 0Ah, 0

msg_overwrite_question1:
	db	0Dh, 0Ah
	db	'Do you want to overwrite '
	db	27h
	db	0

msg_overwrite_question2: 
	db	27h
	db	' file ? '
	db	0

_YES:	db	'YES'
	;db	0Dh, 0Ah, 0
	db	0

_NO:	db	'NO'
	;db	0Dh, 0Ah, 0
	db	0

msg_writing_file:
	db	0Dh, 0Ah
	db	'Writing file ... '
	db	0
msg_OK:
	db	' OK.'
CRLF:
	db	0Dh, 0Ah, 0

msg_file_create_error:
	db	0Dh, 0Ah
	db	"File creating error !"
	db	0Dh, 0Ah, 0

msg_file_write_error:
	db	0Dh, 0Ah
	db	"File writing error !"
	db	0Dh, 0Ah, 0

msg_disk_not_ready_error:
	db	0Dh, 0Ah
	db	"Disk read error or drive not ready ! "
	db	0Dh, 0Ah, 0

msg_inv_backup_file:
	db	0Dh, 0Ah
	db	"Invalid backup file name !", 0Dh, 0Ah
	db	"(Improper file attributes) !"
	db	0Dh, 0Ah, 0

SizeOfFile equ $-100

;=============================================================================
;        	uninitialized data
;=============================================================================

bss_start:

ABSOLUTE bss_start

alignb 2

mbr_file_name:  
	resb	13
	resb	1 ; word alignment

bss_clear_end:

;alignb 2

; Masterboot sector (MBR)

MasterBootBuff:
MasterBootCode: 
	resb	446 
PartitionTable:
	resb	64
MBIDCode:
	resw	1

bss_end:	 	