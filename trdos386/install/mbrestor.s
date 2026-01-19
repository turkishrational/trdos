; ****************************************************************************
; mbrestor.s (MBRESTOR.COM) - TRDOS 386 Harddisk MBR Restore Utility
; 						   (for MSDOS/WINDOWS)
; ****************************************************************************
; Last Update: 11/10/2020
; ----------------------------------------------------------------------------
; Beginning: 10/10/2020
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
; ----------------------------------------------------------------------------
; Purpose: 
;	To restore masterboot sector (MBR) from a file after a modification.
;	(MBSAVE.COM is used to save masterboot sector before using MBRESTOR)
; ****************************************************************************
; nasm mbrestor.s -l mbrestor.lst -o MBRESTOR.COM
; ----------------------------------------------------------------------------
; Derived from: mbsave.s & bsrestor.s by Erdogan Tan, 10/10/2020

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
	jz	short _06		; jump if zero
_01:
	lodsb
	cmp	al, ' '			; is it SPACE ?
	je	short _01 		
	jb	short _06

	mov	byte [option], 'a' ; reset to all (code and pt)

	; check option character ("-")
	cmp	al, '-'
	jne	short _04

	lodsb
	mov	byte [option], 'c' ; set to code
	cmp	al, 'c'
	je	short _02
	cmp	al, 't'
	jne	short _06
	mov	byte [option], 't'  ; set to partition table
_02:
	cmp	byte [si], ' '  ; space
	jne	short _06
_03:
	lodsb
	cmp	al, 20h ; space	
	je	short _03
	jb	short _06
_04:
	; check disk name

	cmp	al, 'h'
	jne	short _06	
	cmp	byte [si], 'd'
	jne	short _06
	inc	si
	lodsb
	cmp	al, '0'
	je	short _05
	jb	short _06
	cmp	al, '3'
	ja	short _06
_05:
	cmp	byte [si], ' '
	je	short _07
_06:
	mov	si, TrDOS_Welcome
	jmp	_52
_07:
	inc	si
	add	al, 80h - '0'
	mov	[DrvNum], al	; 80h .. 83h
_08:
	lodsb
	cmp	al, ' '
	je	short _08
	jb	short _06

	; check backup file name
_09:
       	mov	di, mbr_file_name
	stosb
_10:
	lodsb
	;cmp	al, 0Dh ; ENTER (CR) key
	cmp	al, 20h ; ' '
	jna	short _11
	stosb
	cmp	di, mbr_file_name + 12
	jb	short _10
	cmp	byte [si], 20h 
	ja	short _17
_11:
	sub	al, al
	stosb

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; File name capitalization
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, mbr_file_name
	mov	di, si
	mov	bx, si
_12:
	lodsb
	cmp	al, 'a'
	jnb	short _14
	and	al, al
	jz	short _15
	cmp	al, '.'
	jne	short _13
	mov	bx, di ; dot position	
_13:
	stosb
	jmp	short _12 		
_14:
	cmp	al, 'z'
	ja	short _13
	and	al, 0DFh ; NOT 32
	stosb
	jmp	short _12	
_15:
	mov	[di], al
	dec	di
	cmp	bx, di
	jnb	short _17
	sub	di, bx
	sub	bx, mbr_file_name
	cmp	di, 3
	jna	short _16
	and	bx, bx
	jnz	short _17
	jmp	short _18		
_16:
	cmp	bx, 8
	jna	short _18
_17:
	mov	si, msg_inv_file_name
	jmp	_52

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Find masterboot record/sector (MBR) backup file
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	
_18:
	mov	dx, mbr_file_name
	mov	cx, 3Fh ; File Attributes
	mov	ah, 4Eh ; MS-DOS Function = Find First File
	int	21h
	jnc	short _19

	; mbr backup file not found in working/current directory
	mov	si, msg_file_notfound
	jmp	_52

_19:

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Check mbr backup file features
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, DTA_Attrib
	mov	al, [si]
	;and	al, 1Fh ; directory, volume label, system, hidden, read only
	and	al, 1Eh ; directory, volume label, system, hidden
	jnz	short _17     
	mov	si, DTA_FileSize
	lodsw
	mov	dx, [si]
	or	ax, ax 
	jz	short _20 ; zero file size (invalid backup file)

	and	dx, dx	
	jnz	short _20 ; wrong file size (>1536 bytes)

	cmp	ax, 512	  ; MBSAVE.COM	
	je	short _21 ; correct file size (for masterboot sector)

	cmp	ax, 1024  ; BSSAVE.COM
	je	short _21 ; correct file size (for MBR + FAT boot sector)

	cmp	ax, 1536  ; BSSAVE.COM
	je	short _21 ; correct file size (for MBR + FAT32 boot sectors)	

	; invalid backup file !
_20:
	mov	si, msg_inv_backup_file
	jmp	_52	

_21:

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
_22:
	mov	ax, 0201h
	int	13h
	jnc	short _23		; read masterboot sector, OK
	
 	; reset hard disk(s)
	xor	ah, ah
	;mov	dl, [drv]
	int	13h

	;dec	byte [RetryCount]
	dec	di
	jnz	short _22

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; write disk error message and terminate
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, msg_disk_not_ready_error
	jmp	_52

_23:

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Check MBR if it is valid and it contains a known partition
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	cmp 	word [MBIDCode], 0AA55h
	jne	short _25 ; invalid MBR	

	; check if MBR contains a known type of partition or not
	; (also check bootable partition indicator for 0 or 80h)

	mov	si, MasterBootBuff+pTableOffset
_24:
	mov	bx, known_partitions
	mov	cx, kpc-known_partitions

	test	byte [si], 7Fh
	jz	short _26  ; valid indicator
_25:
	; invalid MBR ! MBR code and PT must be restored from file
	; (also if the PT does not contain a known partition type)
	mov	byte [option], 'e'
	jmp	short _29
_26:
	mov	al, [si+ptFileSystemID]
	or	al, al
	jz	short _28
_27:	
	cmp	al, [bx]
	je	short _29
	dec	cx
	jz	short _28
	inc	bx
	jmp	short _27
_28:
	cmp	si, MasterBootBuff+pTableOffset+64
	jnb	short _25
	add	si, 16
	jmp	short _24
_29:

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; save the disk's MBR if it's partition table is valid
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	cmp	byte [option], 'e'  ; invalid partition table ?
	je	short _30	    ; yes (full restore is needed)			

	mov	si, MasterBootBuff  ; R/W Buffer 
	mov	di, MBR ; Backup
	mov	cx, 256 
	rep	movsw
_30:

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; read MBR in the backup file and check it
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	ax, 3D00h ; open file for read
	mov	dx, mbr_file_name
	int	21h
	jc	short _32
_31:
	;mov	[mbr_file_handle], ax
	mov	bx, ax
	mov	cx, 512 ; read masterboot sector (512 bytes)
	;mov	ax, 3F00h ; read from file
	mov	ah, 3Fh
	mov	dx, MasterBootBuff 
	int	21h

	pushf
	mov	ah, 3Eh ; close file
	;mov	bx, [mbr_file_handle]
	int	21h
	popf
	jnc	short _33
_32:
	mov	si, msg_file_read_error
	jmp	_52
_33:
	; Check file's MBR
	
	cmp 	word [MBIDCode], 0AA55h
	je	short _35 ; Valid MBR	
_34:
	; invalid MBR !
	mov	si, msg_inv_file_mbr
	jmp	_52
_35:
	; check if MBR contains a known type of partition or not
	; (also check bootable partition indicator for 0 or 80h)

	mov	si, MasterBootBuff+pTableOffset
_36:
	mov	bx, known_partitions
	mov	cx, kpc-known_partitions

	test	byte [si], 7Fh
	jz	short _38  ; valid indicator
_37:
	; invalid MBR 
	; (MBR does not contain a known partition type)
	mov	si, msg_kp_notfound
	jmp	_52
_38:
	mov	al, [si+ptFileSystemID]
	or	al, al
	jz	short _40
_39:	
	cmp	al, [bx]
	je	short _41
	dec	cx
	jz	short _40
	inc	bx
	jmp	short _39
_40:
	cmp	si, MasterBootBuff+pTableOffset+64
	jnb	short _37
	add	si, 16
	jmp	short _36
_41:

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; check (present) active partition's boot sector
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	cmp	byte [option], 'c' ; MBR code only
	je	short _43	; MBR code will be overwritten

	cmp	byte [option], 'e' 
				; Invalid MBR, will be overwritten
	je	short _43

	mov	si, MBR+pTableOffset
	call	check_active_p_bs
	jc	short _42

	mov	byte [oldpbs], 1
_42:

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; verify boot sector of the active partition (or dos partition)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, MasterBootBuff+pTableOffset
	call	check_active_p_bs
	jnc	short _43

	cmp	byte [oldpbs], 1
	jb	short _43	

	mov	si, msg_file_pbsv_error
	jmp	_52
_43:

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; MBR overwrite question
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	
	mov	si, msg_overwrite_question
	call	print_msg
_44:
	xor	ax, ax
	int	16h			; wait for keyboard command
	cmp	al, 'y'
	je	short _46		; retry
	cmp	al, 'Y'
	je	short _46
	cmp	al, 'n'
	je	short _45 		; exit
	cmp	al, 'N'
	je	short _45
	cmp	al, 'C'-40h
	je	short _50             
	cmp	al, 27
	jne	short _52
_45:
	mov	si, _NO
	jmp	short _52
_46:
	mov	si, _YES
	call	print_msg

	mov	si, msg_writing_mbr
	call	print_msg

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; set masterboot buffer according to the selected option
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	al, [option]

	cmp	al, 'a'
	je	short _48

	cmp	al, 'e'
	je	short _48

	cmp	al, 'c'
	jne	short _47

	; [option] = 'c' ; restore MBR (bs loader) code only
	; (old partition table will not be overwritten)
	mov	si, MBR+pTableOffset
	mov	di, MasterBootBuff+pTableOffset
	mov	cx, 32
	rep	movsw
	;;mov	word [MasterBootBuff+510], 0AA55h
	jmp	short _48
_47:
	; [option] = 't' ; restore partition table only
	; (old MBR code will not be overwritten)
	mov	si, MBR
	mov	di, MasterBootBuff
	mov	cx, pTableOffset ; 446
	rep	movsb
	;mov	word [MasterBootBuff+510], 0AA55h	
_48:

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Write masterboot sector to disk
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	di, 5

	;mov	ax, 0301h		; write disk
	mov	bx, MasterBootBuff	; masterboot buffer

	mov	cx, 1			; cylinder = 0
					; sector = 1
	mov	dh, 0			; head = 0
	mov	dl, [DrvNum]		; drive number, 80h .. 83h
_49:
	mov	ax, 0301h
	int	13h
	jnc	short _51		; write masterboot sector, OK
	
 	; reset hard disk(s)
	xor	ah, ah
	;mov	dl, [drv]
	int	13h

	;dec	byte [RetryCount]
	dec	di
	jnz	short _49

	; disk write error
	mov	si, CRLF
	call	print_msg
	mov	si, msg_disk_write_error
	jmp	short _52
_50:
	; nothing to do !
	mov	si, CRLF 
	jmp	short _52
_51:
	; disk (mbr) writing is OK.
	mov	si, msg_OK
_52:
	call	print_msg	

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Exit
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	ax, 4C00h		; terminate
	int	21h
_53:
	jmp	short _53

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; LBA read (read one sector)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;lba_read:
;	mov	[rw], 42h
;	jmp	short lba_rw

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; LBA write (write one sector)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;lba_write:
;	mov	byte [rw], 43h
;	jmp	short lba_rw

lba_read:
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
	mov	ah, 42h ; LBA read
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

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; check active partition's (or primary dos partition's) bs
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

check_active_p_bs:
	; si = partition table address
	mov	bp, si
	add	bp, 48
chk_a_pbs_next:
	mov	al, [si+ptFileSystemID]

	mov	byte [bootsig], 38 ; FAT 16 & FAT 12

	cmp	al, 06h
	je	short read_start_sector ; FAT 16 CHS (big)
	cmp	al, 01h
	je	short read_start_sector ; FAT 12 CHS (or LBA)
	cmp	al, 04h
	je	short read_start_sector ; FAT 16 CHS
	cmp	al, 0Eh
	je	short read_start_sector ; FAT 16 LBA (windows)

	mov	byte [bootsig], 66 ; FAT 32

	cmp	al, 0Ch
	je	short read_start_sector ; FAT 32 LBA
	cmp	al, 0Bh
	je	short read_start_sector ; FAT 32 CHS

	mov	byte [bootsig], 0 ; others

	cmp	byte [si], 80h ; bootable (active) flag
       	je	short read_start_sector
	
	cmp	al, 05h
	je	short read_start_sector ; Extended DOS CHS
	cmp	al, 0Fh
	je	short read_start_sector ; Extended DOS (windows) LBA
	
	add	si, 16
	cmp	bp, si
	jnb	short chk_a_pbs_next
	
	; cf = 1 
	retn

read_start_sector:
	mov	ax, [si+ptStartSector]
	mov	dx, [si+ptStartSector+2]
	mov	bx, BootSectorBuff
	call	lba_read
	jc	short chk_a_pbs_retn

	cmp	word [BootSectorBuff+510], 0AA55h
	jne	short chk_a_pbs_stc

	mov	al, [bootsig]
	or	al, al
	jz	short chk_a_pbs_retn ; not a FAT fs (valid boot sector)
	xor	ah, ah
	add	bx, ax ; BootSectorBuffer + BS_BootSig offset
	
	; must be
	; same value of hidden sectors, same (extended boot) signature

	cmp	byte [bx], 29h ; Extd boot signature for FAT file systems
	jne	short chk_a_pbs_stc

	mov	ax, [BootSectorBuff+28] ; Hidden sectors
	mov	dx, [BootSectorBuff+30]
	cmp	ax, [si+ptStartSector]
	jne	short chk_a_pbs_stc
	cmp	dx, [si+ptStartSector+2]
	je	short chk_a_pbs_retn
		
chk_a_pbs_stc:
	stc	; not a valid boot sector

chk_a_pbs_retn:
	retn

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

;rw:	db	42h	; LBA read (default)
 
DrvNum:
	db	0

oldpbs: db	0

;align 2

;mbr_file_handle:
;	dw	0

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  Messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TrDOS_Welcome:
	db	0Dh, 0Ah
	db	"MasterBoot Record/Sector Restore Utility for TR-DOS 386"
	db	0Dh, 0Ah
	db	"v1.0.111020 (c) Erdogan TAN 2020"
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"Usage: "
	db	0Dh, 0Ah 
	db	" mbrestor [option] <hard disk name> <backup file name>"
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"Hard disk names: "
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
	db	"Options: "
	db	0Dh, 0Ah
	db	" -c : restore MBR code only "
	db	0Dh, 0Ah
	db	" -t : restore partition table only "
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"Examples:"
	db	0Dh, 0Ah
	db	" mbrestor -c hd0 mbsector.bin "
	db	0Dh, 0Ah
	db	" mbrestor hd1 mbsector.bin "
	db	0Dh, 0Ah, 0

msg_inv_file_name: 
	db	0Dh, 0Ah
	db	"Invalid file name !", 0Dh, 0Ah
	db	"(File name must fit to 8.3 DOS format) !"
	db	0Dh, 0Ah, 0

msg_kp_notfound:
	db	0Dh, 0Ah
	db	"Invalid backup file !", 0Dh, 0Ah
	db	'(MBR does not contain a known partition type) ! '	
	db	0Dh, 0Ah
	db	0

msg_overwrite_question:
	db	0Dh, 0Ah
	db	"Do you want to overwrite masterboot sector ? "
	db	0

_YES:	db	"YES"
	db	0Dh, 0Ah, 0

_NO:	db	"NO"
	db	0Dh, 0Ah, 0

msg_writing_mbr:
	db	0Dh, 0Ah
	db	"Writing masterboot sector ... "
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
	db	"(Invalid MBR) !"	
	db	0Dh, 0Ah, 0

msg_file_pbsv_error:
	db	0Dh, 0Ah
	db	"MASTERBOOT SECTOR WRITING PERMISSION DENIED ! Because ... "
	db	0Dh, 0Ah
	db	"Start sector of present active (or primary DOS) partition " 
	db	0Dh, 0Ah
	db	"has valid boot sector signature; but start sector of the new "
	db	0Dh, 0Ah
	db	"active (or primary DOS) partition has not a valid boot sector ! "
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

MBR:  ;	backup
	resb	512	

MasterBootBuff:
MasterBootCode: 
	resb	446 
PartitionTable:
	resb	64
MBIDCode:
	resw	1

BootSectorBuff:
	resb	512

option: resb 1
bootsig: resb 1
	

bss_end:	 	