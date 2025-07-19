; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel - v2.0.10) - DRV INIT : trdosk2.s
; ----------------------------------------------------------------------------
; Last Update: 13/05/2025 (Previous: 22/05/2024, v2.0.8)
; ----------------------------------------------------------------------------
; Beginning: 04/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ----------------------------------------------------------------------------
; Derived from TRDOS Operating System v1.0 (8086) source code by Erdogan Tan
; TRDOS2.ASM (09/11/2011)
; ****************************************************************************
; DRV_INIT.ASM (c) 2009-2011 Erdogan TAN  [26/09/2009] Last Update: 07/08/2011
;

ldrv_init: ; Logical Drive Initialization
	; 22/05/2024 (TRDOS 386 Kernel v2.0.8)
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 30/08/2020
	; 25/08/2020
	; 11/08/2020 - 13/08/2020
	; 17/07/2020 - 20/07/2020
	; 14/07/2020 - 15/07/2020
	; 30/01/2018
	; 27/12/2017
	; 12/02/2016
	; 06/01/2016
	;  	('diskinit.inc', 'diskio.inc' integration)
	; 04/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 07/08/2011
	; 20/09/2009
	; 2005

	; 15/07/2020
	;movzx	ecx, byte [HF_NUM] ; number of fixed disks
	;cmp	cl, 1
	;jnb	short load_hd_partition_tables

	mov	al, [HF_NUM] ; number of fixed disks
	and 	al, al
	jnz	short load_hd_partition_tables

	; no any hard disks
	retn

load_hd_partition_tables:
	;mov	esi, [HDPM_TBL_VEC] ; primary master disk FDPT
	; 15/07/2020
	mov	esi, HDPM_TBL_VEC
	mov 	edi, PTable_hd0
	mov 	dl, 80h
	; 15/07/2020
	mov	[hdc], al
	;xor	ecx, ecx ; 0
load_next_hd_partition_table:
	; 20/07/2020
	xor	ecx, ecx ; 0
	;push	ecx
	push	edi ; *
	;push	esi ; FDPT (+ DPTE) address
	; 15/07/2020
	lodsd
	push	esi ; ** ; next FDPT (+ DPTE) address ptr

	;mov	al, [esi+20] ; DPTE offset 4
	;and	al, 40h ;  LBA bit (bit 6)
	;;shr	al, 6
	;mov 	[HD_LBA_yes], al

	; 15/07/2020
	mov	cl, [eax+20]
	and	cl, 40h
	;mov	[HD_LBA_yes], cl
	; 22/05/2024 (BugFix)
	movzx	eax, dl
	add	eax, HD_LBA_yes - 80h
	mov	[eax], cl

	call	load_masterboot
	;jc	short pass_pt_this_hard_disk
	; 13/08/2020
	;jc	pass_pt_this_hard_disk
	; 25/07/2022
	jnc	short load_mbr_ok
	jmp	pass_pt_this_hard_disk

load_mbr_ok:
	mov	ebx, PartitionTable
	mov	esi, ebx
	;mov	ecx, 16
	mov	cl, 16
	rep 	movsd
	mov 	esi, ebx
	;mov 	byte [hdc], 4 ; 4 - partition index
	; 15/07/2020
	mov	byte [PP_Counter], 4
loc_validate_hdp_partition:
	;cmp 	byte [esi+ptFileSystemID], 0
	;jna	short loc_validate_next_hdp_partition2
	; 13/08/2020
	mov	al, [esi+ptFileSystemID]
	and	al, al
	jz	short loc_validate_next_hdp_partition2

	push	esi ; *** ; Masterboot partition table offset
	push	edx ; **** ; dl = Physical drive number

	; 13/08/2020
	cmp	al, 05h  ; Extended partition CHS
 	je	short loc_set_ep_counter
	cmp	al, 0Fh  ; Extended partition LBA
 	jne	short loc_validate_next_hdp_partition0

	;;inc	byte [PP_Counter]
	; 15/07/2020
	;inc 	byte [EP_Counter] ; disk has valid partition(s)

loc_set_ep_counter:
	; 13/08/2020
	cmp	byte [EP_Counter], 80h
	jnb	short loc_validate_next_hdp_partition1

	mov	byte [EP_Counter], dl ; disk drv has extd. part.

	jmp	short loc_validate_next_hdp_partition1

loc_validate_next_hdp_partition0:
	xor	edi, edi ; 0  
	; Input -> ESI = PartitionTable offset
	; DL = Hard disk drive number
	; EDI = 0 -> Primary Partition
	; EDI > 0 -> Extended Partition's Start Sector
	call 	validate_hd_fat_partition
	jnc 	short loc_set_valid_hdp_partition_entry

	;pop	edx
	;push	edx
	mov	edx, [esp]  ; ****
	mov	esi, [esp+4] ; *** ; 30/01/2018
	call	validate_hd_fs_partition
	jc	short loc_validate_next_hdp_partition1
loc_set_valid_hdp_partition_entry:
	mov 	cl, [Last_DOS_DiskNo] 
	add 	cl, 'A'
	; ESI = Logical dos drive description table address
	mov	[esi+LD_Name], cl
	; 15/07/2020
	mov	al, [esi+LD_PhyDrvNo] ; Physical drive number
	;mov	al, [esp] ; ****
	sub	al, 7Fh
		; AL = 1 to 4
	shl	al, 2 ; AL = 4 to 16

	mov	dl, [PP_Counter]

	;sub	al, [PP_Counter]
	sub	al, dl ; [PP_Counter] ; 4 - partition index

	; AL = Partition entry/index, 0 based
	;  0 -> hd 0, Partition Table offset = 0
	; 15 -> hd 3, Partition Table offset = 3

	;mov	[esi+LD_PartitionEntry], al

	; 15/07/2020
	mov	ah, 4
	;sub	ah, [PP_Counter]
	sub	ah, dl

	; AH = Primary partition index, 0 to 3 ; pt entry
	;		(4 to 7 for logical disk partitions)

	;mov 	[esi+LD_DParamEntry], ah
	mov 	[esi+LD_PartitionEntry], ax

loc_validate_next_hdp_partition1:
	pop 	edx ; **** ; dl = Physical drive number 
	pop	esi ; *** ; Masterboot partition table offset

loc_validate_next_hdp_partition2:
	; ESI = PartitionTable offset
	; DL = Hard/Fixed disk drive number

	;dec	byte [hdc] ; 4 - partition index
	;jz	short pass_pt_this_hard_disk
	; 15/07/2020
	dec	byte [PP_Counter] ; 4 - partition index
	jz	short pass_pt_this_hard_disk

	add	esi, 16 ; 10h
	jmp	short loc_validate_hdp_partition

loc_not_any_extd_partitions:
	; 15/07/2020
	retn

loc_next_hd_partition_table:
	inc	dl
	; 15/07/2020
	;add	esi, 32 ; next FDPT address
	add	edi, 64 ; next partition table destination
        jmp     load_next_hd_partition_table

pass_pt_this_hard_disk:
	;pop	esi ; FDPT (+ DPTE) address
	; 15/07/2020
	pop	esi ; ** ; next FDPT (+ DPTE) address ptr
	pop	edi ; * ; Ptable_hd?
	;pop	ecx
	;loop	loc_next_hd_partition_table
	dec	byte [hdc]
	jnz	short loc_next_hd_partition_table

	;cmp	byte [PP_Counter], 1
	;jnb	short load_extended_dos_partitions
	;; Empty partition table
	;retn

	; 11/08/2020
	; 17/07/2020
check_extended_partitions:
	; 15/07/2020
	;cmp	byte [EP_Counter], 0
	;jna	short loc_not_any_extd_partitions
	; 13/08/2020
	mov	al, [EP_Counter] ; 1st disk drv has extd partition
	or	al, al ; 0 ?
	jz	short loc_not_any_extd_partitions

load_extended_dos_partitions:
	;mov	byte [hdc], 80h
	; 13/08/2020
	mov	byte [hdc], al ; 1st disk drv has extd partition
	; 25/08/2020
	sub	al, 80h
	jz	short loc_set_ext_ptable_hd0
	shl	al, 6 ; * 64
	movzx	esi, al
	add	esi, PTable_hd0
	jmp 	short next_hd_extd_partition

	; 25/08/2020
loc_set_ext_ptable_hd0:
	mov	esi, PTable_hd0

next_hd_extd_partition:
	; 17/07/2020
	;mov 	byte [EP_Counter], 0 ; Reset for each physical disk
	; 13/08/2020
	;mov	byte [LD_Counter], 0 ; Reset logical drive index
	mov 	word [EP_Counter], 0 ; Reset EP index and LD index

	push	esi ; **** ; PTable_hd? offset

	mov 	byte [PP_Counter], 4
				; set for each extd partition table
	;;mov	ecx, 4
	;mov	cl, 4
	mov	dl, [hdc]
hd_check_fs_id_05h:
	mov	al, [esi+ptFileSystemID]
	cmp	al, 05h ; Is it an extended dos partition ?
	je	short loc_set_ep_start_sector ; yes
hd_check_fs_id_0Fh:
	cmp	al, 0Fh ; Is it an extended win4 (LBA mode) partition ?
	je	short loc_set_ep_start_sector ; yes

continue_to_check_ep:
	;add	esi, 16
	;loop	hd_check_fs_id_05h
	; 15/07/2020
	;dec	cl
	;jz	short continue_check_ep_next_disk
	dec	byte [PP_Counter] ; 4 --> 0
	jz	short continue_check_ep_next_disk
	add	esi, 16
	jmp	short hd_check_fs_id_05h

loc_set_ep_start_sector:
	; dl = [hdc] ; Drive number
	; 15/07/2020
	mov	ecx, [esi+ptStartSector]
	; 30/08/2020
	mov	[MBR_EP_StartSector], ecx 
	; 20/07/2020
loc_validate_hde_partition_next:
	; 22/05/2024 (BugFix)
	movzx	edi, dl
	add	edi, HD_LBA_yes - 80h
	;
	mov	[EP_StartSector], ecx ; Extended partition's start sector
        mov	ebx, MasterBootBuff
	; 22/05/2024
	cmp	byte [edi], 1 ; LBA ready = Yes
	;cmp	byte [HD_LBA_yes], 1 ; LBA ready = Yes
	jb	short loc_hd_load_ep_05h ; cf = 1 ; 20/07/2020 
	; 11/08/2020
	; (BugFix for extended partition type 05h beyond CHS limit)
	; (Infact if extended partition starts at the beyond of CHS limit,
	;  it's partition ID must be 0Fh but they/somebodies had used 05h.)
	;cmp	al, 05h
	;je	short loc_hd_load_ep_05h
loc_hd_load_ep_0Fh:
	; 04/01/2016
	;push	ecx
	; 15/07/2020
	;mov	ecx, [esi+ptStartSector] ; sector number
	;mov	ebx, MasterBootBuff ; buffer address
	; LBA read/write (with private LBA function) 
	;((Retro UNIX 386 v1 - DISK I/O code by Erdogan Tan))
	; dl = physical drive number (0,1, 80h, 81h, 82h, 83h)
	;mov	ah, 1Bh ; LBA read
	;mov	al, 1 ; sector count
	mov	ax, 1B01h
	call	int13h
	;pop	ecx
	;jnc	short loc_hd_move_ep_table
	; 15/07/2020
	jnc	short loc_validate_hde_partition

continue_check_ep_next_disk:
	; 15/07/2020
	;pop	edi ; PTable_ep?
	pop	esi ; **** ; PTable_hd?
	mov	al, [HF_NUM] ; number of hard disks
	add	al, 7Fh
	cmp	[hdc], al
	jnb	short loc_validating_hd_partitions_ok
	add	esi, 64
	; 15/07/2020
	;add	edi, 64
	inc	byte [hdc]
	jmp	short next_hd_extd_partition

loc_validating_hd_partitions_ok:
	; 15/07/2020
	;mov	al, [Last_DOS_DiskNo]
loc_drv_init_retn:
	retn

loc_hd_load_ep_05h:
	; 20/07/2020 ('diskio.s', int13h, cf = 1 -> bugfix)
	;clc    ; (Bug: int13h would not clear carry flag bit,
	;	; even if there would not be an error)
	;	; ((Fix: now, int13h procedure clears carry flag
	;	;  at the entrance of it.. 20/07/2020))
	; 15/07/2020
	;push	ecx
	mov	dh, [esi+ptBeginHead]
        mov     cx, [esi+ptBeginSector]
	mov	ax, 0201h ; Read 1 sector
	;mov	ebx, MasterBootBuff
	call	int13h ; 20/07/2020
		       ; 'diskio.s' modification, 'clc'
	;pop	ecx
	jc	short continue_check_ep_next_disk
	; 15/07/2020
	;jmp	short loc_validate_hde_partition

	; 15/07/2020
;loc_hd_move_ep_table:
	;;pop	edi
	;;push	edi  ; PTable_ep?
	;mov	edi, [esp]
        ;mov	esi, PartitionTable ; Extended
	;mov	ebx, esi
	;;mov	ecx, 16
	;mov	cl, 16
       	;rep	movsd
	;mov	esi, ebx
;loc_set_hde_sub_partition_count:
	;mov	byte [PP_Counter], 4
	;mov	byte [EP_Counter], 0

loc_validate_hde_partition:
	; 13/08/2020
	; 15/07/2020
	;mov	byte [PP_Counter], 4
	mov	esi, PartitionTable ; (in MasterBootBuff)
	; 13/08/2020
	;jmp	short get_minidisk_partition_entry

;get_minidisk_partition_entry:
;	; 20/07/2020
;	cmp	byte [esi+ptFileSystemID], 0
;	ja	short loc_validate_minidisk_partition
;	; 13/08/2020
;	jmp	short continue_check_ep_next_disk

;	; 11/08/2020
;get_minidisk_partition_entry_next:
;	; 13/08/2020
;	;dec 	byte [PP_Counter]
;	;jz	short continue_check_ep_next_disk
;	; 20/07/2020
;;get_minidisk_partition_entry_next:
;	; 13/08/2020
;	cmp	esi, PartitionTable+64 
;	jnb	short continue_check_ep_next_disk
;
;	add 	esi, 16 ; 10h
;	;jmp	short get_minidisk_partition_entry

	; 13/08/2020
get_minidisk_partition_entry:
	; 20/07/2020
	cmp	byte [esi+ptFileSystemID], 0
	jna	short continue_check_ep_next_disk ; 13/08/2020

loc_validate_minidisk_partition:
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 13/08/2020
	; 20/07/2020
	;push	esi ; *** ; Extended partition table offset

	; 13/08/2020
	inc	byte [EP_Counter] ; current (sub partition) index
				  ; in current extended partition
	mov	edi, EP_StartSector

	; Input -> ESI = PartitionTable offset
	; DL = Hard disk drive number   
	; EDI = Extended partition start sector pointer
	call	validate_hd_fat_partition
	;pop	ecx ; *
	jnc	short loc_set_valid_hde_partition_entry
			 ; jump down to deep !!!

	;pop	esi ; *** ; Extended partition table offset
	; 13/08/2020
	;mov	esi, PartitionTable

	; 11/08/2020
	; ESI = Extended partition table offset
	mov	dl, [hdc]

	;; DL = Hard disk drive number
	;dec	byte [PP_Counter]
	;jz	short continue_check_ep_next_disk
	;add 	esi, 16 ; 10h
	;mov	dl, [hdc]
	;jmp	short get_minidisk_partition_entry

	; 11/08/2020
	;jmp	short get_minidisk_partition_entry_next

	; 23/08/2020
	jmp	short validate_next_minidisk_partition_ok

	; 17/07/2020
	;; jumping down to deep levels !!!
	; ((That is a pitty microsoft preferred ep table chain
	; instead of a single table as mbr partition table!?))

loc_set_valid_hde_partition_entry:
	; 15/07/2020
	mov	al, [hdc] ; Hard disk drive number (>=80h)
	mov	dl, al ; mov dl, [hdc]
	sub	al, 7Fh
		    ; 1 to 4
	shl	al, 2 ; 4 to 16
	sub	al, [PP_Counter] ; al - (4 - partition index)
			; (disk number * 4) + partition index

	; AL = Partition entry/index, 0 based
        ;  0 -> hd 0, Partition Table offset = 0
        ; 15 -> hd 3, Partition Table offset = 3

	;mov	ah, 4 ; Logical dos partition (>= 4)
	;add	ah, [EP_Counter]
		; Logical disk partition index = 4 to 7
		; (Primary disk partition index = 0 to 3)

	; 13/08/2020
	mov	ah, [LD_Counter] ; Logical drive index number
				; (in current extended partition)
	add	ah, 4 ; 4 to 7
	
	; 15/07/2020
	; CX -> AX
	;; 06/01/2016 (TRDOS v2.0)
	;; BUGFIX *
	;;mov	[esi+LD_PartitionEntry], cl
	;;mov	[esi+LD_DParamEntry], ch 
	;mov	[esi+LD_PartitionEntry], cx
	mov	[esi+LD_PartitionEntry], ax

	mov	cl, [Last_DOS_DiskNo]
	add	cl, 'A'
	mov	[esi+LD_Name], cl

	; 17/07/2020
	;cmp	cl, 'Z'
	;jb	short logical_drive_count_ok_for_next
	;pop	esi ; ***
	;pop	esi ; ****
	;retn

;logical_drive_count_ok_for_next:

	;; 15/07/2020
	;inc	byte [EP_Counter]
	; 13/08/2020
	inc	byte [LD_Counter]

	;mov	dl, [hdc]

	; 17/07/2020
	;; Now, 
	;; we are swimming in deep of an extended partition !!!
	; (! sub or chained extended partition tables !)
	; ((Logical dos partitions in extended partition were called
	;  as 'mini disk partition' in msdos 6.0 source code.))

validate_next_minidisk_partition:
	; 13/08/2020
	;pop	esi ; *** ; Extended partition table offset

	; 17/07/2020
	;cmp	byte [EP_Counter], 4
	; 13/08/2020
	cmp	byte [LD_Counter], 4 ; maximum 4 logical disks
				     ; per extended partition
	;jnb	continue_check_ep_next_disk
	; 25/07/2022
	jb	short validate_next_minidisk_partition_ok
	jmp	continue_check_ep_next_disk

validate_next_minidisk_partition_ok:
	; 13/08/2020
	;dec	byte [PP_Counter] ; 4 --> 0
	;jz	continue_check_ep_next_disk

	;cmp	esi, PartitionTable+64
	;jnb	continue_check_ep_next_disk

	;add	esi, 16
	; 13/08/2020
	mov	esi, PartitionTable+16

	; 20/07/2020
	mov	al, [esi+ptFileSystemID]

	; 20/07/2020
	cmp	al, 05h ; Is it an extended dos partition ?
	je	short loc_minidisk_next_ep_lba_chs ; 17/07/2020
	cmp	al, 0Fh ; Is it an extended win4 (LBA mode) partition ?
	;jne	continue_check_ep_next_disk ; AL must be 0 here
					; (when it is not 05h or 0Fh)
					; If AL is not ZERO -> EP Bug!
					; (!Microsoft DOS convention!)
	; 25/07/2022
	je	short loc_minidisk_next_ep_lba_chs
	jmp	continue_check_ep_next_disk

loc_minidisk_next_ep_lba_chs:
	; 17/07/2020
	mov	ecx, [esi+ptStartSector] ; relative start sector number
	;add	ecx, [EP_StartSector]
	; 30/08/2020	
	add	ecx, [MBR_EP_StartSector]
	; 20/07/2020
	jmp	loc_validate_hde_partition_next

validate_hd_fat_partition:
	; 04/05/2025 (TRDOS 386 v2.0.5)
	; 17/07/2020
	; 15/07/2020
	;	(optimization)
	; 14/07/2020
	;	(fat16 -big- partition search bugfix)
	; 27/12/2017
	; 12/02/2016
	; 07/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 07/08/2011
	; 23/07/2011
	; Input
	;   DL = Hard/Fixed Disk Drive Number
	;   ESI = PartitionTable offset
	;   EDI = Extend. Part. Start Sector Pointer
	;   EDI = 0 -> Primary Partition
	;   byte [Last_DOS_DiskNo]
 	; Output
	;  cf=0 -> Validated
	;   ESI = Logical dos drv desc. table
	;   EBX = FAT boot sector buffer
	;   byte [Last_DOS_DiskNo]
	;  cf=1 -> Not a valid FAT partition
	; EAX, EDX, ECX, EDI -> changed

	;mov 	esi, PartitionTable
	mov 	ah, [esi+ptFileSystemID]
	mov	al, 2 ; 27/12/2017
	cmp 	ah, 06h ; FAT16 CHS partition (>=32MB)
	; 12/02/2016
	;;jb	short loc_not_a_valid_fat_partition2
 	;jnb	short vhdp_FAT16_32
	; 14/07/2020 (BugFix)
	ja	short vhdp_FAT16_32
	je	short loc_set_valid_hd_partition_params

vhdp_FAT12_16:
	; 27/12/2017
	dec	al ; mov al, 1
	cmp	ah, al ; 1 ; FAT12 partition
	je	short loc_set_valid_hd_partition_params
	;
	inc	al ; mov al, 2
	cmp	ah, 04h ; FAT16 CHS partition (< 32MB)
	je	short loc_set_valid_hd_partition_params

	; 15/07/2020
	; (ah = 05h, 02h or 03h)
loc_not_a_valid_fat_partition1:
	stc
	; cf=1
	retn

vhdp_FAT16_32:
	; 15/07/2020
	;mov	al, 3
	inc	al
	cmp	ah, 0Ch ; FAT32 LBA partition
	je	short loc_set_valid_hd_partition_params
	ja	short vhdp_check_FAT16_lba

vhdp_check_FAT32_chs:
	cmp	ah, 0Bh ; FAT32 CHS partition 
	je	short loc_set_valid_hd_partition_params
	;jne	short loc_not_a_valid_fat_partition1

	;stc
loc_not_a_valid_fat_partition2:
	retn

vhdp_check_FAT16_lba:
	cmp	ah, 0Eh ; FAT16 LBA partition
	jne	short loc_not_a_valid_fat_partition1

	;mov	al, 2
	dec	al

loc_set_valid_hd_partition_params:
	; 30/07/2022
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 15/07/2020
	;inc 	byte [Last_DOS_DiskNo] ; > 1
	;
	xor	ebx, ebx
	mov	bh, [Last_DOS_DiskNo] ; * 256
	inc	bh ; 15/07/2020
	add	ebx, Logical_DOSDisks
	;
	mov	byte [ebx+LD_DiskType], 2
	mov	byte [ebx+LD_PhyDrvNo], dl
	;mov	byte [ebx+LD_FATType], al ; 2 or 3
	;mov	byte [ebx+LD_FSType], ah ; 06h, 0Eh, 0Bh, 0Ch
	mov	word [ebx+LD_FATType], ax
	;
	mov	ecx, [esi+ptStartSector]
	or	edi, edi 
	jz	short pass_hd_FAT_ep_start_sector_adding
loc_add_hd_FAT_ep_start_sector:
	; 17/07/2020
	add	ecx, [edi]
pass_hd_FAT_ep_start_sector_adding:
	mov	[ebx+LD_StartSector], ecx
loc_hd_FAT_logical_drv_init:
	mov	ebp, ebx
	;mov	dl, [ebx+LD_PhyDrvNo]
	mov	al, [HD_LBA_yes] ; 07/01/2016
	mov	[ebx+LD_LBAYes], al
	mov	ebx, DOSBootSectorBuff ; buffer address
	or	al, al
	jz	short loc_hd_FAT_drv_init_load_bs_chs
loc_hd_FAT_drv_init_load_bs_lba:
	; DL = Physical drive number
   	;mov	ecx, [esi+ptStartSector] ; sector number
	;mov	ebx, DOSBootSectorBuff ; buffer address
	; LBA read/write (with private LBA function) 
	;((Retro UNIX 386 v1 - DISK I/O code by Erdogan Tan))
	; dl = physical drive number (0,1, 80h, 81h, 82h, 83h)
	mov	ah, 1Bh ; LBA read
	mov	al, 1 ; sector count
	call	int13h
	jnc	short loc_hd_drv_FAT_boot_validation
loc_not_a_valid_fat_partition3:
	retn
loc_hd_FAT_drv_init_load_bs_chs:
	mov	dh, [esi+ptBeginHead]
	mov	cx, [esi+ptBeginSector]
	mov	ax, 0201h ; Read 1 sector
	;mov	ebx, DOSBootSectorBuff
	call	int13h
	jc	short loc_not_a_valid_fat_partition3
loc_hd_drv_FAT_boot_validation:
	;mov	esi, DOSBootSectorBuff
	mov	esi, ebx
	cmp	word [esi+BS_Validation], 0AA55h
	jne	short loc_not_a_valid_fat_partition4
	cmp	byte [esi+BPB_Media], 0F8h
	jne	short loc_not_a_valid_fat_partition4

	; 25/07/2022
	xor	ecx, ecx

	; 27/12/2017
	cmp	byte [ebp+LD_FATType], 3
	jne	short loc_hd_FAT16_BPB

loc_hd_drv_FAT32_boot_validation:
	cmp	byte [esi+BS_FAT32_BootSig], 29h
	je	short loc_hd_FAT32_BPB

loc_not_a_valid_fat_partition4:
	stc
	retn

loc_hd_FAT16_BPB:
	cmp	byte [esi+BS_BootSig], 29h
	jne	short loc_not_a_valid_fat_partition4

	cmp	word [esi+BPB_FATSz16], 0
	jna	short loc_hd_big_FAT16_BPB
	;mov	ecx, 32
	; 25/07/2022
	mov	cl, 32
	; ecx = 32
	jmp	short loc_hd_move_FAT_BPB

loc_hd_FAT32_BPB:
	;cmp	word [esi+BPB_FATSz16], 0
	;ja	short loc_not_a_valid_fat_partition4
loc_hd_big_FAT16_BPB:
	;mov	ecx, 45
	; 25/07/2022
	mov	cl, 45
	; ecx = 45
loc_hd_move_FAT_BPB:
	mov 	edi, ebp
	;mov	esi, ebx ; Boot sector
	push	edi
	add	edi, LD_BPB
	rep	movsw
	pop	esi
	movzx	eax, word [esi+LD_BPB+BPB_RsvdSecCnt]
	add	eax, [esi+LD_StartSector]
	mov	[esi+LD_FATBegin], eax
	cmp	byte [esi+LD_FATType], 3
	jb	short loc_set_FAT16_RootDirLoc
loc_set_FAT32_RootDirLoc:
	mov	eax, [esi+LD_BPB+BPB_FATSz32]
        movzx	ebx, byte [esi+LD_BPB+BPB_NumFATs]
	mul	ebx
	add	eax, [esi+LD_FATBegin]
loc_set_FAT32_data_begin:
	mov	[esi+LD_DATABegin], eax
	mov	[esi+LD_ROOTBegin], eax
	; If Root Directory Cluster <> 2 then
	; change the beginning sector value 
	; of the root dir by adding sector offset.
	mov	eax, [esi+LD_BPB+BPB_RootClus]
	;sub	eax, 2
	; 30/07/2022
	dec	eax ; 2 -> 1
	dec	eax ; 1 -> 0
	jz	short short loc_set_32bit_FAT_total_sectors
	;movzx	ebx, byte [esi+LD_BPB+BPB_SecPerClust]
	mov	bl, [esi+LD_BPB+BPB_SecPerClust] 
	mul	ebx
	add	[esi+LD_ROOTBegin], eax
	jmp	short loc_set_32bit_FAT_total_sectors
	;
loc_set_FAT16_RootDirLoc:
	movzx	eax, byte [esi+LD_BPB+BPB_NumFATs]
	movzx	edx, word [esi+LD_BPB+BPB_FATSz16]
	mul	edx
	add	eax, [esi+LD_FATBegin]
	mov	[esi+LD_ROOTBegin], eax
loc_set_FAT16_data_begin:
	mov	[esi+LD_DATABegin], eax 
	;mov	eax, 20h  ; Size of a directory entry
	;;movzx	edx, word [esi+LD_BPB+BPB_RootEntCnt]
        ;mov	dx, [esi+LD_BPB+BPB_RootEntCnt]
        ;mul	edx
	;;mov	ecx, 511
	;mov	cx, 511
	;add	eax, ecx
	;inc	ecx ; 512
	;div	ecx
	; 14/07/2020
	movzx	eax, word [esi+LD_BPB+BPB_RootEntCnt]
	add	ax, 15
	;shr	ax, 4 ; / 16 ; (16 entries per sector)
	; 25/07/2022
	shr	eax, 4
	add	[esi+LD_DATABegin], eax
	;movzx	eax, word [esi+LD_BPB+BPB_TotalSec16]
	mov	ax, [esi+LD_BPB+BPB_TotalSec16]
	;test	ax, ax
	; 25/07/2022
	test	eax, eax
	;jz	short loc_set_32bit_FAT_total_sectors
;loc_set_16bit_FAT_total_sectors:
	;mov	[esi+LD_TotalSectors], eax
	;jmp	short loc_set_hd_FAT_cluster_count
	; 14/07/2020
	jnz	short loc_set_hd_FAT_cluster_count
loc_set_32bit_FAT_total_sectors:
	mov	eax, [esi+LD_BPB+BPB_TotalSec32]
	;mov	[esi+LD_TotalSectors], eax
loc_set_hd_FAT_cluster_count:
	mov	[esi+LD_TotalSectors], eax ; 14/07/2020
	mov	edx, [esi+LD_DATABegin]
	sub	edx, [esi+LD_StartSector]
	sub	eax, edx
	xor	edx, edx ; 0
        movzx   ecx, byte [esi+LD_BPB+BPB_SecPerClust]
        div	ecx
	mov	[esi+LD_Clusters], eax
	; Maximum Valid Cluster Number= EAX +1
	; with 2 reserved clusters
loc_set_hd_FAT_fs_free_sectors:
	;mov	dword [esi+LD_FreeSectors], 0
	; 04/05/2025
	call	get_free_FAT_sectors
	jc	short loc_validate_hd_FAT_partition_retn
	mov	[esi+LD_FreeSectors], eax
	mov	byte [esi+LD_MediaChanged], 6 ; Volume Name Reset

	; 15/07/2020
	inc 	byte [Last_DOS_DiskNo] ; > 1

	;mov	cl, [Last_DOS_DiskNo] 
	;add	cl, 'A'
	;mov	[esi+LD_FS_Name], cl

loc_validate_hd_FAT_partition_retn:
	retn

validate_hd_fs_partition:
	; 06/05/2025 (TRDOS 386 Kernel v2.0.10)
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 03/02/2018
	; 09/12/2017
	; 13/02/2016
	; 10/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 29/01/2011
	; 23/07/2011
	; Input
	;   DL = Hard/Fixed Disk Drive Number
	;   ESI = PartitionTable offset
	;   byte [Last_DOS_DiskNo]
	; Output
	;  cf=0 -> Validated
	;   ESI = Logical dos drive description table
	;   ; 06/05/2025
	;   ;EBX = Singlix FS boot sector buffer
	;   byte [Last_DOS_DiskNo]
	;  cf=1 -> Not a valid 'Singlix FS' partition
	; 06/05/2025
	; EAX, EBX, ECX, EDX, EDI -> changed

	;mov	esi, PartitionTable
	mov	ah, [esi+ptFileSystemID]
	cmp	ah, 0A1h ; SINGLIX FS1 (trfs1) partition
	jne	short loc_validate_hd_fs_partition_stc_retn
loc_set_valid_hd_fs_partition_params:
	inc	byte [Last_DOS_DiskNo] ; > 1
	xor	al, al ; mov al, 0
	;mov	[drv], dl
	sub	ebx, ebx ; 0
	mov	bh, [Last_DOS_DiskNo]
	add	ebx, Logical_DOSDisks
	mov	byte [ebx+LD_DiskType], 2
	mov	[ebx+LD_PhyDrvNo], dl
	;mov	[ebx+LD_FATType], al ; 0
	;mov	[ebx+LD_FSType], ah
	mov	[ebx+LD_FATType], ax
	;mov	eax, [esi+ptStartSector]
	;mov	[ebx+LD_StartSector], eax
loc_hd_fs_logical_drv_init:
	mov	ebp, ebx ; 10/01/2016
	;mov	dl, [ebx+LD_PhyDrvNo]
	mov	al, [HD_LBA_yes] ; 10/01/2016
	mov	[ebx+LD_LBAYes], al
	mov	esi, ebx
	mov	ebx, DOSBootSectorBuff ; buffer address
	or	al, al
	jnz	short loc_hd_fs_drv_init_load_bs_lba
loc_hd_fs_drv_init_load_bs_chs:
	mov	dh, [esi+ptBeginHead]
	mov	cx, [esi+ptBeginSector]
	mov	ax, 0201h ; Read 1 sector
	;mov	ebx, DOSBootSectorBuff
	call	int13h
	jnc	short loc_hd_drv_fs_boot_validation
loc_validate_hd_fs_partition_err_retn:
	retn
loc_validate_hd_fs_partition_stc_retn:
	stc
	retn
loc_hd_fs_drv_init_load_bs_lba:
	; DL = Physical drive number
	;mov	esi, ebx
   	mov	ecx, [esi+ptStartSector] ; sector number
	;mov	ebx, DOSBootSectorBuff ; buffer address
	; LBA read/write (with private LBA function) 
	;((Retro UNIX 386 v1 - DISK I/O code by Erdogan Tan))
	; dl = physical drive number (0,1, 80h, 81h, 82h, 83h)
	mov	ah, 1Bh ; LBA read
	mov	al, 1 ; sector count
	call	int13h
	jc	short loc_validate_hd_fs_partition_err_retn
loc_hd_drv_fs_boot_validation:
	;mov	esi, DOSBootSectorBuff
	mov	esi, ebx ; Boot sector buffer
	cmp	word [esi+BS_Validation], 0AA55h
	jne	short loc_validate_hd_fs_partition_stc_retn
        ;
	;Singlix FS Extensions to TR-DOS (7/6/2009) 
	cmp	word [esi+bs_FS_Identifier], 'FS' ; 03/02/2018
	jne	short loc_validate_hd_fs_partition_stc_retn
        ;'A1h' check is not necessary
	;  if 'FS' check is passed as OK/Yes.
	cmp	byte [esi+bs_FS_PartitionID], 0A1h
	jne	short loc_validate_hd_fs_partition_stc_retn
	;
	mov	edi, ebp ; 10/01/2016
	;
	mov	al, byte [esi+bs_FS_LBA_Ready]
	mov	[edi+LD_FS_LBAYes], al
	;
	; 03/01/2010 CHS -> DOS FAT/BPB compatibility fix
	mov	al, [esi+bs_FS_MediaAttrib]
	mov	byte [edi+LD_FS_MediaAttrib], al
	;
	mov	al, [esi+bs_FS_VersionMaj]
	mov	[edi+LD_FS_VersionMajor], al
	;
	mov	ax, [esi+bs_FS_BytesPerSec]
	mov	[edi+LD_FS_BytesPerSec], ax
	mov	al, [esi+bs_FS_SecPerTrack]
	xor	ah, ah ; 09/12/2017
	mov	[edi+LD_FS_SecPerTrack], ax
	mov	al, [esi+bs_FS_Heads]
	mov	[edi+LD_FS_NumHeads], ax
	;
	mov	eax, [esi+bs_FS_UnDelDirD]
	mov	[edi+LD_FS_UnDelDirD], eax
	mov	edx, [esi+bs_FS_MATLocation]
	mov	[edi+LD_FS_MATLocation], edx
	mov	eax, [esi+bs_FS_RootDirD]
	mov	[edi+LD_FS_RootDirD], eax
	mov	eax, [esi+bs_FS_BeginSector]
	mov	[edi+LD_FS_BeginSector], eax
	mov	eax, [edi+bs_FS_VolumeSize]
	mov	[edi+LD_FS_VolumeSize], eax
	;
	mov	eax, edx ; [edi+LD_FS_MATLocation]
	add	eax, [edi+LD_FS_BeginSector]
	;mov	esi, edi
	; 06/05/2025
	xchg	esi, edi
mread_hd_fs_MAT_sector:
        ;mov	ebx, DOSBootSectorBuff
	;mov	ecx, 1
	; 06/05/2025 - TRDOS 386 v2.0.10
	;; 25/07/2022
	;sub	ecx, ecx
	;inc	cl
	;; ecx = 1
	;call	disk_read
	call	DREAD ; read one sector
	jc	short loc_validate_hd_fs_partition_retn
	; EDI will not be changed
	; 06/05/2025 (ebx has been modified in DREAD proc)
	;mov	esi, ebx
	xchg	edi, esi
	; esi = MAT (master allocation table) buffer addr
	; edi = LDRVT (logical dos drive desc. table) addr
use_hdfs_mat_sector_params:
	mov	eax, [esi+FS_MAT_DATLocation]
	; 06/05/2025
	; eax must be 1 here (DAT is just after the MAT)
	; ((in fact, MAT is DAT header))
	mov	[edi+LD_FS_DATLocation], eax
	mov	eax, [esi+FS_MAT_DATScount]
	mov	[edi+LD_FS_DATSectors], eax
	mov	eax, [esi+FS_MAT_FreeSectors]
        mov     [edi+LD_FS_FreeSectors], eax
	mov	eax, [esi+FS_MAT_FirstFreeSector]
	mov	[edi+LD_FS_FirstFreeSector], eax
	mov	eax, [edi+LD_FS_RootDirD]
	add	eax, [edi+LD_FS_BeginSector]
	;mov	esi, edi
	; 06/05/2025
	xchg	esi, edi
read_hd_fs_RDT_sector:
	;mov	ebx, DOSBootSectorBuff
	; 06/05/2025
	mov	ebx, edi ; DOSBootSectorBuff
	; 06/05/2025 - TRDOS 386 v2.0.10
	;;mov	ecx, 1
	;mov	cl, 1
	;call	disk_read
	call	DREAD ; read one sector
	jc	short loc_validate_hd_fs_partition_retn
	; EDI will not be changed
	;mov	esi, ebx
	; 06/05/2025
	xchg	edi, esi
use_hdfs_RDT_sector_params:
	mov	eax, [esi+FS_RDT_VolumeSerialNo]
	mov	[edi+LD_FS_VolumeSerial], eax
	push	edi
	;mov	ecx, 16
	mov	cl, 16
	; 06/05/2025
	; ecx < 256 (at return from DREAD)
	add	esi, FS_RDT_VolumeName
	add	edi, LD_FS_VolumeName
	rep	movsd ; 64 bytes
	pop	esi
		; Volume Name Reset
        mov     byte [esi+LD_FS_MediaChanged], 6
	;
        ;mov	cl, [Last_DOS_DiskNo]
	;add	cl, 'A'
	;mov	[esi+LD_FS_Name], cl

loc_validate_hd_fs_partition_retn:
	retn

load_masterboot:
	; 06/05/2025 (TRDOS 386 Kernel v2.0.10)
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 14/07/2020 (Reset function has been removed)
	;
	; 10/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 2005 - 2011
	; input -> DL = drive number
	;	  ; 06/05/2025
	;	  ; (ecx < 256)

;	mov	ah, 0Dh ; Alternate disk reset
;	call	int13h
;	jnc	short pass_reset_error
;harddisk_error:
;  	retn
;pass_reset_error:

	mov	ebx, MasterBootBuff
	;mov	ax, 0201h
	;mov	cx, 1
	; 25/07/2022
	;xor	ecx, ecx
	;inc	cl
	mov	cl, 1 ; sector 1
	; 06/05/2025
	; ecx = 1
	mov	eax, ecx ; ch = cylinder = 0
	mov	ah, 2  ; chs read
	; eax = 0201h
	xor	dh, dh ; head = 0
	call	int13h
	jc	short harddisk_error
	;
	cmp	word [MBIDCode], 0AA55h
	je	short load_masterboot_ok
	stc
harddisk_error:
load_masterboot_ok:
	retn

get_free_FAT_sectors:
	; 04/05/2025 (TRDOS 386 v2.0.10)
	; 29/08/2023
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 21/12/2017
	; 29/02/2016
	; 13/02/2016
	; 04/02/2016
	; 07/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 11/07/2010
	; 21/06/2009
	; INPUT: ESI = Logical DOS Drive Description Table address
	; OUTPUT: STC => Error
        ;	cf = 0 and EAX = Free FAT sectors
	; Also, related parameters and FAT buffer will be reset and updated

	xor	eax, eax
	;mov	[esi+LD_FreeSectors], eax ; Reset

	; 04/05/2025
	mov	 [FreeClusterCount], eax ; 0
	dec	eax  ; mov eax, -1

        cmp     byte [esi+LD_FATType], 2
	;jna	short loc_gfc_get_fat_free_clusters
	ja	short gffs_1
gffs_0:
	; invalidate free cluster count & first free cluster at first
	;mov	[esi+LD_BPB+64], eax
	mov	[esi+LD_BPB+FAT_FreeClusters], eax ; -1
	;mov	[esi+LD_BPB+68], eax
	mov	[esi+LD_BPB+FAT_FirstFreeClust], eax ; -1
	inc	eax ; 0
	jmp	short loc_gfc_get_fat_free_clusters
gffs_1:
	; 29/02/2016
	;dec	eax ; 0FFFFFFFFh
	; 04/05/2025
	;mov	[esi+LD_BPB+BPB_Reserved], eax ; Free cluster count (reset)
	mov	[esi+LD_BPB+FAT32_FreeClusters], eax ; -1
	;mov	[esi+LD_BPB+BPB_Reserved+4], eax ; First Free Cluster (reset)
	mov	[esi+LD_BPB+FAT32_FirstFreeClust], eax ; -1
	; 04/05/2025
	;mov	[esi+LD_BPB+BPB_Reserved+8], eax
	mov	[esi+LD_BPB+FAT32_fsinfo_sector], eax ; -1

; 04/05/2025
%if 0
	inc	eax ; 0
	;
	mov	ax, [esi+LD_BPB+BPB_FSInfo]
	add	eax, [esi+LD_StartSector]

	mov	ebx, DOSBootSectorBuff
	;mov	ecx, 1
	; 25/07/2022
	xor	ecx, ecx
	inc	cl
	; ecx = 1
 	call	disk_read
	jnc	short loc_gfc_check_fsinfo_signs
retn_gfc_get_fsinfo_sec:
	retn

loc_gfc_check_fsinfo_signs:
	mov 	ebx, DOSBootSectorBuff ; 13/02/2016
        cmp     dword [ebx], 41615252h
	jne	short retn_gfc_get_fsinfo_stc
	;add	ebx, 484
	;cmp	dword [ebx], 61417272h
	cmp	dword [ebx+484], 61417272h
	jne	short retn_gfc_get_fsinfo_stc
	;add	ebx, 4
	;mov	eax, [ebx]
%else
	; 04/05/2025 - TRDOS 386 v2.0.10
	
	call	read_fat32_fsinfo
	jc	short gffc_err
%endif

	;mov	eax, [ebx+488]
	; 04/05/2025
	mov	eax, [ebx+FSINFO.Free_Count]
	; 29/02/2016
	;mov	[esi+LD_BPB+BPB_Reserved], eax ; Free cluster count
	mov	[esi+LD_BPB+FAT32_FreeClusters], eax

	;mov	edx, [ebx+492]
	; 04/05/2025 
	mov	edx, [ebx+FSINFO.Nxt_Free]

	; 29/08/2023 (BugFix)
	;mov	[esi+LD_BPB+BPB_Reserved+4], edx ; First Free Cluster
	mov	[esi+LD_BPB+FAT32_FirstFreeClust], edx

	; 21/12/2017
	;mov	ebx, eax ; (initial value = 0FFFFFFFFh)
	;inc	ebx ; 0FFFFFFFFh -> 0  
	;jnz	short short retn_from_get_free_fat32_clusters

	; 04/05/2025
	inc	eax ; 0FFFFFFFFh -> 0
	jz	short gffs_2 ; invalid (initial), must be calculated
	dec	eax
	jmp	short retn_from_get_free_fat32_clusters
gffs_2:	
	;mov	[esi+LD_FreeSectors], eax  ; 0 ; Free clusters !
	;jmp	short loc_gfc_get_fat_free_clusters

;retn_gfc_get_fsinfo_stc:
	;stc

loc_gfc_get_fat_free_clusters:
	;mov	eax, 2
	mov	al, 2
	;;mov	[FAT_CurrentCluster], eax
	; 04/05/2025
	;mov	[CLUSNUM], eax

	; 04/05/2025
	; eax = start cluster number (=2)
	call	get_first_free_cluster
	jc	short gffc_err

	mov	[FirstFreeCluster], eax

	; if eax = -1 -> disk volume is full (free clusters = 0)
	inc	eax
	jz	short  gffc_err
		 ; -1 -> 0 ; not any free sectors (disk full)

	mov	[CLUSNUM], eax ; next cluster to search

	inc	dword [FreeClusterCount] ; 1

loc_gfc_loop_get_next_cluster:
	call	get_next_cluster
	jnc	short loc_gfc_free_fat_clusters_cont
	and	eax, eax
	jz	short loc_gfc_pass_inc_free_cluster_count

	; 04/05/2025
	; disk error
	; invalidate free cluster count for safety
	mov	dword [FreeClusterCount], -1

retn_from_get_free_fat_clusters:
	;mov	eax, [esi+LD_FreeSectors] ; Free clusters !
	; 04/05/2025
	mov	eax, [FreeClusterCount]
	mov	edx, [FirstFreeCluster]

	; 04/05/2025
        cmp     byte [esi+LD_FATType], 2
	;jna	short loc_gfc_get_fat_free_clusters
	ja	short gffs_3

	mov	[esi+LD_BPB+FAT_FreeClusters], eax
	mov	[esi+LD_BPB+FAT_FirstFreeClust], edx
	jmp	short gffs_4
gffs_3:
	mov	[esi+LD_BPB+FAT32_FreeClusters], eax
	mov	[esi+LD_BPB+FAT32_FirstFreeClust], edx
	; 04/05/2025
	; this is a flag for saving FSINFO later
	; (if the dword value in this field is 0)
	mov	dword [esi+LD_BPB+FAT32_fsinfo_sector], 0 ; modified

retn_from_get_free_fat32_clusters:
gffs_4:
	; 04/05/2025
	cmp	eax, -1
	je	short gffc_err

        movzx	ebx, byte [esi+LD_BPB+BPB_SecPerClust]
      	mul	ebx
	;mov	[esi+LD_FreeSectors], eax ; Free sectors
retn_get_free_sectors_calc:
gffc_err:	; 04/05/2025
	retn

loc_gfc_free_fat_clusters_cont:
	or	eax, eax
	jnz	short loc_gfc_pass_inc_free_cluster_count
	;inc	dword [esi+LD_FreeSectors] ; Free clusters !
	; 04/05/2025
	inc	dword [FreeClusterCount]

loc_gfc_pass_inc_free_cluster_count:
	;;mov	eax, [FAT_CurrentCluster]
	;mov	eax, ecx ; [FAT_CurrentCluster]
	; 04/05/2025
	;mov	eax, [CLUSNUM]
	mov	eax, ecx ; [CLUSNUM]
	cmp	eax, [esi+LD_Clusters]
	ja	short retn_from_get_free_fat_clusters
	inc	eax
	;mov	[FAT_CurrentCluster], eax
	jmp	short loc_gfc_loop_get_next_cluster

floppy_drv_init:
	; 06/05/2025 (TRDOS 386 Kenrel v2.0.10)
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 09/12/2017
	; 06/07/2016
	; 10/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 24/07/2011
	; 04/07/2009
	; INPUT ->
	;	DL = Drive number (0,1)
	; OUTPUT ->
	;	BL = drive name
	;	BH = drive number
	;	ESI = Logical DOS drv description table
	;	EAX = Volume serial number

	mov	esi, fd0_type ; 10/01/2016
	mov	edi, Logical_DOSDisks
	or	dl, dl
	jz	short loc_drv_init_fd0_fd1
	add	edi, 100h
	inc	esi ; fd1_type ; 10/01/2016
loc_drv_init_fd0_fd1:
	mov	byte [edi+LD_MediaChanged], 0
	cmp	byte [esi], 1 ; type (>0 if it is existing) 
		; 4 = 1.44 MB, 80 track, 3 1/2"
	jb	short read_fd_boot_sector_retn
	mov	[edi+LD_PhyDrvNo], dl
read_fd_boot_sector:
	xor	dh, dh
	mov	ecx, 4 ; Retry Count
read_fd_boot_sector_again:
	push 	ecx
	;mov	cx, 1
	mov	cl, 1
	mov	ax, 0201h ; Read 1 sector
	mov	ebx, DOSBootSectorBuff
	call	int13h
	pop	ecx
	jnc	short use_fd_boot_sector_params
	loop	read_fd_boot_sector_again

read_fd_boot_sector_stc_retn:
	stc
read_fd_boot_sector_retn:
	retn

use_fd_boot_sector_params:
	;mov	esi, DOSBootSectorBuff
	mov	esi, ebx
	cmp	word [esi+BS_Validation], 0AA55h
	jne	short read_fd_boot_sector_stc_retn
        ;cmp    word [esi+bs_FS_Identifier], 'SF'
	; 06/05/2025
	cmp	word [esi+bs_FS_Identifier], 'FS' ; NASM syntax !
	;jne	use_fd_fatfs_boot_sector_params
	; 25/07/2022
	je	short use_fdfs_boot_sector_params
	jmp	use_fd_fatfs_boot_sector_params
use_fdfs_boot_sector_params:
	mov	al, [esi+bs_FS_LBA_Ready]
	mov	[edi+LD_FS_LBAYes], al
	;
	; 03/01/2010 CHS -> DOS FAT/BPB compatibility fix
	mov	al, [esi+bs_FS_MediaAttrib]
	mov	[edi+LD_FS_MediaAttrib], al
	;
        mov	al, [esi+bs_FS_VersionMaj]
	mov	byte [edi+LD_FS_VersionMajor], al
	mov	ax, [esi+bs_FS_BytesPerSec]
	mov	[edi+LD_FS_BytesPerSec], ax
	mov	al, [esi+bs_FS_SecPerTrack]
	sub	ah, ah ; 09/12/2017
	mov	[edi+LD_FS_SecPerTrack], ax
	mov	al, [esi+bs_FS_Heads]
	mov	[edi+LD_FS_NumHeads], ax
	;
	mov	eax, [esi+bs_FS_UnDelDirD]
	mov	[edi+LD_FS_UnDelDirD], eax
	mov	eax, [esi+bs_FS_MATLocation]
	mov	[edi+LD_FS_MATLocation], eax
	mov	eax, [esi+bs_FS_RootDirD]
	mov	[edi+LD_FS_RootDirD], eax
	mov	eax, [esi+bs_FS_BeginSector]
	mov	[edi+LD_FS_BeginSector], eax
	mov	eax, [esi+bs_FS_VolumeSize]
	mov	[edi+LD_FS_VolumeSize], eax
	;		
	mov	esi, edi
 	mov	eax, [esi+LD_FS_MATLocation]
	;add	eax, [edi+LD_FS_BeginSector]
read_fd_MAT_sector_again:
	;mov	ebx, DOSBootSectorBuff
	;mov	ecx, 1
	mov	cl, 1
	call	chs_read
	mov	esi, ebx
	;jnc	short use_fdfs_mat_sector_params
	;jmp	short read_fd_boot_sector_retn
	;retn
	; 25/07/2022
	jc	short read_fd_RDT_sector_retn
use_fdfs_mat_sector_params:
	mov	eax, [esi+FS_MAT_DATLocation]
	mov	[edi+LD_FS_DATLocation], eax
	mov	eax, [esi+FS_MAT_DATScount]
	mov	[edi+LD_FS_DATSectors], eax
	mov	eax, [edi+FS_MAT_FreeSectors]
	mov	[edi+LD_FS_FreeSectors], eax
	mov	eax, [esi+FS_MAT_FirstFreeSector]
	mov	[edi+LD_FS_FirstFreeSector], eax
	;
	;mov	esi, edi
	; 06/05/2025
 	xchg	esi, edi
	mov	eax, [esi+LD_FS_RootDirD]
read_fd_RDT_sector_again:
	;mov	ebx, DOSBootSectorBuff
	;mov	cx, 1
	mov	cl, 1
	call	chs_read
	;mov	esi, ebx
	; 06/05/2025
	; (eax, ebx, ecx, edx regs are changed)
	; ((ecx < 256))
	xchg	edi, esi
	jc	short read_fd_RDT_sector_retn
	; esi = RDT buffer (DOSBootSectorBuff)
	; edi = Logical Dos Drv Desc. Table address
use_fdfs_RDT_sector_params:
	mov	eax, [esi+FS_RDT_VolumeSerialNo]
	mov	[edi+LD_FS_VolumeSerial], eax
	push	edi
	;mov	ecx, 16
	mov	cl, 16
	add	esi, FS_RDT_VolumeName
	add	edi, LD_FS_VolumeName
	rep	movsd ; 64 bytes
	pop	esi
	mov	byte [esi+LD_FATType], 0
	mov	byte [esi+LD_FSType], 0A1h
        jmp     loc_cont_use_fd_boot_sector_params

read_fd_RDT_sector_stc_retn:
	stc
read_fd_RDT_sector_retn:
	retn

use_fd_fatfs_boot_sector_params:
	cmp	byte [esi+BS_BootSig], 29h
	jne	short read_fd_RDT_sector_stc_retn
	cmp	byte [esi+BPB_Media], 0F0h
	jb	short read_fd_RDT_sector_retn
	push	edi
	add	edi, LD_BPB
	; 06/05/2025
	; (ecx < 256)
	;mov	ecx, 16
	mov	cl, 16
	rep	movsd ; 64 bytes
	pop	esi
	xor	eax, eax
	mov	[esi+LD_StartSector], eax ; 0
	mov	ax, [esi+LD_BPB+BPB_FATSz16]
	mov	cl, [esi+LD_BPB+BPB_NumFATs]
  	mul	ecx
	; edx = 0 !
	mov	dx, [esi+LD_BPB+BPB_RsvdSecCnt]
	;mov	[esi+LD_FATBegin], dx
	; 06/05/2025
	mov	[esi+LD_FATBegin], edx
	; 25/07/2022
	add	eax, edx
	;add	ax, dx
	mov	[esi+LD_ROOTBegin], eax
	mov	[esi+LD_DATABegin], eax 
	mov	dx, [esi+LD_BPB+BPB_RootEntCnt]
	;;shl	edx, 5 ; * 32 (Size of a directory entry)
	;shl	dx, 5
	;;add	edx, 511
	;add	dx, 511
	;;shr	edx, 9 ; edx = ((edx*32)+511) / 512
	;shr	dx, 9
	; 06/05/2025
	; following round up is not necessary
	; because root directory entry count is 112 or 224
	;	 (for floppy disks)
	;add	dx, 15 ; 06/07/2016 (+(512/32)-1)

	;shr	dx, 4 ; / 16 (==16 entries per sector)
	; 25/07/2022
	shr	edx, 4
	add 	[esi+LD_DATABegin], edx ; + rd sectors
	; 06/05/2025
	; here, eax <= 18 for floppy disks (with FAT12 fs)
	;movzx	eax, word [esi+LD_BPB+BPB_TotalSec16]
	mov	ax, [esi+LD_BPB+BPB_TotalSec16]
	mov	[esi+LD_TotalSectors], eax
	sub	eax, [esi+LD_DATABegin]
	; 06/05/2025
	; here, ecx = 2 for floppy disks (with FAT12 fs)
  	;movzx	ecx, byte [esi+LD_BPB+BPB_SecPerClust]
	mov	cl, [esi+LD_BPB+BPB_SecPerClust]
	cmp	cl, 1
	jna	short save_fd_fatfs_cluster_count
	; 25/07/2022
	sub	edx, edx
	;sub	dx, dx ; 0
	;sub	dl, dl ; 06/07/2016
	div	ecx
save_fd_fatfs_cluster_count:
	mov	[esi+LD_Clusters], eax

      ; Maximum Valid Cluster Number = EAX +1
      ; with 2 reserved clusters

; 07/05/2025 - TRDOS 386 v2.0.10
%if 0

reset_FAT_buffer_decriptors:
	sub	eax, eax ; 0  
	mov	[FAT_BuffValidData], al ; 0
	mov	[FAT_BuffDrvName], al ; 0
	mov	[FAT_BuffSector], eax ; 0

read_fd_FAT_sectors:
  	mov	ebx, FAT_Buffer
	mov	ax, [esi+LD_BPB+BPB_RsvdSecCnt]
	;mov	ecx, 3
	mov	cl, 3 ; 3 sectors
	call	chs_read
	jc	short read_fd_FAT_sectors_retn
use_fd_FAT_sectors:
	mov	al, [esi+LD_PhyDrvNo]
	add	al, 'A' 
	mov	[FAT_BuffDrvName], al
 	mov	byte [FAT_BuffValidData], 1
	call	fd_init_calculate_free_clusters
	jc	short read_fd_FAT_sectors_retn

%else
	; 07/05/2025
	; esi = Logical Dos Drv Desc. Table
set_fd_FAT_free_sectors:
	call	get_free_FAT_sectors
	jc	short read_fd_FAT_sectors_retn
	; eax = free sectors
	mov	[esi+LD_FreeSectors], eax
%endif

loc_use_fd_boot_sector_params_FAT:
	mov	byte [esi+LD_FATType], 1 ; FAT 12
	mov	byte [esi+LD_FSType], 1
        mov     eax, [esi+LD_BPB+VolumeID]

loc_cont_use_fd_boot_sector_params:
	mov	bh, [esi+LD_PhyDrvNo]
	mov	[esi+LD_DParamEntry], bh
	mov	bl, bh
	add	bl, 'A'
	mov	byte [esi+LD_Name], bl
	mov	byte [esi+LD_DiskType], 1
	mov	byte [esi+LD_LBAYes], 0
	mov	byte [esi+LD_PartitionEntry], 0
	mov	byte [esi+LD_MediaChanged], 6 ; Volume Name Reset

read_fd_FAT_sectors_retn:
	retn

; 07/05/2025 - TRDOS 386 v2.0.10
%if 0

fd_init_calculate_free_clusters:
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 09/12/2017
	; 10/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 04/07/2009
	; INPUT ->
	;     ESI = Logical DOS drive description table address
	; OUTPUT ->
	;    [ESI+LD_FreeSectors] will be set
	
	sub	eax, eax
	mov	[esi+LD_FreeSectors], eax ; 0
	mov	al, 2 ; eax = 2

fd_init_loop_get_next_cluster:
	call	fd_init_get_next_cluster
	jc	short fd_init_calculate_free_clusters_retn

fd_init_free_fat_clusters:
	;cmp 	eax, 0
	;ja	short fd_init_pass_inc_free_cluster_count
	;and	eax, eax
	;jnz	short fd_init_pass_inc_free_cluster_count
	;and	ax, ax
	and	eax, eax ; 25/07/2022
	jnz	short fd_init_pass_inc_free_cluster_count
	; 25/07/2022
	inc	dword [esi+LD_FreeSectors]
        ;inc	word [esi+LD_FreeSectors]

fd_init_pass_inc_free_cluster_count:
  	; 25/07/2022
	mov	eax, [FAT_CurrentCluster]
	;mov	ax, [FAT_CurrentCluster]
	cmp	eax, [esi+LD_Clusters]
	;cmp	ax, [esi+LD_Clusters]
	ja	short short retn_from_fd_init_calculate_free_clusters
	inc	eax
	;inc	ax
	jmp	short fd_init_loop_get_next_cluster

retn_from_fd_init_calculate_free_clusters:
	; 25/07/2022
	;xor	eax, eax
	xor	ah, ah
  	mov	al, [esi+LD_BPB+BPB_SecPerClust]
  	cmp	al, 1
	jna	short fd_init_calculate_free_clusters_retn
	;;movzx	eax, al
	;xor	ah, ah ; 09/12/2017
	; 25/07/2022
	mov	ecx, [esi+LD_FreeSectors]
	;mov	cx, [esi+LD_FreeSectors] ; Count of free clusters
  	mul	ecx
	;mul	cx
	mov	[esi+LD_FreeSectors], eax
	;mov	[esi+LD_FreeSectors], ax
fd_init_calculate_free_clusters_retn:
	retn

fd_init_get_next_cluster:
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 04/02/2016
	; 02/02/2016
	; 10/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 04/07/2009
	; INPUT ->
	;    EAX = Current cluster
	;    ESI = Logical DOS drive description table address
	;    EDX = 0
	; OUTPUT ->
	;    EAX = Next cluster

	mov	[FAT_CurrentCluster], eax
fd_init_get_next_cluster_readnext:
	sub	edx, edx ; 0
  	mov	ebx, 1024 ; 400h
  	div	ebx
  	; EAX = Count of 3 FAT sectors
  	; EDX = Buffer entry index
	mov	ecx, eax
	;mov	eax, 3
	mov	al, 3
	mul	edx ; Multiply by 3
	;shr	ax, 1 ; Divide by 2
	; 25/07/2022
	shr	eax, 1
	mov	ebx, eax ; Buffer byte offset
	add	ebx, FAT_Buffer
	mov	eax, ecx
	;mov	edx, 3
	mov	dx, 3
	mul	edx
  	; EAX = FAT Beginning Sector
	; EDX = 0
	mov	cl, [esi+LD_Name]
	;cmp	byte [FAT_BuffValidData], 0
	;jna	short fd_init_load_FAT_sectors0
	cmp	cl, [FAT_BuffDrvName]
	jne	short fd_init_load_FAT_sectors0
	cmp	eax, [FAT_BuffSector]
	jne	short fd_init_load_FAT_sectors1
	; 25/07/2022
	mov	eax, [FAT_CurrentCluster]
	;mov	al, [FAT_CurrentCluster]
	shr	eax, 1
	;shr	al, 1
	mov	ax, [ebx]
  	jnc	short fd_init_gnc_even
	;shr	ax, 4
	; 25/04/2022
	shr	eax, 4
fd_init_gnc_clc_retn:
	clc
	retn

fd_init_gnc_even:
	and	ah, 0Fh
	retn

fd_init_load_FAT_sectors0:
	mov 	[FAT_BuffDrvName], cl
fd_init_load_FAT_sectors1:
	mov	byte [FAT_BuffValidData], 0
	mov	[FAT_BuffSector], eax
	add	eax, [esi+LD_FATBegin]
 	mov	ebx, FAT_Buffer
	;movzx	ecx, word [esi+LD_BPB+BPB_FATSz16]
	mov	cx, [esi+LD_BPB+BPB_FATSz16]
	;sub	cx, [FAT_BuffSector]
        ; 25/07/2022
	sub	ecx, [FAT_BuffSector]
	;sub	edx, edx
	mov	dl, 3
	; edx = 3 
	;;cmp	ecx, 3
	;cmp	cx, 3
	cmp	ecx, edx ; 3
	jna	short fdinit_pass_fix_sector_count_3
	;;mov	ecx, 3
	;mov	ecx, 3
	mov	ecx, edx ; 3
fdinit_pass_fix_sector_count_3:
	call	chs_read
	jnc	short fd_init_FAT_sectors_no_load_error
	mov	byte [FAT_BuffValidData], 0
		; Drv not ready or read Error !
	mov	eax, ERR_DRV_NOT_RDY ; 15
	;xor	edx, edx
	retn

fd_init_FAT_sectors_no_load_error:
	mov	byte [FAT_BuffValidData], 1
	mov	eax, [FAT_CurrentCluster]
        jmp     fd_init_get_next_cluster_readnext

%endif

get_FAT_volume_name:
	; 13/05/2025
	; 08/05/2025
	; 07/05/2025 (TRDOS 386 Kernel v2.0.10)
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 10/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 12/09/2009
	; INPUT ->
	;	BH = Logical DOS drive number (0,1,2,3,4 ...)
	;       BL = 0
	; OUTPUT ->
	;	CF = 0 -> ESI = Volume name address
	; 	CF = 1 -> Root volume name not found
	;
	; Modified registers:
	;	EAX, EBX, ECX, EDX, ESI, EDI, EBP

	;mov 	ah, 0FFh
	;mov 	al, [Last_Dos_DiskNo]
	;cmp 	al, bh
	;jb     short loc_gfvn_dir_load_err

	mov	esi, ebx
	and	esi, 0FF00h ; esi = bh
	add	esi, Logical_DOSDisks
	mov     al, [esi+LD_Name]
	mov     ah, [esi+LD_FATType]
	cmp     ah, 1
	jb    	short loc_gfvn_dir_load_err
	cmp 	al, 'A'
	jb      short loc_gfvn_dir_load_err
	cmp 	ah, 2 
	ja      short gfvn_load_FAT32_root_dir

	; 07/05/2025 - TRDOS 386 Kernel v2.0.10
	; ! major modification !
	call	load_FAT_root_directory
	jnc	short loc_get_volume_name

	; eax = error code
	
loc_gfvn_dir_load_err:
	retn

	; 08/05/2025
gfvn_load_FAT32_root_dir:
	mov	eax, [esi+LD_BPB+BPB_RootClus]
gfvn_load_FAT32_root_dir_nc:
	; 08/05/2025 - TRDOS 386 Kernel v2.0.10
	; ! major modification !
	call	load_FAT_sub_directory
	jc	short loc_gfvn_dir_load_err

	; 13/05/2025
	; 08/05/2025
	; eax = physical address of dir sector
	; ebx = directory buffer (header) address
	; 13/05/2025
	; [CLUSNUM] = current cluster number
	movzx	ebp, byte [esi+LD_BPB+SecPerClust]
	; ebp = sectors per cluster
	jmp	short check_root_volume_name_ns
	
loc_get_volume_name:

; 07/05/2025
%if 0
        mov     esi, Directory_Buffer
	;xor	cx, cx ; 0
	; 25/07/2022
	xor	ecx, ecx ; 0
check_root_volume_name:
	mov	al, [esi]
	or      al, al
	jz      short loc_get_volume_name_retn
	cmp     byte [esi+0Bh], 08h
	je      short loc_get_volume_name_retn
	cmp     cx, [DirBuff_LastEntry]
	jnb     short pass_check_root_volume_name
	;inc	cx
	; 25/07/2022
	inc	ecx
	add     esi, 32
	jmp     short check_root_volume_name

	; 29/08/2023
;loc_get_volume_name_retn:
	;retn

pass_check_root_volume_name:
	cmp	byte [DirBuff_FATType], 3
	jb	short loc_get_volume_name_retn_xor

	mov	ebx, FAT_Buffer
	mov	esi, Logical_DOSDisks
	xor	eax, eax
	mov	ah, [DirBuff_DRV]
	sub	ah, 'A' 
	add	esi, eax
	mov	eax, [DirBuff_Cluster]
	call	get_next_cluster
	jnc 	short loc_gfvn_load_FAT32_dir_cluster

	cmp     eax, 1
	cmc
	retn

loc_gfvn_load_FAT32_dir_cluster:
	call	load_FAT_sub_directory
	jnc	short loc_get_volume_name
	retn

loc_get_volume_name_retn_xor:
	xor 	eax, eax
	retn
%else
	; 07/05/2025 - TRDOS 386 v2.0.10
	; eax = root directory (1st) sector
	mov	ebp, ecx ; root dir sectors
check_root_volume_name_ns: ; next sector
	; ebx = directory buffer (header) address
	; ebp = root directory sectors (remain)
	; esi = LDRVT address (= edx)
	; eax = root directory sector

	add	ebx, BUFINSIZ
	; ebx = buffer data address

	mov	edi, eax ; save phy sector number
	mov	edx, 16 ; 16 entries per sector
check_root_volume_name:
	mov	al, [ebx]
	or	al, al
	jz	short loc_get_volume_name_stc_retn
	cmp	al, 0E5h
	je	short chk_next_rde ; deleted entry
	cmp	byte [ebx+0Bh], 08h
	;je	short loc_get_volume_name_retn
	; 08/05/2025
	jne	short chk_next_rde

loc_get_volume_name_ok:
	mov	esi, ebx
	; esi = volume name offset/address in dir buf
	; (11 byte volume name)
	retn

chk_next_rde:
	dec	edx
	jz	short load_next_root_dir_sector
	add	ebx, 32 ; directory entry size
	jmp	short check_root_volume_name

load_next_root_dir_sector:
	dec	ebp ; root dir (remain) sectors
	;jz	short loc_get_volume_name_stc_retn
	; 08/05/2025
	jz	short check_fat32_nc_or_gvn_retn

	; load next root dir sector
	mov	eax, edi ; restore phy sector number
	inc	eax
	; esi = LDRVT address

	call    load_FAT_root_directory_ns
	jnc	short check_root_volume_name_ns

loc_get_volume_name_retn:	
	; eax = error code
	retn
	
loc_get_volume_name_stc_retn:
	stc	
	retn

check_fat32_nc_or_gvn_retn:
	; 08/05/2025
	cmp	byte [esi+LD_FATType], 3
	jb	short loc_get_volume_name_retn ; cf = 1

	; 13/05/2025
	mov	eax, [CLUSNUM]
	call	get_next_cluster
	jc	short loc_get_volume_name_retn
			; EndOfClusterChain (eax = 0)
			; or Error (eax = error number)	

	; eax = next cluster (this is not EOCC)
	
	;; FAT32
	;; ecx = next cluster
	;mov	eax, ecx
	;cmp	ecx, 0FFFFFF8h
	;jnb	short loc_get_volume_name_stc_retn

	mov	[CLUSNUM], eax
	
	jmp	gfvn_load_FAT32_root_dir_nc

%endif

get_media_change_status:
	; 10/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 09/09/2009
	; INPUT:
	;     DL = Drive number (physical)
	; OUTPUT: clc & AH = 6 media changed
	;     clc & AH = 0 media not changed
	;     stc -> Drive not ready or an error

	mov	ah, 16h
  	call	int13h
	cmp	ah, 06h
	je	short loc_gmc_status_retn
	or	ah, ah
	jz	short loc_gmc_status_retn
loc_gmc_status_stc_retn:
	stc
loc_gmc_status_retn:
	retn
